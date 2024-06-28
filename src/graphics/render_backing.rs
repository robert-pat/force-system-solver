#![allow(dead_code)]

/* Note on safety: for all of these Vulkan functions, literally everything can be null or
 * otherwise invalid b/c all of
 * the structs are just handles to the actual resources created by the driver. Technically, this
 * makes doing anything with anything unsafe, but I don't know of a way to fix that (aside from
 * validating everything before calling it, which the vulkanalina crate doesn't support beyond
 * null checks). Instead, I'm just going to panic wherever possible & hope windows will clean
 * everything up. I don't really know what else to do.
 *
 * Maybe at some point, I'll come back and actually handle any of the Vulkan error codes
 */

use std::collections::HashSet;
use std::ffi::c_void;
use cgmath::{vec3};
use vulkanalia::loader::{LibloadingLoader, LIBRARY};
use vulkanalia::prelude::v1_0::*;
use vulkanalia::vk::{KhrSurfaceExtension, PhysicalDevice, SwapchainKHR};
use vulkanalia::vk::KhrSwapchainExtension;
use vulkanalia::window as vk_window;
use winit::window::Window;
use crate::parsing;

#[derive(Debug)]
pub(super) struct VulkanApp {
    entry:Entry,
    instance: Instance,
    device: Device,
    data: AppData,
    frame: usize,
    resized: bool,

    mem_mapped_vertices: Option<(*mut c_void, NumBytes)>,
    old_vertices: [Vertex; MAX_VERTICES],
}
impl VulkanApp {
    /// Create a new VulkanApp w/ all of the backing needed. This function will panic if any issue
    /// with Vulkan occurs in the setup process.
    pub(super) fn create(window: &Window) -> Self {
        // Safety: this function is unsafe bc it's calling foreign code;
        // need to be sure to call VulkanApp::destroy() eventually to free resources
        let loader = match unsafe { LibloadingLoader::new(LIBRARY) } {
            Ok(l) => l,
            Err(e) => panic!("Could not initialize library: {}", e),
        };
        // Safety: The requirements for entry are not helpful; the entry passed to
        // instance will be valid because we panic if creating a new entry fails
        let entry = unsafe { Entry::new(loader).unwrap() };
        let instance = unsafe { create_instance(window, &entry) };

        let mut data = AppData {
            // Safety: idk it's not documented like at all
            surface: unsafe { vk_window::create_surface(&instance, &window, &window).unwrap() },
            ..Default::default()
        };

        // Safety: we panic if the function fails, so our physical device can never be invalid
        if unsafe { pick_physical_device(&instance, &mut data) }.is_err() {
            eprintln!("Issue selecting physical device at; {}, line: {}", file!(), line!());
            panic!("Unable to select a physical device: none worked!");
        }

        let device = create_logical_device(&entry, &instance, &mut data);
        create_swapchain(window, &instance, &device, &mut data);
        create_swapchain_image_views(&device, &mut data);
        create_render_pass(&instance, &device, &mut data);
        create_pipeline(&device, &mut data);
        create_frame_buffers(&device, &mut data);

        create_command_pool(&instance, &device, &mut data);
        create_vertex_buffer(&instance, &device, &mut data);
        create_command_buffers(&device, &mut data);
        create_sync_objects(&device, &mut data);

        Self {
            entry,
            instance,
            device,
            data,
            frame: 0,
            resized: false,

            mem_mapped_vertices: None,
            old_vertices: VERTICES,
        }
    }
    pub(super) fn render(&mut self, window: &Window) {
        // Steps: 1) Get image, 2) Run the command buffer for that image, 3) put the image on the
        // swapchain to be displayed
        // We have the fences & semaphores to not double submit work & keep things synced
        unsafe {
            let fences = &[self.data.in_flight_fences[self.frame]];
            self.device.wait_for_fences(fences, true, u64::MAX).unwrap();
        }
        let image_index = unsafe {
            let next = self.device.acquire_next_image_khr(
                self.data.swapchain,
                u64::MAX,
                self.data.image_available_semaphores[self.frame],
                vk::Fence::null(),
            );
            match next {
                Ok((i, _)) => i as usize,
                Err(vk::ErrorCode::OUT_OF_DATE_KHR) => {
                    self.recreate_swapchain(window);
                    return;
                },
                _a => panic!("Error getting image from swapchain: {_a:?}"),
            }
        };
        // images may be out-of-order, so we have to check that ourselves
        if !self.data.images_in_flight[image_index].is_null() {
            let fences = &[self.data.images_in_flight[image_index]];
            unsafe {
                self.device.wait_for_fences(fences,true, u64::MAX).unwrap();
            }
        }
        self.data.images_in_flight[image_index] = self.data.in_flight_fences[self.frame];

        // make the creature do things
        let wait_semaphores = &[self.data.image_available_semaphores[self.frame]];
        let wait_stages = &[vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT];
        let command_buffers = &[self.data.command_buffers[image_index]];
        let signal_semaphores = &[self.data.render_finished_semaphores[self.frame]];
        let submit_info = vk::SubmitInfo::builder()
            .wait_semaphores(wait_semaphores) // what to wait for
            .wait_dst_stage_mask(wait_stages) // where to pause while waiting
            .command_buffers(command_buffers) // what to run
            .signal_semaphores(signal_semaphores); // what to update when done
        unsafe {
            self.device.reset_fences(&[self.data.in_flight_fences[self.frame]]).unwrap();
            let fence = self.data.in_flight_fences[self.frame];
            self.device.queue_submit(self.data.graphics_queue, &[submit_info], fence).unwrap();
        }

        if self.resized {
            self.resized = false;
            unsafe { self.recreate_swapchain(window); }
        }

        let swapchains = &[self.data.swapchain];
        let image_indices = &[image_index as u32];
        let present_info = vk::PresentInfoKHR::builder()
            .wait_semaphores(signal_semaphores)
            .swapchains(swapchains)
            .image_indices(image_indices);
        unsafe {
            #[allow(unused)] // IntelliJ linter doesn't know fmt literals
            match self.device.queue_present_khr(self.data.present_queue, &present_info) {
                Ok(vk::SuccessCode::SUBOPTIMAL_KHR) => self.recreate_swapchain(window),
                Err(vk::ErrorCode::OUT_OF_DATE_KHR) => self.recreate_swapchain(window),
                Err(e) => panic!("Could not queue presenting frame {e:?}"),
                _ => {},
            }
        }
        self.frame = (self.frame + 1) % MAX_FRAMES_IN_FLIGHT;
    }
    pub(super) fn update_truss(&mut self, _t: &Truss2D) {

    }
    pub(super) fn change_something_shiv(&mut self) {
        if self.mem_mapped_vertices.is_none() {
            eprintln!("Mapping memory");
            unsafe {
                let mem_info = map_memory(&self.device, &self.data);
                self.mem_mapped_vertices = Some(mem_info);
            }
        }
        let vertices: [Vertex; 3] = [
            self.old_vertices[0].tick_color(),
            self.old_vertices[1].tick_color(),
            self.old_vertices[2].tick_color(),
        ];
        let (pointer, size) = self.mem_mapped_vertices.unwrap();
        assert_eq!(std::mem::size_of_val(&vertices), size);

        unsafe {
            let pointer = pointer as *mut Vertex;
            std::ptr::copy_nonoverlapping(vertices.as_ptr(), pointer, vertices.len());
        }
        eprintln!("Changed Vertex data from {:?} to {:?}", self.old_vertices, vertices);
        self.old_vertices = vertices;
    }
    unsafe fn recreate_swapchain(&mut self, window: &Window) {
        self.device.device_wait_idle().unwrap();
        self.destroy_swapchain();

        create_swapchain(window, &self.instance, &self.device, &mut self.data);
        create_swapchain_image_views(&self.device, &mut self.data);
        create_render_pass(&self.instance, &self.device, &mut self.data);
        create_pipeline(&self.device, &mut self.data);
        create_frame_buffers(&self.device, &mut self.data);
        create_command_buffers(&self.device, &mut self.data);
        self.data.images_in_flight.resize(self.data.swapchain_images.len(), vk::Fence::null());
    }
    /// Free the allocated GPU resources & Vulkan driver handle.
    ///
    /// Safety: Once this is called, self MUST NOT be called again; i.e. this struct becomes dead.
    /// It can not render, be created, wait, resize, or do anything else
    pub(super) unsafe fn destroy(&mut self) {
        unmap_memory(&self.device, &self.data);
        self.destroy_swapchain();

        self.device.destroy_buffer(self.data.vertex_buffer, None);
        self.device.free_memory(self.data.vertex_buffer_memory, None);
        self.data
            .render_finished_semaphores
            .iter()
            .for_each(|s| self.device.destroy_semaphore(*s, None));
        self.data
            .image_available_semaphores
            .iter()
            .for_each(|s| self.device.destroy_semaphore(*s, None));
        self.data
            .in_flight_fences
            .iter()
            .for_each(|f| self.device.destroy_fence(*f, None));

        self.device
            .destroy_command_pool(self.data.command_pool, None);
        self.instance.destroy_surface_khr(self.data.surface, None);
        self.device.destroy_device(None);
        self.instance.destroy_instance(None);
    }
    /// Safety: destroying things out-of-order can cause UB, not destroying things can leak them
    /// and prob some other stuff. Destroy in reverse creation order.
    unsafe fn destroy_swapchain(&mut self) {
        self.data
            .frame_buffers
            .iter()
            .for_each(|f| self.device.destroy_framebuffer(*f, None));
        self.device
            .free_command_buffers(self.data.command_pool, &self.data.command_buffers);
        self.device.destroy_pipeline(self.data.pipeline, None);
        self.device
            .destroy_pipeline_layout(self.data.pipeline_layout, None);
        self.device.destroy_render_pass(self.data.render_pass, None);
        self.data
            .swapchain_image_views
            .iter()
            .for_each(|v| self.device.destroy_image_view(*v, None));
        self.device.destroy_swapchain_khr(self.data.swapchain, None);
    }
    pub(super) fn wait_device(&self) {
        // this one feel like maybe it's not actually unsafe to call ?
        unsafe { self.device.device_wait_idle() }.unwrap();
    }
    /// This function only has an effect the next time [VulkanApp::render()] is called.
    pub(super) fn resize(&mut self) { self.resized = true; }
}

#[derive(Debug, Default)]
struct AppData{
    /// Handle to the physical device we've requested from the Vulkan driver
    physical_device: PhysicalDevice,
    /// The surface that we have
    surface: vk::SurfaceKHR,
    graphics_queue: vk::Queue,
    present_queue: vk::Queue,

    swapchain: SwapchainKHR,
    swapchain_images: Vec<vk::Image>,
    swapchain_format: vk::Format,
    swapchain_extent: vk::Extent2D,
    swapchain_image_views: Vec<vk::ImageView>,

    render_pass: vk::RenderPass,

    pipeline: vk::Pipeline,
    pipeline_layout: vk::PipelineLayout,
    frame_buffers: Vec<vk::Framebuffer>,

    command_pool: vk::CommandPool,
    command_buffers: Vec<vk::CommandBuffer>,

    image_available_semaphores: Vec<vk::Semaphore>,
    render_finished_semaphores: Vec<vk::Semaphore>,
    in_flight_fences: Vec<vk::Fence>,
    images_in_flight: Vec<vk::Fence>,

    vertex_buffer: vk::Buffer,
    vertex_buffer_memory: vk::DeviceMemory,
}


/* Structs for holding more information about devices / more convenient way to filter them. */
struct QueueFamilyIndices {
    graphics: u32,
    present: u32,
}
impl QueueFamilyIndices {
    fn get(inst: &Instance, surface: &vk::SurfaceKHR, dev: &PhysicalDevice) -> Option<Self> {
        let properties = unsafe {
            inst.get_physical_device_queue_family_properties(*dev)
        };
        let graphics = properties.iter()
            .position(|p| p.queue_flags.contains(vk::QueueFlags::GRAPHICS))
            .map(|i| i as u32)?;
        let present = properties.iter()
            .enumerate()
            .map(|(idx, _)| idx)
            .find(|index| unsafe {
                inst.get_physical_device_surface_support_khr(*dev, *index as u32, *surface).unwrap()
            })
            .map(|i| i as u32)?;

        Some(Self {
            graphics,
            present
        })
    }
    fn to_set(&self) -> HashSet<u32> {
        let mut set = HashSet::new();
        set.insert(self.present);
        set.insert(self.graphics);
        set
    }
}
/// Holds information about what swapchain features a device supports.
struct SwapchainSupport {
    capabilities: vk::SurfaceCapabilitiesKHR,
    formats: Vec<vk::SurfaceFormatKHR>,
    present_mode: Vec<vk::PresentModeKHR>,
}
impl SwapchainSupport {
    fn get(inst: &Instance, surface: &vk::SurfaceKHR, phys_device: PhysicalDevice) -> Option<Self> {
        unsafe {
            // thank god all of these are unsafe and have 800 * 10^14 character long method names
            let cap = inst.get_physical_device_surface_capabilities_khr(phys_device, *surface).unwrap();
            let formats = inst.get_physical_device_surface_formats_khr(phys_device, *surface).unwrap();
            let modes = inst.get_physical_device_surface_present_modes_khr(phys_device, *surface).unwrap();

            Some(Self {
                capabilities: cap,
                formats,
                present_mode: modes
            })
        }
    }
}

type Vec2 = cgmath::Vector2<f32>;
type Vec3 = cgmath::Vector3<f32>;

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct Vertex {
    pos: cgmath::Vector2<f32>,
    color: cgmath::Vector3<f32>,
}
impl Vertex {
    const fn new(pos: Vec2, color: Vec3) -> Self {
        Self { pos, color }
    }
    const fn from_arr(pos: [f32; 2], color: [f32; 3]) -> Self {
        Self {
            pos: cgmath::vec2(pos[0], pos[1]),
            color: vec3(color[0], color[1], color[2]),
        }
    }
    fn binding_description() -> vk::VertexInputBindingDescription {
        vk::VertexInputBindingDescription::builder()
            .binding(0)
            .stride(std::mem::size_of::<Vertex>() as u32)
            .input_rate(vk::VertexInputRate::VERTEX)
            .build()
    }
    fn attribute_descriptions() -> [vk::VertexInputAttributeDescription; 2] {
        let pos = vk::VertexInputAttributeDescription::builder()
            .binding(0)
            .location(0)
            .format(vk::Format::R32G32_SFLOAT)
            .offset(0)
            .build();
        let color = vk::VertexInputAttributeDescription::builder()
            .binding(0)
            .location(1)
            .format(vk::Format::R32G32B32_SFLOAT)
            .offset(std::mem::size_of::<Vec2>() as u32)
            .build();
        [pos, color]
    }
    fn tick_color(&self) -> Vertex {
        let old = self.color;
        let new = vec3(
            (old.x + 0.1) % 1.0,
            (old.y + 0.1) % 1.0,
            (old.z + 0.1) % 1.0,
        );
        Vertex::new(self.pos, new)
    }
}
impl Default for Vertex {
    fn default() -> Self {
        Vertex::from_arr([0f32; 2], [0f32; 3])
    }
}

type NumBytes = usize;
type NumVertex = usize;
const MAX_FRAMES_IN_FLIGHT: usize = 2;
const MAX_VERTICES: usize = 3;
const VERTICES: [Vertex; MAX_VERTICES] = [
    Vertex::new(cgmath::vec2(0.0, -0.5), vec3(1.0, 0.0, 0.0)),
    Vertex::new(cgmath::vec2(0.5, 0.5), vec3(0.0, 1.0, 0.0)),
    Vertex::new(cgmath::vec2(-0.5, 0.5), vec3(0.0, 0.0, 1.0)),
];

/* Working with vertices */
fn create_vertex_buffer(inst: &Instance, device: &Device, data: &mut AppData) {
    let buffer_info = vk::BufferCreateInfo::builder()
        .size((std::mem::size_of::<Vertex>() * MAX_VERTICES) as u64)
        .usage(vk::BufferUsageFlags::VERTEX_BUFFER)
        .sharing_mode(vk::SharingMode::EXCLUSIVE)
        .flags(vk::BufferCreateFlags::empty());
    assert!(!device.handle().is_null());
    unsafe {
        data.vertex_buffer = device.create_buffer(&buffer_info, None).unwrap();
    }
    let requirements = unsafe { device.get_buffer_memory_requirements(data.vertex_buffer) };
    let mem_index = {
        let mem_prop = unsafe {inst.get_physical_device_memory_properties(data.physical_device)};
        let bits = requirements.memory_type_bits;
        // HOST_COHERENT is important bc otherwise the buffer may not be up-to-date (💖 caching)
        let flags = vk::MemoryPropertyFlags::HOST_COHERENT | vk::MemoryPropertyFlags::HOST_VISIBLE;

        mem_prop.memory_types.iter()
            .enumerate()
            .filter(|(i, m)| {
                // I have no idea why we do this
                let suitability = bits & (1 << *i as u32) != 0;
                suitability && m.property_flags.contains(flags)
            })
            .map(|(idx, _)| idx as u32)
            .next()
            .unwrap()
    };
    let memory_info = vk::MemoryAllocateInfo::builder()
        .allocation_size(requirements.size)
        .memory_type_index(mem_index);

    // we need to allocate on the GPU, allocate CPU memory for the vertex data, copy the vertex data
    // into the GPU buffer, and then unmap the CPU buffer (bc its useless now).
    // We promise that the two buffers (when mapped into CPU memory) don't overlap
    unsafe {
        data.vertex_buffer_memory = device.allocate_memory(&memory_info, None).unwrap();
        device.bind_buffer_memory(data.vertex_buffer, data.vertex_buffer_memory, 0).unwrap();
        let memory = device.map_memory(
            data.vertex_buffer_memory,
            0,
            buffer_info.size,
            vk::MemoryMapFlags::empty()
        ).unwrap();
        std::ptr::copy_nonoverlapping(VERTICES.as_ptr(), memory.cast(), VERTICES.len());
        device.unmap_memory(data.vertex_buffer_memory);
    }
}

/// Maps the memory in AppData.vertex_buffer_memory to the CPU and returns the pointer to it.
/// Safety: this pointer must not be written to after [unmap_memory] is called. No more than
/// the returned usize bytes may be written to it.
unsafe fn map_memory(device: &Device, data: &AppData) -> (*mut c_void, NumBytes) {
    match device.map_memory(
        data.vertex_buffer_memory,
        0,
        (std::mem::size_of::<Vertex>() * 3) as u64,
        vk::MemoryMapFlags::empty(),
    ) {
        Ok(p) => (p, std::mem::size_of::<Vertex>() * 3),
        Err(_e) => panic!("Couldn't map memory in shiv {_e:?}"),
    }
}
/// Unmaps the given data.vertex_buffer_memory from the CPU. Once this function is called,
unsafe fn unmap_memory(device: &Device, data: &AppData) {
    device.unmap_memory(data.vertex_buffer_memory);
}

const VALIDATION_LAYER: vk::ExtensionName = vk::ExtensionName::from_bytes(b"VK_LAYER_KHRONOS_validation");
const VALIDATION_ENABLED: bool = cfg!(debug_assertions);

/* Initialization work for */
/// Create a new driver instance to work with for the program.
///
/// Safety: must provide a valid [Entry] handle
unsafe fn create_instance(window: &Window, entry: &Entry) -> Instance {
    let app_info = vk::ApplicationInfo::builder()
        // names are what ever we want
        .application_name(b"force-system-solver")
        .engine_name(b"No Engine\0")
        // same with our versions
        .application_version(vk::make_version(0, 1, 0))
        .engine_version(vk::make_version(0, 1, 0))
        // this must be min api version of Vulkan
        .api_version(vk::make_version(1, 0, 0));
    // Safety: these aren't documented beyond having a valid instance so go wild
    let available_layers: HashSet<_> =
        unsafe { entry.enumerate_instance_layer_properties().unwrap() }
        .iter()
        .map(|l| l.layer_name)
        .collect();
    if VALIDATION_ENABLED && !available_layers.contains(&VALIDATION_LAYER) {
        eprintln!("Unable to create validation layer at {}, {}", file!(), line!());
        panic!("Validation layer requested but not available!");
    }
    let mut layers: Vec<*const i8> = vec![];
    if VALIDATION_ENABLED {
        layers.push(VALIDATION_LAYER.as_ptr());
    }

    let extensions: Vec<_> = vk_window::get_required_instance_extensions(window)
        .iter()
        .map(|ext| ext.as_ptr())
        .collect();
    #[cfg(target_os = "macos")]
    compile_error!("Required Vulkan compat extensions for macOS are not implemented in this crate!");
    let flags = vk::InstanceCreateFlags::empty();

    let info = vk::InstanceCreateInfo::builder()
        .application_info(&app_info)
        .enabled_extension_names(&extensions)
        .enabled_layer_names(&layers)
        .flags(flags);
    // not really sure how to ensure this is safe? doesn't say where it's not
    unsafe { entry.create_instance(&info, None).unwrap() }
}

/// Create a swapchain for the application.
///
/// This function will panic on any error returned from the Vulkan API
fn create_swapchain(window: &Window, inst: &Instance, device: &Device, data: &mut AppData) {
    assert!(!inst.handle().is_null());
    let indices = QueueFamilyIndices::get(inst, &data.surface, &data.physical_device).unwrap();
    let support = SwapchainSupport::get(inst, &data.surface, data.physical_device).unwrap();
    assert!(!support.formats.is_empty());
    assert!(!support.present_mode.is_empty());

    // settings for our swapchain; we can change to PresentModeKHR::MAILBOX for better perf at high
    // frame rates, but I don't think its necessary
    let surface_format = support.formats.iter()
        .find(|f| f.format == vk::Format::B8G8R8A8_SRGB && f.color_space == vk::ColorSpaceKHR::SRGB_NONLINEAR)
        .unwrap_or_else(|| {
            eprintln!("No ideal surface format found, using the first available!");
            &support.formats[0]
        });
    let present_mode = support.present_mode.iter()
        .find(|p_m| **p_m == vk::PresentModeKHR::FIFO)
        .unwrap_or_else(|| {
            eprintln!("No Ideal present mode found, using the first available!");
            &support.present_mode[0]
        });
    let extent = if support.capabilities.current_extent.width != u32::MAX {
        support.capabilities.current_extent
    } else {
        let (min, max) = (support.capabilities.min_image_extent, support.capabilities.max_image_extent);
        let width = window.inner_size().width.clamp(min.width, max.width);
        let height = window.inner_size().height.clamp(min.height, max.height);
        vk::Extent2D{
            width,
            height
        }
    };
    let image_count = {
        let i = support.capabilities.min_image_count + 1;
        let max = support.capabilities.max_image_count; // 0 indicates no max
        if max != 0 && max < i { max } else { i }
    };

    // information about where the queues for graphics & presenting commands comes from
    let mut queue_fam_indices = Vec::new();
    if indices.graphics != indices.present {
        queue_fam_indices.push(indices.graphics);
        queue_fam_indices.push(indices.present);
    }
    let sharing_mode = if queue_fam_indices.is_empty() {
        vk::SharingMode::EXCLUSIVE
    } else {
        vk::SharingMode::CONCURRENT
    };

    let info = vk::SwapchainCreateInfoKHR::builder()
        .surface(data.surface)
        .min_image_count(image_count)
        .image_format(surface_format.format)
        .image_color_space(surface_format.color_space)
        .image_extent(extent)
        .image_array_layers(1)
        .image_usage(vk::ImageUsageFlags::COLOR_ATTACHMENT)
        .image_sharing_mode(sharing_mode)
        .queue_family_indices(&queue_fam_indices)
        .pre_transform(support.capabilities.current_transform)
        .composite_alpha(vk::CompositeAlphaFlagsKHR::OPAQUE)
        .present_mode(*present_mode)
        .clipped(true)
        .old_swapchain(SwapchainKHR::null());

    // Safety: As long as everything is a valid handle; it should be fine. This crate does not
    // have anything else documented
    assert!(!device.handle().is_null());
    unsafe {
        data.swapchain = device.create_swapchain_khr(&info, None).unwrap();
        data.swapchain_images = device.get_swapchain_images_khr(data.swapchain).unwrap();
    }
    data.swapchain_format = surface_format.format;
    data.swapchain_extent = extent;
}

fn create_swapchain_image_views(device: &Device, data: &mut AppData) {
    let views : Vec<vk::ImageView> = data.swapchain_images.iter()
        .map(|i| {
            // allows for swapping color channels (which we don't do)
            let components = vk::ComponentMapping::builder()
                .r(vk::ComponentSwizzle::IDENTITY)
                .g(vk::ComponentSwizzle::IDENTITY)
                .b(vk::ComponentSwizzle::IDENTITY)
                .a(vk::ComponentSwizzle::IDENTITY);

            // "purpose of the image and which part of the image should be accessed"
            // https://kylemayes.github.io/vulkanalia/presentation/image_views.html
            let subresource_range = vk::ImageSubresourceRange::builder()
                .aspect_mask(vk::ImageAspectFlags::COLOR)
                .base_mip_level(0)
                .level_count(1)
                .base_array_layer(0)
                .layer_count(1);

            // put together the info for how to create
            let info = vk::ImageViewCreateInfo::builder()
                .image(*i)
                .view_type(vk::ImageViewType::_2D)
                .format(data.swapchain_format)
                .components(components)
                .subresource_range(subresource_range);
            unsafe {
                device.create_image_view(&info, None).unwrap()
            }
        })
        .collect();
    data.swapchain_image_views = views;
}

fn create_render_pass(_i: &Instance, device: &Device, data: &mut AppData) {
    let color_attachment = vk::AttachmentDescription::builder()
        .format(data.swapchain_format)
        .samples(vk::SampleCountFlags::_1)
        .load_op(vk::AttachmentLoadOp::CLEAR)
        .store_op(vk::AttachmentStoreOp::STORE)
        .stencil_load_op(vk::AttachmentLoadOp::DONT_CARE)
        .stencil_store_op(vk::AttachmentStoreOp::DONT_CARE)
        // how the image is laid out in memory before and after the render pass
        .initial_layout(vk::ImageLayout::UNDEFINED)
        .final_layout(vk::ImageLayout::PRESENT_SRC_KHR);
    let color_attachment_ref = vk::AttachmentReference::builder()
        .attachment(0)
        .layout(vk::ImageLayout::COLOR_ATTACHMENT_OPTIMAL);
    let color_attachments = &[color_attachment_ref];
    let subpass = vk::SubpassDescription::builder()
        .pipeline_bind_point(vk::PipelineBindPoint::GRAPHICS)
        .color_attachments(color_attachments);

    // deal with the initial, secret subpass
    let dependency = vk::SubpassDependency::builder()
        .src_subpass(vk::SUBPASS_EXTERNAL)
        .dst_subpass(0)
        .src_stage_mask(vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)
        .src_access_mask(vk::AccessFlags::empty())
        .dst_stage_mask(vk::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)
        .dst_access_mask(vk::AccessFlags::COLOR_ATTACHMENT_WRITE);

    let attachments = &[color_attachment];
    let subpasses = &[subpass];
    let dependencies = &[dependency];
    let info =  vk::RenderPassCreateInfo::builder()
        .attachments(attachments)
        .subpasses(subpasses)
        .dependencies(dependencies);
    // we can't check validity, but can check non-null
    assert!(!device.handle().is_null());
    unsafe {
        data.render_pass = device.create_render_pass(&info, None).unwrap();
    }
}

fn create_pipeline(device: &Device, data: &mut AppData) {
    let vert = include_bytes!(r"shaders\vert.spv");
    let frag = include_bytes!(r"shaders\frag.spv");

    let vert_module = create_shader_module(device, vert).unwrap();
    let frag_module = create_shader_module(device, frag).unwrap();
    let vert_stage = vk::PipelineShaderStageCreateInfo::builder()
        .stage(vk::ShaderStageFlags::VERTEX)
        .module(vert_module)
        .name(b"main\0");
    let frag_stage = vk::PipelineShaderStageCreateInfo::builder()
        .stage(vk::ShaderStageFlags::FRAGMENT)
        .module(frag_module)
        .name(b"main\0");

    // format for how the vertex info will be sent / packaged
    let desc = [Vertex::binding_description()];
    let attrib = Vertex::attribute_descriptions();
    let vertex_input_state = vk::PipelineVertexInputStateCreateInfo::builder()
            .vertex_binding_descriptions(&desc)
            .vertex_attribute_descriptions(&attrib);

    let input_assembly_state = vk::PipelineInputAssemblyStateCreateInfo::builder()
        .topology(vk::PrimitiveTopology::TRIANGLE_LIST)
        .primitive_restart_enable(false);
    let viewport = vk::Viewport::builder()
        .x(0.0)
        .y(0.0)
        .width(data.swapchain_extent.width as f32)
        .height(data.swapchain_extent.height as f32)
        .min_depth(0.0)
        .max_depth(1.0);
    let scissor = vk::Rect2D::builder()
        .offset(vk::Offset2D { x: 0, y: 0 })
        .extent(data.swapchain_extent);
    let viewports = &[viewport];
    let scissors = &[scissor];
    let viewport_state = vk::PipelineViewportStateCreateInfo::builder()
        .viewports(viewports)
        .scissors(scissors);

    let rasterization_state = vk::PipelineRasterizationStateCreateInfo::builder()
        .depth_clamp_enable(false)
        .rasterizer_discard_enable(false)
        .polygon_mode(vk::PolygonMode::FILL) // we don't just want a wireframe
        .line_width(1.0)
        .cull_mode(vk::CullModeFlags::BACK)
        .front_face(vk::FrontFace::CLOCKWISE)
        .depth_bias_enable(false);
    let multisample_state = vk::PipelineMultisampleStateCreateInfo::builder()
        .sample_shading_enable(false)
        .rasterization_samples(vk::SampleCountFlags::_1);
    let attachment = vk::PipelineColorBlendAttachmentState::builder()
        .color_write_mask(vk::ColorComponentFlags::all())
        .blend_enable(false);
    let attachments = &[attachment];
    let color_blend_state = vk::PipelineColorBlendStateCreateInfo::builder()
        .logic_op_enable(false)
        .logic_op(vk::LogicOp::COPY)
        .attachments(attachments)
        .blend_constants([0.0, 0.0, 0.0, 0.0]);

    let layout_info = vk::PipelineLayoutCreateInfo::builder();
    data.pipeline_layout = unsafe { device.create_pipeline_layout(&layout_info, None).unwrap() };

    let stages = &[vert_stage, frag_stage];
    let info = vk::GraphicsPipelineCreateInfo::builder()
        // programmable stages
        .stages(stages)
        // all of the fixed-function settings
        .vertex_input_state(&vertex_input_state)
        .input_assembly_state(&input_assembly_state)
        .viewport_state(&viewport_state)
        .rasterization_state(&rasterization_state)
        .multisample_state(&multisample_state)
        .color_blend_state(&color_blend_state)
        // other stuff
        .layout(data.pipeline_layout)
        .render_pass(data.render_pass)
        .subpass(0);

    assert!(!device.handle().is_null());
    unsafe {
        data.pipeline = device.create_graphics_pipelines(
            vk::PipelineCache::null(),
            &[info],
            None
        ).unwrap().0[0];
        // these get loaded when the pipeline is created, so we don't need them past here.
        // they get created in create_shader_module()
        device.destroy_shader_module(vert_module, None);
        device.destroy_shader_module(frag_module, None);
    }
}

fn create_frame_buffers(device: &Device, data: &mut AppData) {
    let mut v = Vec::with_capacity(data.swapchain_image_views.len());
    for view in &data.swapchain_image_views {
        let attachments = &[*view];
        let create_info = vk::FramebufferCreateInfo::builder()
            .render_pass(data.render_pass)
            .attachments(attachments)
            .width(data.swapchain_extent.width)
            .height(data.swapchain_extent.height)
            .layers(1);
        unsafe {
            let buffer = device.create_framebuffer(&create_info, None).unwrap();
            v.push(buffer);
        }
    }
    data.frame_buffers = v;
}

fn create_command_pool(inst: &Instance, device: &Device, data: &mut AppData) {
    let indices = QueueFamilyIndices::get(inst, &data.surface, &data.physical_device).unwrap();
    let info = vk::CommandPoolCreateInfo::builder()
        .flags(vk::CommandPoolCreateFlags::empty())
        .queue_family_index(indices.graphics);

    assert!(!device.handle().is_null());
    unsafe {
        data.command_pool = device.create_command_pool(&info, None).unwrap()
    }
}

/// Create a command buffer for each frame buffer & write the commands needed to render the static
/// 3 vertices we have so far.
fn create_command_buffers(device: &Device, data: &mut AppData) {
    let allocate_info = vk::CommandBufferAllocateInfo::builder()
        .command_pool(data.command_pool)
        .level(vk::CommandBufferLevel::PRIMARY)
        .command_buffer_count(data.frame_buffers.len() as u32);
    unsafe {
        data.command_buffers = device.allocate_command_buffers(&allocate_info).unwrap();
    }

    for (idx, c_buffer) in data.command_buffers.iter().enumerate() {
        let inheritance = vk::CommandBufferInheritanceInfo::builder();
        let info = vk::CommandBufferBeginInfo::builder()
            .flags(vk::CommandBufferUsageFlags::SIMULTANEOUS_USE)
            .inheritance_info(&inheritance);
        // start writing our commands
        unsafe {
            device.begin_command_buffer(*c_buffer, &info).unwrap();
        }
        // last bit of setup
        let render_area = vk::Rect2D::builder()
            .offset(vk::Offset2D::default())
            .extent(data.swapchain_extent);
        let color_clear_value = vk::ClearValue {
            color: vk::ClearColorValue {
                float32: [0.0, 0.0, 0.0, 1.0],
            },
        };
        let clear_values = &[color_clear_value];
        let info = vk::RenderPassBeginInfo::builder()
            .render_pass(data.render_pass)
            .framebuffer(data.frame_buffers[idx])
            .render_area(render_area)
            .clear_values(clear_values);
        // record our commands & then stop recording
        unsafe {
            device.cmd_begin_render_pass(*c_buffer, &info, vk::SubpassContents::INLINE);
            device.cmd_bind_pipeline(*c_buffer, vk::PipelineBindPoint::GRAPHICS, data.pipeline);
            device.cmd_bind_vertex_buffers(*c_buffer, 0, &[data.vertex_buffer], &[0]);
            device.cmd_draw(*c_buffer, VERTICES.len() as u32, 1, 0, 0);
            device.cmd_end_render_pass(*c_buffer);
            device.end_command_buffer(*c_buffer).unwrap();
        }
    }
}

fn create_sync_objects(device: &Device, data: &mut AppData) {
    let semaphore_info = vk::SemaphoreCreateInfo::builder();
    let fence_info = vk::FenceCreateInfo::builder()
        // need these to be signaled, so rendering doesn't block forever on the first frame
        .flags(vk::FenceCreateFlags::SIGNALED);

    for _ in 0..MAX_FRAMES_IN_FLIGHT {
        let avail = &mut data.image_available_semaphores;
        let fin = &mut data.render_finished_semaphores;
        let in_flight = &mut data.in_flight_fences;

        unsafe {
            avail.push(device.create_semaphore(&semaphore_info, None).unwrap());
            fin.push(device.create_semaphore(&semaphore_info, None).unwrap());
            in_flight.push(device.create_fence(&fence_info, None).unwrap());
        }
    }
    data.images_in_flight = data.swapchain_images
        .iter()
        .map(|_| vk::Fence::null())
        .collect();
}

/// The required extensions that need to be supported for this program to properly run
const DEVICE_EXTENSIONS: &[vk::ExtensionName] = &[vk::KHR_SWAPCHAIN_EXTENSION.name];

/* Device (physical & virtual) creation and other setup */
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum DeviceSelectionError {
    MissingQueueFamilies,
    MissingExtensions,
    InsufficientSwapchain,
    VulkanError(vk::ErrorCode),
}
impl From<vk::ErrorCode> for DeviceSelectionError {
    fn from(value: vk::ErrorCode) -> Self {
        DeviceSelectionError::VulkanError(value)
    }
}

/// Pick a physical device and add it to the given [AppData] instance. Err(()) indicates that none
/// of the available devices work & the device updated in AppData is unhandled.
///
/// Safety: If this function returns Err(()), the physical device in the AppData instance must not
/// be used, or must be set another way
unsafe fn pick_physical_device(inst: &Instance, data: &mut AppData) -> Result<(), ()> {
    // Safety: Inst needs to have a valid handle; we can check non-null (idk how for validity)
    assert!(!inst.handle().is_null());

    for physical_device in unsafe { inst.enumerate_physical_devices().unwrap() } {
        let res = check_physical_device(inst, &data.surface, physical_device);
        if res.is_ok() {
            data.physical_device = physical_device;
            return Ok(());
        }

        // Safety: I'm assuming that this is just basically always safe. Physical device needs to
        // be a valid id, but I'm getting that from the instance itself, so
        let prop = unsafe { inst.get_physical_device_properties(physical_device) };
        eprintln!("Skipping physical device {} because {res:?}", prop.device_name);
        // cgf(debug) here would be high tier
    }
    Err(())
}

/// Check whether a given physical device meets the requirements to be used.
fn check_physical_device(
    inst: &Instance, surface: &vk::SurfaceKHR, dev: PhysicalDevice
) -> Result<(), DeviceSelectionError> {
    // make sure our device has the needed queues
    if QueueFamilyIndices::get(inst, surface, &dev).is_none() {
        return Err(DeviceSelectionError::MissingQueueFamilies);
    };

    // check to ensure that the device has all required extensions (like the ability to draw
    // to the swapchain we will use)
    let device_extensions: HashSet<_> =
        // Safety: ensure that the instance handle is valid
        unsafe { inst.enumerate_device_extension_properties(dev, None)? }
        .iter()
        .map(|e| e.extension_name)
        .collect();
    if DEVICE_EXTENSIONS.iter().any(|ext| !device_extensions.contains(ext)) {
        return Err(DeviceSelectionError::MissingExtensions)
    }

    // next, see if the supported swapchain meets our needs
    let support = SwapchainSupport::get(inst, surface, dev).unwrap();
    if support.formats.is_empty() || support.present_mode.is_empty() {
        return Err(DeviceSelectionError::InsufficientSwapchain)
    }

    Ok(())
}

/// Create a logical device with the necessary extensions (same as the instance) to use.
///
/// the _e is unused, but if I ever make this Vulkan-compliant for compiling to macOS, it needs to
/// be part of the function, so I'm adding it to the signature now
fn create_logical_device(_e: &Entry, inst: &Instance, data: &mut AppData) -> Device {
    let indices = QueueFamilyIndices::get(inst, &data.surface, &data.physical_device).unwrap();
    let unique_indices = indices.to_set();
    let queue_priorities = &[1.0];
    let queue_infos: Vec<_> = unique_indices
        .iter()
        .map(|i| vk::DeviceQueueCreateInfo::builder().queue_family_index(*i).queue_priorities(queue_priorities))
        .collect();
    let extensions: Vec<_> = DEVICE_EXTENSIONS
        .iter()
        .map(|n| n.as_ptr())
        .collect();
    #[cfg(target_os = "macos")]
    compile_error!("Logical device requires additional extensions for macOS compatibility");
    let mut layers = vec![];
    if VALIDATION_ENABLED {
        layers.push(VALIDATION_LAYER.as_ptr());
    }

    let features = vk::PhysicalDeviceFeatures::builder();
    let info = vk::DeviceCreateInfo::builder()
        .queue_create_infos(&queue_infos)
        .enabled_layer_names(&layers)
        .enabled_extension_names(&extensions)
        .enabled_features(&features);
    // Safety: we have a valid instance and device info, so idk where else the unsafety can be
    unsafe {
        let device =  inst.create_device(data.physical_device, &info, None).unwrap();
        data.graphics_queue = device.get_device_queue(indices.graphics, 0);
        data.present_queue = device.get_device_queue(indices.present, 0);
        device
    }
}


/* Common Helper functions */
fn create_shader_module(device: &Device, bytes: &[u8]) -> Result<vk::ShaderModule, vk::ErrorCode> {
    use vulkanalia::bytecode::Bytecode;
    let bytecode = Bytecode::new(bytes).unwrap();

    let info = vk::ShaderModuleCreateInfo::builder()
        .code_size(bytecode.code_size())
        .code(bytecode.code());
    // We have to trust that a non-null device handle is valid bc we have no way to check
    assert!(!device.handle().is_null());
    unsafe {
        device.create_shader_module(&info, None)
    }
}

/* Functions for higher-level rendering */
use parsing::Truss2D;
/// Convert a [Truss2D] into a vertex data & write it into the provided slice. This function will
/// not write more than max_size vertices into the buffer. If something goes wrong, the function
/// will return an error and nothing will be written to the provided slice. If max_size is larger
/// than buffer.len(), the function will immediately return.
fn build_vertex_data(_t: &Truss2D, buffer: &mut [Vertex], max_size: NumVertex) -> Result<NumVertex, ()> {
    if buffer.len() <= max_size || max_size == 0 {
        return Err(());
    }
    todo!()
}