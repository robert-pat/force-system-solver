#![allow(dead_code)]

use std::collections::HashSet;
use vulkanalia::loader::{LibloadingLoader, LIBRARY};
use vulkanalia::prelude::v1_0::*;
use vulkanalia::vk::{KhrSurfaceExtension, PhysicalDevice};
use vulkanalia::vk::KhrSwapchainExtension;
use vulkanalia::window as vk_window;
use vulkanalia::Version;
use winit::window::Window;

struct VulkanApp {
    entry:Entry,
    instance: Instance,
    device: Device,
    data: AppData,
}
impl VulkanApp {
    /// Create a new VulkanApp w/ all of the backing needed. This function will panic if any issue
    /// with Vulkan occurs in the setup process.
    fn create(window: &Window) -> Self {
        // Safety: this function is unsafe bc it's calling foreign code;
        // need to be sure to call VulkanApp::destroy() eventually to free resources
        let loader = match unsafe { LibloadingLoader::new(LIBRARY) } {
            Ok(l) => l,
            Err(e) => panic!("Could not initialize library: {}", e),
        };
        // Safety: The requirements for entry are not helpful (TODO); the entry passed to
        // instance will be valid because we panic if creating a new entry fails
        let entry = unsafe { Entry::new(loader).unwrap() };
        let instance = unsafe { create_instance(window, &entry) };

        let mut data = AppData {
            // Safety: TODO, this one's not documented
            surface: unsafe { vk_window::create_surface(&instance, &window, &window).unwrap() },
            ..Default::default()
        };

        // Safety: we panic if the function fails, so our physical device can never be invalid
        if unsafe { pick_physical_device(&instance, &mut data) }.is_err() {
            eprintln!("Issue selecting physical device at; {}, line: {}", file!(), line!());
            panic!("Unable to select a physical device: none worked!");
        }

        let device = create_logical_device(&entry, &instance, &mut data);
        todo!()
    }
}

#[derive(Debug, Default)]
struct AppData{
    /// Handle to the physical device we've requested from the Vulkan driver
    physical_device: PhysicalDevice,
    /// The surface that we have
    surface: vk::SurfaceKHR,
    graphics_queue: vk::Queue,
    present_queue: vk::Queue,
}


/* Structs for holding more information about devices / more convenient way to filter them. */
struct QueueFamilyIndices {
    graphics: u32,
    present: u32,
}
impl QueueFamilyIndices {
    fn get(inst: &Instance, surface: &vk::SurfaceKHR, dev: &PhysicalDevice) -> Option<Self> {
        todo!()
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
    fn get(inst: &Instance, surface: &vk::SurfaceKHR, phys_device: PhysicalDevice) -> Self {
        todo!()
    }
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
    // Safety: TODO again, these aren't documented beyond having a valid instance
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
    // Safety: TODO aside from having a valid entry idk
    unsafe { entry.create_instance(&info, None).unwrap() }
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


unsafe fn create_swapchain(
    window: &Window, inst: &Instance, device: &Device, data: &mut AppData
) -> Result<(), ()> {
    todo!("Pick up here!")
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
    // Safety: TODO this is not documented so idk
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
        // Safety: TODO nothing about this call is documented
        unsafe { inst.enumerate_device_extension_properties(dev, None)? }
        .iter()
        .map(|e| e.extension_name)
        .collect();
    if DEVICE_EXTENSIONS.iter().any(|ext| !device_extensions.contains(ext)) {
        return Err(DeviceSelectionError::MissingExtensions)
    }

    // next, see if the supported swapchain meets our needs
    let support = SwapchainSupport::get(inst, surface, dev);
    if support.formats.is_empty() || support.present_mode.is_empty() {
        return Err(DeviceSelectionError::InsufficientSwapchain)
    }

    Ok(())
}