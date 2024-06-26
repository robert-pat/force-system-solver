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
        // Safety: TODO also not documented, assuming bc
        let entry = unsafe { Entry::new(loader).unwrap() };
        let instance = unsafe { create_instance(window, &entry).unwrap() };
        let mut data = AppData {
            surface: unsafe { vk_window::create_surface(&instance, &window, &window).unwrap() },
            ..Default::default()
        };

        // Safety: we panic if the function fails, so our physical device can never be invalid
        if unsafe { pick_physical_device(&instance, &mut data) }.is_err() {
            eprintln!("Issue selecting physical device at; {}, line: {}", file!(), line!());
            panic!("Unable to select a physical device: none worked!");
        }
        todo!()
    }
}

#[derive(Debug, Default)]
struct AppData{
    /// Handle to the physical device we've requested from the Vulkan driver
    physical_device: PhysicalDevice,
    /// The surface that we have
    surface: vk::SurfaceKHR,
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

/* Initialization work for */
/// Create a new driver instance to work with for the program.
///
/// Safety: TODO
unsafe fn create_instance(window: &Window, entry: &Entry) -> Result<Instance, ()> {
    todo!()
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