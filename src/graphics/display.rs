use winit::application::ApplicationHandler;
use winit::dpi::LogicalSize;
use winit::event::{StartCause, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop};
use winit::window::{Window, WindowAttributes, WindowId};

use crate::parsing::Truss2D;

mod render_backing;

#[derive(Debug)]
struct Program {
    minimized: bool,
    paused: bool,
    truss: Truss2D,
    drawing: Option<render_backing::VulkanApp>,
    window: Option<Window>,
}
impl ApplicationHandler for Program {
    fn new_events(&mut self, event_loop: &ActiveEventLoop, cause: StartCause) {
        if cause == StartCause::Init {
            let attrib = WindowAttributes::default()
                .with_title("force-system-solver-debug")
                .with_inner_size(LogicalSize::new(1024, 768));
            let window = event_loop.create_window(attrib).unwrap();
            self.window = Some(window);
        }
    }
    fn resumed(&mut self, _: &ActiveEventLoop) {
        // per docs: only create graphics stuff after 1st redraw event. If we have a window with
        // no vulkan, then make the vulkan, otherwise ignore. We also need a window to actually
        // create the vulkan context
        if self.drawing.is_none() && self.window.is_some() {
            let window = self.window.as_ref().unwrap();
            let app = render_backing::VulkanApp::create(window);
            self.drawing = Some(app);
        }
        self.paused = false;
    }
    fn window_event(&mut self, event_loop: &ActiveEventLoop, window_id: WindowId, event: WindowEvent) {
        if self.window.is_none() {
            return;
        }
        if self.window.as_ref().unwrap().id() != window_id {
            return;
        }
        match event {
            WindowEvent::RedrawRequested => {
                if self.drawing.is_none() || self.window.is_none() {
                    eprintln!("Error: received a redraw request w/o required fields;");
                    eprintln!("Drawing is {:?} and Window is {:?}", self.drawing, self.window);
                    return;
                }
                if !event_loop.exiting() && !self.minimized {
                    let app = self.drawing.as_mut().unwrap();
                    let window = self.window.as_ref().unwrap();
                    app.render(window, &self.truss);
                }
            },
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::Resized(s) => {
                if s.width == 0 || s.height == 0 {
                    self.minimized = true;
                    return;
                }
                self.minimized = false;
            }
            _ => {},
        }
    }
    fn suspended(&mut self, _: &ActiveEventLoop) {
        self.paused = true;
    }
    fn exiting(&mut self, _: &ActiveEventLoop) {
        if self.drawing.is_some() {
            let app = self.drawing.as_mut().unwrap();
            app.wait_device();
            unsafe { app.destroy(); }
        }
    }
}

/// Takes a given truss & runs the drawn display. This function returns once the opened window is
/// closed.
pub(crate) fn init(truss: Truss2D) {
    let mut program = Program {
        minimized: false,
        truss,
        drawing: None,
        window: None,
        paused: false,
    };

    let event_loop = EventLoop::new().unwrap();
    event_loop.run_app(&mut program).unwrap();
}