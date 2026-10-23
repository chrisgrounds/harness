mod app;
mod worker;

use rs_gui_client::protocol::DEFAULT_SOCKET;

fn main() -> eframe::Result {
    let args: Vec<_> = std::env::args().skip(1).collect();
    let path = match args.as_slice() {
        [] => DEFAULT_SOCKET.to_owned(),
        [help] if help == "--help" || help == "-h" => {
            println!(
                "Usage: rs-gui-client [SOCKET]\nDefault socket: {DEFAULT_SOCKET}\nEnter sends; Shift+Enter inserts a newline; Alt+Up/Down recalls input; /quit exits."
            );
            return Ok(());
        }
        [path] if !path.starts_with('-') => path.clone(),
        _ => {
            eprintln!("Usage: rs-gui-client [SOCKET]");
            std::process::exit(2);
        }
    };
    let options = eframe::NativeOptions {
        viewport: eframe::egui::ViewportBuilder::default()
            .with_inner_size([1120.0, 800.0])
            .with_min_inner_size([600.0, 480.0]),
        ..Default::default()
    };
    eframe::run_native(
        "Harness",
        options,
        Box::new(move |cc| Ok(Box::new(app::ChatApp::new(cc, path)))),
    )
}
