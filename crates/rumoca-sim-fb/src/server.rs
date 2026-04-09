//! HTTP server for the SIL viewer.
//!
//! Serves:
//! - `GET /`          → viewer.html
//! - `GET /scene.js`  → scene script (external file or embedded default)

use anyhow::Result;
use tiny_http::{Header, Response, Server};

/// The viewer HTML template (embedded at compile time).
const VIEWER_HTML: &str = include_str!("viewer.html");

/// The default quadrotor scene script (embedded at compile time).
const DEFAULT_SCENE: &str = include_str!("default_scene.js");

/// Start the HTTP server (blocks the calling thread).
pub fn start_http_server(
    port: u16,
    ws_port: u16,
    scene_script: Option<&str>,
    debug: bool,
) -> Result<()> {
    let server = Server::http(format!("0.0.0.0:{}", port))
        .map_err(|e| anyhow::anyhow!("Failed to start HTTP server on port {}: {}", port, e))?;

    // Inject WS port and debug flag into viewer HTML
    let rendered_html = VIEWER_HTML
        .replace("__WS_PORT__", &ws_port.to_string())
        .replace("__DEBUG__", if debug { "true" } else { "false" });
    let scene_js = scene_script.unwrap_or(DEFAULT_SCENE);

    for request in server.incoming_requests() {
        let url = request.url().to_string();

        match url.as_str() {
            "/" => {
                let response = Response::from_string(&rendered_html)
                    .with_header(content_type("text/html; charset=utf-8"));
                let _ = request.respond(response);
            }
            "/scene.js" => {
                let response = Response::from_string(scene_js)
                    .with_header(content_type("application/javascript"));
                let _ = request.respond(response);
            }
            _ => {
                let _ = request.respond(Response::from_string("Not found").with_status_code(404));
            }
        }
    }

    Ok(())
}

/// Create a Content-Type header.
fn content_type(mime: &str) -> Header {
    Header::from_bytes("Content-Type", mime).unwrap()
}
