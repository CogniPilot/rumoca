//! HTTP server for the live simulation viewer.
//!
//! Serves:
//! - `GET /`         → `viewer.html` (Three.js shell), with `__WS_PORT__`
//!   and `__DEBUG__` substituted
//! - `GET /scene.js` → the caller-supplied scene script (vehicle-specific),
//!   or a minimal empty-scene placeholder if none given

use anyhow::Result;
use tiny_http::{Header, Response, Server};

const VIEWER_HTML: &str = include_str!("../web/viewer.html");
const PLACEHOLDER_SCENE: &str = r#"
// No scene provided. Pass --scene <path> to render a vehicle-specific scene.
ctx.onInit = function(api) {
  const THREE = api.THREE;
  api.scene.add(new THREE.HemisphereLight(0xffffff, 0x444444, 1.0));
  api.scene.add(new THREE.GridHelper(10, 10));
};
ctx.onFrame = function() {};
"#;

/// Start the viewer HTTP server (blocks the calling thread). `scene_script`
/// is the text of a JS scene file; pass `None` for a minimal placeholder.
pub fn start_viewer_server(
    port: u16,
    ws_port: u16,
    scene_script: Option<&str>,
    debug: bool,
) -> Result<()> {
    let server = Server::http(format!("0.0.0.0:{port}"))
        .map_err(|e| anyhow::anyhow!("Failed to start HTTP server on port {port}: {e}"))?;

    let rendered_html = VIEWER_HTML
        .replace("__WS_PORT__", &ws_port.to_string())
        .replace("__DEBUG__", if debug { "true" } else { "false" });
    let scene_js = scene_script.unwrap_or(PLACEHOLDER_SCENE);

    for request in server.incoming_requests() {
        let response = match request.url() {
            "/" => Response::from_string(rendered_html.clone())
                .with_header(header("Content-Type", "text/html; charset=utf-8")),
            "/scene.js" => Response::from_string(scene_js)
                .with_header(header("Content-Type", "application/javascript")),
            _ => Response::from_string("Not found").with_status_code(404),
        };
        let _ = request.respond(response);
    }
    Ok(())
}

fn header(name: &str, value: &str) -> Header {
    Header::from_bytes(name, value).expect("valid static header")
}
