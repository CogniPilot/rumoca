//! HTTP server for the live simulation viewer.
//!
//! Serves:
//! - `GET /`         → `viewer.html` (Three.js shell), with `__WS_PORT__`
//!   and `__DEBUG__` substituted
//! - `GET /scene.js` → the caller-supplied scene script (vehicle-specific),
//!   or a minimal empty-scene placeholder if none given

use std::path::{Component, Path, PathBuf};

use anyhow::{Context, Result};
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
    scene_asset_dir: Option<&Path>,
    viewer_config: &serde_json::Value,
    debug: bool,
) -> Result<()> {
    let server = Server::http(format!("0.0.0.0:{port}"))
        .map_err(|e| anyhow::anyhow!("Failed to start HTTP server on port {port}: {e}"))?;

    let rendered_html = VIEWER_HTML
        .replace("__WS_PORT__", &ws_port.to_string())
        .replace("__VIEWER_CONFIG__", &serde_json::to_string(viewer_config)?)
        .replace("__DEBUG__", if debug { "true" } else { "false" });
    let scene_js = scene_script.unwrap_or(PLACEHOLDER_SCENE);

    for request in server.incoming_requests() {
        let response = match request.url() {
            "/" => Response::from_string(rendered_html.clone())
                .with_header(header("Content-Type", "text/html; charset=utf-8")),
            "/scene.js" => Response::from_string(scene_js)
                .with_header(header("Content-Type", "application/javascript")),
            url if url.starts_with("/assets/") => match asset_response(scene_asset_dir, url) {
                Ok(response) => response,
                Err(error) => Response::from_string(error.to_string()).with_status_code(404),
            },
            _ => Response::from_string("Not found").with_status_code(404),
        };
        let _ = request.respond(response);
    }
    Ok(())
}

fn asset_response(
    scene_asset_dir: Option<&Path>,
    url: &str,
) -> Result<Response<std::io::Cursor<Vec<u8>>>> {
    let Some(asset_dir) = scene_asset_dir else {
        anyhow::bail!("No scene asset directory configured");
    };
    let relative = url
        .strip_prefix("/assets/")
        .expect("asset URL prefix checked by caller");
    let path = checked_asset_path(asset_dir, relative)?;
    let bytes = std::fs::read(&path).with_context(|| format!("Read asset: {}", path.display()))?;
    let content_type = content_type_for_path(&path);
    Ok(Response::from_data(bytes).with_header(header("Content-Type", content_type)))
}

fn checked_asset_path(asset_dir: &Path, relative: &str) -> Result<PathBuf> {
    let mut path = asset_dir.to_path_buf();
    for component in Path::new(relative).components() {
        match component {
            Component::Normal(part) => path.push(part),
            _ => anyhow::bail!("Invalid asset path"),
        }
    }
    Ok(path)
}

fn content_type_for_path(path: &Path) -> &'static str {
    match path.extension().and_then(|ext| ext.to_str()) {
        Some("glb") => "model/gltf-binary",
        Some("gltf") => "model/gltf+json",
        Some("bin") => "application/octet-stream",
        Some("png") => "image/png",
        Some("jpg") | Some("jpeg") => "image/jpeg",
        Some("webp") => "image/webp",
        Some("js") => "application/javascript",
        Some("css") => "text/css; charset=utf-8",
        _ => "application/octet-stream",
    }
}

fn header(name: &str, value: &str) -> Header {
    Header::from_bytes(name, value).expect("valid static header")
}
