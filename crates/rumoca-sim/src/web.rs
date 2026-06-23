//! Web report/viewer helpers that consume prepared package assets.
//!
//! Browser source, dependencies, and bundling live in `packages/rumoca-web`.
//! This Rust module never builds package assets; commands that need rich browser libraries
//! load the prepared vendor files at runtime.

use std::io;
use std::path::PathBuf;

use serde_json::{Value, json};

const FALLBACK_RESULTS_APP_CSS: &str = concat!(
    "#resultsRoot { height: 100%; overflow: auto; }",
    "pre { font: 12px/1.45 ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }",
);
const RESULTS_RUNTIME_STYLE: &str = concat!(
    "html, body { height: 100%; margin: 0; }",
    "body { background: #1e1e1e; color: #d4d4d4; ",
    "font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", sans-serif; }",
    "#resultsRoot { height: 100%; }",
);
const FALLBACK_RESULTS_REPORT_INLINE_JS: &str = r#"
globalThis.__rumocaResultsReportMount = function(options) {
  const root = options.root || document.body;
  const pre = document.createElement('pre');
  pre.style.margin = '0';
  pre.style.padding = '16px';
  pre.style.whiteSpace = 'pre-wrap';
  pre.textContent = JSON.stringify({
    model: options.model,
    payload: options.payload,
    views: options.views,
    metrics: options.metrics,
  }, null, 2);
  root.replaceChildren(pre);
};
"#;
#[cfg(feature = "runner")]
const FALLBACK_VIEWER_HTML: &str = r#"<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Rumoca SIL Viewer</title>
<style>
html, body { margin: 0; width: 100%; height: 100%; background: #111; color: #eee; font-family: monospace; }
#container { width: 100vw; height: 100vh; }
#status { position: fixed; top: 12px; left: 12px; background: rgba(0,0,0,0.78); padding: 10px 12px; border-radius: 4px; }
</style>
</head>
<body>
<div id="container"></div>
<div id="status">
  Missing Rumoca viewer shell. Prepare packages/rumoca-web assets in the
  checkout containing viz/viewer.html and vendor/three_viewer.js.
</div>
</body>
</html>
"#;

pub struct ResultsHtmlDocument<'a> {
    pub model_name: &'a str,
    pub payload: &'a Value,
    pub views: &'a Value,
    pub metrics: &'a Value,
    pub title: Option<&'a str>,
}

pub fn default_visualization_views_value() -> Value {
    json!([
        {
            "id": "states_time",
            "title": "States vs Time",
            "type": "timeseries",
            "x": "time",
            "y": ["*states"],
        }
    ])
}

pub fn build_results_html_document(document: ResultsHtmlDocument<'_>) -> String {
    let title = document.title.unwrap_or(document.model_name);
    let page_title = format!("{title} \u{2014} Rumoca Results");
    let inline = build_inline_json_bindings(document);
    let bootstrap_script = build_bootstrap_script(&inline);
    let report_js = read_optional_vendor_asset("results_report_inline.js")
        .unwrap_or_else(|| FALLBACK_RESULTS_REPORT_INLINE_JS.to_string());
    let uplot_css = read_optional_vendor_asset("uplot.min.css").unwrap_or_default();
    let results_app_css = read_optional_vendor_asset("results_app.css")
        .unwrap_or_else(|| FALLBACK_RESULTS_APP_CSS.to_string());
    let head = build_results_html_head(&page_title, &uplot_css, &results_app_css);
    let body = build_results_html_body(&report_js, &bootstrap_script);

    format!("<!DOCTYPE html>\n<html lang=\"en\">\n{head}\n{body}\n</html>")
}

pub fn uplot_css() -> io::Result<String> {
    read_required_vendor_asset("uplot.min.css")
}

pub fn uplot_js() -> io::Result<String> {
    read_required_vendor_asset("uplot_global.js")
}

pub fn three_js() -> io::Result<String> {
    read_required_vendor_asset("three_global.js")
}

fn read_required_vendor_asset(file_name: &str) -> io::Result<String> {
    let attempted = candidate_vendor_paths(file_name);
    for path in &attempted {
        match std::fs::read_to_string(path) {
            Ok(text) => return Ok(text),
            Err(error) if error.kind() == io::ErrorKind::NotFound => {}
            Err(error) => {
                return Err(io::Error::new(
                    error.kind(),
                    format!("read web vendor asset {}: {error}", path.display()),
                ));
            }
        }
    }
    Err(io::Error::new(
        io::ErrorKind::NotFound,
        format!(
            "missing web vendor asset {file_name}; prepare packages/rumoca-web vendor assets \
             in this checkout (searched: {})",
            attempted
                .iter()
                .map(|path| path.display().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ),
    ))
}

fn read_optional_vendor_asset(file_name: &str) -> Option<String> {
    read_required_vendor_asset(file_name).ok()
}

#[cfg(feature = "runner")]
fn read_optional_package_file(relative_path: &str) -> Option<String> {
    candidate_package_paths(relative_path)
        .into_iter()
        .find_map(|path| std::fs::read_to_string(path).ok())
}

fn candidate_vendor_paths(file_name: &str) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Ok(cwd) = std::env::current_dir() {
        dirs.push(cwd.join("packages/rumoca-web/vendor"));
    }
    dirs.push(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("packages/rumoca-web/vendor"),
    );
    dirs.into_iter().map(|dir| dir.join(file_name)).collect()
}

#[cfg(feature = "runner")]
fn candidate_package_paths(relative_path: &str) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Ok(cwd) = std::env::current_dir() {
        dirs.push(cwd.join("packages/rumoca-web"));
    }
    dirs.push(
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join("packages/rumoca-web"),
    );
    dirs.into_iter()
        .map(|dir| dir.join(relative_path))
        .collect()
}

struct InlineJsonBindings {
    model_json: String,
    payload_json: String,
    views_json: String,
    metrics_json: String,
}

fn build_inline_json_bindings(document: ResultsHtmlDocument<'_>) -> InlineJsonBindings {
    InlineJsonBindings {
        model_json: escape_inline_script_json(
            &serde_json::to_string(document.model_name).unwrap_or_default(),
        ),
        payload_json: escape_inline_script_json(&document.payload.to_string()),
        views_json: escape_inline_script_json(&document.views.to_string()),
        metrics_json: escape_inline_script_json(&document.metrics.to_string()),
    }
}

fn build_results_html_head(page_title: &str, uplot_css: &str, results_app_css: &str) -> String {
    let page_title = escape_html(page_title);
    format!(
        "<head>\n\
         <meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n\
         <title>{page_title}</title>\n\
         <style>{uplot_css}</style>\n\
         <style>{results_app_css}</style>\n\
         <style>{RESULTS_RUNTIME_STYLE}</style>\n\
         </head>"
    )
}

fn build_results_html_body(report_js: &str, bootstrap_script: &str) -> String {
    format!(
        "<body>\n\
         <div id=\"resultsRoot\"></div>\n\
         <script>\n{report_js}\n{bootstrap_script}\n</script>\n\
         </body>"
    )
}

fn build_bootstrap_script(inline: &InlineJsonBindings) -> String {
    format!(
        "globalThis.__rumocaResultsReportMount({{\n\
         root: document.getElementById('resultsRoot'),\n\
         model: {model_json},\n\
         payload: {payload_json},\n\
         views: {views_json},\n\
         metrics: {metrics_json},\n\
         }});",
        model_json = inline.model_json,
        payload_json = inline.payload_json,
        views_json = inline.views_json,
        metrics_json = inline.metrics_json,
    )
}

fn escape_html(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
}

fn escape_inline_script_json(text: &str) -> String {
    text.replace('&', "\\u0026")
        .replace('<', "\\u003c")
        .replace('>', "\\u003e")
        .replace('\u{2028}', "\\u2028")
        .replace('\u{2029}', "\\u2029")
}

#[cfg(feature = "runner")]
pub fn start_viewer_server(
    port: u16,
    ws_port: u16,
    scene_script: Option<&str>,
    scene_asset_dir: Option<&std::path::Path>,
    viewer_config: &serde_json::Value,
    debug: bool,
) -> anyhow::Result<()> {
    sim_viewer_server::start_viewer_server(
        port,
        ws_port,
        scene_script,
        scene_asset_dir,
        viewer_config,
        debug,
    )
}

#[cfg(feature = "runner")]
pub mod sim_viewer_server {
    use std::path::{Component, Path, PathBuf};

    use anyhow::{Context, Result};
    use tiny_http::{Header, Response, Server};

    const PLACEHOLDER_SCENE: &str = r#"
// No scene provided. Pass --scene <path> to render a vehicle-specific scene.
ctx.onInit = function(api) {
  const THREE = api.THREE;
  api.scene.add(new THREE.HemisphereLight(0xffffff, 0x444444, 1.0));
  api.scene.add(new THREE.GridHelper(10, 10));
};
ctx.onFrame = function() {};
"#;

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

        let viewer_html = viewer_shell_html();
        let rendered_html = viewer_html
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
                "/vendor/three_viewer.js" => viewer_bundle_response(),
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

    fn viewer_shell_html() -> String {
        viewer_shell_html_from_assets(
            super::read_optional_vendor_asset("three_viewer.js").is_some(),
            super::read_optional_package_file("viz/viewer.html"),
        )
    }

    fn viewer_shell_html_from_assets(
        has_three_viewer_bundle: bool,
        package_viewer_html: Option<String>,
    ) -> String {
        if !has_three_viewer_bundle {
            return super::FALLBACK_VIEWER_HTML.to_string();
        }
        package_viewer_html.unwrap_or_else(|| super::FALLBACK_VIEWER_HTML.to_string())
    }

    fn viewer_bundle_response() -> Response<std::io::Cursor<Vec<u8>>> {
        match super::read_required_vendor_asset("three_viewer.js") {
            Ok(script) => Response::from_string(script)
                .with_header(header("Content-Type", "application/javascript")),
            Err(error) => Response::from_string(error.to_string()).with_status_code(500),
        }
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
        let bytes =
            std::fs::read(&path).with_context(|| format!("Read asset: {}", path.display()))?;
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

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn viewer_shell_falls_back_when_three_viewer_bundle_is_missing() {
            let rich_html = r#"<script type="module" src="/vendor/three_viewer.js"></script>"#;

            let html = viewer_shell_html_from_assets(false, Some(rich_html.to_string()));

            assert!(html.contains("Missing Rumoca viewer shell"));
            assert!(!html.contains("/vendor/three_viewer.js"));
        }

        #[test]
        fn viewer_shell_uses_package_html_when_vendor_bundle_exists() {
            let rich_html = r#"<script type="module" src="/vendor/three_viewer.js"></script>"#;

            let html = viewer_shell_html_from_assets(true, Some(rich_html.to_string()));

            assert_eq!(html, rich_html);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builds_default_visualization_view() {
        let views = default_visualization_views_value();
        assert_eq!(views[0]["id"], "states_time");
        assert_eq!(views[0]["type"], "timeseries");
    }

    #[test]
    fn renders_shared_results_html_shell() {
        let html = build_results_html_document(ResultsHtmlDocument {
            model_name: "Ball",
            payload: &json!({ "version": 1, "names": ["x"], "allData": [[0.0], [1.0]], "nStates": 1, "variableMeta": [], "simDetails": {} }),
            views: &default_visualization_views_value(),
            metrics: &json!({ "simulateSeconds": 0.1, "points": 1, "variables": 1 }),
            title: None,
        });

        assert!(html.contains("__rumocaResultsReportMount"));
        assert!(html.contains("globalThis.__rumocaResultsReportMount({"));
        assert!(html.contains("model: \"Ball\""));
    }
}
