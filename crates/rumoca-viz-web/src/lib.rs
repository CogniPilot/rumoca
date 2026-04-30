//! Web visualization assets and HTML shell for Rumoca simulation reports.

pub mod sim_viewer_server;

pub use sim_viewer_server::start_viewer_server;

use serde_json::{Value, json};

const RESULTS_APP_JS: &str = include_str!("../web/results_app.js");
const RESULTS_APP_CSS: &str = include_str!("../web/results_app.css");
const VISUALIZATION_SHARED_JS: &str = include_str!("../web/visualization_shared.js");
pub const THREE_JS: &str = include_str!("../web/three.min.js");
pub const UPLOT_JS: &str = include_str!("../vendor/uplot.min.js");
pub const UPLOT_CSS: &str = include_str!("../vendor/uplot.min.css");
const RESULTS_RUNTIME_STYLE: &str = concat!(
    "html, body { height: 100%; margin: 0; }",
    "body { background: #1e1e1e; color: #d4d4d4; ",
    "font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", sans-serif; }",
    "#resultsRoot { height: 100%; }",
);
const RESULTS_STATE_STORAGE_HELPERS_JS: &str = r#"
const root = document.getElementById('resultsRoot');
const storageKey = ['rumoca-results', window.location.pathname || 'inline', modelName, 'state'].join(':');
function readStoredJson(key) {
  try {
    const raw = globalThis.localStorage ? globalThis.localStorage.getItem(key) : null;
    return raw ? JSON.parse(raw) : null;
  } catch (_) {
    return null;
  }
}
function writeStoredJson(key, value) {
  if (!globalThis.localStorage) return;
  try {
    globalThis.localStorage.setItem(key, JSON.stringify(value));
  } catch (_) {
    // ignore storage failures
  }
}
function cloneView(view) {
  return Object.assign({}, view || {}, {
    y: Array.isArray(view && view.y) ? [...view.y] : [],
    scatterSeries: Array.isArray(view && view.scatterSeries)
      ? view.scatterSeries.map((series) => Object.assign({}, series))
      : undefined,
  });
}
"#;
const RESULTS_READ_ONLY_BRIDGE_JS: &str = r#"
const persistedState = readStoredJson(storageKey) || {};
const bridge = {
  notify(message) {
    if (message) {
      console.info('[rumoca-results]', message);
    }
  },
  persistState(nextState) {
    writeStoredJson(storageKey, Object.assign({}, readStoredJson(storageKey) || {}, nextState || {}));
  },
};
"#;
const RESULTS_APP_MOUNT_JS: &str = r#"
const app = globalThis.RumocaResultsApp.createResultsApp({
  root,
  model: modelName,
  modelRef: { model: modelName },
  payload: runPayload,
  views: configuredViews,
  metrics: runMetrics,
  activeViewId: typeof persistedState.activeViewId === 'string' ? persistedState.activeViewId : undefined,
  bridge,
  allowViewEditing: false,
});
window.addEventListener('beforeunload', () => {
  if (app && typeof app.dispose === 'function') {
    app.dispose();
  }
});
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
    let head = build_results_html_head(&page_title);
    let body = build_results_html_body(&bootstrap_script);

    format!("<!DOCTYPE html>\n<html lang=\"en\">\n{head}\n{body}\n</html>")
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

fn build_results_html_head(page_title: &str) -> String {
    let page_title = escape_html(page_title);
    format!(
        "<head>\n\
         <meta charset=\"utf-8\">\n\
         <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n\
         <title>{page_title}</title>\n\
         <style>{UPLOT_CSS}</style>\n\
         <style>{RESULTS_APP_CSS}</style>\n\
         <style>{RESULTS_RUNTIME_STYLE}</style>\n\
         </head>"
    )
}

fn build_results_html_body(bootstrap_script: &str) -> String {
    format!(
        "<body>\n\
         <div id=\"resultsRoot\"></div>\n\
         <script>{THREE_JS}</script>\n\
         <script>{UPLOT_JS}</script>\n\
         <script>{VISUALIZATION_SHARED_JS}</script>\n\
         <script>{RESULTS_APP_JS}</script>\n\
         <script>\n{bootstrap_script}\n</script>\n\
         </body>"
    )
}

fn build_bootstrap_script(inline: &InlineJsonBindings) -> String {
    format!(
        "const modelName = {model_json};\n\
         const runPayload = {payload_json};\n\
         const configuredViews = {views_json};\n\
         const runMetrics = {metrics_json};\n\
         {RESULTS_STATE_STORAGE_HELPERS_JS}\n\
         {RESULTS_READ_ONLY_BRIDGE_JS}\n\
         {RESULTS_APP_MOUNT_JS}",
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

        assert!(html.contains("RumocaResultsApp.createResultsApp"));
        assert!(html.contains("window.location.pathname"));
        assert!(html.contains("defaultThreeDimensionalViewerScript"));
        assert!(html.contains("allowViewEditing: false"));
        assert!(!html.contains("saveViews(_ignored"));
    }
}
