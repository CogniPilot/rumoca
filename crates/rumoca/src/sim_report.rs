use std::fs;
use std::io;
use std::path::Path;

use rumoca_session::project::{PlotViewConfig, load_plot_views_for_model};
use rumoca_sim::{
    SimResult, SimulationRequestSummary, SimulationRunMetrics, build_simulation_metrics_value,
    build_simulation_payload,
};
use rumoca_viz_web::{
    ResultsHtmlDocument, build_results_html_document, default_visualization_views_value,
};
use serde_json::{Value, json};

pub(crate) struct SimulationReportDocument {
    pub(crate) html: String,
    pub(crate) payload: Value,
    pub(crate) views: Value,
    pub(crate) metrics: Value,
}

fn build_report_document(
    result: &SimResult,
    model_name: &str,
    request: &SimulationRequestSummary,
    metrics: &SimulationRunMetrics,
    workspace_root: Option<&Path>,
) -> io::Result<SimulationReportDocument> {
    let metrics_value = build_simulation_metrics_value(result, metrics);
    let payload = build_simulation_payload(result, request, metrics);
    let views = load_views_value(model_name, workspace_root)?;
    let html = build_results_html_document(ResultsHtmlDocument {
        model_name,
        payload: &payload,
        views: &views,
        metrics: &metrics_value,
        title: None,
    });
    Ok(SimulationReportDocument {
        html,
        payload,
        views,
        metrics: metrics_value,
    })
}

pub(crate) fn write_html_report(
    result: &SimResult,
    model_name: &str,
    path: &Path,
    request: &SimulationRequestSummary,
    metrics: &SimulationRunMetrics,
    workspace_root: Option<&Path>,
) -> io::Result<SimulationReportDocument> {
    let document = build_report_document(result, model_name, request, metrics, workspace_root)?;
    fs::write(path, &document.html)?;
    Ok(document)
}

fn load_views_value(model_name: &str, workspace_root: Option<&Path>) -> io::Result<Value> {
    let Some(workspace_root) = workspace_root else {
        return Ok(default_visualization_views_value());
    };
    let views = load_plot_views_for_model(workspace_root, model_name)
        .map_err(|error| io::Error::other(error.to_string()))?;
    if views.is_empty() {
        return Ok(default_visualization_views_value());
    }
    Ok(Value::Array(
        views
            .into_iter()
            .map(|view| hydrate_plot_view(workspace_root, view))
            .collect(),
    ))
}

fn hydrate_plot_view(workspace_root: &Path, view: PlotViewConfig) -> Value {
    let mut encoded = json!({
        "id": view.id,
        "title": view.title,
        "type": view.view_type,
        "x": view.x,
        "y": view.y,
        "scriptPath": view.script_path,
    });
    if let Some(script) = view.script {
        encoded["script"] = Value::String(script);
    } else if let Some(script_path) = encoded["scriptPath"].as_str() {
        let script_fs_path = if Path::new(script_path).is_absolute() {
            Path::new(script_path).to_path_buf()
        } else {
            workspace_root.join(script_path)
        };
        if let Ok(script) = fs::read_to_string(&script_fs_path) {
            encoded["script"] = Value::String(script);
        }
    }
    encoded
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_sim::SimVariableMeta;

    #[test]
    fn builds_shared_report_document() {
        let sim = SimResult {
            times: vec![0.0, 1.0],
            names: vec!["x".to_string()],
            data: vec![vec![1.0, 2.0]],
            n_states: 1,
            variable_meta: vec![SimVariableMeta {
                name: "x".to_string(),
                role: "state".to_string(),
                is_state: true,
                value_type: None,
                variability: None,
                time_domain: None,
                unit: None,
                start: None,
                min: None,
                max: None,
                nominal: None,
                fixed: None,
                description: None,
            }],
        };
        let request = SimulationRequestSummary {
            solver: "rk4".to_string(),
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.1),
            rtol: 1e-4,
            atol: 1e-6,
        };
        let metrics = SimulationRunMetrics::default();
        let document =
            build_report_document(&sim, "Ball", &request, &metrics, None).expect("report");
        assert!(document.html.contains("RumocaResultsApp.createResultsApp"));
        assert_eq!(document.payload["nStates"], 1);
        assert_eq!(document.views[0]["id"], "states_time");
    }
}
