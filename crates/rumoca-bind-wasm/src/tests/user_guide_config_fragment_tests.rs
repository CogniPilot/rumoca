//! Guide configuration excerpts use the actual checked config front doors.

use super::user_guide_sources::{GuideFence, markdown_fences};
use super::*;

const BATCH_CONTEXT: &str = r#"
[rumoca]
version = "1"
task = "simulate"
[model]
file = "../models/Ball.mo"
name = "Ball"
"#;

fn scenario_context(fence: &GuideFence) -> String {
    if fence.source.contains("[rumoca]") {
        return fence.source.clone();
    }
    if fence.source.contains("[model]") {
        return format!(
            "[rumoca]\nversion = \"1\"\ntask = \"simulate\"\n{}",
            fence.source
        );
    }
    if fence.source.trim_start().starts_with("source_roots") {
        return format!("{}\n{BATCH_CONTEXT}", fence.source);
    }
    format!("{BATCH_CONTEXT}\n{}", fence.source)
}

#[test]
fn user_guide_toml_fragments_parse_in_their_documented_configuration_context() {
    let fences: Vec<_> = markdown_fences()
        .into_iter()
        .filter(|f| f.language == "toml")
        .collect();
    assert_eq!(fences.len(), 16, "review new configuration excerpts");
    for fence in fences {
        let location = format!("{}:{}", fence.page.display(), fence.line);
        if fence.page.ends_with("tools/vscode.md") {
            let sources = serde_json::json!({"rumoca-workspace.toml": fence.source}).to_string();
            let roots = workspace_effective_source_roots(&sources, "Model.mo").unwrap();
            let roots: Vec<String> = serde_json::from_str(&roots).unwrap();
            assert_eq!(roots.len(), 2, "{location}");
            continue;
        }
        let source = scenario_context(&fence);
        let sources = serde_json::json!({"rumoca-scenario.toml": source}).to_string();
        let response = scenario_get_scenario_config_full(&sources, "rumoca-scenario.toml").unwrap();
        let response: serde_json::Value = serde_json::from_str(&response).unwrap();
        assert_eq!(response["ok"], true, "{location}: {response}");
        assert_eq!(response["config"]["rumoca"]["version"], "1", "{location}");
    }
}
