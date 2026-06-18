use super::*;

#[test]
fn workspace_effective_source_roots_uses_in_memory_workspace_config() {
    let root_config = [
        r#"source_roots = ["vendor/Modelica"]"#,
        "",
        r#"[source_root_scopes."examples/control"]"#,
        r#"source_roots = ["vendor/ControlLib"]"#,
    ]
    .join("\n");
    let sources = serde_json::json!({
        "rumoca-workspace.toml": root_config,
        "examples/control/rumoca-workspace.toml": r#"source_roots = ["../LocalLib"]"#,
        "examples/other/rumoca-workspace.toml": r#"source_roots = ["../OtherLib"]"#,
    })
    .to_string();

    let roots_json = workspace_effective_source_roots(&sources, "examples/control/PID.mo")
        .expect("workspace roots should resolve");
    let roots: Vec<String> = serde_json::from_str(&roots_json).expect("roots JSON should decode");

    assert_eq!(
        roots,
        vec!["vendor/Modelica", "vendor/ControlLib", "examples/LocalLib"],
    );
}
