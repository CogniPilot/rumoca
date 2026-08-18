use super::*;

pub(super) fn validate_workspace_target_commands(
    client: &mut LspStdioClient,
    sim_uri: &Url,
) -> Result<Vec<LspApiValidationEntry>> {
    let (targets_ms, targets_response) =
        execute_lsp_command(client, "rumoca.workspace.getBuiltinTargets", json!({}))?;
    let targets_result = response_result(&targets_response);
    let targets = targets_result
        .as_array()
        .context("getBuiltinTargets should return an array")?;
    ensure!(
        !targets.is_empty(),
        "getBuiltinTargets should return at least one target"
    );
    let dae_target = targets
        .iter()
        .find(|item| item.get("id").and_then(Value::as_str) == Some("dae-modelica"))
        .and_then(|item| item.get("id"))
        .and_then(Value::as_str)
        .context("getBuiltinTargets should include dae-modelica")?;

    let (render_ms, render_response) = execute_lsp_command(
        client,
        "rumoca.workspace.renderTarget",
        json!({
            "uri": sim_uri,
            "model": "Decay",
            "target": dae_target,
        }),
    )?;
    let render = response_result(&render_response);
    ensure!(
        render.get("ok") == Some(&Value::Bool(true)),
        "renderTarget should return ok"
    );
    ensure!(
        render
            .get("files")
            .and_then(Value::as_array)
            .is_some_and(|files| files.iter().any(|file| file
                .get("content")
                .and_then(Value::as_str)
                .is_some_and(|content| content.contains("class Decay")))),
        "renderTarget should render the requested model"
    );

    Ok(vec![
        ok_validation("exec:getTargets", "req", Some(targets_ms), "dae target"),
        ok_validation("exec:renderTarget", "req", Some(render_ms), "class Decay"),
    ])
}
