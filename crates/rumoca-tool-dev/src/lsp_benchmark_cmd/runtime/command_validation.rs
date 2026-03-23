use super::*;

pub(super) fn validate_workspace_template_commands(
    client: &mut LspStdioClient,
    sim_uri: &Url,
) -> Result<Vec<LspApiValidationEntry>> {
    let (templates_ms, templates_response) =
        execute_lsp_command(client, "rumoca.workspace.getBuiltinTemplates", json!({}))?;
    let templates_result = response_result(&templates_response);
    let templates = templates_result
        .as_array()
        .context("getBuiltinTemplates should return an array")?;
    ensure!(
        !templates.is_empty(),
        "getBuiltinTemplates should return at least one template"
    );
    let dae_template = templates
        .iter()
        .find(|item| item.get("id").and_then(Value::as_str) == Some("dae_modelica.mo.jinja"))
        .and_then(|item| item.get("source"))
        .and_then(Value::as_str)
        .context("getBuiltinTemplates should include dae_modelica.mo.jinja")?;

    let (render_ms, render_response) = execute_lsp_command(
        client,
        "rumoca.workspace.renderTemplate",
        json!({
            "uri": sim_uri,
            "model": "Decay",
            "template": dae_template,
        }),
    )?;
    let render = response_result(&render_response);
    ensure!(
        render.get("ok") == Some(&Value::Bool(true)),
        "renderTemplate should return ok"
    );
    ensure!(
        render
            .get("output")
            .and_then(Value::as_str)
            .is_some_and(|output| output.contains("class Decay")),
        "renderTemplate should render the requested model"
    );

    Ok(vec![
        ok_validation(
            "exec:getBuiltinTpl",
            "req",
            Some(templates_ms),
            "dae template",
        ),
        ok_validation("exec:renderTpl", "req", Some(render_ms), "class Decay"),
    ])
}
