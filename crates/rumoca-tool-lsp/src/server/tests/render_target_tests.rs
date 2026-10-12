//! Editor target rendering of FMI targets and of Solve targets whose
//! programs hold compiler-issued pure calls.

use super::*;

async fn render_open_document_target(
    temp_name: &str,
    file_name: &str,
    source: &str,
    model: &str,
    target: &str,
) -> serde_json::Value {
    let temp = new_temp_dir(temp_name);
    let focus = temp.join(file_name);
    std::fs::write(&focus, source).expect("write focus");
    let service = new_test_service();
    let server = service.inner();
    {
        let mut session = server.session.write().await;
        session.update_document(&focus.to_string_lossy(), source);
    }
    server
        .execute_command(ExecuteCommandParams {
            command: "rumoca.workspace.renderTarget".to_string(),
            arguments: vec![serde_json::json!({
                "uri": Url::from_file_path(&focus).expect("file uri").to_string(),
                "model": model,
                "target": target,
            })],
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("execute command should succeed")
        .expect("execute command should return a payload")
}

/// An FMI target renders the unpacked source FMU under its package root, with
/// a minted instantiation token; the editor writes it as text files.
#[test]
fn render_target_command_renders_the_unpacked_fmi3_source_fmu() {
    run_async_test(async {
        let response = render_open_document_target(
            "render-target-fmi3",
            "Ball.mo",
            "model Ball\n  Real h(start = 1, fixed = true);\n  Real v(start = 0, fixed = true);\n\
             equation\n  der(h) = v;\n  der(v) = -9.81;\n  when h < 0 then\n    \
             reinit(v, -0.8*pre(v));\n  end when;\nend Ball;\n",
            "Ball",
            "fmi3",
        )
        .await;
        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "fmi3 renders in the editor: {response}"
        );
        let paths = rendered_target_paths(&response);
        for expected in ["Ball/modelDescription.xml", "Ball/sources/model.c"] {
            assert!(paths.contains(&expected), "missing {expected}: {paths:?}");
        }
        let description = rendered_target_content(&response, "Ball/modelDescription.xml");
        assert!(description.contains("fmiVersion=\"3.0\""), "{description}");
        let token = description
            .split("instantiationToken=\"")
            .nth(1)
            .and_then(|rest| rest.split('"').next())
            .expect("instantiationToken attribute");
        assert!(
            token.trim_matches(['{', '}']).len() == 36,
            "minted token: {token}"
        );
    });
}

/// Solve targets inline the pure call a compacted algebraic `for` family
/// lowers to, so the editor renders what the CLI renders.
#[test]
fn render_target_command_inlines_pure_calls_for_solve_targets() {
    run_async_test(async {
        let response = render_open_document_target(
            "render-target-family-call",
            "FamilyCall.mo",
            "model FamilyCall\n  parameter Real b[3] = {0.1, 0.2, 0.3};\n  \
             Real x[3](start = {1, 2, 3}, each fixed = true);\n  Real a[3];\nequation\n  \
             for i in 1:3 loop\n    a[i] = tanh(x[i] + b[i]);\n  end for;\n  der(x) = -a;\n\
             end FamilyCall;\n",
            "FamilyCall",
            "c-ode",
        )
        .await;
        assert_eq!(
            response.get("ok").and_then(serde_json::Value::as_bool),
            Some(true),
            "c-ode renders the inlined family: {response}"
        );
        assert!(rendered_target_content(&response, "FamilyCall_ode.c").contains("tanh"));
    });
}

fn scalar_admission_target() -> PathBuf {
    let target = new_temp_dir("render-target-scalar-admission");
    let manifest = include_str!("../../../../rumoca-phase-codegen/src/templates/c-ode/target.toml");
    let capabilities = manifest
        .split("[[files]]")
        .next()
        .expect("builtin manifest capabilities");
    std::fs::write(
        target.join("target.toml"),
        format!(
            "{capabilities}\n[[files]]\npath = \"admitted.txt\"\ntemplate = \"admitted.jinja\"\n"
        ),
    )
    .expect("write manifest");
    std::fs::write(target.join("admitted.jinja"), "admitted").expect("write template");
    target
}

#[test]
fn render_target_command_checks_scalar_fallback_before_rendering() {
    run_async_test(async {
        for (conversion, admitted) in [(false, true), (true, false)] {
            let target = scalar_admission_target();
            let assignment = if conversion {
                "k := integer(u); y := k;"
            } else {
                "y := u;"
            };
            let source = format!(
                "function checkedInteger\n input Real u;\n output Real y;\n\
                 protected\n Integer k;\nalgorithm\n {assignment}\nend checkedInteger;\n\
                 model Probe\n Real x(start=1, fixed=true);\nequation\n\
                 der(x)=checkedInteger(x);\nend Probe;\n"
            );
            let response = render_open_document_target(
                "render-target-scalar-admission-model",
                "Probe.mo",
                &source,
                "Probe",
                target.to_str().expect("target path"),
            )
            .await;
            assert_eq!(response["ok"].as_bool(), Some(admitted), "{response}");
            if admitted {
                assert_eq!(
                    rendered_target_content(&response, "admitted.txt"),
                    "admitted"
                );
            } else {
                assert!(
                    response["error"]
                        .as_str()
                        .expect("admission error")
                        .contains("unsupported-feature:scalar.derivative_rhs"),
                    "{response}"
                );
                assert!(response.get("files").is_none(), "{response}");
            }
        }
    });
}

fn rendered_target_paths(response: &serde_json::Value) -> Vec<&str> {
    response
        .get("files")
        .and_then(serde_json::Value::as_array)
        .expect("files array")
        .iter()
        .filter_map(|file| file.get("path").and_then(serde_json::Value::as_str))
        .collect()
}

fn rendered_target_content<'a>(response: &'a serde_json::Value, path: &str) -> &'a str {
    response
        .get("files")
        .and_then(serde_json::Value::as_array)
        .expect("files array")
        .iter()
        .find(|file| file.get("path").and_then(serde_json::Value::as_str) == Some(path))
        .and_then(|file| file.get("content"))
        .and_then(serde_json::Value::as_str)
        .unwrap_or_else(|| panic!("missing rendered content for {path}"))
}
