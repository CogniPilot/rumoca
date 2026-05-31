use super::*;
use rumoca_ir_dae as dae;

fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

#[test]
fn fmi_templates_snapshot_solve_pre_parameters_before_discrete_rows() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "solve".to_string(),
        serde_json::json!({
            "solve_layout": {
                "pre_param_bindings": [
                    {"dest_p_index": 1, "source": {"Y": {"index": 0}}},
                    {"dest_p_index": 2, "source": {"P": {"index": 3}}}
                ]
            },
            "events": {
                "root_conditions": {
                    "programs": [[
                        {"LoadY": {"dst": 0, "index": 0}},
                        {"StoreOutput": {"src": 0}}
                    ]]
                },
                "root_relation_memory_targets": [{"P": {"index": 5}}]
            },
            "discrete": {
                "rhs": {
                    "programs": [[
                        {"LoadP": {"dst": 0, "index": 1}},
                        {"StoreOutput": {"src": 0}}
                    ]]
                },
                "update_targets": [{"P": {"index": 4}}]
            }
        }),
    );

    for target in ["fmi2", "fmi3"] {
        let rendered = render_template_with_dae_json_and_name(
            &dae_json,
            builtin_template(target, "model.c.jinja"),
            "M",
        )
        .unwrap();
        assert!(
            rendered.contains("static void snapshot_pre_parameters(ModelInstance* m) {"),
            "{target} should render a pre-parameter snapshot helper:\n{rendered}"
        );
        assert!(
            rendered.contains("__rumoca_solve_set_p(m, 1, __rumoca_solve_y(m, 0));"),
            "{target} should snapshot Y-sourced pre parameters:\n{rendered}"
        );
        assert!(
            rendered.contains("__rumoca_solve_set_p(m, 2, __rumoca_solve_p(m, 3));"),
            "{target} should snapshot P-sourced pre parameters:\n{rendered}"
        );
        assert_snapshot_before_discrete_rows(target, &rendered);
        assert_root_snapshot_before_relation_memory_commit(target, &rendered);
        if target == "fmi3" {
            assert_fmi3_initial_updates_refresh_pre_params(&rendered);
        }
    }
}

fn assert_snapshot_before_discrete_rows(target: &str, rendered: &str) {
    let event_update = rendered
        .split("/* Save pre-values before discrete update */")
        .nth(1)
        .expect("template should save pre-values before event update");
    let snapshot_pos = event_update
        .find("snapshot_pre_parameters(m);")
        .expect("event update should snapshot lowered pre parameters");
    let compute_pos = event_update
        .find("compute_event_discrete_updates(m);")
        .expect("event update should evaluate discrete rows");
    assert!(
        snapshot_pos < compute_pos,
        "{target} should snapshot lowered pre parameters before discrete rows"
    );
}

fn assert_root_snapshot_before_relation_memory_commit(target: &str, rendered: &str) {
    let root_update = rendered
        .split("/* Check for zero-crossings in event indicators */")
        .nth(1)
        .expect("template should check root events");
    let snapshot_pos = root_update
        .find("snapshot_pre_parameters(m);")
        .expect("root event path should snapshot lowered pre parameters");
    let relation_memory_pos = root_update
        .find("__rumoca_solve_set_p(m, 5, root_relation_memory_value")
        .expect("root event path should commit relation memory");
    assert!(
        snapshot_pos < relation_memory_pos,
        "{target} should snapshot event-entry pre parameters before relation memory"
    );
}

fn assert_fmi3_initial_updates_refresh_pre_params(rendered: &str) {
    let init = rendered
        .split("FMI3_EXPORT fmi3Status fmi3ExitInitializationMode")
        .nth(1)
        .expect("FMI3 template should have an initialization exit");
    let first_snapshot_pos = init
        .find("snapshot_pre_parameters(m);")
        .expect("FMI3 init should seed lowered pre parameters");
    let compute_pos = init
        .find("compute_discrete_updates(m);")
        .expect("FMI3 init should evaluate initial discrete updates");
    let second_snapshot_pos = init[compute_pos..]
        .find("snapshot_pre_parameters(m);")
        .map(|pos| compute_pos + pos)
        .expect("FMI3 init should commit lowered pre parameters after updates");
    assert!(
        first_snapshot_pos < compute_pos && compute_pos < second_snapshot_pos,
        "FMI3 init should snapshot before and after initial discrete updates"
    );
}
