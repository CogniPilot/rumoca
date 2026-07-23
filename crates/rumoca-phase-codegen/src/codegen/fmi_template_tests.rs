use super::*;
use rumoca_ir_dae as dae;

fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

#[test]
fn dae_template_context_projects_scheduled_times_and_preserves_provenance() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("scheduled-event.mo"),
        68,
        78,
    );
    let event = dae::DaeScheduledTimeEvent {
        time: 0.5,
        source_span: Some(span),
    };
    let mut dae = dae::Dae::new();
    dae.events.scheduled_time_events.push(event);

    let expected_record = serde_json::json!({
        "time": 0.5,
        "source_span": serde_json::to_value(span).expect("span should serialize"),
    });
    let canonical = serde_json::to_value(&dae).expect("canonical DAE should serialize");
    assert_eq!(
        canonical["scheduled_time_events"],
        serde_json::json!([expected_record.clone()]),
        "canonical DAE must retain typed scheduled-event provenance",
    );

    let context = dae_template_json(&dae).expect("DAE template context should serialize");
    assert_eq!(
        context["scheduled_time_event_metadata"],
        serde_json::json!([expected_record]),
        "codegen context must expose full scheduled-event metadata",
    );
    assert_eq!(
        context["scheduled_time_events"],
        serde_json::json!([0.5]),
        "stable template schedule surface must contain numeric times",
    );

    let renderer = SolveTemplateRenderer::new_with_dae(
        &solve::SolveProblem::default(),
        &solve::SolveArtifacts::default(),
        dae,
    )
    .expect("Solve renderer should accept scheduled-event metadata");
    for target in ["fmi2", "fmi3"] {
        let rendered = renderer
            .render_with_name(builtin_template(target, "model.c.jinja"), "M")
            .unwrap_or_else(|err| panic!("{target} model.c should render: {err}"));
        let scheduled_array = rendered
            .split("static const double scheduled_events[] = {")
            .nth(1)
            .and_then(|tail| tail.split("};").next())
            .unwrap_or_else(|| panic!("{target} should render a scheduled-event array"));
        assert!(
            scheduled_array.contains("0.5"),
            "{target} scheduled array should contain numeric 0.5:\n{scheduled_array}",
        );
        assert!(
            !scheduled_array.contains("source_span") && !scheduled_array.contains('{'),
            "{target} scheduled array must not contain provenance JSON:\n{scheduled_array}",
        );
    }
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
        .split("FMI3_Export fmi3Status fmi3ExitInitializationMode")
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
