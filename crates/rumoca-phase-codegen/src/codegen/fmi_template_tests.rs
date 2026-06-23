use super::*;
use rumoca_ir_dae as dae;

fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

#[test]
fn fmi2_model_description_value_references_match_c_runtime_layout() {
    let mut dae = dae::Dae::new();
    dae.variables.inputs.insert(
        "zeta_input".into(),
        rumoca_ir_dae::Variable {
            name: "zeta_input".into(),
            ..Default::default()
        },
    );
    dae.variables.inputs.insert(
        "alpha_input".into(),
        rumoca_ir_dae::Variable {
            name: "alpha_input".into(),
            ..Default::default()
        },
    );
    dae.variables.outputs.insert(
        "z_output".into(),
        rumoca_ir_dae::Variable {
            name: "z_output".into(),
            ..Default::default()
        },
    );
    dae.variables.outputs.insert(
        "b_output".into(),
        rumoca_ir_dae::Variable {
            name: "b_output".into(),
            ..Default::default()
        },
    );

    let c = render_template_with_name(&dae, builtin_template("fmi2", "model.c.jinja"), "M")
        .expect("render FMI2 C");
    let xml = render_template_with_name(
        &dae,
        builtin_template("fmi2", "modelDescription.xml.jinja"),
        "M",
    )
    .expect("render FMI2 modelDescription");

    assert!(
        c.contains("double z_output = m->w[0];") && c.contains("double b_output = m->w[1];"),
        "FMI2 C runtime output storage order must preserve DAE layout order:\n{c}"
    );
    assert!(
        xml.contains(r#"<ScalarVariable name="z_output" valueReference="2" causality="output""#)
            && xml.contains(
                r#"<ScalarVariable name="b_output" valueReference="3" causality="output""#
            ),
        "FMI2 modelDescription output valueReferences must match C VR_W + m->w offset:\n{xml}"
    );
}

#[test]
fn fmi2_runtime_y_layout_preserves_solve_load_indices() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        "state_x".into(),
        rumoca_ir_dae::Variable {
            name: "state_x".into(),
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "zeta_y".into(),
        rumoca_ir_dae::Variable {
            name: "zeta_y".into(),
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "alpha_y".into(),
        rumoca_ir_dae::Variable {
            name: "alpha_y".into(),
            ..Default::default()
        },
    );
    dae.variables.outputs.insert(
        "probe".into(),
        rumoca_ir_dae::Variable {
            name: "probe".into(),
            ..Default::default()
        },
    );

    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "solve".to_string(),
        serde_json::json!({
            "continuous": {
                "derivative_rhs": {
                    "programs": [[
                        {"LoadY": {"dst": 0, "index": 1}},
                        {"StoreOutput": {"src": 0}}
                    ]]
                }
            }
        }),
    );

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi2", "model.c.jinja"),
        "M",
    )
    .expect("render FMI2 C");

    assert!(
        rendered.contains("double zeta_y = m->y[0];")
            && rendered.contains("double alpha_y = m->y[1];"),
        "runtime y storage must keep the DAE order used by solve LoadY indices:\n{rendered}"
    );
}

#[test]
fn fmi2_solve_y_access_uses_solve_visible_name_mapping_when_runtime_y_order_differs() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        "state_x".into(),
        rumoca_ir_dae::Variable {
            name: "state_x".into(),
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "alpha_y".into(),
        rumoca_ir_dae::Variable {
            name: "alpha_y".into(),
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "beta_y".into(),
        rumoca_ir_dae::Variable {
            name: "beta_y".into(),
            ..Default::default()
        },
    );

    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "solve".to_string(),
        serde_json::json!({
            "visible_names": ["state_x", "beta_y", "alpha_y"],
            "continuous": {
                "derivative_rhs": {
                    "programs": [[
                        {"LoadY": {"dst": 0, "index": 1}},
                        {"StoreOutput": {"src": 0}}
                    ]]
                }
            }
        }),
    );

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi2", "model.c.jinja"),
        "M",
    )
    .expect("render FMI2 C");

    assert!(
        rendered.contains("case 1: return m->y[1];  /* beta_y */")
            && rendered.contains("case 2: return m->y[0];  /* alpha_y */")
            && rendered.contains("case 1: m->y[1] = value; return;  /* beta_y */")
            && rendered.contains("static RUMOCA_NOINLINE double __rumoca_solve_y"),
        "solve LoadY indices must be mapped through solve.visible_names, not runtime y order:\n{rendered}"
    );
}

#[test]
fn fmi2_solve_derivatives_apply_boptest_zone_energy_physical_override() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.U".into(),
        rumoca_ir_dae::Variable {
            name: "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.U".into(),
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "bogus_algebraic".into(),
        rumoca_ir_dae::Variable {
            name: "bogus_algebraic".into(),
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.Hb_flow".into(),
        rumoca_ir_dae::Variable {
            name: "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.Hb_flow".into(),
            ..Default::default()
        },
    );
    dae.variables.algebraics.insert(
        "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.Q_flow".into(),
        rumoca_ir_dae::Variable {
            name: "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.Q_flow".into(),
            ..Default::default()
        },
    );

    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "solve_blocks".to_string(),
        serde_json::json!({
            "continuous": {
                "derivative_rhs": {
                    "scalar_programs": {
                        "programs": [[
                            {"LoadY": {"dst": 0, "index": 1}},
                            {"StoreOutput": {"src": 0}}
                        ]]
                    }
                }
            }
        }),
    );
    dae_json
        .as_object_mut()
        .unwrap()
        .insert("solve_derivative_nodes".to_string(), serde_json::json!([]));

    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi2", "model.c.jinja"),
        "M",
    )
    .expect("render FMI2 C");

    assert!(
        rendered.contains("physical state override: floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.U")
            && rendered.contains(
                "m->xdot[0] = (floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_Hb_flow + floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_Q_flow);"
            ),
        "BOPTEST zone energy states must override solve derivative rows with physical energy balance:\n{rendered}"
    );
}

#[test]
fn fmi2_initialize_defaults_applies_target_based_parameter_bindings_without_start() {
    let mut dae = dae::Dae::new();
    dae.variables.parameters.insert(
        "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.rho_start".into(),
        rumoca_ir_dae::Variable {
            name: "floor1BoptestAirNetwork.floor.fivZonVAV.zon[1].vol.rho_start".into(),
            ..Default::default()
        },
    );

    let rendered = render_template_with_name(&dae, builtin_template("fmi2", "model.c.jinja"), "M")
        .expect("render FMI2 C");
    let defaults = rendered
        .split("static void initialize_defaults(ModelInstance* m) {")
        .nth(1)
        .expect("template should render initialize_defaults")
        .split("/* =========================================================================")
        .next()
        .expect("initialize_defaults section should have an end marker");

    assert!(
        defaults.contains(
            "= Air_density(floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_p_start, floor1BoptestAirNetwork_floor_fivZonVAV_zon_1_vol_T_start);"
        ),
        "target-based parameter bindings must run during defaults even when the parameter has no start expression:\n{defaults}"
    );
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

#[test]
fn fmi_templates_relax_algebraics_without_single_pass_output_overwrite() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    dae_json.as_object_mut().unwrap().insert(
        "w".to_string(),
        serde_json::json!({
            "output": {
                "ty": "Real",
                "causality": "output",
                "variability": "continuous",
                "start": {"Literal": {"value": {"Real": 0.0}}},
                "dims": []
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
            rendered.contains(
                "for (int __rumoca_alg_iter = 0; __rumoca_alg_iter < 64; ++__rumoca_alg_iter)"
            ) && rendered.contains("if (__rumoca_alg_max_delta < 1.0e-9) break;"),
            "{target} should render convergence-bounded algebraic relaxation passes"
        );

        let derivatives = rendered
            .split("static void compute_derivatives(ModelInstance* m)")
            .nth(1)
            .expect("template should render compute_derivatives")
            .split("/* =========================================================================")
            .next()
            .expect("compute_derivatives section should have an end marker");
        assert!(
            !derivatives.contains("compute_outputs(m);"),
            "{target} should not overwrite relaxed algebraics with a single output pass"
        );
    }
}

#[test]
fn fmi2_template_runs_initial_discrete_updates_while_initial_is_true() {
    let dae = dae::Dae::new();
    let dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    let rendered = render_template_with_dae_json_and_name(
        &dae_json,
        builtin_template("fmi2", "model.c.jinja"),
        "M",
    )
    .unwrap();

    assert!(
        rendered.contains("#define initial() ((m->state == modelInitializationMode) ? 1 : 0)"),
        "FMI2 initial() must reflect initialization mode, not a hardcoded false value"
    );
    let exit_initialization = rendered
        .split("FMI2_EXPORT fmi2Status fmi2ExitInitializationMode")
        .nth(1)
        .expect("template should render fmi2ExitInitializationMode")
        .split("m->state = modelEventMode;")
        .next()
        .expect("exit initialization body should set event mode");
    assert!(
        exit_initialization.contains("compute_discrete_updates(m, 1);"),
        "FMI2 must evaluate initial when-discrete updates before leaving initialization mode:\n{exit_initialization}"
    );
    assert!(
        exit_initialization
            .contains("compute_derivatives(m);\n    compute_outputs(m);\n    m->dirty_values = 0;"),
        "FMI2 must refresh initialized outputs before leaving initialization mode:\n{exit_initialization}"
    );
}

#[test]
fn fmi_templates_emit_dae_z_discrete_fallback_when_solve_rows_are_absent() {
    assert_dae_z_discrete_updates_render(false);
}

#[test]
fn fmi_templates_emit_dae_z_discrete_updates_when_solve_rows_exist() {
    assert_dae_z_discrete_updates_render(true);
}

#[test]
fn fmi_templates_emit_state_event_updates_for_reinit_lowering() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    let object = dae_json.as_object_mut().unwrap();
    object.insert("symbol_refs".to_string(), serde_json::json!(["x"]));
    object.insert(
        "x".to_string(),
        serde_json::json!({
            "x": {
                "name": "x",
                "dims": [],
                "start": {"Literal": {"value": {"Real": 0.0}}},
                "unit": null,
                "nominal": null,
                "min": null,
                "max": null,
                "description": null
            }
        }),
    );
    object.insert(
        "f_z".to_string(),
        serde_json::json!([{
            "lhs": "x",
            "rhs": {"Literal": {"value": {"Real": 2.0}}}
        }]),
    );

    for target in ["fmi2", "fmi3"] {
        let rendered = render_template_with_dae_json_and_name(
            &dae_json,
            builtin_template(target, "model.c.jinja"),
            "M",
        )
        .unwrap();
        let discrete_updates = rendered
            .split("static void compute_discrete_updates(ModelInstance* m, int apply_state_event_updates) {")
            .nth(1)
            .expect("template should render compute_discrete_updates")
            .split("/* =========================================================================")
            .next()
            .expect("compute_discrete_updates section should have an end marker");
        assert!(
            discrete_updates.contains("if (apply_state_event_updates)")
                && discrete_updates.contains("m->x[0] = 2.0;  /* event state update: x */"),
            "{target} should write reinit-lowered state updates back to the FMI state vector:\n{discrete_updates}"
        );
        assert!(
            !discrete_updates.contains("(void)m;"),
            "{target} must not turn a state event update into a no-op:\n{discrete_updates}"
        );
    }
}

fn assert_dae_z_discrete_updates_render(with_solve_rows: bool) {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    let object = dae_json.as_object_mut().unwrap();
    object.insert("symbol_refs".to_string(), serde_json::json!(["z"]));
    object.insert(
        "z".to_string(),
        serde_json::json!({
            "z": {
                "name": "z",
                "dims": [],
                "start": {"Literal": {"value": {"Real": 0.0}}},
                "unit": null,
                "nominal": null,
                "min": null,
                "max": null,
                "description": null
            }
        }),
    );
    object.insert(
        "f_z".to_string(),
        serde_json::json!([{
            "lhs": "z",
            "rhs": {"Literal": {"value": {"Real": 1.0}}}
        }]),
    );
    if with_solve_rows {
        object.insert(
            "solve".to_string(),
            serde_json::json!({
                "discrete": {
                    "rhs": {
                        "programs": [[
                            {"LoadP": {"dst": 0, "index": 0}},
                            {"StoreOutput": {"src": 0}}
                        ]]
                    },
                    "update_targets": [{"P": {"index": 0}}]
                }
            }),
        );
    }

    for target in ["fmi2", "fmi3"] {
        let rendered = render_template_with_dae_json_and_name(
            &dae_json,
            builtin_template(target, "model.c.jinja"),
            "M",
        )
        .unwrap();
        let discrete_updates = rendered
            .split("static void compute_discrete_updates(ModelInstance* m, int apply_state_event_updates) {")
            .nth(1)
            .expect("template should render compute_discrete_updates")
            .split("/* =========================================================================")
            .next()
            .expect("compute_discrete_updates section should have an end marker");
        assert!(
            discrete_updates.contains("m->z[0] = 1.0;  /* discrete update: z */"),
            "{target} should emit DAE f_z updates regardless of solve discrete row presence:\n{discrete_updates}"
        );
        assert!(
            !discrete_updates.contains("(void)m;"),
            "{target} must not turn a DAE f_z discrete partition into a no-op:\n{discrete_updates}"
        );
    }
}

#[test]
fn fmi_templates_emit_dae_m_discrete_updates() {
    let dae = dae::Dae::new();
    let mut dae_json = dae_template_json(&dae).expect("dae_template_json should not fail");
    let object = dae_json.as_object_mut().unwrap();
    object.insert(
        "symbol_refs".to_string(),
        serde_json::json!(["hotWaterPlant.boilerPlant.secPumCon.On"]),
    );
    object.insert(
        "m".to_string(),
        serde_json::json!({
            "hotWaterPlant.boilerPlant.secPumCon.On": {
                "name": "hotWaterPlant.boilerPlant.secPumCon.On",
                "dims": [],
                "start": {"Literal": {"value": {"Boolean": false}}},
                "unit": null,
                "nominal": null,
                "min": null,
                "max": null,
                "description": null
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
        let discrete_updates = rendered
            .split("static void compute_discrete_updates(ModelInstance* m, int apply_state_event_updates) {")
            .nth(1)
            .expect("template should render compute_discrete_updates")
            .split("/* =========================================================================")
            .next()
            .expect("compute_discrete_updates section should have an end marker");
        assert!(
            discrete_updates.contains(
                "m->m[0] = 1.0;  /* discrete valued update: hotWaterPlant.boilerPlant.secPumCon.On */"
            ),
            "{target} should write DAE m discrete updates back to the FMI discrete-valued vector:\n{discrete_updates}"
        );
        assert!(
            !discrete_updates.contains("(void)m;"),
            "{target} must not turn a DAE m discrete partition into a no-op:\n{discrete_updates}"
        );
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
        .find("compute_discrete_updates(m, 1);")
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
