use super::*;

#[test]
fn retained_native_default_functions_issue_original_exact_starts() {
    let _lock = session_test_guard();
    with_source_component(typed_inputs::SOURCE, "Step", |component| {
        let root = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(0.0)
            .unwrap();
        let host =
            rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
                .unwrap()
                .into_host();
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        let transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        assert!(transport.input_values().all(|(_, value)| value.is_ok()));
        let variable = root
            .metadata()
            .variables()
            .iter()
            .find(|variable| variable.name() == "previous.identity.sequence")
            .unwrap();
        let rumoca_ir_solve::SolveDeclarationStart::Integer(values) = variable.declaration_start()
        else {
            panic!("Integer")
        };
        assert_eq!(values.value(0), Some(&0));
        Ok(String::new())
    })
    .unwrap();
    with_source_component(SLAM_DEFAULT_POLICY, "DefaultPolicyProbe", |component| {
        let root = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(0.0)
            .unwrap();
        let maximum = root
            .metadata()
            .variables()
            .iter()
            .find(|variable| variable.name() == "policy.maximumPCG")
            .unwrap();
        let rumoca_ir_solve::SolveDeclarationStart::Integer(values) = maximum.declaration_start()
        else {
            panic!("Integer")
        };
        assert_eq!(values.value(0), Some(&48));
        let boolean = root
            .metadata()
            .variables()
            .iter()
            .find(|variable| variable.name() == "policy.filter.constrainVelocityAndBias")
            .unwrap();
        let rumoca_ir_solve::SolveDeclarationStart::Boolean(values) = boolean.declaration_start()
        else {
            panic!("Boolean")
        };
        assert_eq!(values.value(0), Some(&false));
        let host =
            rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
                .unwrap()
                .into_host();
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        assert!(
            host.transport_from_source(&y, &root.model().parameters)
                .unwrap()
                .input_values()
                .all(|(_, value)| value.is_ok())
        );
        Ok(String::new())
    })
    .unwrap();
}

// Policy declarations and function bodies copied verbatim from the SLAM
// application source so the declared starts match the issued FMI metadata.
pub(super) const SLAM_DEFAULT_POLICY: &str = r#"
package SchmidtGraphPoseCorrection
  record Policy
    Real weight; Boolean constrainVelocityAndBias;
    Real maximumPositionInnovation; Real maximumAngularInnovation;
    Real maximumPositionCorrection; Real maximumAngularCorrection; Real maximumNis;
  end Policy;
  function DefaultPolicy
    output Policy result;
  algorithm
    result.weight := 0.5; result.constrainVelocityAndBias := false;
    result.maximumPositionInnovation := 5; result.maximumAngularInnovation := 0.35;
    result.maximumPositionCorrection := 5; result.maximumAngularCorrection := 0.35;
    result.maximumNis := 36;
  end DefaultPolicy;
end SchmidtGraphPoseCorrection;
package RGBDGraphProcessing
  record Policy
    SchmidtGraphPoseCorrection.Policy filter;
    Integer maximumIterations;
    Integer maximumPCG;
    Integer maximumBacktracks;
    Real initialDamping;
    Real maximumPositionStep;
    Real maximumAngleStep;
    Real pcgTolerance;
    Integer covariancePCG;
    Real covarianceTolerance;
    Real anchorWeight;
    Real anchorPositionLimit;
    Real anchorAngleLimit;
    Real transportWeight;
    Real consistencyTolerance;
    Real maximumConfidence;
  end Policy;
  function DefaultPolicy
    output Policy result;
  algorithm
    result.filter := SchmidtGraphPoseCorrection.DefaultPolicy();
    result.maximumIterations := 8;
    result.maximumPCG := 48;
    result.maximumBacktracks := 8;
    result.initialDamping := 1e-3;
    result.maximumPositionStep := 0.5;
    result.maximumAngleStep := 0.15;
    result.pcgTolerance := 1e-6;
    result.covariancePCG := 48;
    result.covarianceTolerance := 1e-10;
    result.anchorWeight := 0.5;
    result.anchorPositionLimit := 5.0;
    result.anchorAngleLimit := 0.35;
    result.transportWeight := 0.5;
    result.consistencyTolerance := 1e-6;
    result.maximumConfidence := 8;
  end DefaultPolicy;
end RGBDGraphProcessing;
model DefaultPolicyProbe
  input RGBDGraphProcessing.Policy policy = RGBDGraphProcessing.DefaultPolicy();
  output Integer result;
equation
  result = policy.maximumPCG;
end DefaultPolicyProbe;
"#;

#[test]
fn nested_function_fill_and_parameter_binding_retain_repeat_source() {
    let _lock = session_test_guard();
    let source = REPEAT_FUNCTION_SOURCE;
    with_source_component(source, "RepeatFunctionProbe", |component| {
        let root = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(0.0)
            .unwrap();
        let variable = root
            .metadata()
            .variables()
            .iter()
            .find(|variable| variable.name() == "values")
            .unwrap();
        let rumoca_ir_solve::SolveDeclarationStart::Integer(values) = variable.declaration_start()
        else {
            panic!("Integer")
        };
        assert!(matches!(
            values.runs().next(),
            Some(rumoca_ir_solve::SolveExactInitialRun::Repeat {
                value: i64::MAX,
                count: 4096,
                ..
            })
        ));
        assert_eq!(values.runs().count(), 1);
        let host =
            rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
                .unwrap()
                .into_host();
        let y = vec![0.0; root.model().problem.layout.y_scalars()];
        let transport = host
            .transport_from_source(&y, &root.model().parameters)
            .unwrap();
        assert!(
            transport
                .input_values()
                .all(|(_, value)| value == Ok(rumoca_ir_solve::SolveValueKind::Integer(i64::MAX)))
        );
        Ok(String::new())
    })
    .unwrap();
}

const REPEAT_FUNCTION_SOURCE: &str = r#"
function ExactFill
  output Integer values[4096];
algorithm
  values := fill(9223372036854775807,4096);
end ExactFill;
function NestedFill
  output Integer values[4096];
algorithm
  values := ExactFill();
end NestedFill;
model RepeatFunctionProbe
  parameter Integer defaults[4096] = NestedFill();
  input Integer values[4096] = defaults;
  output Integer result;
equation
  result = values[1];
end RepeatFunctionProbe;
"#;

#[test]
fn structural_folds_preserve_exact_source_runs_at_small_and_tensor_extents() {
    let _lock = session_test_guard();
    let mut wire_lengths = Vec::new();
    for extent in [2, 65536] {
        let source = typed_fold_source(extent);
        with_source_component(&source, "TypedFoldProbe", |component| {
            let root = component.runtime_view().retain_assertion_root();
            wire_lengths.push(
                serde_json::to_vec(&rumoca_sim::retained_fmi_component_wire(&root).unwrap())
                    .unwrap()
                    .len(),
            );
            assert_fold_layout_bindings(&root, extent);
            assert_fold_exact_starts(&root, extent);
            Ok(String::new())
        })
        .unwrap();
    }
    assert!(
        wire_lengths[1] < wire_lengths[0] + 8192,
        "compact fold wire must follow source runs, not tensor extent: lengths={wire_lengths:?}"
    );
}

fn root_variable<'a>(
    root: &'a rumoca_ir_solve::RetainedAssertionRoot,
    name: &str,
) -> &'a rumoca_ir_solve::fmi::FmiVariable {
    root.metadata()
        .variables()
        .iter()
        .find(|v| v.name() == name)
        .unwrap()
}

fn assert_fold_layout_bindings(root: &rumoca_ir_solve::RetainedAssertionRoot, extent: usize) {
    let layout = &root.model().problem.layout;
    assert_eq!(
        layout.bindings().len(),
        10,
        "one binding per original declaration"
    );
    for name in ["large", "maximum", "disabled", "signedZero"] {
        let base = root_variable(root, name).storage().unwrap().base();
        assert_eq!(layout.shape(name), Some(&[extent][..]));
        assert!(layout.shape_span(name).is_some());
        for ordinal in [0, extent / 2, extent - 1] {
            let spelling = format!("{name}[{}]", ordinal + 1);
            let expected = rumoca_ir_solve::scalar_slot_p(base + ordinal);
            assert_eq!(layout.binding(&spelling), Some(expected));
            assert_eq!(layout.scalar_name_for_slot(expected), Some(spelling));
        }
        assert_eq!(layout.binding(&format!("{name}[0]")), None);
        assert_eq!(layout.binding(&format!("{name}[{}]", extent + 1)), None);
        assert_eq!(layout.binding(&format!("foreign.{name}[1]")), None);
        assert_eq!(layout.binding(&format!("{name}[:]")), None);
    }
}

fn assert_fold_exact_starts(root: &rumoca_ir_solve::RetainedAssertionRoot, extent: usize) {
    let variable = |name| root_variable(root, name);
    for (name, expected) in [("large", 9007199254740993), ("maximum", i64::MAX)] {
        let rumoca_ir_solve::SolveDeclarationStart::Integer(values) =
            variable(name).declaration_start()
        else {
            panic!("Integer")
        };
        assert_eq!(values.runs().count(), 1);
        assert!(
            matches!(values.runs().next(), Some(rumoca_ir_solve::SolveExactInitialRun::Repeat { value, count, .. }) if *value == expected && *count == extent)
        );
    }
    let rumoca_ir_solve::SolveDeclarationStart::Boolean(values) =
        variable("disabled").declaration_start()
    else {
        panic!("Boolean")
    };
    assert!(
        matches!(values.runs().next(), Some(rumoca_ir_solve::SolveExactInitialRun::Repeat { value:false,count,.. }) if *count == extent)
    );
    let rumoca_ir_solve::SolveDeclarationStart::Real(values) =
        variable("signedZero").declaration_start()
    else {
        panic!("Real")
    };
    let run = values.runs().next().unwrap();
    assert_eq!(run.repeated_bits(), Some((-0.0f64).to_bits()));
    assert_eq!(run.count(), extent);
    let rumoca_ir_solve::SolveDeclarationStart::Integer(values) =
        variable("authored").declaration_start()
    else {
        panic!("Integer")
    };
    assert!(matches!(
        values.runs().next(),
        Some(rumoca_ir_solve::SolveExactInitialRun::Literal { .. })
    ));
    assert_eq!(values.value(0), Some(&9007199254740993));
    assert_eq!(values.value(1), Some(&9007199254740993));
}

#[test]
fn original_visible_projection_producer_retains_declarations_at_tensor_extents() {
    let _lock = session_test_guard();
    let mut lengths = Vec::new();
    for extent in [2, 65536] {
        let source = format!(
            r#"model VisibleTensor
  input Real sensor[{extent}](each unit="m", start=fill(-0.0,{extent}));
  output Real measured[{extent}](each unit="m");
equation
  measured = sensor;
end VisibleTensor;"#
        );
        with_source_component(&source, "VisibleTensor", |component| {
            let root = component.runtime_view().retain_assertion_root();
            let model = root.model();
            let visible = model.visible_projection();
            assert_eq!(visible.inputs().len(), 2);
            assert_eq!(visible.scalar_count(), 2 * extent);
            let wire =
                serde_json::to_value(rumoca_sim::retained_fmi_component_wire(&root).unwrap())
                    .unwrap();
            let model_wire = &wire["solve_model"];
            for removed in ["visible_names", "visible_value_rows", "variable_meta"] {
                assert!(
                    model_wire.get(removed).is_none(),
                    "superseded visible field {removed}"
                );
            }
            lengths.push(serde_json::to_vec(visible.inputs()).unwrap().len());
            assert_visible_points(model, &visible, extent);
            assert_eq!(visible.name(2 * extent), None);
            assert_eq!(visible.index("sensor[0]"), None);
            assert_eq!(visible.index("foreign.sensor[1]"), None);
            let declarations = declaration_indices(&visible);
            assert!(declarations.windows(2).all(|pair| pair[0] < pair[1]));
            for corruption in 0..4 {
                let foreign = corrupted_visible_inputs(&visible, extent, corruption);
                assert!(
                    rumoca_ir_solve::VisibleProjection::construct(&model.problem, foreign).is_err(),
                    "corruption {corruption} was admitted"
                );
            }
            for corruption in 0..4 {
                let mut problem = model.problem.clone();
                corrupt_storage_run(
                    &mut problem.solve_layout.variable_storage_runs[declarations[0]],
                    corruption,
                );
                assert!(
                    rumoca_ir_solve::VisibleProjection::construct(
                        &problem,
                        visible.inputs().to_vec()
                    )
                    .is_err(),
                    "original allocation corruption {corruption} was admitted"
                );
            }
            Ok(String::new())
        })
        .unwrap();
    }
    assert!(
        lengths[1] < lengths[0] + 512,
        "visible source metadata grew {lengths:?}"
    );
}

fn assert_visible_points(
    model: &rumoca_ir_solve::SolveModel,
    visible: &rumoca_ir_solve::VisibleProjectionView<'_>,
    extent: usize,
) {
    for ordinal in [0, extent / 2, extent - 1] {
        for name in ["sensor", "measured"] {
            let prefix = visible.index(&format!("{name}[1]")).unwrap();
            let point = prefix + ordinal;
            let expected = format!("{name}[{}]", ordinal + 1);
            assert_eq!(visible.name(point), Some(expected.clone()));
            assert_eq!(visible.index(&expected), Some(point));
            assert_eq!(visible.slot(point), model.problem.layout.binding(&expected));
            let meta = visible.metadata(point).unwrap();
            assert_eq!(meta.name, expected);
            assert_eq!(meta.unit.as_deref(), Some("m"));
            assert_eq!(
                meta.source_span,
                model.problem.layout.shape_span(name).unwrap()
            );
        }
    }
}

fn corrupted_visible_inputs(
    visible: &rumoca_ir_solve::VisibleProjectionView<'_>,
    extent: usize,
    corruption: usize,
) -> Vec<rumoca_ir_solve::VisibleProjectionInput> {
    let mut foreign = visible.inputs().to_vec();
    let rumoca_ir_solve::VisibleProjectionInput::Declaration(input) = &mut foreign[0] else {
        panic!("original declaration")
    };
    match corruption {
        0 => input.declaration = usize::MAX,
        1 => input.binding = rumoca_core::VarName::intern("foreign"),
        2 => {
            input.attributes.source_span = rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("foreign-visible.mo"),
                1,
                2,
            )
        }
        _ => input.state_coordinates.push((
            extent,
            rumoca_ir_solve::SolveStateCoordinate {
                variable: "sensor[1]".into(),
                derivative_order: 0,
            },
        )),
    }
    foreign
}

fn corrupt_storage_run(run: &mut rumoca_ir_solve::SolveVariableStorageRun, corruption: usize) {
    match corruption {
        0 => run.value_kind = rumoca_ir_solve::SolveVariableValueKind::Integer,
        1 => run.role = rumoca_ir_solve::SolveVariableStorageRole::Parameter,
        2 => run.scalar_count += 1,
        _ => match &mut run.base {
            rumoca_ir_solve::ScalarSlot::Y { byte_offset, .. }
            | rumoca_ir_solve::ScalarSlot::P { byte_offset, .. } => *byte_offset += 1,
            _ => panic!("original numeric storage"),
        },
    }
}

fn typed_fold_source(extent: usize) -> String {
    r#"
function LargeFill
  output Integer values[EXTENT];
algorithm
  values := fill(9007199254740993,EXTENT);
end LargeFill;
model TypedFoldProbe
  final parameter Integer large[EXTENT] = LargeFill();
  final parameter Integer maximum[EXTENT] = fill(9223372036854775807,EXTENT);
  final parameter Boolean disabled[EXTENT] = fill(false,EXTENT);
  final parameter Real signedZero[EXTENT] = fill(-0.0,EXTENT);
  final parameter Integer authored[2] = {9007199254740993,9007199254740993};
  output Integer result;
  output Integer maxResult;
  output Integer authoredResult;
  output Boolean flag;
  output Real realResult;
equation
  result = large[1];
  maxResult = maximum[1];
  authoredResult = authored[1];
  flag = disabled[1];
  realResult = signedZero[1];
end TypedFoldProbe;
"#
    .replace("EXTENT", &extent.to_string())
}

fn declaration_indices(visible: &rumoca_ir_solve::VisibleProjectionView<'_>) -> Vec<usize> {
    visible
        .inputs()
        .iter()
        .map(|member| {
            let rumoca_ir_solve::VisibleProjectionInput::Declaration(input) = member else {
                panic!("original declaration")
            };
            input.declaration
        })
        .collect()
}
