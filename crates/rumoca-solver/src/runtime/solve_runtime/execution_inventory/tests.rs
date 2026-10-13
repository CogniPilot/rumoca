use super::*;

impl SolveRuntime {
    pub(in crate::runtime::solve_runtime) fn scalar_fixture_mut(
        &mut self,
    ) -> &mut ScalarExecutionInventory {
        match &mut self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => scalar,
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => panic!("fixture requires scalar execution"),
        }
    }
}

fn assert_event_free_schedule(runtime: &SolveRuntime) {
    let y = runtime.model.initial_y.to_vec();
    let p = runtime.model.parameters.to_vec();
    let mut schedule = SolveStopSchedule::new(&runtime.model.problem, 0.0, 2.0);
    assert!(
        runtime
            .current_dynamic_time_event_stop(&y, &p, 0.0)
            .unwrap()
            .is_none()
    );
    let (time, event) = runtime
        .next_runtime_event_stop(&y, &p, &mut schedule, 0.0, 2.0)
        .unwrap();
    assert_eq!(time, 2.0);
    assert!(event.is_none());
}

#[test]
fn ordinary_empty_runtime_retains_scalar_inventory() {
    let model = solve::SolveModel::default();
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    assert!(runtime.scalar_execution().is_ok());
    assert_event_free_schedule(&runtime);
    #[cfg(feature = "native-wasm")]
    assert!(runtime.native_host().is_none());
}

#[cfg(feature = "native-wasm")]
fn compact_source_block(count: usize, span: rumoca_core::Span) -> solve::ComputeBlock {
    use solve::{BinaryOp, LinearOp};

    let n = u32::try_from(count).unwrap();
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_program_spans(
            vec![vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::TensorLoad {
                    dst_start: 1,
                    input: solve::TensorInputKind::Y,
                    input_start: 0,
                    count,
                    seed_start: None,
                    lanes: 1,
                },
                LinearOp::TensorFill {
                    dst_start: 1 + n,
                    value_start: 0,
                    count,
                    lanes: 1,
                },
                LinearOp::TensorBinary {
                    dst_start: 1 + 2 * n,
                    op: BinaryOp::Sub,
                    lhs_start: 1,
                    rhs_start: 1 + n,
                    count,
                    lhs_stride: 1,
                    rhs_stride: 1,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 1 + 2 * n,
                    count,
                    stride: 1,
                },
            ]],
            vec![span],
        )
        .unwrap(),
    )
}

#[cfg(feature = "native-wasm")]
fn compact_inputs(count: usize, span: rumoca_core::Span) -> Vec<solve::fmi::FmiVariableInput> {
    use solve::fmi::{FmiCausality, FmiVariability, FmiVariableInput};
    use solve::{SolveVariableStorageRole as Role, SolveVariableValueKind as Kind};

    let variable = FmiVariableInput {
        name: "result".into(),
        role: Role::Output,
        value_kind: Kind::Real,
        dimensions: vec![count as u32],
        start: solve::SolveDeclarationStart::Real(
            solve::SolveInitialValues::repeat(0.0, count).unwrap(),
        ),
        effective_input_default: None,
        minimum: None,
        maximum: None,
        nominal: None,
        unit: None,
        description: None,
        causality: FmiCausality::Output,
        declared_causality: None,
        variability: FmiVariability::Continuous,
        tunable: false,
        evaluable: false,
        declaration: span,
        text_start: None,
        enumeration: None,
    };
    let integer = FmiVariableInput {
        name: "integer".into(),
        role: Role::ExternalInput,
        value_kind: Kind::Integer,
        dimensions: vec![],
        start: solve::SolveDeclarationStart::Integer(
            solve::SolveExactInitialValues::construct(
                1,
                vec![solve::SolveExactInitialRun::Repeat {
                    start: 0,
                    value: i64::MAX,
                    count: 1,
                }],
            )
            .unwrap(),
        ),
        effective_input_default: None,
        minimum: None,
        maximum: None,
        nominal: None,
        unit: None,
        description: None,
        causality: FmiCausality::Input,
        declared_causality: None,
        variability: FmiVariability::Discrete,
        tunable: false,
        evaluable: false,
        declaration: span,
        text_start: None,
        enumeration: None,
    };
    vec![variable, integer]
}

#[cfg(feature = "native-wasm")]
fn compact_component() -> solve::fmi::FmiComponent {
    use rumoca_core::{SourceId, Span};
    use solve::{SolveVariableStorageRole as Role, SolveVariableValueKind as Kind};

    let count = 16;
    let span = Span::from_offsets(SourceId::from_source_name("NativeInventory.mo"), 1, 2);
    let source = compact_source_block(count, span);
    let mut model = solve::SolveModel::default();
    model.problem.layout = solve::VarLayout::from_parts_with_shapes_and_spans(
        IndexMap::from([
            ("result".into(), solve::scalar_slot_y(0)),
            ("integer".into(), solve::scalar_slot_p(0)),
        ]),
        IndexMap::from([("result".into(), vec![count])]),
        IndexMap::from([("result".into(), span)]),
        count,
        1,
    )
    .unwrap();
    model.problem.solve_layout = solve::SolveLayout {
        output_scalar_count: count,
        parameter_count: 1,
        compiled_parameter_len: 1,

        variable_storage_runs: vec![
            solve::SolveVariableStorageRun {
                base: solve::scalar_slot_y(0),
                scalar_count: count,
                role: Role::Output,
                value_kind: Kind::Real,
            },
            solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(0),
                scalar_count: 1,
                role: Role::ExternalInput,
                value_kind: Kind::Integer,
            },
        ],
        variable_declarations: vec![
            solve::SolveVariableDeclaration::new(Role::Output, Kind::Real),
            solve::SolveVariableDeclaration::new(Role::ExternalInput, Kind::Integer),
        ],
        ..Default::default()
    };
    model.problem.continuous.implicit_rhs = source;
    let stages = solve::ProjectionBlocks::from_stages(vec![
        solve::ProjectionStage::SingletonDeclarationRun {
            source: solve::RefreshScalarProgramSource::checked(0, 0).unwrap(),
            first_row: 0,
            declaration: 0,
            storage: model.problem.solve_layout.variable_storage_runs[0],
        },
    ])
    .unwrap();
    model.problem.continuous.implicit_row_targets =
        solve::ImplicitRowTargets::from_projection(&stages);
    model.problem.continuous.algebraic_projection_plan.blocks = stages;
    model.problem.continuous.refresh_owners =
        solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem).unwrap();
    let (continuous, initialization) =
        solve_eval::derive_solve_structural_artifacts(&model.problem, &model.artifacts).unwrap();
    model.artifacts.continuous.structural = continuous;
    model.artifacts.initialization.structural = initialization;
    model.initial_y = solve::SolveInitialValues::repeat(0.0, count).unwrap();
    model.parameters = vec![0.0].into();
    solve::fmi::FmiComponent::construct(model, compact_inputs(count, span)).unwrap()
}

#[cfg(feature = "native-wasm")]
#[test]
fn prepared_compact_owner_skips_scalar_preparation_and_preserves_refusal() {
    let component = compact_component();
    let root = component
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(0.0)
        .unwrap();
    assert!(
        root.model()
            .artifacts
            .continuous
            .structural
            .checked_assignment()
            .is_some()
    );
    assert!(
        root.model()
            .artifacts
            .continuous
            .structural
            .require_scalar_derivatives()
            .is_err()
    );
    let product =
        rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
            .unwrap();
    let host = product.into_host();
    let runtime = SolveRuntime::new_with_native_owner(
        SolveRuntimeModel::retained(root.clone()),
        host.clone(),
        0.0,
    )
    .unwrap();
    assert_event_free_schedule(&runtime);
    assert!(matches!(
        runtime.execution,
        RuntimeExecutionInventory::Native(_)
    ));
    assert!(std::ptr::eq(
        runtime.native_host().unwrap().product(),
        host.product()
    ));
    assert!(std::ptr::eq(
        runtime.model.retained_assertion_root().unwrap().metadata(),
        root.metadata()
    ));
    let mut output = vec![13.0; runtime.solver_count];
    assert!(matches!(
        runtime.full_solver_y_into(0.0, &[], &[], 1e-8, 8, &mut output),
        Err(RuntimeSolveError::UnsupportedModel { .. })
    ));
    assert_eq!(output, vec![13.0; 16]);
    assert!(SolveRuntime::new(root.model()).is_err());
    let foreign = compact_component()
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(0.0)
        .unwrap();
    assert!(matches!(
        SolveRuntime::new_with_native_owner(SolveRuntimeModel::retained(foreign), host, 0.0),
        Err(RuntimeSolveError::UnsupportedModel { .. })
    ));
    drop(component);
    assert_eq!(runtime.solver_count, 16);
    assert!(runtime.scalar_execution().is_err());
}

#[cfg(feature = "native-wasm")]
#[test]
fn native_instance_admission_checks_original_resolved_model_and_product_anchors() {
    let component = compact_component();
    let original = component.runtime_view().retain_assertion_root();
    let root = original.resolved_periodic_schedules_at(2.0).unwrap();
    let product =
        rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
            .unwrap();
    let host = product.into_host();
    let transport = host
        .transport_from_source(&root.model().initial_y, &root.model().parameters)
        .unwrap();
    let inputs = transport
        .input_values()
        .map(|(lane, value)| (lane.p_index(), value.unwrap()))
        .collect::<Vec<_>>();
    assert_eq!(inputs, [(0, solve::SolveValueKind::Integer(i64::MAX))]);

    for coordinate in [5.0, f64::from_bits(2.0_f64.to_bits() + 1)] {
        let instance = original.resolved_periodic_schedules_at(coordinate).unwrap();
        assert!(instance.shares_root_owner(&root));
        assert!(std::ptr::eq(instance.metadata(), root.metadata()));
        for (model, requested) in [
            (SolveRuntimeModel::retained(instance.clone()), coordinate),
            (SolveRuntimeModel::retained(instance), 2.0),
            (SolveRuntimeModel::retained(root.clone()), coordinate),
        ] {
            assert!(matches!(
                SolveRuntime::new_with_native_owner(model, host.clone(), requested),
                Err(RuntimeSolveError::UnsupportedModel { reason })
                    if reason == "native execution startTime differs from its original resolved anchors"
            ));
        }
    }
    assert!(matches!(
        SolveRuntime::new_with_native_owner(
            SolveRuntimeModel::retained(original),
            host.clone(),
            2.0,
        ),
        Err(RuntimeSolveError::UnsupportedModel { .. })
    ));
    for requested in [f64::NAN, f64::INFINITY, f64::NEG_INFINITY] {
        assert!(matches!(
            SolveRuntime::new_with_native_owner(
                SolveRuntimeModel::retained(root.clone()),
                host.clone(),
                requested,
            ),
            Err(RuntimeSolveError::UnsupportedModel { .. })
        ));
    }
    let same_instance = component
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(2.0)
        .unwrap();
    let runtime = SolveRuntime::new_with_native_owner(
        SolveRuntimeModel::retained(same_instance),
        host.clone(),
        2.0,
    )
    .unwrap();
    assert!(std::ptr::eq(
        runtime.native_host().unwrap().product(),
        host.product()
    ));
    assert_eq!(
        runtime
            .model
            .retained_assertion_root()
            .unwrap()
            .resolved_start_time(),
        Some(2.0)
    );
    let foreign = compact_component()
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(2.0)
        .unwrap();
    assert!(matches!(
        SolveRuntime::new_with_native_owner(SolveRuntimeModel::retained(foreign), host, 2.0),
        Err(RuntimeSolveError::UnsupportedModel { reason })
            if reason == "prepared native entry belongs to another component source"
    ));
}
