use super::*;
use crate::*;
use rumoca_core::{ProvenanceSpan, SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("ContinuousSource.mo"), 1, 2)
}

fn block(operations: Vec<LinearOp>) -> ComputeBlock {
    ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_source_span(
            vec![operations],
            ProvenanceSpan::new(span(), "continuous source fixture").unwrap(),
        )
        .unwrap(),
    )
}

fn model(count: usize) -> SolveModel {
    let role = SolveVariableStorageRole::State;
    SolveModel {
        problem: SolveProblem {
            layout: VarLayout::from_parts_with_shapes_and_spans(
                [("state".into(), scalar_slot_y(0))].into(),
                [("state".into(), vec![count])].into(),
                [("state".into(), span())].into(),
                count,
                0,
            )
            .unwrap(),
            solve_layout: SolveLayout {
                state_scalar_count: count,
                variable_storage_runs: vec![SolveVariableStorageRun {
                    base: scalar_slot_y(0),
                    scalar_count: count,
                    role,
                    value_kind: SolveVariableValueKind::Real,
                }],
                variable_declarations: vec![SolveVariableDeclaration::new(
                    role,
                    SolveVariableValueKind::Real,
                )],
                ..Default::default()
            },
            continuous: ContinuousSolveSystem {
                derivative_rhs: block(vec![
                    LinearOp::TensorLoad {
                        dst_start: 0,
                        input: TensorInputKind::Y,
                        input_start: 0,
                        count,
                        seed_start: None,
                        lanes: 1,
                    },
                    LinearOp::StoreOutputRange {
                        start: 0,
                        count,
                        stride: 1,
                    },
                ]),
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    }
}

#[test]
fn primal_handover_borrows_one_model_without_coordinate_materialization() {
    for count in [4, 1_000_000] {
        let model = model(count);
        let source = model.continuous_numerical_source().unwrap();
        assert!(std::ptr::eq(source.layout(), &model.problem.layout));
        assert!(std::ptr::eq(
            source.coordinates(),
            &model.problem.solve_layout
        ));
        assert!(std::ptr::eq(source.calls(), &model.pure_calls));
        assert!(std::ptr::eq(source.continuous(), &model.problem.continuous));
        assert!(std::ptr::eq(
            source.derivative_source().source(),
            &model.problem.continuous.derivative_rhs
        ));
        assert_eq!(source.derivative_source().output_count(), count);
        assert_eq!(source.implicit_source().output_count(), 0);
        assert_eq!(source.manifold_source().output_count(), 0);
        assert_eq!(source.coordinates().variable_storage_runs.len(), 1);
        assert_eq!(source.layout().bindings().len(), 1);
        assert!(source.assignments().is_empty());
    }
}

#[test]
fn detached_derivative_artifacts_are_not_source_authority() {
    let mut model = model(4);
    let unrelated = block(vec![
        LinearOp::Const { dst: 0, value: 99. },
        LinearOp::StoreOutput { src: 0 },
    ]);
    let ComputeNode::ScalarPrograms(wrong_derivative) = &unrelated.nodes[0] else {
        panic!("fixture is scalar");
    };
    model.artifacts.continuous.full_jacobian_v = wrong_derivative.clone();
    let source = model.continuous_numerical_source().unwrap();
    assert_eq!(source.derivative_source().output_count(), 4);
    assert_eq!(model.artifacts.continuous.full_jacobian_v.output_count(), 1);
    assert!(std::ptr::eq(
        source.derivative_source().source(),
        &model.problem.continuous.derivative_rhs
    ));
}

#[test]
fn stale_layout_roles_and_unknown_schema_refuse_before_source_handover() {
    let mut changed = model(4);
    changed.problem.solve_layout.variable_storage_runs[0].role =
        SolveVariableStorageRole::Algebraic;
    let error = changed.continuous_numerical_source().unwrap_err();
    assert!(matches!(error, ContinuousNumericalSourceError::Source(_)));
    assert!(error.to_string().contains("immutable declaration"));
    assert!(error.source_span().is_none());
    let mut changed = model(4);
    changed.problem.schema_version = 0;
    let error = changed.continuous_numerical_source().unwrap_err();
    assert!(matches!(
        error,
        ContinuousNumericalSourceError::Source(ref source) if matches!(**source, SolveProblemShapeContractError::SchemaVersion { .. })
    ));
}

fn with_refresh(mut model: SolveModel) -> SolveModel {
    let implicit = block(vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 1. },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]);
    let row = AlgebraicRefreshRow::checked(AlgebraicRefreshRowDraft {
        owner_id: RefreshRowOwnerId::checked(0).unwrap(),
        source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
        equation_index: 0,
        output_offset: 0,
        target_index: 0,
        assignment_target: Some(0),
        assignment_shape: Some(TargetAssignmentShape::Direct {
            target_y_index: 0,
            expr_reg: 1,
            target_scale: 1.,
            expr_eval_len: 2,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .unwrap();
    let owners = ContinuousRefreshOwners::checked_for_source(
        &implicit,
        RefreshPlan {
            rows: vec![row],
            dynamic_causal_seed_rows: RefreshRowSelection::checked(1, [0]).unwrap(),
            ..Default::default()
        },
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    model.problem.continuous.implicit_rhs = implicit;
    model.problem.continuous.refresh_owners = owners;
    model
}

#[test]
fn model_source_binds_original_refresh_once_and_rejects_equal_foreign_program() {
    let mut model = with_refresh(model(1));
    {
        let source = model.continuous_numerical_source().unwrap();
        let [bound] = source.assignments() else {
            panic!("one original assignment");
        };
        assert_eq!(bound.assignment().target_indices(), &[0]);
        assert_eq!(bound.final_scalar_program().unwrap().output_count(), 1);
        assert_eq!(bound.provenance(), span());
    }
    let wire = serde_json::to_value(&model.problem.continuous.implicit_rhs).unwrap();
    model.problem.continuous.implicit_rhs = serde_json::from_value(wire).unwrap();
    let error = model.continuous_numerical_source().unwrap_err();
    assert!(matches!(error, ContinuousNumericalSourceError::Refresh(_)));
    assert!(error.to_string().contains("issued owner"));
    assert!(error.source_span().is_none());
}

#[test]
fn the_actual_helper_namespace_is_retained_and_missing_owners_refuse() {
    let mut model = model(1);
    let arithmetic = model.pure_calls.arithmetic();
    let ty = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    model.pure_calls = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![ty.clone()],
            vec![SolvePureCallOutput::result(ty)],
            span(),
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], span())?;
                builder.store(outputs[0], value, span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    model.problem.continuous.derivative_rhs = block(vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::PureCall {
            dst_start: 1,
            input_starts: vec![0].into(),
            site: model.pure_calls.owners()[0].call_site(),
        },
        LinearOp::StoreOutput { src: 1 },
    ]);
    let source = model.continuous_numerical_source().unwrap();
    assert!(std::ptr::eq(source.calls(), &model.pure_calls));
    model.pure_calls = SolvePureCallTable::default();
    let error = model.continuous_numerical_source().unwrap_err();
    assert!(error.to_string().contains("pure-call"));
    assert_eq!(error.source_span(), Some(span()));
}
