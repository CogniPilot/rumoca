// SPEC_0021 file-size exception: this root integration fixture covers several
// cross-owner Solve contracts. split plan: move serialization and event-owner
// fixture families into dedicated test modules without duplicating builders.
use super::*;
use indexmap::IndexMap;
use rumoca_core::{
    InstanceId, RealMatrixMultiplySemantics, SourceId, SourceOccurrenceId, StructuredIndexBinder,
    VarName,
};
use serde::de::DeserializeOwned;

const REPRESENTATIVE_SOLVE_PROBLEM_GOLDEN: &str =
    include_str!("../tests/golden/representative_solve_problem.solve.json");

fn empty_continuous_system() -> ContinuousSolveSystem {
    let implicit_rhs = ComputeBlock::default();
    ContinuousSolveSystem::construct(
        &SolveLayout::default(),
        ContinuousSolveSystemInputs::new(
            implicit_rhs,
            Vec::new(),
            AlgebraicProjectionPlan::default(),
            ComputeBlock::default(),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            ComputeBlock::default(),
            ContinuousRefreshPlanInputs::empty(),
        ),
    )
    .expect("empty fixture has a checked continuous system")
}

fn empty_solve_problem() -> SolveProblem {
    SolveProblem::construct(
        VarLayout::default(),
        SolveLayout::default(),
        empty_continuous_system(),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
    .expect("the explicit zero-equation test root satisfies every construction relation")
}

#[test]
fn solve_problem_rejects_disagreement_between_y_layout_and_solver_inventory() {
    let error = SolveProblem::construct(
        VarLayout::from_parts(IndexMap::new(), 1, 0),
        SolveLayout::default(),
        empty_continuous_system(),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
    .expect_err("one Y slot cannot have an empty solver-name inventory");

    assert_eq!(
        error,
        SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "layout.y_scalars/solve_layout.solver_maps.names",
            expected: 1,
            actual: 0,
            span: None,
        }
    );
}

fn assert_missing_wire_field_rejected<T>(
    mut value: serde_json::Value,
    object_pointer: &str,
    field: &str,
) where
    T: DeserializeOwned + std::fmt::Debug,
{
    let object = value
        .pointer_mut(object_pointer)
        .unwrap_or_else(|| panic!("fixture is missing JSON object at `{object_pointer}`"))
        .as_object_mut()
        .unwrap_or_else(|| panic!("fixture JSON pointer `{object_pointer}` is not an object"));
    assert!(
        object.remove(field).is_some(),
        "fixture object `{object_pointer}` is missing required field `{field}`"
    );
    let error = match serde_json::from_value::<T>(value) {
        Ok(_) => panic!(
            "current Solve wire field `{object_pointer}/{field}` deserialized through a default"
        ),
        Err(error) => error,
    };
    assert!(
        error
            .to_string()
            .contains(&format!("missing field `{field}`")),
        "unexpected omission error for {object_pointer}/{field}: {error}"
    );
}

fn test_tensor_domain(count: usize) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "i".to_string(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    }
}

fn fixture_span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("ir_solve_tests_source_44.mo"),
        0,
        1,
    )
}

fn fixture_provenance() -> rumoca_core::ProvenanceSpan {
    fixture_span()
        .require_provenance("Solve IR fixture")
        .expect("fixture span is source-backed")
}

fn binary64_first_product_profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::FULL,
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    )
}

fn fixture_scalar_rows(rows: usize, purpose: &'static str) -> ScalarProgramBlock {
    if rows == 0 {
        return ScalarProgramBlock::default();
    }
    ScalarProgramBlock::with_source_span(
        (0..rows)
            .map(|_| {
                vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        fixture_span()
            .require_provenance(purpose)
            .expect("fixture span is source-backed"),
    )
    .expect("fixture rows are computable")
}

/// One checked continuous system whose derivative program covers `state_count`
/// leading Y slots and nothing else.
fn state_continuous_system(derivative_rows: usize) -> ContinuousSolveSystem {
    let implicit_rhs = ComputeBlock::default();
    let solve_layout = SolveLayout {
        state_scalar_count: derivative_rows,
        ..SolveLayout::default()
    };
    ContinuousSolveSystem::construct(
        &solve_layout,
        ContinuousSolveSystemInputs::new(
            implicit_rhs,
            Vec::new(),
            AlgebraicProjectionPlan::default(),
            ComputeBlock::default(),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            ComputeBlock::from_scalar_program_block(fixture_scalar_rows(
                derivative_rows,
                "state fixture derivative",
            )),
            ContinuousRefreshPlanInputs::empty(),
        ),
    )
    .expect("state fixture has a checked continuous system")
}

fn state_problem_with_derivative_rows(
    state_count: usize,
    derivative_rows: usize,
) -> Result<SolveProblem, SolveProblemShapeContractError> {
    state_problem_with_derivative_block(
        state_count,
        ComputeBlock::from_scalar_program_block(fixture_scalar_rows(
            derivative_rows,
            "state fixture derivative",
        )),
    )
}

fn state_problem_with_derivative_block(
    state_count: usize,
    derivative_rhs: ComputeBlock,
) -> Result<SolveProblem, SolveProblemShapeContractError> {
    let implicit_rhs = ComputeBlock::default();
    let scalar_names: Vec<String> = (1..=state_count).map(|i| format!("x[{i}]")).collect();
    let solve_layout = SolveLayout {
        solver_maps: SolverNameIndexMaps {
            name_to_idx: scalar_names
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, name)| (name, index))
                .collect(),
            base_to_indices: IndexMap::from([(
                "x".to_string(),
                (0..state_count).collect::<Vec<_>>(),
            )]),
            names: scalar_names,
        },
        variable_storage_runs: vec![SolveVariableStorageRun {
            base: crate::SolveStorageCoordinate::Y(0),
            scalar_count: state_count,
            role: SolveVariableStorageRole::State,
            value_kind: SolveVariableValueKind::Real,
        }],
        variable_declarations: vec![SolveVariableDeclaration::new(
            SolveVariableStorageRole::State,
            SolveVariableValueKind::Real,
        )],
        state_scalar_count: state_count,
        ..SolveLayout::default()
    };
    let continuous = ContinuousSolveSystem::construct(
        &solve_layout,
        ContinuousSolveSystemInputs::new(
            implicit_rhs,
            Vec::new(),
            AlgebraicProjectionPlan::default(),
            ComputeBlock::default(),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            derivative_rhs,
            ContinuousRefreshPlanInputs::empty(),
        ),
    )
    .expect("state fixture has a checked continuous system");
    SolveProblem::construct(
        make_layout(&[("x", vec![state_count])], &[]),
        solve_layout,
        continuous,
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
}

fn assert_derivative_state_count_error(
    error: SolveProblemShapeContractError,
    expected: usize,
    actual: usize,
) {
    assert_eq!(
        error,
        SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "continuous.derivative_rhs.exact_output_coverage",
            expected,
            actual,
            span: None,
        }
    );
}

#[test]
fn solve_problem_accepts_exact_constant_derivative_state_cardinality() {
    state_problem_with_derivative_rows(2, 2)
        .expect("two constant derivative outputs cover exactly two states");
}

fn derivative_map_node(
    extents: &[usize],
    start: usize,
    strides: Vec<AffineStencilIndexStrideTerm>,
) -> ComputeNode {
    let binders = extents
        .iter()
        .copied()
        .enumerate()
        .map(|(dimension, extent)| StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(dimension as u32),
            display_name: format!("i{dimension}"),
            lower: 0,
            upper: i64::try_from(extent).expect("fixture extent fits i64") - 1,
            step: 1,
        })
        .collect();
    ComputeNode::Map {
        domain: StructuredIndexDomain { binders },
        output_map: TensorOutputMap { start, strides },
        base_ops: vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: Vec::new(),
        const_strides: Vec::new(),
        metadata: TensorNodeMetadata::default(),
        span: fixture_span(),
    }
}

#[test]
fn derivative_coverage_accepts_compact_permuted_and_reversed_native_maps() {
    let permuted = derivative_map_node(
        &[2, 3],
        0,
        vec![
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            },
            AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: 2,
            },
        ],
    );
    state_problem_with_derivative_block(
        6,
        ComputeBlock {
            nodes: vec![permuted],
        },
    )
    .expect("axis-permuted mixed-radix output ownership is exact and compact");

    let reversed = derivative_map_node(
        &[2],
        1,
        vec![AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: -1,
        }],
    );
    state_problem_with_derivative_block(
        2,
        ComputeBlock {
            nodes: vec![reversed],
        },
    )
    .expect("a reversed dense native output map remains exact and injective");
}

#[test]
fn derivative_coverage_refuses_noninjective_native_and_cross_node_overlap() {
    let noninjective = derivative_map_node(
        &[2],
        0,
        vec![AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 0,
        }],
    );
    let error = state_problem_with_derivative_block(
        2,
        ComputeBlock {
            nodes: vec![noninjective],
        },
    )
    .expect_err("a native tensor map cannot write one derivative slot twice");
    assert!(matches!(
        error,
        SolveProblemShapeContractError::DerivativeOutputCoverage {
            kind: DerivativeOutputCoverageKind::NonInjective,
            node_index: 0,
            ..
        }
    ));

    let scalar = fixture_scalar_rows(1, "overlap scalar");
    let native = derivative_map_node(&[1], 0, Vec::new());
    let error = state_problem_with_derivative_block(
        2,
        ComputeBlock {
            nodes: vec![ComputeNode::ScalarPrograms(scalar), native],
        },
    )
    .expect_err("two compute nodes cannot own the same derivative output");
    assert!(matches!(
        error,
        SolveProblemShapeContractError::DerivativeOutputCoverage {
            kind: DerivativeOutputCoverageKind::Overlap,
            node_index: 1,
            index: 0,
            ..
        }
    ));
}

#[test]
fn empty_native_map_does_not_advance_derivative_output_coverage() {
    let empty = derivative_map_node(&[0], 41, Vec::new());
    state_problem_with_derivative_block(
        1,
        ComputeBlock {
            nodes: vec![
                empty,
                ComputeNode::ScalarPrograms(fixture_scalar_rows(
                    1,
                    "derivative after empty native map",
                )),
            ],
        },
    )
    .expect("an empty produced set cannot move the following output owner away from zero");
}

#[test]
fn solve_problem_refuses_missing_constant_derivative_output() {
    let error = state_problem_with_derivative_rows(2, 1)
        .expect_err("one constant derivative output cannot cover two states");
    assert_derivative_state_count_error(error, 2, 1);
}

#[test]
fn solve_problem_refuses_surplus_constant_derivative_output() {
    let error = state_problem_with_derivative_rows(2, 3)
        .expect_err("three constant derivative outputs cannot cover two states");
    assert_derivative_state_count_error(error, 2, 3);
}

fn mutate_derivative_scalar_wire_count(wire: &mut serde_json::Value, surplus: bool) {
    let scalar = &mut wire["continuous"]["derivative_rhs"]["nodes"][0]["ScalarPrograms"];
    for field in ["programs", "program_spans", "output_indices"] {
        let values = scalar[field]
            .as_array_mut()
            .expect("state derivative scalar wire field is an array");
        if surplus {
            let mut extra = values[0].clone();
            if field == "output_indices" {
                extra = serde_json::json!(2);
            }
            values.push(extra);
        } else {
            values.pop().expect("fixture has two derivative rows");
        }
    }
}

#[test]
fn solve_problem_wire_replay_refuses_missing_derivative_output() {
    let problem = state_problem_with_derivative_rows(2, 2).expect("valid source fixture");
    let mut wire = serde_json::to_value(problem).expect("serialize fixture");
    mutate_derivative_scalar_wire_count(&mut wire, false);

    let error = serde_json::from_value::<SolveProblem>(wire)
        .expect_err("wire replay cannot mint a root with a missing derivative output");
    assert!(
        error
            .to_string()
            .contains("continuous.derivative_rhs.exact_output_coverage expected 2 rows, got 1"),
        "wire replay must report the typed derivative/state contract: {error}"
    );
}

#[test]
fn solve_problem_wire_replay_refuses_surplus_derivative_output() {
    let problem = state_problem_with_derivative_rows(2, 2).expect("valid source fixture");
    let mut wire = serde_json::to_value(problem).expect("serialize fixture");
    mutate_derivative_scalar_wire_count(&mut wire, true);

    let error = serde_json::from_value::<SolveProblem>(wire)
        .expect_err("wire replay cannot mint a root with a surplus derivative output");
    assert!(
        error
            .to_string()
            .contains("continuous.derivative_rhs.exact_output_coverage expected 2 rows, got 3"),
        "wire replay must report the typed derivative/state contract: {error}"
    );
}

#[test]
fn solve_problem_mutation_refuses_a_hole_in_the_derivative_output_domain() {
    let mut problem = state_problem_with_derivative_rows(2, 2).expect("valid source fixture");
    let ComputeNode::ScalarPrograms(block) = &mut problem.continuous.derivative_rhs.nodes[0] else {
        panic!("state derivative fixture is a scalar-program block");
    };
    block.output_indices[1] = 2;

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::SolverIndexOutOfBounds {
            context: "continuous.derivative_rhs.exact_output_coverage",
            index: 2,
            upper_bound: 2,
            span: Some(fixture_span()),
        })
    );
}

#[test]
fn solve_problem_wire_replay_refuses_a_hole_in_the_derivative_output_domain() {
    let problem = state_problem_with_derivative_rows(2, 2).expect("valid source fixture");
    let mut wire = serde_json::to_value(problem).expect("serialize fixture");
    wire["continuous"]["derivative_rhs"]["nodes"][0]["ScalarPrograms"]["output_indices"][1] =
        serde_json::json!(2);

    let error = serde_json::from_value::<SolveProblem>(wire)
        .expect_err("wire replay cannot mint a derivative output hole");
    assert!(
        error.to_string().contains(
            "continuous.derivative_rhs.exact_output_coverage references solver index 2, but upper bound is 2"
        ),
        "wire replay must report the exact dense output-domain contract: {error}"
    );
}

/// Artifacts matching [`state_continuous_system`] for `state_count` states in
/// a layout with `full_columns` total Y and P scalars.
fn state_artifacts(state_count: usize, full_columns: usize) -> SolveArtifactInputs {
    let derivative = (state_count > 0).then(|| {
        StructuralPattern::full(
            state_count,
            full_columns,
            PatternProvenance::derived(PatternDerivation::DependencyPropagation, fixture_span())
                .expect("fixture span is source-backed"),
        )
        .expect("fixture derivative pattern shape is valid")
    });
    SolveArtifactInputs {
        continuous: ContinuousSolveArtifacts {
            structural: ContinuousStructuralArtifacts::derived(
                None,
                Vec::new(),
                Vec::new(),
                None,
                Vec::new(),
                derivative,
            ),
            mass_matrix: MassMatrix::Identity,
            full_jacobian_v: fixture_scalar_rows(state_count, "state fixture jacobian"),
            ..ContinuousSolveArtifacts::default()
        },
        ..SolveArtifactInputs::empty()
    }
}

/// Construct one sealed root whose Y column is `initial_y.len()` state slots,
/// exercising the exact runtime-vector admission checks of
/// [`SolveModel::construct`].
fn construct_state_model(
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
) -> Result<SolveModel, SolveModelConstructionError> {
    let state_count = initial_y.len();
    construct_state_model_with_artifacts(
        initial_y,
        solver_nominals,
        state_artifacts(state_count, state_count),
    )
}

fn construct_state_model_with_artifacts(
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    artifacts: SolveArtifactInputs,
) -> Result<SolveModel, SolveModelConstructionError> {
    let catalog_nominals = Some(solver_nominals.clone());
    construct_state_model_with_catalog_nominals(
        initial_y,
        solver_nominals,
        catalog_nominals,
        artifacts,
    )
}

fn construct_state_model_with_catalog_nominals(
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    catalog_nominals: Option<Vec<f64>>,
    artifacts: SolveArtifactInputs,
) -> Result<SolveModel, SolveModelConstructionError> {
    let state_count = initial_y.len();
    let scalar_names: Vec<String> = (1..=state_count).map(|i| format!("x[{i}]")).collect();
    let problem = state_problem_with_derivative_rows(state_count, state_count)
        .expect("state fixture problem is valid by construction");
    let visible_value_rows = ScalarProgramBlock::with_source_span(
        (0..state_count)
            .map(|index| {
                vec![
                    LinearOp::LoadY { dst: 0, index },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        fixture_provenance(),
    )
    .expect("state fixture visible rows are computable");
    let entry: SolveVariableCatalogSourceEntry = (
        SolveVariableSource::new(
            SourceOccurrenceId::try_from(InstanceId::new(1))
                .expect("fixture source occurrence is explicitly nonzero"),
            "x".to_string(),
            vec![u32::try_from(state_count).expect("state fixture extent fits u32")],
            scalar_names,
            fixture_span(),
        ),
        SolveVariableSourceAttributes::new(
            SolveVariableCausality::Local,
            SolveVariableVariability::Continuous,
            false,
            None,
            None,
            rumoca_core::Fixity::Fixed,
        ),
        SolveVariableEvaluatedValues::new(Some(initial_y.clone()), None, None, catalog_nominals),
    );
    SolveModel::construct(
        problem,
        SolvePureCallTable::empty(binary64_first_product_profile()),
        artifacts,
        SolveModelRuntimeInputs {
            initial_y,
            solver_nominals,
            parameters: Vec::new(),
        },
        visible_value_rows,
        [entry],
    )
}

fn event_iteration_contract_fixture() -> SolveProblem {
    SolveProblem {
        layout: VarLayout::from_parts(IndexMap::new(), 0, 2),
        solve_layout: SolveLayout {
            variable_storage_runs: vec![SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::P(0),
                scalar_count: 1,
                role: SolveVariableStorageRole::DiscreteReal,
                value_kind: SolveVariableValueKind::Real,
            }],
            variable_declarations: vec![SolveVariableDeclaration::new(
                SolveVariableStorageRole::DiscreteReal,
                SolveVariableValueKind::Real,
            )],
            compiled_parameter_len: 2,
            discrete_real_scalar_names: vec!["z".to_string()],
            pre_param_bindings: vec![PreParamBinding {
                dest_p_index: 1,
                source: PreParamSource::P { index: 0 },
                clock_schedule: None,
            }],
            ..SolveLayout::default()
        },
        discrete: DiscreteSolveSystem {
            event_iteration_plan: EventIterationPlan {
                runs: vec![EventIterationRun {
                    variable: 0,
                    pre_binding_start: 0,
                    owner: EventIterationOwner::ScalarRows { start_row: 0 },
                }],
            },
            rhs: ScalarProgramBlock::with_source_span(
                vec![vec![
                    LinearOp::Const { dst: 0, value: 1.0 },
                    LinearOp::StoreOutput { src: 0 },
                ]],
                fixture_provenance(),
            )
            .expect("event-iteration fixture program is computable"),
            update_targets: vec![scalar_slot_p(0)],
            row_roles: vec![DiscreteRowRole::Equation],
            pre_modes: vec![DiscreteEventPreMode::FollowCurrent],
            observation_refresh: vec![false],
            integrator_history_effects: vec![IntegratorHistoryEffect::Preserve],
            clock_owners: vec![None],
            ..DiscreteSolveSystem::default()
        },
        ..empty_solve_problem()
    }
}

#[test]
fn event_iteration_contract_accepts_complete_typed_reverse_bijection() {
    event_iteration_contract_fixture()
        .validate_constructed_parts()
        .expect("complete typed producer, plan, and pre binding must validate");
}

#[test]
fn event_iteration_contract_rejects_scalar_owner_relabelled_to_hold() {
    let mut problem = event_iteration_contract_fixture();
    problem.discrete.event_iteration_plan.runs[0].owner = EventIterationOwner::Hold;

    let error = problem
        .validate_constructed_parts()
        .expect_err("a live scalar producer cannot be hidden by relabelling its owner Hold");
    assert!(error.to_string().contains("equation producer"), "{error}");
}

#[test]
fn event_iteration_contract_rejects_coordinated_storage_relabel_and_plan_deletion() {
    let mut problem = event_iteration_contract_fixture();
    problem.solve_layout.variable_storage_runs[0].role = SolveVariableStorageRole::Parameter;
    problem.discrete.event_iteration_plan.runs.clear();
    problem.solve_layout.pre_param_bindings.clear();
    problem.discrete.row_roles[0] = DiscreteRowRole::EventAction;

    let error = problem.validate_constructed_parts().expect_err(
        "an equation producer cannot escape reverse ownership by relabelling its storage role",
    );
    assert!(
        error.to_string().contains("immutable declaration"),
        "{error}"
    );
}

#[test]
fn variable_declaration_replay_rejects_boolean_to_integer_relabel() {
    let mut problem = event_iteration_contract_fixture();
    problem.solve_layout.variable_storage_runs[0].role = SolveVariableStorageRole::DiscreteValue;
    problem.solve_layout.variable_storage_runs[0].value_kind = SolveVariableValueKind::Integer;

    let error = problem
        .validate_constructed_parts()
        .expect_err("storage metadata cannot relabel the canonical declared value kind");
    assert!(
        error.to_string().contains("immutable declaration"),
        "{error}"
    );
}

#[test]
fn variable_declaration_constructs_only_role_compatible_time_domains() {
    assert_eq!(
        SolveVariableDeclaration::new(
            SolveVariableStorageRole::Parameter,
            SolveVariableValueKind::Real,
        )
        .time_domain(),
        SolveVariableTimeDomain::Static
    );
    assert_eq!(
        SolveVariableDeclaration::new(
            SolveVariableStorageRole::DiscreteReal,
            SolveVariableValueKind::Real,
        )
        .time_domain(),
        SolveVariableTimeDomain::EventDiscrete
    );
    assert_eq!(
        SolveVariableDeclaration::event_discontinuous(
            SolveVariableStorageRole::Output,
            SolveVariableValueKind::Real,
        )
        .expect("a proved Real output may be event-held")
        .time_domain(),
        SolveVariableTimeDomain::EventDiscontinuous
    );
    assert!(
        SolveVariableDeclaration::event_discontinuous(
            SolveVariableStorageRole::State,
            SolveVariableValueKind::Real,
        )
        .is_err(),
        "state storage cannot forge an event-discontinuous declaration"
    );
}

#[test]
fn variable_declaration_wire_rejects_forged_time_domain() {
    let problem = event_iteration_contract_fixture();
    let mut wire = serde_json::to_value(problem).expect("fixture Solve problem serializes");
    wire["solve_layout"]["variable_declarations"][0]["time_domain"] =
        serde_json::json!("continuous_time");
    let error = serde_json::from_value::<SolveProblem>(wire)
        .expect_err("wire replay must reject a domain incompatible with discrete storage");
    assert!(
        error.to_string().contains("requires time domain"),
        "{error}"
    );
}

#[test]
fn event_iteration_contract_rejects_deleted_run_and_pre_binding() {
    let mut problem = event_iteration_contract_fixture();
    problem.discrete.event_iteration_plan.runs.clear();
    problem.solve_layout.pre_param_bindings.clear();

    let error = problem
        .validate_constructed_parts()
        .expect_err("canonical discrete storage requires both its plan run and pre binding");
    assert!(error.to_string().contains("reverse bijection"), "{error}");
}

#[test]
fn event_iteration_contract_rejects_short_clock_column_without_panicking() {
    let mut problem = event_iteration_contract_fixture();
    problem.discrete.clock_owners.clear();

    let error = problem
        .validate_constructed_parts()
        .expect_err("a truncated clock column must reject through the checked validator");
    assert!(error.to_string().contains("clock range"), "{error}");
}

#[test]
fn variable_storage_contract_rejects_external_input_producer() {
    let mut problem = event_iteration_contract_fixture();
    problem.solve_layout.variable_storage_runs[0].role = SolveVariableStorageRole::ExternalInput;
    problem.solve_layout.variable_declarations[0] = SolveVariableDeclaration::new(
        SolveVariableStorageRole::ExternalInput,
        SolveVariableValueKind::Real,
    );
    problem.discrete.event_iteration_plan.runs.clear();
    problem.solve_layout.pre_param_bindings.clear();
    problem.discrete.row_roles[0] = DiscreteRowRole::EventAction;

    let error = problem
        .validate_constructed_parts()
        .expect_err("an external input is read-only even for an event-action producer");
    assert!(
        error
            .to_string()
            .contains("canonical non-discrete variable"),
        "{error}"
    );
}

#[test]
fn variable_storage_contract_rejects_forged_discrete_value_kind() {
    let mut problem = event_iteration_contract_fixture();
    problem.solve_layout.variable_storage_runs[0].role = SolveVariableStorageRole::DiscreteValue;
    problem.solve_layout.variable_storage_runs[0].value_kind = SolveVariableValueKind::String;

    let error = problem
        .validate_constructed_parts()
        .expect_err("a wire cannot relabel a discrete runtime coordinate as String");
    assert!(
        error.to_string().contains("immutable declaration"),
        "{error}"
    );
}

#[test]
fn tensor_output_count_uses_compact_domain_bounds() {
    let domain = test_tensor_domain(1_000_000);
    let output_map = TensorOutputMap::dense_contiguous(7, &domain)
        .expect("large domain has valid dense strides");

    assert_eq!(
        output_map.output_count(&domain),
        Ok(1_000_007),
        "output count should be derived from compact bounds"
    );
}

#[test]
fn tensor_output_count_combines_correlated_terms_per_dimension() {
    let domain = test_tensor_domain(4);
    let output_map = TensorOutputMap {
        start: 3,
        strides: vec![
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: -2,
            },
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 3,
            },
        ],
    };

    assert_eq!(output_map.output_count(&domain), Ok(7));
}

#[test]
fn tensor_output_indices_combine_terms_before_checked_arithmetic() {
    let domain = test_tensor_domain(3);
    let output_map = TensorOutputMap {
        start: 3,
        strides: vec![
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: isize::MAX,
            },
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            },
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: -isize::MAX,
            },
        ],
    };

    assert_eq!(output_map.output_indices(&domain), Ok(vec![3, 4, 5]));
}

#[test]
fn tensor_shape_contract_validates_large_domain_without_scalarizing_it() {
    let domain = test_tensor_domain(1_000_000_000);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain)
                .expect("large domain has valid dense strides"),
            domain,
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: fixture_span(),
        }],
    };

    block
        .validate_shape_contract("large compact tensor")
        .expect("compact tensor validation must not materialize domain points");
}

#[test]
fn tensor_shape_contract_accepts_empty_map_and_affine_stencil_domains() {
    let domain = test_tensor_domain(0);
    let output_map = TensorOutputMap::dense_contiguous(17, &domain)
        .expect("empty domain has valid dense strides");
    let node = |affine_stencil| {
        let base_ops = vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        if affine_stencil {
            ComputeNode::AffineStencil {
                domain: domain.clone(),
                output_map: output_map.clone(),
                base_ops,
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: TensorNodeMetadata::default(),
                span: fixture_span(),
            }
        } else {
            ComputeNode::Map {
                domain: domain.clone(),
                output_map: output_map.clone(),
                base_ops,
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: TensorNodeMetadata::default(),
                span: fixture_span(),
            }
        }
    };

    for tensor_node in [node(false), node(true)] {
        let block = ComputeBlock {
            nodes: vec![tensor_node],
        };
        block
            .validate_shape_contract("empty compact tensor")
            .expect("empty Map and AffineStencil domains are valid zero-iteration tensors");
        assert_eq!(block.len(), Ok(0));
        assert!(block.is_empty());
    }
}

#[test]
fn tensor_shape_contract_combines_duplicate_load_strides_before_range_validation() {
    let domain = test_tensor_domain(3);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain)
                .expect("three-element domain has valid dense strides"),
            domain,
            base_ops: vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![
                AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: isize::MAX,
                    }],
                },
                AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 1,
                    }],
                },
                AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: -isize::MAX,
                    }],
                },
            ],
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: fixture_span(),
        }],
    };

    block
        .validate_shape_contract("correlated load strides")
        .expect("the combined load stride is one");
}

#[test]
fn tensor_shape_contract_rejects_stride_metadata_at_the_ir_boundary() {
    let domain = test_tensor_domain(2);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain)
                .expect("two-element domain has valid dense strides"),
            domain,
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 0,
                terms: vec![AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: fixture_span(),
        }],
    };

    let error = block
        .validate_shape_contract("invalid load stride")
        .expect_err("a load stride on Const must fail in Solve IR");
    assert!(
        matches!(
            error,
            SolveProblemShapeContractError::AffineStrideOperation {
                actual: Some("Const"),
                ..
            }
        ),
        "{error}"
    );
}

#[test]
fn tensor_shape_contract_rejects_non_finite_combined_constant_stride() {
    let domain = test_tensor_domain(2);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain)
                .expect("two-element domain has valid dense strides"),
            domain,
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: vec![
                AffineStencilConstStride {
                    op_position: 0,
                    terms: vec![AffineStencilConstStrideTerm {
                        dimension: 0,
                        stride: f64::MAX,
                    }],
                },
                AffineStencilConstStride {
                    op_position: 0,
                    terms: vec![AffineStencilConstStrideTerm {
                        dimension: 0,
                        stride: f64::MAX,
                    }],
                },
            ],
            metadata: TensorNodeMetadata::default(),
            span: fixture_span(),
        }],
    };

    assert!(matches!(
        block.validate_shape_contract("constant stride overflow"),
        Err(
            SolveProblemShapeContractError::NonFiniteAffineConstantStride {
                op_position: 0,
                stride_dimension: 0,
                ..
            }
        )
    ));
}

#[test]
fn scalar_program_block_with_source_span_preserves_explicit_fixture_span() {
    let block = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_provenance(),
    )
    .expect("scalar fixture is computable");
    assert_eq!(block.program_spans(), [fixture_span()]);
}

fn make_layout(y_shapes: &[(&str, Vec<usize>)], p_shapes: &[(&str, Vec<usize>)]) -> VarLayout {
    let mut bindings = IndexMap::new();
    let mut shapes = IndexMap::new();
    let mut y_offset = 0usize;
    let mut p_offset = 0usize;
    for (name, shape) in y_shapes {
        let size: usize = shape.iter().product();
        bindings.insert(name.to_string(), scalar_slot_y(y_offset));
        shapes.insert(name.to_string(), shape.clone());
        y_offset += size;
    }
    for (name, shape) in p_shapes {
        let size: usize = shape.iter().product();
        bindings.insert(name.to_string(), scalar_slot_p(p_offset));
        shapes.insert(name.to_string(), shape.clone());
        p_offset += size;
    }
    let shape_spans = shapes
        .keys()
        .map(|name| (name.clone(), fixture_span()))
        .collect();
    VarLayout::from_parts_with_shapes_and_spans(bindings, shapes, shape_spans, y_offset, p_offset)
        .expect("representative Solve fixture layout should satisfy shape contract")
}

fn representative_solve_problem_fixture() -> SolveProblem {
    let layout = make_layout(
        &[("x", vec![1]), ("y", vec![1]), ("hold.y", vec![1])],
        &[
            ("p", vec![1]),
            ("d", vec![2]),
            ("__pre__.d", vec![2]),
            ("__root_relation__", vec![1]),
            ("__clock_activation__", vec![1]),
        ],
    );
    let solve_layout = representative_solve_layout();
    let continuous = representative_continuous_system(&solve_layout);
    let initialization = representative_initialization_system();
    let events = representative_event_partition();
    let (discrete, projection) =
        representative_discrete_system(&solve_layout, &events.root_relation_memory_targets);
    let discrete = projection
        .prepare(discrete, events)
        .expect("representative discrete projection remains exactly applicable");
    SolveProblem::construct_prepared(
        layout,
        solve_layout,
        continuous,
        initialization,
        discrete,
        representative_clock_partition(),
    )
    .expect("the representative SolveProblem is valid by construction")
}

fn representative_solver_maps() -> SolverNameIndexMaps {
    let mut name_to_idx = IndexMap::new();
    name_to_idx.insert("x".to_string(), 0);
    name_to_idx.insert("y".to_string(), 1);
    name_to_idx.insert("hold.y".to_string(), 2);

    let mut base_to_indices = IndexMap::new();
    base_to_indices.insert("x".to_string(), vec![0]);
    base_to_indices.insert("y".to_string(), vec![1]);
    base_to_indices.insert("hold.y".to_string(), vec![2]);

    SolverNameIndexMaps {
        names: vec!["x".to_string(), "y".to_string(), "hold.y".to_string()],
        name_to_idx,
        base_to_indices,
    }
}

fn representative_solve_layout() -> SolveLayout {
    SolveLayout {
        solver_maps: representative_solver_maps(),
        variable_storage_runs: vec![
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(0),
                scalar_count: 1,
                role: SolveVariableStorageRole::State,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(1),
                scalar_count: 1,
                role: SolveVariableStorageRole::Algebraic,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(2),
                scalar_count: 1,
                role: SolveVariableStorageRole::Output,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::P(0),
                scalar_count: 1,
                role: SolveVariableStorageRole::Parameter,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::P(1),
                scalar_count: 2,
                role: SolveVariableStorageRole::DiscreteReal,
                value_kind: SolveVariableValueKind::Real,
            },
        ],
        variable_declarations: vec![
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::State,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Algebraic,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Output,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Parameter,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::DiscreteReal,
                SolveVariableValueKind::Real,
            ),
        ],
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        output_scalar_count: 1,
        parameter_count: 1,
        static_parameter_names: vec!["p".to_string()],
        compiled_parameter_len: 7,
        discrete_real_scalar_names: vec!["d[1]".to_string(), "d[2]".to_string()],
        relation_memory_parameter_indices: vec![5],
        pre_param_bindings: vec![
            PreParamBinding {
                dest_p_index: 3,
                source: PreParamSource::P { index: 1 },
                clock_schedule: None,
            },
            PreParamBinding {
                dest_p_index: 4,
                source: PreParamSource::P { index: 2 },
                clock_schedule: None,
            },
        ],
        ..SolveLayout::default()
    }
}

fn representative_continuous_system(solve_layout: &SolveLayout) -> ContinuousSolveSystem {
    let implicit_rhs = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_source_span(
                vec![vec![
                    LinearOp::LoadY { dst: 0, index: 0 },
                    LinearOp::LoadP { dst: 1, index: 0 },
                    LinearOp::Binary {
                        dst: 2,
                        op: BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ]],
                fixture_provenance(),
            )
            .expect("implicit scalar fixture is computable"),
        )],
    };
    ContinuousSolveSystem::construct(
        solve_layout,
        ContinuousSolveSystemInputs::new(
            implicit_rhs,
            vec![Some(scalar_slot_y(1))],
            AlgebraicProjectionPlan {
                blocks: vec![AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![1],
                    tearing: None,
                }],
            },
            ComputeBlock::from_scalar_program_block(
                ScalarProgramBlock::with_source_span(
                    vec![vec![
                        LinearOp::LoadY { dst: 0, index: 1 },
                        LinearOp::StoreOutput { src: 0 },
                    ]],
                    fixture_provenance(),
                )
                .expect("residual scalar fixture is computable"),
            ),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            representative_derivative_rhs(),
            ContinuousRefreshPlanInputs::empty(),
        ),
    )
    .expect("representative fixture has a checked continuous system")
}

fn representative_derivative_rhs() -> ComputeBlock {
    ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::LoadP { dst: 0, index: 0 }],
            lhs_start: 0,
            rhs_ops: vec![LinearOp::LoadY { dst: 1, index: 0 }],
            rhs_start: 1,
            m: 1,
            k: 1,
            n: 1,
            lhs_pattern: crate::fixture_pattern(1, 1, true),
            rhs_pattern: crate::fixture_pattern(1, 1, false),
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    }
}

fn representative_initialization_system() -> InitializationSolveSystem {
    InitializationSolveSystem::construct(
        ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(
                vec![vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    LinearOp::StoreOutput { src: 0 },
                ]],
                fixture_provenance(),
            )
            .expect("initial scalar fixture is computable"),
        ),
        vec![Some(scalar_slot_y(1))],
        vec![InitializationRowRole::Solved],
        1,
        vec![scalar_slot_y(1)],
        InitializationProjectionPlan {
            blocks: vec![InitializationProjectionBlock {
                rows: vec![0],
                unknowns: vec![scalar_slot_y(1)],
            }],
        },
        (ScalarProgramBlock::default(), Vec::new()),
    )
    .expect("the representative initialization system is exactly correlated")
}

fn representative_discrete_system(
    solve_layout: &SolveLayout,
    root_relation_memory_targets: &[Option<ScalarSlot>],
) -> (DiscreteSolveSystem, RuntimeAssignmentProjection) {
    let rhs = ScalarProgramBlock::with_source_span(
        vec![
            vec![
                LinearOp::LoadP { dst: 0, index: 5 },
                LinearOp::Const { dst: 1, value: 1.0 },
                LinearOp::Const { dst: 2, value: 0.0 },
                LinearOp::Select {
                    dst: 3,
                    cond: 0,
                    if_true: 1,
                    if_false: 2,
                },
                LinearOp::StoreOutput { src: 3 },
            ],
            vec![
                LinearOp::LoadP { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_provenance(),
    )
    .expect("discrete scalar fixture is computable");
    let update_targets = vec![scalar_slot_p(1), scalar_slot_p(2)];
    let row_roles = vec![DiscreteRowRole::Equation; 2];
    let pre_modes = vec![DiscreteEventPreMode::FollowCurrent; 2];
    let clock_owners = vec![None; 2];
    let projection = derive_runtime_assignment_projection(
        solve_layout,
        &rhs,
        &update_targets,
        &row_roles,
        &pre_modes,
        &clock_owners,
        root_relation_memory_targets,
    )
    .expect("representative runtime rows derive from their main B.1c owners");
    let runtime_assignment_rhs = projection.rhs().clone();
    let runtime_assignment_targets = projection.targets().to_vec();
    let runtime_assignment_source_rows = projection.source_rows().to_vec();
    let runtime_assignment_roles = projection.roles().to_vec();
    let system = DiscreteSolveSystem {
        runtime_assignment_source_rows,
        runtime_assignment_rhs: runtime_assignment_rhs.clone(),
        runtime_assignment_targets: runtime_assignment_targets.clone(),
        runtime_assignment_roles,
        post_commit_assignment_rhs: projection.post_commit_rhs().clone(),
        post_commit_assignment_targets: projection.post_commit_targets().to_vec(),
        post_commit_assignment_runtime_rows: projection.post_commit_runtime_rows().to_vec(),
        rhs,
        update_targets,
        row_roles,
        pre_modes,
        observation_refresh: vec![false; 2],
        observation_refresh_reads_y: false,
        integrator_history_effects: vec![IntegratorHistoryEffect::Restart; 2],
        clock_owners,
        event_iteration_plan: EventIterationPlan {
            runs: vec![EventIterationRun {
                variable: 4,
                pre_binding_start: 0,
                owner: EventIterationOwner::ScalarRows { start_row: 0 },
            }],
        },
        guarded_assignments: Vec::new(),
        event_transactions: Vec::new(),
        structured_rhs: ComputeBlock::default(),
        structured_updates: Vec::new(),
        clock_partition_order: Vec::new(),
        clock_partition_intermediates: ScalarProgramBlock::default(),
        clock_partition_intermediate_targets: Vec::new(),
        clock_partition_intermediate_clocks: Vec::new(),
    };
    (system, projection)
}

fn structured_discrete_fixture(base: ScalarSlot) -> (ComputeBlock, StructuredDiscreteUpdate) {
    let domain = test_tensor_domain(2);
    let node = ComputeNode::Map {
        output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
        domain: domain.clone(),
        base_ops: vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: Vec::new(),
        const_strides: Vec::new(),
        metadata: TensorNodeMetadata::default(),
        span: fixture_span(),
    };
    let update = StructuredDiscreteUpdate {
        node_index: 0,
        target: StructuredDiscreteTargetMap {
            base,
            map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
        },
        role: DiscreteRowRole::EventAction,
        pre_mode: DiscreteEventPreMode::FollowCurrent,
        observation_refresh: false,
        integrator_history_effect: IntegratorHistoryEffect::Preserve,
        clock_owner: None,
    };
    (ComputeBlock { nodes: vec![node] }, update)
}

fn guarded_range_program(output_count: usize) -> Vec<LinearOp> {
    let mut program = (0..output_count)
        .map(|index| LinearOp::Const {
            dst: index as Reg,
            value: index as f64,
        })
        .collect::<Vec<_>>();
    program.push(LinearOp::StoreOutputRange {
        start: 0,
        count: output_count,
        stride: 1,
    });
    program
}

#[test]
fn guarded_assignment_program_owns_compact_target_ranges() {
    let program = GuardedAssignmentProgram::checked(GuardedAssignmentProgramInput {
        program: guarded_range_program(3),
        provenance: fixture_provenance(),
        target_ranges: vec![(scalar_slot_p(2), 3)],
        role: DiscreteRowRole::Equation,
        pre_mode: DiscreteEventPreMode::Fixed,
        observation_refresh: false,
        integrator_history_effect: IntegratorHistoryEffect::Restart,
        clock_owner: None,
    })
    .expect("one compact guarded range is valid");

    assert_eq!(program.output_count(), 3);
    assert_eq!(program.register_count(), 3);
    assert_eq!(program.target_ranges().len(), 1);
    assert_eq!(program.target_ranges()[0].base(), scalar_slot_p(2));
    assert_eq!(program.target_ranges()[0].count(), 3);
}

#[test]
fn discrete_optional_clock_owner_keys_reject_omission() {
    let guarded = GuardedAssignmentProgram::checked(GuardedAssignmentProgramInput {
        program: guarded_range_program(1),
        provenance: fixture_provenance(),
        target_ranges: vec![(scalar_slot_p(0), 1)],
        role: DiscreteRowRole::Equation,
        pre_mode: DiscreteEventPreMode::Fixed,
        observation_refresh: false,
        integrator_history_effect: IntegratorHistoryEffect::Restart,
        clock_owner: None,
    })
    .expect("guarded fixture is valid");
    assert_missing_wire_field_rejected::<GuardedAssignmentProgram>(
        serde_json::to_value(guarded).expect("serialize guarded fixture"),
        "",
        "clock_owner",
    );

    let (_, update) = structured_discrete_fixture(scalar_slot_p(0));
    assert_missing_wire_field_rejected::<StructuredDiscreteUpdate>(
        serde_json::to_value(update).expect("serialize structured update"),
        "",
        "clock_owner",
    );
}

#[test]
fn guarded_assignment_program_rejects_overlapping_target_ranges() {
    let error = GuardedAssignmentProgram::checked(GuardedAssignmentProgramInput {
        program: guarded_range_program(4),
        provenance: fixture_provenance(),
        target_ranges: vec![(scalar_slot_p(0), 2), (scalar_slot_p(1), 2)],
        role: DiscreteRowRole::Equation,
        pre_mode: DiscreteEventPreMode::Fixed,
        observation_refresh: false,
        integrator_history_effect: IntegratorHistoryEffect::Restart,
        clock_owner: None,
    })
    .expect_err("overlapping target ranges cannot be constructed");

    assert!(matches!(
        error,
        SolveProblemShapeContractError::GuardedAssignmentProgram {
            detail: "target ranges overlap",
            ..
        }
    ));
}

#[test]
fn guarded_assignment_wire_replays_output_width_proof() {
    let program = GuardedAssignmentProgram::checked(GuardedAssignmentProgramInput {
        program: guarded_range_program(2),
        provenance: fixture_provenance(),
        target_ranges: vec![(scalar_slot_p(0), 2)],
        role: DiscreteRowRole::EventAction,
        pre_mode: DiscreteEventPreMode::FollowCurrent,
        observation_refresh: false,
        integrator_history_effect: IntegratorHistoryEffect::Preserve,
        clock_owner: None,
    })
    .expect("guarded fixture is valid");
    let mut wire = serde_json::to_value(program).expect("serialize guarded fixture");
    wire["target_ranges"][0]["count"] = serde_json::json!(3);

    let error = serde_json::from_value::<GuardedAssignmentProgram>(wire)
        .expect_err("wire cannot forge target/output coverage");
    assert!(
        error
            .to_string()
            .contains("program output width does not equal its compact target ranges")
    );
}

#[test]
fn solve_variable_declaration_wire_replays_time_domain_proof() {
    let forged = serde_json::json!({
        "role": "State",
        "value_kind": "Real",
        "time_domain": "event_discontinuous"
    });
    let error = serde_json::from_value::<SolveVariableDeclaration>(forged)
        .expect_err("wire replay cannot forge an event-discontinuous state");
    assert!(
        error
            .to_string()
            .contains("State Real storage cannot be event-discontinuous")
    );

    let valid = SolveVariableDeclaration::event_discontinuous(
        SolveVariableStorageRole::Output,
        SolveVariableValueKind::Real,
    )
    .expect("real output admits the proved domain");
    let replayed = serde_json::from_value::<SolveVariableDeclaration>(
        serde_json::to_value(valid).expect("serialize declaration"),
    )
    .expect("valid declaration replays through its constructor");
    assert_eq!(replayed, valid);
}

fn event_transaction_fixture_with_table() -> (EventTransactionProgram, SolvePureCallTable) {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
        RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let tensor = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2]).unwrap();
    let mut table = SolvePureCallTable::builder(arithmetic);
    let owner = table
        .add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![tensor.clone()],
            vec![
                SolvePureCallOutput::result(tensor.clone()),
                SolvePureCallOutput::assertion_predicate(),
            ],
            fixture_span(),
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], fixture_span())?;
                builder.store(outputs[0], value, fixture_span())?;
                let predicate = builder.constant(SolveValue::boolean(true), fixture_span())?;
                builder.store(outputs[1], predicate, fixture_span())
            },
        )
        .unwrap();
    let transaction = EventTransactionProgram::checked(
        EventTransactionConstruction {
            site: table.call_site(owner).unwrap(),
            inputs: vec![(scalar_slot_p(2), tensor.clone())],
            targets: vec![(scalar_slot_p(0), tensor, None)],
            producer_owners: vec![EventTransactionProducerOwner::ScalarRows { start_row: 0 }],
            assertions: vec![SolveEventAction {
                kind: SolveEventActionKind::Assert,
                message: SolveEventMessage {
                    parts: vec![SolveEventMessagePart::Text("checked".to_string())],
                },
                span: fixture_span(),
                origin: "source".to_string(),
                clock_owner: None,
            }],
            assertion_action_indices: vec![vec![0]],
            statement_count: 2,
            clock_owners: Vec::new(),
        },
        fixture_provenance(),
    )
    .expect("one aggregate input and atomic target form a transaction");
    (transaction, table.finish())
}

fn event_transaction_fixture() -> EventTransactionProgram {
    event_transaction_fixture_with_table().0
}

#[test]
fn event_transaction_owns_compact_typed_storage_ranges() {
    let transaction = event_transaction_fixture();
    assert_eq!(transaction.inputs().len(), 1);
    assert_eq!(transaction.inputs()[0].source(), scalar_slot_p(2));
    assert_eq!(transaction.inputs()[0].value_type().scalar_count(), 2);
    assert_eq!(transaction.targets().len(), 1);
    assert_eq!(transaction.targets()[0].base(), scalar_slot_p(0));
    assert_eq!(transaction.statement_count(), 2);
    assert_eq!(transaction.assertion_count(), 1);
}

#[test]
fn artifact_only_pure_call_requires_an_authorized_owner() {
    let (_, issued) = event_transaction_fixture_with_table();
    let site = issued.owners()[0].call_site().clone();
    let artifacts = ContinuousSolveArtifacts {
        implicit_jacobian_v_scalar: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::PureCall {
                    dst_start: 2,
                    input_starts: Box::new([0]),
                    site,
                },
                LinearOp::StoreOutputRange {
                    start: 2,
                    count: 3,
                    stride: 1,
                },
            ]],
            fixture_provenance(),
        )
        .expect("artifact-only typed call fixture is checked"),
        ..ContinuousSolveArtifacts::default()
    };
    let unauthorized = SolvePureCallTable::empty(issued.arithmetic());
    let mut validator = ModelPureCallSiteValidator {
        table: &unauthorized,
    };

    assert!(matches!(
        validator.visit_continuous_artifacts(&artifacts),
        Err(SolveProblemShapeContractError::PureCallSiteMismatch { .. })
    ));
}

#[test]
fn nested_directional_pure_call_requires_its_issuing_table() {
    let profile = binary64_first_product_profile();
    let real = SolveValueType::scalar(SolveScalarType::real(profile));
    let issued = SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(91).unwrap()),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            fixture_span(),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], fixture_span())?;
                builder.store(outputs[0], input, fixture_span())
            },
        )?;
        Ok(())
    })
    .expect("identity call owner has a directional relation");
    let site = issued.owners()[0]
        .call_site()
        .directional()
        .expect("identity call has a directional relation")
        .clone();
    let conditional = FunctionConditionalProgram::checked(
        2,
        [2],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCaptureRange {
                    dst_start: 0,
                    index_start: 0,
                    count: 2,
                },
                LinearOp::PureCallDirectional {
                    dst_start: 2,
                    input_starts: Box::new([0, 1]),
                    site,
                },
                LinearOp::StoreOutputRange {
                    start: 2,
                    count: 2,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ],
    )
    .expect("nested directional call fixture is checked");
    let operations = [LinearOp::FunctionConditional {
        dst_start: 2,
        capture_start: 0,
        program: std::sync::Arc::new(conditional),
    }];
    let (_, foreign) = event_transaction_fixture_with_table();
    let mut validator = ModelPureCallSiteValidator { table: &foreign };

    assert!(matches!(
        validator.visit_linear_op_slice(
            LinearOpSliceOwner::ScalarProgram {
                owner: ScalarProgramBlockOwner::VisibleValueRows,
                program_index: 0,
                span: Some(fixture_span()),
            },
            &operations,
        ),
        Err(SolveProblemShapeContractError::PureCallSiteMismatch { .. })
    ));
}

#[test]
fn event_transaction_wire_rejects_a_forged_target_type() {
    let mut wire = serde_json::to_value(event_transaction_fixture()).unwrap();
    wire["targets"][0]["value_type"]["dimensions"] = serde_json::json!([3]);
    wire["targets"][0]["value_type"]["scalar_count"] = serde_json::json!(3);
    let error = serde_json::from_value::<EventTransactionProgram>(wire).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("typed call outputs do not match atomic targets"),
        "wire replay derives the target interface from its issued site: {error}"
    );
}

#[test]
fn event_transaction_wire_rejects_a_result_in_its_assertion_suffix() {
    let mut wire = serde_json::to_value(event_transaction_fixture()).unwrap();
    wire["site"]["outputs"][1]["kind"] = serde_json::json!("result");
    let error = serde_json::from_value::<EventTransactionProgram>(wire).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("transaction suffix is not a checked assertion-predicate tuple"),
        "wire replay rejects a non-predicate suffix: {error}"
    );
}

#[test]
fn event_transaction_wire_requires_complete_producer_and_action_projections() {
    let mut missing_owner = serde_json::to_value(event_transaction_fixture()).unwrap();
    missing_owner["producer_owners"] = serde_json::json!([]);
    let owner_error = serde_json::from_value::<EventTransactionProgram>(missing_owner).unwrap_err();
    assert!(
        owner_error
            .to_string()
            .contains("producer projections do not cover the complete target tuple")
    );

    let mut missing_action = serde_json::to_value(event_transaction_fixture()).unwrap();
    missing_action["assertion_action_indices"][0] = serde_json::json!([]);
    let action_error =
        serde_json::from_value::<EventTransactionProgram>(missing_action).unwrap_err();
    assert!(
        action_error
            .to_string()
            .contains("one assertion predicate has no event-action projection")
    );
}

#[test]
fn solve_model_wire_rejects_a_forged_event_transaction_call_owner() {
    let (problem, pure_calls) = transaction_owner_problem_fixture();
    let entries = transaction_owner_catalog_entries;
    let visible_rows = transaction_owner_visible_rows;
    let runtime = || SolveModelRuntimeInputs {
        initial_y: Vec::new(),
        solver_nominals: Vec::new(),
        parameters: vec![0.0; 6],
    };
    SolveModel::construct(
        problem.clone(),
        pure_calls.clone(),
        SolveArtifactInputs::empty(),
        runtime(),
        visible_rows(),
        entries(),
    )
    .expect("fixture has one exact issued owner");

    let mut wire = serde_json::to_value(&problem).unwrap();
    wire["discrete"]["event_transactions"][0]["site"]["owner"] = serde_json::json!(1);
    let forged = serde_json::from_value::<SolveProblem>(wire).unwrap();
    let error = SolveModel::construct(
        forged,
        pure_calls,
        SolveArtifactInputs::empty(),
        runtime(),
        visible_rows(),
        entries(),
    )
    .expect_err("a forged transaction owner cannot seal a Solve root");
    assert!(
        error.to_string().contains(
            "discrete.event_transactions references pure-call owner 1 with a missing or mismatched interface"
        ),
        "wire replay must prove the exact model-level owner link: {error}"
    );
}

fn transaction_owner_catalog_entries() -> Vec<SolveVariableCatalogSourceEntry> {
    vec![(
        SolveVariableSource::new(
            SourceOccurrenceId::try_from(InstanceId::new(1))
                .expect("fixture source occurrence is explicitly nonzero"),
            "x".to_string(),
            vec![2],
            vec!["x[1]".to_string(), "x[2]".to_string()],
            fixture_span(),
        ),
        SolveVariableSourceAttributes::new(
            SolveVariableCausality::Local,
            SolveVariableVariability::Discrete,
            false,
            None,
            None,
            rumoca_core::Fixity::Free,
        ),
        SolveVariableEvaluatedValues::new(Some(vec![0.0, 0.0]), None, None, None),
    )]
}

fn transaction_owner_visible_rows() -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        vec![
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadP { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_provenance(),
    )
    .expect("visible fixture rows are computable")
}

fn transaction_owner_problem_fixture() -> (SolveProblem, SolvePureCallTable) {
    let (transaction, pure_calls) = event_transaction_fixture_with_table();
    let solve_layout = SolveLayout {
        variable_storage_runs: vec![SolveVariableStorageRun {
            base: crate::SolveStorageCoordinate::P(0),
            scalar_count: 2,
            role: SolveVariableStorageRole::DiscreteReal,
            value_kind: SolveVariableValueKind::Real,
        }],
        variable_declarations: vec![SolveVariableDeclaration::new(
            SolveVariableStorageRole::DiscreteReal,
            SolveVariableValueKind::Real,
        )],
        compiled_parameter_len: 6,
        discrete_real_scalar_names: vec!["x[1]".into(), "x[2]".into()],
        pre_param_bindings: vec![
            PreParamBinding {
                dest_p_index: 4,
                source: PreParamSource::P { index: 0 },
                clock_schedule: None,
            },
            PreParamBinding {
                dest_p_index: 5,
                source: PreParamSource::P { index: 1 },
                clock_schedule: None,
            },
        ],
        ..SolveLayout::default()
    };
    let discrete = DiscreteSolveSystem {
        rhs: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_provenance(),
        )
        .unwrap(),
        update_targets: vec![scalar_slot_p(0), scalar_slot_p(1)],
        row_roles: vec![DiscreteRowRole::Equation; 2],
        pre_modes: vec![DiscreteEventPreMode::FollowCurrent; 2],
        observation_refresh: vec![false; 2],
        integrator_history_effects: vec![IntegratorHistoryEffect::Preserve; 2],
        clock_owners: vec![None; 2],
        event_iteration_plan: EventIterationPlan {
            runs: vec![EventIterationRun {
                variable: 0,
                pre_binding_start: 0,
                owner: EventIterationOwner::EventTransaction {
                    program_index: 0,
                    target_index: 0,
                },
            }],
        },
        event_transactions: vec![transaction.clone()],
        ..DiscreteSolveSystem::default()
    };
    let events = SolveEventPartition {
        actions: transaction.assertions().to_vec(),
        action_conditions: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_provenance(),
        )
        .unwrap(),
        ..SolveEventPartition::default()
    };
    let problem = SolveProblem {
        layout: VarLayout::from_parts(IndexMap::new(), 0, 6),
        solve_layout,
        discrete,
        events,
        ..empty_solve_problem()
    };
    (problem, pure_calls)
}

fn representative_event_partition() -> SolveEventPartition {
    SolveEventPartition {
        root_conditions: ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadTime { dst: 0 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Ge,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            fixture_provenance(),
        )
        .expect("root scalar fixture is computable"),
        root_relation_memory_targets: vec![Some(scalar_slot_p(5))],
        root_zero_domains: vec![RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![RootRelationRefreshRole::Frozen],
        scheduled_time_events: vec![0.1],
        ..SolveEventPartition::default()
    }
}

fn representative_clock_partition() -> SolveClockPartition {
    SolveClockPartition {
        periodic_event_schedules: vec![
            PeriodicEventSchedule::new(rumoca_core::ClockLattice::from_seconds(0.1, 0.0).unwrap())
                .unwrap(),
        ],
        activation_parameter_indices: vec![6],
    }
}

#[test]
fn periodic_schedule_resolves_start_relative_anchor_at_instance_boundary() {
    let lattice = rumoca_core::ClockLattice::from_seconds(0.25, 0.25).unwrap();
    let schedule = rumoca_core::PeriodicClockSchedule::simulation_start_relative(lattice).unwrap();
    let schedule = PeriodicEventSchedule::from_schedule(schedule).unwrap();

    let resolved = schedule.resolved_at(2.0).unwrap();
    assert_eq!(resolved.anchor(), rumoca_core::ClockPhaseAnchor::Absolute);
    assert_eq!(resolved.phase_seconds(), 2.25);
    assert_eq!(
        schedule.anchor(),
        rumoca_core::ClockPhaseAnchor::SimulationStart,
        "compile-time Solve IR must remain independent of instance startTime"
    );
}

fn assert_same_json_shape<T: serde::Serialize>(actual: &T, expected: &T) {
    assert_eq!(
        serde_json::to_value(actual).expect("serialize actual"),
        serde_json::to_value(expected).expect("serialize expected")
    );
}

#[test]
fn y_slice_returns_some_for_y_array_variable() {
    let layout = make_layout(&[("x", vec![3, 3])], &[]);
    let src = layout
        .y_slice("x")
        .expect("3×3 Y-slot variable should yield YSlice");
    assert!(matches!(src, TensorSource::YSlice { start: 0, shape } if shape == [3, 3]));
}

#[test]
fn p_slice_returns_some_for_p_array_variable() {
    let layout = make_layout(&[], &[("A", vec![2, 4])]);
    let src = layout
        .p_slice("A")
        .expect("2×4 P-slot variable should yield PSlice");
    assert!(matches!(src, TensorSource::PSlice { start: 0, shape } if shape == [2, 4]));
}

#[test]
fn indexed_bindings_are_derived_from_shape_metadata() {
    let layout = make_layout(&[("body.frame.R.T", vec![3, 3])], &[]);
    let entries = layout
        .indexed_bindings()
        .get(&ComponentReferenceKey::generated("body.frame.R.T"))
        .expect("array layout should expose structured scalar slots");

    assert_eq!(entries.len(), 9);
    assert_eq!(entries[0].indices, vec![1, 1]);
    assert!(matches!(entries[0].slot, ScalarSlot::Y { index: 0 }));
    assert_eq!(entries[8].indices, vec![3, 3]);
    assert!(matches!(entries[8].slot, ScalarSlot::Y { index: 8 }));
}

#[test]
fn scalar_program_block_rejects_span_count_mismatch_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("bad_scalar_spans.mo"), 2, 5);

    let err = ScalarProgramBlock::with_program_spans(
        vec![vec![LinearOp::StoreOutput { src: 0 }]],
        vec![span, span],
    )
    .expect_err("explicit scalar row spans must match row count");

    assert!(matches!(
        err,
        SolveProblemShapeContractError::ScalarProgramSpanMismatch {
            programs: 1,
            spans: 2,
            span: actual,
            ..
        } if actual == Some(span)
    ));
}

#[test]
fn scalar_program_block_rejects_output_index_count_mismatch_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("bad_scalar_outputs.mo"), 7, 11);

    let err = ScalarProgramBlock::with_output_indices(
        vec![vec![LinearOp::StoreOutput { src: 0 }]],
        vec![span],
        vec![0, 1],
    )
    .expect_err("explicit scalar output indices must match row count");

    assert!(matches!(
        err,
        SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
            programs: 1,
            output_indices: 2,
            span: actual,
            ..
        } if actual == Some(span)
    ));
}

#[test]
fn y_slice_returns_none_for_p_slot_variable() {
    let layout = make_layout(&[], &[("p", vec![2])]);
    assert!(
        layout.y_slice("p").is_none(),
        "P-slot variable must not yield YSlice"
    );
}

#[test]
fn p_slice_returns_none_for_y_slot_variable() {
    let layout = make_layout(&[("x", vec![2])], &[]);
    assert!(
        layout.p_slice("x").is_none(),
        "Y-slot variable must not yield PSlice"
    );
}

#[test]
fn y_slice_returns_none_for_scalar_variable_without_shape() {
    let mut bindings = IndexMap::new();
    bindings.insert("s".to_string(), scalar_slot_y(0));
    let layout = VarLayout::from_parts_with_shapes_and_spans(
        bindings,
        IndexMap::new(),
        IndexMap::new(),
        1,
        0,
    )
    .expect("scalar variable fixture layout should satisfy shape contract");
    assert!(
        layout.y_slice("s").is_none(),
        "scalar variable with no recorded shape must not yield YSlice"
    );
}

#[test]
fn y_slice_returns_none_for_unknown_variable() {
    let layout = make_layout(&[("x", vec![2])], &[]);
    assert!(layout.y_slice("unknown").is_none());
}

fn serde_roundtrip_tensor_block_fixture() -> ComputeBlock {
    ComputeBlock {
        nodes: vec![
            serde_roundtrip_scalar_node(),
            serde_roundtrip_matmul_node(),
            serde_roundtrip_linsolve_node(),
            serde_roundtrip_map_node(),
            serde_roundtrip_affine_stencil_node(),
        ],
    }
}

fn serde_roundtrip_scalar_node() -> ComputeNode {
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_provenance(),
        )
        .expect("round-trip scalar fixture is computable"),
    )
}

fn serde_roundtrip_matmul_node() -> ComputeNode {
    ComputeNode::MatMul {
        lhs_ops: vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::Move { dst: 1, src: 0 },
        ],
        lhs_start: 1,
        rhs_ops: vec![
            LinearOp::LoadSeed { dst: 2, index: 0 },
            LinearOp::Move { dst: 3, src: 2 },
        ],
        rhs_start: 3,
        m: 1,
        k: 1,
        n: 1,
        lhs_pattern: crate::fixture_pattern(1, 1, true),
        rhs_pattern: crate::fixture_pattern(1, 1, false),
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn serde_roundtrip_linsolve_node() -> ComputeNode {
    ComputeNode::LinSolve {
        setup_ops: vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 1 },
            LinearOp::LoadP { dst: 2, index: 2 },
            LinearOp::LoadY { dst: 3, index: 0 },
        ],
        matrix_start: 0,
        rhs_start: 3,
        n: 2,
        next_reg: 4,
        matrix_pattern: crate::fixture_pattern(2, 2, false),
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn serde_roundtrip_map_node() -> ComputeNode {
    ComputeNode::Map {
        domain: test_tensor_domain(3),
        output_map: TensorOutputMap::dense_contiguous(0, &test_tensor_domain(3))
            .expect("valid dense output map"),
        base_ops: vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: Vec::new(),
        const_strides: vec![AffineStencilConstStride {
            op_position: 0,
            terms: vec![AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 1.0,
            }],
        }],
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn serde_roundtrip_affine_stencil_node() -> ComputeNode {
    ComputeNode::AffineStencil {
        domain: test_tensor_domain(8),
        output_map: TensorOutputMap::dense_contiguous(0, &test_tensor_domain(8))
            .expect("valid dense output map"),
        base_ops: vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: vec![AffineStencilLoadStride {
            op_position: 0,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: Vec::new(),
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn assert_tensor_node_tags_survive_json(json: &str) {
    for tag in [
        "MatMul",
        "LinSolve",
        "Map",
        "AffineStencil",
        "lhs_pattern",
        "metadata",
    ] {
        assert!(json.contains(tag), "{tag} must appear in JSON: {json}");
    }
}

fn assert_tensor_nodes_survive_roundtrip(back: &ComputeBlock) {
    assert_eq!(
        back.nodes.len(),
        5,
        "all five compute nodes must survive round-trip"
    );
    assert!(matches!(&back.nodes[0], ComputeNode::ScalarPrograms(_)));
    assert!(matches!(&back.nodes[2], ComputeNode::LinSolve { n: 2, .. }));
    assert!(matches!(&back.nodes[3], ComputeNode::Map { .. }));
    assert!(matches!(
        &back.nodes[4],
        ComputeNode::AffineStencil { domain, .. }
            if domain
                .scalar_count()
                .expect("fixture domain should have a valid scalar count")
                == 8
    ));
    assert_roundtrip_matmul_shape(&back.nodes[1]);
}

fn assert_roundtrip_matmul_shape(node: &ComputeNode) {
    let ComputeNode::MatMul {
        m: 1,
        k: 1,
        n: 1,
        lhs_pattern,
        metadata:
            TensorNodeMetadata {
                element_type: TensorElementType::Real64,
                layout: TensorLayout::RowMajorDense,
                scalar_fallback: ScalarFallback::Exact,
            },
        ..
    } = node
    else {
        panic!("round-tripped node should retain its MatMul shape and metadata");
    };
    assert!(matches!(
        lhs_pattern.view(),
        StructuralPatternView::Diagonal
    ));
}

#[test]
fn compute_block_tensor_nodes_survive_serde_roundtrip() {
    let block = serde_roundtrip_tensor_block_fixture();
    let json = serde_json::to_string(&block).expect("serialize ComputeBlock");
    assert_tensor_node_tags_survive_json(&json);

    let back: ComputeBlock = serde_json::from_str(&json).expect("deserialize ComputeBlock");
    assert_tensor_nodes_survive_roundtrip(&back);
}

#[test]
fn solve_problem_json_has_supported_schema_version() {
    let value = serde_json::to_value(empty_solve_problem()).expect("serialize SolveProblem");
    assert_eq!(
        value
            .get("schema_version")
            .and_then(serde_json::Value::as_u64),
        Some(u64::from(SOLVE_SCHEMA_VERSION))
    );

    let mut missing = value.clone();
    missing
        .as_object_mut()
        .expect("SolveProblem JSON should be object")
        .remove("schema_version");
    assert!(
        serde_json::from_value::<SolveProblem>(missing).is_err(),
        "SolveProblem JSON must carry an explicit schema_version"
    );

    for unsupported_version in [SOLVE_SCHEMA_VERSION - 1, SOLVE_SCHEMA_VERSION + 1] {
        let mut unsupported = value.clone();
        unsupported["schema_version"] = serde_json::json!(unsupported_version);
        let err = serde_json::from_value::<SolveProblem>(unsupported)
            .expect_err("unsupported SolveProblem schema version must fail");
        assert!(err.to_string().contains("unsupported Solve schema_version"));
    }
}

#[test]
fn solve_problem_json_requires_every_current_semantic_schedule_field() {
    let value =
        serde_json::to_value(representative_solve_problem_fixture()).expect("serialize fixture");
    for (object_pointer, field) in [
        ("/continuous/algebraic_projection_plan/blocks/0", "tearing"),
        ("/initialization", "mandatory_row_count"),
        ("/discrete", "observation_refresh_reads_y"),
        ("/discrete", "structured_rhs"),
        ("/discrete", "structured_updates"),
        ("/discrete", "clock_partition_order"),
        ("/discrete", "clock_partition_intermediates"),
        ("/discrete", "clock_partition_intermediate_targets"),
        ("/discrete", "clock_partition_intermediate_clocks"),
    ] {
        assert_missing_wire_field_rejected::<SolveProblem>(value.clone(), object_pointer, field);
    }
}

#[test]
fn nested_solve_wire_requires_current_identity_and_owner_fields() {
    let key_part = ComponentReferenceKeyPart {
        ident: VarName::new("x"),
        subscripts: Vec::new(),
    };
    assert_missing_wire_field_rejected::<ComponentReferenceKeyPart>(
        serde_json::to_value(key_part).expect("serialize component-reference key part"),
        "",
        "subscripts",
    );

    let action = SolveEventAction {
        kind: SolveEventActionKind::Assert,
        message: SolveEventMessage::default(),
        span: fixture_span(),
        origin: "source".to_string(),
        clock_owner: None,
    };
    assert_missing_wire_field_rejected::<SolveEventAction>(
        serde_json::to_value(action).expect("serialize event action"),
        "",
        "clock_owner",
    );

    let schedule = representative_clock_partition()
        .periodic_event_schedules
        .into_iter()
        .next()
        .expect("fixture has one periodic schedule");
    assert_missing_wire_field_rejected::<PeriodicEventSchedule>(
        serde_json::to_value(schedule).expect("serialize periodic schedule"),
        "",
        "anchor",
    );
}

#[test]
fn solve_wire_optional_keys_require_explicit_null_instead_of_omission() {
    let problem =
        serde_json::to_value(representative_solve_problem_fixture()).expect("serialize fixture");
    for (object_pointer, field) in [
        ("/solve_layout", "initial_event_parameter_index"),
        ("/solve_layout", "terminal_event_parameter_index"),
        ("/solve_layout", "initial_homotopy_parameter_index"),
        ("/solve_layout/pre_param_bindings/0", "clock_schedule"),
    ] {
        assert_missing_wire_field_rejected::<SolveProblem>(problem.clone(), object_pointer, field);
    }

    let transaction = serde_json::to_value(event_transaction_fixture()).expect("serialize fixture");
    assert_missing_wire_field_rejected::<EventTransactionProgram>(
        transaction,
        "/targets/0",
        "clock_owner",
    );

    let format = SolveStringConversionFormat::Options {
        minimum_length: None,
        left_justified: None,
        significant_digits: None,
    };
    let format = serde_json::to_value(format).expect("serialize string-format options");
    for field in ["minimum_length", "left_justified", "significant_digits"] {
        assert_missing_wire_field_rejected::<SolveStringConversionFormat>(
            format.clone(),
            "/Options",
            field,
        );
    }

    let metadata = SolveVariableMeta {
        name: "x".to_string(),
        source_span: fixture_span(),
        role: "state".to_string(),
        is_state: true,
        value_type: None,
        variability: None,
        time_domain: None,
        unit: None,
        start: None,
        min: None,
        max: None,
        nominal: None,
        fixed: rumoca_core::Fixity::Free,
        description: None,
    };
    let metadata = serde_json::to_value(metadata).expect("serialize variable metadata");
    for field in [
        "value_type",
        "variability",
        "time_domain",
        "unit",
        "start",
        "min",
        "max",
        "nominal",
        "fixed",
        "description",
    ] {
        assert_missing_wire_field_rejected::<SolveVariableMeta>(metadata.clone(), "", field);
    }
}

#[test]
fn mass_matrix_wire_format_stays_compact_and_roundtrips_sparse_entries() {
    let identity = serde_json::to_value(MassMatrix::Identity).expect("serialize identity");
    assert_eq!(identity, serde_json::json!({ "kind": "identity" }));

    let sparse = MassMatrix::Sparse {
        entries: vec![
            MassMatrixEntry {
                row: 0,
                column: 0,
                value: 2.0,
            },
            MassMatrixEntry {
                row: 1,
                column: 1,
                value: 3.0,
            },
        ],
    };
    let json = serde_json::to_string(&sparse).expect("serialize sparse mass matrix");
    let decoded: MassMatrix = serde_json::from_str(&json).expect("deserialize sparse mass matrix");

    assert_eq!(decoded, sparse);
}

#[test]
fn representative_solve_problem_json_roundtrip_preserves_schema_shape() {
    let problem = representative_solve_problem_fixture();
    let json = serde_json::to_string_pretty(&problem).expect("serialize SolveProblem");
    let decoded: SolveProblem = serde_json::from_str(&json).expect("deserialize SolveProblem");
    assert_same_json_shape(&decoded, &problem);
}

#[test]
fn solve_problem_json_requires_integrator_history_effects() {
    let mut value =
        serde_json::to_value(representative_solve_problem_fixture()).expect("serialize fixture");
    value["discrete"]
        .as_object_mut()
        .expect("discrete system is an object")
        .remove("integrator_history_effects");

    let error = serde_json::from_value::<SolveProblem>(value)
        .expect_err("integrator-history evidence must not default on the wire");
    assert!(
        error
            .to_string()
            .contains("missing field `integrator_history_effects`"),
        "unexpected omission error: {error}"
    );
}

#[test]
fn solve_problem_json_requires_post_commit_certificates() {
    let value =
        serde_json::to_value(representative_solve_problem_fixture()).expect("serialize fixture");
    for (section, field) in [
        ("discrete", "runtime_assignment_source_rows"),
        ("discrete", "runtime_assignment_roles"),
        ("discrete", "post_commit_assignment_rhs"),
        ("discrete", "post_commit_assignment_targets"),
        ("discrete", "post_commit_assignment_runtime_rows"),
        ("events", "root_relation_refresh_roles"),
    ] {
        let mut omitted = value.clone();
        omitted[section]
            .as_object_mut()
            .expect("Solve section is an object")
            .remove(field);
        let error = serde_json::from_value::<SolveProblem>(omitted)
            .expect_err("certificate fields must not default on the wire");
        assert!(
            error
                .to_string()
                .contains(&format!("missing field `{field}`")),
            "unexpected omission error for {field}: {error}"
        );
    }
}

#[test]
fn solve_problem_shape_rejects_forged_runtime_assignment_role() {
    let mut problem = representative_solve_problem_fixture();
    problem.discrete.runtime_assignment_roles[0] = RuntimeAssignmentRole::RelationEvaluating;
    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_roles",
            row: 0,
            ..
        })
    ));
}

fn replace_representative_runtime_projection(problem: &mut SolveProblem, source_rows: &[usize]) {
    let programs = source_rows
        .iter()
        .map(|source_row| {
            let program = problem
                .discrete
                .rhs
                .program_index_for_output(*source_row)
                .expect("hostile source row resolves");
            problem
                .discrete
                .rhs
                .program(program)
                .expect("hostile source program resolves")
                .to_vec()
        })
        .collect::<Vec<_>>();
    let spans = source_rows
        .iter()
        .map(|source_row| {
            problem
                .discrete
                .rhs
                .span_for_output(*source_row)
                .expect("hostile source row has provenance")
        })
        .collect::<Vec<_>>();
    let targets = source_rows
        .iter()
        .map(|source_row| problem.discrete.update_targets[*source_row])
        .collect::<Vec<_>>();
    let rhs = ScalarProgramBlock::with_program_spans(programs, spans)
        .expect("hostile projection rows remain computable");
    problem.discrete.runtime_assignment_source_rows = source_rows.to_vec();
    problem.discrete.runtime_assignment_rhs = rhs.clone();
    problem.discrete.runtime_assignment_targets = targets.clone();
    problem.discrete.runtime_assignment_roles = derive_runtime_assignment_roles(
        &rhs,
        &targets,
        &problem.solve_layout.relation_memory_parameter_indices,
    )
    .expect("hostile projection roles remain derivable");
    problem.discrete.post_commit_assignment_rhs = rhs;
    problem.discrete.post_commit_assignment_targets = targets;
    problem.discrete.post_commit_assignment_runtime_rows = (0..source_rows.len()).collect();
}

fn representative_first_runtime_program(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadP { dst: 0, index: 5 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value },
        LinearOp::Select {
            dst: 3,
            cond: 0,
            if_true: 1,
            if_false: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ]
}

fn replace_representative_source_and_runtime_constant(
    problem: &mut SolveProblem,
    source_value: f64,
    runtime_value: f64,
) {
    let second = problem
        .discrete
        .rhs
        .program(1)
        .expect("representative second source program exists")
        .to_vec();
    let spans = problem.discrete.rhs.program_spans().to_vec();
    problem.discrete.rhs = ScalarProgramBlock::with_program_spans(
        vec![
            representative_first_runtime_program(source_value),
            second.clone(),
        ],
        spans.clone(),
    )
    .expect("hostile source block remains computable");
    let runtime = ScalarProgramBlock::with_program_spans(
        vec![representative_first_runtime_program(runtime_value), second],
        spans,
    )
    .expect("hostile runtime block remains computable");
    problem.discrete.runtime_assignment_rhs = runtime.clone();
    problem.discrete.post_commit_assignment_rhs = runtime;
}

#[test]
fn solve_problem_rejects_omitted_extra_and_reordered_runtime_source_projections() {
    for source_rows in [&[0][..], &[0, 1, 1][..], &[1, 0][..]] {
        let mut problem = representative_solve_problem_fixture();
        replace_representative_runtime_projection(&mut problem, source_rows);
        assert!(matches!(
            problem.validate_constructed_parts(),
            Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "discrete.runtime_assignment_source_rows",
                ..
            })
        ));
    }
}

#[test]
fn solve_problem_rejects_runtime_witness_relabel_without_copy_change() {
    let mut problem = representative_solve_problem_fixture();
    problem.discrete.runtime_assignment_source_rows[0] = 1;
    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_source_rows",
            row: 0,
            ..
        })
    ));
}

#[test]
fn solve_problem_rejects_runtime_program_not_copied_from_witness() {
    let mut problem = representative_solve_problem_fixture();
    let wrong_program = problem
        .discrete
        .rhs
        .program(1)
        .expect("second source program exists")
        .to_vec();
    problem.discrete.runtime_assignment_rhs = ScalarProgramBlock::with_program_spans(
        vec![
            wrong_program,
            problem
                .discrete
                .rhs
                .program(1)
                .expect("second source program exists")
                .to_vec(),
        ],
        problem.discrete.rhs.program_spans().to_vec(),
    )
    .expect("hostile runtime block remains computable");
    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_source_rows",
            row: 0,
            ..
        })
    ));
}

#[test]
fn runtime_projection_copy_distinguishes_positive_and_negative_zero() {
    let mut problem = representative_solve_problem_fixture();
    replace_representative_source_and_runtime_constant(&mut problem, 0.0, -0.0);
    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_source_rows",
            row: 0,
            ..
        })
    ));
}

#[test]
fn runtime_projection_copy_accepts_an_unchanged_nan_payload() {
    let mut problem = representative_solve_problem_fixture();
    let nan = f64::from_bits(0x7ff8_0000_0000_0044);
    replace_representative_source_and_runtime_constant(&mut problem, nan, nan);
    problem
        .validate_constructed_parts()
        .expect("same-bit NaN instructions retain exact source identity");
}

fn nested_fold_instruction(value: f64, guarded: bool) -> LinearOp {
    let program = std::sync::Arc::new(
        FunctionFoldProgram::checked(
            StructuredIndexDomain {
                binders: Vec::new(),
            },
            1,
            0,
            vec![
                LinearOp::Const { dst: 0, value },
                LinearOp::StoreOutput { src: 0 },
            ],
        )
        .expect("nested exact-equality fixture is a checked fold"),
    );
    if guarded {
        LinearOp::GuardedFunctionFold {
            dst_start: 0,
            initial_start: 0,
            capture_start: 0,
            activation: 1,
            program,
        }
    } else {
        LinearOp::FunctionFold {
            dst_start: 0,
            initial_start: 0,
            capture_start: 0,
            program,
        }
    }
}

fn nested_conditional_instruction(value: f64) -> LinearOp {
    let output = |value| {
        vec![
            LinearOp::Const { dst: 0, value },
            LinearOp::StoreOutput { src: 0 },
        ]
    };
    let program =
        FunctionConditionalProgram::checked(0, [1], [(output(1.0), output(value))], output(value))
            .expect("nested exact-equality fixture is a checked conditional");
    LinearOp::FunctionConditional {
        dst_start: 0,
        capture_start: 0,
        program: std::sync::Arc::new(program),
    }
}

fn nested_store_fold_instruction(value: f64) -> LinearOp {
    let LinearOp::FunctionFold { program, .. } = nested_fold_instruction(value, false) else {
        unreachable!("fixture constructor always returns a fold")
    };
    LinearOp::StoreOutputFunctionFold {
        initial: vec![FoldInitialSource::Registers { start: 0, count: 1 }].into(),
        capture_start: 0,
        program,
        result_base: 0,
        count: 1,
        condition: None,
        nested_when_true: false,
    }
}

#[test]
fn recursive_instruction_identity_is_bit_exact_in_every_nested_program_owner() {
    let nan = f64::from_bits(0x7ff8_0000_0000_0044);
    for constructor in [
        |value| nested_fold_instruction(value, false),
        |value| nested_fold_instruction(value, true),
        nested_conditional_instruction,
        nested_store_fold_instruction,
    ] {
        assert!(!constructor(0.0).bitwise_eq(&constructor(-0.0)));
        assert!(constructor(nan).bitwise_eq(&constructor(nan)));
    }
}

#[test]
fn solve_problem_rejects_runtime_projection_from_y_backed_owner() {
    for forged_target in [scalar_slot_y(0), scalar_slot_y(2)] {
        let mut problem = representative_solve_problem_fixture();
        problem.discrete.update_targets[0] = forged_target;
        problem.discrete.runtime_assignment_targets[0] = forged_target;
        problem.discrete.post_commit_assignment_targets[0] = forged_target;
        let derived = derive_runtime_assignment_projection(
            &problem.solve_layout,
            &problem.discrete.rhs,
            &problem.discrete.update_targets,
            &problem.discrete.row_roles,
            &problem.discrete.pre_modes,
            &problem.discrete.clock_owners,
            &problem.events.root_relation_memory_targets,
        )
        .expect("a Y-backed source is ineligible rather than projectable");
        assert!(derived.source_rows().is_empty());
        problem
            .validate_constructed_parts()
            .expect_err("a carried Y-backed runtime projection must reject");
    }
}

#[test]
fn solve_problem_rejects_runtime_projection_from_ineligible_main_row() {
    for mutation in 0..2 {
        let mut problem = representative_solve_problem_fixture();
        match mutation {
            0 => problem.discrete.row_roles[0] = DiscreteRowRole::EventAction,
            1 => problem.discrete.pre_modes[0] = DiscreteEventPreMode::Fixed,
            _ => unreachable!(),
        }
        let error = problem
            .validate_constructed_parts()
            .expect_err("ineligible source row must reject");
        assert!(
            matches!(
                error,
                SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.runtime_assignment_source_rows",
                    ..
                }
            ),
            "unexpected error: {error:?}"
        );
    }

    let mut problem = representative_solve_problem_fixture();
    problem.discrete.clock_owners[0] = problem.clocks.periodic_clock_id(0);
    assert!(problem.validate_constructed_parts().is_err());

    let mut problem = representative_solve_problem_fixture();
    problem.discrete.update_targets[0] = scalar_slot_p(0);
    problem.discrete.runtime_assignment_targets[0] = scalar_slot_p(0);
    problem.discrete.post_commit_assignment_targets[0] = scalar_slot_p(0);
    assert!(problem.validate_constructed_parts().is_err());
}

#[test]
fn runtime_projection_refuses_future_unclocked_multi_output_owner_explicitly() {
    let problem = representative_solve_problem_fixture();
    let rhs = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::LoadP { dst: 0, index: 5 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ]],
        fixture_provenance(),
    )
    .expect("multi-output hostile source is computable");
    let error = match derive_runtime_assignment_projection(
        &problem.solve_layout,
        &rhs,
        &problem.discrete.update_targets,
        &problem.discrete.row_roles,
        &problem.discrete.pre_modes,
        &problem.discrete.clock_owners,
        &problem.events.root_relation_memory_targets,
    ) {
        Ok(_) => panic!("an eligible multi-output source cannot be silently omitted"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_source_rows",
            row: 0,
            detail: "root-reachable multi-output scalar source needs an explicit projection capability",
            ..
        }
    ));
}

fn one_row_runtime_projection(
    program: Vec<LinearOp>,
) -> Result<RuntimeAssignmentProjection, SolveProblemShapeContractError> {
    let problem = representative_solve_problem_fixture();
    let rhs = ScalarProgramBlock::with_source_span(vec![program], fixture_provenance())
        .expect("hostile dependency fixture remains computable");
    derive_runtime_assignment_projection(
        &problem.solve_layout,
        &rhs,
        &[scalar_slot_p(1)],
        &[DiscreteRowRole::Equation],
        &[DiscreteEventPreMode::FollowCurrent],
        &[None],
        &[Some(scalar_slot_p(5))],
    )
}

#[test]
fn runtime_projection_reads_the_complete_tensor_load_p_range() {
    let projection = one_row_runtime_projection(vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::P,
            input_start: 5,
            count: 1,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("P5 tensor load makes the P1 source root-reachable");
    assert_eq!(projection.source_rows(), [0]);
}

#[test]
fn runtime_projection_recurses_into_store_output_function_fold() {
    let inner = FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: Vec::new(),
        },
        1,
        0,
        vec![
            LinearOp::LoadP { dst: 0, index: 5 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("nested dependency fixture is a checked fold");
    let outer = FunctionFoldProgram::checked(
        StructuredIndexDomain {
            binders: Vec::new(),
        },
        1,
        0,
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutputFunctionFold {
                initial: vec![FoldInitialSource::Registers { start: 0, count: 1 }].into(),
                capture_start: 0,
                program: std::sync::Arc::new(inner),
                result_base: 0,
                count: 1,
                condition: None,
                nested_when_true: false,
            },
        ],
    )
    .expect("outer dependency fixture is a checked fold");
    let projection = one_row_runtime_projection(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 1 },
    ])
    .expect("nested P5 load makes the P1 source root-reachable");
    assert_eq!(projection.source_rows(), [0]);
}

#[test]
fn runtime_relation_role_recurses_into_function_conditional_regions() {
    let scalar_output = |operations: Vec<LinearOp>| {
        let mut operations = operations;
        operations.push(LinearOp::StoreOutput { src: 0 });
        operations
    };
    let conditional = FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            scalar_output(vec![LinearOp::Const { dst: 0, value: 1.0 }]),
        )],
        scalar_output(vec![LinearOp::Const { dst: 0, value: 0.0 }]),
    )
    .expect("nested relation fixture is a checked conditional");
    let rhs = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_provenance(),
    )
    .expect("nested relation outer program is computable");
    assert_eq!(
        derive_runtime_assignment_roles(&rhs, &[scalar_slot_p(1)], &[]).unwrap(),
        [RuntimeAssignmentRole::RelationEvaluating]
    );
}

#[test]
fn prepared_runtime_projection_rejects_root_fact_substitution() {
    let solve_layout = representative_solve_layout();
    let mut events = representative_event_partition();
    let (discrete, projection) =
        representative_discrete_system(&solve_layout, &events.root_relation_memory_targets);
    events.root_relation_memory_targets[0] = Some(scalar_slot_p(6));
    let error = match projection.prepare(discrete, events) {
        Ok(_) => panic!("a prepared projection cannot join different root facts"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_source_rows",
            row: 0,
            detail: "prepared projection is joined to different root-target facts",
            ..
        }
    ));
}

#[test]
fn prepared_runtime_projection_rejects_relation_inventory_substitution() {
    let solve_layout = representative_solve_layout();
    let events = representative_event_partition();
    let (discrete, projection) =
        representative_discrete_system(&solve_layout, &events.root_relation_memory_targets);
    let prepared = projection
        .prepare(discrete, events)
        .expect("the unchanged representative facts prepare");
    let mut substituted_layout = solve_layout;
    substituted_layout.relation_memory_parameter_indices[0] = 4;
    let error = prepared
        .into_system_for(&substituted_layout)
        .expect_err("runtime role evidence cannot join another relation inventory");
    assert!(matches!(
        error,
        SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_source_rows",
            detail: "prepared projection is joined to different source facts",
            ..
        }
    ));
}

#[test]
fn prepared_runtime_projection_binds_none_positions_and_vector_length_exactly() {
    for (issued, substituted) in [
        (vec![], vec![None]),
        (vec![None], vec![]),
        (
            vec![Some(scalar_slot_p(5)), None],
            vec![None, Some(scalar_slot_p(5))],
        ),
    ] {
        let projection = derive_runtime_assignment_projection(
            &SolveLayout::default(),
            &ScalarProgramBlock::default(),
            &[],
            &[],
            &[],
            &[],
            &issued,
        )
        .expect("empty discrete source has an exact empty runtime projection");
        let events = SolveEventPartition {
            root_relation_memory_targets: substituted,
            ..SolveEventPartition::default()
        };
        assert!(
            projection
                .prepare(DiscreteSolveSystem::default(), events)
                .is_err()
        );
    }
}

#[test]
fn runtime_certificate_derivations_refuse_parallel_column_truncation() {
    let rhs = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_provenance(),
    )
    .expect("one source row is computable");
    assert!(derive_runtime_assignment_roles(&rhs, &[], &[]).is_err());
    assert!(derive_root_reachable_runtime_rows(&rhs, &[], &[], &[]).is_err());
    assert!(derive_root_relation_refresh_roles(&rhs, &rhs, &[], 0, 0).is_err());
}

#[test]
fn solve_problem_shape_rejects_relation_bearing_post_commit_owner() {
    let mut problem = representative_solve_problem_fixture();
    problem.discrete.rhs = ScalarProgramBlock::with_source_span(
        vec![
            vec![
                LinearOp::LoadP { dst: 0, index: 5 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::Compare {
                    dst: 2,
                    op: CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            vec![
                LinearOp::LoadP { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_provenance(),
    )
    .expect("relation-bearing source fixture is computable");
    replace_representative_runtime_projection(&mut problem, &[0, 1]);
    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.post_commit_assignment_runtime_rows",
            row: 0,
            ..
        })
    ));
}

#[test]
fn solve_problem_shape_rejects_forged_root_refresh_role() {
    let mut problem = representative_solve_problem_fixture();
    problem.events.root_relation_refresh_roles[0] = RootRelationRefreshRole::AlgebraicDependent;
    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "events.root_relation_refresh_roles",
            row: 0,
            ..
        })
    ));
}

#[test]
fn solve_problem_shape_rejects_misaligned_integrator_history_effects() {
    let mut problem = representative_solve_problem_fixture();
    problem.discrete.integrator_history_effects.clear();

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "discrete.integrator_history_effects",
            expected: 2,
            actual: 0,
            span: None,
        })
    );
}

#[test]
fn solve_problem_json_rejects_omitted_layout_collections() {
    let value =
        serde_json::to_value(representative_solve_problem_fixture()).expect("serialize fixture");
    for field in ["shapes", "shape_spans"] {
        let mut omitted = value.clone();
        omitted["layout"]
            .as_object_mut()
            .expect("fixture layout is an object")
            .remove(field);
        let error = serde_json::from_value::<SolveProblem>(omitted)
            .expect_err("required layout collection must not default");
        assert!(
            error
                .to_string()
                .contains(&format!("missing field `{field}`")),
            "unexpected omission error for {field}: {error}"
        );
    }
}

#[test]
fn representative_solve_problem_json_matches_committed_golden() {
    let problem = representative_solve_problem_fixture();
    let actual = serde_json::to_value(&problem).expect("serialize representative SolveProblem");
    let expected: serde_json::Value = serde_json::from_str(REPRESENTATIVE_SOLVE_PROBLEM_GOLDEN)
        .expect("valid SolveProblem golden JSON");

    serde_json::from_value::<SolveProblem>(expected.clone())
        .expect("golden uses supported Solve schema");
    assert_eq!(actual, expected);
}

#[test]
fn representative_solve_problem_bincode_roundtrip_preserves_schema_shape() {
    let problem = representative_solve_problem_fixture();
    let bytes = bincode::serialize(&problem).expect("serialize SolveProblem as bincode");
    let decoded: SolveProblem =
        bincode::deserialize(&bytes).expect("deserialize SolveProblem from bincode");
    assert_same_json_shape(&decoded, &problem);
}

#[test]
fn structured_discrete_map_has_one_checked_compact_target_owner() {
    let mut problem = representative_solve_problem_fixture();
    let (block, update) = structured_discrete_fixture(scalar_slot_p(1));
    problem.discrete.rhs = ScalarProgramBlock::default();
    problem.discrete.update_targets.clear();
    problem.discrete.row_roles.clear();
    problem.discrete.pre_modes.clear();
    problem.discrete.observation_refresh.clear();
    problem.discrete.integrator_history_effects.clear();
    problem.discrete.clock_owners.clear();
    problem.discrete.runtime_assignment_source_rows.clear();
    problem.discrete.runtime_assignment_rhs = ScalarProgramBlock::default();
    problem.discrete.runtime_assignment_targets.clear();
    problem.discrete.runtime_assignment_roles.clear();
    problem.discrete.post_commit_assignment_rhs = ScalarProgramBlock::default();
    problem.discrete.post_commit_assignment_targets.clear();
    problem.discrete.post_commit_assignment_runtime_rows.clear();
    problem.discrete.structured_rhs = block;
    problem.discrete.structured_updates = vec![update];
    problem.discrete.event_iteration_plan.runs[0].owner =
        EventIterationOwner::StructuredUpdate { update_index: 0 };

    assert_eq!(
        problem.discrete.structured_assignments(0).unwrap(),
        vec![(scalar_slot_p(1), 0), (scalar_slot_p(2), 1)]
    );
    problem.validate_constructed_parts().unwrap();
}

#[test]
fn structured_discrete_shape_rejects_unclaimed_compute_nodes() {
    let mut problem = representative_solve_problem_fixture();
    let (block, _) = structured_discrete_fixture(scalar_slot_p(1));
    problem.discrete.structured_rhs = block;

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "discrete.structured_updates",
            expected: 1,
            actual: 0,
            span: None,
        })
    );
}

#[test]
fn structured_discrete_shape_rejects_parallel_scalar_target_owner() {
    let mut problem = representative_solve_problem_fixture();
    let (block, update) = structured_discrete_fixture(scalar_slot_p(1));
    problem.discrete.structured_rhs = block;
    problem.discrete.structured_updates = vec![update];

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.event_iteration_plan",
            row: 0,
            detail: "a structured discrete producer is not owned by its plan run",
            span: None,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_duplicate_clock_activation_lanes() {
    let mut problem = representative_solve_problem_fixture();
    problem
        .clocks
        .periodic_event_schedules
        .push(problem.clocks.periodic_event_schedules[0].clone());
    problem.clocks.activation_parameter_indices = vec![0, 0];

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DuplicateIndex {
            context: "clocks.activation_parameter_indices",
            index: 0,
            span: None,
        })
    );
}

/// A state load outside the layout is a construction defect, not a runtime
/// surprise: forward-mode AD offsets the parameter seeds by `y_scalars`, so an
/// unowned `Y` index aliases derivative columns instead of adding one.
#[test]
fn solve_problem_shape_contract_rejects_state_load_outside_layout() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.residual = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 9 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_provenance(),
        )
        .expect("unowned state load fixture is computable"),
    );

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context: "continuous.residual",
            storage: "Y",
            index: 9,
            extent: 3,
            span: Some(fixture_span()),
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_tensor_load_run_outside_layout() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.residual = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::TensorLoad {
                    dst_start: 0,
                    input: TensorInputKind::Y,
                    input_start: 2,
                    count: 2,
                    seed_start: None,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ]],
            fixture_provenance(),
        )
        .expect("tensor load fixture is computable"),
    );

    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context: "continuous.residual",
            storage: "Y",
            index: 3,
            extent: 3,
            ..
        })
    ));
}

#[test]
fn solve_problem_shape_contract_rejects_affine_adjusted_load_maximum() {
    let mut problem = representative_solve_problem_fixture();
    let domain = test_tensor_domain(3);
    problem.continuous.residual = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain)
                .expect("fixture map is dense"),
            domain,
            base_ops: vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 0,
                terms: vec![AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: fixture_span(),
        }],
    };

    assert!(matches!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context: "continuous.residual",
            storage: "Y",
            index: 3,
            extent: 3,
            ..
        })
    ));
}

#[test]
fn solve_problem_shape_contract_rejects_loads_in_nested_typed_regions() {
    let conditional = FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                LinearOp::LoadY { dst: 0, index: 9 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("nested conditional fixture is checked");
    let conditional_row = vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutput { src: 0 },
    ];

    let fold = FunctionFoldProgram::checked(
        test_tensor_domain(1),
        1,
        0,
        vec![
            LinearOp::LoadY { dst: 0, index: 9 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("nested guarded fold fixture is checked");
    let guarded_fold_row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::GuardedFunctionFold {
            dst_start: 2,
            initial_start: 0,
            capture_start: 0,
            activation: 1,
            program: std::sync::Arc::new(fold),
        },
        LinearOp::StoreOutput { src: 2 },
    ];

    for row in [conditional_row, guarded_fold_row] {
        let mut problem = representative_solve_problem_fixture();
        problem.continuous.residual = ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(vec![row], fixture_provenance())
                .expect("nested region fixture is computable"),
        );
        assert!(matches!(
            problem.validate_constructed_parts(),
            Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                context: "continuous.residual",
                storage: "Y",
                index: 9,
                extent: 3,
                ..
            })
        ));
    }
}

#[test]
fn solve_problem_shape_contract_rejects_rectangular_projection_block() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.algebraic_projection_plan.blocks[0]
        .y_indices
        .clear();

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(
            SolveProblemShapeContractError::ProjectionBlockShapeMismatch {
                context: "continuous.algebraic_projection_plan",
                row_count: 1,
                unknown_count: 0,
                span: None,
            }
        )
    );
}

#[test]
fn solve_problem_shape_contract_rejects_a_non_algebraic_implicit_target() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.implicit_row_targets[0] = Some(scalar_slot_y(0));

    let error = problem
        .validate_constructed_parts()
        .expect_err("an implicit algebraic row cannot target a state slot");
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ContinuousRefreshOwner { detail }
            if detail.contains("do not cover the required algebraic Y inventory")
    ));
}

#[test]
fn solve_problem_shape_contract_rejects_a_projection_target_mismatch() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.algebraic_projection_plan.blocks[0].y_indices[0] = 0;

    let error = problem
        .validate_constructed_parts()
        .expect_err("the projection target must match the row's authoritative target");
    assert!(matches!(
        error,
        SolveProblemShapeContractError::ContinuousRefreshOwner { detail }
            if detail.contains("projection row/target pairs")
    ));
}

#[test]
fn solve_problem_shape_contract_rejects_duplicate_initial_projection_unknown() {
    let mut problem = representative_solve_problem_fixture();
    problem.initialization.projection_unknowns = vec![scalar_slot_y(1), scalar_slot_y(1)];

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DuplicateProjectionUnknown {
            context: "initialization.projection_unknowns",
            unknown: format!("{:?}", scalar_slot_y(1)),
            span: None,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_unaligned_root_relation_memory() {
    let mut problem = representative_solve_problem_fixture();
    problem.events.root_relation_memory_targets.clear();

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "events.root_relation_memory_targets",
            expected: 1,
            actual: 0,
            span: None,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_unaligned_root_zero_domains() {
    let mut problem = representative_solve_problem_fixture();
    problem.events.root_zero_domains.clear();

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "events.root_zero_domains",
            expected: 1,
            actual: 0,
            span: None,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_duplicate_delay_parameter_slots() {
    let mut problem = representative_solve_problem_fixture();
    let delay_rows = ScalarProgramBlock::with_source_span(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 0.1 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 0.2 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        fixture_provenance(),
    )
    .expect("delay scalar fixture is computable");
    problem.events.delays.source_rhs = delay_rows.clone();
    problem.events.delays.delay_time_rhs = delay_rows.clone();
    problem.events.delays.delay_max_rhs = delay_rows;
    problem.events.delays.value_parameter_indices = vec![0, 0];
    problem.events.delays.source_is_discrete = vec![false, false];

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::DuplicateIndex {
            context: "events.delays.value_parameter_indices",
            index: 0,
            span: None,
        })
    );
}

#[test]
fn solve_problem_shape_contract_requires_terminal_parameter_index() {
    let mut problem = representative_solve_problem_fixture();
    problem.events.has_terminal_event = true;

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "solve_layout.terminal_event_parameter_index",
            expected: 1,
            actual: 0,
            span: None,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_zero_tensor_dimension() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.derivative_rhs = ComputeBlock {
        nodes: vec![ComputeNode::LinSolve {
            setup_ops: Vec::new(),
            matrix_start: 0,
            rhs_start: 0,
            n: 0,
            next_reg: 0,
            matrix_pattern: crate::fixture_pattern(0, 0, false),
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    };

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::ZeroTensorDimension {
            context: "continuous.derivative_rhs".to_string(),
            node_index: 0,
            dimension: "LinSolve",
            span: Span::DUMMY,
        })
    );
}

#[test]
fn solve_problem_shape_contract_rejects_zero_step_tensor_domain() {
    let mut problem = representative_solve_problem_fixture();
    problem.continuous.derivative_rhs = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 0,
                }],
            },
            output_map: TensorOutputMap {
                start: 0,
                strides: Vec::new(),
            },
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    };

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::StructuredIndexDomain {
            context: "continuous.derivative_rhs".to_string(),
            node_index: 0,
            dimension: "Map",
            error: StructuredIndexDomainError::ZeroStep {
                binder_id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
            },
            span: Span::DUMMY,
        })
    );
}

#[test]
fn solve_model_variable_scale_combines_nominal_and_start_magnitude() {
    let model = construct_state_model(vec![1.0e6, 1.0e-12], vec![2.0, 1.0e-9])
        .expect("finite starts and normal positive nominals seal a root");

    assert_eq!(model.state_nominal_values(), &[2.0, 1.0e-9]);
    assert_eq!(model.solver_variable_scales(), &[1.0e6, 1.0e-9]);
}

#[test]
fn solve_model_absent_catalog_nominal_requires_the_effective_one_value() {
    let error = construct_state_model_with_catalog_nominals(
        vec![0.0],
        vec![2.0],
        None,
        state_artifacts(1, 1),
    )
    .expect_err("an omitted nominal denotes 1.0 and cannot authenticate a different solver value");

    assert!(matches!(
        error,
        SolveModelConstructionError::VariableCatalog(
            SolveVariableCatalogError::RuntimeNominalMismatch { name, .. }
        ) if name == "x"
    ));
}

#[test]
fn solve_variable_catalog_rejects_scalar_names_from_a_different_y_run() {
    let problem = state_problem_with_derivative_rows(1, 1)
        .expect("one-state fixture problem is valid by construction");
    let initial_y = [0.0];
    let solver_nominals = [1.0];
    let mut catalog = SolveVariableCatalog::begin(&problem, &initial_y, &solver_nominals, &[]);
    let error = catalog
        .issue(
            SolveVariableSource::new(
                SourceOccurrenceId::try_from(InstanceId::new(1))
                    .expect("fixture source occurrence is explicitly nonzero"),
                "x".to_string(),
                vec![1],
                vec!["foreign[1]".to_string()],
                fixture_span(),
            ),
            SolveVariableSourceAttributes::new(
                SolveVariableCausality::Local,
                SolveVariableVariability::Continuous,
                false,
                None,
                None,
                rumoca_core::Fixity::Fixed,
            ),
            SolveVariableEvaluatedValues::new(
                Some(initial_y.to_vec()),
                None,
                None,
                Some(solver_nominals.to_vec()),
            ),
        )
        .expect_err("presentation names cannot be paired with a foreign Y storage run");

    assert!(matches!(
        error,
        SolveVariableCatalogError::StorageNameMismatch { name, .. } if name == "x"
    ));
}

#[test]
fn solve_model_construction_refuses_a_nan_initial_value_by_index() {
    let error = construct_state_model(vec![1.0e6, 1.0e-12, f64::NAN, 3.0], vec![1.0; 4])
        .expect_err("a NaN start cannot wear a finite solver scale");

    assert_eq!(
        error,
        SolveModelConstructionError::InvalidInitialValue { index: 2 }
    );
}

#[test]
fn solve_model_construction_refuses_an_infinite_initial_value_by_index() {
    for poisoned in [f64::INFINITY, f64::NEG_INFINITY] {
        let error = construct_state_model(vec![1.0, 2.0, 4.0, poisoned], vec![1.0; 4])
            .expect_err("an infinite start cannot wear a finite solver scale");

        assert_eq!(
            error,
            SolveModelConstructionError::InvalidInitialValue { index: 3 }
        );
    }
}

#[test]
fn solve_model_construction_refuses_every_non_normal_or_non_positive_nominal() {
    for poisoned in [
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
        0.0,
        -0.0,
        -1.0,
        f64::MIN_POSITIVE / 2.0,
    ] {
        let error = construct_state_model(vec![1.0, 2.0], vec![1.0, poisoned])
            .expect_err("a nominal outside the strictly positive normal range must be refused");

        assert_eq!(
            error,
            SolveModelConstructionError::InvalidSolverNominal { index: 1 },
            "nominal {poisoned:?} must be refused with its index"
        );
    }
}

#[test]
fn solve_model_construction_refuses_out_of_bounds_artifact_y_p_and_seed_loads() {
    for (operation, storage, index, extent) in [
        (LinearOp::LoadY { dst: 99, index: 1 }, "Y", 1, 1),
        (LinearOp::LoadP { dst: 99, index: 0 }, "P", 0, 0),
        (LinearOp::LoadSeed { dst: 99, index: 1 }, "seed", 1, 1),
    ] {
        let mut artifacts = state_artifacts(1, 1);
        artifacts.continuous.full_jacobian_v.programs[0].insert(0, operation);
        let error = construct_state_model_with_artifacts(vec![1.0], vec![1.0], artifacts)
            .expect_err("artifact loads must fit the exact retained launch ABI");
        let SolveModelConstructionError::Shape(shape) = error else {
            panic!("expected an artifact shape error");
        };
        assert_eq!(
            *shape,
            SolveProblemShapeContractError::VariableIndexOutOfBounds {
                context: "artifacts.continuous.full_jacobian_v",
                storage,
                index,
                extent,
                span: Some(fixture_span()),
            }
        );
    }
}

#[test]
fn solve_model_construction_refuses_out_of_bounds_artifact_tensor_seed_run() {
    let mut artifacts = state_artifacts(1, 1);
    artifacts.continuous.full_jacobian_v.programs[0].insert(
        0,
        LinearOp::TensorLoad {
            dst_start: 99,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 1,
            seed_start: Some(1),
            lanes: 2,
        },
    );
    let error = construct_state_model_with_artifacts(vec![1.0], vec![1.0], artifacts)
        .expect_err("tensor seed run must fit the exact retained launch ABI");
    let SolveModelConstructionError::Shape(shape) = error else {
        panic!("expected an artifact shape error");
    };
    assert!(matches!(
        *shape,
        SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context: "artifacts.continuous.full_jacobian_v",
            storage: "seed",
            index: 1,
            extent: 1,
            ..
        }
    ));
}

fn scalar_catalog_entry(
    source_occurrence: u32,
    name: &str,
    scalar_names: &[&str],
    causality: SolveVariableCausality,
    variability: SolveVariableVariability,
    fixed: rumoca_core::Fixity,
    start: Vec<f64>,
) -> SolveVariableCatalogSourceEntry {
    let dimensions = if scalar_names.len() == 1 {
        Vec::new()
    } else {
        vec![u32::try_from(scalar_names.len()).expect("fixture extent fits u32")]
    };
    (
        SolveVariableSource::new(
            SourceOccurrenceId::try_from(InstanceId::new(source_occurrence))
                .expect("fixture source occurrence is explicitly nonzero"),
            name.to_string(),
            dimensions,
            scalar_names.iter().map(|name| name.to_string()).collect(),
            fixture_span(),
        ),
        SolveVariableSourceAttributes::new(causality, variability, false, None, None, fixed),
        SolveVariableEvaluatedValues::new(Some(start), None, None, None),
    )
}

fn projection_fixture_solve_layout() -> SolveLayout {
    let names = ["x", "z", "out"];
    SolveLayout {
        solver_maps: SolverNameIndexMaps {
            name_to_idx: names
                .iter()
                .enumerate()
                .map(|(index, name)| (name.to_string(), index))
                .collect(),
            base_to_indices: names
                .iter()
                .enumerate()
                .map(|(index, name)| (name.to_string(), vec![index]))
                .collect(),
            names: names.iter().map(|name| name.to_string()).collect(),
        },
        variable_storage_runs: vec![
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(0),
                scalar_count: 1,
                role: SolveVariableStorageRole::State,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(1),
                scalar_count: 1,
                role: SolveVariableStorageRole::Output,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(2),
                scalar_count: 1,
                role: SolveVariableStorageRole::Output,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::P(0),
                scalar_count: 2,
                role: SolveVariableStorageRole::Parameter,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::P(2),
                scalar_count: 1,
                role: SolveVariableStorageRole::Parameter,
                value_kind: SolveVariableValueKind::Real,
            },
        ],
        variable_declarations: vec![
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::State,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Output,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Output,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Parameter,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::Parameter,
                SolveVariableValueKind::Real,
            ),
        ],
        state_scalar_count: 1,
        algebraic_scalar_count: 0,
        output_scalar_count: 2,
        parameter_count: 3,
        static_parameter_names: vec![
            "gain[1]".to_string(),
            "gain[2]".to_string(),
            "bias".to_string(),
        ],
        ..SolveLayout::default()
    }
}

fn projection_fixture_entries() -> Vec<SolveVariableCatalogSourceEntry> {
    vec![
        scalar_catalog_entry(
            1,
            "x",
            &["x"],
            SolveVariableCausality::Local,
            SolveVariableVariability::Continuous,
            rumoca_core::Fixity::Fixed,
            vec![-0.0],
        ),
        scalar_catalog_entry(
            2,
            "z",
            &["z"],
            SolveVariableCausality::Output,
            SolveVariableVariability::Continuous,
            rumoca_core::Fixity::Free,
            vec![11.0],
        ),
        scalar_catalog_entry(
            3,
            "out",
            &["out"],
            SolveVariableCausality::Output,
            SolveVariableVariability::Continuous,
            rumoca_core::Fixity::Free,
            vec![12.0],
        ),
        scalar_catalog_entry(
            4,
            "gain",
            &["gain[1]", "gain[2]"],
            SolveVariableCausality::Parameter,
            SolveVariableVariability::Fixed,
            rumoca_core::Fixity::Fixed,
            vec![-0.0, 2.5],
        ),
        scalar_catalog_entry(
            5,
            "bias",
            &["bias"],
            SolveVariableCausality::Parameter,
            SolveVariableVariability::Fixed,
            rumoca_core::Fixity::Fixed,
            vec![7.0],
        ),
    ]
}

#[test]
fn solve_model_template_projections_preserve_checked_slot_order_and_negative_zero() {
    let problem = SolveProblem::construct(
        make_layout(
            &[("x", vec![1]), ("z", vec![1]), ("out", vec![1])],
            &[("gain", vec![2]), ("bias", vec![1])],
        ),
        projection_fixture_solve_layout(),
        state_continuous_system(1),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
    .expect("projection fixture problem is valid by construction");
    let visible_value_rows = ScalarProgramBlock::with_source_span(
        (0..3)
            .map(|index| {
                vec![
                    LinearOp::LoadY { dst: 0, index },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        fixture_provenance(),
    )
    .expect("projection fixture visible rows are computable");
    let model = SolveModel::construct(
        problem,
        SolvePureCallTable::empty(binary64_first_product_profile()),
        state_artifacts(1, 6),
        SolveModelRuntimeInputs {
            initial_y: vec![-0.0, 11.0, 12.0],
            solver_nominals: vec![1.0, 1.0, 1.0],
            parameters: vec![-0.0, 2.5, 7.0],
        },
        visible_value_rows,
        projection_fixture_entries(),
    )
    .expect("projection fixture model is valid by construction");

    assert_eq!(model.state_names(), ["x"]);
    assert_eq!(
        model.static_parameter_names(),
        ["gain[1]", "gain[2]", "bias"]
    );
    assert_eq!(model.initial_state_values().len(), 1);
    assert_eq!(
        model.initial_state_values()[0].to_bits(),
        (-0.0_f64).to_bits()
    );
    assert_eq!(
        model.static_parameter_values()[0].to_bits(),
        (-0.0_f64).to_bits()
    );
    assert_eq!(model.static_parameter_values()[1..], [2.5, 7.0]);
}

#[test]
fn solve_problem_rejects_static_parameter_names_without_exact_slot_coverage() {
    let mut problem = representative_solve_problem_fixture();
    problem.solve_layout.static_parameter_names.clear();

    assert_eq!(
        problem.validate_constructed_parts(),
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context: "solve_layout.static_parameter_names",
            expected: 1,
            actual: 0,
            span: None,
        })
    );
}

#[test]
fn solve_problem_rejects_a_state_inventory_beyond_the_solver_partition() {
    let error = SolveProblem::construct(
        VarLayout::default(),
        SolveLayout {
            state_scalar_count: 10,
            ..SolveLayout::default()
        },
        empty_continuous_system(),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
    .expect_err("ten claimed states cannot fit an empty solver inventory");

    assert_eq!(
        error,
        SolveProblemShapeContractError::SolveLayoutPartition {
            context: "solve_layout.state/algebraic/output_scalar_count",
            expected: 0,
            actual: Some(10),
        }
    );
}

/// The exact zero-Y hostile layout observed in downstream fixtures: one
/// claimed state over an empty Y column and an empty solver-name inventory.
#[test]
fn solve_problem_rejects_one_claimed_state_over_an_empty_y_column() {
    let error = SolveProblem::construct(
        VarLayout::default(),
        SolveLayout {
            state_scalar_count: 1,
            ..SolveLayout::default()
        },
        empty_continuous_system(),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
    .expect_err("one claimed state cannot fit an empty Y column");

    assert_eq!(
        error,
        SolveProblemShapeContractError::SolveLayoutPartition {
            context: "solve_layout.state/algebraic/output_scalar_count",
            expected: 0,
            actual: Some(1),
        }
    );
}

#[test]
fn solve_problem_wire_rejects_one_claimed_state_over_an_empty_y_column() {
    let mut wire = serde_json::to_value(empty_solve_problem()).expect("serialize fixture");
    wire["solve_layout"]["state_scalar_count"] = serde_json::json!(1);

    let error = serde_json::from_value::<SolveProblem>(wire)
        .expect_err("a zero-Y state claim must be refused at deserialization");
    assert!(
        error.to_string().contains("Solve layout role partition"),
        "wire replay must route through the partition proof: {error}"
    );
}

#[test]
fn solve_problem_rejects_a_parameter_prefix_beyond_its_storage_column() {
    let error = SolveProblem::construct(
        VarLayout::from_parts(IndexMap::new(), 0, 1),
        SolveLayout {
            parameter_count: 2,
            static_parameter_names: vec!["a".to_string(), "b".to_string()],
            ..SolveLayout::default()
        },
        empty_continuous_system(),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
    .expect_err("two static parameters cannot prefix a one-scalar P column");

    assert_eq!(
        error,
        SolveProblemShapeContractError::SolveLayoutPrefix {
            context: "solve_layout.parameter_count/layout.p_scalars",
            bound: 1,
            actual: 2,
        }
    );
}

#[test]
fn solve_problem_rejects_state_and_algebraic_storage_outside_their_dense_segments() {
    let mut problem = representative_solve_problem_fixture();
    problem.solve_layout.variable_storage_runs[0].base = SolveStorageCoordinate::Y(1);
    problem.solve_layout.variable_storage_runs[1].base = SolveStorageCoordinate::Y(0);

    let error = problem
        .validate_constructed_parts()
        .expect_err("an algebraic lane cannot occupy the leading state segment");
    assert!(
        error.to_string().contains("dense Y role segment"),
        "{error}"
    );
}

#[test]
fn solve_problem_rejects_same_role_state_storage_reordered_from_declaration_order() {
    let names = ["x", "y"];
    let solve_layout = SolveLayout {
        solver_maps: SolverNameIndexMaps {
            name_to_idx: names
                .iter()
                .enumerate()
                .map(|(index, name)| (name.to_string(), index))
                .collect(),
            names: names.iter().map(|name| name.to_string()).collect(),
            ..SolverNameIndexMaps::default()
        },
        variable_storage_runs: vec![
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(1),
                scalar_count: 1,
                role: SolveVariableStorageRole::State,
                value_kind: SolveVariableValueKind::Real,
            },
            SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(0),
                scalar_count: 1,
                role: SolveVariableStorageRole::State,
                value_kind: SolveVariableValueKind::Real,
            },
        ],
        variable_declarations: vec![
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::State,
                SolveVariableValueKind::Real,
            ),
            SolveVariableDeclaration::new(
                SolveVariableStorageRole::State,
                SolveVariableValueKind::Real,
            ),
        ],
        state_scalar_count: 2,
        ..SolveLayout::default()
    };
    let error = SolveProblem::construct(
        make_layout(&[("x", vec![1]), ("y", vec![1])], &[]),
        solve_layout,
        state_continuous_system(2),
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        SolveEventPartition::default(),
        SolveClockPartition::default(),
    )
    .expect_err("same-role state variables cannot exchange their Y identities");

    assert!(
        error
            .to_string()
            .contains("contiguous in declaration order"),
        "{error}"
    );
}

#[test]
fn solve_problem_rejects_static_parameter_storage_outside_its_dense_prefix() {
    let mut problem = representative_solve_problem_fixture();
    problem.solve_layout.variable_storage_runs[3].base = SolveStorageCoordinate::P(1);

    let error = problem
        .validate_constructed_parts()
        .expect_err("a static parameter cannot escape the leading P prefix");
    assert!(
        error.to_string().contains("dense P static prefix"),
        "{error}"
    );
}

#[test]
fn solve_problem_wire_rejects_a_state_inventory_beyond_the_solver_partition() {
    let mut wire =
        serde_json::to_value(representative_solve_problem_fixture()).expect("serialize fixture");
    wire["solve_layout"]["state_scalar_count"] = serde_json::json!(10);

    let error = serde_json::from_value::<SolveProblem>(wire)
        .expect_err("a hostile state inventory must be refused at deserialization");
    assert!(
        error.to_string().contains("Solve layout role partition"),
        "wire replay must route through the partition proof: {error}"
    );
}

#[test]
fn solve_problem_wire_rejects_a_parameter_prefix_beyond_its_storage_column() {
    let mut wire =
        serde_json::to_value(representative_solve_problem_fixture()).expect("serialize fixture");
    wire["solve_layout"]["parameter_count"] = serde_json::json!(8);
    wire["solve_layout"]["static_parameter_names"] =
        serde_json::json!(["a", "b", "c", "d", "e", "f", "g", "h"]);

    let error = serde_json::from_value::<SolveProblem>(wire)
        .expect_err("a hostile parameter prefix must be refused at deserialization");
    assert!(
        error.to_string().contains("Solve layout prefix"),
        "wire replay must route through the prefix proof: {error}"
    );
}

/// SOLVE-C57: a minimal discrete system with one clock-owned scalar row and
/// no issued clock-partition step.
fn clocked_scalar_discrete_fixture() -> DiscreteSolveSystem {
    clocked_scalar_discrete_fixture_rows(1)
}

/// The same fixture with `rows` clock-owned scalar rows, all on one clock.
fn clocked_scalar_discrete_fixture_rows(rows: usize) -> DiscreteSolveSystem {
    let clock = SolveClockPartition {
        periodic_event_schedules: vec![
            PeriodicEventSchedule::from_seconds(0.1, 0.0).expect("fixture clock lattice is exact"),
        ],
        activation_parameter_indices: vec![1],
    }
    .periodic_clock_id(0)
    .expect("fixture clock identity resolves");
    DiscreteSolveSystem {
        rhs: ScalarProgramBlock::with_source_span(
            (0..rows)
                .map(|row| {
                    vec![
                        LinearOp::LoadP { dst: 0, index: row },
                        LinearOp::StoreOutput { src: 0 },
                    ]
                })
                .collect(),
            fixture_provenance(),
        )
        .expect("clocked discrete fixture rows are computable"),
        update_targets: (0..rows).map(scalar_slot_p).collect(),
        row_roles: vec![DiscreteRowRole::Equation; rows],
        pre_modes: vec![DiscreteEventPreMode::EventEntry; rows],
        observation_refresh: vec![false; rows],
        integrator_history_effects: vec![IntegratorHistoryEffect::Restart; rows],
        clock_owners: vec![Some(clock); rows],
        ..DiscreteSolveSystem::default()
    }
}

#[test]
fn clock_partition_order_requires_a_step_for_every_clock_owned_row() {
    let discrete = clocked_scalar_discrete_fixture();
    let error = discrete
        .validate_clock_partition_order()
        .expect_err("a clock-owned row without an issued step must be rejected");
    assert!(
        error
            .to_string()
            .contains("clock-owned scalar row has no issued step"),
        "unexpected rejection: {error}"
    );
}

#[test]
fn clock_partition_order_accepts_exact_coverage_and_rejects_duplicates() {
    let mut discrete = clocked_scalar_discrete_fixture();
    discrete.clock_partition_order = vec![ClockPartitionStep::ScalarRows {
        start_row: 0,
        count: 1,
    }];
    discrete
        .validate_clock_partition_order()
        .expect("exact coverage is valid");
    discrete
        .clock_partition_order
        .push(ClockPartitionStep::ScalarRows {
            start_row: 0,
            count: 1,
        });
    let error = discrete
        .validate_clock_partition_order()
        .expect_err("a row scheduled twice must be rejected");
    assert!(
        error.to_string().contains("scalar row scheduled twice"),
        "unexpected rejection: {error}"
    );
}

#[test]
fn clock_partition_order_rejects_steps_naming_unclocked_producers() {
    let mut discrete = clocked_scalar_discrete_fixture();
    discrete.clock_owners = vec![None];
    discrete.clock_partition_order = vec![ClockPartitionStep::ScalarRows {
        start_row: 0,
        count: 1,
    }];
    let error = discrete
        .validate_clock_partition_order()
        .expect_err("a step naming an unclocked row must be rejected");
    assert!(
        error
            .to_string()
            .contains("scalar step names an unclocked row"),
        "unexpected rejection: {error}"
    );
}

/// SOLVE-C57 design §4 row 6 / §2.2: a target owned by a DAE-C21/SOLVE-C55
/// model-event transaction is excluded from clock-partition admission. The
/// transaction is that target's owner, so the clock partition neither needs
/// nor may issue a step for it — a C57 producer consumes its committed value
/// instead. Both halves are checked: the exclusion is not a coverage failure,
/// and claiming the target is a typed rejection rather than a second owner.
#[test]
fn clock_partition_order_excludes_transaction_owned_producers() {
    // The transaction's target is a two-element tensor, so it owns rows 0..2.
    let mut discrete = clocked_scalar_discrete_fixture_rows(2);
    discrete.event_transactions = vec![event_transaction_fixture()];
    discrete
        .validate_clock_partition_order()
        .expect("a transaction-owned row needs no clock-partition step");

    discrete.clock_partition_order = vec![ClockPartitionStep::ScalarRows {
        start_row: 0,
        count: 2,
    }];
    let error = discrete
        .validate_clock_partition_order()
        .expect_err("a step claiming a transaction-owned row must be rejected");
    assert!(
        error
            .to_string()
            .contains("scalar step names a transaction-owned row"),
        "unexpected rejection: {error}"
    );
}

fn trivial_initialization_row_block(rows: usize) -> ComputeBlock {
    ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_source_span(
            (0..rows)
                .map(|_| {
                    vec![
                        LinearOp::Const { dst: 0, value: 0.0 },
                        LinearOp::StoreOutput { src: 0 },
                    ]
                })
                .collect(),
            fixture_provenance(),
        )
        .expect("initialization row fixture is computable"),
    )
}

fn trivial_update_block(rows: usize) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        (0..rows)
            .map(|_| {
                vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        fixture_provenance(),
    )
    .expect("update row fixture is computable")
}

/// The empty value and the issuer agree exactly, so stating the empty system
/// as a literal loses no proof.
#[test]
fn empty_initialization_system_is_exactly_correlated() {
    let issued = InitializationSolveSystem::construct(
        ComputeBlock::default(),
        Vec::new(),
        Vec::new(),
        0,
        Vec::new(),
        InitializationProjectionPlan::default(),
        (ScalarProgramBlock::default(), Vec::new()),
    )
    .expect("the empty initialization parts are exactly correlated");
    assert_eq!(
        serde_json::to_value(&issued).expect("issued empty system serializes"),
        serde_json::to_value(InitializationSolveSystem::empty())
            .expect("the stated empty system serializes"),
    );
}

/// The blocker-2 forgery: relabel a mandatory source row as a stated-value
/// check, clear its target, and empty the plan. Every per-part correlation is
/// self-consistent, so only the row-obligation witness can refuse it.
#[test]
fn erasing_a_mandatory_row_owner_by_relabeling_is_refused() {
    let error = InitializationSolveSystem::construct(
        trivial_initialization_row_block(1),
        vec![None],
        vec![InitializationRowRole::StatedValueCheck],
        1,
        Vec::new(),
        InitializationProjectionPlan::default(),
        (ScalarProgramBlock::default(), Vec::new()),
    )
    .expect_err("a mandatory source row cannot stand as a stated-value check");
    assert!(
        error
            .to_string()
            .contains("mandatory source row 0 is recorded as a stated-value check"),
        "the refusal names the erased obligation: {error}"
    );
}

/// Update targets form a closed writable domain: a duplicate identity, a
/// `Time` target, and a constant target each refuse the mint.
#[test]
fn update_targets_outside_the_closed_writable_domain_are_refused() {
    let duplicate = InitializationSolveSystem::construct(
        ComputeBlock::default(),
        Vec::new(),
        Vec::new(),
        0,
        Vec::new(),
        InitializationProjectionPlan::default(),
        (
            trivial_update_block(2),
            vec![scalar_slot_p(0), scalar_slot_p(0)],
        ),
    )
    .expect_err("one storage slot cannot be written by two update rows");
    assert!(
        duplicate.to_string().contains("written by two update rows"),
        "the duplicate refusal names the double write: {duplicate}"
    );

    let unwritable = InitializationSolveSystem::construct(
        ComputeBlock::default(),
        Vec::new(),
        Vec::new(),
        0,
        Vec::new(),
        InitializationProjectionPlan::default(),
        (trivial_update_block(1), vec![ScalarSlot::Time]),
    )
    .expect_err("`time` has no writable runtime storage");
    assert!(
        unwritable
            .to_string()
            .contains("no writable runtime storage"),
        "the unwritable refusal names the domain: {unwritable}"
    );

    let constant = InitializationSolveSystem::construct(
        ComputeBlock::default(),
        Vec::new(),
        Vec::new(),
        0,
        Vec::new(),
        InitializationProjectionPlan::default(),
        (trivial_update_block(1), vec![ScalarSlot::Constant(0.0)]),
    )
    .expect_err("a constant has no writable storage identity");
    assert!(
        constant.to_string().contains("no writable runtime storage"),
        "the constant refusal names the domain: {constant}"
    );
}

/// Projection-unknown uniqueness runs over the logical storage identity.
#[test]
fn projection_unknown_identity_refuses_duplicate_coordinates() {
    let duplicate = InitializationSolveSystem::construct(
        trivial_initialization_row_block(2),
        vec![Some(scalar_slot_y(0)), Some(scalar_slot_y(0))],
        vec![InitializationRowRole::Solved; 2],
        2,
        vec![scalar_slot_y(0), scalar_slot_y(0)],
        InitializationProjectionPlan {
            blocks: vec![InitializationProjectionBlock {
                rows: vec![0, 1],
                unknowns: vec![scalar_slot_y(0), scalar_slot_y(0)],
            }],
        },
        (ScalarProgramBlock::default(), Vec::new()),
    )
    .expect_err("one coordinate cannot be owned by two rows");
    assert!(
        duplicate
            .to_string()
            .contains("claimed by two block positions"),
        "the duplicate refusal names the identity: {duplicate}"
    );
}

#[test]
fn storage_run_wire_is_current_and_addressable_only() {
    for coordinate in [
        SolveStorageCoordinate::Y(0),
        SolveStorageCoordinate::P(usize::MAX),
    ] {
        let run = SolveVariableStorageRun {
            base: coordinate,
            scalar_count: 1,
            role: SolveVariableStorageRole::Parameter,
            value_kind: SolveVariableValueKind::Real,
        };
        let wire = serde_json::to_value(run).unwrap();
        assert_eq!(
            serde_json::from_value::<SolveVariableStorageRun>(wire.clone()).unwrap(),
            run
        );
        for obsolete in [
            serde_json::json!({"Y": {"index": 0, "byte_offset": 0}}),
            serde_json::json!({"P": {"index": 0, "byte_offset": 0}}),
            serde_json::json!("Time"),
            serde_json::json!({"Constant": 0.0}),
        ] {
            let mut forged = wire.clone();
            forged["base"] = obsolete;
            assert!(serde_json::from_value::<SolveVariableStorageRun>(forged).is_err());
        }
        let mut forged = wire;
        forged["byte_offset"] = serde_json::json!(0);
        let error = serde_json::from_value::<SolveVariableStorageRun>(forged).unwrap_err();
        assert!(error.to_string().contains("byte_offset"), "{error}");
    }
}

#[test]
fn solve_problem_wire_rejects_obsolete_storage_fields() {
    let current = serde_json::to_value(representative_solve_problem_fixture()).unwrap();
    let mut operand = current.clone();
    operand["layout"]["bindings"]["x"]["Y"]["byte_offset"] = serde_json::json!(0);
    let error = serde_json::from_value::<SolveProblem>(operand).unwrap_err();
    assert!(error.to_string().contains("byte_offset"), "{error}");

    let mut run = current;
    run["solve_layout"]["variable_storage_runs"][0]["base"] =
        serde_json::json!({"Y": {"index": 0, "byte_offset": 0}});
    assert!(serde_json::from_value::<SolveProblem>(run).is_err());
}
