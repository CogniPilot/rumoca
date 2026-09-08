use rumoca_ir_solve as solve;
use std::collections::BTreeMap;
use std::sync::Arc;

use rumoca_core::TargetInvocationBrand;

use crate::{TemplateBindingValue, TemplateBindings};

/// Build the neutral binding set a target file receives, mirroring the exact
/// names the compiler binds: `model_name`, the immutable `artifact` projection,
/// and one flattened text scalar per identity key.
pub(super) fn artifact_bindings<'inv>(
    brand: TargetInvocationBrand<'inv>,
    generated_at: &str,
    generation_tool: &str,
    artifact_stem: &str,
    identities: &BTreeMap<String, String>,
    checksums: &BTreeMap<String, String>,
) -> TemplateBindings<'inv> {
    let mut bindings = TemplateBindings::construct(brand);
    bindings
        .bind(
            "model_name",
            TemplateBindingValue::Text(artifact_stem.to_owned()),
        )
        .expect("model_name is a template identifier");
    let artifact = BTreeMap::from([
        (
            "generated_at".to_owned(),
            TemplateBindingValue::Text(generated_at.to_owned()),
        ),
        (
            "generation_tool".to_owned(),
            TemplateBindingValue::Text(generation_tool.to_owned()),
        ),
        (
            "artifact_stem".to_owned(),
            TemplateBindingValue::Text(artifact_stem.to_owned()),
        ),
        (
            "checksums".to_owned(),
            TemplateBindingValue::Object(
                checksums
                    .iter()
                    .map(|(key, value)| (key.clone(), TemplateBindingValue::Text(value.clone())))
                    .collect(),
            ),
        ),
    ]);
    bindings
        .bind("artifact", TemplateBindingValue::Object(artifact))
        .expect("artifact is a template identifier");
    for (key, value) in identities {
        bindings
            .bind(
                format!("__rumoca_artifact_identity_v1_{key}"),
                TemplateBindingValue::Text(value.clone()),
            )
            .expect("a canonical identity key forms a template identifier");
    }
    bindings
}

pub(super) fn solve_artifacts(problem: &solve::SolveProblem) -> solve::SolveArtifactInputs {
    rumoca_phase_solve::lower_solve_artifacts(problem)
        .expect("Solve fixture supports the production artifact derivation")
}

pub(super) fn solve_layout_with_names(
    mut layout: solve::SolveLayout,
    names: impl IntoIterator<Item = String>,
) -> solve::SolveLayout {
    assert!(
        layout.solver_maps.names.is_empty()
            && layout.solver_maps.name_to_idx.is_empty()
            && layout.solver_maps.base_to_indices.is_empty(),
        "fixture solver names have exactly one construction input"
    );
    let names = names.into_iter().collect::<Vec<_>>();
    let mut name_to_idx = indexmap::IndexMap::new();
    let mut base_to_indices = indexmap::IndexMap::new();
    for (index, name) in names.iter().enumerate() {
        assert!(
            name_to_idx.insert(name.clone(), index).is_none(),
            "fixture solver names are unique"
        );
        base_to_indices.insert(name.clone(), vec![index]);
    }
    layout.solver_maps = solve::SolverNameIndexMaps {
        name_to_idx,
        base_to_indices,
        names,
    };
    layout
}

pub(super) fn solve_layout_for_y(y_scalars: usize) -> solve::SolveLayout {
    solve_layout_with_names(
        solve::SolveLayout {
            state_scalar_count: y_scalars,
            ..solve::SolveLayout::default()
        },
        (0..y_scalars).map(|index| format!("y[{index}]")),
    )
}

pub(super) fn explicit_ode_layout(
    state_scalars: usize,
    parameter_scalars: usize,
) -> solve::SolveLayout {
    solve_layout_with_names(
        solve::SolveLayout {
            state_scalar_count: state_scalars,
            parameter_count: parameter_scalars,
            compiled_parameter_len: parameter_scalars,
            static_parameter_names: (0..parameter_scalars)
                .map(|index| format!("p[{index}]"))
                .collect(),
            ..solve::SolveLayout::default()
        },
        (0..state_scalars).map(|index| format!("y[{index}]")),
    )
}

pub(super) fn solve_model_with_artifacts(
    problem: solve::SolveProblem,
    artifacts: solve::SolveArtifactInputs,
) -> solve::SolveModel {
    let y_scalars = problem.layout().y_scalars();
    let p_scalars = problem.layout().p_scalars();
    solve_model_with_values(
        problem,
        artifacts,
        vec![0.0; y_scalars],
        vec![1.0; y_scalars],
        vec![0.0; p_scalars],
    )
}

pub(super) fn solve_model_with_values(
    problem: solve::SolveProblem,
    artifacts: solve::SolveArtifactInputs,
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
) -> solve::SolveModel {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    solve::SolveModel::construct(
        problem,
        solve::SolvePureCallTable::empty(arithmetic),
        artifacts,
        solve::SolveModelRuntimeInputs {
            initial_y,
            solver_nominals,
            parameters,
        },
        solve::ScalarProgramBlock::default(),
        std::iter::empty(),
    )
    .expect("Solve fixture satisfies the production root constructor")
}

pub(super) fn render_solve_fixture_template(
    problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifactInputs,
    template: &str,
    model_name: &str,
) -> Result<String, crate::errors::CodegenError> {
    let model = Arc::new(solve_model_with_artifacts(
        problem.clone(),
        artifacts.clone(),
    ));
    super::PreparedSolveModelRendering::new_for_test(model, model_name)?.render(template)
}

pub(super) fn builtin_template(target: &str, template: &str) -> &'static str {
    crate::templates::builtin_target(target)
        .and_then(|target| target.template_source(template))
        .expect("built-in target template must exist")
}

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("codegen_solve_fixture.mo"),
        1,
        2,
    )
}

fn full_pattern(size: usize) -> solve::StructuralPattern {
    let span = fixture_span();
    let provenance =
        solve::PatternProvenance::derived(solve::PatternDerivation::TensorOperand, span)
            .expect("fixture provenance");
    let dependencies = (0..size).map(|_| (0..size).collect()).collect::<Vec<_>>();
    solve::StructuralPattern::from_row_dependencies(size, size, &dependencies, provenance)
        .expect("fixture pattern")
}

pub(super) fn solve_problem_with_two_by_two_linsolve_derivative() -> solve::SolveProblem {
    let solve_layout = solve_layout_for_y(2);
    let continuous = continuous_system_with_derivative(
        &solve_layout,
        solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::LinSolve {
                setup_ops: vec![
                    solve::LinearOp::Const { dst: 0, value: 2.0 },
                    solve::LinearOp::Const { dst: 1, value: 0.0 },
                    solve::LinearOp::Const { dst: 2, value: 0.0 },
                    solve::LinearOp::Const { dst: 3, value: 4.0 },
                    solve::LinearOp::Const { dst: 4, value: 8.0 },
                    solve::LinearOp::Const {
                        dst: 5,
                        value: 20.0,
                    },
                ],
                matrix_start: 0,
                rhs_start: 4,
                n: 2,
                next_reg: 6,
                matrix_pattern: full_pattern(2),
                metadata: Default::default(),
                span: fixture_span(),
            }],
        },
    );
    let layout = solve::VarLayout::from_parts(indexmap::IndexMap::new(), 2, 0);
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    solve::SolveProblem::construct(
        layout,
        solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks,
    )
    .expect("Solve fixture aggregates satisfy the checked root contract")
}

pub(super) fn continuous_system_with_derivative(
    solve_layout: &solve::SolveLayout,
    derivative_rhs: solve::ComputeBlock,
) -> solve::ContinuousSolveSystem {
    let implicit_rhs = solve::ComputeBlock::default();
    solve::ContinuousSolveSystem::construct(
        solve_layout,
        solve::ContinuousSolveSystemInputs::new(
            implicit_rhs,
            Vec::new(),
            solve::AlgebraicProjectionPlan::default(),
            solve::ComputeBlock::default(),
            (
                solve::ComputeBlock::default(),
                solve::AlgebraicProjectionPlan::default(),
            ),
            derivative_rhs,
            solve::ContinuousRefreshPlanInputs::empty(),
        ),
    )
    .expect("explicit fixture continuous system satisfies the production constructor")
}

pub(super) struct ContinuousSystemFixture {
    pub implicit_rhs: solve::ComputeBlock,
    pub implicit_row_targets: Vec<Option<solve::ScalarSlot>>,
    pub algebraic_projection_plan: solve::AlgebraicProjectionPlan,
    pub residual: solve::ComputeBlock,
    pub manifold: (solve::ComputeBlock, solve::AlgebraicProjectionPlan),
    pub derivative_rhs: solve::ComputeBlock,
}

pub(super) fn checked_continuous_system(
    solve_layout: &solve::SolveLayout,
    discrete: &solve::DiscreteSolveSystem,
    events: &solve::SolveEventPartition,
    clocks: &solve::SolveClockPartition,
    fixture: ContinuousSystemFixture,
) -> solve::ContinuousSolveSystem {
    let ContinuousSystemFixture {
        implicit_rhs,
        implicit_row_targets,
        mut algebraic_projection_plan,
        residual,
        manifold,
        derivative_rhs,
    } = fixture;
    let refresh_plans = rumoca_eval_solve::refresh_plan::build_continuous_refresh_plans(
        solve_layout,
        (
            &implicit_rhs,
            &implicit_row_targets,
            &mut algebraic_projection_plan,
            &derivative_rhs,
        ),
        discrete,
        events,
        clocks,
    )
    .expect("fixture programs support production continuous-refresh derivation");
    solve::ContinuousSolveSystem::construct(
        solve_layout,
        solve::ContinuousSolveSystemInputs::new(
            implicit_rhs,
            implicit_row_targets,
            algebraic_projection_plan,
            residual,
            manifold,
            derivative_rhs,
            refresh_plans,
        ),
    )
    .expect("fixture continuous system satisfies the production constructor")
}

/// Test-only Solve scalar-plan C spelling fixture (see
/// `test_fixtures/README.md`): keeps the Solve renderer machinery verified by
/// C compile-and-execute tests while no product target emits this spelling.
pub(super) fn solve_c_fixture_source() -> &'static str {
    include_str!("test_fixtures/solve_c_spelling.c.jinja")
}

/// Header half of the test-only Solve C spelling fixture.
pub(super) fn solve_c_fixture_header() -> &'static str {
    include_str!("test_fixtures/solve_c_spelling.h.jinja")
}
