use rumoca_ir_solve as solve;
use rumoca_solver::SimOptions;

use crate::session::SimulationSession;

fn block(rows: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
    let span = solve::source_span_from_offsets(49, 0, 1)
        .require_provenance("Diffsol common-host fixture")
        .expect("fixture span is source-backed");
    solve::ScalarProgramBlock::with_source_span(rows, span).expect("fixture program is computable")
}

fn unit_integrator() -> solve::fmi::FmiComponent {
    let derivative = solve::ComputeBlock::from_scalar_program_block(block(vec![vec![
        solve::LinearOp::Const { dst: 0, value: 1.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]]));
    let mut problem = solve::SolveProblem::with_derivative_rhs(
        derivative,
        solve::VarLayout::from_parts(Default::default(), 1, 0),
    )
    .expect("explicit derivative problem constructs");
    problem.solve_layout.solver_maps.names = vec!["x".to_owned()];
    problem.solve_layout.solver_maps.name_to_idx = indexmap::IndexMap::from([("x".to_owned(), 0)]);
    problem.solve_layout.solver_maps.base_to_indices =
        indexmap::IndexMap::from([("x".to_owned(), vec![0])]);
    problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&problem)
            .expect("unit-integrator refresh owners construct");
    let mut model = solve::SolveModel {
        problem,
        initial_y: vec![0.0],
        solver_nominals: vec![1.0],
        ..Default::default()
    };
    model.artifacts.continuous.mass_matrix = solve::MassMatrix::Identity;
    model.artifacts.continuous.full_jacobian_v = block(vec![vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]]);
    model.problem.solve_layout.variable_storage_runs = vec![solve::SolveVariableStorageRun {
        base: solve::ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        },
        scalar_count: 1,
        role: solve::SolveVariableStorageRole::State,
        value_kind: solve::SolveVariableValueKind::Real,
    }];
    model.problem.solve_layout.variable_declarations = vec![solve::SolveVariableDeclaration::new(
        solve::SolveVariableStorageRole::State,
        solve::SolveVariableValueKind::Real,
    )];
    solve::fmi::FmiComponent::construct(model, vec![state_input()])
        .expect("unit-integrator FMI component constructs")
}

fn state_input() -> solve::fmi::FmiVariableInput {
    solve::fmi::FmiVariableInput {
        name: "x".to_owned(),
        scalar_names: vec!["x".to_owned()],
        role: solve::SolveVariableStorageRole::State,
        value_kind: solve::SolveVariableValueKind::Real,
        dimensions: Vec::new(),
        start: vec![0.0],
        minimum: None,
        maximum: None,
        nominal: Some(vec![1.0]),
        unit: None,
        description: None,
        causality: solve::fmi::FmiCausality::Local,
        variability: solve::fmi::FmiVariability::Continuous,
        tunable: false,
        declaration: solve::source_span_from_offsets(49, 0, 1),
    }
}

#[test]
fn the_live_bdf_wrapper_uses_the_common_host_session() {
    let mut session = SimulationSession::new(
        unit_integrator(),
        SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.01),
            ..Default::default()
        },
    )
    .expect("common live BDF session constructs");

    session.advance_to(0.1).expect("common host advances");
    assert_eq!(session.time().to_bits(), 0.1f64.to_bits());
    assert_eq!(session.max_schedule_advance_dt(), None);
}

#[test]
fn a_zero_state_component_selects_the_same_time_only_plugin() {
    let mut model = solve::SolveModel::default();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&model.problem)
            .expect("empty refresh owners construct");
    let component = solve::fmi::FmiComponent::construct(model, Vec::new())
        .expect("zero-state FMI component constructs");
    let mut session = SimulationSession::new(
        component,
        SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("zero-state common session constructs");

    session.advance_to(0.25).expect("time-only plugin advances");
    assert_eq!(session.time().to_bits(), 0.25f64.to_bits());
}
