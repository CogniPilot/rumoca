use crate::{
    ScalarSlot, SolveProblem, SolveVariableStorageRole as Role, SolveVariableStorageRun,
    SolveVariableValueKind as Kind, VarLayout,
};

fn input(base: usize, count: usize, kind: Kind) -> SolveVariableStorageRun {
    SolveVariableStorageRun {
        base: ScalarSlot::P {
            index: base,
            byte_offset: base.saturating_mul(8),
        },
        scalar_count: count,
        role: Role::ExternalInput,
        value_kind: kind,
    }
}

fn problem(capacity: usize, runs: Vec<SolveVariableStorageRun>) -> SolveProblem {
    let mut problem = SolveProblem {
        layout: VarLayout::from_parts(Default::default(), 0, capacity),
        ..Default::default()
    };
    problem.solve_layout.variable_storage_runs = runs;
    problem
}

#[test]
fn discrete_input_metadata_size_follows_declarations_not_cells() {
    let encoded = |count| {
        let problem = problem(count, vec![input(0, count, Kind::Integer)]);
        serde_json::to_vec(&super::discrete_inputs(&problem).unwrap())
            .unwrap()
            .len()
    };
    let small = encoded(6);
    let large = encoded(167_497);
    assert!(
        large <= small + 16,
        "one source declaration grew from {small} to {large} bytes"
    );
}

#[test]
fn discrete_input_runs_preserve_order_duplicates_and_selected_types() {
    let mut parameter = input(3, 2, Kind::Integer);
    parameter.role = Role::Parameter;
    let problem = problem(
        12,
        vec![
            input(6, 2, Kind::Boolean),
            input(0, 3, Kind::Real),
            parameter,
            input(10, 2, Kind::Enumeration),
            input(6, 1, Kind::Integer),
            input(0, 2, Kind::String),
        ],
    );
    let inputs = super::discrete_inputs(&problem).unwrap();
    let runs: Vec<_> = inputs
        .runs
        .iter()
        .map(|run| (run.p_base, run.count, run.seen_offset))
        .collect();
    assert_eq!(runs, [(6, 2, 0), (10, 2, 2), (6, 1, 4)]);
    assert_eq!(inputs.scalar_count, 5);
}

#[test]
fn discrete_input_runs_admit_empty_layout_and_zero_length_source_run() {
    let empty = super::discrete_inputs(&SolveProblem::default()).unwrap();
    assert!(empty.runs.is_empty());
    assert_eq!(empty.scalar_count, 0);
    let problem = problem(0, vec![input(0, 0, Kind::Integer)]);
    let zero = super::discrete_inputs(&problem).unwrap();
    assert_eq!(zero.runs.len(), 1);
    assert_eq!(zero.runs[0].count, 0);
    assert_eq!(zero.scalar_count, 0);
}

#[test]
fn discrete_input_runs_refuse_foreign_column_and_outside_capacity() {
    let mut foreign = input(0, 1, Kind::Integer);
    foreign.base = ScalarSlot::Y {
        index: 0,
        byte_offset: 0,
    };
    assert!(super::discrete_inputs(&problem(1, vec![foreign])).is_err());
    assert!(super::discrete_inputs(&problem(5, vec![input(4, 2, Kind::Boolean)])).is_err());
    let mut problem = problem(1, vec![input(0, 2, Kind::Integer)]);
    problem.solve_layout.compiled_parameter_len = 2;
    assert!(
        super::discrete_inputs(&problem).is_err(),
        "same frame owns capacity"
    );
}

#[test]
fn discrete_input_runs_refuse_parameter_and_seen_prefix_overflow() {
    let overflow = problem(usize::MAX, vec![input(usize::MAX, 1, Kind::Integer)]);
    assert_eq!(
        super::discrete_inputs(&overflow).unwrap_err(),
        "a discrete input parameter range overflowed"
    );
    let overflow = problem(
        usize::MAX,
        vec![
            input(0, usize::MAX, Kind::Integer),
            input(0, 1, Kind::Boolean),
        ],
    );
    assert_eq!(
        super::discrete_inputs(&overflow).unwrap_err(),
        "the discrete input history size overflowed"
    );
}

#[test]
fn discrete_input_selection_keeps_unselected_storage_unread() {
    let mut foreign = input(0, 1, Kind::Real);
    foreign.base = ScalarSlot::Y {
        index: 0,
        byte_offset: 0,
    };
    let excluded = super::discrete_inputs(&problem(0, vec![foreign])).unwrap();
    assert!(excluded.runs.is_empty());
    assert_eq!(excluded.scalar_count, 0);
}

#[test]
fn scalar_event_profile_serializes_only_the_current_input_run_contract() {
    let profile = super::validate(&crate::SolveModel::default()).unwrap();
    let json = serde_json::to_value(profile).unwrap();
    assert!(json.get("discrete_inputs").is_none());
    assert_eq!(json["discrete_input_runs"]["scalar_count"], 0);
    assert_eq!(json["discrete_input_runs"]["runs"], serde_json::json!([]));
}
