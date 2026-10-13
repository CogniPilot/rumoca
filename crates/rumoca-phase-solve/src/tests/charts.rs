//! Withholding alternate charts renumbers the issued exchanges that survive.

use rumoca_ir_solve as solve;

fn exchange(scalar: u32, status: solve::ChartExchangeStatus) -> solve::ChartExchange {
    let coordinate = |variable: &str| solve::ChartCoordinate {
        variable: variable.to_string(),
        scalar,
    };
    solve::ChartExchange {
        dependent: coordinate("dependent"),
        incoming: coordinate("incoming"),
        status,
    }
}

/// A withheld chart leaves the set, its exchange takes the withholding status,
/// later issued exchanges shift down by one chart, and an exchange that was
/// never issued keeps its status.
#[test]
fn withheld_charts_leave_the_set_and_later_exchanges_renumber() {
    use solve::ChartExchangeStatus::{Issued, WithheldByCap, WithheldByConstruction};
    let mut set = solve::ReducedChartSet {
        charts: (0..4)
            .map(|chart| solve::ReducedChart {
                independent_y_indices: vec![chart],
                ..Default::default()
            })
            .collect(),
        exchanges: vec![
            exchange(1, Issued { chart: 1 }),
            exchange(2, Issued { chart: 2 }),
            exchange(3, Issued { chart: 3 }),
            exchange(4, WithheldByCap),
        ],
    };
    crate::withhold_charts(&mut set, &[(2, WithheldByConstruction)]);
    let kept: Vec<_> = set
        .charts
        .iter()
        .map(|chart| chart.independent_y_indices[0])
        .collect();
    assert_eq!(kept, [0, 1, 3]);
    let statuses: Vec<_> = set
        .exchanges
        .iter()
        .map(|exchange| exchange.status)
        .collect();
    assert_eq!(
        statuses,
        [
            Issued { chart: 1 },
            WithheldByConstruction,
            Issued { chart: 2 },
            WithheldByCap
        ]
    );
}

fn run(
    index: usize,
    role: solve::SolveVariableStorageRole,
    value_kind: solve::SolveVariableValueKind,
) -> solve::SolveVariableStorageRun {
    solve::SolveVariableStorageRun {
        base: solve::scalar_slot_y(index),
        scalar_count: 1,
        role,
        value_kind,
    }
}

fn solve_layout(
    runs: Vec<solve::SolveVariableStorageRun>,
    states: usize,
    algebraics: usize,
) -> solve::SolveLayout {
    solve::SolveLayout {
        variable_declarations: runs
            .iter()
            .map(|run| solve::SolveVariableDeclaration::new(run.role, run.value_kind))
            .collect(),
        variable_storage_runs: runs,
        state_scalar_count: states,
        algebraic_scalar_count: algebraics,
        ..Default::default()
    }
}

fn var_layout(shape_start: usize) -> solve::VarLayout {
    let source = rumoca_core::SourceId::from_source_name("chart.mo");
    let name = rumoca_core::VarName::new("$state_coordinates");
    solve::VarLayout::from_interned_parts_with_shapes_and_spans(
        [(name.clone(), solve::scalar_slot_y(0))].into(),
        [(name.clone(), vec![1])].into(),
        [(
            name,
            rumoca_core::Span::from_offsets(source, shape_start, shape_start + 1),
        )]
        .into(),
        1,
        0,
    )
    .unwrap()
}

fn admitted(
    (a, a_start): (&solve::SolveLayout, usize),
    (b, b_start): (&solve::SolveLayout, usize),
) -> bool {
    crate::same_exchange_layout((&var_layout(a_start), a), (&var_layout(b_start), b))
}

/// Two charts of one exchange share their addressing and storage, whatever
/// source span a synthetic shape was declared at; a moved slot, another value
/// kind, another role or another split is a different layout.
#[test]
fn alternate_chart_layout_matches_by_addressing_not_declaration_span() {
    use solve::SolveVariableStorageRole::{Algebraic, State};
    use solve::SolveVariableValueKind::{Integer, Real};
    let primary = solve_layout(vec![run(0, State, Real), run(1, Algebraic, Real)], 1, 1);
    let same = solve_layout(vec![run(0, State, Real), run(1, Algebraic, Real)], 1, 1);
    assert!(admitted((&primary, 25), (&same, 46)));
    let moved = solve_layout(vec![run(1, State, Real), run(0, Algebraic, Real)], 1, 1);
    assert!(!admitted((&primary, 25), (&moved, 25)));
    let retyped = solve_layout(vec![run(0, State, Integer), run(1, Algebraic, Real)], 1, 1);
    assert!(!admitted((&primary, 25), (&retyped, 25)));
    let rerolled = solve_layout(vec![run(0, Algebraic, Real), run(1, State, Real)], 1, 1);
    assert!(!admitted((&primary, 25), (&rerolled, 25)));
    let split = solve_layout(vec![run(0, State, Real), run(1, Algebraic, Real)], 2, 0);
    assert!(!admitted((&primary, 25), (&split, 25)));
}
