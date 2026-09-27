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
