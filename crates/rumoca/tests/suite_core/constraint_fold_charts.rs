//! SPEC_0040 STRUCT-T07 constraint-fold chart rows: a reduced constraint group
//! whose reconstructed state-class coordinate can fold issues ranked single
//! exchanges as alternate charts, with a coverage record of every exchange.

use std::collections::HashMap;
use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_ir_solve::{ChartCoordinate, ChartExchangeStatus, SolveModel};

const SPLIT_CIRCLE_CHART: &str = include_str!("../fixtures/index_reduction/SplitCircleChart.mo");

fn lowered(source: &str, model: &str) -> SolveModel {
    let dae = Compiler::new()
        .model(model)
        .compile_str(source, "constraint_fold_charts.mo")
        .unwrap()
        .dae;
    rumoca_phase_solve::lower_solve_model(&dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone()
}

fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.exists().then_some(root)
}

fn lowered_msl(root: &std::path::Path, model: &str) -> SolveModel {
    let dae = Compiler::new()
        .model(model)
        .source_root(root.to_str().expect("MSL root path is UTF-8"))
        .compile_str(
            "package ChartProbe import Modelica; end ChartProbe;",
            "constraint_fold_charts.mo",
        )
        .unwrap()
        .dae;
    rumoca_phase_solve::lower_solve_model(&dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone()
}

fn name(model: &SolveModel, y: usize) -> &str {
    &model.problem.solve_layout.solver_maps.names[y]
}

fn coordinate(variable: &str, scalar: u32) -> ChartCoordinate {
    ChartCoordinate {
        variable: variable.to_string(),
        scalar,
    }
}

#[test]
fn a_split_circle_exchanges_its_reconstructed_coordinate_for_the_integrated_one() {
    // `x` and `y` are separate state declarations bound by `x*x + y*y = 1`, a
    // loop closure the formal path reduces. The primary integrates `y` (its start
    // is fixed) and reconstructs `x`, whose slope `2*x` vanishes at a quarter
    // turn; the one coupled exchange integrates `x` and reconstructs `y`.
    let model = lowered(SPLIT_CIRCLE_CHART, "SplitCircleChart");
    let set = &model.problem.continuous.reduced_chart_set;
    assert_eq!(set.charts.len(), 2, "a primary and one exchange");
    let dependent = |chart: usize| {
        set.charts[chart]
            .dependent_y_indices
            .iter()
            .map(|&y| name(&model, y))
            .collect::<Vec<_>>()
    };
    let independent = |chart: usize| {
        set.charts[chart]
            .independent_y_indices
            .iter()
            .map(|&y| name(&model, y))
            .collect::<Vec<_>>()
    };
    assert_eq!((dependent(0), independent(0)), (vec!["x"], vec!["y"]));
    assert_eq!((dependent(1), independent(1)), (vec!["y"], vec!["x"]));
    assert!(
        set.charts[0].plan.is_none(),
        "the primary runs as the model"
    );
    assert!(
        set.charts[1].plan.is_some(),
        "the exchange carries its plan"
    );
    // The exchange is regular where the primary folds, not at the trial point
    // (y = 0), so its trial conditioning is recorded and does not drop it.
    assert!(set.charts[1].trial_rcond <= set.charts[1].trial_singular_threshold);
    assert_eq!(set.exchanges.len(), 1);
    assert_eq!(set.exchanges[0].dependent, coordinate("x", 0));
    assert_eq!(set.exchanges[0].incoming, coordinate("y", 0));
    assert_eq!(
        set.exchanges[0].status,
        ChartExchangeStatus::Issued { chart: 1 }
    );
}

#[test]
fn an_exchange_shares_the_primary_solver_layout() {
    let model = lowered(SPLIT_CIRCLE_CHART, "SplitCircleChart");
    let set = &model.problem.continuous.reduced_chart_set;
    let plan = set.charts[1].plan.as_ref().expect("the exchange plan");
    // The alternate reconstructs `y` in its own projection plan over the same
    // solver-Y slots the primary uses.
    let y = model
        .problem
        .solve_layout
        .solver_maps
        .names
        .iter()
        .position(|candidate| candidate == "y")
        .expect("y has a solver slot");
    assert!(
        plan.algebraic_projection_plan
            .blocks
            .iter()
            .any(|block| block.y_indices.contains(&y)),
        "the exchange plan reconstructs y"
    );
}

#[test]
fn a_model_whose_only_integrated_coordinate_is_always_issues_no_exchange() {
    // `p` is `StateSelect.always`, so the circle's reconstructed coordinate has no
    // admissible partner and the chart set stays as the static selection left it.
    let source = r#"
model ForcedCircle
  Real x(start = 1);
  Real p(start = 0, fixed = true, stateSelect = StateSelect.always);
  Real vx(start = 0);
  Real vp(start = 1, fixed = true, stateSelect = StateSelect.always);
  Real lambda;
equation
  der(x) = vx;
  der(p) = vp;
  der(vx) = lambda*x;
  der(vp) = lambda*p;
  x*x + p*p = 1;
end ForcedCircle;
"#;
    let model = lowered(source, "ForcedCircle");
    let set = &model.problem.continuous.reduced_chart_set;
    assert!(set.exchanges.is_empty());
    assert!(set.charts.iter().all(|chart| chart.plan.is_none()));
}

#[test]
fn msl_universal_constraint_exchanges_its_middle_cardan_angle() {
    let Some(root) = msl_root() else {
        return;
    };
    let model = lowered_msl(
        &root,
        "Modelica.Mechanics.MultiBody.Examples.Constraints.UniversalConstraint",
    );
    let set = &model.problem.continuous.reduced_chart_set;
    let angle = "freeMotionScalarInit.initAngle.angle";
    assert_eq!(
        set.exchanges
            .iter()
            .map(|e| (e.dependent.clone(), e.incoming.clone(), e.status))
            .collect::<Vec<_>>(),
        vec![
            (
                coordinate(angle, 1),
                coordinate(angle, 0),
                ChartExchangeStatus::Issued { chart: 1 }
            ),
            (
                coordinate(angle, 1),
                coordinate(angle, 2),
                ChartExchangeStatus::Issued { chart: 2 }
            ),
        ]
    );
    assert_eq!(set.charts.len(), 3);
    assert!(set.charts[1..].iter().all(|chart| chart.plan.is_some()));
}

#[test]
fn msl_fourbar1_issues_no_exchange() {
    // The loop's only integrated coordinate, `j1.phi`, is `StateSelect.always`.
    let Some(root) = msl_root() else {
        return;
    };
    let model = lowered_msl(
        &root,
        "Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1",
    );
    let set = &model.problem.continuous.reduced_chart_set;
    assert!(set.charts.is_empty());
    assert!(set.exchanges.is_empty());
}

#[test]
fn a_locked_quaternion_exchanges_its_integrated_component() {
    // Three holonomic rows leave one integrated quaternion component; each
    // reconstructed component is a state-class dependent the norm couples to it.
    let model = lowered(
        include_str!("../fixtures/index_reduction/QuaternionLockInline.mo"),
        "QuaternionLockInline",
    );
    let set = &model.problem.continuous.reduced_chart_set;
    assert_eq!(
        set.exchanges
            .iter()
            .map(|e| (e.dependent.clone(), e.incoming.clone(), e.status))
            .collect::<Vec<_>>(),
        (1..4)
            .map(|scalar| (
                coordinate("q", scalar),
                coordinate("q", 0),
                ChartExchangeStatus::Issued {
                    chart: scalar as usize
                }
            ))
            .collect::<Vec<_>>()
    );
    assert_eq!(set.charts.len(), 4);
}
