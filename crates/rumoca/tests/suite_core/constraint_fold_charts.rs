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
fn a_linear_loop_closure_is_exempt_by_its_constant_slope() {
    // `x1 + 2*x2 = 1` is affine with constant coefficients, so every chart's slope
    // is a constant matrix that cannot vanish: the exchange is ranked but withheld,
    // and no executable alternate (hence no C refusal) is issued.
    let model = lowered(
        include_str!("../fixtures/index_reduction/LinearLoop.mo"),
        "LinearLoop",
    );
    let set = &model.problem.continuous.reduced_chart_set;
    assert_eq!(
        set.exchanges
            .iter()
            .map(|e| (e.dependent.clone(), e.incoming.clone(), e.status))
            .collect::<Vec<_>>(),
        vec![(
            coordinate("x1", 0),
            coordinate("x2", 0),
            ChartExchangeStatus::WithheldBySlopeInvariance
        )]
    );
    assert!(set.charts.iter().all(|chart| chart.plan.is_none()));
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

/// Simulate a fixture through the default component host.
fn simulate(
    source: &str,
    model: &str,
    t_end: f64,
    tolerance: f64,
) -> Result<rumoca_sim::SimResult, rumoca_sim::SimulationDiagnosticError> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "constraint_fold_charts.mo")
        .unwrap();
    rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end,
            dt: Some(0.01),
            rtol: tolerance,
            atol: tolerance,
            ..Default::default()
        },
    )
}

fn column(result: &rumoca_sim::SimResult, name: &str) -> usize {
    result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("trace exposes {name}"))
}

#[test]
fn a_split_circle_switches_charts_and_completes_its_revolution_on_the_physical_branch() {
    // x(t) = cos t, y(t) = sin t. A fixed chart reconstructing x from
    // x*x + y*y = 1 lands on the mirror root after a quarter turn; switching to
    // the exchange before each fold keeps the physical branch.
    let result = simulate(
        SPLIT_CIRCLE_CHART,
        "SplitCircleChart",
        std::f64::consts::TAU + 0.5,
        1e-8,
    )
    .expect("the split circle completes a revolution through chart switches");
    let (x, y) = (column(&result, "x"), column(&result, "y"));
    let mut worst = 0.0_f64;
    for (index, &time) in result.times.iter().enumerate() {
        worst = worst
            .max((result.data[x][index] - time.cos()).abs())
            .max((result.data[y][index] - time.sin()).abs());
    }
    assert!(
        worst < 1e-4,
        "the switched trajectory stays on the physical branch: worst error {worst}"
    );
}

#[test]
fn an_undetermined_index_one_algebraic_keeps_its_typed_projection_failure() {
    // `a2` is an index-1 algebraic of `sin(a3)*sin(a1) = cos(a3)*sin(a2)*cos(a1)`,
    // left undetermined where cos(a3) = 0 (t = 0.5). No reduced chart exists to
    // exchange, so the certified projection reports the singular row.
    let error = simulate(
        include_str!("../fixtures/index_reduction/CardanChartFold.mo"),
        "CardanChartFold",
        1.0,
        1e-10,
    )
    .expect_err("the undetermined algebraic is not silently continued");
    let message = error.to_string();
    assert!(
        message.contains("algebraic projection did not establish coordinate convergence")
            && message.contains("target=a2"),
        "{message}"
    );
}

#[test]
fn msl_universal_constraint_keeps_the_constrained_body_on_the_joint() {
    let Some(root) = msl_root() else {
        return;
    };
    let dae = Compiler::new()
        .model("Modelica.Mechanics.MultiBody.Examples.Constraints.UniversalConstraint")
        .source_root(root.to_str().expect("MSL root path is UTF-8"))
        .compile_str(
            "package ChartProbe import Modelica; end ChartProbe;",
            "constraint_fold_charts.mo",
        )
        .unwrap()
        .dae;
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &dae,
        &rumoca_sim::SimOptions {
            t_end: 10.0,
            dt: Some(0.01),
            rtol: 1e-10,
            atol: 1e-10,
            ..Default::default()
        },
    )
    .expect("UniversalConstraint runs to t = 10 through its Cardan fold");
    let mut worst = 0.0_f64;
    for k in 1..=3 {
        let joint = column(&result, &format!("bodyOfJoint.frame_b.r_0[{k}]"));
        let constrained = column(&result, &format!("bodyOfConstraint.frame_b.r_0[{k}]"));
        for (a, b) in result.data[joint].iter().zip(&result.data[constrained]) {
            worst = worst.max((a - b).abs());
        }
    }
    assert!(
        worst < 1e-4,
        "the constrained body tracks the jointed body: worst gap {worst}"
    );
}

/// Round-trip `model` through its wire and require every alternate plan to be
/// reproduced exactly by patching the decoded primary, while the wire carries
/// only deltas: under half the size of the whole plans.
fn assert_chart_deltas_are_faithful(model: &SolveModel) {
    let continuous = &model.problem.continuous;
    let wire = serde_json::to_vec(
        &serde_json::to_value(rumoca_phase_solve::solve_model_wire(model).unwrap()).unwrap(),
    )
    .unwrap();
    let replayed = rumoca_phase_solve::deserialize_solve_model(
        &mut serde_json::Deserializer::from_slice(&wire),
    )
    .expect("the chart-carrying model replays from its wire");
    let decoded = &replayed.problem.continuous;
    let mut alternates = 0;
    for (original, decoded) in continuous
        .reduced_chart_set
        .charts
        .iter()
        .zip(&decoded.reduced_chart_set.charts)
    {
        let (Some(original), Some(decoded)) = (&original.plan, &decoded.plan) else {
            assert!(original.plan.is_none() && decoded.plan.is_none());
            continue;
        };
        alternates += 1;
        assert_eq!(json(&decoded.residual), json(&original.residual));
        assert_eq!(json(&decoded.implicit_rhs), json(&original.implicit_rhs));
        assert_eq!(decoded.implicit_row_targets, original.implicit_row_targets);
        assert_eq!(
            decoded.algebraic_projection_plan,
            original.algebraic_projection_plan
        );
        assert_eq!(
            json(&decoded.derivative_rhs),
            json(&original.derivative_rhs)
        );
        assert_eq!(
            json(&decoded.refresh_owners),
            json(&original.refresh_owners)
        );
    }
    assert!(alternates > 0, "the model carries executable alternates");
    let chart_set =
        serde_json::to_string(&serde_json::to_value(continuous).unwrap()["reduced_chart_set"])
            .unwrap()
            .len();
    // Standalone, a chart serializes its whole plan: the size a copy would take.
    let whole_plans = serde_json::to_string(&continuous.reduced_chart_set.charts)
        .unwrap()
        .len();
    assert!(
        chart_set * 2 < whole_plans,
        "the chart set travels as deltas: {chart_set} bytes against {whole_plans} for whole plans"
    );
}

fn json<T: serde::Serialize>(value: &T) -> serde_json::Value {
    serde_json::to_value(value).unwrap()
}

#[test]
fn a_split_circle_alternate_travels_as_a_faithful_delta() {
    assert_chart_deltas_are_faithful(&lowered(SPLIT_CIRCLE_CHART, "SplitCircleChart"));
}

#[test]
fn msl_universal_constraint_alternates_travel_as_faithful_deltas() {
    let Some(root) = msl_root() else {
        return;
    };
    assert_chart_deltas_are_faithful(&lowered_msl(
        &root,
        "Modelica.Mechanics.MultiBody.Examples.Constraints.UniversalConstraint",
    ));
}
