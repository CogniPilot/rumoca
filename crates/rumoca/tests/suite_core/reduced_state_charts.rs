//! SPEC_0053 section 2a: the reduced state selection issues a bounded set of
//! admissible reconstruction charts for a folding definitional first-integral
//! coordinate group, carried inertly on the continuous solve system.
//!
//! A conserved holonomic norm `g = x*x - 1` has no globally injective reduced
//! chart: its gradient `g_d = 2*x_i` vanishes as `x_i` passes through zero, so a
//! fixed reduced basis folds. The compiler enumerates one admissible
//! reconstruction chart per source scalar of the group, keeping the primary
//! (regular at the trial point) as chart zero and its mirror alongside, and maps
//! each to the real lowered shape: a separate scalar algebraic block plus the
//! generated `$state_coordinates`. Stage 1 only records the set; it changes no
//! primary basis and no trajectory.

use std::collections::HashMap;
use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_ir_solve::{AlgebraicProjectionPlan, SolveModel};

fn lowered(source: &str, model: &str) -> SolveModel {
    let dae = Compiler::new()
        .model(model)
        .compile_str(source, "reduced_state_charts.mo")
        .unwrap()
        .dae;
    rumoca_phase_solve::lower_solve_model(&dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone()
}

/// The Modelica Standard Library root the cohort resolves against, resolved
/// relative to this crate. Returns `None` when the library artifact is absent,
/// so the negative cohort assertions below are exercised wherever it is present
/// and skipped where it is not fetched.
fn msl_root() -> Option<PathBuf> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    root.exists().then_some(root)
}

fn lowered_msl(root: &std::path::Path, model: &str) -> SolveModel {
    // The local unit only has to reference the `Modelica` root so it is loaded;
    // the model itself is resolved by name out of that root.
    let dae = Compiler::new()
        .model(model)
        .source_root(root.to_str().expect("MSL root path is UTF-8"))
        .compile_str(
            "package ChartProbe import Modelica; end ChartProbe;",
            "reduced_state_charts.mo",
        )
        .unwrap()
        .dae;
    rumoca_phase_solve::lower_solve_model(&dae, &HashMap::new(), |_| {})
        .unwrap()
        .model()
        .clone()
}

/// True when `y_index` is reconstructed by its own scalar (one row, one unknown)
/// algebraic projection block: the real lowered shape of a reduced first-integral
/// reconstruction, as opposed to a fictional single multi-chart manifold block.
fn has_scalar_block(plan: &AlgebraicProjectionPlan, y_index: usize) -> bool {
    plan.blocks
        .iter()
        .any(|block| block.rows.len() == 1 && block.y_indices == [y_index])
}

const CIRCLE_CHART: &str = include_str!("../fixtures/index_reduction/CircleChart.mo");

#[test]
fn circle_chart_yields_two_reduced_charts_over_the_real_lowered_shape() {
    let model = lowered(CIRCLE_CHART, "CircleChart");
    let names = &model.problem.solve_layout.solver_maps().names;
    let continuous = &model.problem.continuous;
    let charts = &continuous.reduced_chart_set.charts;

    // Exactly two admissible charts: the primary and its single mirror.
    assert_eq!(charts.len(), 2, "CircleChart is a two-component norm group");

    let primary = &charts[0];
    let mirror = &charts[1];

    // The primary integrates q[2] and reconstructs q[1]; its dependent Jacobian
    // is regular at the trial point.
    assert_eq!(primary.independent_y_indices.len(), 1);
    assert_eq!(primary.dependent_y_indices.len(), 1);
    assert_eq!(names[primary.independent_y_indices[0]], "q[2]");
    assert_eq!(names[primary.dependent_y_indices[0]], "q[1]");
    assert!(
        primary.trial_rcond > primary.trial_singular_threshold,
        "the primary chart is regular at the trial point: rcond {} threshold {}",
        primary.trial_rcond,
        primary.trial_singular_threshold
    );

    // The mirror integrates q[1] and reconstructs q[2]: the exact swap of the
    // primary. It is a structurally admissible chart even though its dependent
    // Jacobian is singular at this trial point, because it is regular where the
    // primary folds (q[1] -> 0).
    assert_eq!(names[mirror.independent_y_indices[0]], "q[1]");
    assert_eq!(names[mirror.dependent_y_indices[0]], "q[2]");
    assert_eq!(mirror.independent_y_indices, primary.dependent_y_indices);
    assert_eq!(mirror.dependent_y_indices, primary.independent_y_indices);

    // Every chart maps to the real lowered shape: its reconstructed coordinate is
    // a separate scalar algebraic block, not a fictional multi-chart manifold
    // block, and no state is projected onto a retained manifold.
    assert!(continuous.manifold_projection_plan.blocks.is_empty());
    for chart in charts {
        assert!(has_scalar_block(
            &continuous.algebraic_projection_plan,
            chart.dependent_y_indices[0]
        ));
    }
}

#[test]
fn models_without_a_folding_first_integral_group_carry_no_reduced_charts() {
    // An unconstrained ODE has no holonomic constraint and never reduces.
    let unconstrained = r#"
model Unconstrained
  Real x(start=1); Real vx(start=0);
equation
  der(x) = vx;
  der(vx) = -x;
end Unconstrained;
"#;

    // A holonomic system whose position constraint is expressed through implicit
    // contact coordinates reduces, but its deepest stage is not a single-scalar
    // first-integral norm group, so it issues no reconstruction charts.
    let implicit_contact = r#"
model ImplicitContact
  parameter Real radius = 1;
  Real x(start=1,fixed=true); Real y(start=0,fixed=true);
  Real vx(start=0,fixed=true); Real vy(start=1,fixed=true);
  Real s; Real w; Real lambda;
equation
  der(x) = vx;
  der(y) = vy;
  der(vx) = -lambda*x;
  der(vy) = -lambda*y;
  s + w = x; s - w = y; 2*(s*s + w*w) = radius*radius;
end ImplicitContact;
"#;

    for (source, model) in [
        (unconstrained, "Unconstrained"),
        (implicit_contact, "ImplicitContact"),
        (
            include_str!("../fixtures/index_reduction/QuaternionLockInline.mo"),
            "QuaternionLockInline",
        ),
    ] {
        let lowered = lowered(source, model);
        assert!(
            lowered
                .problem
                .continuous
                .reduced_chart_set
                .charts
                .is_empty(),
            "{model} carries no reduced state-selection charts"
        );
    }
}

#[test]
fn a_redundant_loop_closure_carries_no_reduced_charts() {
    // A kinematic loop closure is over-determining at the position level and is
    // reduced by the ordinary reducer, not by the formal first-integral path, so
    // it issues no reconstruction charts.
    let Some(root) = msl_root() else {
        return;
    };
    let lowered = lowered_msl(
        &root,
        "Modelica.Mechanics.MultiBody.Examples.Constraints.PrismaticConstraint",
    );
    assert!(
        lowered
            .problem
            .continuous
            .reduced_chart_set
            .charts
            .is_empty(),
        "a redundant loop closure carries no reduced state-selection charts"
    );
}

#[test]
fn a_retained_definitional_model_carries_no_reduced_charts() {
    // A tree of quaternion bodies carries only conserved unit-norm first
    // integrals, which are retained and enforced by a min-norm manifold
    // projection rather than reduced to an independent basis. The reduce-path
    // chart enumeration never runs, so it issues no reconstruction charts.
    let Some(root) = msl_root() else {
        return;
    };
    let lowered = lowered_msl(
        &root,
        "Modelica.Mechanics.MultiBody.Examples.Rotational3DEffects.GyroscopicEffects",
    );
    let continuous = &lowered.problem.continuous;
    assert!(
        !continuous.manifold_projection_plan.blocks.is_empty(),
        "GyroscopicEffects retains its norms on a manifold projection"
    );
    assert!(
        continuous.reduced_chart_set.charts.is_empty(),
        "a retained definitional model carries no reduced state-selection charts"
    );
}
