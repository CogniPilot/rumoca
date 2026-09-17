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
use rumoca_ir_solve::{AlgebraicProjectionPlan, ComputeBlock, ComputeNode, LinearOp, SolveModel};

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

/// The residual row a single-row, single-unknown algebraic block uses to
/// reconstruct solver-Y index `y`, if one does.
fn scalar_block_row_for(plan: &AlgebraicProjectionPlan, y: usize) -> Option<usize> {
    plan.blocks
        .iter()
        .find(|block| block.rows.len() == 1 && block.y_indices == [y])
        .map(|block| block.rows[0])
}

/// The solver-Y index a single-row, single-unknown algebraic block reconstructs
/// from residual row `row`, if one does.
fn scalar_block_reconstructed_from(plan: &AlgebraicProjectionPlan, row: usize) -> Option<usize> {
    plan.blocks
        .iter()
        .find(|block| block.rows == [row] && block.y_indices.len() == 1)
        .map(|block| block.y_indices[0])
}

/// The sole solver-Y index a state-derivative kernel loads: the base formal
/// derivative slot that advances its first integrated coordinate.
fn sole_load_y_index(block: &ComputeBlock) -> Option<usize> {
    block.nodes.iter().find_map(|node| match node {
        ComputeNode::Map { base_ops, .. } => base_ops.iter().find_map(|op| match op {
            LinearOp::LoadY { index, .. } => Some(*index),
            _ => None,
        }),
        _ => None,
    })
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
fn circle_chart_primary_chart_carries_no_executable_plan() {
    // The primary basis (chart index zero) is executed by the enclosing
    // continuous system, so it carries no separate re-lowered plan; only the
    // alternate charts do.
    let model = lowered(CIRCLE_CHART, "CircleChart");
    let charts = &model.problem.continuous.reduced_chart_set.charts;
    assert_eq!(charts.len(), 2);
    assert!(
        charts[0].plan.is_none(),
        "the primary chart is executed by the enclosing continuous system"
    );
}

#[test]
fn circle_chart_alternate_chart_re_lowers_to_a_regular_mirror_plan() {
    // The alternate chart is the primary coordinate transformation run with the
    // mirror Independent set, re-lowered through the same machinery. Its plan must
    // reconstruct the mirror dependent coordinate q[2] from the conserved
    // q*q = 1 residual (a regular reconstruction at a generic non-fold point) and
    // advance its own integrated coordinate q[1] by q[1]'s formal derivative, not
    // q[2]'s. Both artifacts live in the same solver-Y space as the primary basis.
    let model = lowered(CIRCLE_CHART, "CircleChart");
    let maps = model.problem.solve_layout.solver_maps();
    let names = &maps.names;
    let continuous = &model.problem.continuous;
    let charts = &continuous.reduced_chart_set.charts;
    assert_eq!(charts.len(), 2);

    let primary = &charts[0];
    let alternate = &charts[1];
    let plan = alternate
        .plan
        .as_ref()
        .expect("an alternate chart carries a re-lowered executable plan");

    // The alternate integrates q[1] and reconstructs q[2]: the exact swap of the
    // primary, confirmed against the source names in the shared solver-Y space.
    assert_eq!(names[alternate.independent_y_indices[0]], "q[1]");
    assert_eq!(names[alternate.dependent_y_indices[0]], "q[2]");

    // The primary reconstructs its dependent q[1] from the conserved-norm residual
    // row; the alternate reconstructs its dependent q[2] from that SAME row. A
    // regular first-integral reconstruction away from the fold.
    let norm_row = scalar_block_row_for(
        &continuous.algebraic_projection_plan,
        primary.dependent_y_indices[0],
    )
    .expect("the primary reconstructs its dependent from a single norm residual row");
    assert_eq!(
        scalar_block_reconstructed_from(&plan.algebraic_projection_plan, norm_row),
        Some(alternate.dependent_y_indices[0]),
        "the alternate reconstructs q[2] from the q*q=1 residual row"
    );

    // The alternate reconstructs its now-integrated coordinate q[1] from the
    // generated $state_coordinates identity row -- the row that binds the primary's
    // integrated coordinate q[2] in the primary plan. This identity row exists only
    // because the alternate was re-lowered with q[1] integrated.
    let identity_row = scalar_block_row_for(
        &continuous.algebraic_projection_plan,
        primary.independent_y_indices[0],
    )
    .expect("the primary reconstructs its integrated coordinate from an identity row");
    assert_eq!(
        scalar_block_reconstructed_from(&plan.algebraic_projection_plan, identity_row),
        Some(alternate.independent_y_indices[0]),
        "the alternate reconstructs q[1] from the generated identity row"
    );

    // The alternate derivative kernel advances q[1] by its own formal derivative
    // ($formal_derivative.1.q[1]); the primary advances q[2] by $formal_derivative.1.q[2].
    let integrated = &names[alternate.independent_y_indices[0]];
    let alternate_slot = maps
        .name_to_idx
        .get(&format!("$formal_derivative.1.{integrated}"))
        .copied()
        .expect("q[1] has a formal derivative slot");
    let primary_integrated = &names[primary.independent_y_indices[0]];
    let primary_slot = maps
        .name_to_idx
        .get(&format!("$formal_derivative.1.{primary_integrated}"))
        .copied()
        .expect("q[2] has a formal derivative slot");
    assert_eq!(
        sole_load_y_index(&plan.derivative_rhs),
        Some(alternate_slot),
        "the alternate kernel loads q[1]'s derivative slot"
    );
    assert_eq!(
        sole_load_y_index(&continuous.derivative_rhs),
        Some(primary_slot),
        "the primary kernel loads q[2]'s derivative slot"
    );
    assert_ne!(
        sole_load_y_index(&plan.derivative_rhs),
        sole_load_y_index(&continuous.derivative_rhs),
        "the alternate advances a different integrated coordinate than the primary"
    );
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
