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
use rumoca_ir_solve::{
    AlgebraicProjectionPlan, ComputeBlock, ComputeNode, ContinuousSolveSystem, LinearOp, SolveModel,
};

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

    // STRUCT-T02 quotients the prolonged `der(q) = v` relation after state
    // selection, so each kernel reads its integrated q[i]'s derivative through
    // the representative v[i]: the alternate advances q[1] by v[1], the primary
    // q[2] by v[2].
    let integrated = &names[alternate.independent_y_indices[0]];
    let primary_integrated = &names[primary.independent_y_indices[0]];
    let velocity_slot = |integrated: &str| {
        maps.name_to_idx
            .get(&integrated.replacen('q', "v", 1))
            .copied()
            .expect("each integrated q[i] has a velocity v[i]")
    };
    assert_eq!(
        sole_load_y_index(&plan.derivative_rhs),
        Some(velocity_slot(integrated)),
        "the alternate kernel loads v[1]"
    );
    assert_eq!(
        sole_load_y_index(&continuous.derivative_rhs),
        Some(velocity_slot(primary_integrated)),
        "the primary kernel loads v[2]"
    );
    assert_ne!(
        sole_load_y_index(&plan.derivative_rhs),
        sole_load_y_index(&continuous.derivative_rhs),
        "the alternate advances a different integrated coordinate than the primary"
    );
}

/// Build the solver model of one alternate reduced chart by splicing its carried
/// continuous kernel and artifacts into the shared problem skeleton. This is the
/// per-chart image a runtime activates across a fold: the alternate basis executes
/// as the continuous system, backed by the alternate's own artifacts and refresh
/// owners, in the same solver-Y space as the primary.
fn alternate_chart_model(model: &SolveModel) -> SolveModel {
    let plan = model.problem.continuous.reduced_chart_set.charts[1]
        .plan
        .as_ref()
        .expect("the alternate chart carries an executable plan");
    let mut alternate = model.clone();
    alternate.problem.continuous = ContinuousSolveSystem {
        implicit_rhs: plan.implicit_rhs.clone(),
        implicit_row_targets: plan.implicit_row_targets.clone(),
        algebraic_projection_plan: plan.algebraic_projection_plan.clone(),
        residual: plan.residual.clone(),
        manifold_residual: ComputeBlock::default(),
        manifold_projection_plan: AlgebraicProjectionPlan::default(),
        derivative_rhs: plan.derivative_rhs.clone(),
        refresh_owners: plan.refresh_owners.clone(),
        reduced_chart_set: Default::default(),
    };
    alternate.artifacts.continuous = plan.artifacts.clone();
    alternate
}

#[test]
fn circle_chart_alternate_chart_carries_runtime_executable_artifacts() {
    // Stage 1c: each alternate reduced chart carries the complete runtime-executable
    // image of its basis, not only the reconstruction kernel. The continuous solver
    // constructor reads a forward-mode AD Jacobian-vector product of the implicit
    // residual, its per-row scalar form, a full state Jacobian, the derived
    // structural patterns, and the issued continuous refresh owners; all of them
    // must be present on the alternate chart or the chart is not executable.
    let model = lowered(CIRCLE_CHART, "CircleChart");
    let charts = &model.problem.continuous.reduced_chart_set.charts;
    assert_eq!(charts.len(), 2);
    let plan = charts[1]
        .plan
        .as_ref()
        .expect("the alternate chart carries an executable plan");

    // The alternate carries its own construction-issued continuous refresh owners.
    assert!(
        plan.refresh_owners.is_issued(),
        "the alternate chart carries construction-issued continuous refresh owners"
    );

    let artifacts = &plan.artifacts;
    // The implicit residual JVP is row-aligned with the alternate residual: one
    // Jacobian row per residual row. The alternate residual holds the shared
    // q*q = 1 norm row plus the generated $state_coordinates identity row for its
    // now-integrated coordinate, so a JVP that covers every row (not just the norm
    // row) is the proof the alternate residual is differentiable end to end.
    let residual_rows = plan
        .implicit_rhs
        .len()
        .expect("the alternate implicit residual is shaped");
    assert!(
        residual_rows >= 2,
        "the alternate residual holds the shared norm row and its generated identity row"
    );
    assert!(
        !artifacts.implicit_jacobian_v.nodes.is_empty(),
        "the alternate implicit JVP is a non-empty kernel"
    );
    assert_eq!(
        artifacts
            .implicit_jacobian_v
            .len()
            .expect("the alternate implicit JVP is shaped"),
        residual_rows,
        "the alternate implicit JVP covers every alternate residual row"
    );

    // The per-row scalar JVP and the full state-derivative Jacobian kernel used by
    // the state-only path are both present.
    assert!(
        artifacts.implicit_jacobian_v_scalar.row_count() > 0,
        "the alternate carries a per-row scalar implicit JVP"
    );
    assert!(
        artifacts.full_jacobian_v.row_count() > 0,
        "the alternate carries a full state-derivative Jacobian kernel"
    );

    // The derived structural artifacts are present and cover every alternate
    // algebraic block, including the block that reconstructs the now-integrated
    // coordinate from the generated identity row.
    assert!(
        artifacts.structural.implicit().is_some(),
        "the alternate carries a derived implicit Jacobian structure"
    );
    assert_eq!(
        artifacts.structural.algebraic_projection().len(),
        plan.algebraic_projection_plan.blocks.len(),
        "one derived Jacobian structure per alternate algebraic block"
    );

    // A reduced first-integral chart retains no manifold, so its manifold JVP is
    // empty.
    assert!(
        artifacts.manifold_jacobian_v.nodes.is_empty(),
        "a reduced first-integral chart retains no manifold projection"
    );

    // The primary chart is executed by the enclosing continuous system and carries
    // no separate plan or artifacts, exactly as before Stage 1c.
    assert!(charts[0].plan.is_none());
}

#[test]
fn circle_chart_alternate_chart_constructs_a_solve_runtime() {
    // The de-risking gate for the Stage 2 runtime swap: the alternate chart's
    // carried image is complete enough to construct the continuous solver runtime
    // with no missing-artifact error. Constructing the runtime exercises the exact
    // constructor path that reads the AD Jacobian, the structural artifacts, and
    // the refresh owners, so a successful build proves the alternate is executable
    // and that Stage 1c closed the last compiler gap.
    let model = lowered(CIRCLE_CHART, "CircleChart");
    let alternate = alternate_chart_model(&model);
    let runtime = rumoca_solver::SolveRuntime::new(&alternate);
    assert!(
        runtime.is_ok(),
        "the alternate reduced chart is a complete runtime-executable image: {:?}",
        runtime.err()
    );
}

#[test]
fn circle_chart_alternate_chart_artifacts_survive_the_model_wire() {
    // Chart artifacts are derived data and are absent from the serialized wire, like
    // the primary artifacts. Decoding the model rebuilds every alternate chart's
    // executable image through the same assembly used at construction, so a decoded
    // chart is byte-identical to the constructed one and remains runtime-executable.
    let model = lowered(CIRCLE_CHART, "CircleChart");
    let wire = serde_json::to_vec(
        &serde_json::to_value(rumoca_phase_solve::solve_model_wire(&model).unwrap()).unwrap(),
    )
    .unwrap();
    let replayed = rumoca_phase_solve::deserialize_solve_model(
        &mut serde_json::Deserializer::from_slice(&wire),
    )
    .expect("the chart-carrying model replays from its wire");

    let original = model.problem.continuous.reduced_chart_set.charts[1]
        .plan
        .as_ref()
        .expect("the constructed alternate carries a plan");
    let decoded = replayed.problem.continuous.reduced_chart_set.charts[1]
        .plan
        .as_ref()
        .expect("the alternate plan survives the wire");

    // The rebuilt artifacts are byte-identical to the constructed ones.
    assert_eq!(
        serde_json::to_value(&decoded.artifacts.implicit_jacobian_v_scalar).unwrap(),
        serde_json::to_value(&original.artifacts.implicit_jacobian_v_scalar).unwrap(),
        "the decoded scalar implicit JVP is byte-identical to the constructed one"
    );
    assert_eq!(
        serde_json::to_value(&decoded.artifacts.full_jacobian_v).unwrap(),
        serde_json::to_value(&original.artifacts.full_jacobian_v).unwrap(),
        "the decoded full Jacobian kernel is byte-identical to the constructed one"
    );
    assert_eq!(
        decoded
            .artifacts
            .implicit_jacobian_v
            .len()
            .expect("decoded JVP is shaped"),
        original
            .artifacts
            .implicit_jacobian_v
            .len()
            .expect("constructed JVP is shaped"),
    );
    assert!(!decoded.artifacts.implicit_jacobian_v.nodes.is_empty());
    assert_eq!(
        decoded.artifacts.structural.algebraic_projection().len(),
        original.artifacts.structural.algebraic_projection().len(),
    );
    assert!(decoded.refresh_owners.is_issued());

    // The decoded alternate still constructs a continuous solver runtime.
    let alternate = alternate_chart_model(&replayed);
    assert!(
        rumoca_solver::SolveRuntime::new(&alternate).is_ok(),
        "the decoded alternate chart is still runtime-executable"
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

/// SPEC_0053 section 2a, Stage 2 runtime chart swap (carrier A): a full
/// revolution of `CircleChart` re-selects a regular reduced chart at each
/// quarter turn and completes on the physical branch, matching the analytic
/// unit circle rather than the mirror root a fixed reduced basis folds onto.
///
/// The interior trace points are event-left observations the recorder captures
/// by saving the component FMU state at each accepted step and restoring it to
/// observe the left limit. Because those saved states span the chart switches
/// (the active chart index and its runtime), the analytic match of the whole
/// trace is also the get/set-FMU-state round-trip evidence: a save and restore
/// across a basis change preserves every observable.
#[test]
fn circle_chart_runtime_chart_swap_completes_the_revolution_on_the_physical_branch() {
    use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

    let compiled = Compiler::new()
        .model("CircleChart")
        .compile_str(CIRCLE_CHART, "reduced_state_charts.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: std::f64::consts::TAU,
            dt: Some(0.01),
            rtol: 1e-8,
            atol: 1e-8,
            ..Default::default()
        },
    )
    .expect("CircleChart completes a full revolution through the runtime chart swaps");

    let column = |name: &str| {
        result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("trace exposes {name}"))
    };
    let (q1, q2, v1, v2) = (
        column("q[1]"),
        column("q[2]"),
        column("v[1]"),
        column("v[2]"),
    );

    // q(t) = (cos t, sin t), v(t) = (-sin t, cos t). A mirror-branch failure
    // would leave q[1] off by up to two units; the sound swap keeps every
    // observable within a small multiple of the integration tolerance.
    let mut worst = 0.0_f64;
    for (index, &time) in result.times.iter().enumerate() {
        for (col, expected) in [
            (q1, time.cos()),
            (q2, time.sin()),
            (v1, -time.sin()),
            (v2, time.cos()),
        ] {
            worst = worst.max((result.data[col][index] - expected).abs());
        }
    }
    assert!(
        worst < 1e-4,
        "the swapped trajectory stays on the physical branch: worst observable error {worst}"
    );
}
