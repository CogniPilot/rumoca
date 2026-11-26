//! A structured algebraic family kept as one `Map` (a dense layer, an image
//! kernel written with `sum`) has two forms in the lowered model.
//!
//! `continuous.residual` keeps the `Map`: one bounded loop for a native
//! backend. `continuous.implicit_rhs`, the source the refresh owners issue
//! exact assignments from, keeps the same rows as scalar programs, because a
//! row inside a `Map` has no scalar program to issue an assignment from. A
//! target that executes only assignments would otherwise refuse the model,
//! and the staged refresh would settle the row another way.

use rumoca::Compiler;
use rumoca_eval_solve::{RowEvalContext, eval_scalar_program_block_with_context};
use rumoca_ir_solve::{ComputeBlock, ComputeNode, ProjectionStage, SolveModel};
use rumoca_sim::SimOptions;

/// A dense layer between two scalar equations, with no state: the two forms
/// have the same nodes, and differ in the layer's node alone.
const AROUND: &str = r#"
model Around
  input Real x[4] = {0.5, -1.5, 2.0, 0.25};
  parameter Real w[3, 4] = {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 13}};
  Real before;
  Real y[3];
  output Real after;
equation
  before = 2*x[1];
  for row in 1:3 loop
    y[row] = sum(w[row, column]*x[column] for column in 1:4);
  end for;
  after = y[1] + y[3] + before;
end Around;
"#;

/// The layer followed by a second family that reads it. The second family is
/// an ordinary one (no `sum`), which the projection plan owns as one
/// declaration run of the refresh source.
const STACKED: &str = r#"
model Stacked
  input Real x[4] = {0.5, -1.5, 2.0, 0.25};
  parameter Real w[3, 4] = {{1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10, 11, 13}};
  Real y[3];
  Real z[3];
  output Real after;
equation
  for row in 1:3 loop
    y[row] = sum(w[row, column]*x[column] for column in 1:4);
  end for;
  for i in 1:3 loop
    z[i] = 2*y[i] + 1;
  end for;
  after = z[1] + z[3];
end Stacked;
"#;

/// `s[1] = a + b` written as a one-point family with `sum`.
const ONE_POINT: &str = r#"
model OnePoint
  Real a(start = 1, fixed = true);
  Real b(start = 2, fixed = true);
  Real s[1];
  Real acc(start = 0, fixed = true);
equation
  der(a) = 1;
  der(b) = 2;
  for i in 1:1 loop
    s[i] = sum({a, b});
  end for;
  der(acc) = s[1];
end OnePoint;
"#;

fn lower(source: &str, model: &str) -> SolveModel {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("{model} lowers: {error:?}"))
}

fn map_nodes(block: &ComputeBlock) -> usize {
    block
        .nodes
        .iter()
        .filter(|node| matches!(node, ComputeNode::Map { .. }))
        .count()
}

/// Every residual row of `block` at one fixed sample point.
fn rows_at_the_sample_point(model: &SolveModel, block: &ComputeBlock) -> Vec<f64> {
    let rows = rumoca_eval_solve::to_scalar_program_block(block)
        .expect("a continuous residual has a scalar view");
    let y = (0..model.problem.layout.y_scalars())
        .map(|slot| 0.25 + 0.5 * slot as f64)
        .collect::<Vec<_>>();
    let p = (0..model.problem.layout.p_scalars())
        .map(|slot| 1.5 - 0.125 * slot as f64)
        .collect::<Vec<_>>();
    let mut values = vec![f64::NAN; rows.stored_output_count()];
    eval_scalar_program_block_with_context(
        &rows,
        &y,
        &p,
        0.3,
        RowEvalContext {
            pure_calls: Some(&model.pure_calls),
            ..RowEvalContext::default()
        },
        &mut values,
    )
    .expect("the residual evaluates at the sample point");
    values
}

#[test]
fn a_mapped_family_has_the_same_rows_in_the_compact_residual_and_its_refresh_source() {
    let model = lower(AROUND, "Around");
    let continuous = &model.problem.continuous;
    assert_eq!(map_nodes(&continuous.residual), 1, "the layer is one Map");
    assert_eq!(
        map_nodes(&continuous.implicit_rhs),
        0,
        "the refresh source keeps the layer as scalar programs"
    );
    // Node for node: the scalar rows around the layer are the same nodes
    // in both forms, and the layer is the Map in one and scalar programs in
    // the other, so a node index names the same rows in both.
    assert!(
        continuous.residual.nodes.len() > 1,
        "scalar rows and the layer"
    );
    assert_eq!(
        continuous.residual.nodes.len(),
        continuous.implicit_rhs.nodes.len()
    );
    for (index, (compact, source)) in continuous
        .residual
        .nodes
        .iter()
        .zip(&continuous.implicit_rhs.nodes)
        .enumerate()
    {
        let rows = |node: &ComputeNode| {
            rumoca_eval_solve::to_scalar_program_block(&ComputeBlock {
                nodes: vec![node.clone()],
            })
            .expect("a residual node has a scalar view")
            .stored_output_count()
        };
        assert_eq!(
            rows(compact),
            rows(source),
            "node {index} holds the same rows"
        );
        assert!(
            matches!(source, ComputeNode::ScalarPrograms(_)),
            "node {index}"
        );
    }
    let compact = rows_at_the_sample_point(&model, &continuous.residual);
    let source = rows_at_the_sample_point(&model, &continuous.implicit_rhs);
    assert_eq!(compact.len(), 5, "before, three layer rows, after");
    assert!(compact.iter().all(|row| row.is_finite()), "{compact:?}");
    let bits = |rows: &[f64]| rows.iter().map(|row| row.to_bits()).collect::<Vec<_>>();
    assert_eq!(bits(&compact), bits(&source), "{compact:?} / {source:?}");
}

/// The native schedule reads the compact residual, so the layer is one stage
/// over its three targets, and every algebraic target is owned exactly once.
#[test]
fn the_native_schedule_takes_a_mapped_family_from_the_compact_residual() {
    let model = lower(AROUND, "Around");
    let owners = &model.problem.continuous.refresh_owners;
    let schedule = owners.native_assignment_schedule().unwrap_or_else(|| {
        panic!(
            "Around has a native schedule: {:?}",
            owners.native_assignment_refusal()
        )
    });
    let layer_stages = schedule
        .stages()
        .iter()
        .filter(|stage| map_nodes(stage.value_kernel()) == 1)
        .collect::<Vec<_>>();
    assert_eq!(layer_stages.len(), 1, "one stage holds the layer's Map");
    assert_eq!(layer_stages[0].target_count(), 3);
    let owned = schedule
        .stages()
        .iter()
        .map(|stage| stage.target_count())
        .sum::<usize>();
    assert_eq!(owned, 5, "before, three layer rows, after");
}

/// A declaration run of the projection plan after a mapped family: the plan
/// is derived from the refresh source, the native schedule reads the compact
/// residual, and both still name the same rows.
#[test]
fn a_family_after_a_mapped_family_keeps_the_plan_and_the_native_schedule_agreeing() {
    let model = lower(STACKED, "Stacked");
    let continuous = &model.problem.continuous;
    assert_eq!(map_nodes(&continuous.residual), 1);
    assert_eq!(map_nodes(&continuous.implicit_rhs), 0);
    assert_eq!(
        continuous.residual.nodes.len(),
        continuous.implicit_rhs.nodes.len()
    );
    assert!(
        continuous
            .algebraic_projection_plan
            .blocks
            .stages()
            .iter()
            .any(|stage| matches!(stage, ProjectionStage::SingletonDeclarationRun { .. })),
        "the plan owns a declaration run"
    );
    let bits = |rows: &[f64]| rows.iter().map(|row| row.to_bits()).collect::<Vec<_>>();
    let compact = rows_at_the_sample_point(&model, &continuous.residual);
    assert_eq!(compact.len(), 7, "three layer rows, three more, after");
    assert_eq!(
        bits(&compact),
        bits(&rows_at_the_sample_point(&model, &continuous.implicit_rhs))
    );
    let owners = &continuous.refresh_owners;
    let schedule = owners.native_assignment_schedule().unwrap_or_else(|| {
        panic!(
            "Stacked has a native schedule: {:?}",
            owners.native_assignment_refusal()
        )
    });
    let layer_stages = schedule
        .stages()
        .iter()
        .filter(|stage| map_nodes(stage.value_kernel()) == 1)
        .collect::<Vec<_>>();
    assert_eq!(layer_stages.len(), 1, "one stage holds the layer's Map");
    assert_eq!(layer_stages[0].target_count(), 3);
    let owned = schedule
        .stages()
        .iter()
        .map(|stage| stage.target_count())
        .sum::<usize>();
    assert_eq!(owned, 7, "every target is owned once");
}

/// One domain point is one scalar equation, not a loop: it is not mapped, and
/// the model integrates `s = a + b = 3 + 3t` to `acc(1) = 4.5`.
#[test]
fn a_one_point_family_stays_a_scalar_row_and_keeps_its_value() {
    let model = lower(ONE_POINT, "OnePoint");
    assert_eq!(map_nodes(&model.problem.continuous.residual), 0);
    let compiled = Compiler::new()
        .model("OnePoint")
        .compile_str(ONE_POINT, "OnePoint.mo")
        .expect("OnePoint compiles");
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("OnePoint simulates");
    let column = result
        .names
        .iter()
        .position(|name| name == "acc")
        .expect("acc is recorded");
    let acc = *result.data[column].last().expect("a sample");
    assert!((acc - 4.5).abs() < 1e-6, "acc(1) = {acc}");
}
