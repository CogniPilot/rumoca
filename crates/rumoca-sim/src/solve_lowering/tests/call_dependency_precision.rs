//! Dependency summaries of functions with loops, from Modelica source to the
//! issued pure-call owner: an output depends on the inputs its own
//! assignments read, on every pass, and on nothing another output read.

use std::collections::BTreeSet;

use super::tangent_jacobian::lower_source;

/// The call site of the one owner with this interface.
fn owner_site(
    source: &str,
    model: &str,
    inputs: usize,
    outputs: usize,
) -> rumoca_ir_solve::SolvePureCallSite {
    let lowered = lower_source(source, model, &[]);
    let mut sites = lowered
        .pure_calls
        .owners()
        .iter()
        .map(|owner| owner.call_site())
        .filter(|site| site.inputs().len() == inputs && site.outputs().len() == outputs)
        .collect::<Vec<_>>();
    sites.dedup();
    assert_eq!(sites.len(), 1, "one owner with this interface");
    sites.remove(0)
}

/// The input leaves each output of the one owner with this interface reads.
fn output_inputs(source: &str, model: &str, inputs: usize, outputs: usize) -> Vec<Vec<usize>> {
    owner_site(source, model, inputs, outputs)
        .output_dependencies()
        .iter()
        .map(|output| {
            let mut leaves: Vec<_> = output
                .iter()
                .flat_map(rumoca_ir_solve::SolveCallDependency::inputs)
                .collect();
            leaves.sort_unstable();
            leaves.dedup();
            leaves
        })
        .collect()
}

/// The input scalars (numbered over the concatenated inputs) each output
/// scalar reads, as the structural analysis expands one call of the site.
fn output_scalar_inputs(site: rumoca_ir_solve::SolvePureCallSite) -> Vec<BTreeSet<usize>> {
    use rumoca_ir_solve::{LinearOp, StructuralPattern, TensorInputKind};
    let mut count = 0;
    let starts = site
        .inputs()
        .iter()
        .map(|input| {
            let start = count;
            count += input.scalar_count();
            start
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let output_count = site.output_scalar_count().expect("a scalar call interface");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("call_dependency_precision.mo"),
        0,
        1,
    );
    StructuralPattern::derive_output_y_dependencies(
        &[
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: count as usize,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::PureCall {
                dst_start: count,
                input_starts: starts,
                site,
            },
            LinearOp::StoreOutputRange {
                start: count,
                count: output_count,
                stride: 1,
            },
        ],
        Some(span),
    )
    .expect("the call's output dependencies derive")
}

fn two_arrays(function: &str, body: &str) -> String {
    format!(
        "function {function}\n\
         input Real x[3];\n input Real y[3];\n\
         output Real a[3];\n output Real b[3];\n\
         algorithm\n a := zeros(3);\n b := zeros(3);\n{body}\
         end {function};\n\
         model {function}Model\n\
         Real x[3](each start = 1, each fixed = true);\n\
         Real y[3](each start = 2, each fixed = true);\n\
         Real a[3];\n Real b[3];\n\
         equation\n der(x) = -x;\n der(y) = -2 * y;\n\
         (a, b) = {function}(x, y);\n\
         end {function}Model;\n"
    )
}

#[test]
fn loop_outputs_keep_separate_input_dependencies() {
    let source = two_arrays(
        "Split",
        " for i in 1:3 loop\n a[i] := x[i];\n b[i] := y[i];\n end for;\n",
    );
    assert_eq!(
        output_inputs(&source, "SplitModel", 2, 2),
        vec![vec![0], vec![1]]
    );
}

#[test]
fn nested_loops_keep_separate_input_dependencies() {
    let source = two_arrays(
        "Nested",
        " for i in 1:3 loop\n for j in 1:2 loop\n\
         a[i] := a[i] + j * x[i];\n b[i] := b[i] + j * y[i];\n\
         end for;\n end for;\n",
    );
    assert_eq!(
        output_inputs(&source, "NestedModel", 2, 2),
        vec![vec![0], vec![1]]
    );
}

#[test]
fn loop_carried_accumulator_depends_on_every_pass_of_its_own_inputs_only() {
    let source = two_arrays(
        "Accumulate",
        " for i in 2:3 loop\n a[i] := a[i - 1] + x[i];\n b[i] := b[i - 1] * y[i];\n end for;\n",
    );
    assert_eq!(
        output_inputs(&source, "AccumulateModel", 2, 2),
        vec![vec![0], vec![1]]
    );
}

#[test]
fn index_affine_in_the_loop_variable_keeps_its_array() {
    let source = two_arrays(
        "Affine",
        " for i in 1:2 loop\n a[2 * i - 1] := x[i];\n b[4 - i] := y[i + 1];\n end for;\n",
    );
    assert_eq!(
        output_inputs(&source, "AffineModel", 2, 2),
        vec![vec![0], vec![1]]
    );
}

#[test]
fn early_exit_loop_adds_its_predicate_reads_to_every_carry() {
    let source = "function Early\n\
         input Real x[3];\n input Real y[3];\n input Real n;\n\
         output Real a[3];\n output Real b[3];\n\
         protected\n Integer j;\n\
         algorithm\n a := zeros(3);\n b := zeros(3);\n j := 1;\n\
         while j <= min(n, 3) loop\n a[j] := x[j];\n b[j] := y[j];\n j := j + 1;\n end while;\n\
         end Early;\n\
         model EarlyModel\n\
         Real x[3](each start = 1, each fixed = true);\n\
         Real y[3](each start = 2, each fixed = true);\n\
         Real a[3];\n Real b[3];\n\
         equation\n der(x) = -x;\n der(y) = -2 * y;\n\
         (a, b) = Early(x, y, 2 + x[1]);\n\
         end EarlyModel;\n";
    assert_eq!(
        output_inputs(source, "EarlyModel", 3, 2),
        vec![vec![0, 2], vec![1, 2]]
    );
}

#[test]
fn dynamic_index_is_conservative_on_its_array_only() {
    let source = "function Dynamic\n\
         input Real x[3];\n input Real y[3];\n input Real k;\n\
         output Real a[3];\n output Real b[3];\n\
         algorithm\n a := zeros(3);\n b := zeros(3);\n\
         for i in 1:3 loop\n a[integer(k)] := a[integer(k)] + x[i];\n b[i] := y[i];\n end for;\n\
         end Dynamic;\n\
         model DynamicModel\n\
         Real x[3](each start = 1, each fixed = true);\n\
         Real y[3](each start = 2, each fixed = true);\n\
         Real a[3];\n Real b[3];\n\
         equation\n der(x) = -x;\n der(y) = -2 * y;\n\
         (a, b) = Dynamic(x, y, 1 + x[1]);\n\
         end DynamicModel;\n";
    assert_eq!(
        output_inputs(source, "DynamicModel", 3, 2),
        vec![vec![0, 2], vec![1]]
    );
}

/// The shape of a joint-limit row builder: an outer loop over joints, scalar
/// temporaries (`d`, `distance`, `active`, the row index) rewritten on every
/// pass, friction rows written at the joint index and limit rows written by an
/// inner loop at `n + 2 * (i - 1) + side`.
const JOINT_ROWS: &str = "function Impedance
  input Real position;
  output Real d;
  output Real dp;
algorithm
  d := 0.5 + 0.1 * position * position;
  dp := 0.2 * position;
end Impedance;
function Rows
  input Real position[2];
  input Real velocity[2];
  input Real frictionLoss[2];
  input Real inverseWeight[2];
  output Real regularizer[6];
  output Real reference[6];
  output Real lower[6];
protected
  Real d;
  Real dp;
  Real distance;
  Real direction;
  Integer row;
  Boolean active;
algorithm
  regularizer := ones(6);
  reference := zeros(6);
  lower := zeros(6);
  d := 0;
  dp := 0;
  distance := 0;
  direction := 0;
  row := 1;
  active := false;
  for i in 1:2 loop
    (d, dp) := Impedance(position = 0);
    regularizer[i] := if frictionLoss[i] > 0 then max(1.0e-15, (1 - d) * inverseWeight[i] / d) else 1;
    reference[i] := if frictionLoss[i] > 0 then -velocity[i] else 0;
    lower[i] := -frictionLoss[i];
    for side in 1:2 loop
      row := 2 + 2 * (i - 1) + side;
      direction := if side == 1 then 1 else -1;
      distance := if side == 1 then position[i] else 1 - position[i];
      active := distance < 0;
      (d, dp) := Impedance(position = distance);
      regularizer[row] := if active then max(1.0e-15, (1 - d) * inverseWeight[i] / d) else 1;
      reference[row] := if active then -direction * velocity[i] - d * distance else 0;
    end for;
  end for;
end Rows;
model RowsModel
  Real position[2](each start = 1, each fixed = true);
  Real velocity[2](each start = 2, each fixed = true);
  Real frictionLoss[2](each start = 3, each fixed = true);
  Real inverseWeight[2](each start = 4, each fixed = true);
  Real regularizer[6];
  Real reference[6];
  Real lower[6];
equation
  der(position) = -position;
  der(velocity) = -velocity;
  der(frictionLoss) = -frictionLoss;
  der(inverseWeight) = -inverseWeight;
  (regularizer, reference, lower) = Rows(position, velocity, frictionLoss, inverseWeight);
end RowsModel;
";

#[test]
fn joint_row_outputs_read_only_the_inputs_their_own_assignments_read() {
    // Inputs: position, velocity, frictionLoss, inverseWeight.
    let site = owner_site(JOINT_ROWS, "RowsModel", 4, 3);
    let leaves = site
        .output_dependencies()
        .iter()
        .map(|output| {
            output
                .iter()
                .flat_map(rumoca_ir_solve::SolveCallDependency::inputs)
                .collect::<BTreeSet<_>>()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        leaves,
        vec![
            BTreeSet::from([0, 2, 3]),
            BTreeSet::from([0, 1, 2]),
            BTreeSet::from([2]),
        ],
        "regularizer, reference and lower"
    );
}

#[test]
fn joint_row_elements_keep_their_own_joint_and_pass() {
    // Input scalars: position 0..2, velocity 2..4, frictionLoss 4..6,
    // inverseWeight 6..8; output scalars: regularizer 0..6, reference 6..12,
    // lower 12..18.
    let outputs = output_scalar_inputs(owner_site(JOINT_ROWS, "RowsModel", 4, 3));
    assert_eq!(outputs.len(), 18);
    let set = |inputs: &[usize]| inputs.iter().copied().collect::<BTreeSet<_>>();
    // Friction rows: the joint's own friction and weight, not the position the
    // limit rows of an earlier pass read through `d`.
    assert_eq!(outputs[0], set(&[4, 6]));
    assert_eq!(outputs[1], set(&[5, 7]));
    assert_eq!(outputs[6], set(&[2, 4]));
    assert_eq!(outputs[7], set(&[3, 5]));
    // The friction bound reads the friction of its joint alone.
    assert_eq!(outputs[12], set(&[4]));
    assert_eq!(outputs[13], set(&[5]));
    // Limit rows of joint i, written by the inner loop at `n + 2 * (i - 1) +
    // side`: the joint's position and weight only.
    for (row, joint) in [(2, 0), (3, 0), (4, 1), (5, 1)] {
        assert_eq!(outputs[row], set(&[joint, 6 + joint]), "regularizer {row}");
        assert_eq!(
            outputs[6 + row],
            set(&[joint, 2 + joint]),
            "reference {row}"
        );
    }
    // Rows no assignment writes keep the initial constants.
    for row in 2..6 {
        assert!(outputs[12 + row].is_empty(), "lower {row}");
    }
}
