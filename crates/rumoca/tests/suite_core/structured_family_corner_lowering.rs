//! Regression: a structured `for`-equation family may only be lowered from its
//! CORNER cells when the compiler has PROVEN the family uniform over its whole
//! domain.
//!
//! The compact direct-derivative path builds one tensor node from the base cell
//! plus one unit neighbor per binder and lifts it over every point of the
//! domain. Applied to a family whose interior cells carry a different body, that
//! silently replaces the interior with the corner kernel — the DAE still prints
//! `der(x[1:4]) = {1, 1, 100, 1}` while the evaluated derivative comes back
//! `{1, 1, 1, 1}`. Wrong numbers, no diagnostic.
//!
//! Both models below are non-regular (their per-cell bodies genuinely diverge),
//! so they must fall through to the row-based lowering, which reads every row.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

/// One interior cell disagrees with every other cell. The disagreement is at
/// `i == 3`, which is NOT a corner of `1:4` (the corners are `i == 1` and
/// `i == 2`), so corner-only lowering cannot see it.
const NON_UNIFORM_INTERIOR: &str = r#"
model NonUniformFamily
  Real x[4](each start = 0.0, each fixed = true);
equation
  for i in 1:4 loop
    if i == 3 then
      der(x[i]) = 100;
    else
      der(x[i]) = 1;
    end if;
  end for;
end NonUniformFamily;
"#;

/// Alternating bodies: the two corners differ by `-10`, so corner extrapolation
/// produces an error that GROWS with the array length rather than staying
/// bounded.
const ALTERNATING_BODIES: &str = r#"
model AlternatingFamily
  Real x[6](each start = 0.0, each fixed = true);
equation
  for i in 1:6 loop
    if mod(i, 2) == 0 then
      der(x[i]) = 10;
    else
      der(x[i]) = 20;
    end if;
  end for;
end AlternatingFamily;
"#;

/// A genuinely regular family, kept alongside the two above so a fix that simply
/// disables compact lowering everywhere is still visibly wrong: this one must
/// keep evaluating correctly.
const REGULAR_FAMILY: &str = r#"
model RegularFamily
  Real x[5](each start = 1.0, each fixed = true);
equation
  for i in 1:5 loop
    der(x[i]) = -2*x[i];
  end for;
end RegularFamily;
"#;

fn evaluated_derivatives(source: &str, model: &str, file: &str) -> Vec<f64> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .unwrap_or_else(|error| panic!("`{model}` should compile to a DAE: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("`{model}` should lower and evaluate: {error}"));
    assert!(
        probe.report.error.is_none(),
        "`{model}` eval error: {:?}",
        probe.report.error
    );
    probe
        .report
        .derivatives
        .iter()
        .map(|slot| slot.value)
        .collect()
}

#[test]
fn non_uniform_interior_cell_is_not_replaced_by_the_corner_body() {
    let derivatives = evaluated_derivatives(
        NON_UNIFORM_INTERIOR,
        "NonUniformFamily",
        "NonUniformFamily.mo",
    );
    assert_eq!(
        derivatives,
        vec![1.0, 1.0, 100.0, 1.0],
        "der(x[3]) is 100 in the DAE (`der(x[1:4]) = {{1, 1, 100, 1}}`); corner-only \
         lowering must not overwrite it with the corner body"
    );
}

#[test]
fn alternating_family_bodies_are_not_linearly_extrapolated() {
    let derivatives = evaluated_derivatives(
        ALTERNATING_BODIES,
        "AlternatingFamily",
        "AlternatingFamily.mo",
    );
    assert_eq!(
        derivatives,
        vec![20.0, 10.0, 20.0, 10.0, 20.0, 10.0],
        "extrapolating the -10 corner delta over the domain makes the error grow \
         without bound with the array size"
    );
}

#[test]
fn regular_family_still_evaluates_correctly() {
    let derivatives = evaluated_derivatives(REGULAR_FAMILY, "RegularFamily", "RegularFamily.mo");
    assert_eq!(derivatives, vec![-2.0; 5]);
}

#[test]
fn partial_state_range_with_neighbor_reads_is_a_native_stencil() {
    let source = r#"
model CascadedFirstOrder
  constant Integer N = 8;
  Real x[N](each start = 1.0);
equation
  der(x[1]) = 1.0 - x[1];
  for i in 2:N loop
    der(x[i]) = x[i - 1] - x[i];
  end for;
end CascadedFirstOrder;
"#;
    let compiled = Compiler::new()
        .model("CascadedFirstOrder")
        .compile_str(source, "CascadedFirstOrder.mo")
        .expect("the compact partial derivative family should compile");
    let solve = rumoca_sim::lower_solve_problem(&compiled.dae)
        .expect("the partial derivative family should lower to Solve IR");
    let counts = solve.compute_node_counts();
    assert_eq!(
        counts.affine_stencil, 1,
        "the i=2:N neighbor relation must remain one native AffineStencil"
    );
}

#[test]
fn structured_family_binder_is_a_scalar_function_argument() {
    let source = r#"
function selectElement
  input Real values[:];
  input Integer index;
  output Real selected;
algorithm
  selected := values[index];
end selectElement;

model StructuredCall
  Real y[3];
equation
  for i in 1:3 loop
    y[i] = selectElement({10.0, 20.0, 30.0}, i);
  end for;
end StructuredCall;
"#;
    let compiled = Compiler::new()
        .model("StructuredCall")
        .compile_str(source, "StructuredCall.mo")
        .expect("a compact domain binder carries scalar shape evidence into call selection");
    compiled.dae.inspect(|view| {
        assert_eq!(view.continuous_family_count(), 1);
        let family = view
            .continuous_family(0)
            .expect("the source for-equation remains one tensor-native owner");
        assert_eq!(
            view.domain(family.domain())
                .expect("the family domain resolves")
                .scalar_count(),
            3
        );
    });
}

#[test]
fn mixed_slice_and_scalar_loop_has_one_authoritative_family() {
    let source = r#"
model MixedFamily
  Real source[3];
  Real matrix[3, 2];
  Real scalar[2];
equation
  source = {1.0, 2.0, 3.0};
  for i in 1:2 loop
    matrix[:, i] = source;
    scalar[i] = i;
  end for;
end MixedFamily;
"#;
    let compiled = Compiler::new()
        .model("MixedFamily")
        .compile_str(source, "MixedFamily.mo")
        .expect("the source loop supersedes child array views instead of overlapping them");
    compiled.dae.inspect(|view| {
        let mut row_counts = view
            .continuous_owners()
            .filter_map(|owner| match owner {
                rumoca_ir_dae::ContinuousOwnerView::Structured { family, .. } => {
                    Some(family.scalar_rows())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        row_counts.sort_unstable();
        assert_eq!(row_counts, vec![2, 3, 6]);
    });
}

/// A clamped 2-D grid whose interior loop body holds two equations: the
/// method-of-lines shape of the user guide's PDE examples. The interior
/// derivative slots of `w` are a strided sub-range of the array (rows
/// `2..N-1` of every column `2..N-1`), interleaved with the clamped edges.
const GRID_TWO_BODY_INTERIOR: &str = r#"
model GridTwoBodyInterior
  parameter Integer N = 5;
  Real u[N, N](each start = 1.0, each fixed = true);
  Real w[N, N](each start = 0.0, each fixed = true);
equation
  for i in 1:N loop
    der(u[i, 1]) = 0.0;
    der(u[i, N]) = 0.0;
    der(w[i, 1]) = 0.0;
    der(w[i, N]) = 0.0;
  end for;
  for j in 2:N - 1 loop
    der(u[1, j]) = 0.0;
    der(u[N, j]) = 0.0;
    der(w[1, j]) = 0.0;
    der(w[N, j]) = 0.0;
  end for;
  for i in 2:N - 1 loop
    for j in 2:N - 1 loop
      der(u[i, j]) = w[i, j];
      der(w[i, j]) = u[i + 1, j] + u[i - 1, j] + u[i, j + 1] + u[i, j - 1]
        - 4.0 * u[i, j] + 10.0 * i + j;
    end for;
  end for;
end GridTwoBodyInterior;
"#;

/// Each body of a nested loop keeps one family over the whole `(i, j)` domain;
/// splitting the inner bodies must not unroll the outer binder into one family
/// per row.
#[test]
fn nested_loop_bodies_each_keep_one_grid_family() {
    let compiled = Compiler::new()
        .model("GridTwoBodyInterior")
        .compile_str(GRID_TWO_BODY_INTERIOR, "GridTwoBodyInterior.mo")
        .expect("the grid model should compile");
    compiled.dae.inspect(|view| {
        let interior = view
            .continuous_owners()
            .filter_map(|owner| match owner {
                rumoca_ir_dae::ContinuousOwnerView::Structured { family, .. } => {
                    let domain = view
                        .domain(family.domain())
                        .expect("family domain resolves");
                    (domain.structured().binders.len() == 2).then_some(family.scalar_rows())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            interior,
            vec![9, 9],
            "the two interior bodies must stay two 3x3 families, not one family per row"
        );
    });
}

/// The interior derivative families write strided state slots; each lowers to
/// one rank-2 tensor node whose output map places every value in its own slot,
/// with no scalar rows left for the interior.
#[test]
fn strided_grid_interior_lowers_to_rank_two_tensor_nodes() {
    let compiled = Compiler::new()
        .model("GridTwoBodyInterior")
        .compile_str(GRID_TWO_BODY_INTERIOR, "GridTwoBodyInterior.mo")
        .expect("the grid model should compile");
    let solve = rumoca_sim::lower_solve_problem(&compiled.dae)
        .expect("the grid model should lower to Solve IR");
    let mut rank_two_rows = Vec::new();
    let mut scalar_rows = 0usize;
    for node in &solve.continuous.derivative_rhs.nodes {
        match node {
            rumoca_ir_solve::ComputeNode::Map { domain, .. }
            | rumoca_ir_solve::ComputeNode::AffineStencil { domain, .. }
                if domain.binders.len() == 2 =>
            {
                rank_two_rows.push(domain.scalar_count().expect("domain size"));
            }
            rumoca_ir_solve::ComputeNode::ScalarPrograms(block) => {
                scalar_rows += block.programs().len();
            }
            _ => {}
        }
    }
    assert_eq!(rank_two_rows, vec![9, 9]);
    assert!(
        scalar_rows < 16,
        "only edge rows may stay scalar, found {scalar_rows} scalar derivative rows"
    );

    // u = 1 everywhere, so der(w[i, j]) = 10 i + j on the interior and 0 on
    // the edges; a misplaced strided output map moves these values.
    let derivatives = evaluated_derivatives(
        GRID_TWO_BODY_INTERIOR,
        "GridTwoBodyInterior",
        "GridTwoBodyInterior.mo",
    );
    let mut expected = vec![0.0; 50];
    for i in 2..=4 {
        for j in 2..=4 {
            expected[25 + (i - 1) * 5 + (j - 1)] = (10 * i + j) as f64;
        }
    }
    assert_eq!(derivatives, expected);
}
