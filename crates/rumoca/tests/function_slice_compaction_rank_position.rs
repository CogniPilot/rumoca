//! Regression for the *rank position* of a slice-compacted inner element loop.
//!
//! `for j in r loop A[..,j,..] := e; end for` compacts to `A[..,r,..] := e[j := r]`
//! only when every operand of `e` that gains the sliced dimension gains it in
//! the position the scalar loop stacked it: leading, and as the operand's only
//! new dimension. `function_inner_index_slice_compaction.rs` pins the operator
//! half of that claim — which operators keep their meaning once *some* operand
//! gains a dimension. This file pins the half that operator identity alone
//! cannot see: an expression whose operands gain the dimension in the *wrong
//! place* type-checks with exactly the shape the target gained and evaluates
//! the wrong elements.
//!
//! Both shapes below are ordinary numeric-code shapes and both were silently
//! wrong: `{f(a[i,j]), g(a[i,j])}` — an MLS §10.4 array construction whose
//! elements each gain the sliced dimension — builds the *transpose* of the
//! matrix the loop wrote, and `a[i,:] * b[j,:]` — an MLS §10.6.4 scalar product
//! of two vectors — becomes a vector-matrix product once `b[j,:]` is raised
//! from a vector to a matrix.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, eval_dae_at};

/// The per-element values the source loop defines, and the loop nesting the
/// constructed function kept.
struct Lowered {
    elements: Vec<f64>,
    loop_depth: usize,
}

/// Compile one observer model and read back the values its function computed.
///
/// The derivative slots are read in declaration order, so the caller pins the
/// exact element order the scalar loop defines rather than a set of values a
/// transposition would also satisfy.
fn lower(name: &str, source: &str) -> Lowered {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .expect("a perfect inner element loop must compile");
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("a perfect inner element loop must evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    let elements = probe
        .report
        .derivatives
        .iter()
        .map(|slot| slot.value)
        .collect();
    let mut loop_depth = 0;
    compiled.dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str().ends_with("elementLoop"))
            .expect("the constructed element loop remains visible");
        loop_depth = max_loop_depth(function.statements());
    });
    Lowered {
        elements,
        loop_depth,
    }
}

fn max_loop_depth(statements: dae::FunctionStatements<'_>) -> usize {
    statements
        .into_iter()
        .map(|statement| match statement {
            dae::FunctionStatementView::For { statements, .. } => 1 + max_loop_depth(statements),
            _ => 0,
        })
        .max()
        .unwrap_or(0)
}

fn assert_elements(actual: &[f64], expected: &[f64]) {
    assert_eq!(
        actual.len(),
        expected.len(),
        "expected one derivative per element, got {actual:?}"
    );
    for (index, (actual_value, expected_value)) in actual.iter().zip(expected).enumerate() {
        assert!(
            (actual_value - expected_value).abs() < 1.0e-12,
            "element {index}: expected {expected_value}, got {actual_value}; all values: {actual:?}"
        );
    }
}

/// An MLS §10.4 array construction stacks its elements along a *new leading*
/// dimension, so elements that each gained the sliced dimension build the
/// transpose of the matrix the scalar loop wrote.
///
/// `y[i,j,:] := {a[i,j], 10*a[i,j]}` writes, for each `j`, the two-element
/// vector `{a[i,j], 10*a[i,j]}`. Compacting `j` yields
/// `y[i,r,:] := {a[i,r], 10*a[i,r]}`, whose value is the 2x2 matrix
/// `{{a[i,1], a[i,2]}, {10*a[i,1], 10*a[i,2]}}` — the transpose of the
/// `{{a[i,1], 10*a[i,1]}, {a[i,2], 10*a[i,2]}}` the source defines. The two
/// have the same shape, so nothing was reported: for `a = {{1,2},{3,4}}` the
/// function returned `{1, 2, 10, 20, 3, 4, 30, 40}` where the loop defines
/// `{1, 10, 2, 20, 3, 30, 4, 40}`.
#[test]
fn an_array_construction_over_the_inner_index_keeps_the_source_element_order() {
    let lowered = lower(
        "ObserveStackedConstructor",
        r#"
within;

function elementLoop
  input Real a[2, 2];
  output Real y[2, 2, 2];
algorithm
  for i in 1:2 loop
    for j in 1:2 loop
      y[i, j, :] := {a[i, j], 10.0 * a[i, j]};
    end for;
  end for;
end elementLoop;

model ObserveStackedConstructor
  Real stacked[2, 2, 2];
  Real state[8](each start = 0.0, each fixed = true);
equation
  stacked = elementLoop({{1.0, 2.0}, {3.0, 4.0}});
  der(state) = {
    stacked[1, 1, 1], stacked[1, 1, 2], stacked[1, 2, 1], stacked[1, 2, 2],
    stacked[2, 1, 1], stacked[2, 1, 2], stacked[2, 2, 1], stacked[2, 2, 2]};
end ObserveStackedConstructor;
"#,
    );
    assert_elements(
        &lowered.elements,
        &[1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0],
    );
}

/// MLS §10.6.4 reads the *ranks* of `*`'s operands, so raising one operand from
/// a vector to a matrix changes which product the expression denotes.
///
/// `y[i,j] := a[i,:] * b[j,:]` is the scalar product of two rows — the ordinary
/// spelling of `a * transpose(b)`. Compacting `j` yields `a[i,:] * b[r,:]`,
/// where `b[r,:]` is a whole matrix and MLS §10.6.4 makes the expression the
/// vector-matrix product that contracts `b`'s *first* dimension: the function
/// returned `a * b` instead. Both are 3x3, so nothing was reported.
#[test]
fn a_scalar_product_of_two_rows_is_not_raised_into_a_vector_matrix_product() {
    let lowered = lower(
        "ObserveRowDotProduct",
        r#"
within;

function elementLoop
  input Real a[3, 3];
  input Real b[3, 3];
  output Real y[3, 3];
algorithm
  for i in 1:3 loop
    for j in 1:3 loop
      y[i, j] := a[i, :] * b[j, :];
    end for;
  end for;
end elementLoop;

model ObserveRowDotProduct
  Real dots[3, 3];
  Real state[9](each start = 0.0, each fixed = true);
equation
  dots = elementLoop(
    {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {7.0, 8.0, 9.0}},
    {{1.0, 0.0, 2.0}, {3.0, 1.0, 0.0}, {0.0, 4.0, 1.0}});
  der(state) = {
    dots[1, 1], dots[1, 2], dots[1, 3],
    dots[2, 1], dots[2, 2], dots[2, 3],
    dots[3, 1], dots[3, 2], dots[3, 3]};
end ObserveRowDotProduct;
"#,
    );
    // `a * transpose(b)`: row i of a dotted with row j of b.
    assert_elements(
        &lowered.elements,
        &[7.0, 5.0, 11.0, 16.0, 17.0, 26.0, 25.0, 29.0, 41.0],
    );
}

/// The flight-model witness for the shape above, with its explicit range
/// subscript and its own dimensions.
///
/// `NavigationEstimator`'s conjugate reset writes
/// `conjugated[i,j] := rotated[i, 1:n] * rotationJacobian[j, :]` — the ordinary
/// spelling of `rotated * transpose(rotationJacobian)`. Compacting `j` emitted
/// the contraction `rotated[i,k] * rotationJacobian[k,col]`, so the estimator
/// ran `rotated * rotationJacobian` with no diagnostic. The left factor is
/// written as an explicit `1:n` range rather than `:` because that is what the
/// model writes, and because the two reach the compaction as different
/// subscript forms.
#[test]
fn the_conjugate_reset_shape_keeps_its_transposed_contraction() {
    let lowered = lower(
        "ObserveConjugateReset",
        r#"
within;

function elementLoop
  input Real rotated[3, 3];
  input Real rotationJacobian[3, 3];
  output Real conjugated[3, 3];
algorithm
  for i in 1:3 loop
    for j in 1:3 loop
      conjugated[i, j] := rotated[i, 1:3] * rotationJacobian[j, :];
    end for;
  end for;
end elementLoop;

model ObserveConjugateReset
  Real conjugated[3, 3];
  Real state[9](each start = 0.0, each fixed = true);
equation
  conjugated = elementLoop(
    {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {7.0, 8.0, 9.0}},
    {{1.0, 0.0, 2.0}, {3.0, 1.0, 0.0}, {0.0, 4.0, 1.0}});
  der(state) = {
    conjugated[1, 1], conjugated[1, 2], conjugated[1, 3],
    conjugated[2, 1], conjugated[2, 2], conjugated[2, 3],
    conjugated[3, 1], conjugated[3, 2], conjugated[3, 3]};
end ObserveConjugateReset;
"#,
    );
    assert_elements(
        &lowered.elements,
        &[7.0, 5.0, 11.0, 16.0, 17.0, 26.0, 25.0, 29.0, 41.0],
    );
}

/// The compaction this fix must not give up.
///
/// `y[i,j] := s[i] * a[i,j]` gains the sliced dimension in exactly one operand,
/// as that operand's only dimension, against a scalar factor — the shape the
/// guard exists to keep compact. It must still collapse into one slice write.
#[test]
fn a_scalar_factor_against_one_sliced_operand_still_compacts() {
    let lowered = lower(
        "ObserveRowScaleStillCompacts",
        r#"
within;

function elementLoop
  input Real a[3, 3];
  input Real s[3];
  output Real y[3, 3];
algorithm
  for i in 1:3 loop
    for j in 1:3 loop
      y[i, j] := s[i] * a[i, j];
    end for;
  end for;
end elementLoop;

model ObserveRowScaleStillCompacts
  Real scaled[3, 3];
  Real state[9](each start = 0.0, each fixed = true);
equation
  scaled = elementLoop(
    {{1.0, 2.0, 3.0}, {2.0, 4.0, 5.0}, {3.0, 5.0, 6.0}}, {0.5, 1.0, 2.0});
  der(state) = {
    scaled[1, 1], scaled[1, 2], scaled[1, 3],
    scaled[2, 1], scaled[2, 2], scaled[2, 3],
    scaled[3, 1], scaled[3, 2], scaled[3, 3]};
end ObserveRowScaleStillCompacts;
"#,
    );
    assert_elements(
        &lowered.elements,
        &[0.5, 1.0, 1.5, 2.0, 4.0, 5.0, 6.0, 10.0, 12.0],
    );
    assert_eq!(
        lowered.loop_depth, 1,
        "the inner element loop must still collapse into one slice write"
    );
}

/// A matrix-times-vector whose sliced operand is the *left* factor keeps its
/// meaning, and must keep its compact form.
///
/// `y[i,j] := a[j,:] * v` is, per iteration, a scalar product of two vectors.
/// Compacting `j` gives `a[r,:] * v`, where MLS §10.6.4 makes the matrix-vector
/// product contract `a`'s *second* dimension — leaving the sliced dimension as
/// the result's only dimension, which is exactly the stack the loop wrote. This
/// is the mirror image of the vector-matrix shape above, and refusing it would
/// be an over-tightening rather than a fix.
#[test]
fn a_matrix_vector_product_over_the_sliced_left_factor_still_compacts() {
    let lowered = lower(
        "ObserveRowsTimesVector",
        r#"
within;

function elementLoop
  input Real a[3, 3];
  input Real v[3];
  output Real y[3, 3];
algorithm
  for i in 1:3 loop
    for j in 1:3 loop
      y[i, j] := a[j, :] * v;
    end for;
  end for;
end elementLoop;

model ObserveRowsTimesVector
  Real rows[3, 3];
  Real state[9](each start = 0.0, each fixed = true);
equation
  rows = elementLoop(
    {{1.0, 2.0, 3.0}, {4.0, 5.0, 6.0}, {7.0, 8.0, 9.0}}, {1.0, 0.0, 2.0});
  der(state) = {
    rows[1, 1], rows[1, 2], rows[1, 3],
    rows[2, 1], rows[2, 2], rows[2, 3],
    rows[3, 1], rows[3, 2], rows[3, 3]};
end ObserveRowsTimesVector;
"#,
    );
    // Row j of a dotted with v, repeated for every i.
    assert_elements(
        &lowered.elements,
        &[7.0, 16.0, 25.0, 7.0, 16.0, 25.0, 7.0, 16.0, 25.0],
    );
    assert_eq!(
        lowered.loop_depth, 1,
        "a matrix-vector product over the sliced left factor must stay compact"
    );
}
