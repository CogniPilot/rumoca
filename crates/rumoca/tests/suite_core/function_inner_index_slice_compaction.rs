//! Regression for slice-compacting a perfect inner element loop.
//!
//! `for j in r loop A[i,j] := e; end for` and `A[i,r] := e[j := r]` write the
//! same coordinates only when every operator in `e` keeps its meaning once an
//! operand gains the sliced dimension. MLS §10.6.4 gives `*` on two arrays the
//! *scalar product*, so slicing both operands of a product turns the row of
//! per-element products the source wrote into a contraction; MLS §10.6.3 makes
//! `+` and `-` require equal shapes, so slicing exactly one operand makes the
//! two disagree.
//!
//! Both faults are reachable from ordinary covariance-rescaling source, and
//! they fail differently: the shape-visible ones were rejected as `ED020`
//! ("expression shape mismatch") against an assignment that is legal Modelica,
//! while `s[j] * a[j,j]` type-checked after compaction and silently evaluated
//! the vector-matrix product instead. Every shape below is therefore pinned to
//! the value the scalar loop defines, not merely to "it compiles", and the
//! shapes that must *keep* their compact slice write additionally pin the loop
//! nesting the function retains — otherwise refusing every compaction would
//! satisfy this file.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, eval_dae_at};

const MATRIX: &str = "{{1.0, 2.0, 3.0}, {2.0, 4.0, 5.0}, {3.0, 5.0, 6.0}}";
const SCALE: &str = "{0.5, 1.0, 2.0}";

/// Build one observer around a function body, so every shape below differs
/// only in the assignment under test.
fn model_source(name: &str, body: &str) -> String {
    format!(
        r#"
within;

function elementLoop
  input Real a[3, 3];
  input Real s[3];
  output Real y[3, 3];
algorithm
  for i in 1:3 loop
    for j in 1:3 loop
      {body}
    end for;
  end for;
end elementLoop;

model {name}
  Real scaled[3, 3];
  Real state[9](each start = 0.0, each fixed = true);
equation
  scaled = elementLoop({MATRIX}, {SCALE});
  der(state) = {{
    scaled[1, 1], scaled[1, 2], scaled[1, 3],
    scaled[2, 1], scaled[2, 2], scaled[2, 3],
    scaled[3, 1], scaled[3, 2], scaled[3, 3]}};
end {name};
"#
    )
}

/// The per-element values the source loop defines, and the loop nesting the
/// constructed function kept.
struct Lowered {
    elements: Vec<f64>,
    loop_depth: usize,
    statement_count: usize,
}

fn lower(name: &str, body: &str) -> Lowered {
    let source = model_source(name, body);
    let compiled = Compiler::new()
        .model(name)
        .compile_str(&source, &format!("{name}.mo"))
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
    let mut statement_count = 0;
    compiled.dae.inspect(|view| {
        let function = (0..view.function_count())
            .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
            .find(|function| function.name().as_str().ends_with("elementLoop"))
            .expect("the constructed element loop remains visible");
        loop_depth = max_loop_depth(function.statements());
        statement_count = count_statements(function.statements());
    });
    Lowered {
        elements,
        loop_depth,
        statement_count,
    }
}

fn count_statements(statements: dae::FunctionStatements<'_>) -> usize {
    statements
        .into_iter()
        .map(|statement| match statement {
            dae::FunctionStatementView::For { statements, .. } => 1 + count_statements(statements),
            _ => 1,
        })
        .sum()
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

/// Refusing the slice rewrite must not cost the function its compact owner.
///
/// The whole point of `compact_function_loops` is that a function loop never
/// falls through to scalar statement expansion, so a shape this fix stops
/// compacting has to keep some other tensor-native form — nine scalar writes
/// for a 3x3 would be a code-size regression that scales with the covariance
/// dimension of the flight model this defect was found in.
fn assert_compact_owner(lowered: &Lowered) {
    assert!(
        lowered.statement_count <= 3,
        "a refused slice rewrite must keep a compact owner, got {} statements at loop depth {}",
        lowered.statement_count,
        lowered.loop_depth
    );
}

fn assert_elements(actual: &[f64], expected: [f64; 9]) {
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

/// `y[i,j] := s[i] * a[i,j] * s[j]` is the natural symmetric rescaling, and the
/// exact statement flight estimator code had to rewrite as slice operations.
/// Both factors name the inner index, so compacting the loop would contract
/// them into a scalar product.
#[test]
fn a_product_of_two_inner_index_reads_is_accepted_and_stays_element_wise() {
    let lowered = lower(
        "ObserveSymmetricRescale",
        "y[i, j] := s[i] * a[i, j] * s[j];",
    );
    assert_elements(
        &lowered.elements,
        [0.25, 1.0, 3.0, 1.0, 4.0, 10.0, 3.0, 10.0, 24.0],
    );
    assert_compact_owner(&lowered);
}

/// `y[i,j] := s[j] * a[j,j]` names the inner index twice in one read. Slicing
/// both subscripts selects a whole submatrix rather than the diagonal element
/// the scalar loop walked, and the resulting vector-matrix product has exactly
/// the shape the target gained — so this shape compiled and returned the wrong
/// numbers rather than reporting anything. The wrong values were the product
/// `s * a`, `{8.5, 15.0, 18.5}` on every row.
#[test]
fn naming_the_inner_index_twice_in_one_read_keeps_the_diagonal_element() {
    let lowered = lower("ObserveDiagonalScale", "y[i, j] := s[j] * a[j, j];");
    assert_elements(
        &lowered.elements,
        [0.5, 4.0, 12.0, 0.5, 4.0, 12.0, 0.5, 4.0, 12.0],
    );
    assert_compact_owner(&lowered);
}

/// `y[i,j] := s[i] + a[i,j]` slices only the second operand of a same-shape
/// operator. Nothing here reads the inner index twice, which is why the fault
/// was the compaction rather than the subscript.
#[test]
fn a_same_shape_operator_over_one_inner_index_read_is_accepted() {
    let lowered = lower("ObserveOuterPlusElement", "y[i, j] := s[i] + a[i, j];");
    assert_elements(
        &lowered.elements,
        [1.5, 2.5, 3.5, 3.0, 5.0, 6.0, 5.0, 7.0, 8.0],
    );
    assert_compact_owner(&lowered);
}

/// `y[i,j] := a[i,j] / s[j]` slices the divisor, which MLS §10.6.4 admits only
/// as a scalar.
#[test]
fn dividing_by_an_inner_index_read_is_accepted() {
    let lowered = lower("ObserveInnerDivide", "y[i, j] := a[i, j] / s[j];");
    assert_elements(
        &lowered.elements,
        [2.0, 2.0, 1.5, 4.0, 4.0, 2.5, 6.0, 5.0, 3.0],
    );
    assert_compact_owner(&lowered);
}

/// The compaction this fix must NOT give up: only one operand of the product
/// gains the sliced dimension, so `s[i] * a[i,r]` is still the row of products
/// the source wrote, and the inner loop must still collapse into it.
#[test]
fn a_single_sliced_product_operand_still_compacts_to_the_same_values() {
    let lowered = lower("ObserveRowScale", "y[i, j] := s[i] * a[i, j];");
    assert_elements(
        &lowered.elements,
        [0.5, 1.0, 1.5, 2.0, 4.0, 5.0, 6.0, 10.0, 12.0],
    );
    assert_eq!(
        lowered.loop_depth, 1,
        "the inner element loop must still collapse into one slice write"
    );
}

/// The element-wise product is legal with both operands sliced, and it must
/// keep its compact form: MLS §10.6.5 broadcasts rather than contracting, so
/// nothing about `.*` changes when a dimension appears.
#[test]
fn an_element_wise_product_of_two_inner_index_reads_still_compacts() {
    let lowered = lower("ObserveElementWise", "y[i, j] := a[i, j] .* s[j];");
    assert_elements(
        &lowered.elements,
        [0.5, 2.0, 6.0, 1.0, 4.0, 10.0, 1.5, 5.0, 12.0],
    );
    assert_eq!(
        lowered.loop_depth, 1,
        "an element-wise product must keep its compact slice write"
    );
}
