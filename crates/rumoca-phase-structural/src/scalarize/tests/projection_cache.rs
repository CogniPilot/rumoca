//! Regression tests for the memoized index projection.
//!
//! `project_at` caches on the *address* of the operand subtree plus the index.
//! These tests pin the two properties that keeps honest: a nested matrix product
//! still expands to the mathematically correct sum of products, and every output
//! element gets its own expansion (a stale cache entry would make two elements
//! share one).

use super::*;

/// Flattens a projected expression into its `name[i,j]` leaves, left to right.
fn leaves(expr: &Expression) -> Vec<String> {
    let mut collected = Vec::new();
    collect_leaves(expr, &mut collected);
    collected
}

fn collect_leaves(expr: &Expression, out: &mut Vec<String>) {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let indices = subscripts
                .iter()
                .map(|subscript| match subscript {
                    Subscript::Index { value, .. } => value.to_string(),
                    _ => "?".to_string(),
                })
                .collect::<Vec<_>>()
                .join(",");
            out.push(format!("{}[{}]", name.as_str(), indices));
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_leaves(lhs, out);
            collect_leaves(rhs, out);
        }
        Expression::Unary { rhs, .. } => collect_leaves(rhs, out),
        _ => out.push("<other>".to_string()),
    }
}

fn matrix_dae() -> dae::Dae {
    let mut model = dae::Dae::default();
    for name in ["a", "b", "c"] {
        model
            .variables
            .algebraics
            .insert(VarName::new(name), variable(name, &[2, 2]));
    }
    model
}

fn matrix_product() -> Expression {
    let span = test_span();
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(var("a")),
            rhs: Box::new(var("b")),
            span,
        }),
        rhs: Box::new(var("c")),
        span,
    }
}

#[test]
fn nested_matrix_product_expands_to_the_full_sum_of_products() {
    let model = matrix_dae();
    let context = build_expression_scalarization_context(&model).expect("scalarization context");
    let rows = scalarize_expression_rows(&matrix_product(), 4, &context).expect("projected rows");

    assert_eq!(rows.len(), 4);
    // Element (1,1) of (a*b)*c is sum_k (sum_j a[1,j]*b[j,k]) * c[k,1].
    assert_eq!(
        leaves(&rows[0]),
        vec![
            "a[1,1]", "b[1,1]", "a[1,2]", "b[2,1]", "c[1,1]", "a[1,1]", "b[1,2]", "a[1,2]",
            "b[2,2]", "c[2,1]",
        ]
    );
}

#[test]
fn every_output_element_gets_its_own_expansion() {
    let model = matrix_dae();
    let context = build_expression_scalarization_context(&model).expect("scalarization context");
    let expr = matrix_product();
    let rows = scalarize_expression_rows(&expr, 4, &context).expect("projected rows");

    for (left, right) in (0..rows.len()).flat_map(|i| (i + 1..rows.len()).map(move |j| (i, j))) {
        assert_ne!(
            rows[left], rows[right],
            "elements {left} and {right} share an expansion"
        );
    }
}

#[test]
fn repeated_projection_of_the_same_tree_is_stable() {
    let model = matrix_dae();
    let context = build_expression_scalarization_context(&model).expect("scalarization context");
    let expr = matrix_product();

    let first = scalarize_expression_rows(&expr, 4, &context).expect("projected rows");
    let second = scalarize_expression_rows(&expr, 4, &context).expect("projected rows");
    assert_eq!(first, second);
}
