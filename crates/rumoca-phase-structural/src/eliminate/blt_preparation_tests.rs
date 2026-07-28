//! Regression tests for BLT preparation on array-shaped systems.
//!
//! `prepare_blt_elimination` sorts an array-shaped system only to surface a
//! structural singularity: the caller eliminates through scalar blocks only and
//! discards the decomposition. So it does not hand those blocks back at all —
//! `blocks` is `None` whenever the sort ran on a scalarized view of an
//! array-shaped system.
//!
//! Two things need pinning, and they need different fixtures.
//!
//! * The whole-pipeline behaviour through `eliminate_trivial`: the reported
//!   `blt_error` and the boundary-phase `n_eliminated`. Note that the boundary
//!   phase runs first and usually consumes the array rows, so by the time BLT
//!   preparation runs those systems are *scalar*-shaped —
//!   `array_shaped_system_reports_no_blt_error` and
//!   `singular_array_shaped_system_still_reports_blt_error` exercise the
//!   `uses_scalar_view == false` branch despite their array-shaped inputs.
//! * The `uses_scalar_view == true` branch itself, which no pipeline-level
//!   fixture reaches by accident. Those tests call `prepare_blt_elimination`
//!   directly with rows that are still array-shaped, and pin both halves of
//!   the decision: no blocks are handed back, and profiling still sees the
//!   size of the decomposition the sort actually computed.

use super::*;
use rumoca_core::Span;
use rumoca_ir_dae::Equation;

fn test_span(offset: usize) -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("eliminate_blt_preparation_test.mo"),
        offset,
        offset + 1,
    )
}

fn test_variable(name: &str, offset: usize, dims: Vec<i64>) -> dae::Variable {
    let mut variable = dae::Variable::new(VarName::new(name), test_span(offset));
    variable.source_span = test_span(offset);
    variable.dims = dims;
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: vec![],
        span: test_span(5),
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![var(name)],
        span: test_span(7),
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: test_span(9),
    }
}

fn array_literal(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: test_span(11),
    }
}

fn residual(rhs: Expression, scalar_count: usize) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(13),
        origin: "blt preparation test".to_string(),
        scalar_count,
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(15),
    }
}

/// One scalar ODE plus a two-element array algebraic defined by a single array
/// row, so `prepare_blt_elimination` takes the `scalar_count != 1` path.
fn well_posed_array_dae() -> Dae {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x", 21, Vec::new()));
    dae.variables
        .algebraics
        .insert(VarName::new("y"), test_variable("y", 23, vec![2]));
    dae.continuous
        .equations
        .push(residual(sub(der("x"), real(1.0)), 1));
    dae.continuous.equations.push(residual(
        sub(var("y"), array_literal(vec![var("x"), real(2.0)])),
        2,
    ));
    dae
}

#[test]
fn array_shaped_system_reports_no_blt_error() {
    let mut dae = well_posed_array_dae();

    let result = eliminate_trivial(&mut dae).expect("array-shaped elimination should succeed");

    assert!(
        result.blt_error.is_none(),
        "a well-posed array system must not report a BLT structural error"
    );
    assert_eq!(
        result.n_eliminated, 1,
        "the boundary phase still eliminates the array row it owns"
    );
    assert!(
        result
            .substitutions
            .iter()
            .any(|substitution| substitution.var_name.as_str().starts_with('y')),
        "the array algebraic should be substituted by the boundary phase"
    );
}

#[test]
fn singular_array_shaped_system_still_reports_blt_error() {
    let mut dae = well_posed_array_dae();
    // Two scalar algebraics sharing a single row leave the reduced system one
    // equation short. The sort still runs for array-shaped systems precisely so
    // this is reported, even though its block decomposition is discarded.
    for (name, offset) in [("w", 31usize), ("v", 33)] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name, offset, Vec::new()));
    }
    dae.continuous.equations.push(residual(
        Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("w")),
            rhs: Box::new(var("v")),
            span: test_span(35),
        },
        1,
    ));

    let result = eliminate_trivial(&mut dae).expect("elimination itself should not fail");

    assert!(
        result.blt_error.is_some(),
        "a singular array-shaped system must still surface its BLT structural error"
    );
}

/// A system whose continuous rows are *still* array-shaped when BLT
/// preparation runs, so `prepare_blt_elimination` takes the
/// `uses_scalar_view == true` branch: a two-element state vector with one array
/// ODE row, plus a two-element algebraic vector defined by one array row.
fn array_view_blt_dae() -> Dae {
    let mut dae = Dae::new();
    dae.variables
        .states
        .insert(VarName::new("x"), test_variable("x", 51, vec![2]));
    dae.variables
        .algebraics
        .insert(VarName::new("y"), test_variable("y", 53, vec![2]));
    dae.continuous.equations.push(residual(
        sub(der("x"), array_literal(vec![real(1.0), real(2.0)])),
        2,
    ));
    dae.continuous
        .equations
        .push(residual(sub(var("y"), var("x")), 2));
    dae
}

/// The branch the caller depends on: an array-shaped system is sorted, but its
/// blocks are never handed back, because they index a scalarized view the
/// caller does not have. Eliminating through them would address rows and
/// unknowns that do not exist in the DAE the caller is about to mutate.
#[test]
fn array_shaped_preparation_withholds_its_blocks() {
    let dae = array_view_blt_dae();

    let prepared = prepare_blt_elimination(&dae, false, false)
        .expect("sorting an array-shaped system should succeed");

    assert!(
        prepared.blocks.is_none(),
        "blocks sorted from a scalarized view must not be handed to the caller"
    );
    assert!(
        prepared.error.is_none(),
        "a well-posed array-shaped system is not singular"
    );
}

/// Withholding the blocks must not also withhold their profile. `sort_dae` pays
/// the same cost either way, so the block count profiling reports is the count
/// the sort produced — reporting `blocks.len()` would report `0` for every
/// array-shaped system and hide exactly the sorts the profile exists to expose.
#[test]
fn array_shaped_preparation_still_reports_its_sorted_block_count() {
    let array_view = array_view_blt_dae();
    let mut scalar_view = array_view_blt_dae();
    crate::scalarize::scalarize_equations(&mut scalar_view)
        .expect("the fixture scalarizes into four scalar rows");

    let from_array_view = prepare_blt_elimination(&array_view, false, false)
        .expect("sorting an array-shaped system should succeed");
    let from_scalar_view = prepare_blt_elimination(&scalar_view, false, false)
        .expect("sorting the equivalent scalar system should succeed");

    assert_eq!(
        from_array_view.sorted_block_count, 4,
        "the sort decomposed the scalarized view into one block per scalar row"
    );
    assert_eq!(
        from_array_view.sorted_block_count, from_scalar_view.sorted_block_count,
        "the same system must profile the same block count whether or not the \
         caller is handed the blocks"
    );
    assert_eq!(
        from_scalar_view.blocks.map(|blocks| blocks.len()),
        Some(4),
        "an already-scalar system is handed its blocks, which is the only \
         difference between the two branches"
    );
}

/// The sort still runs for array-shaped systems, and that is the whole reason
/// it runs: a singular one must surface its structural error even though its
/// blocks are discarded.
#[test]
fn singular_array_shaped_preparation_reports_its_error() {
    let mut dae = array_view_blt_dae();
    for (name, offset) in [("w", 61usize), ("v", 63)] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), test_variable(name, offset, Vec::new()));
    }
    dae.continuous.equations.push(residual(
        Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("w")),
            rhs: Box::new(var("v")),
            span: test_span(65),
        },
        1,
    ));

    let prepared =
        prepare_blt_elimination(&dae, false, false).expect("preparation itself should not fail");

    assert!(
        prepared.error.is_some(),
        "a singular array-shaped system must surface its structural error"
    );
    assert!(prepared.blocks.is_none(), "a failed sort has no blocks");
    assert_eq!(
        prepared.sorted_block_count, 0,
        "a failed sort produced no decomposition to profile"
    );
}

/// End to end: an array-shaped row that survives the boundary phase must not be
/// eliminated through BLT blocks sorted from a scalarized view.
#[test]
fn array_shaped_system_is_not_eliminated_through_blt_blocks() {
    let mut dae = array_view_blt_dae();

    let result = eliminate_trivial(&mut dae).expect("array-shaped elimination should succeed");

    assert!(result.blt_error.is_none());
    assert_eq!(
        result.n_eliminated, 1,
        "only the boundary phase's array alias row is eliminated; the BLT phase \
         is skipped for an array-shaped view"
    );
    assert!(
        result
            .substitutions
            .iter()
            .all(|substitution| substitution.var_name.as_str().starts_with('y')),
        "no state-derivative row may be substituted by a BLT phase that never ran"
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|equation| equation.scalar_count == 2),
        "the array ODE row must survive as an array row"
    );
}
