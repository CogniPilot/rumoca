//! Compile-time `if` branches must not contribute clock sources (MLS §16.5).
//!
//! `Modelica.Clocked.ClockSignals.Clocks.PeriodicExactClock` assigns its clock
//! from an `if` whose condition is a fixed parameter comparison. Offering the
//! dead branch's right-hand side as a clock source gave the partition a period
//! `resolutionFactor` times too long, so it never activated.

use super::*;

fn span(start: usize, end: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), start, end)
}

fn var(name: &str, offset: usize) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: span(offset, offset + name.len()),
    }
}

fn literal(value: f64, offset: usize) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: span(offset, offset + 1),
    }
}

fn binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: span(0, 1),
    }
}

/// `if resolution < 5 then c - slow else c - fast`, with `resolution = 6`.
fn periodic_exact_clock_residual(condition: rumoca_core::Expression) -> rumoca_core::Expression {
    let taken = binary(rumoca_core::OpBinary::Sub, var("c", 10), var("fast", 20));
    let dead = binary(rumoca_core::OpBinary::Sub, var("c", 30), var("slow", 40));
    rumoca_core::Expression::If {
        branches: vec![(condition, dead)],
        else_branch: Box::new(taken),
        span: span(0, 1),
    }
}

fn resolution_constants() -> HashMap<String, f64> {
    HashMap::from([("resolution".to_string(), 6.0)])
}

fn source_names(
    residual: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
) -> Vec<String> {
    let mut out = Vec::new();
    collect_assignments_from_residual(residual, constants, &mut out);
    out.into_iter()
        .filter(|(target, _)| target == "c")
        .filter_map(|(_, expr)| match expr {
            rumoca_core::Expression::VarRef { name, .. } => Some(name.as_str().to_string()),
            _ => None,
        })
        .collect()
}

#[test]
fn statically_false_branch_is_not_a_clock_source() {
    let condition = binary(
        rumoca_core::OpBinary::Lt,
        var("resolution", 0),
        literal(5.0, 50),
    );
    let residual = periodic_exact_clock_residual(condition);

    assert_eq!(
        source_names(&residual, &resolution_constants()),
        vec!["fast".to_string()],
        "the branch `resolution < 5` cannot run when `resolution = 6`"
    );
}

#[test]
fn statically_true_branch_shadows_the_else_branch() {
    let condition = binary(
        rumoca_core::OpBinary::Ge,
        var("resolution", 0),
        literal(5.0, 50),
    );
    let residual = periodic_exact_clock_residual(condition);

    assert_eq!(
        source_names(&residual, &resolution_constants()),
        vec!["slow".to_string()],
        "a statically taken branch leaves no reachable `else` branch"
    );
}

#[test]
fn run_time_condition_keeps_both_branches() {
    let condition = binary(
        rumoca_core::OpBinary::Lt,
        var("unknown", 0),
        literal(5.0, 50),
    );
    let residual = periodic_exact_clock_residual(condition);

    assert_eq!(
        source_names(&residual, &resolution_constants()),
        vec!["slow".to_string(), "fast".to_string()],
        "an undecided condition leaves every branch a candidate clock source"
    );
}
