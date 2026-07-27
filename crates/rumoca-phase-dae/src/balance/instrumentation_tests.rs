//! Regression tests proving the balance instrumentation (clamps + exclusion
//! counts) is inert with respect to the balance verdict.
//!
//! These live in a sibling module so `balance.rs` stays under the SPEC_0021
//! file-size budget.

use super::{BalanceExclusionCounts, balance, balance_detail};
use rumoca_core::{SourceId, Span, VarName};
use rumoca_ir_dae as dae;

fn fixture_span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("balance_instrumentation.mo"),
        1,
        2,
    )
}

fn var(name: &str) -> dae::Variable {
    dae::Variable {
        name: VarName::new(name),
        ..dae::Variable::empty_with_span(fixture_span())
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: fixture_span(),
    }
}

fn alias_eq(lhs: &str, rhs: &str, origin: &str) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref(lhs)),
            rhs: Box::new(var_ref(rhs)),
            span: fixture_span(),
        },
        span: fixture_span(),
        origin: origin.to_string(),
        scalar_count: 1,
    }
}

/// Golden numbers captured before the diagnostic fields were added. If the
/// clamp/exclusion refactor ever leaks into the arithmetic these move.
#[test]
fn balance_detail_arithmetic_unchanged_by_diagnostic_fields() {
    let mut model = dae::Dae::default();
    model
        .variables
        .algebraics
        .insert(VarName::new("a"), var("a"));
    model
        .variables
        .algebraics
        .insert(VarName::new("b"), var("b"));
    model
        .variables
        .algebraics
        .insert(VarName::new("c"), var("c"));
    model
        .continuous
        .equations
        .push(alias_eq("a", "b", "component equation"));
    model.metadata.interface_flow_count = 5;
    model.metadata.overconstrained_interface_count = 4;
    model.metadata.oc_break_edge_scalar_count = 3;

    let detail = balance_detail(&model).expect("valid balance fixture");
    assert_eq!(detail.f_x_scalar, 1);
    assert_eq!(detail.raw_unknowns(), 3);
    // f_x(1) + iflow clamped to the deficit(2) == unknowns(3); the oc term and
    // the break-edge correction are both fully clamped away.
    assert_eq!(detail.equations_unknowns(), (3, 3));
    assert_eq!(detail.balance(), 0);
    assert_eq!(balance(&model).expect("valid balance fixture"), 0);

    let clamps = detail.clamps();
    assert_eq!(clamps.interface_flow_dropped, 3);
    assert_eq!(clamps.oc_interface_dropped, 4);
    assert_eq!(clamps.break_edge_dropped, 3);
    assert_eq!(clamps.aggregate_candidates_dropped, 0);
}

#[test]
fn balance_exclusion_counts_match_filtered_rows() {
    let mut model = dae::Dae::default();
    model
        .variables
        .algebraics
        .insert(VarName::new("a"), var("a"));
    model.variables.inputs.insert(VarName::new("u"), var("u"));
    model
        .variables
        .parameters
        .insert(VarName::new("p"), var("p"));
    model
        .variables
        .parameters
        .insert(VarName::new("q"), var("q"));

    // Counted: constrains the continuous unknown `a`.
    model
        .continuous
        .equations
        .push(alias_eq("a", "p", "component equation"));
    // Excluded (connection alias between two non-unknowns).
    model
        .continuous
        .equations
        .push(alias_eq("p", "q", "connection equation: p = q"));
    // Excluded (binding equation that is an input-only alias).
    model
        .continuous
        .equations
        .push(alias_eq("u", "p", "binding equation for u"));
    // Excluded (references neither a continuous unknown nor an input).
    model
        .continuous
        .equations
        .push(alias_eq("p", "q", "component equation"));

    let detail = balance_detail(&model).expect("valid balance fixture");
    let excluded = detail.excluded;
    assert_eq!(excluded.connection_no_continuous_ref, 1, "{excluded}");
    assert_eq!(excluded.binding_input_alias, 1, "{excluded}");
    assert_eq!(excluded.no_continuous_or_input_ref, 1, "{excluded}");
    assert_eq!(excluded.redundant_connection_alias, 0, "{excluded}");
    assert_eq!(excluded.total(), 3);
    // Exclusions account for exactly the rows that did not reach `f_x`.
    assert_eq!(
        model.continuous.equations.len() - excluded.total(),
        detail.f_x_scalar
    );
}

#[test]
fn balance_exclusion_counts_report_redundant_connection_aliases() {
    let mut model = dae::Dae::default();
    model
        .variables
        .algebraics
        .insert(VarName::new("a"), var("a"));
    model
        .variables
        .parameters
        .insert(VarName::new("p"), var("p"));
    // `a` is defined by a component equation, so the connection alias that
    // re-states it is redundant.
    model
        .continuous
        .equations
        .push(alias_eq("a", "p", "component equation"));
    model
        .continuous
        .equations
        .push(alias_eq("a", "p", "connection equation: a = p"));

    let detail = balance_detail(&model).expect("valid balance fixture");
    assert_eq!(detail.excluded.redundant_connection_alias, 1);
    assert_eq!(detail.f_x_scalar, 1);
}

#[test]
fn balance_exclusion_counts_are_inert_when_every_row_counts() {
    let mut model = dae::Dae::default();
    model
        .variables
        .algebraics
        .insert(VarName::new("a"), var("a"));
    model
        .variables
        .parameters
        .insert(VarName::new("p"), var("p"));
    model
        .continuous
        .equations
        .push(alias_eq("a", "p", "component equation"));

    let detail = balance_detail(&model).expect("valid balance fixture");
    assert_eq!(detail.excluded, BalanceExclusionCounts::default());
    assert!(detail.excluded.is_inert());
}
