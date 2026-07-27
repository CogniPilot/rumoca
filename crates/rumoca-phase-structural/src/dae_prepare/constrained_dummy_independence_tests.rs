//! Regression coverage for the independence precondition of the
//! Mattsson-Söderlind dummy-derivative construction.
//!
//! The construction is only an exchange — one unknown for one row — when the
//! differentiated constraint it appends actually *determines* the generated
//! dummy derivative. If the DAE already carries a row that writes `der(x)` as
//! some `v`, the appended row restates it: alias substitution collapses the two
//! into the same equation, duplicate removal drops one, and the demotion has
//! contributed an unknown without contributing an equation.
//!
//! The shape below is the rotational alias chain every MSL `Revolute` with an
//! axis flange produces (`Modelica.Mechanics.MultiBody.Examples.Elementary.
//! DoublePendulum`): the joint angle, its flange twin, and the derivative-alias
//! rows `w = der(phi)` / `a = der(w)` on both sides. Demoting the flange twin is
//! correct; demoting the joint angle *as well* leaves both angles algebraic with
//! only the single alias row between them to determine them, and the whole
//! model loses a degree of freedom.

use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("constrained_dummy_independence_test.mo"),
        1,
        2,
    )
}

fn continuous_variable(name: &str) -> Variable {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: test_span(),
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: test_span(),
    }
}

fn der(name: &str) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var(name)],
        span: test_span(),
    }
}

fn eq(rhs: Expression) -> Equation {
    Equation {
        lhs: None,
        rhs,
        span: test_span(),
        origin: "top-level model equation".to_string(),
        scalar_count: 1,
    }
}

/// A joint angle `phi` and the flange coordinate `p` that mirrors it.
///
/// ```text
/// 0 = w  - der(phi)   // phi's derivative alias
/// 0 = a  - der(w)
/// 0 = p  - fb         // p's defining constraint
/// 0 = pw - der(p)     // p's derivative alias
/// 0 = fb - phi        // the flange connection, readable either way round
/// 0 = a               // the drive that closes the system
/// ```
///
/// `p` is a genuine dependent coordinate: the constraint `p = fb` determines
/// it, and its differentiated constraint `__dummyder__.p = w` is a row the DAE
/// does not already have — nothing else writes `der(p)` in terms of a known
/// quantity, because `pw` is itself an unknown.
///
/// `phi` is not. Reading the flange row the other way round makes `phi = fb` a
/// candidate definition, and its time derivative resolves — through the dummy
/// the first round generated for `p` — back to `w`. The row `w = der(phi)`
/// already says exactly that, so appending it again would leave both `phi` and
/// `p` algebraic with one independent row between them and take the model's
/// only degree of freedom with it.
fn flange_alias_chain_dae() -> Dae {
    let mut dae = Dae::new();
    for name in ["phi", "w", "p"] {
        dae.variables
            .states
            .insert(VarName::new(name), continuous_variable(name));
    }
    for name in ["a", "fb", "pw"] {
        dae.variables
            .algebraics
            .insert(VarName::new(name), continuous_variable(name));
    }
    dae.continuous.equations = vec![
        eq(sub(var("w"), der("phi"))),
        eq(sub(var("a"), der("w"))),
        eq(sub(var("p"), var("fb"))),
        eq(sub(var("pw"), der("p"))),
        eq(sub(var("fb"), var("phi"))),
        eq(var("a")),
    ];
    dae
}

/// Scalar rows minus scalar unknowns, counted the way the structural matcher
/// counts them: one unknown per algebraic, one per distinct `der()` column, and
/// none for a state (the integrator supplies it).
fn structural_balance(dae: &Dae) -> i64 {
    let rows: usize = dae
        .continuous
        .equations
        .iter()
        .map(|equation| equation.scalar_count)
        .sum();
    let mut derivative_columns = std::collections::BTreeSet::new();
    for equation in &dae.continuous.equations {
        collect_derivative_columns(&equation.rhs, &mut derivative_columns);
    }
    let unknowns = dae.variables.algebraics.len() + derivative_columns.len();
    i64::try_from(rows).expect("row count fits i64") - i64::try_from(unknowns).expect("fits i64")
}

fn collect_derivative_columns(expr: &Expression, out: &mut std::collections::BTreeSet<String>) {
    struct Collector<'a> {
        out: &'a mut std::collections::BTreeSet<String>,
    }
    impl ExpressionVisitor for Collector<'_> {
        fn visit_expression(&mut self, expr: &Expression) {
            if let Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } = expr
                && let [Expression::VarRef { name, .. }] = args.as_slice()
            {
                self.out.insert(name.as_str().to_string());
            }
            self.walk_expression(expr);
        }
    }
    Collector { out }.visit_expression(expr);
}

#[test]
fn a_state_whose_derivative_a_row_already_names_is_not_demoted() {
    let mut dae = flange_alias_chain_dae();
    reduce_constrained_dummy_derivatives(&mut dae).expect("constrained dummy reduction");
    let states: Vec<&str> = dae.variables.states.keys().map(VarName::as_str).collect();
    assert!(
        states.contains(&"phi"),
        "`phi`'s differentiated constraint only restates the row `w = der(phi)`, \
         so demoting it would remove a coordinate without adding an equation; states={states:?}"
    );
    assert!(
        !states.contains(&"p"),
        "`p` is the dependent coordinate and must still be reduced; states={states:?}"
    );
}

#[test]
fn reducing_the_flange_alias_chain_preserves_the_structural_balance() {
    let mut dae = flange_alias_chain_dae();
    let before = structural_balance(&dae);
    reduce_constrained_dummy_derivatives(&mut dae).expect("constrained dummy reduction");
    assert_eq!(
        structural_balance(&dae),
        before,
        "every demoted scalar state must bring exactly one defining row with it"
    );
}
