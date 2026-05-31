//! Dummy-derivative elimination for index-reduced DAEs.
//!
//! When a model exposes a variable that is defined to equal a state derivative
//! (`di = der(x)`, a *dummy derivative*), the state derivative appears in two
//! distinct roles:
//!
//! 1. The defining equation `di = der(x)` — the trivial link between the
//!    algebraic `di` and the state derivative `der(x)`.
//! 2. Constitutive / coupling equations such as `v = L*der(x)` (and, for
//!    mutually-coupled inductors, `v = L*der(x1) + M*der(x2)`).
//!
//! Mirroring OpenModelica, the correct treatment is to make `di` an *algebraic
//! unknown* determined by the constitutive/coupling equations (an algebraic
//! linear system), with `der(x) = di` as the trivial state-derivative link.
//! Concretely we substitute `der(x) -> di` in every equation **except** the
//! defining equation, so:
//!
//! - The defining equation remains the unique equation carrying `der(x)` and is
//!   consumed as state `x`'s derivative row (`der(x) = di`).
//! - All other equations become derivative-free algebraic constraints that
//!   determine `di` (and the rest of the network).
//!
//! Without this, the defining equation is also (incorrectly) flagged as a
//! state-derivative equation and dropped, leaving `di` with no determining
//! equation — a structurally singular implicit Newton system that the sparse LU
//! cannot factor. See [`crate::ensure_implicit_system_determined`].

use std::collections::{HashMap, HashSet};

use rumoca_core::{BuiltinFunction, Expression, ExpressionRewriter, Reference, Span, Subscript};
use rumoca_ir_dae as dae;

/// If `dae_model` contains dummy-derivative definitions, return a rewritten copy
/// with `der(x) -> di` substituted in all non-defining continuous equations.
/// Returns `None` when there is nothing to rewrite, so callers can keep using
/// the original borrow without an allocation.
pub(crate) fn eliminate_dummy_derivatives(dae_model: &dae::Dae) -> Option<dae::Dae> {
    let aliases = collect_dummy_derivative_aliases(dae_model);
    if aliases.state_to_dummy.is_empty() {
        return None;
    }

    let mut rewriter = DerToDummyRewriter {
        state_to_dummy: &aliases.state_to_dummy,
    };
    let mut rewritten = dae_model.clone();
    for (idx, equation) in rewritten.continuous.equations.iter_mut().enumerate() {
        if aliases.defining_equation_indices.contains(&idx) {
            // Keep the defining equation intact so `der(x)` survives here and is
            // consumed as state `x`'s derivative row.
            continue;
        }
        equation.rhs = rewriter.rewrite_expression(&equation.rhs);
    }
    Some(rewritten)
}

struct DummyDerivativeAliases {
    /// state name -> dummy-derivative algebraic name (`der(state) == dummy`).
    state_to_dummy: HashMap<String, String>,
    /// Indices into `continuous.equations` of the `di = der(x)` defining rows.
    defining_equation_indices: HashSet<usize>,
}

fn collect_dummy_derivative_aliases(dae_model: &dae::Dae) -> DummyDerivativeAliases {
    let mut state_to_dummy = HashMap::new();
    let mut dummy_used = HashSet::new();
    let mut defining_equation_indices = HashSet::new();

    for (idx, equation) in dae_model.continuous.equations.iter().enumerate() {
        let Some((state, dummy)) = dummy_definition(dae_model, equation) else {
            continue;
        };
        // A dummy derivative links exactly one state to one fresh algebraic. Skip
        // any conflicting re-definition so the mapping stays a bijection.
        if state_to_dummy.contains_key(&state) || dummy_used.contains(&dummy) {
            continue;
        }
        state_to_dummy.insert(state.clone(), dummy.clone());
        dummy_used.insert(dummy);
        defining_equation_indices.insert(idx);
    }

    DummyDerivativeAliases {
        state_to_dummy,
        defining_equation_indices,
    }
}

/// Recognize an equation of the form `dummy = der(state)` (in residual form
/// `dummy - der(state)` / `der(state) - dummy`, or explicit `dummy := der(state)`),
/// where `dummy` is a scalar algebraic and `state` is a scalar continuous state.
fn dummy_definition(dae_model: &dae::Dae, equation: &dae::Equation) -> Option<(String, String)> {
    if let Some(lhs) = equation.lhs.as_ref() {
        let dummy = scalar_algebraic_name(dae_model, lhs.as_str())?;
        let state = der_of_state(dae_model, &equation.rhs)?;
        return Some((state, dummy));
    }
    let (lhs, rhs) = split_subtraction(&equation.rhs)?;
    if let Some(dummy) = scalar_algebraic_expr(dae_model, lhs)
        && let Some(state) = der_of_state(dae_model, rhs)
    {
        return Some((state, dummy));
    }
    if let Some(dummy) = scalar_algebraic_expr(dae_model, rhs)
        && let Some(state) = der_of_state(dae_model, lhs)
    {
        return Some((state, dummy));
    }
    None
}

fn split_subtraction(expr: &Expression) -> Option<(&Expression, &Expression)> {
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => Some((lhs, rhs)),
        _ => None,
    }
}

/// Name of the state whose derivative this expression is (`der(state)`), if the
/// argument is a scalar continuous state.
fn der_of_state(dae_model: &dae::Dae, expr: &Expression) -> Option<String> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args,
        ..
    } = expr
    else {
        return None;
    };
    let [arg] = args.as_slice() else {
        return None;
    };
    let name = scalar_var_ref_name(arg)?;
    dae_model
        .variables
        .states
        .contains_key(&rumoca_core::VarName::new(name.as_str()))
        .then_some(name)
}

fn scalar_algebraic_expr(dae_model: &dae::Dae, expr: &Expression) -> Option<String> {
    let name = scalar_var_ref_name(expr)?;
    scalar_algebraic_name(dae_model, name.as_str())
}

fn scalar_algebraic_name(dae_model: &dae::Dae, name: &str) -> Option<String> {
    dae_model
        .variables
        .algebraics
        .contains_key(&rumoca_core::VarName::new(name))
        .then(|| name.to_string())
}

fn scalar_var_ref_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.as_str().to_string()),
        _ => None,
    }
}

/// Rewrites `der(state) -> dummy` for every aliased state.
struct DerToDummyRewriter<'a> {
    state_to_dummy: &'a HashMap<String, String>,
}

impl ExpressionRewriter for DerToDummyRewriter<'_> {
    fn walk_builtin_call_expression(
        &mut self,
        function: BuiltinFunction,
        args: &[Expression],
        span: Span,
    ) -> Expression {
        if function == BuiltinFunction::Der
            && let [arg] = args
            && let Some(state) = scalar_var_ref_name(arg)
            && let Some(dummy) = self.state_to_dummy.get(&state)
        {
            return Expression::VarRef {
                name: Reference::from(rumoca_core::VarName::new(dummy.as_str())),
                subscripts: Vec::<Subscript>::new(),
                span,
            };
        }
        Expression::BuiltinCall {
            function,
            args: args
                .iter()
                .map(|arg| self.rewrite_expression(arg))
                .collect(),
            span,
        }
    }
}
