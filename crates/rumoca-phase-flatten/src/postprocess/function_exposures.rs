//! The package each collected function is exposed through (MLS §7.3).
//!
//! `Medium.f` written in a model names the function `f` of the package the
//! slot `Medium` selects there. A constant the function's declarations read
//! (`n` in `Real X[n]` of an inherited record) takes the value that package
//! gives it, which differs from the declaring package when the selected
//! package extends it with modifications (`MoistAir` extends
//! `PartialCondensingGases(substanceNames = {"water", "air"})`). The call's
//! structured prefix proves the exposure; a call inside a function without a
//! prefix inherits the exposures of the calling function. A function no call
//! proves an exposure for is exposed by the package it was instantiated
//! from, the enclosing scope of its own flat path.
//!
//! One function instance can be reached through several packages (a helper
//! that two media share). It keeps every exposure; a constant it reads takes
//! the value all of them agree on, and packages that give the constant
//! different values make the read ambiguous, which is refused (EF034) rather
//! than answered with one package's value or the declaring package's.

use rumoca_core::{ExpressionVisitor, FunctionInstanceId, Reference};
use rumoca_ir_flat::StatementVisitor;
use std::collections::BTreeSet;

use rumoca_core::{DefId, Expression, Span};
use rustc_hash::FxHashMap;

use crate::{Context, FlattenError};
use rumoca_ir_flat as flat;

/// The qualified names of every package that exposes each function instance,
/// sorted.
pub(super) fn function_exposures(
    flat: &flat::Model,
    ctx: &Context,
) -> FxHashMap<FunctionInstanceId, Vec<String>> {
    let mut model_calls = CallCollector::new(ctx, Vec::new());
    for variable in flat.variables.values() {
        for expression in [
            &variable.binding,
            &variable.start,
            &variable.min,
            &variable.max,
            &variable.nominal,
        ]
        .into_iter()
        .flatten()
        {
            model_calls.visit_expression(expression);
        }
    }
    for equation in flat.equations.iter().chain(&flat.initial_equations) {
        model_calls.visit_expression(&equation.residual);
    }
    for algorithm in flat.algorithms.iter().chain(&flat.initial_algorithms) {
        for statement in &algorithm.statements {
            model_calls.visit_statement(statement);
        }
    }
    let mut exposures: FxHashMap<FunctionInstanceId, BTreeSet<String>> = FxHashMap::default();
    extend(&mut exposures, model_calls.calls);
    propagate(flat, ctx, &mut exposures, &rustc_hash::FxHashSet::default());
    // A function no call proves an exposure for is exposed by the package it
    // was instantiated from: the enclosing scope of its own flat path. A
    // proven exposure takes precedence, because the declaration path of a
    // replaceable package alias (`PartialDistributedVolume.Medium`) names its
    // default, not the package an instance selects.
    let unexposed = flat
        .functions
        .values()
        .filter_map(|function| {
            let instance = function.instance_id?;
            let scope = crate::path_utils::enclosing_scope(function.name.as_str())?;
            (!exposures.contains_key(&instance)).then(|| (instance, scope.to_string()))
        })
        .collect::<Vec<_>>();
    let proven = exposures
        .keys()
        .copied()
        .collect::<rustc_hash::FxHashSet<_>>();
    extend(&mut exposures, unexposed);
    propagate(flat, ctx, &mut exposures, &proven);
    exposures
        .into_iter()
        .map(|(instance, packages)| (instance, packages.into_iter().collect()))
        .collect()
}

/// Pass every exposure of a function to its unprefixed calls, to a fixed point.
/// A `settled` instance keeps the exposures it has.
fn propagate(
    flat: &flat::Model,
    ctx: &Context,
    exposures: &mut FxHashMap<FunctionInstanceId, BTreeSet<String>>,
    settled: &rustc_hash::FxHashSet<FunctionInstanceId>,
) {
    // Function bodies inherit every exposure of the caller for unprefixed
    // calls. The sets only grow and are bounded by the packages the model
    // names, so the iteration reaches a fixed point; it stops at the first
    // pass that changes no set.
    loop {
        let before = exposures.clone();
        for function in flat.functions.values() {
            let inherited = function
                .instance_id
                .and_then(|instance| exposures.get(&instance))
                .map(|packages| packages.iter().cloned().collect::<Vec<_>>())
                .unwrap_or_default();
            let mut calls = CallCollector::new(ctx, inherited);
            for statement in &function.body {
                calls.visit_statement(statement);
            }
            let defaults = function
                .inputs
                .iter()
                .chain(&function.outputs)
                .chain(&function.locals)
                .filter_map(|parameter| parameter.default.as_ref());
            for default in defaults {
                calls.visit_expression(default);
            }
            let calls = calls
                .calls
                .into_iter()
                .filter(|(instance, _)| !settled.contains(instance))
                .collect();
            extend(exposures, calls);
        }
        if *exposures == before {
            break;
        }
    }
}

fn extend(
    exposures: &mut FxHashMap<FunctionInstanceId, BTreeSet<String>>,
    calls: Vec<(FunctionInstanceId, String)>,
) {
    for (instance, package) in calls {
        exposures.entry(instance).or_default().insert(package);
    }
}

/// The value the exposing packages give the constant `target` (MLS §7.3).
///
/// `None` when no exposing package gives it its own value, so the declaration's
/// value applies. A package that does not modify the constant contributes the
/// declaration's value; packages that disagree are refused (EF034).
pub(super) fn exposed_constant_value<'a>(
    ctx: &'a Context,
    exposures: &[String],
    target: DefId,
    name: &str,
    span: Span,
) -> Result<Option<&'a Expression>, FlattenError> {
    let scoped = exposures
        .iter()
        .map(|package| ctx.constant_values_by_scope.get(&(package.clone(), target)))
        .collect::<Vec<_>>();
    if scoped.iter().all(Option::is_none) {
        return Ok(None);
    }
    let declared = ctx.constant_values_by_def_id.get(&target);
    let values = scoped
        .iter()
        .map(|value| value.or(declared))
        .collect::<Vec<_>>();
    let first = values[0];
    let agree = values.iter().all(|value| match (value, first) {
        (Some(value), Some(first)) => rumoca_core::expressions_semantically_equal(value, first),
        (None, None) => true,
        _ => false,
    });
    if !agree {
        return Err(FlattenError::conflicting_exposed_constant(
            name,
            exposures.join(", "),
            span,
        ));
    }
    Ok(first)
}

struct CallCollector<'ctx> {
    ctx: &'ctx Context,
    inherited: Vec<String>,
    calls: Vec<(FunctionInstanceId, String)>,
}

impl<'ctx> CallCollector<'ctx> {
    fn new(ctx: &'ctx Context, inherited: Vec<String>) -> Self {
        Self {
            ctx,
            inherited,
            calls: Vec::new(),
        }
    }

    fn exposing_packages(&self, name: &Reference) -> Vec<String> {
        let prefix = name
            .component_ref()
            .and_then(|component| component.parts().iter().rev().nth(1))
            .and_then(|slot| self.ctx.selected_package(slot.def_id, name.instance_id()))
            .map(|(_, package)| package.to_string());
        prefix.map_or_else(|| self.inherited.clone(), |package| vec![package])
    }
}

impl ExpressionVisitor for CallCollector<'_> {
    fn visit_function_call(
        &mut self,
        name: &Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        if let Some(resolved) = name.resolved_function() {
            for package in self.exposing_packages(name) {
                self.calls.push((resolved.instance_id, package));
            }
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

impl StatementVisitor for CallCollector<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: Span::DUMMY,
        }
    }

    /// `k` declared as 1, given 2 by `A` and `B`, 3 by `C`, and left alone by `D`.
    fn context() -> (Context, DefId) {
        let target = DefId::new(7);
        let mut ctx = Context::new();
        ctx.constant_values_by_def_id.insert(target, real(1.0));
        for (package, value) in [("A", 2.0), ("B", 2.0), ("C", 3.0)] {
            ctx.constant_values_by_scope
                .insert((package.to_string(), target), real(value));
        }
        (ctx, target)
    }

    fn exposed(packages: &[&str]) -> Result<Option<Expression>, FlattenError> {
        let (ctx, target) = context();
        let packages = packages.iter().map(ToString::to_string).collect::<Vec<_>>();
        exposed_constant_value(&ctx, &packages, target, "k", Span::DUMMY)
            .map(|value| value.cloned())
    }

    #[test]
    fn packages_that_agree_give_their_value() {
        assert_eq!(exposed(&["A", "B"]).unwrap(), Some(real(2.0)));
    }

    #[test]
    fn packages_that_give_no_value_leave_the_declaration() {
        assert_eq!(exposed(&[]).unwrap(), None);
        assert_eq!(exposed(&["D"]).unwrap(), None);
    }

    #[test]
    fn packages_that_disagree_are_refused() {
        for packages in [&["A", "C"][..], &["A", "D"][..]] {
            let error = exposed(packages).expect_err("no single value");
            assert!(
                matches!(error, FlattenError::ConflictingExposedConstant { .. }),
                "{error:?}"
            );
        }
    }
}
