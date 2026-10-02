//! The package each collected function is exposed through (MLS §7.3).
//!
//! `Medium.f` written in a model names the function `f` of the package the
//! slot `Medium` selects there. A constant the function's declarations read
//! (`n` in `Real X[n]` of an inherited record) takes the value that package
//! gives it, which differs from the declaring package when the selected
//! package extends it with modifications (`MoistAir` extends
//! `PartialCondensingGases(substanceNames = {"water", "air"})`). The call's
//! structured prefix proves the exposure; a call inside a function without a
//! prefix inherits the exposure of the calling function.

use rumoca_core::{ExpressionVisitor, FunctionInstanceId, Reference};
use rumoca_ir_flat::StatementVisitor;
use rustc_hash::FxHashMap;

use crate::Context;
use rumoca_ir_flat as flat;

/// The qualified name of the package that exposes each function instance, for
/// the instances whose every exposing call agrees on one package.
pub(super) fn function_exposures(
    flat: &flat::Model,
    ctx: &Context,
) -> FxHashMap<FunctionInstanceId, String> {
    let mut model_calls = CallCollector::new(ctx, None);
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
    let mut exposures = Exposures::default();
    exposures.extend(model_calls.calls);
    // Function bodies inherit the caller's exposure for unprefixed calls; a
    // call chain is at most as long as the function table, so iterate to a
    // fixed point bounded by it.
    for _ in 0..=flat.functions.len() {
        let before = exposures.resolved.len();
        for function in flat.functions.values() {
            let inherited = function
                .instance_id
                .and_then(|instance| exposures.resolved.get(&instance).cloned());
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
            exposures.extend(calls.calls);
        }
        if exposures.resolved.len() == before {
            break;
        }
    }
    exposures.resolved
}

#[derive(Default)]
struct Exposures {
    resolved: FxHashMap<FunctionInstanceId, String>,
    conflicting: rustc_hash::FxHashSet<FunctionInstanceId>,
}

impl Exposures {
    fn extend(&mut self, calls: Vec<(FunctionInstanceId, String)>) {
        for (instance, package) in calls {
            if self.conflicting.contains(&instance) {
                continue;
            }
            match self.resolved.get(&instance) {
                Some(existing) if *existing != package => {
                    self.resolved.remove(&instance);
                    self.conflicting.insert(instance);
                }
                Some(_) => {}
                None => {
                    self.resolved.insert(instance, package);
                }
            }
        }
    }
}

struct CallCollector<'ctx> {
    ctx: &'ctx Context,
    inherited: Option<String>,
    calls: Vec<(FunctionInstanceId, String)>,
}

impl<'ctx> CallCollector<'ctx> {
    fn new(ctx: &'ctx Context, inherited: Option<String>) -> Self {
        Self {
            ctx,
            inherited,
            calls: Vec::new(),
        }
    }

    fn exposing_package(&self, name: &Reference) -> Option<String> {
        let prefix = name
            .component_ref()
            .and_then(|component| component.parts().iter().rev().nth(1))
            .and_then(|slot| self.ctx.selected_package(slot.def_id, name.instance_id()))
            .map(|(_, package)| package.to_string());
        prefix.or_else(|| self.inherited.clone())
    }
}

impl ExpressionVisitor for CallCollector<'_> {
    fn visit_function_call(
        &mut self,
        name: &Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        if let Some(resolved) = name.resolved_function()
            && let Some(package) = self.exposing_package(name)
        {
            self.calls.push((resolved.instance_id, package));
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

impl StatementVisitor for CallCollector<'_> {}
