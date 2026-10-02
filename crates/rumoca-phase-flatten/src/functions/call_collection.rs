//! Function-call request collection over a flat model and over function bodies.
//!
//! Every callable a model reaches is discovered here: the walk visits each
//! equation, binding, attribute, `when` owner, assertion, and algorithm
//! statement, and records one [`FunctionRequest`] per call site. The same
//! visitor collects a converted function's own dependencies (MLS §12.2 body
//! calls, plus record-typed parameter constructors), which drives the
//! transitive collection worklist and reachability pruning.

use super::*;

/// Collect all user function calls from a flat::Model.
///
/// Walks through all equations and expressions to find function calls,
/// returning a set of unique function names that need definitions.
#[cfg(test)]
pub(crate) fn collect_function_calls(flat: &flat::Model) -> HashSet<String> {
    collect_function_call_requests(flat)
        .into_iter()
        .map(|request| request.name)
        .collect()
}

pub(super) fn collect_function_call_requests(flat: &flat::Model) -> Vec<FunctionRequest> {
    let mut calls = FunctionRequests::default();
    let values = ValueNames::new(flat.variables.keys().map(|name| name.as_str()));

    // Collect from equations
    for eq in &flat.equations {
        collect_from_expression(&eq.residual, &mut calls, &values);
    }

    // Collect from initial equations
    for eq in &flat.initial_equations {
        collect_from_expression(&eq.residual, &mut calls, &values);
    }

    // Collect from variable bindings and attributes
    for var in flat.variables.values() {
        if let Some(binding) = &var.binding {
            collect_from_expression(binding, &mut calls, &values);
        }
        if let Some(start) = &var.start {
            collect_from_expression(start, &mut calls, &values);
        }
        if let Some(min) = &var.min {
            collect_from_expression(min, &mut calls, &values);
        }
        if let Some(max) = &var.max {
            collect_from_expression(max, &mut calls, &values);
        }
        if let Some(nominal) = &var.nominal {
            collect_from_expression(nominal, &mut calls, &values);
        }
    }

    // Collect from complete when/elsewhen owners in source-priority order.
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            collect_from_expression(&branch.condition, &mut calls, &values);
            for eq in &branch.equations {
                collect_from_when_equation(eq, &mut calls, &values);
            }
        }
    }

    // Collect from assertions
    for assertion in &flat.assert_equations {
        collect_from_expression(&assertion.condition, &mut calls, &values);
        collect_from_expression(&assertion.message, &mut calls, &values);
        if let Some(level) = &assertion.level {
            collect_from_expression(level, &mut calls, &values);
        }
    }
    for assertion in &flat.initial_assert_equations {
        collect_from_expression(&assertion.condition, &mut calls, &values);
        collect_from_expression(&assertion.message, &mut calls, &values);
        if let Some(level) = &assertion.level {
            collect_from_expression(level, &mut calls, &values);
        }
    }

    // Collect from algorithm statements
    for algorithm in &flat.algorithms {
        for statement in &algorithm.statements {
            collect_from_statement(statement, &mut calls, &values);
        }
    }
    for algorithm in &flat.initial_algorithms {
        for statement in &algorithm.statements {
            collect_from_statement(statement, &mut calls, &values);
        }
    }

    calls.into_entries()
}

/// Collect function calls from a WhenEquation.
fn collect_from_when_equation(
    eq: &rumoca_ir_flat::WhenEquation,
    calls: &mut FunctionRequests,
    values: &ValueNames,
) {
    match eq {
        flat::WhenEquation::Assign { value, .. } => {
            collect_from_expression(value, calls, values);
        }
        flat::WhenEquation::Reinit { value, .. } => {
            collect_from_expression(value, calls, values);
        }
        flat::WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_from_expression(condition, calls, values);
            collect_from_expression(message, calls, values);
            if let Some(level) = level {
                collect_from_expression(level, calls, values);
            }
        }
        flat::WhenEquation::Terminate { message, .. } => {
            collect_from_expression(message, calls, values);
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (cond, eqs) in branches {
                collect_from_expression(cond, calls, values);
                for eq in eqs {
                    collect_from_when_equation(eq, calls, values);
                }
            }
            if let Some(else_branch) = else_branch {
                for eq in else_branch {
                    collect_from_when_equation(eq, calls, values);
                }
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            // Collect function calls from the multi-output function call expression
            collect_from_expression(function, calls, values);
        }
    }
}

struct FunctionCallCollector<'a> {
    calls: &'a mut FunctionRequests,
    values: &'a ValueNames,
}

impl rumoca_core::ExpressionVisitor for FunctionCallCollector<'_> {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        self.calls.insert(FunctionRequest::from_reference(name));
        // MLS 3.7 §12.4.2.1: an argument may name a function, which the
        // call evaluates through the callee's functional input.
        for arg in args {
            if let rumoca_core::Expression::VarRef {
                name: argument,
                subscripts,
                ..
            } = arg
                && subscripts.is_empty()
                && argument.component_ref().is_some()
                && !self.values.names(argument.as_str())
            {
                self.calls.insert(FunctionRequest::from_reference(argument));
            }
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

impl rumoca_ir_flat::visitor::StatementVisitor for FunctionCallCollector<'_> {
    fn visit_statement_function_call(
        &mut self,
        comp: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        outputs: &[Option<rumoca_core::ComponentReference>],
    ) {
        self.calls.insert(FunctionRequest::from_reference(comp));
        if let Some(component) = comp.component_ref() {
            self.visit_component_reference(component);
        }
        for arg in args {
            self.visit_expression(arg);
        }
        for output in outputs.iter().flatten() {
            self.visit_component_reference(output);
        }
    }
}

/// Collect function calls from an expression using the visitor pattern.
fn collect_from_expression(
    expr: &rumoca_core::Expression,
    calls: &mut FunctionRequests,
    values: &ValueNames,
) {
    let mut collector = FunctionCallCollector { calls, values };
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expr);
}

pub(crate) fn collect_function_dep_requests(func: &rumoca_core::Function) -> Vec<FunctionRequest> {
    let mut deps = FunctionRequests::default();
    let values = ValueNames::new(
        func.inputs
            .iter()
            .chain(&func.outputs)
            .chain(&func.locals)
            .map(|param| param.name.as_str()),
    );

    for param in func
        .inputs
        .iter()
        .chain(func.outputs.iter())
        .chain(func.locals.iter())
    {
        if param.type_class == Some(rumoca_core::ClassType::Record) {
            deps.insert(FunctionRequest::from_type_param(param));
        }
        if let Some(default) = &param.default {
            collect_from_expression(default, &mut deps, &values);
        }
        for subscript in &param.shape_expr {
            collect_from_subscript(subscript, &mut deps, &values);
        }
    }

    for stmt in &func.body {
        collect_from_statement(stmt, &mut deps, &values);
    }

    for derivative in &func.derivatives {
        deps.insert(FunctionRequest::from_reference(
            &derivative.derivative_function,
        ));
    }

    deps.into_entries()
}

fn collect_from_subscript(
    subscript: &rumoca_core::Subscript,
    deps: &mut FunctionRequests,
    values: &ValueNames,
) {
    if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
        collect_from_expression(expr, deps, values);
    }
}

fn collect_from_statement(
    stmt: &rumoca_core::Statement,
    deps: &mut FunctionRequests,
    values: &ValueNames,
) {
    let mut collector = FunctionCallCollector {
        calls: deps,
        values,
    };
    rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut collector, stmt);
}

/// The value names in scope of a collection (model variables, or a function's
/// formals and locals), by root identifier, so an argument that reads a value
/// is never requested as a function.
struct ValueNames {
    roots: HashSet<String>,
}

impl ValueNames {
    fn new<'a>(names: impl IntoIterator<Item = &'a str>) -> Self {
        Self {
            roots: names
                .into_iter()
                .map(|name| root(name).to_string())
                .collect(),
        }
    }

    fn names(&self, name: &str) -> bool {
        name == "time" || self.roots.contains(root(name))
    }
}

fn root(name: &str) -> &str {
    name.split(['.', '[']).next().unwrap_or(name)
}
