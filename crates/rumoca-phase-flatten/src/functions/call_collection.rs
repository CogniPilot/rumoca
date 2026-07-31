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

    // Collect from equations
    for eq in &flat.equations {
        collect_from_expression(&eq.residual, &mut calls);
    }

    // Collect from initial equations
    for eq in &flat.initial_equations {
        collect_from_expression(&eq.residual, &mut calls);
    }

    // Collect from variable bindings and attributes
    for var in flat.variables.values() {
        if let Some(binding) = &var.binding {
            collect_from_expression(binding, &mut calls);
        }
        if let Some(start) = &var.start {
            collect_from_expression(start, &mut calls);
        }
        if let Some(min) = &var.min {
            collect_from_expression(min, &mut calls);
        }
        if let Some(max) = &var.max {
            collect_from_expression(max, &mut calls);
        }
        if let Some(nominal) = &var.nominal {
            collect_from_expression(nominal, &mut calls);
        }
    }

    // Collect from complete when/elsewhen owners in source-priority order.
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            collect_from_expression(&branch.condition, &mut calls);
            for eq in &branch.equations {
                collect_from_when_equation(eq, &mut calls);
            }
        }
    }

    // Collect from assertions
    for assertion in &flat.assert_equations {
        collect_from_expression(&assertion.condition, &mut calls);
        collect_from_expression(&assertion.message, &mut calls);
        if let Some(level) = &assertion.level {
            collect_from_expression(level, &mut calls);
        }
    }
    for assertion in &flat.initial_assert_equations {
        collect_from_expression(&assertion.condition, &mut calls);
        collect_from_expression(&assertion.message, &mut calls);
        if let Some(level) = &assertion.level {
            collect_from_expression(level, &mut calls);
        }
    }

    // Collect from algorithm statements
    for algorithm in &flat.algorithms {
        for statement in &algorithm.statements {
            collect_from_statement(statement, &mut calls);
        }
    }
    for algorithm in &flat.initial_algorithms {
        for statement in &algorithm.statements {
            collect_from_statement(statement, &mut calls);
        }
    }

    calls.into_entries()
}

/// Collect function calls from a WhenEquation.
fn collect_from_when_equation(eq: &rumoca_ir_flat::WhenEquation, calls: &mut FunctionRequests) {
    match eq {
        flat::WhenEquation::Assign { value, .. } => {
            collect_from_expression(value, calls);
        }
        flat::WhenEquation::Reinit { value, .. } => {
            collect_from_expression(value, calls);
        }
        flat::WhenEquation::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_from_expression(condition, calls);
            collect_from_expression(message, calls);
            if let Some(level) = level {
                collect_from_expression(level, calls);
            }
        }
        flat::WhenEquation::Terminate { message, .. } => {
            collect_from_expression(message, calls);
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => {
            for (cond, eqs) in branches {
                collect_from_expression(cond, calls);
                for eq in eqs {
                    collect_from_when_equation(eq, calls);
                }
            }
            if let Some(else_branch) = else_branch {
                for eq in else_branch {
                    collect_from_when_equation(eq, calls);
                }
            }
        }
        flat::WhenEquation::FunctionCallOutputs { function, .. } => {
            // Collect function calls from the multi-output function call expression
            collect_from_expression(function, calls);
        }
    }
}

struct FunctionCallCollector<'a> {
    calls: &'a mut FunctionRequests,
}

impl rumoca_core::ExpressionVisitor for FunctionCallCollector<'_> {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        self.calls.insert(FunctionRequest::from_reference(name));
        self.walk_function_call(name, args, is_constructor);
    }
}

impl rumoca_ir_flat::visitor::StatementVisitor for FunctionCallCollector<'_> {
    fn visit_statement_function_call(
        &mut self,
        comp: &rumoca_core::ComponentReference,
        args: &[rumoca_core::Expression],
        outputs: &[Option<rumoca_core::ComponentReference>],
    ) {
        self.calls
            .insert(FunctionRequest::from_component_reference(comp));
        self.visit_component_reference(comp);
        for arg in args {
            self.visit_expression(arg);
        }
        for output in outputs.iter().flatten() {
            self.visit_component_reference(output);
        }
    }
}

/// Collect function calls from an expression using the visitor pattern.
fn collect_from_expression(expr: &rumoca_core::Expression, calls: &mut FunctionRequests) {
    let mut collector = FunctionCallCollector { calls };
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expr);
}

pub(crate) fn collect_function_dep_requests(func: &rumoca_core::Function) -> Vec<FunctionRequest> {
    let mut deps = FunctionRequests::default();

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
            collect_from_expression(default, &mut deps);
        }
        for subscript in &param.shape_expr {
            collect_from_subscript(subscript, &mut deps);
        }
    }

    for stmt in &func.body {
        collect_from_statement(stmt, &mut deps);
    }

    deps.into_entries()
}

fn collect_from_subscript(subscript: &rumoca_core::Subscript, deps: &mut FunctionRequests) {
    if let rumoca_core::Subscript::Expr { expr, .. } = subscript {
        collect_from_expression(expr, deps);
    }
}

fn collect_from_statement(stmt: &rumoca_core::Statement, deps: &mut FunctionRequests) {
    let mut collector = FunctionCallCollector { calls: deps };
    rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut collector, stmt);
}
