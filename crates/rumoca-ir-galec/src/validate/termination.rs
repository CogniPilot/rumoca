//! Termination analysis (S-2.10/S-2.11, GAL-017): the static function call
//! graph must be cycle-free (no direct or mutual recursion); every user
//! function must be transitively called from `DoStep` (dead functions are
//! illegal); `Startup` may call builtins only.
//!
//! `Recalibrate` presence and the exactly-three-methods rule are guaranteed
//! by construction ([`crate::ast::Block`] has the three methods as dedicated
//! fields), so they need no checks here. Calls to unknown targets are
//! reported by the type analysis (EG015).

use std::collections::{HashMap, HashSet};

use crate::ast::{Condition, Expression, FunctionCall, LimitTarget, Reference, Spanned, Statement};
use crate::diagnostic::{GalecError, Location, PathSegment};

use super::context::{BlockContext, Callee, lexeme, reference_parts, resolve_call};

pub(super) fn check(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    startup_builtins_only(ctx, diags);
    let graph = user_call_graph(ctx);
    report_cycles(ctx, &graph, diags);
    report_unreachable(ctx, &graph, diags);
}

fn startup_builtins_only(ctx: &BlockContext<'_>, diags: &mut Vec<GalecError>) {
    let base = vec![
        PathSegment::Block(lexeme(&ctx.block.name)),
        PathSegment::Method(crate::ast::BlockMethodKind::Startup),
    ];
    for_each_call(
        &ctx.block.startup.statements,
        &base,
        &mut |call, location| {
            if matches!(resolve_call(ctx, &call.function), Some(Callee::User(_))) {
                diags.push(GalecError::StartupCallsUserFunction {
                    location,
                    name: lexeme(&call.function),
                });
            }
        },
    );
}

/// Edges between user functions, in declaration order for determinism.
/// Method bodies contribute reachability roots but are not graph nodes
/// (they are not callable: `Startup`/`Recalibrate`/`DoStep` are not in the
/// function namespace).
fn user_call_graph(ctx: &BlockContext<'_>) -> HashMap<String, Vec<String>> {
    let mut graph = HashMap::new();
    for function in user_functions(ctx) {
        graph.insert(
            lexeme(&function.name),
            user_callees(ctx, &function.statements),
        );
    }
    graph
}

fn user_functions<'a>(
    ctx: &BlockContext<'a>,
) -> impl Iterator<Item = &'a crate::ast::UserFunction> {
    ctx.block
        .protected_functions
        .iter()
        .chain(&ctx.block.public_functions)
}

fn user_callees(ctx: &BlockContext<'_>, statements: &[Spanned<Statement>]) -> Vec<String> {
    let mut callees = Vec::new();
    for_each_call(statements, &[], &mut |call, _| {
        if matches!(resolve_call(ctx, &call.function), Some(Callee::User(_))) {
            let name = lexeme(&call.function);
            if !callees.contains(&name) {
                callees.push(name);
            }
        }
    });
    callees
}

/// Iterative-friendly DFS cycle detection with explicit colors; each cycle
/// is reported once, anchored at the function that closes it.
fn report_cycles(
    ctx: &BlockContext<'_>,
    graph: &HashMap<String, Vec<String>>,
    diags: &mut Vec<GalecError>,
) {
    let mut finished: HashSet<String> = HashSet::new();
    for function in user_functions(ctx) {
        let name = lexeme(&function.name);
        let mut stack = Vec::new();
        dfs_cycles(ctx, graph, &name, &mut stack, &mut finished, diags);
    }
}

fn dfs_cycles(
    ctx: &BlockContext<'_>,
    graph: &HashMap<String, Vec<String>>,
    node: &str,
    stack: &mut Vec<String>,
    finished: &mut HashSet<String>,
    diags: &mut Vec<GalecError>,
) {
    if finished.contains(node) {
        return;
    }
    if let Some(position) = stack.iter().position(|n| n == node) {
        let mut cycle = stack[position..].to_vec();
        cycle.push(node.to_string());
        diags.push(GalecError::RecursiveCall {
            location: function_location(ctx, node),
            cycle: cycle.join(" -> "),
        });
        return;
    }
    stack.push(node.to_string());
    if let Some(callees) = graph.get(node) {
        for callee in callees {
            dfs_cycles(ctx, graph, callee, stack, finished, diags);
        }
    }
    stack.pop();
    finished.insert(node.to_string());
}

fn report_unreachable(
    ctx: &BlockContext<'_>,
    graph: &HashMap<String, Vec<String>>,
    diags: &mut Vec<GalecError>,
) {
    let mut reached: HashSet<String> = HashSet::new();
    let mut frontier = user_callees(ctx, &ctx.block.do_step.statements);
    while let Some(name) = frontier.pop() {
        if reached.insert(name.clone())
            && let Some(callees) = graph.get(&name)
        {
            frontier.extend(callees.iter().cloned());
        }
    }
    for function in user_functions(ctx) {
        let name = lexeme(&function.name);
        if !reached.contains(&name) {
            diags.push(GalecError::UnreachableFunction {
                location: function_location(ctx, &name),
                name,
            });
        }
    }
}

fn function_location(ctx: &BlockContext<'_>, name: &str) -> Location {
    Location::at(vec![
        PathSegment::Block(lexeme(&ctx.block.name)),
        PathSegment::Function(name.to_string()),
    ])
}

// ---------------------------------------------------------------------------
// Call collection
// ---------------------------------------------------------------------------

/// Visit every function call in a statement list, including calls nested in
/// expressions, with a statement-precise location.
fn for_each_call(
    statements: &[Spanned<Statement>],
    base: &[PathSegment],
    visit: &mut impl FnMut(&FunctionCall, Location),
) {
    for (index, statement) in statements.iter().enumerate() {
        let mut path = base.to_vec();
        path.push(PathSegment::Statement(index));
        statement_calls(&statement.node, &path, visit);
    }
}

fn statement_calls(
    statement: &Statement,
    path: &[PathSegment],
    visit: &mut impl FnMut(&FunctionCall, Location),
) {
    let here = || Location::at(path.to_vec());
    match statement {
        Statement::Assignment { target, value } => {
            reference_calls(target, path, visit);
            expression_calls(value, path, visit);
        }
        Statement::MultiAssignment { targets, call } => {
            for target in targets {
                reference_calls(target, path, visit);
            }
            visit(call, here());
            for argument in &call.arguments {
                expression_calls(argument, path, visit);
            }
        }
        Statement::Call(call) => {
            visit(call, here());
            for argument in &call.arguments {
                expression_calls(argument, path, visit);
            }
        }
        Statement::If(if_statement) => {
            for branch in &if_statement.branches {
                condition_calls(&branch.condition, path, visit);
                for_each_call(&branch.body, path, visit);
            }
            if let Some(else_body) = &if_statement.else_body {
                for_each_call(else_body, path, visit);
            }
        }
        Statement::For(for_loop) => {
            bound_calls(for_loop, path, visit);
            for_each_call(&for_loop.body, path, visit);
        }
        Statement::Limit(targets) => {
            for target in targets {
                if let LimitTarget::Reference(reference) = target {
                    reference_calls(reference, path, visit);
                }
            }
        }
        Statement::Signal(_) => {}
    }
}

fn bound_calls(
    for_loop: &crate::ast::ForLoop,
    path: &[PathSegment],
    visit: &mut impl FnMut(&FunctionCall, Location),
) {
    expression_calls(&for_loop.start, path, visit);
    if let Some(step) = &for_loop.step {
        expression_calls(step, path, visit);
    }
    expression_calls(&for_loop.stop, path, visit);
}

fn condition_calls(
    condition: &Condition,
    path: &[PathSegment],
    visit: &mut impl FnMut(&FunctionCall, Location),
) {
    match condition {
        Condition::Expression(expression) => expression_calls(expression, path, visit),
        Condition::SignalCheck(check) => {
            if let Some(fallback) = &check.fallback {
                expression_calls(fallback, path, visit);
            }
        }
    }
}

/// Calls inside reference subscripts. Such calls are invalid GALEC (dims
/// rejects non-builtin/signaling calls in static positions), but walking
/// them keeps this analysis' reachability and Startup checks precise
/// instead of misdiagnosing (e.g. EG027 for a function only called from a
/// subscript).
fn reference_calls(
    reference: &Reference,
    path: &[PathSegment],
    visit: &mut impl FnMut(&FunctionCall, Location),
) {
    for part in reference_parts(reference) {
        for subscript in &part.subscripts {
            expression_calls(subscript, path, visit);
        }
    }
}

fn expression_calls(
    expression: &Expression,
    path: &[PathSegment],
    visit: &mut impl FnMut(&FunctionCall, Location),
) {
    match expression {
        Expression::Bool(_) | Expression::Integer(_) | Expression::Real(_) => {}
        Expression::Ref(reference) | Expression::Neg(reference) => {
            reference_calls(reference, path, visit);
        }
        Expression::Size { array, dimension } => {
            reference_calls(array, path, visit);
            expression_calls(dimension, path, visit);
        }
        Expression::Call(call) => {
            visit(call, Location::at(path.to_vec()));
            for argument in &call.arguments {
                expression_calls(argument, path, visit);
            }
        }
        Expression::Paren(inner) | Expression::Not(inner) => expression_calls(inner, path, visit),
        Expression::If(if_expression) => {
            for (condition, value) in &if_expression.branches {
                expression_calls(condition, path, visit);
                expression_calls(value, path, visit);
            }
            expression_calls(&if_expression.else_value, path, visit);
        }
        Expression::Array(elements) => {
            for element in elements {
                expression_calls(element, path, visit);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            expression_calls(lhs, path, visit);
            expression_calls(rhs, path, visit);
        }
    }
}
