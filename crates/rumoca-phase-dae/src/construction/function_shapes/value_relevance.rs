//! Which function inputs a specialization must key on by *value*.
//!
//! MLS §12.2 lets a function's declared array dimensions be written over its
//! inputs, and MLS §4.4.2 restricts a dimension to a scalar evaluable
//! Integer/enumeration/Boolean expression. Only an input whose *value* one of
//! those translation-time constructs actually reads can change a
//! specialization's shapes. A function assertion is a checked call-scoped
//! action when the shape key does not independently prove it; assertion-only
//! values therefore do not multiply shape specializations. Only shape-driving
//! inputs belong in
//! [`FunctionSpecializationKey::input_values`].
//!
//! Keying on any more than that is not merely wasteful. `g(1) … g(10)` would
//! mint ten byte-identical DAE functions that later stages resolve by name, and
//! `f(n) = if n <= 0 then 0 else 1 + f(n - 1)` would never reach the memoized
//! fixed point that makes the analysis terminate, because every activation
//! would carry a new key.
//!
//! Reading too *many* names is safe; reading too few is also safe, in the sense
//! that matters: an input whose value a dimension reads but whose value the key
//! drops leaves that dimension unproven, and `evaluate_shape_integer` rejects it
//! by name (`ED019`) instead of guessing an extent. Both errors are therefore
//! bounded, and this analysis errs toward reading more.

use super::*;

/// Input positions each callable must key a specialization on by value.
pub(super) struct ValueReadInputs {
    masks: HashMap<VarName, Vec<bool>>,
}

impl ValueReadInputs {
    pub(super) fn analyze(
        flat: &flat::Model,
        dimension_typed: &HashMap<VarName, Vec<bool>>,
    ) -> Self {
        let mut reads: HashMap<VarName, HashSet<VarName>> = flat
            .functions
            .iter()
            .map(|(name, function)| (name.clone(), declared_value_reads(function)))
            .collect();
        let mut masks = build_masks(flat, dimension_typed, &reads);
        // A call argument is a value read exactly when the callee keys on that
        // position, so the relation is closed under the static call graph. Each
        // round can only add names, and the name set of one function is finite,
        // so the loop terminates; the function count bounds the rounds because
        // one round propagates at least one edge of any chain that is still
        // growing.
        for _ in 0..=flat.functions.len() {
            let mut grew = false;
            for (name, function) in &flat.functions {
                let added = keyed_call_argument_names(function, &masks);
                let owned = reads.get_mut(name).expect("every function seeds its reads");
                grew |= added
                    .into_iter()
                    .fold(false, |grew, read| grew | owned.insert(read));
            }
            if !grew {
                break;
            }
            masks = build_masks(flat, dimension_typed, &reads);
        }
        Self { masks }
    }

    /// Whether the specialization key carries input `ordinal` by value.
    pub(super) fn reads_value(&self, function: &VarName, ordinal: usize) -> bool {
        self.masks
            .get(function)
            .and_then(|mask| mask.get(ordinal))
            .copied()
            .unwrap_or(false)
    }
}

/// Names `function` passes to a call position the callee keys on by value.
fn keyed_call_argument_names(
    function: &rumoca_core::Function,
    masks: &HashMap<VarName, Vec<bool>>,
) -> HashSet<VarName> {
    let mut names = HashSet::new();
    for_each_call(function, &mut |callee, arguments| {
        let Some(mask) = masks.get(callee) else {
            return;
        };
        for (argument, _) in arguments.iter().zip(mask).filter(|(_, keyed)| **keyed) {
            collect_read_names(argument, &mut names);
        }
    });
    names
}

fn build_masks(
    flat: &flat::Model,
    dimension_typed: &HashMap<VarName, Vec<bool>>,
    reads: &HashMap<VarName, HashSet<VarName>>,
) -> HashMap<VarName, Vec<bool>> {
    flat.functions
        .iter()
        .map(|(name, function)| {
            let typed = dimension_typed.get(name);
            let read = reads.get(name);
            let mask = function
                .inputs
                .iter()
                .enumerate()
                .map(|(ordinal, input)| {
                    typed.is_some_and(|typed| typed.get(ordinal).copied().unwrap_or(false))
                        && read.is_some_and(|read| read.contains(&VarName::new(&input.name)))
                })
                .collect();
            (name.clone(), mask)
        })
        .collect()
}

/// Names whose value a translation-time construct of `function` reads.
///
/// The translation-time constructs are the ones that reach
/// `evaluate_shape_integer` / `ShapeEnvironment::proven_extent`: a declared
/// dimension (MLS §12.2), a compact range bound (MLS §10.4.1, §11.2.2), and a
/// `zeros`/`ones`/`fill` extent (MLS §10.3).
fn declared_value_reads(function: &rumoca_core::Function) -> HashSet<VarName> {
    let mut reads = HashSet::new();
    for value in function
        .inputs
        .iter()
        .chain(&function.outputs)
        .chain(&function.locals)
    {
        for subscript in &value.shape_expr {
            if let Subscript::Expr { expr, .. } = subscript {
                collect_read_names(expr, &mut reads);
            }
        }
    }
    for_each_expression(function, &mut |expression| match expression {
        Expression::Range {
            start, step, end, ..
        } => {
            collect_read_names(start, &mut reads);
            if let Some(step) = step {
                collect_read_names(step, &mut reads);
            }
            collect_read_names(end, &mut reads);
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Zeros | BuiltinFunction::Ones | BuiltinFunction::Fill,
            args,
            ..
        } => {
            for argument in args {
                collect_read_names(argument, &mut reads);
            }
        }
        _ => {}
    });
    // MLS §12.4.4: a local's declaration equation settles the value a later
    // dimension reads, so a read of the local is a read of everything that
    // equation is written over.
    for _ in 0..=function.locals.len() {
        let mut grew = false;
        for local in &function.locals {
            let Some(default) = &local.default else {
                continue;
            };
            if !reads.contains(&VarName::new(&local.name)) {
                continue;
            }
            let mut added = HashSet::new();
            collect_read_names(default, &mut added);
            for read in added {
                grew |= reads.insert(read);
            }
        }
        if !grew {
            break;
        }
    }
    reads
}

/// Every free reference name in `expression`.
///
/// This deliberately does not model which operand position an extent rule
/// would actually fold: `evaluate_shape_integer` ends in a whole-expression
/// `proven_extent` fold, so any name the expression mentions can decide the
/// result.
fn collect_read_names(expression: &Expression, reads: &mut HashSet<VarName>) {
    if let Expression::VarRef { name, .. } = expression {
        reads.insert(name.var_name().clone());
    }
    for child in expression_children(expression) {
        collect_read_names(child, reads);
    }
}

fn for_each_call(
    function: &rumoca_core::Function,
    visit: &mut impl FnMut(&VarName, &[Expression]),
) {
    for_each_expression(function, &mut |expression| {
        if let Expression::FunctionCall { name, args, .. } = expression {
            visit(name.var_name(), args);
        }
    });
    for_each_statement(&function.body, &mut |statement| {
        if let rumoca_core::Statement::FunctionCall { comp, args, .. } = statement {
            visit(&comp.to_var_name(), args);
        }
    });
}

/// Visit every expression of `function`, including nested sub-expressions.
fn for_each_expression(function: &rumoca_core::Function, visit: &mut impl FnMut(&Expression)) {
    for value in function
        .inputs
        .iter()
        .chain(&function.outputs)
        .chain(&function.locals)
    {
        if let Some(default) = &value.default {
            visit_expression_tree(default, visit);
        }
    }
    for_each_statement(&function.body, &mut |statement| {
        for expression in statement_expressions(statement) {
            visit_expression_tree(expression, visit);
        }
    });
}

/// Root expressions owned by a Flat function declaration and all nested flow
/// statements. Expression-tree recursion remains the responsibility of the
/// consuming analysis, matching `all_model_expressions`.
pub(in crate::construction) fn function_expressions(
    function: &rumoca_core::Function,
) -> Vec<&Expression> {
    let mut expressions = Vec::new();
    for parameter in function
        .inputs
        .iter()
        .chain(&function.outputs)
        .chain(&function.locals)
    {
        expressions.extend(
            [
                parameter.default.as_ref(),
                parameter.min.as_ref(),
                parameter.max.as_ref(),
            ]
            .into_iter()
            .flatten(),
        );
        expressions.extend(parameter.shape_expr.iter().filter_map(|subscript| {
            let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
                return None;
            };
            Some(expr.as_ref())
        }));
    }
    collect_statement_expression_roots(&function.body, &mut expressions);
    expressions
}

fn collect_statement_expression_roots<'function>(
    statements: &'function [rumoca_core::Statement],
    expressions: &mut Vec<&'function Expression>,
) {
    for statement in statements {
        expressions.extend(statement_expressions(statement));
        match statement {
            rumoca_core::Statement::For { equations, .. } => {
                collect_statement_expression_roots(equations, expressions);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_statement_expression_roots(&block.stmts, expressions);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_statement_expression_roots(&block.stmts, expressions);
                }
                collect_statement_expression_roots(
                    else_block.as_deref().unwrap_or_default(),
                    expressions,
                );
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_statement_expression_roots(&block.stmts, expressions);
                }
            }
            rumoca_core::Statement::Assignment { .. }
            | rumoca_core::Statement::FunctionCall { .. }
            | rumoca_core::Statement::Reinit { .. }
            | rumoca_core::Statement::Assert { .. }
            | rumoca_core::Statement::Empty { .. }
            | rumoca_core::Statement::Return { .. }
            | rumoca_core::Statement::Break { .. } => {}
        }
    }
}

fn visit_expression_tree(expression: &Expression, visit: &mut impl FnMut(&Expression)) {
    visit(expression);
    for child in expression_children(expression) {
        visit_expression_tree(child, visit);
    }
}

/// Visit every statement of `statements`, including nested bodies.
fn for_each_statement(
    statements: &[rumoca_core::Statement],
    visit: &mut impl FnMut(&rumoca_core::Statement),
) {
    for statement in statements {
        visit(statement);
        match statement {
            rumoca_core::Statement::For { equations, .. } => for_each_statement(equations, visit),
            rumoca_core::Statement::While { block, .. } => for_each_statement(&block.stmts, visit),
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    for_each_statement(&block.stmts, visit);
                }
                for_each_statement(else_block.as_deref().unwrap_or_default(), visit);
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    for_each_statement(&block.stmts, visit);
                }
            }
            rumoca_core::Statement::Assignment { .. }
            | rumoca_core::Statement::FunctionCall { .. }
            | rumoca_core::Statement::Reinit { .. }
            | rumoca_core::Statement::Assert { .. }
            | rumoca_core::Statement::Empty { .. }
            | rumoca_core::Statement::Return { .. }
            | rumoca_core::Statement::Break { .. } => {}
        }
    }
}

/// The expressions one statement owns directly, excluding nested bodies.
fn statement_expressions(statement: &rumoca_core::Statement) -> Vec<&Expression> {
    let mut expressions = Vec::new();
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => {
            expressions.extend(component_subscript_expressions(comp));
            expressions.push(value);
        }
        rumoca_core::Statement::Reinit {
            variable, value, ..
        } => {
            expressions.extend(component_subscript_expressions(variable));
            expressions.push(value);
        }
        rumoca_core::Statement::For { indices, .. } => {
            expressions.extend(indices.iter().map(|index| &index.range));
        }
        rumoca_core::Statement::While { block, .. } => expressions.push(&block.cond),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block: _,
            ..
        } => expressions.extend(cond_blocks.iter().map(|block| &block.cond)),
        rumoca_core::Statement::When { blocks, .. } => {
            expressions.extend(blocks.iter().map(|block| &block.cond));
        }
        rumoca_core::Statement::FunctionCall { comp, args, .. } => {
            expressions.extend(component_subscript_expressions(comp));
            expressions.extend(args);
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            expressions.push(condition);
            expressions.push(message);
            expressions.extend(level.as_deref());
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
    expressions
}

fn component_subscript_expressions(
    component: &rumoca_core::ComponentReference,
) -> Vec<&Expression> {
    component
        .parts()
        .iter()
        .flat_map(|part| part.subs.iter())
        .filter_map(|subscript| match subscript {
            Subscript::Expr { expr, .. } => Some(expr.as_ref()),
            Subscript::Index { .. } | Subscript::Colon { .. } => None,
        })
        .collect()
}
