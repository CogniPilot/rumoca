//! Eliminate loop-local prefixes only under exact dominance and use proofs.

use super::*;

/// Replace a perfect inner element loop with its tensor-native slice write.
///
/// `for j in r loop A[i,j] := B[i,j]; end for` owns exactly the same ordered
/// coordinate map as `A[i,r] := B[i,r]`. Keeping the enclosing loop intact
/// preserves source order while removing a nested transition the DAE would
/// otherwise have to scalarize.
pub(super) fn compact_perfect_inner_element_loops(
    statements: &[rumoca_core::Statement],
) -> Vec<rumoca_core::Statement> {
    compact_element_loops(statements, false)
}

fn compact_element_loops(
    statements: &[rumoca_core::Statement],
    nested: bool,
) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .map(|statement| match statement {
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => rumoca_core::Statement::For {
                indices: indices.clone(),
                equations: compact_element_loops(equations, true),
                span: *span,
            },
            _ => statement.clone(),
        })
        .map(|statement| {
            if nested {
                compact_one_perfect_inner_loop(statement)
            } else {
                statement
            }
        })
        .collect()
}

/// Inline loop-local prefix definitions into one trailing statement.
///
/// The local must be unobserved after the loop, absent from outputs, and not
/// reassigned by the trailing statement. The prefix definition dominates each
/// substituted use, so a conditional or nested loop can retain one compact
/// owner without manufacturing a scalar scratch transition.
pub(super) fn inline_loop_local_prefixes(
    statements: &[rumoca_core::Statement],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    inline_loop_local_prefixes_in_scope(statements, &[], locals, outputs)
}

fn inline_loop_local_prefixes_in_scope(
    statements: &[rumoca_core::Statement],
    enclosing_suffix: &[rumoca_core::Statement],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    statements
        .iter()
        .enumerate()
        .map(|(ordinal, statement)| {
            let mut suffix = statements[ordinal + 1..].to_vec();
            suffix.extend_from_slice(enclosing_suffix);
            inline_one_loop_local_prefix(statement, &suffix, locals, outputs)
        })
        .collect()
}

fn inline_one_loop_local_prefix(
    statement: &rumoca_core::Statement,
    suffix: &[rumoca_core::Statement],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
) -> rumoca_core::Statement {
    if let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    {
        let cond_blocks = cond_blocks
            .iter()
            .map(|block| rumoca_core::StatementBlock {
                cond: block.cond.clone(),
                stmts: inline_loop_local_prefixes_in_scope(&block.stmts, suffix, locals, outputs),
            })
            .collect();
        let else_block = else_block.as_ref().map(|statements| {
            inline_loop_local_prefixes_in_scope(statements, suffix, locals, outputs)
        });
        return rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span: *span,
        };
    }
    let rumoca_core::Statement::For {
        indices,
        equations,
        span,
    } = statement
    else {
        return statement.clone();
    };
    let equations = inline_loop_local_prefixes_in_scope(equations, suffix, locals, outputs);
    let equations = inline_dominated_loop_locals(equations, suffix, locals, outputs);
    let Some((trailing, prefix)) = equations.split_last() else {
        return statement.clone();
    };
    if prefix.is_empty() {
        return statement_with_equations(indices, equations, *span);
    }
    let Some(substitutions) =
        loop_local_substitutions(prefix, suffix, std::slice::from_ref(trailing), outputs)
    else {
        return statement_with_equations(indices, equations, *span);
    };
    let mut rewriter = LocalSubstitution {
        values: &substitutions,
    };
    rumoca_core::Statement::For {
        indices: indices.clone(),
        equations: vec![rewriter.rewrite_statement(trailing)],
        span: *span,
    }
}

fn inline_dominated_loop_locals(
    mut statements: Vec<rumoca_core::Statement>,
    outer_suffix: &[rumoca_core::Statement],
    locals: &HashSet<VarName>,
    outputs: &HashSet<VarName>,
) -> Vec<rumoca_core::Statement> {
    let mut ordinal = 0usize;
    while ordinal < statements.len() {
        let Some((target, value)) = scalar_local_definition(&statements[ordinal]) else {
            ordinal += 1;
            continue;
        };
        let suffix = &statements[ordinal + 1..];
        if !locals.contains(&target)
            || outputs.contains(&target)
            || statements_read_name(outer_suffix, &target)
            || statements_read_nonrewritable_name(suffix, &target)
            || statements_partially_assign_name(suffix, &target)
            || expression_reads_name(&value, &target)
            || expression_dependencies_change(&value, suffix)
        {
            ordinal += 1;
            continue;
        }
        let substitutions = HashMap::from([(target, value)]);
        let mut rewriter = LocalSubstitution {
            values: &substitutions,
        };
        let rewritten = rewriter.rewrite_statements(suffix);
        statements.splice(ordinal.., rewritten);
    }
    statements
}

fn scalar_local_definition(statement: &rumoca_core::Statement) -> Option<(VarName, Expression)> {
    let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
        return None;
    };
    let [part] = comp.parts() else {
        return None;
    };
    part.subs
        .is_empty()
        .then(|| (VarName::new(&part.ident), value.clone()))
}

fn expression_dependencies_change(
    expression: &Expression,
    statements: &[rumoca_core::Statement],
) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references
        .iter()
        .any(|reference| statements_assign_name(statements, reference))
}

fn loop_local_substitutions(
    prefix: &[rumoca_core::Statement],
    suffix: &[rumoca_core::Statement],
    nested_body: &[rumoca_core::Statement],
    outputs: &HashSet<VarName>,
) -> Option<HashMap<VarName, Expression>> {
    let mut substitutions = HashMap::new();
    for statement in prefix {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return None;
        };
        let [part] = comp.parts() else {
            return None;
        };
        let target = VarName::new(&part.ident);
        if !part.subs.is_empty()
            || outputs.contains(&target)
            || statements_read_name(suffix, &target)
            || statements_read_nonrewritable_name(nested_body, &target)
            || statements_partially_assign_name(nested_body, &target)
            || expression_reads_name(value, &target)
        {
            return None;
        }
        let mut rewriter = LocalSubstitution {
            values: &substitutions,
        };
        substitutions.insert(target, rewriter.rewrite_expression(value));
    }
    Some(substitutions)
}

pub(super) fn statement_with_equations(
    indices: &[rumoca_core::ForIndex],
    equations: Vec<rumoca_core::Statement>,
    span: Span,
) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: indices.to_vec(),
        equations,
        span,
    }
}

pub(super) struct LocalSubstitution<'a> {
    pub(super) values: &'a HashMap<VarName, Expression>,
}

impl ExpressionRewriter for LocalSubstitution<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if subscripts.is_empty()
            && let Some(value) = self.values.get(name.var_name())
        {
            return value.clone();
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

impl StatementRewriter for LocalSubstitution<'_> {}

pub(super) fn statements_read_name(statements: &[rumoca_core::Statement], name: &VarName) -> bool {
    struct Finder<'a> {
        name: &'a VarName,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= reference_reads_name(reference, self.name);
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for Finder<'_> {}

    let mut finder = Finder { name, found: false };
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut finder, statement);
        if finder.found {
            return true;
        }
    }
    false
}

fn reference_reads_name(reference: &Reference, name: &VarName) -> bool {
    let reference = reference.var_name().as_str();
    let name = name.as_str();
    reference == name
        || reference
            .strip_prefix(name)
            .is_some_and(|suffix| suffix.starts_with('.'))
}

pub(super) fn statements_read_nonrewritable_name(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> bool {
    struct Finder<'a> {
        name: &'a VarName,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= reference_reads_name(reference, self.name)
                && (reference.var_name() != self.name || !subscripts.is_empty());
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for Finder<'_> {}

    let mut finder = Finder { name, found: false };
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut finder, statement);
    }
    finder.found
}

pub(super) fn expression_reads_name(expression: &Expression, name: &VarName) -> bool {
    struct Finder<'a> {
        name: &'a VarName,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= reference_reads_name(reference, self.name);
            self.walk_var_ref(reference, subscripts);
        }
    }
    let mut finder = Finder { name, found: false };
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expression);
    finder.found
}

fn statements_assign_name(statements: &[rumoca_core::Statement], name: &VarName) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Assignment { comp, .. } => comp.to_var_name() == *name,
        rumoca_core::Statement::For { equations, .. } => statements_assign_name(equations, name),
        rumoca_core::Statement::While { block, .. } => statements_assign_name(&block.stmts, name),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_assign_name(&block.stmts, name))
                || else_block
                    .as_deref()
                    .is_some_and(|block| statements_assign_name(block, name))
        }
        rumoca_core::Statement::When { blocks, .. } => blocks
            .iter()
            .any(|block| statements_assign_name(&block.stmts, name)),
        _ => false,
    })
}

fn component_targets_name(component: &rumoca_core::ComponentReference, name: &VarName) -> bool {
    matches!(component.parts(), [part] if part.ident == name.as_str())
}

pub(super) fn statements_partially_assign_name(
    statements: &[rumoca_core::Statement],
    name: &VarName,
) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Assignment { comp, .. } => {
            component_targets_name(comp, name) && scalar_assignment_target(comp).is_none()
        }
        rumoca_core::Statement::For { equations, .. } => {
            statements_partially_assign_name(equations, name)
        }
        rumoca_core::Statement::While { block, .. } => {
            statements_partially_assign_name(&block.stmts, name)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| statements_partially_assign_name(&block.stmts, name))
                || else_block
                    .as_deref()
                    .is_some_and(|branch| statements_partially_assign_name(branch, name))
        }
        _ => false,
    })
}

fn compact_one_perfect_inner_loop(statement: rumoca_core::Statement) -> rumoca_core::Statement {
    let rumoca_core::Statement::For {
        indices,
        equations,
        span: _,
    } = &statement
    else {
        return statement;
    };
    let [index] = indices.as_slice() else {
        return statement;
    };
    let [
        rumoca_core::Statement::Assignment {
            comp,
            value,
            span: assignment_span,
        },
    ] = equations.as_slice()
    else {
        return statement;
    };
    let mut replacement = BinderSliceReplacement {
        binder: &index.ident,
        range: &index.range,
        replacements: 0,
        unsupported: false,
    };
    let parts = comp
        .parts()
        .iter()
        .cloned()
        .map(|mut part| {
            part.subs = replacement.rewrite_subscripts(&part.subs);
            part
        })
        .collect();
    let Ok(comp) = comp.with_replaced_parts(parts) else {
        return statement;
    };
    let target_replacements = replacement.replacements;
    let value = replacement.rewrite_expression(value);
    if replacement.unsupported
        || target_replacements != 1
        || replacement.replacements == target_replacements
    {
        return statement;
    }
    rumoca_core::Statement::Assignment {
        comp,
        value,
        span: *assignment_span,
    }
}

struct BinderSliceReplacement<'a> {
    binder: &'a str,
    range: &'a Expression,
    replacements: usize,
    unsupported: bool,
}

impl ExpressionRewriter for BinderSliceReplacement<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        if matches!(
            expression,
            Expression::VarRef { name, subscripts, .. }
                if name.as_str() == self.binder && subscripts.is_empty()
        ) {
            self.unsupported = true;
        }
        self.walk_expression(expression)
    }

    fn rewrite_subscript(&mut self, subscript: &Subscript) -> Subscript {
        if let Subscript::Expr { expr, span } = subscript
            && matches!(
                expr.as_ref(),
                Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == self.binder && subscripts.is_empty()
            )
        {
            self.replacements += 1;
            return Subscript::Expr {
                expr: Box::new(self.range.clone()),
                span: *span,
            };
        }
        match subscript {
            Subscript::Index { value, span } => Subscript::Index {
                value: *value,
                span: *span,
            },
            Subscript::Colon { span } => Subscript::Colon { span: *span },
            Subscript::Expr { expr, span } => Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)),
                span: *span,
            },
        }
    }
}

pub(super) fn first_dependent_loop_range(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<Span>, ToDaeError> {
    for statement in statements {
        let found = match statement {
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                let range_span = first_unsettled_range(indices, static_integers, shapes)?;
                range_span.or(first_dependent_loop_range(
                    equations,
                    static_integers,
                    shapes,
                )?)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => first_dependent_block_range(cond_blocks, static_integers, shapes)?.or(else_block
                .as_ref()
                .map_or(Ok(None), |block| {
                    first_dependent_loop_range(block, static_integers, shapes)
                })?),
            rumoca_core::Statement::While { block, .. } => {
                first_dependent_loop_range(&block.stmts, static_integers, shapes)?
            }
            rumoca_core::Statement::When { blocks, .. } => {
                first_dependent_block_range(blocks, static_integers, shapes)?
            }
            _ => None,
        };
        if found.is_some() {
            return Ok(found);
        }
    }
    Ok(None)
}

fn first_unsettled_range(
    indices: &[rumoca_core::ForIndex],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<Span>, ToDaeError> {
    for index in indices {
        if static_function_range(&index.range, static_integers, shapes)?.is_none() {
            return expression_span(&index.range).map(Some);
        }
    }
    Ok(None)
}

fn first_dependent_block_range(
    blocks: &[rumoca_core::StatementBlock],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<Span>, ToDaeError> {
    for block in blocks {
        if let Some(span) = first_dependent_loop_range(&block.stmts, static_integers, shapes)? {
            return Ok(Some(span));
        }
    }
    Ok(None)
}
