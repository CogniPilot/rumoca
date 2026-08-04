use super::*;
use rumoca_core::{
    BuiltinFunction, ExpressionRewriter, Literal, OpBinary, OpUnary, Reference, StatementRewriter,
    Subscript,
};

/// Evaluate a branch condition that the specialization settles to a constant.
///
/// After a binder is substituted, a guard like `row == column` becomes a
/// comparison of literals; resolving it to the taken branch is semantically
/// exact and keeps the unrolled body free of the spurious runtime conditionals
/// that would otherwise fork a value's SSA definition. A genuinely runtime guard
/// (`value <= tolerance`, `ok`) yields `None` and stays a conditional.
fn static_condition(
    expression: &Expression,
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Option<bool>, ToDaeError> {
    match expression {
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Ok(Some(*value)),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => Ok(static_condition(rhs, static_integers, shapes)?.map(|value| !value)),
        Expression::Binary {
            op: op @ (OpBinary::And | OpBinary::Or),
            lhs,
            rhs,
            ..
        } => {
            let lhs = static_condition(lhs, static_integers, shapes)?;
            let rhs = static_condition(rhs, static_integers, shapes)?;
            Ok(match op {
                OpBinary::And => match (lhs, rhs) {
                    (Some(false), _) | (_, Some(false)) => Some(false),
                    (Some(true), Some(true)) => Some(true),
                    _ => None,
                },
                _ => match (lhs, rhs) {
                    (Some(true), _) | (_, Some(true)) => Some(true),
                    (Some(false), Some(false)) => Some(false),
                    _ => None,
                },
            })
        }
        Expression::Binary {
            op:
                op @ (OpBinary::Eq
                | OpBinary::Neq
                | OpBinary::Lt
                | OpBinary::Le
                | OpBinary::Gt
                | OpBinary::Ge),
            lhs,
            rhs,
            ..
        } => {
            let (Some(lhs), Some(rhs)) = (
                static_shape_integer_expression(lhs, static_integers, shapes)?,
                static_shape_integer_expression(rhs, static_integers, shapes)?,
            ) else {
                return Ok(None);
            };
            Ok(Some(match op {
                OpBinary::Eq => lhs == rhs,
                OpBinary::Neq => lhs != rhs,
                OpBinary::Lt => lhs < rhs,
                OpBinary::Le => lhs <= rhs,
                OpBinary::Gt => lhs > rhs,
                _ => lhs >= rhs,
            }))
        }
        _ => Ok(None),
    }
}

/// Upper bound on the scalar iterations one function-body unrolling may emit.
///
/// Data-dependent loop nests (e.g. Cholesky's `for column in 1:row`) have no
/// rectangular structured domain, so they are lowered by unrolling the proven
/// enclosing ranges. Control-sized kernels stay tiny; this cap only rejects a
/// pathological proven extent before it exhausts memory.
const UNROLL_ITERATION_LIMIT: usize = 1 << 16;

/// Fully unroll every loop in a function body when it contains a data-dependent
/// loop bound, so the checked lowering sees only statically bounded work.
///
/// A range like `1:row` reads an enclosing binder and so has no rectangular
/// [`StructuredIndexDomain`]. When any loop in the body is written that way, the
/// whole body is unrolled over its proven ranges: each enclosing binder is
/// substituted with its settled value, and an integer local whose value then
/// becomes constant (Cholesky's `solveRow := n + 1 - reverseRow`) is propagated,
/// so the triangular nest becomes straight-line scalar statements the definedness
/// and shape proofs admit directly. Bodies whose loops are all statically
/// rectangular are returned unchanged so the compact structured path keeps
/// handling them.
pub(super) fn unroll_data_dependent_function_loops(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    function_name: &str,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    if !contains_data_dependent_loop(statements, static_integers, shapes)? {
        return Ok(statements.to_vec());
    }
    let mut unrolled = Vec::new();
    let mut emitted = 0usize;
    unroll_sequence(
        statements,
        static_integers,
        shapes,
        function_name,
        &mut unrolled,
        &mut emitted,
    )?;
    // Straight-line lowering needs literal array extents: a `zeros(size(A, 1), …)`
    // that the structured path resolves through the shape environment must carry
    // its settled extent as a literal here, since no loop domain remains to hold it.
    fold_size_extents(&unrolled, static_integers, shapes)
}

/// Replace every `size(x, k)` (and `size(x)`) that the specialization settles
/// with its literal Integer value.
struct SizeExtentFold<'a> {
    static_integers: &'a HashMap<VarName, i64>,
    shapes: &'a ShapeEnvironment,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for SizeExtentFold<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Size,
            span,
            ..
        } = expression
        {
            match static_shape_integer_expression(expression, self.static_integers, self.shapes) {
                Ok(Some(value)) => {
                    return Expression::Literal {
                        value: Literal::Integer(value),
                        span: *span,
                    };
                }
                Ok(None) => {}
                Err(error) => {
                    self.error.get_or_insert(error);
                }
            }
        }
        self.walk_expression(expression)
    }
}

impl StatementRewriter for SizeExtentFold<'_> {}

fn fold_size_extents(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let mut fold = SizeExtentFold {
        static_integers,
        shapes,
        error: None,
    };
    let folded = fold.rewrite_statements(statements);
    match fold.error {
        Some(error) => Err(error),
        None => Ok(folded),
    }
}

/// Substitute one loop binder (or a proven integer local) with an Integer
/// literal throughout a body.
///
/// The binder is a fresh scalar of its loop (MLS §11.2.2), so an unsubscripted
/// reference to it is the loop coordinate. A nested loop that rebinds the same
/// name shadows it, so its body is left untouched while its range — evaluated in
/// the outer scope — is still rewritten.
struct NameSubstitution<'a> {
    name: &'a str,
    value: i64,
}

impl ExpressionRewriter for NameSubstitution<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if subscripts.is_empty() && name.var_name().as_str() == self.name {
            return Expression::Literal {
                value: rumoca_core::Literal::Integer(self.value),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }
}

impl StatementRewriter for NameSubstitution<'_> {
    fn rewrite_statement(&mut self, statement: &rumoca_core::Statement) -> rumoca_core::Statement {
        if let rumoca_core::Statement::For {
            indices,
            equations,
            span,
        } = statement
            && indices.iter().any(|index| index.ident.as_str() == self.name)
        {
            return rumoca_core::Statement::For {
                indices: self.rewrite_for_indices(indices),
                equations: equations.clone(),
                span: *span,
            };
        }
        self.walk_statement(statement)
    }
}

fn substitute_name(
    statements: &[rumoca_core::Statement],
    name: &str,
    value: i64,
) -> Vec<rumoca_core::Statement> {
    NameSubstitution { name, value }.rewrite_statements(statements)
}

/// Whether any loop range in the body reads a value that is not statically
/// evaluable from the specialization alone — the signature of a bound written
/// over an enclosing loop binder or a computed integer index.
fn contains_data_dependent_loop(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<bool, ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                for index in indices {
                    if static_function_range(&index.range, static_integers, shapes)?.is_none() {
                        return Ok(true);
                    }
                }
                if contains_data_dependent_loop(equations, static_integers, shapes)? {
                    return Ok(true);
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    if contains_data_dependent_loop(&block.stmts, static_integers, shapes)? {
                        return Ok(true);
                    }
                }
                if let Some(else_block) = else_block
                    && contains_data_dependent_loop(else_block, static_integers, shapes)?
                {
                    return Ok(true);
                }
            }
            rumoca_core::Statement::While { block, .. } => {
                if contains_data_dependent_loop(&block.stmts, static_integers, shapes)? {
                    return Ok(true);
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    if contains_data_dependent_loop(&block.stmts, static_integers, shapes)? {
                        return Ok(true);
                    }
                }
            }
            _ => {}
        }
    }
    Ok(false)
}

fn unroll_sequence(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    function_name: &str,
    out: &mut Vec<rumoca_core::Statement>,
    emitted: &mut usize,
) -> Result<(), ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => unroll_for(
                indices,
                equations,
                *span,
                static_integers,
                shapes,
                function_name,
                out,
                emitted,
            )?,
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => {
                // Resolve constant guards: drop statically-false branches, and
                // stop at the first statically-true branch (it, or the original
                // else once every earlier guard is false, is the taken sequence).
                let mut runtime_blocks: Vec<rumoca_core::StatementBlock> = Vec::new();
                let mut taken: Option<&[rumoca_core::Statement]> = None;
                let mut fallback: Option<&[rumoca_core::Statement]> =
                    else_block.as_deref();
                for block in cond_blocks {
                    match static_condition(&block.cond, static_integers, shapes)? {
                        Some(false) => continue,
                        Some(true) => {
                            if runtime_blocks.is_empty() {
                                taken = Some(block.stmts.as_slice());
                            } else {
                                // Earlier runtime guards may still fire; this
                                // always-true branch is their else.
                                fallback = Some(block.stmts.as_slice());
                            }
                            break;
                        }
                        None => runtime_blocks.push(rumoca_core::StatementBlock {
                            cond: block.cond.clone(),
                            stmts: block.stmts.clone(),
                        }),
                    }
                }
                if let Some(taken) = taken {
                    unroll_sequence(
                        taken,
                        static_integers,
                        shapes,
                        function_name,
                        out,
                        emitted,
                    )?;
                } else if runtime_blocks.is_empty() {
                    if let Some(fallback) = fallback {
                        unroll_sequence(
                            fallback,
                            static_integers,
                            shapes,
                            function_name,
                            out,
                            emitted,
                        )?;
                    }
                } else {
                    let mut rewritten_blocks = Vec::with_capacity(runtime_blocks.len());
                    for block in &runtime_blocks {
                        rewritten_blocks.push(rumoca_core::StatementBlock {
                            cond: block.cond.clone(),
                            stmts: unroll_block(
                                &block.stmts,
                                static_integers,
                                shapes,
                                function_name,
                                emitted,
                            )?,
                        });
                    }
                    let rewritten_else = match fallback {
                        Some(fallback) => Some(unroll_block(
                            fallback,
                            static_integers,
                            shapes,
                            function_name,
                            emitted,
                        )?),
                        None => None,
                    };
                    out.push(rumoca_core::Statement::If {
                        cond_blocks: rewritten_blocks,
                        else_block: rewritten_else,
                        span: *span,
                    });
                }
            }
            rumoca_core::Statement::While { block, span } => {
                out.push(rumoca_core::Statement::While {
                    block: rumoca_core::StatementBlock {
                        cond: block.cond.clone(),
                        stmts: unroll_block(
                            &block.stmts,
                            static_integers,
                            shapes,
                            function_name,
                            emitted,
                        )?,
                    },
                    span: *span,
                });
            }
            other => {
                *emitted += 1;
                if *emitted > UNROLL_ITERATION_LIMIT {
                    return Err(unroll_overflow(function_name, statement_span(other)));
                }
                out.push(other.clone());
            }
        }
    }
    Ok(())
}

fn unroll_block(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    function_name: &str,
    emitted: &mut usize,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let mut block = Vec::new();
    unroll_sequence(
        statements,
        static_integers,
        shapes,
        function_name,
        &mut block,
        emitted,
    )?;
    Ok(block)
}

#[allow(clippy::too_many_arguments)]
fn unroll_for(
    indices: &[rumoca_core::ForIndex],
    equations: &[rumoca_core::Statement],
    span: Span,
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    function_name: &str,
    out: &mut Vec<rumoca_core::Statement>,
    emitted: &mut usize,
) -> Result<(), ToDaeError> {
    let Some((first, rest)) = indices.split_first() else {
        return unroll_sequence(
            equations,
            static_integers,
            shapes,
            function_name,
            out,
            emitted,
        );
    };
    let Some((lower, step, upper)) = static_function_range(&first.range, static_integers, shapes)?
    else {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            format!(
                "`{function_name}.{}` loop bound is not statically evaluable, so a data-dependent \
                 nest cannot be unrolled",
                first.ident
            ),
            expression_span(&first.range).unwrap_or(span),
        ));
    };
    // The peeled inner content is the remaining index family (if any) over the
    // original body, so `for i in 1:n, j in 1:i` and `for i .. for j in 1:i ..`
    // unroll identically: substituting `i` settles the dependent `j` range.
    let inner: Vec<rumoca_core::Statement> = if rest.is_empty() {
        equations.to_vec()
    } else {
        vec![rumoca_core::Statement::For {
            indices: rest.to_vec(),
            equations: equations.to_vec(),
            span,
        }]
    };
    let mut value = lower;
    while (step > 0 && value <= upper) || (step < 0 && value >= upper) {
        // Settle the binder, then propagate any integer local whose value that
        // settles (a reversed loop index, a running offset) so a bound written
        // over it is constant before its own loop is unrolled.
        let settled = substitute_name(&inner, &first.ident, value);
        let folded = constant_fold_integer_locals(&settled, static_integers, shapes)?;
        unroll_sequence(
            &folded,
            static_integers,
            shapes,
            function_name,
            out,
            emitted,
        )?;
        if *emitted > UNROLL_ITERATION_LIMIT {
            return Err(unroll_overflow(function_name, span));
        }
        value = value
            .checked_add(step)
            .ok_or_else(|| unroll_overflow(function_name, span))?;
    }
    Ok(())
}

/// Propagate top-level integer locals whose value is now a translation-time
/// constant, dropping the settled declaration.
///
/// A single-assignment integer local — Cholesky's `solveRow := n + 1 - reverseRow`
/// once `reverseRow` is a literal — has one value across the statements that read
/// it, so substituting it is exact. Only a target assigned exactly once is folded,
/// so a genuinely reassigned accumulator is never mistaken for a constant.
fn constant_fold_integer_locals(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let mut work = statements.to_vec();
    loop {
        let mut folded = None;
        for (index, statement) in work.iter().enumerate() {
            let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
                continue;
            };
            let [part] = comp.parts() else {
                continue;
            };
            if !part.subs.is_empty() {
                continue;
            }
            if assignment_count(&work, &part.ident) != 1 {
                continue;
            }
            if let Some(constant) =
                static_shape_integer_expression(value, static_integers, shapes)?
            {
                folded = Some((index, part.ident.clone(), constant));
                break;
            }
        }
        let Some((index, name, constant)) = folded else {
            return Ok(work);
        };
        work = substitute_name(&work, &name, constant);
        work.remove(index);
    }
}

/// Count how many statements assign the named declaration, through every nesting.
fn assignment_count(statements: &[rumoca_core::Statement], name: &str) -> usize {
    let mut count = 0;
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. }
            | rumoca_core::Statement::Reinit { variable: comp, .. } => {
                if comp.parts().first().is_some_and(|part| part.ident.as_str() == name) {
                    count += 1;
                }
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                for output in outputs.iter().flatten() {
                    if output.parts().first().is_some_and(|part| part.ident.as_str() == name) {
                        count += 1;
                    }
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                count += assignment_count(equations, name);
            }
            rumoca_core::Statement::While { block, .. } => {
                count += assignment_count(&block.stmts, name);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    count += assignment_count(&block.stmts, name);
                }
                if let Some(else_block) = else_block {
                    count += assignment_count(else_block, name);
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    count += assignment_count(&block.stmts, name);
                }
            }
            _ => {}
        }
    }
    count
}

fn unroll_overflow(function_name: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function loop domain",
        format!(
            "`{function_name}` unrolls a data-dependent loop nest beyond the checked iteration \
             limit; its proven extents are too large for straight-line lowering"
        ),
        span,
    )
}

fn statement_span(statement: &rumoca_core::Statement) -> Span {
    match statement {
        rumoca_core::Statement::Empty { span }
        | rumoca_core::Statement::Assignment { span, .. }
        | rumoca_core::Statement::Return { span }
        | rumoca_core::Statement::Break { span }
        | rumoca_core::Statement::For { span, .. }
        | rumoca_core::Statement::While { span, .. }
        | rumoca_core::Statement::If { span, .. }
        | rumoca_core::Statement::When { span, .. }
        | rumoca_core::Statement::FunctionCall { span, .. }
        | rumoca_core::Statement::Reinit { span, .. }
        | rumoca_core::Statement::Assert { span, .. } => *span,
    }
}
