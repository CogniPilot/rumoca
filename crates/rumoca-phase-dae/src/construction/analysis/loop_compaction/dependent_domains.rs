//! Rectangularize finite dependent domains without scalar expansion.

#[cfg(test)]
mod tests;

use super::*;
use rumoca_core::BuiltinFunction;

struct DependentEnvelope {
    lower: i64,
    upper: i64,
    step: i64,
}

pub(super) fn rectangularize_dependent_loops(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    rectangularize_loops_in_scope(statements, static_integers, shapes, &HashMap::new())
}

fn rectangularize_loops_in_scope(
    statements: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    enclosing_bounds: &HashMap<VarName, (i64, i64)>,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    statements
        .iter()
        .map(|statement| {
            rectangularize_statement(statement, static_integers, shapes, enclosing_bounds)
        })
        .collect()
}

fn rectangularize_statement(
    statement: &rumoca_core::Statement,
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    enclosing_bounds: &HashMap<VarName, (i64, i64)>,
) -> Result<rumoca_core::Statement, ToDaeError> {
    if let rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    } = statement
    {
        return rectangularize_conditional(
            cond_blocks,
            else_block.as_deref(),
            *span,
            static_integers,
            shapes,
            enclosing_bounds,
        );
    }
    let rumoca_core::Statement::For {
        indices,
        equations,
        span,
    } = statement
    else {
        return Ok(statement.clone());
    };
    let mut bounds = enclosing_bounds.clone();
    let mut rectangular = Vec::with_capacity(indices.len());
    let mut guards = Vec::new();
    for index in indices {
        let range_span = expression_span(&index.range)?;
        let (index_static, index_shapes) = scoped_domain_facts(static_integers, shapes, &bounds);
        if let Some((lower, step, upper)) =
            static_function_range(&index.range, &index_static, &index_shapes)?
        {
            bounds.insert(
                VarName::new(&index.ident),
                ordered_range_bounds(lower, step, upper),
            );
            rectangular.push(index.clone());
            continue;
        }
        let Some(envelope) =
            dependent_range_envelope(&index.range, &index_static, &index_shapes, &bounds)?
        else {
            rectangular.push(index.clone());
            continue;
        };
        require_entry_invariant_range(&index.range, equations, range_span)?;
        let Some(reference) = first_binder_reference(equations, &index.ident) else {
            rectangular.push(index.clone());
            continue;
        };
        guards.push(range_membership_guard(
            reference,
            &index.range,
            envelope.step,
            range_span,
        )?);
        rectangular.push(rumoca_core::ForIndex {
            ident: index.ident.clone(),
            range: oriented_integer_range(
                envelope.lower,
                envelope.upper,
                envelope.step,
                range_span,
            ),
        });
        bounds.insert(VarName::new(&index.ident), (envelope.lower, envelope.upper));
    }
    let equations = rectangularize_loop_body(equations, static_integers, shapes, &bounds)?;
    let equations = match combine_guards(guards) {
        Some(condition) => vec![rumoca_core::Statement::If {
            cond_blocks: vec![rumoca_core::StatementBlock {
                cond: condition,
                stmts: equations,
            }],
            else_block: None,
            span: *span,
        }],
        None => equations,
    };
    Ok(rumoca_core::Statement::For {
        indices: rectangular,
        equations,
        span: *span,
    })
}

fn rectangularize_loop_body(
    equations: &[rumoca_core::Statement],
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    bounds: &HashMap<VarName, (i64, i64)>,
) -> Result<Vec<rumoca_core::Statement>, ToDaeError> {
    let (body_static, body_shapes) = scoped_domain_facts(static_integers, shapes, bounds);
    let equations = rectangularize_loops_in_scope(equations, &body_static, &body_shapes, bounds)?;
    let mut rewriter = MaskedComprehensionRewriter {
        static_integers: &body_static,
        shapes: &body_shapes,
        bounds,
        error: None,
    };
    let equations = rewriter.rewrite_statements(&equations);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(equations),
    }
}

fn scoped_domain_facts(
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    bounds: &HashMap<VarName, (i64, i64)>,
) -> (HashMap<VarName, i64>, ShapeEnvironment) {
    let static_integers = static_integers
        .iter()
        .filter(|(name, _)| !bounds.contains_key(*name))
        .map(|(name, value)| (name.clone(), *value))
        .collect();
    let mut shapes = shapes.clone();
    for (name, (lower, upper)) in bounds {
        shapes.bind_integer_bounds(name.clone(), *lower, *upper);
    }
    (static_integers, shapes)
}

fn rectangularize_conditional(
    branches: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    span: Span,
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    bounds: &HashMap<VarName, (i64, i64)>,
) -> Result<rumoca_core::Statement, ToDaeError> {
    let cond_blocks = branches
        .iter()
        .map(|block| {
            Ok(rumoca_core::StatementBlock {
                cond: block.cond.clone(),
                stmts: rectangularize_loops_in_scope(
                    &block.stmts,
                    static_integers,
                    shapes,
                    bounds,
                )?,
            })
        })
        .collect::<Result<Vec<_>, ToDaeError>>()?;
    let else_block = fallback
        .map(|branch| rectangularize_loops_in_scope(branch, static_integers, shapes, bounds))
        .transpose()?;
    Ok(rumoca_core::Statement::If {
        cond_blocks,
        else_block,
        span,
    })
}

fn require_entry_invariant_range(
    range: &Expression,
    body: &[rumoca_core::Statement],
    span: Span,
) -> Result<(), ToDaeError> {
    let mut references = Vec::new();
    range.collect_var_refs(&mut references);
    if references
        .iter()
        .any(|reference| super::loop_local_substitution::statements_assign_name(body, reference))
    {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            "a bounded dynamic range requires an entry snapshot when its body writes a range operand",
            span,
        ));
    }
    Ok(())
}

struct MaskedComprehensionRewriter<'a> {
    static_integers: &'a HashMap<VarName, i64>,
    shapes: &'a ShapeEnvironment,
    bounds: &'a HashMap<VarName, (i64, i64)>,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for MaskedComprehensionRewriter<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        let Expression::ArrayComprehension {
            expr,
            indices,
            filter: None,
            span,
        } = expression
        else {
            return self.walk_expression(expression);
        };
        let [index] = indices.as_slice() else {
            return self.walk_expression(expression);
        };
        let is_static = match static_function_range(&index.range, self.static_integers, self.shapes)
        {
            Ok(value) => value.is_some(),
            Err(error) => {
                self.error.get_or_insert(error);
                return expression.clone();
            }
        };
        if is_static {
            return self.walk_expression(expression);
        }
        let envelope = match dependent_range_envelope(
            &index.range,
            self.static_integers,
            self.shapes,
            self.bounds,
        ) {
            Ok(value) => value,
            Err(error) => {
                self.error.get_or_insert(error);
                return expression.clone();
            }
        };
        let Some(envelope) = envelope else {
            return self.walk_expression(expression);
        };
        // This zero-filled reduction view retains its existing unit-stride
        // shape contract. Strided statement domains are admitted separately.
        if envelope.step != 1 {
            return self.walk_expression(expression);
        }
        let Some(reference) = first_expression_binder_reference(expr, &index.name) else {
            return self.walk_expression(expression);
        };
        let guard = match range_membership_guard(reference, &index.range, envelope.step, *span) {
            Ok(guard) => guard,
            Err(error) => {
                self.error.get_or_insert(error);
                return expression.clone();
            }
        };
        Expression::ArrayComprehension {
            expr: Box::new(Expression::If {
                branches: vec![(guard, self.rewrite_expression(expr))],
                else_branch: Box::new(Expression::Literal {
                    value: Literal::Integer(0),
                    span: *span,
                }),
                span: *span,
            }),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: index.name.clone(),
                range: integer_range(envelope.lower, envelope.upper, *span),
            }],
            filter: None,
            span: *span,
        }
    }
}

impl StatementRewriter for MaskedComprehensionRewriter<'_> {}

fn ordered_range_bounds(lower: i64, step: i64, upper: i64) -> (i64, i64) {
    if (step > 0 && lower <= upper) || (step < 0 && lower >= upper) {
        (lower.min(upper), lower.max(upper))
    } else {
        (lower, lower)
    }
}

fn dependent_range_envelope(
    range: &Expression,
    static_integers: &HashMap<VarName, i64>,
    shapes: &ShapeEnvironment,
    bounds: &HashMap<VarName, (i64, i64)>,
) -> Result<Option<DependentEnvelope>, ToDaeError> {
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return Ok(None);
    };
    let step = match step {
        None => 1,
        Some(step) => {
            let Some(step) = static_shape_integer_expression(step, static_integers, shapes)? else {
                return Ok(None);
            };
            step
        }
    };
    if step == 0 || step.checked_abs().is_none() {
        return Ok(None);
    }
    let mut scoped_shapes = shapes.clone();
    for (name, value) in static_integers {
        scoped_shapes.bind_scalar_value(name.clone(), EvalValue::Integer(*value));
    }
    for (name, (lower, upper)) in bounds {
        scoped_shapes.bind_integer_bounds(name.clone(), *lower, *upper);
    }
    let Some((start_lower, start_upper)) = scoped_shapes.proven_integer_bounds(start) else {
        return Ok(None);
    };
    let Some((end_lower, end_upper)) = scoped_shapes.proven_integer_bounds(end) else {
        return Ok(None);
    };
    let (lower, upper) = if step > 0 {
        (start_lower, end_upper)
    } else {
        (end_lower, start_upper)
    };
    if step != 1
        && step != -1
        && (lower.checked_sub(start_upper).is_none() || upper.checked_sub(start_lower).is_none())
    {
        return Ok(None);
    }
    Ok(Some(DependentEnvelope { lower, upper, step }))
}

fn first_binder_reference(
    statements: &[rumoca_core::Statement],
    binder: &str,
) -> Option<Reference> {
    struct Finder<'a> {
        binder: &'a str,
        found: Option<Reference>,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            if self.found.is_none()
                && subscripts.is_empty()
                && reference.var_name() == &VarName::new(self.binder)
            {
                self.found = Some(reference.clone());
            }
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl rumoca_ir_flat::visitor::StatementVisitor for Finder<'_> {}
    let mut finder = Finder {
        binder,
        found: None,
    };
    for statement in statements {
        rumoca_ir_flat::visitor::StatementVisitor::visit_statement(&mut finder, statement);
        if finder.found.is_some() {
            break;
        }
    }
    finder.found
}

fn first_expression_binder_reference(expression: &Expression, binder: &str) -> Option<Reference> {
    struct Finder<'a> {
        binder: &'a str,
        found: Option<Reference>,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            if self.found.is_none()
                && subscripts.is_empty()
                && reference.var_name() == &VarName::new(self.binder)
            {
                self.found = Some(reference.clone());
            }
            self.walk_var_ref(reference, subscripts);
        }
    }
    let mut finder = Finder {
        binder,
        found: None,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expression);
    finder.found
}

fn range_membership_guard(
    binder: Reference,
    range: &Expression,
    stride: i64,
    span: Span,
) -> Result<Expression, ToDaeError> {
    let Expression::Range { start, end, .. } = range else {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            "a masked compact loop requires an explicit range",
            span,
        ));
    };
    let binder = || Expression::VarRef {
        name: binder.clone(),
        subscripts: Vec::new(),
        span,
    };
    let lower = Expression::Binary {
        op: if stride > 0 {
            OpBinary::Ge
        } else {
            OpBinary::Le
        },
        lhs: Box::new(binder()),
        rhs: start.clone(),
        span,
    };
    let upper = Expression::Binary {
        op: if stride > 0 {
            OpBinary::Le
        } else {
            OpBinary::Ge
        },
        lhs: Box::new(binder()),
        rhs: end.clone(),
        span,
    };
    let membership = Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(lower),
        rhs: Box::new(upper),
        span,
    };
    if stride == 1 || stride == -1 {
        return Ok(membership);
    }
    let Some(magnitude) = stride.checked_abs().filter(|magnitude| *magnitude > 0) else {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            "a bounded dynamic stride must have a finite positive magnitude",
            span,
        ));
    };
    let integer = |value| Expression::Literal {
        value: Literal::Integer(value),
        span,
    };
    let congruence = Expression::Binary {
        op: OpBinary::Eq,
        lhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Mod,
            args: vec![
                Expression::Binary {
                    op: OpBinary::Sub,
                    lhs: Box::new(binder()),
                    rhs: start.clone(),
                    span,
                },
                integer(magnitude),
            ],
            span,
        }),
        rhs: Box::new(integer(0)),
        span,
    };
    Ok(Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(membership),
        rhs: Box::new(congruence),
        span,
    })
}

fn oriented_integer_range(lower: i64, upper: i64, source_step: i64, span: Span) -> Expression {
    if source_step > 0 {
        return integer_range(lower, upper, span);
    }
    Expression::Range {
        start: Box::new(Expression::Literal {
            value: Literal::Integer(upper),
            span,
        }),
        step: Some(Box::new(Expression::Literal {
            value: Literal::Integer(-1),
            span,
        })),
        end: Box::new(Expression::Literal {
            value: Literal::Integer(lower),
            span,
        }),
        span,
    }
}

pub(super) fn integer_range(lower: i64, upper: i64, span: Span) -> Expression {
    let integer = |value| Expression::Literal {
        value: Literal::Integer(value),
        span,
    };
    Expression::Range {
        start: Box::new(integer(lower)),
        step: None,
        end: Box::new(integer(upper)),
        span,
    }
}

fn combine_guards(mut guards: Vec<Expression>) -> Option<Expression> {
    let mut condition = guards.pop()?;
    while let Some(guard) = guards.pop() {
        let span = expression_span(&condition).ok()?;
        condition = Expression::Binary {
            op: OpBinary::And,
            lhs: Box::new(guard),
            rhs: Box::new(condition),
            span,
        };
    }
    Some(condition)
}
