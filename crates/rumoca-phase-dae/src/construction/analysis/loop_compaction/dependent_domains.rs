//! Rectangularize finite dependent domains without scalar expansion.

use super::*;

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
        let cond_blocks = cond_blocks
            .iter()
            .map(|block| {
                Ok(rumoca_core::StatementBlock {
                    cond: block.cond.clone(),
                    stmts: rectangularize_loops_in_scope(
                        &block.stmts,
                        static_integers,
                        shapes,
                        enclosing_bounds,
                    )?,
                })
            })
            .collect::<Result<Vec<_>, ToDaeError>>()?;
        let else_block = else_block
            .as_ref()
            .map(|branch| {
                rectangularize_loops_in_scope(branch, static_integers, shapes, enclosing_bounds)
            })
            .transpose()?;
        return Ok(rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span: *span,
        });
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
        if let Some((lower, step, upper)) =
            static_function_range(&index.range, static_integers, shapes)?
        {
            bounds.insert(
                VarName::new(&index.ident),
                ordered_range_bounds(lower, step, upper),
            );
            rectangular.push(index.clone());
            continue;
        }
        let Some((lower, upper)) =
            dependent_range_envelope(&index.range, static_integers, shapes, &bounds)?
        else {
            rectangular.push(index.clone());
            continue;
        };
        let Some(reference) = first_binder_reference(equations, &index.ident) else {
            rectangular.push(index.clone());
            continue;
        };
        guards.push(range_membership_guard(reference, &index.range, range_span)?);
        rectangular.push(rumoca_core::ForIndex {
            ident: index.ident.clone(),
            range: integer_range(lower, upper, range_span),
        });
        bounds.insert(VarName::new(&index.ident), (lower, upper));
    }
    let equations = rectangularize_loops_in_scope(equations, static_integers, shapes, &bounds)?;
    let mut rewriter = MaskedComprehensionRewriter {
        static_integers,
        shapes,
        bounds: &bounds,
        error: None,
    };
    let equations = rewriter.rewrite_statements(&equations);
    if let Some(error) = rewriter.error {
        return Err(error);
    }
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
        let Some((lower, upper)) = envelope else {
            return self.walk_expression(expression);
        };
        let Some(reference) = first_expression_binder_reference(expr, &index.name) else {
            return self.walk_expression(expression);
        };
        let guard = match range_membership_guard(reference, &index.range, *span) {
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
                range: integer_range(lower, upper, *span),
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
) -> Result<Option<(i64, i64)>, ToDaeError> {
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
    if step != 1 || bounds.len() > 8 {
        return Ok(None);
    }
    if let Some(envelope) = shapes.proven_range_bounds(range) {
        return Ok(Some(envelope));
    }
    let mut environments = vec![static_integers.clone()];
    for (name, (lower, upper)) in bounds {
        let mut expanded = Vec::with_capacity(environments.len() * 2);
        for environment in environments {
            let mut lower_environment = environment.clone();
            lower_environment.insert(name.clone(), *lower);
            expanded.push(lower_environment);
            if upper != lower {
                let mut upper_environment = environment;
                upper_environment.insert(name.clone(), *upper);
                expanded.push(upper_environment);
            }
        }
        environments = expanded;
    }
    let mut starts = Vec::with_capacity(environments.len());
    let mut ends = Vec::with_capacity(environments.len());
    for environment in &environments {
        let Some(start) = static_shape_integer_expression(start, environment, shapes)? else {
            return Ok(None);
        };
        let Some(end) = static_shape_integer_expression(end, environment, shapes)? else {
            return Ok(None);
        };
        starts.push(start);
        ends.push(end);
    }
    Ok(starts.into_iter().min().zip(ends.into_iter().max()))
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
    span: Span,
) -> Result<Expression, ToDaeError> {
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            "a masked compact loop requires an explicit range",
            span,
        ));
    };
    if step.is_some() {
        return Err(ToDaeError::unsupported_flat(
            "function loop domain",
            "a masked compact loop currently requires unit stride",
            span,
        ));
    }
    let binder = || Expression::VarRef {
        name: binder.clone(),
        subscripts: Vec::new(),
        span,
    };
    let lower = Expression::Binary {
        op: OpBinary::Ge,
        lhs: Box::new(binder()),
        rhs: start.clone(),
        span,
    };
    let upper = Expression::Binary {
        op: OpBinary::Le,
        lhs: Box::new(binder()),
        rhs: end.clone(),
        span,
    };
    Ok(Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(lower),
        rhs: Box::new(upper),
        span,
    })
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
