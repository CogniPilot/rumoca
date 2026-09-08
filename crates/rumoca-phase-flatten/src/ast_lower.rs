#[cfg(test)]
mod tests;

use rumoca_core::{ComponentRefPart, ComponentReference, Reference, SourceMap};
use rumoca_core::{DefId, Span};
use rumoca_ir_ast as ast;

use crate::FlattenError;
use crate::source_spans::required_location_span;
use crate::static_subscripts::try_constant_integer;

type LowerResult<T> = Result<T, FlattenError>;

#[derive(Clone, Copy, Default)]
pub(crate) struct LoweringContext<'a> {
    pub(crate) instance_name: Option<&'a str>,
    pub(crate) predefined_string_declaration: Option<DefId>,
    pub(crate) predefined_intrinsics: PredefinedIntrinsicIds,
}

#[derive(Clone, Copy, Default)]
pub(crate) struct PredefinedIntrinsicIds {
    identities: [Option<DefId>; rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED.len()],
    assertion: Option<DefId>,
}

impl PredefinedIntrinsicIds {
    pub(crate) fn from_tree(tree: &ast::ClassTree) -> Self {
        Self {
            identities: std::array::from_fn(|index| {
                tree.scope_tree
                    .predefined_member(&rumoca_core::ComponentPath::from_flat_path(
                        rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED[index].name(),
                    ))
            }),
            assertion: tree
                .scope_tree
                .predefined_member(&rumoca_core::ComponentPath::from_flat_path("assert")),
        }
    }

    pub(crate) fn resolve(self, target: Option<DefId>) -> Option<rumoca_core::BuiltinFunction> {
        let target = target?;
        self.identities
            .into_iter()
            .zip(rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED)
            .find_map(|(identity, intrinsic)| (identity == Some(target)).then_some(*intrinsic))
    }

    fn is_assertion(self, target: Option<DefId>) -> bool {
        self.assertion.is_some() && self.assertion == target
    }
}

#[cfg(test)]
pub(crate) fn expression_from_ast(expr: &ast::Expression) -> LowerResult<rumoca_core::Expression> {
    expression_from_ast_with_intrinsics(expr, PredefinedIntrinsicIds::default())
}

pub(crate) fn expression_from_ast_with_intrinsics(
    expr: &ast::Expression,
    predefined_intrinsics: PredefinedIntrinsicIds,
) -> LowerResult<rumoca_core::Expression> {
    expression_from_ast_with_context(
        expr,
        LoweringContext {
            instance_name: None,
            predefined_string_declaration: None,
            predefined_intrinsics,
        },
    )
}

pub(crate) fn expression_from_ast_with_context(
    expr: &ast::Expression,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    match expr {
        ast::Expression::Empty { span } => Err(FlattenError::invalid_ast_recovery(
            "Expression::Empty is a parser-recovery node",
            *span,
        )),

        ast::Expression::Binary { op, lhs, rhs, span } => {
            binary_expression_from_ast(op, lhs, rhs, *span, context)
        }

        ast::Expression::Unary { op, rhs, span } => {
            unary_expression_from_ast(op, rhs, *span, context)
        }

        ast::Expression::ComponentReference(cr) => {
            expression_from_component_ref_with_context(cr, context)
        }

        ast::Expression::FunctionCall {
            comp,
            args,
            is_partial_application,
            span,
        } => convert_function_call_with_context(
            comp,
            args,
            if *is_partial_application {
                rumoca_core::FunctionCallKind::PartialApplication
            } else {
                rumoca_core::FunctionCallKind::Invocation
            },
            *span,
            context,
        ),

        ast::Expression::DerivativeCall { args, span } => {
            lower_derivative_call(args, *span, context)
        }

        ast::Expression::Terminal {
            terminal_type,
            token,
            ..
        } => Ok(rumoca_core::Expression::Literal {
            value: convert_terminal(terminal_type, token, expr.span())?,
            span: expr.span(),
        }),

        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => convert_if_with_context(branches, else_branch, expr.span(), context),

        ast::Expression::Array {
            elements,
            is_matrix,
            ..
        } => Ok(rumoca_core::Expression::Array {
            elements: convert_expr_vec_with_context(elements, context)?,
            is_matrix: *is_matrix,
            span: expr.span(),
        }),

        ast::Expression::Tuple { elements, .. } => Ok(rumoca_core::Expression::Tuple {
            elements: convert_expr_vec_with_context(elements, context)?,
            span: expr.span(),
        }),

        ast::Expression::Range {
            start, step, end, ..
        } => range_expression_from_ast(start, step.as_deref(), end, expr.span(), context),

        ast::Expression::Parenthesized { inner, .. } => {
            expression_from_ast_with_context(inner, context)
        }

        ast::Expression::ArrayComprehension {
            expr: body,
            indices,
            filter,
            ..
        } => convert_array_comprehension_with_context(body, indices, filter, expr.span(), context),

        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => convert_class_modification_with_context(target, modifications, context),

        ast::Expression::NamedArgument { span, .. } => Err(FlattenError::invalid_ast_recovery(
            "Expression::NamedArgument is only meaningful as a function-call argument",
            *span,
        )),

        ast::Expression::Modification { span, .. } => Err(FlattenError::invalid_ast_recovery(
            "Expression::Modification is only meaningful as a modifier or named-call carrier",
            *span,
        )),

        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => convert_array_index_with_context(base, subscripts, expr.span(), context),

        ast::Expression::FieldAccess {
            base,
            field,
            field_def_id,
            ..
        } => Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(expression_from_ast_with_context(base, context)?),
            field: field.clone(),
            field_def_id: field_def_id
                .ok_or_else(|| FlattenError::missing_flat_variable_identity(field, expr.span()))?,
            span: expr.span(),
        }),
    }
}

fn lower_derivative_call(
    args: &[ast::Expression],
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let function = rumoca_core::BuiltinFunction::Der;
    Ok(rumoca_core::Expression::BuiltinCall {
        function,
        args: lower_builtin_call_args(function, args, span, context)?,
        span,
    })
}

fn binary_expression_from_ast(
    op: &rumoca_core::OpBinary,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    match op {
        rumoca_core::OpBinary::Empty => Err(FlattenError::invalid_ast_recovery(
            "OpBinary::Empty is a parser-recovery operator",
            span,
        )),
        rumoca_core::OpBinary::Assign => Err(FlattenError::invalid_ast_recovery(
            "OpBinary::Assign is a component-modification carrier, not a value operator",
            span,
        )),
        _ => Ok(rumoca_core::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(expression_from_ast_with_context(lhs, context)?),
            rhs: Box::new(expression_from_ast_with_context(rhs, context)?),
            span,
        }),
    }
}

fn unary_expression_from_ast(
    op: &rumoca_core::OpUnary,
    rhs: &ast::Expression,
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    if matches!(op, rumoca_core::OpUnary::Empty) {
        return Err(FlattenError::invalid_ast_recovery(
            "OpUnary::Empty is a parser-recovery operator",
            span,
        ));
    }
    Ok(rumoca_core::Expression::Unary {
        op: op.clone(),
        rhs: Box::new(expression_from_ast_with_context(rhs, context)?),
        span,
    })
}

fn range_expression_from_ast(
    start: &ast::Expression,
    step: Option<&ast::Expression>,
    end: &ast::Expression,
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    Ok(rumoca_core::Expression::Range {
        start: Box::new(expression_from_ast_with_context(start, context)?),
        step: step
            .map(|step| expression_from_ast_with_context(step, context).map(Box::new))
            .transpose()?,
        end: Box::new(expression_from_ast_with_context(end, context)?),
        span,
    })
}

/// Lower the exact residual shape used by an MLS multi-result equation.
/// Direct `Empty` tuple elements are omitted receivers only in this context.
pub(crate) fn equation_residual_from_ast_with_context(
    expression: &ast::Expression,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let ast::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = expression
    else {
        return expression_from_ast_with_context(expression, context);
    };
    if !ast::is_invocation_tuple_equation(lhs, rhs) {
        return expression_from_ast_with_context(expression, context);
    }
    Ok(rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(tuple_receiver_from_ast_with_context(lhs, context)?),
        rhs: Box::new(expression_from_ast_with_context(rhs, context)?),
        span: *span,
    })
}

fn tuple_receiver_from_ast_with_context(
    expression: &ast::Expression,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    match expression {
        ast::Expression::Parenthesized { inner, .. } => {
            tuple_receiver_from_ast_with_context(inner, context)
        }
        ast::Expression::Tuple { elements, span } => Ok(rumoca_core::Expression::Tuple {
            elements: elements
                .iter()
                .map(|element| match element {
                    ast::Expression::Empty { span } => {
                        Ok(rumoca_core::Expression::Empty { span: *span })
                    }
                    _ => expression_from_ast_with_context(element, context),
                })
                .collect::<LowerResult<Vec<_>>>()?,
            span: *span,
        }),
        _ => expression_from_ast_with_context(expression, context),
    }
}

fn convert_array_index_with_context(
    base: &ast::Expression,
    subscripts: &[ast::Subscript],
    span: rumoca_core::Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let base = Box::new(expression_from_ast_with_context(base, context)?);
    let subscripts = subscripts
        .iter()
        .enumerate()
        .map(|(dimension, sub)| subscript_from_ast_for_base(sub, &base, dimension, span, context))
        .collect::<LowerResult<Vec<_>>>()?;
    Ok(rumoca_core::Expression::Index {
        base,
        subscripts,
        span,
    })
}

#[cfg(test)]
pub(crate) fn statement_from_ast(stmt: &ast::Statement) -> LowerResult<rumoca_core::Statement> {
    statement_from_ast_with_source_map(stmt, None)
}

#[cfg(test)]
pub(crate) fn statement_from_ast_with_source_map(
    stmt: &ast::Statement,
    source_map: Option<&SourceMap>,
) -> LowerResult<rumoca_core::Statement> {
    statement_from_ast_with_context_and_source_map(
        stmt,
        LoweringContext {
            instance_name: None,
            predefined_string_declaration: None,
            predefined_intrinsics: PredefinedIntrinsicIds::default(),
        },
        source_map,
    )
}

pub(crate) fn statement_from_ast_with_context_and_source_map(
    stmt: &ast::Statement,
    context: LoweringContext<'_>,
    source_map: Option<&SourceMap>,
) -> LowerResult<rumoca_core::Statement> {
    let span = ast_statement_span(stmt, source_map)?;
    statement_from_ast_with_span(stmt, context, source_map, span)
}

fn statement_from_ast_with_span(
    stmt: &ast::Statement,
    context: LoweringContext<'_>,
    source_map: Option<&SourceMap>,
    span: Span,
) -> LowerResult<rumoca_core::Statement> {
    match stmt {
        ast::Statement::Empty => Err(FlattenError::invalid_ast_recovery(
            "Statement::Empty is a parser-recovery node",
            span,
        )),
        ast::Statement::Assignment { comp, value } => Ok(rumoca_core::Statement::Assignment {
            comp: component_reference_from_ast_with_context(comp, context)?,
            value: expression_from_ast_with_context(value, context)?,
            span,
        }),
        ast::Statement::Return { .. } => Ok(rumoca_core::Statement::Return { span }),
        ast::Statement::Break { .. } => Ok(rumoca_core::Statement::Break { span }),
        ast::Statement::For { indices, equations } => Ok(rumoca_core::Statement::For {
            indices: indices
                .iter()
                .map(|index| for_index_from_ast_with_context(index, context))
                .collect::<LowerResult<Vec<_>>>()?,
            equations: equations
                .iter()
                .map(|inner| {
                    statement_from_ast_with_context_and_source_map(inner, context, source_map)
                })
                .collect::<LowerResult<Vec<_>>>()?,
            span,
        }),
        ast::Statement::While(block) => Ok(rumoca_core::Statement::While {
            block: statement_block_from_ast_with_context_and_source_map(
                block, context, source_map,
            )?,
            span,
        }),
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => if_statement_from_ast(
            cond_blocks,
            else_block.as_deref(),
            context,
            source_map,
            span,
        ),
        ast::Statement::When(blocks) => Ok(rumoca_core::Statement::When {
            blocks: blocks
                .iter()
                .map(|block| {
                    statement_block_from_ast_with_context_and_source_map(block, context, source_map)
                })
                .collect::<LowerResult<Vec<_>>>()?,
            span,
        }),
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => lower_function_call_statement(comp, args, outputs, context, span),
        ast::Statement::Reinit { variable, value } => Ok(rumoca_core::Statement::Reinit {
            variable: component_reference_from_ast_with_context(variable, context)?,
            value: expression_from_ast_with_context(value, context)?,
            span,
        }),
        ast::Statement::Assert {
            condition,
            message,
            level,
        } => Ok(rumoca_core::Statement::Assert {
            condition: expression_from_ast_with_context(condition, context)?,
            message: Box::new(expression_from_ast_with_context(message, context)?),
            level: level
                .as_ref()
                .map(|expr| expression_from_ast_with_context(expr, context))
                .transpose()?
                .map(Box::new),
            span,
        }),
    }
}

fn lower_function_call_statement(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    outputs: &[ast::Expression],
    context: LoweringContext<'_>,
    span: Span,
) -> LowerResult<rumoca_core::Statement> {
    if outputs.is_empty()
        && context
            .predefined_intrinsics
            .is_assertion(comp.target_def_id())
    {
        let decoded = crate::equations::decode_assert_arguments(args, span)?;
        return Ok(rumoca_core::Statement::Assert {
            condition: expression_from_ast_with_context(decoded.condition, context)?,
            message: Box::new(expression_from_ast_with_context(decoded.message, context)?),
            level: decoded
                .level
                .map(|level| expression_from_ast_with_context(level, context))
                .transpose()?
                .map(Box::new),
            span,
        });
    }
    let function_name = comp.to_string();
    Ok(rumoca_core::Statement::FunctionCall {
        comp: rumoca_core::Reference::from_component_reference(function_component_ref_from_ast(
            comp, context,
        )?),
        // Multi-output call statements carry the same source-level positional and
        // named argument layout as expression-position calls, so their actuals
        // pass through the shared named-argument marker projection. Lowering them
        // as plain expressions would strand an `Expression::NamedArgument` in the
        // argument list, which the Flat construction boundary rejects.
        args: args
            .iter()
            .map(|arg| convert_call_arg_with_context(arg, context))
            .collect::<LowerResult<Vec<_>>>()?,
        outputs: outputs
            .iter()
            .map(|output| {
                output_component_reference_from_ast(output, &function_name, context, span)
            })
            .collect::<LowerResult<Vec<_>>>()?,
        span,
    })
}

fn if_statement_from_ast(
    cond_blocks: &[ast::StatementBlock],
    else_block: Option<&[ast::Statement]>,
    context: LoweringContext<'_>,
    source_map: Option<&SourceMap>,
    span: Span,
) -> LowerResult<rumoca_core::Statement> {
    Ok(rumoca_core::Statement::If {
        cond_blocks: cond_blocks
            .iter()
            .map(|block| {
                statement_block_from_ast_with_context_and_source_map(block, context, source_map)
            })
            .collect::<LowerResult<Vec<_>>>()?,
        else_block: else_block
            .map(|stmts| {
                stmts
                    .iter()
                    .map(|inner| {
                        statement_from_ast_with_context_and_source_map(inner, context, source_map)
                    })
                    .collect::<LowerResult<Vec<_>>>()
            })
            .transpose()?,
        span,
    })
}

fn output_component_reference_from_ast(
    expr: &ast::Expression,
    function_name: &str,
    context: LoweringContext<'_>,
    owner_span: Span,
) -> LowerResult<Option<rumoca_core::ComponentReference>> {
    match expr {
        ast::Expression::ComponentReference(comp) => Ok(Some(
            component_reference_from_ast_with_context(comp, context)?,
        )),
        ast::Expression::Empty { .. } => Ok(None),
        _ => {
            let span = required_ast_span(expr.span(), "function-call output receiver")
                .or_else(|_| required_ast_span(owner_span, "function-call statement owner"))?;
            Err(FlattenError::invalid_function_call_output(
                function_name,
                "receiver is not a component reference or an omitted slot",
                span,
            ))
        }
    }
}

fn ast_statement_span(stmt: &ast::Statement, source_map: Option<&SourceMap>) -> LowerResult<Span> {
    if let Some(span) = ast_statement_syntax_span(stmt) {
        return Ok(span);
    }
    if let Some(location) = stmt.get_location()
        && let Some(map) = source_map
    {
        return required_location_span(map, location, "algorithm statement");
    }
    ast_statement_syntax_span(stmt).ok_or_else(|| {
        FlattenError::missing_source_context("algorithm statement is missing source provenance")
    })
}

fn required_ast_span(span: Span, context: &'static str) -> LowerResult<Span> {
    crate::source_spans::required_span(span, context)
}

fn ast_statement_syntax_span(stmt: &ast::Statement) -> Option<Span> {
    let span = match stmt {
        ast::Statement::Empty => return None,
        ast::Statement::Assignment { comp, value } => {
            first_non_dummy_span([comp.span, value.span()])?
        }
        ast::Statement::Return { .. } | ast::Statement::Break { .. } => return None,
        ast::Statement::For { indices, equations } => indices
            .iter()
            .map(|index| index.range.span())
            .find(|span| !span.is_dummy())
            .or_else(|| equations.iter().find_map(ast_statement_syntax_span))?,
        ast::Statement::While(block) => block.cond.span(),
        ast::Statement::If { cond_blocks, .. } => cond_blocks
            .iter()
            .map(|block| block.cond.span())
            .find(|span| !span.is_dummy())?,
        ast::Statement::When(blocks) => blocks
            .iter()
            .map(|block| block.cond.span())
            .find(|span| !span.is_dummy())?,
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => first_non_dummy_span(
            std::iter::once(comp.span)
                .chain(args.iter().map(ast::Expression::span))
                .chain(outputs.iter().map(ast::Expression::span)),
        )?,
        ast::Statement::Reinit { variable, value } => {
            first_non_dummy_span([variable.span, value.span()])?
        }
        ast::Statement::Assert {
            condition,
            message,
            level,
        } => first_non_dummy_span(
            std::iter::once(condition.span())
                .chain(std::iter::once(message.span()))
                .chain(level.iter().map(|expr| expr.span())),
        )?,
    };
    Some(span)
}

fn first_non_dummy_span(spans: impl IntoIterator<Item = Span>) -> Option<Span> {
    spans.into_iter().find(|span| !span.is_dummy())
}

fn for_index_from_ast_with_context(
    index: &ast::ForIndex,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::ForIndex> {
    Ok(rumoca_core::ForIndex {
        ident: index.ident.text.to_string(),
        range: expression_from_ast_with_context(&index.range, context)?,
    })
}

fn statement_block_from_ast_with_context_and_source_map(
    block: &ast::StatementBlock,
    context: LoweringContext<'_>,
    source_map: Option<&SourceMap>,
) -> LowerResult<rumoca_core::StatementBlock> {
    Ok(rumoca_core::StatementBlock {
        cond: expression_from_ast_with_context(&block.cond, context)?,
        stmts: block
            .stmts
            .iter()
            .map(|stmt| statement_from_ast_with_context_and_source_map(stmt, context, source_map))
            .collect::<LowerResult<Vec<_>>>()?,
    })
}

fn component_reference_from_ast_with_context(
    comp: &ast::ComponentReference,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::ComponentReference> {
    let comp_span = required_ast_span(comp.span, "AST component reference")?;
    let parts = comp
        .parts
        .iter()
        .map(|part| {
            Ok(rumoca_core::ComponentRefPart {
                ident: part.ident.text.to_string(),
                span: comp_span,
                subs: component_part_subscripts_from_ast(part, comp_span, context)?,
                def_id: part.def_id.ok_or_else(|| {
                    FlattenError::missing_flat_variable_identity(
                        part.ident.text.as_ref(),
                        comp_span,
                    )
                })?,
            })
        })
        .collect::<LowerResult<Vec<_>>>()?;
    rumoca_core::ComponentReference::construct(comp.local, comp_span, parts)
        .map_err(|error| FlattenError::missing_flat_variable_identity(error.to_string(), comp_span))
}

fn function_component_ref_from_ast(
    comp: &ast::ComponentReference,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::ComponentReference> {
    component_reference_from_ast_with_context(comp, context)
}

fn subscript_from_ast(
    sub: &ast::Subscript,
    owner_span: rumoca_core::Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Subscript> {
    match sub {
        ast::Subscript::Expression(expr) => {
            let span = expr.span();
            if let Some(val) = try_constant_integer(expr) {
                return Ok(rumoca_core::Subscript::index(val, span));
            }
            Ok(rumoca_core::Subscript::expr(
                Box::new(expression_from_ast_with_context(expr, context)?),
                span,
            ))
        }
        ast::Subscript::Range { .. } => Ok(rumoca_core::Subscript::try_generated_colon(
            owner_span,
            "flat component subscript",
        )
        .map_err(|err| FlattenError::missing_source_context(err.to_string()))?),
        ast::Subscript::Empty => Err(FlattenError::invalid_ast_subscript(
            "empty recovery subscript cannot lower as a component selector",
            owner_span,
        )),
    }
}

fn component_part_subscripts_from_ast(
    part: &ast::ComponentRefPart,
    owner_span: rumoca_core::Span,
    context: LoweringContext<'_>,
) -> LowerResult<Vec<rumoca_core::Subscript>> {
    let Some(subs) = part.subs.as_ref() else {
        return Ok(Vec::new());
    };
    subs.iter()
        .map(|sub| subscript_from_ast(sub, owner_span, context))
        .collect()
}

fn expression_from_component_ref_with_context(
    cr: &ast::ComponentReference,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let cr_span = required_ast_span(cr.span, "AST component reference expression")?;
    if component_ref_has_subscripts(cr) {
        return component_ref_with_structured_subscripts(cr, cr_span, context);
    }

    expression_from_component_ref(cr, cr_span, context)
}

fn expression_from_component_ref(
    cr: &ast::ComponentReference,
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let name = reference_from_ast_component_ref(cr, context)?;

    Ok(rumoca_core::Expression::VarRef {
        name,
        subscripts: vec![],
        span,
    })
}

fn component_ref_has_subscripts(cr: &ast::ComponentReference) -> bool {
    cr.parts
        .iter()
        .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
}

fn component_ref_with_structured_subscripts(
    cr: &ast::ComponentReference,
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let mut pending_parts = Vec::new();
    let mut current = None;

    for part in &cr.parts {
        let ident = part.ident.text.to_string();
        let def_id = part.def_id.ok_or_else(|| {
            FlattenError::missing_flat_variable_identity(part.ident.text.as_ref(), span)
        })?;
        let Some(subs) = part.subs.as_ref().filter(|subs| !subs.is_empty()) else {
            if let Some(expr) = current.take() {
                current = Some(rumoca_core::Expression::FieldAccess {
                    base: Box::new(expr),
                    field: ident,
                    field_def_id: def_id,
                    span,
                });
            } else {
                pending_parts.push(ComponentRefPart {
                    ident,
                    span,
                    subs: Vec::new(),
                    def_id,
                });
            }
            continue;
        };

        let base = if let Some(expr) = current.take() {
            rumoca_core::Expression::FieldAccess {
                base: Box::new(expr),
                field: ident,
                field_def_id: def_id,
                span,
            }
        } else {
            pending_parts.push(ComponentRefPart {
                ident,
                span,
                subs: Vec::new(),
                def_id,
            });
            let base_ref =
                ComponentReference::construct(cr.local, span, std::mem::take(&mut pending_parts))
                    .map_err(|error| {
                    FlattenError::missing_flat_variable_identity(error.to_string(), span)
                })?;
            // Qualification records the spelling of exactly this base (the
            // parts up to and including the first subscripted one), so the
            // enclosing instance scope survives the split into `Index` nodes.
            rumoca_core::Expression::VarRef {
                name: named_reference(cr, base_ref),
                subscripts: vec![],
                span,
            }
        };

        let flat_subscripts = subs
            .iter()
            .enumerate()
            .map(|(dimension, sub)| {
                subscript_from_ast_for_base(sub, &base, dimension, span, context)
            })
            .collect::<LowerResult<Vec<_>>>()?;
        current = Some(rumoca_core::Expression::Index {
            base: Box::new(base),
            subscripts: flat_subscripts,
            span,
        });
    }

    current.map_or_else(|| expression_from_component_ref(cr, span, context), Ok)
}

fn reference_from_ast_component_ref(
    cr: &ast::ComponentReference,
    context: LoweringContext<'_>,
) -> LowerResult<Reference> {
    let reference = component_reference_from_ast(cr, context)?;
    Ok(named_reference(cr, reference))
}

/// Spell a lowered reference with the qualified name recorded by qualification.
///
/// The recorded spelling describes the parts that lowering keeps on one
/// reference, so it applies both to a whole unsubscripted reference and to the
/// base a subscripted reference splits into.
fn named_reference(cr: &ast::ComponentReference, reference: ComponentReference) -> Reference {
    match cr.qualified_display_name() {
        Some(display) => Reference::with_component_reference(display.as_str(), reference),
        None => Reference::from_component_reference(reference),
    }
}

fn component_reference_from_ast(
    cr: &ast::ComponentReference,
    context: LoweringContext<'_>,
) -> LowerResult<ComponentReference> {
    component_reference_from_ast_with_context(cr, context)
}

fn subscript_from_ast_for_base(
    sub: &ast::Subscript,
    base: &rumoca_core::Expression,
    dimension: usize,
    owner_span: rumoca_core::Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Subscript> {
    match sub {
        ast::Subscript::Expression(expr) => {
            let span = expr.span();
            if let Some(val) = try_constant_integer(expr) {
                return Ok(rumoca_core::Subscript::index(val, span));
            }
            Ok(rumoca_core::Subscript::expr(
                Box::new(expression_from_ast_in_subscript(
                    expr, base, dimension, owner_span, context,
                )?),
                span,
            ))
        }
        ast::Subscript::Range { .. } => Ok(rumoca_core::Subscript::colon(owner_span)),
        ast::Subscript::Empty => Err(FlattenError::invalid_ast_subscript(
            "empty recovery subscript cannot lower as an indexed-expression selector",
            owner_span,
        )),
    }
}

fn expression_from_ast_in_subscript(
    expr: &ast::Expression,
    base: &rumoca_core::Expression,
    dimension: usize,
    owner_span: rumoca_core::Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            ..
        } => end_subscript_expression(base, dimension, expr.span(), owner_span),
        ast::Expression::Binary { op, lhs, rhs, .. } => Ok(rumoca_core::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(expression_from_ast_in_subscript(
                lhs, base, dimension, owner_span, context,
            )?),
            rhs: Box::new(expression_from_ast_in_subscript(
                rhs, base, dimension, owner_span, context,
            )?),
            span: expr.span(),
        }),
        ast::Expression::Unary { op, rhs, .. } => Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(expression_from_ast_in_subscript(
                rhs, base, dimension, owner_span, context,
            )?),
            span: expr.span(),
        }),
        ast::Expression::Parenthesized { inner, .. } => {
            expression_from_ast_in_subscript(inner, base, dimension, owner_span, context)
        }
        ast::Expression::Range {
            start, step, end, ..
        } => Ok(rumoca_core::Expression::Range {
            start: Box::new(expression_from_ast_in_subscript(
                start, base, dimension, owner_span, context,
            )?),
            step: step
                .as_ref()
                .map(|step| {
                    expression_from_ast_in_subscript(step, base, dimension, owner_span, context)
                        .map(Box::new)
                })
                .transpose()?,
            end: Box::new(expression_from_ast_in_subscript(
                end, base, dimension, owner_span, context,
            )?),
            span: expr.span(),
        }),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => Ok(rumoca_core::Expression::If {
            branches: branches
                .iter()
                .map(|(condition, value)| {
                    Ok((
                        expression_from_ast_in_subscript(
                            condition, base, dimension, owner_span, context,
                        )?,
                        expression_from_ast_in_subscript(
                            value, base, dimension, owner_span, context,
                        )?,
                    ))
                })
                .collect::<LowerResult<Vec<_>>>()?,
            else_branch: Box::new(expression_from_ast_in_subscript(
                else_branch,
                base,
                dimension,
                owner_span,
                context,
            )?),
            span: expr.span(),
        }),
        ast::Expression::Array {
            elements,
            is_matrix,
            ..
        } => Ok(rumoca_core::Expression::Array {
            elements: elements
                .iter()
                .map(|element| {
                    expression_from_ast_in_subscript(element, base, dimension, owner_span, context)
                })
                .collect::<LowerResult<Vec<_>>>()?,
            is_matrix: *is_matrix,
            span: expr.span(),
        }),
        // Nested component/index expressions establish their own nearest-array
        // context, so their `end` tokens are resolved by normal lowering.
        ast::Expression::ComponentReference(_) | ast::Expression::ArrayIndex { .. } => {
            expression_from_ast_with_context(expr, context)
        }
        _ => expression_from_ast_with_context(expr, context),
    }
}

fn end_subscript_expression(
    base: &rumoca_core::Expression,
    dimension: usize,
    span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> LowerResult<rumoca_core::Expression> {
    let dimension = i64::try_from(dimension)
        .ok()
        .and_then(|value| value.checked_add(1))
        .ok_or_else(|| {
            FlattenError::unsupported_equation(
                "array subscript dimension exceeds Modelica Integer range",
                owner_span,
            )
        })?;
    Ok(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            base.clone(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(dimension),
                span,
            },
        ],
        span,
    })
}

#[cfg(test)]
fn convert_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
) -> LowerResult<rumoca_core::Expression> {
    convert_function_call_with_context(
        comp,
        args,
        rumoca_core::FunctionCallKind::Invocation,
        comp.span,
        LoweringContext {
            instance_name: None,
            predefined_string_declaration: None,
            predefined_intrinsics: PredefinedIntrinsicIds::default(),
        },
    )
}

fn convert_function_call_with_context(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    call_kind: rumoca_core::FunctionCallKind,
    call_span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    if call_kind == rumoca_core::FunctionCallKind::PartialApplication {
        return lower_user_function_call(comp, args, call_kind, call_span, context);
    }
    if is_get_instance_name_call(comp) {
        return lower_get_instance_name_call(args, context, call_span);
    }

    if comp.parts.len() == 1 {
        let func_name = &comp.parts[0].ident.text;
        if func_name.as_ref() == rumoca_core::PURITY_WRAPPER {
            return lower_purity_wrapper(args, call_span, context);
        }
        if comp.target_def_id() == context.predefined_string_declaration
            && context.predefined_string_declaration.is_some()
        {
            return lower_string_conversion(comp, args, call_span, context);
        }
        if let Some(intrinsic) = context.predefined_intrinsics.resolve(comp.target_def_id()) {
            return Ok(rumoca_core::Expression::BuiltinCall {
                function: intrinsic,
                args: lower_builtin_call_args(intrinsic, args, call_span, context)?,
                span: call_span,
            });
        }
        if let Some(builtin) = rumoca_core::BuiltinFunction::from_name(func_name) {
            if builtin.requires_predefined_identity() {
                return lower_user_function_call(comp, args, call_kind, call_span, context);
            }
            return Ok(rumoca_core::Expression::BuiltinCall {
                function: builtin,
                args: lower_builtin_call_args(builtin, args, call_span, context)?,
                span: call_span,
            });
        }
    }

    lower_user_function_call(comp, args, call_kind, call_span, context)
}

/// Erase an MLS §12.3 `pure(functionCall(…))` wrapper.
///
/// The wrapper "only by-passes the purity checking of the callee
/// impureFunction; the argument expressions of the function call are not
/// affected" (MLS 3.7 §12.3): it computes nothing, so Flat carries the call it
/// wraps and nothing else. Resolve already suppressed the one purity check the
/// wrapper exists for, and it has no second meaning to preserve here.
///
/// The grammar admits `pure(…)` with any argument list, so a wrapper that does
/// not wrap exactly one expression is rejected with its own span rather than
/// lowered to something invented.
fn lower_purity_wrapper(
    args: &[ast::Expression],
    call_span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let [wrapped] = args else {
        return Err(FlattenError::invalid_function_call_args(
            rumoca_core::PURITY_WRAPPER,
            format!(
                "MLS §12.3 `pure(…)` wraps exactly one function call; this call passes {} \
                 argument(s)",
                args.len()
            ),
            call_span,
        ));
    };
    expression_from_ast_with_context(wrapped, context)
}

fn lower_user_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    call_kind: rumoca_core::FunctionCallKind,
    call_span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let function_ref = reference_from_ast_component_ref(comp, context)?;

    Ok(rumoca_core::Expression::FunctionCall {
        name: function_ref,
        args: args
            .iter()
            .map(|a| convert_call_arg_with_context(a, context))
            .collect::<LowerResult<Vec<_>>>()?,
        is_constructor: false,
        call_kind,
        span: call_span,
    })
}

/// Build the positional argument vector for a builtin-operator call.
///
/// `BuiltinCall` args are consumed positionally by the builtin evaluator, which
/// has no step that later resolves named actuals: unlike a user function, a
/// builtin owns no collected signature carrying `__rumoca_named_arg__` markers.
/// Named actuals must therefore be projected into declaration order here, at the
/// only site with both the actuals and the operator's identity in hand.
///
/// A call with no named actual keeps its source order untouched, so the common
/// case never pays for slot bookkeeping.
fn lower_builtin_call_args(
    builtin: rumoca_core::BuiltinFunction,
    args: &[ast::Expression],
    call_span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<Vec<rumoca_core::Expression>> {
    if !args
        .iter()
        .any(|arg| matches!(arg, ast::Expression::NamedArgument { .. }))
    {
        return args
            .iter()
            .map(|arg| expression_from_ast_with_context(arg, context))
            .collect();
    }
    resolve_builtin_named_arguments(builtin, args, call_span, context)
}

/// Project a builtin call's positional and named actuals into declaration order
/// against the operator's MLS-defined formal names.
///
/// Positional actuals form a prefix; named actuals fill by name. The result is
/// the dense positional prefix the builtin evaluator expects. Arity is left to
/// the shared builtin signature (`argument_count_range`), so under-supply that
/// leaves a trailing slot empty simply yields a shorter vector rather than an
/// invented default; a named actual that fills a slot past an unfilled earlier
/// one is a gap the operator cannot default and is refused.
fn resolve_builtin_named_arguments(
    builtin: rumoca_core::BuiltinFunction,
    args: &[ast::Expression],
    call_span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<Vec<rumoca_core::Expression>> {
    let formals = builtin.named_formals();
    if formals.is_empty() {
        let named = args
            .iter()
            .find(|arg| matches!(arg, ast::Expression::NamedArgument { .. }))
            .expect("a named actual is present because the caller routed here");
        let name = match named {
            ast::Expression::NamedArgument { name, .. } => name.text.as_ref(),
            _ => unreachable!("filtered to a named argument"),
        };
        return Err(FlattenError::invalid_function_call_args(
            builtin.name(),
            format!(
                "builtin operator `{}` takes only positional arguments; `{name}` was passed by name",
                builtin.name()
            ),
            named.span(),
        ));
    }

    let mut slots: Vec<Option<rumoca_core::Expression>> =
        (0..formals.len()).map(|_| None).collect();
    let mut next_positional = 0usize;
    let mut seen_named = false;
    for argument in args {
        if let ast::Expression::NamedArgument { name, value, .. } = argument {
            seen_named = true;
            let Some(slot) = formals
                .iter()
                .position(|formal| *formal == name.text.as_ref())
            else {
                return Err(FlattenError::invalid_function_call_args(
                    builtin.name(),
                    format!(
                        "`{}` is not a named formal of builtin operator `{}`",
                        name.text,
                        builtin.name()
                    ),
                    argument.span(),
                ));
            };
            if slots[slot]
                .replace(expression_from_ast_with_context(value, context)?)
                .is_some()
            {
                return Err(FlattenError::invalid_function_call_args(
                    builtin.name(),
                    format!("named argument `{}` is supplied more than once", name.text),
                    argument.span(),
                ));
            }
            continue;
        }
        // The grammar admits named arguments only as a trailing suffix, so a
        // positional actual after a named one cannot be parsed; this refusal is
        // a defensive backstop for a hand-built or future AST rather than a case
        // the parser can currently reach.
        if seen_named {
            return Err(FlattenError::invalid_function_call_args(
                builtin.name(),
                "a positional argument may not follow a named argument",
                argument.span(),
            ));
        }
        if next_positional >= formals.len() {
            return Err(FlattenError::invalid_function_call_args(
                builtin.name(),
                format!(
                    "{} positional argument(s) exceed the {} named formal(s) of builtin operator `{}`",
                    next_positional + 1,
                    formals.len(),
                    builtin.name()
                ),
                argument.span(),
            ));
        }
        slots[next_positional] = Some(expression_from_ast_with_context(argument, context)?);
        next_positional += 1;
    }

    let mut positional = Vec::with_capacity(formals.len());
    let mut gap_at: Option<usize> = None;
    for (index, slot) in slots.into_iter().enumerate() {
        match (slot, gap_at) {
            (Some(value), None) => positional.push(value),
            (Some(_), Some(gap)) => {
                return Err(FlattenError::invalid_function_call_args(
                    builtin.name(),
                    format!(
                        "named argument fills slot `{}` while the earlier slot `{}` has no \
                         argument; builtin operators have no default arguments",
                        formals[index], formals[gap]
                    ),
                    call_span,
                ));
            }
            (None, _) => {
                gap_at.get_or_insert(index);
            }
        }
    }
    Ok(positional)
}

fn lower_string_conversion(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    call_span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let declaration = comp.target_def_id().ok_or_else(|| {
        FlattenError::unsupported_equation(
            "predefined String conversion is missing its resolved declaration identity",
            call_span,
        )
    })?;
    let Some((value, named)) = args.split_first() else {
        return Err(FlattenError::unsupported_equation(
            "String() requires one scalar value argument",
            call_span,
        ));
    };
    if matches!(value, ast::Expression::NamedArgument { .. }) {
        return Err(FlattenError::unsupported_equation(
            "String() first argument must be positional",
            value.span(),
        ));
    }

    let mut minimum_length = None;
    let mut left_justified = None;
    let mut significant_digits = None;
    let mut explicit_format = None;
    for argument in named {
        let ast::Expression::NamedArgument { name, value, .. } = argument else {
            return Err(FlattenError::unsupported_equation(
                "String() formatting arguments must be named",
                argument.span(),
            ));
        };
        let lowered = Box::new(expression_from_ast_with_context(value, context)?);
        let slot = match name.text.as_ref() {
            "minimumLength" => &mut minimum_length,
            "leftJustified" => &mut left_justified,
            "significantDigits" => &mut significant_digits,
            "format" => &mut explicit_format,
            unknown => {
                return Err(FlattenError::unsupported_equation(
                    format!("String() has no named argument `{unknown}`"),
                    argument.span(),
                ));
            }
        };
        if slot.replace(lowered).is_some() {
            return Err(FlattenError::unsupported_equation(
                format!(
                    "String() named argument `{}` is specified more than once",
                    name.text
                ),
                argument.span(),
            ));
        }
    }

    let format = match explicit_format {
        Some(value)
            if minimum_length.is_none()
                && left_justified.is_none()
                && significant_digits.is_none() =>
        {
            rumoca_core::StringConversionFormat::Format { value }
        }
        Some(_) => {
            return Err(FlattenError::unsupported_equation(
                "String() `format` is mutually exclusive with minimumLength, leftJustified, and significantDigits",
                call_span,
            ));
        }
        None => rumoca_core::StringConversionFormat::Options {
            minimum_length,
            left_justified,
            significant_digits,
        },
    };
    Ok(rumoca_core::Expression::StringConversion {
        declaration,
        value: Box::new(expression_from_ast_with_context(value, context)?),
        format,
        span: call_span,
    })
}

fn is_get_instance_name_call(comp: &ast::ComponentReference) -> bool {
    comp.parts.len() == 1 && comp.parts[0].ident.text.as_ref() == "getInstanceName"
}

fn lower_get_instance_name_call(
    args: &[ast::Expression],
    context: LoweringContext<'_>,
    span: Span,
) -> LowerResult<rumoca_core::Expression> {
    if !args.is_empty() {
        return Err(FlattenError::unsupported_equation(
            "getInstanceName() takes no arguments",
            span,
        ));
    }
    let instance_name = context.instance_name.ok_or_else(|| {
        FlattenError::unsupported_equation(
            "getInstanceName() requires a model/block instance scope",
            span,
        )
    })?;
    Ok(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(instance_name.to_string()),
        span,
    })
}

fn convert_terminal(
    terminal_type: &ast::TerminalType,
    token: &rumoca_core::Token,
    span: rumoca_core::Span,
) -> LowerResult<rumoca_core::Literal> {
    match terminal_type {
        ast::TerminalType::UnsignedReal => Ok(token
            .text
            .parse()
            .map(rumoca_core::Literal::Real)
            .map_err(|_| {
            FlattenError::malformed_numeric_literal(token.text.to_string(), span)
        })?),
        ast::TerminalType::UnsignedInteger => Ok(token
            .text
            .parse()
            .map(rumoca_core::Literal::Integer)
            .map_err(|_| FlattenError::malformed_numeric_literal(token.text.to_string(), span))?),
        ast::TerminalType::Bool => Ok(rumoca_core::Literal::Boolean(
            token.text.eq_ignore_ascii_case("true"),
        )),
        ast::TerminalType::String => Ok(rumoca_core::Literal::String(strip_quotes(&token.text))),
        ast::TerminalType::End => Err(FlattenError::unsupported_equation(
            "`end` is only valid inside an array subscript with a known base dimension",
            span,
        )),
        ast::TerminalType::Empty => Err(FlattenError::InvalidLiteralTerminal {
            description: "empty AST terminal".to_string(),
            span,
        }),
    }
}

fn convert_comprehension_indices(
    indices: &[ast::ForIndex],
    context: LoweringContext<'_>,
) -> LowerResult<Vec<rumoca_core::ComprehensionIndex>> {
    indices
        .iter()
        .map(|index| {
            Ok(rumoca_core::ComprehensionIndex {
                name: index.ident.text.to_string(),
                range: expression_from_ast_with_context(&index.range, context)?,
            })
        })
        .collect()
}

fn convert_expr_vec_with_context(
    exprs: &[ast::Expression],
    context: LoweringContext<'_>,
) -> LowerResult<Vec<rumoca_core::Expression>> {
    exprs
        .iter()
        .map(|expr| expression_from_ast_with_context(expr, context))
        .collect()
}

fn convert_if_with_context(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    Ok(rumoca_core::Expression::If {
        branches: branches
            .iter()
            .map(|(cond, then_expr)| {
                Ok((
                    expression_from_ast_with_context(cond, context)?,
                    expression_from_ast_with_context(then_expr, context)?,
                ))
            })
            .collect::<LowerResult<Vec<_>>>()?,
        else_branch: Box::new(expression_from_ast_with_context(else_branch, context)?),
        span,
    })
}

fn convert_array_comprehension_with_context(
    expr: &ast::Expression,
    indices: &[ast::ForIndex],
    filter: &Option<std::sync::Arc<ast::Expression>>,
    span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    Ok(rumoca_core::Expression::ArrayComprehension {
        expr: Box::new(expression_from_ast_with_context(expr, context)?),
        indices: convert_comprehension_indices(indices, context)?,
        filter: filter
            .as_ref()
            .map(|cond| expression_from_ast_with_context(cond, context).map(Box::new))
            .transpose()?,
        span,
    })
}

fn wrap_named_constructor_arg(
    name: &str,
    value: rumoca_core::Expression,
    span: Span,
) -> LowerResult<rumoca_core::Expression> {
    let span = required_ast_span(span, "named constructor argument")?;
    Ok(rumoca_core::Expression::FunctionCall {
        name: Reference::generated(format!("{}{name}", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)),
        args: vec![value],
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    })
}

fn convert_call_arg_with_context(
    expr: &ast::Expression,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    match expr {
        ast::Expression::NamedArgument { name, value, .. } => wrap_named_constructor_arg(
            &name.text,
            expression_from_ast_with_context(value, context)?,
            expr.span(),
        ),
        // A value-less modification is not a call argument; it falls through
        // to the general lowering, which refuses a bare modification carrier.
        ast::Expression::Modification {
            target,
            value: Some(value),
            ..
        } => {
            let arg_name = target
                .parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".");
            wrap_named_constructor_arg(
                &arg_name,
                expression_from_ast_with_context(value, context)?,
                expr.span(),
            )
        }
        _ => expression_from_ast_with_context(expr, context),
    }
}

fn convert_class_modification_with_context(
    target: &ast::ComponentReference,
    modifications: &[ast::Expression],
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let target_span = required_ast_span(target.span, "class modification target")?;
    let constructor_ref = reference_from_ast_component_ref(target, context)?;
    Ok(rumoca_core::Expression::FunctionCall {
        name: constructor_ref,
        args: modifications
            .iter()
            .map(|expr| convert_call_arg_with_context(expr, context))
            .collect::<LowerResult<Vec<_>>>()?,
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span: target_span,
    })
}

fn strip_quotes(text: &str) -> String {
    if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
        text[1..text.len() - 1].to_string()
    } else {
        text.to_string()
    }
}
