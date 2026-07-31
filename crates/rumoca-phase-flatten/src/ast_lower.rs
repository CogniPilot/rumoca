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
        }
    }

    fn resolve(self, target: Option<DefId>) -> Option<rumoca_core::BuiltinFunction> {
        let target = target?;
        self.identities
            .into_iter()
            .zip(rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED)
            .find_map(|(identity, intrinsic)| (identity == Some(target)).then_some(*intrinsic))
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
        ast::Expression::Empty { span } => Ok(rumoca_core::Expression::Empty { span: *span }),

        ast::Expression::Binary { op, lhs, rhs, .. } => Ok(rumoca_core::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(expression_from_ast_with_context(lhs, context)?),
            rhs: Box::new(expression_from_ast_with_context(rhs, context)?),
            span: expr.span(),
        }),

        ast::Expression::Unary { op, rhs, .. } => Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(expression_from_ast_with_context(rhs, context)?),
            span: expr.span(),
        }),

        ast::Expression::ComponentReference(cr) => {
            expression_from_component_ref_with_context(cr, context)
        }

        ast::Expression::FunctionCall {
            comp, args, span, ..
        } => convert_function_call_with_context(comp, args, *span, context),

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
        } => Ok(rumoca_core::Expression::Range {
            start: Box::new(expression_from_ast_with_context(start, context)?),
            step: step
                .as_ref()
                .map(|s| expression_from_ast_with_context(s, context).map(Box::new))
                .transpose()?,
            end: Box::new(expression_from_ast_with_context(end, context)?),
            span: expr.span(),
        }),

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

        ast::Expression::NamedArgument { value, .. } => {
            expression_from_ast_with_context(value, context)
        }

        ast::Expression::Modification { value, .. } => {
            expression_from_ast_with_context(value, context)
        }

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
        ast::Statement::Empty => Ok(rumoca_core::Statement::Empty { span }),
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
        } => Ok(rumoca_core::Statement::FunctionCall {
            comp: function_component_ref_from_ast(comp, context)?,
            args: args
                .iter()
                .map(|arg| expression_from_ast_with_context(arg, context))
                .collect::<LowerResult<Vec<_>>>()?,
            outputs: outputs
                .iter()
                .map(|output| output_component_reference_from_ast(output, context))
                .collect::<LowerResult<Vec<_>>>()?,
            span,
        }),
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
    context: LoweringContext<'_>,
) -> LowerResult<Option<rumoca_core::ComponentReference>> {
    match expr {
        ast::Expression::ComponentReference(comp) => Ok(Some(
            component_reference_from_ast_with_context(comp, context)?,
        )),
        _ => Ok(None),
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
    span.require_provenance(context)
        .map(|provenance| provenance.span())
        .map_err(|err| FlattenError::missing_source_context(err.to_string()))
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
        ast::Subscript::Range { .. } | ast::Subscript::Empty => Ok(
            rumoca_core::Subscript::try_generated_colon(owner_span, "flat component subscript")
                .map_err(|err| FlattenError::missing_source_context(err.to_string()))?,
        ),
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
        ast::Subscript::Range { .. } | ast::Subscript::Empty => {
            Ok(rumoca_core::Subscript::colon(owner_span))
        }
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
    call_span: Span,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    if is_get_instance_name_call(comp) {
        return lower_get_instance_name_call(args, context, call_span);
    }

    if comp.parts.len() == 1 {
        let func_name = &comp.parts[0].ident.text;
        if comp.target_def_id() == context.predefined_string_declaration
            && context.predefined_string_declaration.is_some()
        {
            return lower_string_conversion(comp, args, call_span, context);
        }
        if let Some(intrinsic) = context.predefined_intrinsics.resolve(comp.target_def_id()) {
            return Ok(rumoca_core::Expression::BuiltinCall {
                function: intrinsic,
                args: args
                    .iter()
                    .map(|argument| expression_from_ast_with_context(argument, context))
                    .collect::<LowerResult<Vec<_>>>()?,
                span: call_span,
            });
        }
        if let Some(builtin) = rumoca_core::BuiltinFunction::from_name(func_name) {
            if builtin.requires_predefined_identity() {
                return lower_user_function_call(comp, args, call_span, context);
            }
            return Ok(rumoca_core::Expression::BuiltinCall {
                function: builtin,
                args: args
                    .iter()
                    .map(|a| expression_from_ast_with_context(a, context))
                    .collect::<LowerResult<Vec<_>>>()?,
                span: call_span,
            });
        }
    }

    lower_user_function_call(comp, args, call_span, context)
}

fn lower_user_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
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
        span: call_span,
    })
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
        ast::TerminalType::Empty => Ok(rumoca_core::Literal::Integer(0)),
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

pub(crate) const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

fn wrap_named_constructor_arg(
    name: &str,
    value: rumoca_core::Expression,
    span: Span,
) -> LowerResult<rumoca_core::Expression> {
    let span = required_ast_span(span, "named constructor argument")?;
    Ok(rumoca_core::Expression::FunctionCall {
        name: Reference::generated(format!("{NAMED_CONSTRUCTOR_ARG_PREFIX}{name}")),
        args: vec![value],
        is_constructor: true,
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
        ast::Expression::Modification { target, value, .. } => {
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn test_span() -> Span {
        span_at(1, 2)
    }

    fn span_at(start: usize, end: usize) -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("ast_lower_test.mo"),
            start,
            end,
        )
    }

    fn test_def_id(name: &str) -> DefId {
        let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
            hash.wrapping_mul(16_777_619) ^ u32::from(byte)
        });
        DefId::new(hash.max(1))
    }

    fn part(name: &str) -> ast::ComponentRefPart {
        ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from(name),
                ..rumoca_core::Token::default()
            },
            subs: None,
            def_id: Some(test_def_id(name)),
        }
    }

    fn component_ref(names: &[&str]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: names.iter().map(|name| part(name)).collect(),
            span: test_span(),
            qualified_display_name: None,
        }
    }

    fn ast_var(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![part(name)],
            span: test_span(),
            qualified_display_name: None,
        })
    }

    fn ast_var_with_span(name: &str, span: Span) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![part(name)],
            span,
            qualified_display_name: None,
        })
    }

    fn function_ref(name: &str) -> ast::ComponentReference {
        component_ref(&[name])
    }

    fn resolved_function_ref(name: &str, target: DefId) -> ast::ComponentReference {
        let mut reference = function_ref(name);
        reference.set_target_def_id(Some(target));
        reference
    }

    fn integer(value: i64, span: Span) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: Arc::from(value.to_string()),
                ..rumoca_core::Token::default()
            },
            span,
        }
    }

    #[test]
    fn scalar_lowering_preserves_identity_and_each_source_span() {
        let reference_span = span_at(3, 4);
        let literal_span = span_at(7, 9);
        let binary_span = span_at(3, 9);
        let reference = ast_var_with_span("x", reference_span);
        let expected_id = match &reference {
            ast::Expression::ComponentReference(reference) => reference.target_def_id().unwrap(),
            _ => unreachable!(),
        };
        let expression = ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Arc::new(reference),
            rhs: Arc::new(integer(2, literal_span)),
            span: binary_span,
        };

        let lowered = expression_from_ast(&expression).unwrap();
        let rumoca_core::Expression::Binary { lhs, rhs, span, .. } = lowered else {
            panic!("expected binary expression");
        };
        assert_eq!(span, binary_span);
        let rumoca_core::Expression::VarRef { name, span, .. } = lhs.as_ref() else {
            panic!("expected exact variable reference");
        };
        assert_eq!(*span, reference_span);
        assert_eq!(
            name.component_ref().map(ComponentReference::target_def_id),
            Some(expected_id)
        );
        assert!(matches!(
            rhs.as_ref(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span,
            } if *span == literal_span
        ));
    }

    #[test]
    fn derivative_lowering_is_structurally_discoverable() {
        let derivative_span = span_at(10, 16);
        let call = ast::Expression::FunctionCall {
            comp: function_ref("der"),
            args: vec![ast_var_with_span("x", span_at(14, 15))],
            is_partial_application: false,
            span: derivative_span,
        };

        let lowered = expression_from_ast(&call).unwrap();
        assert_eq!(lowered.span(), Some(derivative_span));
        assert!(lowered.contains_der());
        assert_eq!(
            lowered.get_der_variable().map(|name| name.as_str()),
            Some("x")
        );
        let mut states = Vec::new();
        lowered.collect_state_variables(&mut states);
        assert_eq!(states, vec![rumoca_core::VarName::new("x")]);
    }

    #[test]
    fn constructor_lowering_requires_identity_and_preserves_named_argument_span() {
        let constructor_span = span_at(20, 42);
        let argument_span = span_at(31, 41);
        let mut target = component_ref(&["Alias", "Record"]);
        let target_id = DefId::new(77);
        target.set_target_def_id(Some(target_id));
        target.span = constructor_span;
        target.set_qualified_display_name("Pkg.Record");
        let expression = ast::Expression::ClassModification {
            target,
            modifications: vec![ast::Expression::NamedArgument {
                name: rumoca_core::Token {
                    text: Arc::from("value"),
                    ..rumoca_core::Token::default()
                },
                value: Arc::new(integer(3, span_at(39, 40))),
                span: argument_span,
            }],
            each_flags: vec![false],
            final_flags: vec![false],
            redeclare_flags: vec![false],
            span: constructor_span,
        };

        let lowered = expression_from_ast(&expression).unwrap();
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = lowered
        else {
            panic!("expected constructor call");
        };
        assert!(is_constructor);
        assert_eq!(span, constructor_span);
        assert_eq!(name.as_str(), "Pkg.Record");
        assert_eq!(
            name.component_ref().map(ComponentReference::target_def_id),
            Some(target_id)
        );
        let [
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: true,
                span,
            },
        ] = args.as_slice()
        else {
            panic!("expected generated named-argument wrapper");
        };
        assert_eq!(name.as_str(), "__rumoca_named_arg__.value");
        assert!(name.is_generated());
        assert_eq!(*span, argument_span);
        assert!(matches!(
            args.as_slice(),
            [rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(3),
                ..
            }]
        ));

        let mut missing = component_ref(&["Missing"]);
        missing.set_target_def_id(None);
        let error = expression_from_ast(&ast::Expression::ClassModification {
            target: missing,
            modifications: Vec::new(),
            each_flags: Vec::new(),
            final_flags: Vec::new(),
            redeclare_flags: Vec::new(),
            span: constructor_span,
        })
        .unwrap_err();
        assert!(matches!(
            error,
            FlattenError::MissingFlatVariableIdentity { .. }
        ));

        let error = expression_from_ast(&ast::Expression::ComponentReference(
            ast::ComponentReference {
                local: false,
                parts: Vec::new(),
                span: constructor_span,
                qualified_display_name: None,
            },
        ))
        .unwrap_err();
        assert!(matches!(
            error,
            FlattenError::MissingFlatVariableIdentity { ref name, span }
                if name.contains("requires at least one identity-bearing part")
                    && span == constructor_span
        ));
    }

    #[test]
    fn comprehension_lowering_preserves_structure_and_owner_provenance() {
        let owner_span = span_at(50, 80);
        let body_span = span_at(51, 52);
        let range_span = span_at(60, 64);
        let filter_span = span_at(70, 75);
        let expression = ast::Expression::ArrayComprehension {
            expr: Arc::new(ast_var_with_span("x", body_span)),
            indices: vec![ast::ForIndex {
                ident: rumoca_core::Token {
                    text: Arc::from("i"),
                    ..rumoca_core::Token::default()
                },
                range: ast::Expression::Range {
                    start: Arc::new(integer(1, span_at(60, 61))),
                    step: None,
                    end: Arc::new(integer(4, span_at(63, 64))),
                    span: range_span,
                },
            }],
            filter: Some(Arc::new(ast_var_with_span("enabled", filter_span))),
            span: owner_span,
        };

        let lowered = expression_from_ast(&expression).unwrap();
        let rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            span,
        } = lowered
        else {
            panic!("expected array comprehension");
        };
        assert_eq!(span, owner_span);
        assert_eq!(expr.span(), Some(body_span));
        assert_eq!(indices.len(), 1);
        assert_eq!(indices[0].name, "i");
        assert_eq!(indices[0].range.span(), Some(range_span));
        assert_eq!(filter.expect("filter").span(), Some(filter_span));
    }

    #[test]
    fn subscript_lowering_folds_arithmetic_and_retains_dynamic_identity() {
        let arithmetic = ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Arc::new(integer(2, span_at(82, 83))),
            rhs: Arc::new(integer(3, span_at(84, 85))),
            span: span_at(82, 85),
        };
        let i = ast_var_with_span("i", span_at(87, 88));
        let j = ast_var_with_span("j", span_at(90, 91));
        let expected_dynamic_ids = [&i, &j].map(|expression| match expression {
            ast::Expression::ComponentReference(reference) => reference.target_def_id().unwrap(),
            _ => unreachable!(),
        });
        let mut indexed = part("a");
        indexed.subs = Some(vec![
            ast::Subscript::Expression(arithmetic),
            ast::Subscript::Expression(i),
            ast::Subscript::Expression(j),
        ]);
        let expression = ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![indexed],
            span: span_at(81, 92),
            qualified_display_name: None,
        });

        let lowered = expression_from_ast(&expression).unwrap();
        let rumoca_core::Expression::Index { subscripts, .. } = lowered else {
            panic!("expected indexed reference");
        };
        assert!(matches!(
            subscripts.first(),
            Some(rumoca_core::Subscript::Index { value: 5, .. })
        ));
        for (subscript, expected_id) in subscripts[1..].iter().zip(expected_dynamic_ids) {
            let rumoca_core::Subscript::Expr { expr, .. } = subscript else {
                panic!("expected dynamic subscript");
            };
            let rumoca_core::Expression::VarRef { name, .. } = expr.as_ref() else {
                panic!("expected dynamic exact reference");
            };
            assert_eq!(
                name.component_ref().map(ComponentReference::target_def_id),
                Some(expected_id)
            );
        }
    }

    #[test]
    fn interval_requires_the_exact_predefined_declaration_identity() {
        let predefined_interval = DefId::new(40);
        let shadowed_interval = DefId::new(41);
        let mut identities =
            [None; rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED.len()];
        let interval = rumoca_core::BuiltinFunction::PREDEFINED_IDENTITY_REQUIRED
            .iter()
            .position(|builtin| *builtin == rumoca_core::BuiltinFunction::Interval)
            .expect("Interval requires predefined identity");
        identities[interval] = Some(predefined_interval);
        let context = LoweringContext {
            predefined_intrinsics: PredefinedIntrinsicIds { identities },
            ..LoweringContext::default()
        };

        let predefined = convert_function_call_with_context(
            &resolved_function_ref("interval", predefined_interval),
            &[ast_var("u")],
            test_span(),
            context,
        )
        .unwrap();
        assert!(matches!(
            predefined,
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Interval,
                ..
            }
        ));

        let shadowed = convert_function_call_with_context(
            &resolved_function_ref("interval", shadowed_interval),
            &[ast_var("u")],
            test_span(),
            context,
        )
        .unwrap();
        assert!(matches!(
            shadowed,
            rumoca_core::Expression::FunctionCall { .. }
        ));

        let mut indexed = part("a");
        indexed.subs = Some(vec![ast::Subscript::Expression(
            ast::Expression::FunctionCall {
                comp: resolved_function_ref("interval", predefined_interval),
                args: vec![ast_var("u")],
                is_partial_application: false,
                span: test_span(),
            },
        )]);
        let indexed = expression_from_component_ref_with_context(
            &ast::ComponentReference {
                local: false,
                parts: vec![indexed],
                span: test_span(),
                qualified_display_name: None,
            },
            context,
        )
        .unwrap();
        let rumoca_core::Expression::Index { subscripts, .. } = indexed else {
            panic!("expected indexed expression");
        };
        assert!(matches!(
            &subscripts[0],
            rumoca_core::Subscript::Expr { expr, .. }
                if matches!(
                    expr.as_ref(),
                    rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Interval,
                        ..
                    }
                )
        ));
    }

    #[test]
    fn unresolved_interval_spelling_never_mints_a_predefined_intrinsic() {
        let lowered = convert_function_call_with_context(
            &function_ref("interval"),
            &[ast_var("u")],
            test_span(),
            LoweringContext::default(),
        )
        .unwrap();

        assert!(matches!(
            lowered,
            rumoca_core::Expression::FunctionCall { .. }
        ));
    }

    #[test]
    fn get_instance_name_lowers_to_instance_string_literal() {
        let expr = convert_function_call_with_context(
            &function_ref("getInstanceName"),
            &[],
            test_span(),
            LoweringContext {
                instance_name: Some("Vehicle.engine.controller"),
                predefined_string_declaration: None,
                predefined_intrinsics: PredefinedIntrinsicIds::default(),
            },
        )
        .unwrap();

        let rumoca_core::Expression::Literal { value, .. } = expr else {
            panic!("expected literal");
        };
        assert_eq!(
            value,
            rumoca_core::Literal::String("Vehicle.engine.controller".to_string())
        );
    }

    #[test]
    fn function_call_lowering_preserves_the_ast_call_span() {
        let call_span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("ast_lower_test.mo"),
            10,
            24,
        );
        let expression = ast::Expression::FunctionCall {
            comp: function_ref("previous"),
            args: vec![ast_var("x")],
            is_partial_application: false,
            span: call_span,
        };
        let lowered = expression_from_ast(&expression).unwrap();
        assert_eq!(lowered.span(), Some(call_span));
    }

    #[test]
    fn get_instance_name_requires_instance_scope() {
        let err = convert_function_call_with_context(
            &function_ref("getInstanceName"),
            &[],
            test_span(),
            LoweringContext::default(),
        )
        .unwrap_err();

        assert!(
            err.to_string()
                .contains("requires a model/block instance scope")
        );
    }

    #[test]
    fn get_instance_name_rejects_arguments() {
        let err = convert_function_call_with_context(
            &function_ref("getInstanceName"),
            &[ast_var("x")],
            test_span(),
            LoweringContext {
                instance_name: Some("Vehicle.engine.controller"),
                predefined_string_declaration: None,
                predefined_intrinsics: PredefinedIntrinsicIds::default(),
            },
        )
        .unwrap_err();

        assert!(err.to_string().contains("takes no arguments"));
    }

    #[test]
    fn function_call_lowering_keeps_exact_member_path_and_identity() {
        let receiver_def = DefId::new(1);
        let member_def = DefId::new(2);
        let mut comp = component_ref(&["receiver", "member"]);
        comp.set_root_def_id(Some(receiver_def));
        comp.set_target_def_id(Some(member_def));

        let expr = convert_function_call(&comp, &[]).unwrap();
        let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
            panic!("expected function call");
        };
        assert_eq!(name.as_str(), "receiver.member");
        let reference = name.component_ref().expect("structured function reference");
        assert_eq!(reference.root_def_id(), receiver_def);
        assert_eq!(reference.target_def_id(), member_def);
    }

    #[test]
    fn function_call_display_name_does_not_replace_structured_identity() {
        let function_def = DefId::new(2);
        let mut comp = component_ref(&["Receiver", "member"]);
        comp.set_target_def_id(Some(function_def));
        comp.set_qualified_display_name("Pkg.Receiver.member");

        let expr = convert_function_call(&comp, &[]).unwrap();
        let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
            panic!("expected function call");
        };
        assert_eq!(name.as_str(), "Pkg.Receiver.member");
        let reference = name.component_ref().expect("structured function reference");
        assert_eq!(reference.target_def_id(), function_def);
        assert_eq!(reference.parts()[0].ident.as_str(), "Receiver");
    }

    #[test]
    fn function_call_qualified_display_preserves_use_site_parts() {
        let function_def = DefId::new(4);
        let mut comp = component_ref(&["world", "gravityAcceleration"]);
        comp.set_target_def_id(Some(function_def));
        comp.set_qualified_display_name("Modelica.Mechanics.MultiBody.World.gravityAcceleration");

        let expr = convert_function_call(&comp, &[]).unwrap();
        let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
            panic!("expected function call");
        };
        assert_eq!(
            name.as_str(),
            "Modelica.Mechanics.MultiBody.World.gravityAcceleration"
        );
        let reference = name.component_ref().expect("structured function reference");
        assert_eq!(reference.parts()[0].ident.as_str(), "world");
        assert_eq!(reference.target_def_id(), function_def);
    }

    #[test]
    fn statement_lowering_uses_ast_assignment_span_without_source_map() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("algorithm_assignment_span.mo"),
            8,
            14,
        );
        let stmt = ast::Statement::Assignment {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![part("x")],
                span,
                qualified_display_name: None,
            },
            value: ast_var("y"),
        };

        let lowered = statement_from_ast(&stmt).unwrap();
        assert_eq!(lowered.source_span(), Some(span));
    }

    #[test]
    fn statement_lowering_uses_reference_span_when_prefix_token_location_is_generated() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("algorithm_prefixed_assignment_span.mo"),
            8,
            14,
        );
        let stmt = ast::Statement::Assignment {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![part("Model"), part("x")],
                span,
                qualified_display_name: None,
            },
            value: ast_var_with_span("y", span),
        };

        let lowered = statement_from_ast(&stmt).unwrap();
        assert_eq!(lowered.source_span(), Some(span));
    }

    #[test]
    fn statement_lowering_uses_ast_if_condition_span_without_source_map() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("algorithm_if_span.mo"),
            3,
            12,
        );
        let stmt = ast::Statement::If {
            cond_blocks: vec![ast::StatementBlock {
                cond: ast_var_with_span("condition", span),
                stmts: Vec::new(),
            }],
            else_block: None,
        };

        let lowered = statement_from_ast(&stmt).unwrap();
        assert_eq!(lowered.source_span(), Some(span));
    }

    #[test]
    fn function_call_lowering_keeps_concrete_path_when_def_id_names_constraint() {
        let partial_function_def = DefId::new(3);
        let mut comp = component_ref(&[
            "Modelica",
            "Media",
            "Air",
            "ReferenceAir",
            "Air_pT",
            "specificEnthalpy",
        ]);
        comp.set_target_def_id(Some(partial_function_def));

        let expr = convert_function_call(&comp, &[]).unwrap();
        let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
            panic!("expected function call");
        };
        assert_eq!(
            name.as_str(),
            "Modelica.Media.Air.ReferenceAir.Air_pT.specificEnthalpy"
        );
    }

    #[test]
    fn dynamic_final_subscript_keeps_local_index_base() {
        let variable_def = DefId::new(3);
        let comp = ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: Arc::from("leg_v_b"),
                    ..rumoca_core::Token::default()
                },
                subs: Some(vec![
                    ast::Subscript::Empty,
                    ast::Subscript::Expression(ast_var("i")),
                ]),
                def_id: Some(variable_def),
            }],
            span: test_span(),
            qualified_display_name: None,
        };

        let expr =
            expression_from_component_ref_with_context(&comp, LoweringContext::default()).unwrap();
        let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = expr
        else {
            panic!("expected dynamic final subscript to lower as structured index");
        };
        let rumoca_core::Expression::VarRef { name, .. } = base.as_ref() else {
            panic!("expected indexed base to remain a variable reference");
        };

        assert_eq!(name.as_str(), "leg_v_b");
        assert_eq!(subscripts.len(), 2);
    }

    #[test]
    fn end_subscript_lowers_to_size_of_selected_base_dimension() {
        let mut indexed = part("v");
        indexed.subs = Some(vec![ast::Subscript::Expression(
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::End,
                token: rumoca_core::Token {
                    text: Arc::from("end"),
                    ..rumoca_core::Token::default()
                },
                span: test_span(),
            },
        )]);
        let comp = ast::ComponentReference {
            local: false,
            parts: vec![indexed],
            span: test_span(),
            qualified_display_name: None,
        };

        let lowered = expression_from_component_ref_with_context(&comp, LoweringContext::default())
            .expect("end should lower in a valid subscript context");
        let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = lowered
        else {
            panic!("expected indexed expression");
        };
        let [rumoca_core::Subscript::Expr { expr, .. }] = subscripts.as_slice() else {
            panic!("expected expression subscript");
        };
        let rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            ..
        } = expr.as_ref()
        else {
            panic!("end should become size(base, dimension)");
        };
        assert_eq!(args[0], *base);
        assert!(matches!(
            args[1],
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                ..
            }
        ));
    }

    #[test]
    fn end_outside_subscript_is_rejected_instead_of_becoming_zero() {
        let expr = ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            token: rumoca_core::Token {
                text: Arc::from("end"),
                ..rumoca_core::Token::default()
            },
            span: test_span(),
        };

        let err = expression_from_ast(&expr).expect_err("bare end is invalid");
        assert!(
            err.to_string()
                .contains("only valid inside an array subscript"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn structured_subscript_base_carries_exact_final_target_for_flat_projection() {
        let mut fluid_constants = part("fluidConstants");
        let fluid_constants_def_id = fluid_constants.def_id.unwrap();
        fluid_constants.subs = Some(vec![ast::Subscript::Expression(
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token {
                    text: Arc::from("1"),
                    ..rumoca_core::Token::default()
                },
                span: test_span(),
            },
        )]);
        let mut comp = ast::ComponentReference {
            local: false,
            parts: vec![
                part("source"),
                part("medium"),
                fluid_constants,
                part("criticalTemperature"),
            ],
            span: test_span(),
            qualified_display_name: None,
        };
        comp.set_target_def_id(Some(DefId::new(77)));

        let expr =
            expression_from_component_ref_with_context(&comp, LoweringContext::default()).unwrap();
        let rumoca_core::Expression::FieldAccess { base, .. } = expr else {
            panic!("expected field access after indexed package constant");
        };
        let rumoca_core::Expression::Index { base, .. } = base.as_ref() else {
            panic!("expected indexed package constant base");
        };
        let rumoca_core::Expression::VarRef { name, .. } = base.as_ref() else {
            panic!("expected indexed base to be a variable reference");
        };

        assert_eq!(name.as_str(), "source.medium.fluidConstants");
        assert_eq!(name.target_def_id(), Some(fluid_constants_def_id));
    }
}
