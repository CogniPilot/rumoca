use rumoca_core::{
    ComponentRefPart, ComponentReference, Reference, SourceMap, split_path_with_indices,
    top_level_last_segment,
};
use rumoca_core::{DefId, Span};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

use crate::FlattenError;
use crate::static_subscripts::try_constant_integer;

type LowerResult<T> = Result<T, FlattenError>;

#[derive(Clone, Copy, Default)]
pub(crate) struct LoweringContext<'a> {
    pub(crate) def_map: Option<&'a IndexMap<DefId, String>>,
    pub(crate) instance_name: Option<&'a str>,
}

pub(crate) fn expression_from_ast(expr: &ast::Expression) -> LowerResult<rumoca_core::Expression> {
    expression_from_ast_with_def_map(expr, None)
}

pub(crate) fn expression_from_ast_with_def_map(
    expr: &ast::Expression,
    def_map: Option<&IndexMap<DefId, String>>,
) -> LowerResult<rumoca_core::Expression> {
    expression_from_ast_with_context(
        expr,
        LoweringContext {
            def_map,
            instance_name: None,
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
            expression_from_component_ref_with_def_map(cr, context.def_map)
        }

        ast::Expression::FunctionCall { comp, args, .. } => {
            convert_function_call_with_context(comp, args, context)
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
            expr,
            indices,
            filter,
            ..
        } => convert_array_comprehension_with_context(expr, indices, filter, expr.span(), context),

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
        } => {
            let base_flat = Box::new(expression_from_ast_with_context(base, context)?);
            let flat_subs = subscripts
                .iter()
                .map(subscript_from_ast)
                .collect::<LowerResult<Vec<_>>>()?;
            Ok(rumoca_core::Expression::Index {
                base: base_flat,
                subscripts: flat_subs,
                span: expr.span(),
            })
        }

        ast::Expression::FieldAccess { base, field, .. } => {
            Ok(rumoca_core::Expression::FieldAccess {
                base: Box::new(expression_from_ast_with_context(base, context)?),
                field: field.clone(),
                span: expr.span(),
            })
        }
    }
}

#[cfg(test)]
pub(crate) fn statement_from_ast_with_def_map(
    stmt: &ast::Statement,
    def_map: Option<&IndexMap<DefId, String>>,
) -> LowerResult<rumoca_core::Statement> {
    statement_from_ast_with_def_map_and_source_map(stmt, def_map, None)
}

#[cfg(test)]
pub(crate) fn statement_from_ast_with_def_map_and_source_map(
    stmt: &ast::Statement,
    def_map: Option<&IndexMap<DefId, String>>,
    source_map: Option<&SourceMap>,
) -> LowerResult<rumoca_core::Statement> {
    statement_from_ast_with_context_and_source_map(
        stmt,
        LoweringContext {
            def_map,
            instance_name: None,
        },
        source_map,
    )
}

pub(crate) fn statement_from_ast_with_context_and_source_map(
    stmt: &ast::Statement,
    context: LoweringContext<'_>,
    source_map: Option<&SourceMap>,
) -> LowerResult<rumoca_core::Statement> {
    let span = ast_statement_span(stmt, source_map);
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
            comp: component_reference_from_ast_with_def_map(comp, context.def_map)?,
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
            comp: function_component_ref_from_ast(comp, context.def_map)?,
            args: args
                .iter()
                .map(|arg| expression_from_ast_with_context(arg, context))
                .collect::<LowerResult<Vec<_>>>()?,
            outputs: outputs
                .iter()
                .map(|output| output_component_reference_from_ast(output, context.def_map))
                .collect::<LowerResult<Vec<_>>>()?
                .into_iter()
                .flatten()
                .collect(),
            span,
        }),
        ast::Statement::Reinit { variable, value } => Ok(rumoca_core::Statement::Reinit {
            variable: component_reference_from_ast_with_def_map(variable, context.def_map)?,
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
    def_map: Option<&IndexMap<DefId, String>>,
) -> LowerResult<Option<rumoca_core::ComponentReference>> {
    match expr {
        ast::Expression::ComponentReference(comp) => Ok(Some(
            component_reference_from_ast_with_def_map(comp, def_map)?,
        )),
        _ => Ok(None),
    }
}

fn ast_statement_span(stmt: &ast::Statement, source_map: Option<&SourceMap>) -> Span {
    let Some(location) = stmt.get_location() else {
        return Span::DUMMY;
    };
    source_map.map_or(Span::DUMMY, |map| {
        map.location_to_span(
            &location.file_name,
            location.start as usize,
            location.end as usize,
        )
    })
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

fn component_reference_from_ast_with_def_map(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> LowerResult<rumoca_core::ComponentReference> {
    if comp.parts.is_empty()
        && let Some(def_id) = comp.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return Ok(component_reference_from_path(path, comp.span, Some(def_id)));
    }

    Ok(rumoca_core::ComponentReference {
        local: comp.local,
        span: comp.span,
        parts: comp
            .parts
            .iter()
            .map(|part| {
                Ok(rumoca_core::ComponentRefPart {
                    ident: part.ident.text.to_string(),
                    span: comp.span,
                    subs: part
                        .subs
                        .as_ref()
                        .map(|subs| subs.iter().map(subscript_from_ast).collect())
                        .transpose()?
                        .unwrap_or_default(),
                })
            })
            .collect::<LowerResult<Vec<_>>>()?,
        def_id: comp.def_id,
    })
}

fn function_component_ref_from_ast(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> LowerResult<rumoca_core::ComponentReference> {
    if let Some(def_id) = comp.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return Ok(component_reference_from_path(path, comp.span, Some(def_id)));
    }

    component_reference_from_ast_with_def_map(comp, None)
}

fn component_reference_from_path(
    path: &str,
    span: rumoca_core::Span,
    def_id: Option<DefId>,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: split_path_with_indices(path)
            .into_iter()
            .map(|segment| rumoca_core::ComponentRefPart {
                ident: segment.to_string(),
                span,
                subs: Vec::new(),
            })
            .collect(),
        def_id,
    }
}

fn subscript_from_ast(sub: &ast::Subscript) -> LowerResult<rumoca_core::Subscript> {
    match sub {
        ast::Subscript::Expression(expr) => {
            let span = expr.span();
            if let Some(val) = try_constant_integer(expr) {
                return Ok(rumoca_core::Subscript::index(val, span));
            }
            Ok(rumoca_core::Subscript::expr(
                Box::new(expression_from_ast(expr)?),
                span,
            ))
        }
        ast::Subscript::Range { .. } | ast::Subscript::Empty => Ok(
            rumoca_core::Subscript::generated_colon(rumoca_core::Span::DUMMY),
        ),
    }
}

fn expression_from_component_ref_with_def_map(
    cr: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> LowerResult<rumoca_core::Expression> {
    if cr.parts.is_empty()
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        let component_ref = component_reference_from_path(path, cr.span, Some(def_id));
        return Ok(rumoca_core::Expression::VarRef {
            name: Reference::from_component_reference(component_ref),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        });
    }

    if cr.parts.iter().all(|part| part.subs.is_none())
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
        && is_enum_literal_ref(cr, path)
    {
        let component_ref = component_reference_from_path(path, cr.span, Some(def_id));
        return Ok(rumoca_core::Expression::VarRef {
            name: Reference::from_component_reference(component_ref),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        });
    }

    if component_ref_has_subscripts(cr) {
        return component_ref_with_structured_subscripts(cr);
    }

    expression_from_component_ref(cr)
}

fn expression_from_component_ref(
    cr: &ast::ComponentReference,
) -> LowerResult<rumoca_core::Expression> {
    let name = reference_from_ast_component_ref(cr)?;

    Ok(rumoca_core::Expression::VarRef {
        name,
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    })
}

fn component_ref_has_subscripts(cr: &ast::ComponentReference) -> bool {
    cr.parts
        .iter()
        .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
}

fn component_ref_with_structured_subscripts(
    cr: &ast::ComponentReference,
) -> LowerResult<rumoca_core::Expression> {
    let mut pending_name_parts = Vec::new();
    let mut current = None;

    for part in &cr.parts {
        let ident = part.ident.text.to_string();
        let Some(subs) = part.subs.as_ref().filter(|subs| !subs.is_empty()) else {
            if let Some(expr) = current.take() {
                current = Some(rumoca_core::Expression::FieldAccess {
                    base: Box::new(expr),
                    field: ident,
                    span: cr.span,
                });
            } else {
                pending_name_parts.push(ident);
            }
            continue;
        };

        let base = if let Some(expr) = current.take() {
            rumoca_core::Expression::FieldAccess {
                base: Box::new(expr),
                field: ident,
                span: cr.span,
            }
        } else {
            pending_name_parts.push(ident);
            let base_ref =
                component_reference_from_name_parts(&pending_name_parts, cr.def_id, cr.span);
            pending_name_parts.clear();
            rumoca_core::Expression::VarRef {
                name: Reference::from_component_reference(base_ref),
                subscripts: vec![],
                span: cr.span,
            }
        };

        current = Some(rumoca_core::Expression::Index {
            base: Box::new(base),
            subscripts: subs
                .iter()
                .map(|sub| subscript_from_ast_with_fallback(sub, cr.span))
                .collect::<LowerResult<Vec<_>>>()?,
            span: cr.span,
        });
    }

    current.map_or_else(|| expression_from_component_ref(cr), Ok)
}

fn component_reference_from_name_parts(
    parts: &[String],
    target_def_id: Option<DefId>,
    span: rumoca_core::Span,
) -> ComponentReference {
    ComponentReference {
        local: false,
        span,
        parts: parts
            .iter()
            .map(|part| ComponentRefPart {
                ident: part.clone(),
                span,
                subs: Vec::new(),
            })
            .collect(),
        def_id: target_def_id,
    }
}

fn reference_from_ast_component_ref(cr: &ast::ComponentReference) -> LowerResult<Reference> {
    Ok(Reference::from_component_reference(
        component_reference_from_ast(cr)?,
    ))
}

fn component_reference_from_ast(cr: &ast::ComponentReference) -> LowerResult<ComponentReference> {
    component_reference_from_ast_with_target_def_id(cr, cr.def_id)
}

fn component_reference_from_ast_with_target_def_id(
    cr: &ast::ComponentReference,
    target_def_id: Option<DefId>,
) -> LowerResult<ComponentReference> {
    Ok(ComponentReference {
        local: cr.local,
        span: cr.span,
        parts: cr
            .parts
            .iter()
            .map(|part| {
                Ok(ComponentRefPart {
                    ident: part.ident.text.to_string(),
                    span: cr.span,
                    subs: part
                        .subs
                        .as_ref()
                        .map(|subs| {
                            subs.iter()
                                .map(|sub| subscript_from_ast_with_fallback(sub, cr.span))
                                .collect()
                        })
                        .transpose()?
                        .unwrap_or_default(),
                })
            })
            .collect::<LowerResult<Vec<_>>>()?,
        def_id: target_def_id,
    })
}

fn subscript_from_ast_with_fallback(
    sub: &ast::Subscript,
    fallback_span: rumoca_core::Span,
) -> LowerResult<rumoca_core::Subscript> {
    match sub {
        ast::Subscript::Expression(expr) => {
            let span = expr.span();
            if let Some(val) = try_constant_integer(expr) {
                return Ok(rumoca_core::Subscript::index(val, span));
            }
            Ok(rumoca_core::Subscript::expr(
                Box::new(expression_from_ast(expr)?),
                span,
            ))
        }
        ast::Subscript::Range { .. } | ast::Subscript::Empty => {
            Ok(rumoca_core::Subscript::colon(fallback_span))
        }
    }
}

fn is_enum_literal_ref(cr: &ast::ComponentReference, canonical_path: &str) -> bool {
    let Some(last_part) = cr.parts.last() else {
        return false;
    };

    let textual_literal = last_part.ident.text.as_ref();
    if !is_quoted_identifier(textual_literal) {
        return false;
    }

    is_quoted_identifier(top_level_last_segment(canonical_path))
}

fn is_quoted_identifier(name: &str) -> bool {
    name.starts_with('\'') && name.ends_with('\'') && name.len() >= 2
}

#[cfg(test)]
fn convert_function_call_with_def_map(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    def_map: Option<&IndexMap<DefId, String>>,
) -> LowerResult<rumoca_core::Expression> {
    convert_function_call_with_context(
        comp,
        args,
        LoweringContext {
            def_map,
            instance_name: None,
        },
    )
}

fn convert_function_call_with_context(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    if is_get_instance_name_call(comp) {
        return lower_get_instance_name_call(args, context, comp.span);
    }

    if comp.parts.len() == 1 {
        let func_name = &comp.parts[0].ident.text;
        if rumoca_core::predefined_component_type(func_name.as_ref())
            == Some(rumoca_core::PredefinedComponentType::String)
        {
            return predefined_type_constructor_call(comp, args, context);
        }
        if let Some(builtin) = rumoca_core::BuiltinFunction::from_name(func_name) {
            return Ok(rumoca_core::Expression::BuiltinCall {
                function: builtin,
                args: args
                    .iter()
                    .map(|a| expression_from_ast_with_context(a, context))
                    .collect::<LowerResult<Vec<_>>>()?,
                span: comp.span,
            });
        }
    }

    let function_ref = match resolved_function_call_reference(comp, context.def_map) {
        Some(function_ref) => function_ref,
        None => Reference::from_component_reference(component_reference_from_ast(comp)?),
    };

    Ok(rumoca_core::Expression::FunctionCall {
        name: function_ref,
        args: args
            .iter()
            .map(|a| convert_call_arg_with_context(a, context))
            .collect::<LowerResult<Vec<_>>>()?,
        is_constructor: false,
        span: comp.span,
    })
}

fn predefined_type_constructor_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    Ok(rumoca_core::Expression::FunctionCall {
        name: Reference::from_component_reference(component_reference_from_ast_with_def_map(
            comp,
            context.def_map,
        )?),
        args: args
            .iter()
            .map(|a| convert_call_arg_with_context(a, context))
            .collect::<LowerResult<Vec<_>>>()?,
        is_constructor: true,
        span: comp.span,
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

fn resolved_function_call_reference(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Option<Reference> {
    let def_id = comp.def_id?;
    let resolved = resolved_function_call_name(comp, def_map)?;
    Some(Reference::from_component_reference(
        component_reference_from_path(&resolved, comp.span, Some(def_id)),
    ))
}

fn resolved_function_call_name(
    comp: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Option<String> {
    let resolved = comp
        .def_id
        .and_then(|def_id| def_map.and_then(|map| map.get(&def_id)))?;
    let call_leaf = comp.parts.last()?.ident.text.as_ref();
    let resolved_leaf = top_level_last_segment(resolved.as_str());
    if !resolved_path_ends_with_component_ref(resolved, comp)
        && !is_receiver_member_function_call(comp, call_leaf, resolved_leaf)
    {
        return None;
    }

    // `ComponentReference::def_id` usually names the first segment. Function
    // resolution rewrites successful multi-segment calls so the DefId names the
    // final callable. If lookup stopped at a receiver component such as
    // `world` in `world.gravityAcceleration(...)`, keep the textual member
    // call so flatten's component-override rewrite can resolve it from the
    // receiver type.
    (resolved_leaf == call_leaf).then(|| resolved.clone())
}

fn is_receiver_member_function_call(
    comp: &ast::ComponentReference,
    call_leaf: &str,
    resolved_leaf: &str,
) -> bool {
    let Some(receiver) = comp.parts.first() else {
        return false;
    };
    comp.parts.len() == 2
        && receiver
            .ident
            .text
            .chars()
            .next()
            .is_some_and(char::is_lowercase)
        && resolved_leaf == call_leaf
}

fn resolved_path_ends_with_component_ref(resolved: &str, comp: &ast::ComponentReference) -> bool {
    if comp.parts.is_empty() {
        return true;
    }
    let resolved_parts = split_path_with_indices(resolved);
    if resolved_parts.len() < comp.parts.len() {
        return false;
    }
    resolved_parts[resolved_parts.len() - comp.parts.len()..]
        .iter()
        .zip(&comp.parts)
        .all(|(resolved_part, comp_part)| *resolved_part == comp_part.ident.text.as_ref())
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
        ast::TerminalType::End | ast::TerminalType::Empty => Ok(rumoca_core::Literal::Integer(0)),
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

const NAMED_CONSTRUCTOR_ARG_PREFIX: &str = "__rumoca_named_arg__.";

fn wrap_named_constructor_arg(
    name: &str,
    value: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: Reference::new(format!("{NAMED_CONSTRUCTOR_ARG_PREFIX}{name}")),
        args: vec![value],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn convert_call_arg_with_context(
    expr: &ast::Expression,
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    match expr {
        ast::Expression::NamedArgument { name, value, .. } => Ok(wrap_named_constructor_arg(
            &name.text,
            expression_from_ast_with_context(value, context)?,
        )),
        ast::Expression::Modification { target, value, .. } => {
            let arg_name = target
                .parts
                .iter()
                .map(|p| p.ident.text.to_string())
                .collect::<Vec<_>>()
                .join(".");
            Ok(wrap_named_constructor_arg(
                &arg_name,
                expression_from_ast_with_context(value, context)?,
            ))
        }
        _ => expression_from_ast_with_context(expr, context),
    }
}

fn convert_class_modification_with_context(
    target: &ast::ComponentReference,
    modifications: &[ast::Expression],
    context: LoweringContext<'_>,
) -> LowerResult<rumoca_core::Expression> {
    let constructor_name = target
        .def_id
        .and_then(|def_id| context.def_map.and_then(|map| map.get(&def_id).cloned()))
        .map_or_else(
            || {
                component_reference_from_ast(target)
                    .map(|reference| reference.to_var_name().to_string())
            },
            Ok,
        );
    let constructor_name = constructor_name?;
    let constructor_def_id = target.def_id;
    let constructor_ref =
        component_reference_from_path(&constructor_name, target.span, constructor_def_id);
    Ok(rumoca_core::Expression::FunctionCall {
        name: Reference::from_component_reference(constructor_ref),
        args: modifications
            .iter()
            .map(|expr| convert_call_arg_with_context(expr, context))
            .collect::<LowerResult<Vec<_>>>()?,
        is_constructor: true,
        span: target.span,
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

    fn part(name: &str) -> ast::ComponentRefPart {
        ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from(name),
                ..rumoca_core::Token::default()
            },
            subs: None,
        }
    }

    fn ast_var(name: &str) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![part(name)],
            span: Span::DUMMY,
            def_id: None,
        })
    }

    fn function_ref(name: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: vec![part(name)],
            span: Span::DUMMY,
            def_id: None,
        }
    }

    #[test]
    fn get_instance_name_lowers_to_instance_string_literal() {
        let expr = convert_function_call_with_context(
            &function_ref("getInstanceName"),
            &[],
            LoweringContext {
                def_map: None,
                instance_name: Some("Vehicle.engine.controller"),
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
    fn get_instance_name_requires_instance_scope() {
        let err = convert_function_call_with_context(
            &function_ref("getInstanceName"),
            &[],
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
            LoweringContext {
                def_map: None,
                instance_name: Some("Vehicle.engine.controller"),
            },
        )
        .unwrap_err();

        assert!(err.to_string().contains("takes no arguments"));
    }

    #[test]
    fn function_call_lowering_keeps_member_name_when_def_id_resolves_receiver() {
        let receiver_def = DefId::new(1);
        let mut def_map = IndexMap::default();
        def_map.insert(receiver_def, "Pkg.Receiver".to_string());
        let comp = ast::ComponentReference {
            local: false,
            parts: vec![part("receiver"), part("member")],
            span: Span::DUMMY,
            def_id: Some(receiver_def),
        };

        assert_eq!(resolved_function_call_name(&comp, Some(&def_map)), None);

        let expr = convert_function_call_with_def_map(&comp, &[], Some(&def_map)).unwrap();
        let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
            panic!("expected function call");
        };
        assert_eq!(name.as_str(), "receiver.member");
    }

    #[test]
    fn function_call_lowering_uses_def_id_when_it_resolves_callable_leaf() {
        let function_def = DefId::new(2);
        let mut def_map = IndexMap::default();
        def_map.insert(function_def, "Pkg.Receiver.member".to_string());
        let comp = ast::ComponentReference {
            local: false,
            parts: vec![part("Receiver"), part("member")],
            span: Span::DUMMY,
            def_id: Some(function_def),
        };

        assert_eq!(
            resolved_function_call_name(&comp, Some(&def_map)).as_deref(),
            Some("Pkg.Receiver.member")
        );
    }

    #[test]
    fn function_call_lowering_canonicalizes_receiver_member_function_target() {
        let function_def = DefId::new(4);
        let mut def_map = IndexMap::default();
        def_map.insert(
            function_def,
            "Modelica.Mechanics.MultiBody.World.gravityAcceleration".to_string(),
        );
        let comp = ast::ComponentReference {
            local: false,
            parts: vec![part("world"), part("gravityAcceleration")],
            span: Span::DUMMY,
            def_id: Some(function_def),
        };

        let expr = convert_function_call_with_def_map(&comp, &[], Some(&def_map)).unwrap();
        let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
            panic!("expected function call");
        };
        assert_eq!(
            name.as_str(),
            "Modelica.Mechanics.MultiBody.World.gravityAcceleration"
        );
    }

    #[test]
    fn function_call_lowering_keeps_concrete_path_when_def_id_names_constraint() {
        let partial_function_def = DefId::new(3);
        let mut def_map = IndexMap::default();
        def_map.insert(
            partial_function_def,
            "Modelica.Media.Interfaces.PartialMedium.specificEnthalpy".to_string(),
        );
        let comp = ast::ComponentReference {
            local: false,
            parts: vec![
                part("Modelica"),
                part("Media"),
                part("Air"),
                part("ReferenceAir"),
                part("Air_pT"),
                part("specificEnthalpy"),
            ],
            span: Span::DUMMY,
            def_id: Some(partial_function_def),
        };

        assert_eq!(resolved_function_call_name(&comp, Some(&def_map)), None);
        let expr = convert_function_call_with_def_map(&comp, &[], Some(&def_map)).unwrap();
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
        let mut def_map = IndexMap::default();
        def_map.insert(variable_def, "QuadrotorSIL.leg_v_b".to_string());
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
            }],
            span: Span::DUMMY,
            def_id: Some(variable_def),
        };

        let expr = expression_from_component_ref_with_def_map(&comp, Some(&def_map)).unwrap();
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
    fn structured_subscript_base_preserves_target_def_id() {
        let fluid_constants_def = DefId::new(4);
        let mut fluid_constants = part("fluidConstants");
        fluid_constants.subs = Some(vec![ast::Subscript::Expression(
            ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token {
                    text: Arc::from("1"),
                    ..rumoca_core::Token::default()
                },
                span: Span::DUMMY,
            },
        )]);
        let comp = ast::ComponentReference {
            local: false,
            parts: vec![
                part("source"),
                part("medium"),
                fluid_constants,
                part("criticalTemperature"),
            ],
            span: Span::DUMMY,
            def_id: Some(fluid_constants_def),
        };

        let expr = expression_from_component_ref_with_def_map(&comp, None).unwrap();
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
        assert_eq!(name.target_def_id(), Some(fluid_constants_def));
    }
}
