use std::sync::Arc;

use rumoca_eval_ast::eval_instantiate::{InstantiateEvalCtx, try_eval_integer_expr};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

use super::resolve_mod_to_array;
use crate::inheritance::resolve_effective_components_for_eval;
use crate::{InstantiateError, InstantiateResult};

fn generated_integer_subscript(value: i64, span: rumoca_core::Span) -> ast::Subscript {
    ast::Subscript::Expression(ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: value.to_string().into(),
            ..rumoca_core::Token::default()
        },
        span,
    })
}

fn project_range_selection(
    eval_ctx: &InstantiateEvalCtx<'_>,
    subscript: &ast::Subscript,
    element_index: Option<i64>,
) -> InstantiateResult<ast::Subscript> {
    let ast::Subscript::Expression(ast::Expression::Range {
        start,
        step,
        end,
        span,
    }) = subscript
    else {
        return Ok(subscript.clone());
    };
    let Some(element_index) = element_index else {
        return Ok(subscript.clone());
    };
    let structural_integer = |value: &ast::Expression, part: &str| -> InstantiateResult<i64> {
        try_eval_integer_expr(eval_ctx, value).ok_or_else(|| {
            InstantiateError::structural_param_error(
                "array modifier range".to_string(),
                format!("range {part} is not structurally evaluable"),
                *span,
            )
            .into()
        })
    };
    let start_value = structural_integer(start, "start")?;
    let step_value = step
        .as_deref()
        .map(|value| structural_integer(value, "step"))
        .transpose()?
        .unwrap_or(1);
    let end_value = structural_integer(end, "end")?;
    let offset = element_index.checked_sub(1).ok_or_else(|| {
        InstantiateError::array_dim_mismatch(
            "array modifier selection".to_string(),
            "a positive one-based element index".to_string(),
            element_index.to_string(),
            *span,
        )
    })?;
    let selected = offset
        .checked_mul(step_value)
        .and_then(|value| start_value.checked_add(value))
        .ok_or_else(|| {
            InstantiateError::array_dim_mismatch(
                "array modifier selection".to_string(),
                "an index representable as Integer".to_string(),
                "integer overflow".to_string(),
                *span,
            )
        })?;
    let in_range = match step_value.cmp(&0) {
        std::cmp::Ordering::Greater => selected <= end_value,
        std::cmp::Ordering::Less => selected >= end_value,
        std::cmp::Ordering::Equal => false,
    };
    if !in_range {
        return Err(InstantiateError::array_dim_mismatch(
            "array modifier selection".to_string(),
            format!("an element of {start_value}:{step_value}:{end_value}"),
            selected.to_string(),
            *span,
        )
        .into());
    }
    Ok(generated_integer_subscript(selected, *span))
}

fn project_expression_selection(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    expr: &ast::Expression,
    result_index: &mut impl Iterator<Item = i64>,
) -> InstantiateResult<ast::Subscript> {
    let resolved = resolve_mod_to_array(
        expr,
        &ast::ModificationEnvironment::default(),
        effective_components,
        tree,
    );
    let is_declared_vector = matches!(
        expr,
        ast::Expression::ComponentReference(reference)
            if reference.parts.len() == 1
                && effective_components
                    .get(reference.parts[0].ident.text.as_ref())
                    .is_some_and(|component| {
                        !component.shape.is_empty() || !component.shape_expr.is_empty()
                    })
    );
    if let ast::Expression::Array { elements, .. } = resolved {
        let Some(element_index) = result_index.next() else {
            return Ok(ast::Subscript::Expression(expr.clone()));
        };
        let offset = usize::try_from(element_index)
            .ok()
            .and_then(|index| index.checked_sub(1))
            .ok_or_else(|| {
                InstantiateError::array_dim_mismatch(
                    "array modifier vector selection".to_string(),
                    "a positive one-based element index".to_string(),
                    element_index.to_string(),
                    expr.span(),
                )
            })?;
        let selected = elements.get(offset).ok_or_else(|| {
            InstantiateError::array_dim_mismatch(
                "array modifier vector selection".to_string(),
                format!("an index no greater than {}", elements.len()),
                element_index.to_string(),
                expr.span(),
            )
        })?;
        return Ok(ast::Subscript::Expression(selected.clone()));
    }
    if is_declared_vector {
        let Some(element_index) = result_index.next() else {
            return Ok(ast::Subscript::Expression(expr.clone()));
        };
        return Ok(ast::Subscript::Expression(ast::Expression::ArrayIndex {
            base: Arc::new(expr.clone()),
            subscripts: vec![generated_integer_subscript(element_index, expr.span())],
            span: expr.span(),
        }));
    }
    Ok(ast::Subscript::Expression(expr.clone()))
}

/// Compose an array-component element index with an existing array selection.
///
/// Fixed scalar subscripts do not consume a result dimension; ranges, colons,
/// and vector subscripts do (MLS section 10.5).
pub(super) fn project_array_selection_for_element(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    selection: Option<&[ast::Subscript]>,
    result_indices: &[i64],
    span: rumoca_core::Span,
) -> InstantiateResult<Vec<ast::Subscript>> {
    let Some(selection) = selection else {
        return Ok(result_indices
            .iter()
            .copied()
            .map(|index| generated_integer_subscript(index, span))
            .collect());
    };
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: &ast::ModificationEnvironment::default(),
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let mut result_index = result_indices.iter().copied();
    let mut projected = Vec::with_capacity(selection.len().saturating_add(result_indices.len()));
    for subscript in selection {
        match subscript {
            ast::Subscript::Expression(ast::Expression::Range { .. }) => projected.push(
                project_range_selection(&eval_ctx, subscript, result_index.next())?,
            ),
            ast::Subscript::Range { .. } | ast::Subscript::Empty => {
                projected.push(result_index.next().map_or_else(
                    || subscript.clone(),
                    |index| generated_integer_subscript(index, span),
                ));
            }
            ast::Subscript::Expression(expr) => projected.push(project_expression_selection(
                tree,
                effective_components,
                expr,
                &mut result_index,
            )?),
        }
    }
    projected.extend(result_index.map(|index| generated_integer_subscript(index, span)));
    Ok(projected)
}
