//! Instance-local component replacements, applied before component expansion.

use crate::inheritance::{
    activate_constrainedby_defaults_for_redeclare, apply_redeclared_dimensions,
    validate_redeclaration,
};
use crate::{InstantiateContext, InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast::{self as ast, AstIndexMap as IndexMap};
use std::borrow::Cow;

#[derive(Default)]
pub(crate) struct ComponentRedeclarations {
    by_def_id: IndexMap<DefId, ast::Component>,
}

impl ComponentRedeclarations {
    /// Source modifier targets are instance-owned (MLS §7.2); bind them to the
    /// modified class's effective declarations before making a semantic map.
    pub(crate) fn from_component(
        tree: &ast::ClassTree,
        component: &ast::Component,
        target_class: &ast::ClassDef,
        parents: &IndexMap<String, ast::Component>,
        ctx: &mut InstantiateContext,
        imports: &[(String, String)],
    ) -> InstantiateResult<Self> {
        let mut result = Self::default();
        if !component
            .source_modification_redeclare_flags
            .contains(&true)
        {
            return Ok(result);
        }
        let template =
            crate::templates::get_or_compute_template(tree, target_class, &mut ctx.template_cache)?;
        let targets = &template.effective_components;
        for (modifier, redeclare) in component
            .source_modifications
            .iter()
            .zip(&component.source_modification_redeclare_flags)
        {
            let ast::Expression::Modification {
                target,
                value,
                span,
            } = modifier
            else {
                continue;
            };
            if !redeclare || target.parts.len() != 1 {
                continue;
            }
            let part = &target.parts[0];
            let Some(original) = targets.get(part.ident.text.as_ref()) else {
                // Class/package replacements have their own owner.
                continue;
            };
            let Some(id) = original.def_id else {
                return Err(Box::new(InstantiateError::missing_resolved_identity(
                    original.name.clone(),
                    *span,
                )));
            };
            let replacement = replace_component_type(tree, original, value, *span)?;
            let replacement = replace_dimensions(replacement, part, tree, parents, ctx, imports)?;
            result.by_def_id.insert(id, replacement);
        }
        Ok(result)
    }

    pub(crate) fn apply<'a>(
        &self,
        components: &'a IndexMap<String, ast::Component>,
    ) -> Cow<'a, IndexMap<String, ast::Component>> {
        if self.by_def_id.is_empty() {
            return Cow::Borrowed(components);
        }
        let mut result = components.clone();
        for component in result.values_mut() {
            if let Some(replacement) = component.def_id.and_then(|id| self.by_def_id.get(&id)) {
                *component = replacement.clone();
            }
        }
        Cow::Owned(result)
    }
}

pub(crate) fn has_unapplied_redeclare(component: &ast::Component) -> bool {
    component.has_unapplied_redeclare
        || component
            .source_modifications
            .iter()
            .any(crate::traversal_adapter::expression_contains_redeclare)
}

fn replace_component_type(
    tree: &ast::ClassTree,
    original: &ast::Component,
    value: &ast::Expression,
    span: rumoca_core::Span,
) -> InstantiateResult<ast::Component> {
    let reference = match value {
        ast::Expression::ClassModification { target, .. } => target,
        ast::Expression::ComponentReference(reference) => reference,
        _ => {
            return Err(Box::new(InstantiateError::redeclare_error(
                &original.name,
                "component redeclaration has no resolved replacement type",
                span,
            )));
        }
    };
    let type_id = reference.target_def_id().ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            reference.to_string(),
            span,
        ))
    })?;
    let type_name = tree.def_map.get(&type_id).ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            reference.to_string(),
            span,
        ))
    })?;
    validate_redeclaration(tree, original, &original.name, Some(type_name), span)?;
    let mut replacement = original.clone();
    replacement.type_def_id = Some(type_id);
    replacement.type_name = ast::Name::from_string(type_name);
    replacement.type_name.def_id = Some(type_id);
    replacement.redeclared_by_modification = true;
    activate_constrainedby_defaults_for_redeclare(&mut replacement);
    Ok(replacement)
}

fn replace_dimensions(
    mut component: ast::Component,
    target: &ast::ComponentRefPart,
    tree: &ast::ClassTree,
    parents: &IndexMap<String, ast::Component>,
    ctx: &InstantiateContext,
    imports: &[(String, String)],
) -> InstantiateResult<ast::Component> {
    let Some(subscripts) = target.subs.as_ref().filter(|subs| !subs.is_empty()) else {
        return Ok(component);
    };
    let qualified = crate::dims::qualify_shape_subscripts_imports(tree, subscripts, imports);
    let mut dimensions = Vec::with_capacity(qualified.len());
    for subscript in qualified {
        let resolved = match subscript {
            ast::Subscript::Expression(expression) => {
                let value = rumoca_eval_ast::eval_instantiate::try_eval_integer_shape_expr(
                    &expression,
                    ctx.mod_env(),
                    parents,
                    tree,
                    crate::inheritance::resolve_effective_components_for_eval,
                )
                .ok_or_else(|| {
                    Box::new(InstantiateError::structural_param_error(
                        &component.name,
                        "cannot prove redeclared dimensions in the enclosing scope",
                        expression.span(),
                    ))
                })?;
                let dimension_type = matches!(&expression,
                    ast::Expression::ComponentReference(reference)
                    if reference.target_def_id().and_then(|id| tree.get_class_by_def_id(id)).is_some()
                        || ast::Subscript::Expression(expression.clone()).literal_dimension().is_some());
                if dimension_type {
                    ast::Subscript::Expression(expression)
                } else {
                    ast::Subscript::Expression(ast::Expression::Terminal {
                        terminal_type: ast::TerminalType::UnsignedInteger,
                        token: rumoca_core::Token {
                            text: value.to_string().into(),
                            ..Default::default()
                        },
                        span: expression.span(),
                    })
                }
            }
            other => other,
        };
        dimensions.push(resolved);
    }
    apply_redeclared_dimensions(&mut component, &dimensions);
    Ok(component)
}
