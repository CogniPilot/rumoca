use super::*;

/// Evaluate a Boolean attribute represented by one value for the whole
/// declaration. Array values are accepted only when every element agrees.
/// Undecidable or nonuniform values must not become an absent attribute.
pub fn try_eval_uniform_boolean_attribute(
    ctx: &InstantiateEvalCtx<'_>,
    expression: &ast::Expression,
    source_scope: Option<&ast::QualifiedName>,
) -> Option<bool> {
    let env = ConditionEvalEnv {
        mod_env: ctx.mod_env,
        effective_components: ctx.effective_components,
        tree: ctx.tree,
        resolve_class_components: ctx.resolve_class_components,
    };
    let scope = source_scope.map(ast::QualifiedName::to_flat_string);
    eval_uniform(expression, env, scope.as_deref(), 0)
}

fn eval_uniform(
    expression: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope: Option<&str>,
    depth: usize,
) -> Option<bool> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let recurse = |expression| eval_uniform(expression, env, scope, depth + 1);
    match expression {
        ast::Expression::ComponentReference(reference) => {
            let (binding, binding_scope) = resolve_component_ref_expr(
                reference,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                scope,
            )?;
            eval_uniform(&binding, env, binding_scope.as_deref(), depth + 1)
        }
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::Array { elements, .. } => {
            let first = recurse(elements.first()?)?;
            elements
                .iter()
                .skip(1)
                .all(|element| recurse(element) == Some(first))
                .then_some(first)
        }
        ast::Expression::FunctionCall { comp, args, .. }
            if comp.parts.len() == 1 && comp.parts[0].ident.text.as_ref() == "fill" =>
        {
            let (value, dimensions) = args.split_first()?;
            let ctx = InstantiateEvalCtx {
                tree: env.tree,
                mod_env: env.mod_env,
                effective_components: env.effective_components,
                resolve_class_components: env.resolve_class_components,
            };
            if dimensions.is_empty()
                || !dimensions
                    .iter()
                    .all(|dimension| try_eval_integer_expr(&ctx, dimension).is_some_and(|n| n >= 0))
            {
                return None;
            }
            recurse(value)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_scoped_string_condition_with_depth(condition, env, scope, depth + 1)? {
                    return recurse(value);
                }
            }
            recurse(else_branch)
        }
        _ => eval_scoped_string_condition_with_depth(expression, env, scope, depth + 1),
    }
}
