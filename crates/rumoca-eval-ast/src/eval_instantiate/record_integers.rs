//! MLS §§7.2, 12.6: structural integers selected from bound record values.

use super::*;

pub(super) fn eval_bound_field(
    binding: &ast::Expression,
    field: &ast::ComponentRefPart,
    env: IntegerEvalEnv<'_>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    match binding {
        ast::Expression::ComponentReference(reference) => {
            let mut reference = reference.clone();
            reference.parts.push(field.clone());
            eval_integer_component_ref(&reference, env, depth + 1, None)
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            constructor_integer_field(comp, args, field, env, depth)
        }
        _ => None,
    }
}

fn constructor_integer_field(
    constructor: &ast::ComponentReference,
    args: &[ast::Expression],
    field: &ast::ComponentRefPart,
    env: IntegerEvalEnv<'_>,
    depth: usize,
) -> Option<i64> {
    let record = env.tree.get_class_by_def_id(constructor.target_def_id()?)?;
    if record.class_type != rumoca_core::ClassType::Record {
        return None;
    }
    let mut fields = (env.resolve_class_components)(env.tree, record);
    let target = fields
        .values()
        .find(|component| component.def_id == field.def_id)?;
    let target_id = target.def_id?;
    let inputs = fields
        .values()
        .filter(|component| !component.is_protected && !component.is_final)
        .filter_map(|component| component.def_id)
        .collect::<Vec<_>>();
    let mut positional = 0;
    for argument in args {
        let (target, value) = match argument {
            ast::Expression::NamedArgument { name, value, .. } => {
                (fields.get_mut(name.text.as_ref())?, value.as_ref())
            }
            value => {
                let id = *inputs.get(positional)?;
                positional += 1;
                (
                    fields
                        .values_mut()
                        .find(|component| component.def_id == Some(id))?,
                    value,
                )
            }
        };
        // Arguments are evaluated in the caller's scope, while defaults below
        // read the record's fields. An undecidable argument clears the default:
        // it must never silently recover the declaration's old value.
        target.binding = evaluated_argument(value, env, depth + 1);
    }
    let target = fields
        .values()
        .find(|component| component.def_id == Some(target_id))?;
    let value = target.binding.as_ref()?;
    try_eval_integer_expr_with_depth_and_locals(
        value,
        &ast::ModificationEnvironment::default(),
        &fields,
        env.tree,
        env.resolve_class_components,
        depth + 1,
        None,
    )
}

fn evaluated_argument(
    value: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
) -> Option<ast::Expression> {
    let integer = try_eval_integer_expr_with_depth_and_locals(
        value,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        depth,
        None,
    )?;
    Some(ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: integer.to_string().into(),
            ..Default::default()
        },
        span: value.span(),
    })
}
