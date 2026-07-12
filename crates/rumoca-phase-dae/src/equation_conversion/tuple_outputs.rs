use rumoca_ir_flat as flat;

use super::tuple_arity_error;
use crate::errors::ToDaeError;

pub(super) fn tuple_function_output_expressions(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
    lhs_count: usize,
    flat: &flat::Model,
) -> Result<Vec<rumoca_core::Expression>, ToDaeError> {
    let function = flat.functions.get(name.var_name()).ok_or_else(|| {
        ToDaeError::runtime_contract_violation_at(
            format!(
                "mixed tuple assignment RHS function `{}` is missing from Flat.functions",
                name.as_str()
            ),
            span,
        )
    })?;
    if function.outputs.len() != lhs_count {
        return Err(tuple_arity_error(lhs_count, function.outputs.len(), span));
    }
    function
        .outputs
        .iter()
        .map(|output| projected_output_call(name, args, output, span))
        .collect()
}

fn projected_output_call(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    output: &rumoca_core::FunctionParam,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let projected_name = name
        .with_appended_parts(
            &[rumoca_core::ComponentRefPart {
                ident: output.name.clone(),
                span,
                subs: vec![],
            }],
            span,
        )
        .ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!(
                    "invalid tuple function output `{}.{}`",
                    name.as_str(),
                    output.name
                ),
                span,
            )
        })?;
    Ok(rumoca_core::Expression::FunctionCall {
        name: projected_name,
        args: args.to_vec(),
        is_constructor: false,
        span,
    })
}
