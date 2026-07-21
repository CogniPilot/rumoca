use std::collections::HashMap;

use rumoca_ir_dae::{self as dae, Dae};

use super::{StructuralError, structural_contract_violation};
use crate::projection_maps::{checked_projection_subscript, output_scalar_count};

pub(super) type ConstructorInputMap =
    HashMap<rumoca_core::FunctionInstanceId, Vec<rumoca_core::FunctionParam>>;

pub(super) fn build_constructor_input_map(
    dae: &Dae,
) -> Result<ConstructorInputMap, StructuralError> {
    dae.symbols
        .functions
        .values()
        .filter(|function| function.is_constructor)
        .map(|function| {
            function
                .instance_id
                .map(|instance_id| (instance_id, function.inputs.clone()))
                .ok_or_else(|| {
                    structural_contract_violation(
                        format!(
                            "constructor `{}` lacks flattened instance identity",
                            function.name
                        ),
                        function.span,
                    )
                })
        })
        .collect()
}

pub(super) fn expression_contains_dynamic_function_output(
    expr: &rumoca_core::Expression,
    dynamic_outputs: &HashMap<rumoca_core::FunctionInstanceId, String>,
) -> bool {
    struct Finder<'a> {
        dynamic_outputs: &'a HashMap<rumoca_core::FunctionInstanceId, String>,
        found: bool,
    }
    impl rumoca_core::ExpressionVisitor for Finder<'_> {
        fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
            if self.found {
                return;
            }
            if let rumoca_core::Expression::FunctionCall { name, .. } = expr
                && name.resolved_function().is_some_and(|resolved| {
                    self.dynamic_outputs.contains_key(&resolved.instance_id)
                })
            {
                self.found = true;
                return;
            }
            self.walk_expression(expr);
        }
    }
    let mut finder = Finder {
        dynamic_outputs,
        found: false,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut finder, expr);
    finder.found
}

pub(super) fn project_wrapped_dynamic_function_output(
    expr: &rumoca_core::Expression,
    scalar_index: usize,
    expected_dims: Option<&[i64]>,
    context_span: Option<rumoca_core::Span>,
    allow_projection: bool,
    dynamic_outputs: &HashMap<rumoca_core::FunctionInstanceId, String>,
) -> Result<Option<rumoca_core::Expression>, StructuralError> {
    if !allow_projection
        || matches!(expr, rumoca_core::Expression::FunctionCall { .. })
        || !expression_contains_dynamic_function_output(expr, dynamic_outputs)
    {
        return Ok(None);
    }
    let Some(dims) = expected_dims else {
        return Ok(None);
    };
    let span = expr
        .span()
        .or(context_span)
        .filter(|span| !span.is_dummy())
        .ok_or_else(|| StructuralError::UnspannedContractViolation {
            reason: "wrapped dynamic function output lacks source provenance".to_string(),
        })?;
    let count = output_scalar_count(dims, span)?;
    if !(1..=count).contains(&scalar_index) {
        return Ok(None);
    }
    let indices = dae::flat_index_to_subscripts(dims, scalar_index - 1).ok_or_else(|| {
        structural_contract_violation(
            "invalid wrapped dynamic function output index".to_string(),
            span,
        )
    })?;
    Ok(Some(rumoca_core::Expression::Index {
        base: Box::new(expr.clone()),
        subscripts: indices
            .into_iter()
            .map(|index| checked_projection_subscript(index, span))
            .collect::<Result<Vec<_>, _>>()?,
        span,
    }))
}

pub(super) fn build_dynamic_function_output_map(
    dae: &Dae,
) -> Result<HashMap<rumoca_core::FunctionInstanceId, String>, StructuralError> {
    dae.symbols
        .functions
        .values()
        .filter_map(|function| {
            let [output] = function.outputs.as_slice() else {
                return None;
            };
            output.dims.iter().any(|dim| *dim <= 0).then(|| {
                function
                    .instance_id
                    .map(|instance_id| (instance_id, output.name.clone()))
                    .ok_or_else(|| {
                        structural_contract_violation(
                            format!(
                                "function `{}` lacks flattened instance identity",
                                function.name
                            ),
                            function.span,
                        )
                    })
            })
        })
        .collect()
}

pub(super) fn build_function_output_dims_map(
    dae: &Dae,
) -> Result<HashMap<rumoca_core::FunctionInstanceId, Vec<i64>>, StructuralError> {
    dae.symbols
        .functions
        .values()
        .filter_map(|function| {
            let [output] = function.outputs.as_slice() else {
                return None;
            };
            (output.dims.is_empty() || output.dims.iter().all(|dim| *dim > 0)).then(|| {
                function
                    .instance_id
                    .map(|instance_id| (instance_id, output.dims.clone()))
                    .ok_or_else(|| {
                        structural_contract_violation(
                            format!(
                                "function `{}` lacks flattened instance identity",
                                function.name
                            ),
                            function.span,
                        )
                    })
            })
        })
        .collect()
}
