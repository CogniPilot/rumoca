use std::collections::HashMap;

use rumoca_ir_dae::Dae;

use super::{StructuralError, structural_contract_violation};

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
