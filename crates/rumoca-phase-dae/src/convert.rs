use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

pub(crate) fn flat_to_dae_var_name(name: &rumoca_core::VarName) -> rumoca_core::VarName {
    name.clone()
}

pub(crate) fn dae_to_flat_var_name(name: &rumoca_core::VarName) -> rumoca_core::VarName {
    name.clone()
}

pub(crate) fn flat_to_dae_expression(expr: &rumoca_core::Expression) -> rumoca_core::Expression {
    expr.clone()
}

pub(crate) fn dae_to_flat_expression(expr: &rumoca_core::Expression) -> rumoca_core::Expression {
    expr.clone()
}

pub(crate) fn remap_flat_for_equations(
    for_equations: &[flat::ForEquation],
    flat_to_dae_index: &IndexMap<usize, usize>,
) -> Vec<dae::ForEquation> {
    for_equations
        .iter()
        .filter_map(|for_eq| remap_flat_for_equation(for_eq, flat_to_dae_index))
        .collect()
}

fn remap_flat_for_equation(
    for_eq: &flat::ForEquation,
    flat_to_dae_index: &IndexMap<usize, usize>,
) -> Option<dae::ForEquation> {
    let mut flat_idx = for_eq.first_equation_index;
    let mut first_dae_index = None;
    let mut expected_next_dae_index = None;
    let mut iterations = Vec::with_capacity(for_eq.iterations.len());

    for iteration in &for_eq.iterations {
        let mut dae_equation_count = 0;
        for source_idx in flat_idx..flat_idx + iteration.equation_count {
            let dae_idx = *flat_to_dae_index.get(&source_idx)?;
            if let Some(expected) = expected_next_dae_index
                && dae_idx != expected
            {
                return None;
            }
            first_dae_index.get_or_insert(dae_idx);
            expected_next_dae_index = Some(dae_idx + 1);
            dae_equation_count += 1;
        }
        if dae_equation_count == 0 {
            return None;
        }
        iterations.push(dae::ForEquationIteration {
            index_values: iteration.index_values.clone(),
            equation_count: dae_equation_count,
        });
        flat_idx += iteration.equation_count;
    }

    Some(dae::ForEquation {
        index_names: for_eq.index_names.clone(),
        first_equation_index: first_dae_index?,
        iterations,
        span: for_eq.span,
        origin: for_eq.origin.to_string(),
    })
}

pub(crate) fn flat_to_dae_function_map(
    functions: &rumoca_ir_flat::VarNameIndexMap<rumoca_core::Function>,
) -> IndexMap<rumoca_core::VarName, rumoca_core::Function> {
    functions
        .iter()
        .map(|(name, function)| (name.clone(), function.clone()))
        .collect()
}
