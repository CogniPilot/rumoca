use super::*;

pub(super) fn validate_function_outputs_assigned(
    function: &rumoca_core::Function,
) -> Result<(), FlattenError> {
    if function.external.is_some() || function.outputs.is_empty() {
        return Ok(());
    }

    let output_names: IndexSet<&str> = function
        .outputs
        .iter()
        .map(|output| output.name.as_str())
        .collect();
    let mut assigned: IndexSet<&str> = function
        .outputs
        .iter()
        .filter(|output| output.default.is_some())
        .map(|output| output.name.as_str())
        .collect();
    collect_assigned_outputs_before_return(&function.body, &output_names, &mut assigned);

    for output in &function.outputs {
        if !assigned.contains(output.name.as_str()) {
            return Err(FlattenError::function_output_unassigned(
                function.name.as_str(),
                &output.name,
                output.span,
            ));
        }
    }
    Ok(())
}

fn collect_assigned_outputs_before_return<'a>(
    statements: &'a [rumoca_core::Statement],
    outputs: &IndexSet<&'a str>,
    assigned: &mut IndexSet<&'a str>,
) {
    for statement in statements {
        if collect_statement_assigned_outputs(statement, outputs, assigned) {
            return;
        }
    }
}

fn collect_statement_assigned_outputs<'a>(
    statement: &'a rumoca_core::Statement,
    outputs: &IndexSet<&'a str>,
    assigned: &mut IndexSet<&'a str>,
) -> bool {
    match statement {
        rumoca_core::Statement::Assignment { comp, .. } => {
            collect_assigned_output_from_component_reference(comp, outputs, assigned);
            false
        }
        rumoca_core::Statement::FunctionCall {
            outputs: targets, ..
        } => {
            for target in targets {
                collect_assigned_output_from_component_reference(target, outputs, assigned);
            }
            false
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_assigned_outputs_before_return(&block.stmts, outputs, assigned);
            }
            if let Some(else_stmts) = else_block {
                collect_assigned_outputs_before_return(else_stmts, outputs, assigned);
            }
            false
        }
        rumoca_core::Statement::For { equations, .. } => {
            collect_assigned_outputs_before_return(equations, outputs, assigned);
            false
        }
        rumoca_core::Statement::While { block, .. } => {
            collect_assigned_outputs_before_return(&block.stmts, outputs, assigned);
            false
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_assigned_outputs_before_return(&block.stmts, outputs, assigned);
            }
            false
        }
        rumoca_core::Statement::Return { .. } => true,
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Break { .. }
        | rumoca_core::Statement::Reinit { .. }
        | rumoca_core::Statement::Assert { .. } => false,
    }
}

fn collect_assigned_output_from_component_reference<'a>(
    comp: &'a rumoca_core::ComponentReference,
    outputs: &IndexSet<&'a str>,
    assigned: &mut IndexSet<&'a str>,
) {
    let Some(first) = comp.parts.first() else {
        return;
    };
    let output = first.ident.as_str();
    if outputs.contains(output) {
        assigned.insert(output);
    }
}
