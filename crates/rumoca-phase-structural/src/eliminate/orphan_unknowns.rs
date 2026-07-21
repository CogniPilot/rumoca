use rumoca_core::VarName;
use rumoca_ir_dae::Dae;

use super::{StructuralError, expression_references_boundary_unknown};

fn is_referenced(dae: &Dae, name: &VarName) -> Result<bool, StructuralError> {
    for equation in &dae.continuous.equations {
        if expression_references_boundary_unknown(&equation.rhs, name, dae)? {
            return Ok(true);
        }
    }
    Ok(false)
}

pub(super) fn drop_unreferenced_continuous_unknowns(dae: &mut Dae) -> Result<(), StructuralError> {
    let mut algebraics = Vec::new();
    for name in dae.variables.algebraics.keys() {
        if !is_referenced(dae, name)? {
            algebraics.push(name.clone());
        }
    }
    let mut outputs = Vec::new();
    for name in dae.variables.outputs.keys() {
        if !is_referenced(dae, name)? {
            outputs.push(name.clone());
        }
    }
    for name in algebraics {
        dae.variables.algebraics.shift_remove(&name);
    }
    for name in outputs {
        dae.variables.outputs.shift_remove(&name);
    }
    Ok(())
}

pub(super) fn output_partition_contains_unknown(dae: &Dae, name: &VarName) -> bool {
    dae.variables.outputs.contains_key(name)
        || rumoca_ir_dae::component_base_name(name.as_str())
            .is_some_and(|base| dae.variables.outputs.contains_key(&VarName::new(base)))
}
