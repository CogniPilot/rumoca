use rumoca_core::VarName;
use rumoca_ir_dae::{Dae, expr_contains_var};

use super::collect_exact_reference_expr_names_in_dae;

pub(super) fn drop_unreferenced_continuous_unknowns(dae: &mut Dae) {
    let exact_references = collect_continuous_exact_references(dae);
    let referenced =
        |name: &VarName| exact_reference_keeps_unknown(dae, &exact_references, name);
    let algebraics = dae
        .variables
        .algebraics
        .keys()
        .filter(|name| !referenced(name))
        .cloned()
        .collect::<Vec<_>>();
    let outputs = dae
        .variables
        .outputs
        .keys()
        .filter(|name| !referenced(name))
        .cloned()
        .collect::<Vec<_>>();
    for name in algebraics {
        dae.variables.algebraics.shift_remove(&name);
    }
    for name in outputs {
        dae.variables.outputs.shift_remove(&name);
    }
}

fn collect_continuous_exact_references(dae: &Dae) -> Vec<VarName> {
    let mut refs = Vec::new();
    for equation in &dae.continuous.equations {
        collect_exact_reference_expr_names_in_dae(dae, &equation.rhs, &mut refs);
    }
    refs.sort();
    refs.dedup();
    refs
}

fn exact_reference_keeps_unknown(dae: &Dae, exact_refs: &[VarName], name: &VarName) -> bool {
    if exact_refs.binary_search(name).is_ok() {
        return true;
    }
    if continuous_unknown_is_scalar(dae, name) {
        return false;
    }
    dae.continuous
        .equations
        .iter()
        .any(|eq| expr_contains_var(&eq.rhs, name))
}

fn continuous_unknown_is_scalar(dae: &Dae, name: &VarName) -> bool {
    dae.variables
        .algebraics
        .get(name)
        .or_else(|| dae.variables.outputs.get(name))
        .is_none_or(|var| var.dims.iter().all(|dim| *dim == 1))
}

pub(super) fn output_partition_contains_unknown(dae: &Dae, name: &VarName) -> bool {
    dae.variables.outputs.contains_key(name)
        || rumoca_ir_dae::component_base_name(name.as_str())
            .is_some_and(|base| dae.variables.outputs.contains_key(&VarName::new(base)))
}
