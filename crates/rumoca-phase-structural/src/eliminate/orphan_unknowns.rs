use rumoca_core::VarName;
use rumoca_ir_dae::{Dae, expr_contains_var};

pub(super) fn drop_unreferenced_continuous_unknowns(dae: &mut Dae) {
    let referenced = |name: &VarName| {
        dae.continuous
            .equations
            .iter()
            .any(|eq| expr_contains_var(&eq.rhs, name))
    };
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

pub(super) fn output_partition_contains_unknown(dae: &Dae, name: &VarName) -> bool {
    dae.variables.outputs.contains_key(name)
        || rumoca_ir_dae::component_base_name(name.as_str())
            .is_some_and(|base| dae.variables.outputs.contains_key(&VarName::new(base)))
}
