//! Name resolution helpers for ToDAE equation/binding analysis.

use std::collections::HashSet;

use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::flat_to_dae_var_name;
use crate::path_utils::subscript_fallback_chain;

/// Resolve a variable reference name against a DAE variable set.
///
/// Connection equations may reference array elements (e.g. `y[1]`) while DAE maps
/// store the base variable (`y`). Return the matched name (exact first, then base).
pub(crate) fn resolve_var_name_with_subscript_fallback(
    name: &flat::VarName,
    mut contains: impl FnMut(&flat::VarName) -> bool,
) -> Option<flat::VarName> {
    if contains(name) {
        return Some(name.clone());
    }
    subscript_fallback_chain(name)
        .into_iter()
        .find(|candidate| contains(candidate))
}

/// Resolve a variable name against a set with subscript fallback.
///
/// This treats refs like `x[1]` as `x` when the set stores the base variable.
pub(crate) fn resolve_name_against_set(
    name: &flat::VarName,
    set: &HashSet<flat::VarName>,
) -> Option<flat::VarName> {
    resolve_var_name_with_subscript_fallback(name, |n| set.contains(n))
}

/// Extract a VarRef name from an equation side expression.
///
/// Handles plain VarRef and Index-wrapped VarRef forms.
pub(crate) fn extract_varref_name(expr: &flat::Expression) -> Option<flat::VarName> {
    match expr {
        flat::Expression::VarRef { name, .. } => Some(name.clone()),
        flat::Expression::Index { base, .. } => extract_varref_name(base),
        _ => None,
    }
}

pub(crate) fn is_dae_input_name(dae: &dae::Dae, name: &flat::VarName) -> bool {
    resolve_var_name_with_subscript_fallback(name, |n| {
        dae.inputs.contains_key(&flat_to_dae_var_name(n))
    })
    .is_some()
}

pub(crate) fn is_dae_continuous_unknown_name(dae: &dae::Dae, name: &flat::VarName) -> bool {
    resolve_var_name_with_subscript_fallback(name, |n| {
        dae.states.contains_key(&flat_to_dae_var_name(n))
            || dae.algebraics.contains_key(&flat_to_dae_var_name(n))
            || dae.outputs.contains_key(&flat_to_dae_var_name(n))
    })
    .is_some()
}

pub(crate) fn is_dae_fixed_value_name(dae: &dae::Dae, name: &flat::VarName) -> bool {
    resolve_var_name_with_subscript_fallback(name, |n| {
        dae.parameters.contains_key(&flat_to_dae_var_name(n))
            || dae.constants.contains_key(&flat_to_dae_var_name(n))
            || dae.discrete_reals.contains_key(&flat_to_dae_var_name(n))
            || dae.discrete_valued.contains_key(&flat_to_dae_var_name(n))
    })
    .is_some()
}

#[cfg(test)]
mod tests {
    use rumoca_ir_dae as dae;

    use super::*;

    #[test]
    fn test_extract_varref_name_handles_index_wrapped_varref() {
        let expr = flat::Expression::Index {
            base: Box::new(flat::Expression::VarRef {
                name: flat::VarName::new("arr"),
                subscripts: vec![],
            }),
            subscripts: vec![flat::Subscript::Index(1)],
        };
        assert_eq!(extract_varref_name(&expr), Some(flat::VarName::new("arr")));
    }

    #[test]
    fn test_is_dae_input_name_uses_subscript_fallback() {
        let mut dae = dae::Dae::new();
        dae.inputs.insert(
            dae::VarName::new("u"),
            dae::Variable::new(dae::VarName::new("u")),
        );
        assert!(is_dae_input_name(&dae, &flat::VarName::new("u[1]")));
    }

    #[test]
    fn test_resolve_var_name_with_subscript_fallback_handles_multiple_layers() {
        let target = flat::VarName::new("pipe.statesFM.phase");
        let resolved = resolve_var_name_with_subscript_fallback(
            &flat::VarName::new("pipe.statesFM[2:(pipe.n + 1)].phase[1]"),
            |name| name == &target,
        );
        assert_eq!(resolved, Some(target));
    }
}
