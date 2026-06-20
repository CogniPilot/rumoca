//! Name resolution helpers for ToDAE equation/binding analysis.

use std::collections::HashSet;

use rumoca_ir_dae as dae;

use crate::flat_to_dae_var_name;
use crate::path_utils::subscript_fallback_chain;

/// Resolve a variable reference name against a DAE variable set.
///
/// Connection equations may reference array elements (e.g. `y[1]`) while DAE maps
/// store the base variable (`y`). Return the matched name (exact first, then base).
pub(crate) fn resolve_var_name_with_subscript_fallback(
    name: &rumoca_core::VarName,
    mut contains: impl FnMut(&rumoca_core::VarName) -> bool,
) -> Option<rumoca_core::VarName> {
    if contains(name) {
        return Some(name.clone());
    }
    subscript_fallback_chain(name.as_str())
        .into_iter()
        .find(|candidate| contains(candidate))
}

/// Resolve a variable name against a set with subscript fallback.
///
/// This treats refs like `x[1]` as `x` when the set stores the base variable.
pub(crate) fn resolve_name_against_set(
    name: &rumoca_core::VarName,
    set: &HashSet<rumoca_core::VarName>,
) -> Option<rumoca_core::VarName> {
    resolve_var_name_with_subscript_fallback(name, |n| set.contains(n))
}

/// Extract a VarRef name from an equation side expression.
///
/// Handles plain VarRef and Index-wrapped VarRef forms.
pub(crate) fn extract_varref_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => Some(name.var_name().clone()),
        rumoca_core::Expression::Index { base, .. } => extract_varref_name(base),
        _ => None,
    }
}

pub(crate) fn is_dae_input_name(dae: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    resolve_var_name_with_subscript_fallback(name, |n| {
        dae.variables.inputs.contains_key(&flat_to_dae_var_name(n))
    })
    .is_some()
}

pub(crate) fn is_dae_continuous_unknown_name(dae: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    resolve_var_name_with_subscript_fallback(name, |n| {
        dae.variables.states.contains_key(&flat_to_dae_var_name(n))
            || dae
                .variables
                .algebraics
                .contains_key(&flat_to_dae_var_name(n))
            || dae.variables.outputs.contains_key(&flat_to_dae_var_name(n))
    })
    .is_some()
}

pub(crate) fn is_dae_fixed_value_name(dae: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    resolve_var_name_with_subscript_fallback(name, |n| {
        dae.variables
            .parameters
            .contains_key(&flat_to_dae_var_name(n))
            || dae
                .variables
                .constants
                .contains_key(&flat_to_dae_var_name(n))
            || dae
                .variables
                .discrete_reals
                .contains_key(&flat_to_dae_var_name(n))
            || dae
                .variables
                .discrete_valued
                .contains_key(&flat_to_dae_var_name(n))
    })
    .is_some()
}

#[cfg(test)]
mod tests {
    use rumoca_ir_dae as dae;

    use super::*;

    #[test]
    fn test_extract_varref_name_handles_index_wrapped_varref() {
        let expr = rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("arr").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(
            extract_varref_name(&expr),
            Some(rumoca_core::VarName::new("arr"))
        );
    }

    #[test]
    fn test_is_dae_input_name_uses_subscript_fallback() {
        let mut dae = dae::Dae::new();
        dae.variables.inputs.insert(
            rumoca_core::VarName::new("u"),
            dae::Variable::new(
                rumoca_core::VarName::new("u"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        assert!(is_dae_input_name(&dae, &rumoca_core::VarName::new("u[1]")));
    }

    #[test]
    fn test_resolve_var_name_with_subscript_fallback_handles_multiple_layers() {
        let target = rumoca_core::VarName::new("pipe.statesFM.phase");
        let resolved = resolve_var_name_with_subscript_fallback(
            &rumoca_core::VarName::new("pipe.statesFM[2:(pipe.n + 1)].phase[1]"),
            |name| name == &target,
        );
        assert_eq!(resolved, Some(target));
    }
}
