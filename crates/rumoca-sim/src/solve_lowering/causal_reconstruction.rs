use indexmap::IndexSet;
use rumoca_core::{Reference, VarName};
use rumoca_ir_dae as dae;
use rumoca_phase_structural::eliminate::{CausalSubstitutionPlan, Substitution};

/// Restore selected eliminated definitions after structural matching as
/// explicit causal algebraic assignments.
///
/// These slots are not states. The Solve-IR refresh planner schedules their
/// exact assignment DAG for derivative/root/output dependency closures, while
/// state-only integrators continue to advance only the selected state vector.
///
/// # Why two declaration sources
///
/// `source` is the DAE as the model declared it, snapshotted before the
/// structural rewrites run. `prepared` is that same DAE after them, so it is the
/// only one that declares the variables index reduction generates
/// (`__dummyder__.<state>`).
///
/// Both are needed because the factoring step that precedes this one
/// (`factor_retained_computations_in_dae`) rewrites rows to read *any* retained
/// target, generated ones included. A target this function declines to restore
/// therefore leaves rows reading a name no partition declares, and Solve
/// lowering reports it as "not a DAE variable" (`EL005`) — the shape
/// `Modelica.Mechanics.Rotational.Examples.First` hit once index reduction
/// started naming `der(inertia1.w)`.
pub(super) fn restore_shared_causal_assignments(
    lowered: &mut dae::Dae,
    source: &dae::Dae,
    prepared: &dae::Dae,
    plan: &CausalSubstitutionPlan,
) -> IndexSet<VarName> {
    let mut restored = IndexSet::new();
    for substitution in &plan.substitutions {
        if !plan.retained_targets.contains(&substitution.var_name) {
            continue;
        }
        if continuous_variable_exists(lowered, &substitution.var_name) {
            restored.insert(substitution.var_name.clone());
            continue;
        }
        let Some((variable, as_output)) = source_observable_variable(source, substitution)
            .or_else(|| source_observable_variable(prepared, substitution))
        else {
            continue;
        };
        let Some(lhs) = restored_target_reference(substitution, variable) else {
            continue;
        };
        let scalar_count = variable.size();
        let span = variable.source_span;
        if as_output {
            lowered
                .variables
                .outputs
                .insert(substitution.var_name.clone(), variable.clone());
        } else {
            lowered
                .variables
                .algebraics
                .insert(substitution.var_name.clone(), variable.clone());
        }
        lowered
            .continuous
            .equations
            .push(dae::Equation::explicit_with_scalar_count(
                lhs,
                substitution.expr.clone(),
                span,
                "causal reconstruction after structural elimination",
                scalar_count,
            ));
        restored.insert(substitution.var_name.clone());
    }
    restored
}

fn source_observable_variable<'a>(
    source: &'a dae::Dae,
    substitution: &Substitution,
) -> Option<(&'a dae::Variable, bool)> {
    let name = &substitution.var_name;
    if let Some(variable) = source.variables.outputs.get(name) {
        return Some((variable, true));
    }
    source
        .variables
        .states
        .get(name)
        .or_else(|| source.variables.algebraics.get(name))
        .map(|variable| (variable, false))
}

fn restored_target_reference(
    substitution: &Substitution,
    variable: &dae::Variable,
) -> Option<Reference> {
    substitution.var_ref.clone().or_else(|| {
        variable
            .component_ref
            .clone()
            .map(Reference::from_component_reference)
            .or_else(|| {
                (variable.origin == dae::VariableOrigin::Generated)
                    .then(|| Reference::generated(substitution.var_name.as_str()))
            })
    })
}

fn continuous_variable_exists(dae: &dae::Dae, name: &VarName) -> bool {
    dae.variables.states.contains_key(name)
        || dae.variables.algebraics.contains_key(name)
        || dae.variables.outputs.contains_key(name)
}
