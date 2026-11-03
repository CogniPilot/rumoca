//! The source-owned parameter/algebraic relation after initialization.
//!
//! Event/Step requests may vary tunable parameters and discrete Real inputs.
//! Their dependent parameter bindings must propagate, but initial equations
//! and initial-state assignments must not run again. This prepares that
//! numerical distinction without widening any C mode or capability flag.

use super::initialization::{Relation, append_parameter_discretes, combined_targets};
use super::{
    FmiCCodegenView, FmiTangentMode, FmiTangentPlan, FmiTangentPlanError, algebraic_inventory,
    derive_rhs, to_scalar_program_block,
};

pub(super) fn derive(
    component: &FmiCCodegenView,
) -> Result<FmiTangentPlan<'_>, FmiTangentPlanError> {
    let problem = component.problem();
    let continuous = &problem.continuous;
    if !continuous.reduced_chart_set.charts.is_empty()
        || !continuous.manifold_projection_plan.is_empty()
    {
        return Err(FmiTangentPlanError::AlternateCoordinates);
    }
    if component.requires_event_iteration() {
        return Err(FmiTangentPlanError::ImplicitInventory(
            "post-initialization scalar-event transactions need their sensitivity owner".into(),
        ));
    }
    let y_len = problem.layout.y_scalars();
    let seed_len = rumoca_ir_solve::SolveTangentSchedule::seed_len(problem)
        .map_err(|error| FmiTangentPlanError::Contract(error.to_string()))?;
    let mut relation = Relation::default();
    let (rows, targets) = algebraic_inventory(component)?;
    let algebraic = to_scalar_program_block(&continuous.implicit_rhs)
        .map_err(|error| FmiTangentPlanError::Derivation(error.into()))?;
    relation.append(&algebraic, &rows, &targets, false)?;
    append_live_parameter_bindings(component, &mut relation)?;
    append_parameter_discretes(component, &mut relation)?;
    FmiTangentPlan::issue(
        component,
        FmiTangentMode::EventOrStep,
        Vec::new(),
        relation.finish(y_len, seed_len - 1)?,
        derive_rhs(component)?,
        seed_len,
        None,
    )
}

/// The same checked execution projection drives primal refresh and tangent
/// preparation. A mixed program executes whole; only its parameter results
/// constrain the post-initial relation. State-start results are private scratch,
/// and never replay initialization writes or introduce extra unknowns.
fn append_live_parameter_bindings(
    component: &FmiCCodegenView,
    relation: &mut Relation,
) -> Result<(), FmiTangentPlanError> {
    let init = &component.problem().initialization;
    let block = component.parameter_binding_source();
    if !block.shares_program_owner(init.update_rhs()) {
        return Err(FmiTangentPlanError::Contract(
            "binding execution does not retain the component source owner".into(),
        ));
    }
    for execution in component.parameter_binding_execution() {
        let rows = execution.parameter_rows();
        if rows.is_empty() {
            continue;
        }
        let slots = rows
            .iter()
            .map(|&row| {
                init.update_targets().get(row).copied().ok_or_else(|| {
                    FmiTangentPlanError::Contract("binding commit row has no target".into())
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let targets = combined_targets(component, &slots)?;
        // Relation::append retains complete selected programs and their source
        // spans, including unused state results and their failure obligations.
        relation.append(block, rows, &targets, true)?;
    }
    Ok(())
}
