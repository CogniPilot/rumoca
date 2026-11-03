//! C-template projection of source-correlated total-tangent programs.
//!
//! The complete preparation owner issues all three numerical modes together.
//! Templates project that result into capability attributes and API bodies;
//! an incomplete profile cannot advertise support or acquire a partial API.

use std::sync::Arc;

use minijinja::Value;
use rumoca_eval_solve::fmi_tangents::{
    FmiDerivativePlans, FmiTangentMode, FmiTangentPlan, FmiTangentPlanError,
};
use rumoca_ir_solve::{ScalarProgramBlock, fmi::FmiCCodegenView};

use super::scalar_program_plan::ScalarProgramPlan;
use crate::errors::CodegenError;

pub(super) fn value(component: &FmiCCodegenView) -> Result<Value, CodegenError> {
    let plans = match FmiDerivativePlans::derive(component) {
        Ok(plans) => plans,
        Err(
            FmiTangentPlanError::IncompleteProfile(reason)
            | FmiTangentPlanError::ImplicitInventory(reason)
            | FmiTangentPlanError::InitializationInventory(reason),
        ) => return Ok(unavailable(&reason)),
        Err(FmiTangentPlanError::AlternateCoordinates) => {
            return Ok(unavailable(
                "alternate-coordinate sensitivity owner required",
            ));
        }
        Err(FmiTangentPlanError::Derivation(error)) => return Ok(unavailable(&error.to_string())),
        Err(error) => return Err(CodegenError::template(error.to_string())),
    };
    let continuous = tangent_value(plans.continuous(), FmiTangentMode::ContinuousOrStep)?;
    let initialization = tangent_value(plans.initialization(), FmiTangentMode::Initialization)?;
    let post = tangent_value(plans.post_initialization(), FmiTangentMode::EventOrStep)?;
    Ok(minijinja::context! {
        available => true,
        work_len => continuous.work_len.max(initialization.work_len).max(post.work_len),
        sizes_len => continuous.sizes_len.max(initialization.sizes_len).max(post.sizes_len),
        continuous => continuous.value,
        initialization => initialization.value,
        post => post.value,
    })
}

struct ProjectedTangent {
    value: Value,
    work_len: usize,
    sizes_len: usize,
}

struct NumericalProjection {
    value: Value,
}

fn program_value(block: &ScalarProgramBlock) -> Result<Value, CodegenError> {
    Ok(Value::from_object(ScalarProgramPlan::new(Arc::new(
        block.clone(),
    ))?))
}

fn assignment_values(tangent: &FmiTangentPlan<'_>) -> Result<Vec<Value>, CodegenError> {
    tangent
        .assignments()
        .iter()
        .map(|segment| {
            Ok(minijinja::context! {
                targets => segment.targets(),
                plan => program_value(segment.program())?,
            })
        })
        .collect()
}

/// The plan's issued workspace; one issued for other phase programs is a
/// contract failure.
fn issued_workspace<'plan, 'component>(
    tangent: &'plan FmiTangentPlan<'component>,
) -> Result<&'plan rumoca_ir_solve::SolveTangentWorkspace<'component>, CodegenError> {
    tangent
        .workspace()
        .map_err(|error| CodegenError::template(format!("FMI tangent workspace: {error}")))
}

fn implicit_value(tangent: &FmiTangentPlan<'_>) -> Result<NumericalProjection, CodegenError> {
    let Some(system) = tangent.implicit() else {
        return Ok(absent_projection());
    };
    Ok(NumericalProjection {
        value: minijinja::context! {
            present => true,
            n => system.targets().len(),
            targets => system.targets(),
            rows => system.rows(),
            subtract_targets => system.subtract_targets(),
            tolerance => rumoca_eval_solve::projection_policy::ALGEBRAIC_REFRESH_TOLERANCE,
            matrix_len => issued_workspace(tangent)?.implicit_matrix_len(),
            plan => program_value(system.program())?,
        },
    })
}

fn consistency_value(tangent: &FmiTangentPlan<'_>) -> Result<NumericalProjection, CodegenError> {
    let Some(checks) = tangent.consistency() else {
        return Ok(absent_projection());
    };
    Ok(NumericalProjection {
        value: minijinja::context! {
            present => true,
            rows => checks.rows(),
            tolerance => rumoca_eval_solve::projection_policy::ALGEBRAIC_REFRESH_TOLERANCE,
            plan => program_value(checks.program())?,
        },
    })
}

fn absent_projection() -> NumericalProjection {
    NumericalProjection {
        value: minijinja::context! { present => false },
    }
}

fn tangent_value(
    tangent: &FmiTangentPlan<'_>,
    expected_mode: FmiTangentMode,
) -> Result<ProjectedTangent, CodegenError> {
    if tangent.mode() != expected_mode {
        return Err(CodegenError::template(
            "FMI tangent mode does not match its issued consumer",
        ));
    }
    let assignments = assignment_values(tangent)?;
    let implicit = implicit_value(tangent)?;
    let consistency = consistency_value(tangent)?;
    let workspace = issued_workspace(tangent)?;
    let problem = tangent.component().problem();
    Ok(ProjectedTangent {
        value: minijinja::context! {
            available => true,
            implicit => implicit.value,
            consistency => consistency.value,
            assignments => assignments,
            rhs => program_value(tangent.rhs())?,
            seed_len => tangent.seed_len(),
            time_seed => tangent.time_seed_index(),
            scratch_len => workspace.scratch_len(),
            occurrences => Value::from_serialize(workspace.occurrences().collect::<Vec<_>>()),
            uses => minijinja::context! {
                seed => workspace.uses_seed(),
                states => workspace.uses_states(),
                scratch => workspace.uses_scratch(),
                private => workspace.uses_private(),
                pivots => workspace.uses_pivots(),
            },
            y_len => problem.layout.y_scalars(),
            p_len => problem.layout.p_scalars(),
            state_len => problem.solve_layout.state_scalar_count(),
        },
        work_len: workspace.work_len(),
        sizes_len: workspace.implicit_pivot_len(),
    })
}

fn unavailable(reason: &str) -> Value {
    minijinja::context! { available => false, refusal => reason }
}
