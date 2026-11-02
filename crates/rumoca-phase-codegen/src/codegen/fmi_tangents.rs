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
    extra_len: usize,
    sizes_len: usize,
    scratch_len: usize,
}

fn program_value(block: &ScalarProgramBlock) -> Result<Value, CodegenError> {
    Ok(Value::from_object(ScalarProgramPlan::new(Arc::new(
        block.clone(),
    ))?))
}

fn assignment_values(tangent: &FmiTangentPlan<'_>) -> Result<(Vec<Value>, usize), CodegenError> {
    let mut assignments = Vec::with_capacity(tangent.assignments().len());
    let mut scratch_len = 0usize;
    for segment in tangent.assignments() {
        let outputs = segment.program().output_indices();
        if outputs.len() != segment.targets().len()
            || outputs.iter().copied().ne(0..segment.targets().len())
        {
            return Err(CodegenError::template(
                "FMI tangent segment does not cover its simultaneous target inventory",
            ));
        }
        scratch_len = scratch_len.max(segment.targets().len());
        assignments.push(minijinja::context! {
            targets => segment.targets(),
            plan => program_value(segment.program())?,
        });
    }
    Ok((assignments, scratch_len))
}

fn implicit_value(tangent: &FmiTangentPlan<'_>) -> Result<NumericalProjection, CodegenError> {
    let Some(system) = tangent.implicit() else {
        return Ok(absent_projection());
    };
    let n = system.targets().len();
    let matrix_len = n
        .checked_mul(n)
        .ok_or_else(|| CodegenError::template("FMI implicit tangent matrix extent overflow"))?;
    let extra_len = tangent
        .seed_len()
        .checked_add(matrix_len)
        .and_then(|length| length.checked_add(n))
        .ok_or_else(|| CodegenError::template("FMI implicit tangent workspace extent overflow"))?;
    let sizes_len = n
        .checked_mul(2)
        .ok_or_else(|| CodegenError::template("FMI implicit tangent pivot extent overflow"))?;
    Ok(NumericalProjection {
        value: minijinja::context! {
            present => true,
            n => n,
            targets => system.targets(),
            rows => system.rows(),
            subtract_targets => system.subtract_targets(),
            tolerance => rumoca_eval_solve::projection_policy::ALGEBRAIC_REFRESH_TOLERANCE,
            matrix_len => matrix_len,
            plan => program_value(system.program())?,
        },
        extra_len,
        sizes_len,
        scratch_len: system.program().output_count(),
    })
}

fn consistency_value(tangent: &FmiTangentPlan<'_>) -> Result<NumericalProjection, CodegenError> {
    let Some(checks) = tangent.consistency() else {
        return Ok(absent_projection());
    };
    let extra_len = tangent
        .seed_len()
        .checked_add(checks.rows().len())
        .ok_or_else(|| CodegenError::template("FMI consistency workspace extent overflow"))?;
    Ok(NumericalProjection {
        value: minijinja::context! {
            present => true,
            rows => checks.rows(),
            tolerance => rumoca_eval_solve::projection_policy::ALGEBRAIC_REFRESH_TOLERANCE,
            plan => program_value(checks.program())?,
        },
        extra_len,
        sizes_len: 0,
        scratch_len: checks.program().output_count(),
    })
}

fn absent_projection() -> NumericalProjection {
    NumericalProjection {
        value: minijinja::context! { present => false },
        extra_len: 0,
        sizes_len: 0,
        scratch_len: 0,
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
    let (assignments, assignment_scratch) = assignment_values(tangent)?;
    let implicit = implicit_value(tangent)?;
    let consistency = consistency_value(tangent)?;
    let scratch_len = tangent
        .rhs()
        .output_count()
        .max(assignment_scratch)
        .max(implicit.scratch_len)
        .max(consistency.scratch_len);
    // Implicit solve and consistency execute sequentially. Both own their
    // private saved-seed region; it is safe to reuse the larger extra region.
    let extra_len = implicit.extra_len.max(consistency.extra_len);
    let problem = tangent.component().problem();
    let work_len = tangent
        .seed_len()
        .checked_add(problem.solve_layout.state_scalar_count())
        .and_then(|length| length.checked_add(scratch_len))
        .and_then(|length| length.checked_add(extra_len))
        .ok_or_else(|| CodegenError::template("FMI tangent workspace extent overflow"))?;
    Ok(ProjectedTangent {
        value: minijinja::context! {
            available => true,
            implicit => implicit.value,
            consistency => consistency.value,
            assignments => assignments,
            rhs => program_value(tangent.rhs())?,
            seed_len => tangent.seed_len(),
            time_seed => tangent.time_seed_index(),
            scratch_len => scratch_len,
            y_len => problem.layout.y_scalars(),
            p_len => problem.layout.p_scalars(),
            state_len => problem.solve_layout.state_scalar_count(),
        },
        work_len,
        sizes_len: implicit.sizes_len,
    })
}

fn unavailable(reason: &str) -> Value {
    minijinja::context! { available => false, refusal => reason }
}
