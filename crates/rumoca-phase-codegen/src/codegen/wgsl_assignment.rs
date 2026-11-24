//! WebGPU code for independent evaluations of a stateless Solve schedule.
use super::{CodegenError, create_environment, solve_template_compute_block_json};

/// Render a checked assignment schedule as a binding-free WGSL value function.
pub fn render_wgsl_value_function(
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
) -> Result<String, CodegenError> {
    let layout = schedule.work_layout();
    let mut stages = Vec::with_capacity(schedule.stages().len());
    for stage in schedule.stages() {
        let range = stage
            .target_range()
            .ok_or_else(|| CodegenError::template("callable WGSL needs dense targets"))?;
        let kernel = stage.value_kernel();
        if range.len() != kernel.len()? {
            return Err(CodegenError::template(
                "callable WGSL stage output count mismatch",
            ));
        }
        stages.push(minijinja::context! {
            start => range.start,
            block => solve_template_compute_block_json(kernel)?
        });
    }
    let context = minijinja::context! {
        stages => stages,
        p_count => layout.p_scalars().max(1),
        y_count => layout.y_scalars().max(1)
    };
    Ok(create_environment().render_str(include_str!("wgsl_value_function.jinja"), context)?)
}
