//! Conservative admission of one complete original scalar program.

#[cfg(test)]
mod tests;

use rumoca_ir_solve::{BinaryOp, LinearOp, ScalarProgramBlock, VarLayout};

pub(super) fn execution_receipt(
    policy: rumoca_solver::SimExecutionPolicy,
    states: usize,
    external_tables: usize,
) -> rumoca_solver::SimExecutionReceipt {
    rumoca_solver::SimExecutionReceipt::admission(
        policy,
        states,
        external_tables,
        rumoca_solver::SimExecutionEngine::WasmProgram,
    )
}

#[cfg(test)]
fn model_context_admitted(
    policy: rumoca_solver::SimExecutionPolicy,
    states: usize,
    external_tables: usize,
) -> bool {
    execution_receipt(policy, states, external_tables).is_compiled()
}

pub(super) fn single_program(
    source: &ScalarProgramBlock,
    program: usize,
    layout: &VarLayout,
) -> Result<ScalarProgramBlock, String> {
    let ops = source
        .program(program)
        .ok_or("WASM source program is absent")?;
    if !matches!(ops.last(), Some(LinearOp::StoreOutput { .. }))
        || ScalarProgramBlock::program_output_count(ops) != 1
    {
        return Err("WASM ME profile requires one terminal scalar output".into());
    }
    for op in ops {
        match op {
            LinearOp::LoadY { index, .. } if *index < layout.y_scalars() => {}
            LinearOp::LoadP { index, .. } if *index < layout.p_scalars() => {}
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::StoreOutput { .. } => {}
            LinearOp::Binary { op, .. } if !matches!(op, BinaryOp::Min | BinaryOp::Max) => {}
            _ => {
                return Err(format!(
                    "WASM ME scalar profile declines {}",
                    op.kind_name()
                ));
            }
        }
    }
    let span = source
        .program_span(program)
        .ok_or("WASM source span is absent")?;
    ScalarProgramBlock::with_program_spans(vec![ops.to_vec()], vec![span])
        .map_err(|error| error.to_string())
}

pub(super) fn validate_inputs(
    layout: &VarLayout,
    y: usize,
    p: usize,
    external_tables: usize,
) -> Result<(), String> {
    if y != layout.y_scalars() || p != layout.p_scalars() {
        return Err("WASM ME input lengths differ from the issued layout".into());
    }
    if external_tables != 0 {
        return Err("WASM ME scalar profile declines external tables".into());
    }
    Ok(())
}
