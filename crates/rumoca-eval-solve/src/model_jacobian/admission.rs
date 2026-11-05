use super::*;
use rumoca_ir_solve::{ComputeNode, LinearOp, SolveRealFormat};

pub(super) struct Program<'source> {
    pub(super) ops: &'source [LinearOp],
    pub(super) outputs: Vec<usize>,
    pub(super) span: Span,
}

pub(super) fn unsupported(reason: &'static str, span: Option<Span>) -> ModelStateJacobianError {
    ModelStateJacobianError::Unsupported { reason, span }
}

pub(super) fn programs<'source>(
    model: &'source SolveModel,
    source: &ContinuousNumericalSource<'source>,
) -> Result<Vec<Program<'source>>, ModelStateJacobianError> {
    let problem = &model.problem;
    let n = model.state_scalar_count();
    if n == 0 || source.layout().y_scalars() != n {
        return Err(unsupported(
            "requires nonempty state-only solver coordinates",
            None,
        ));
    }
    if model
        .problem
        .solve_layout
        .variable_storage_runs
        .iter()
        .any(|run| {
            matches!(run.base, rumoca_ir_solve::ScalarSlot::Y { .. })
                && (run.role != rumoca_ir_solve::SolveVariableStorageRole::State
                    || run.value_kind != rumoca_ir_solve::SolveVariableValueKind::Real)
        })
    {
        return Err(unsupported("requires Real state storage owners", None));
    }
    if source.calls().arithmetic().real_format() != SolveRealFormat::Binary64 {
        return Err(unsupported("requires Binary64 source arithmetic", None));
    }
    if rumoca_ir_solve::solve_has_events(problem)
        || rumoca_ir_solve::solve_has_runtime_events(problem)
        || rumoca_ir_solve::solve_has_clocks(problem)
    {
        return Err(unsupported("event, history or clock owners", None));
    }
    let continuous = source.continuous();
    if !continuous.implicit_rhs.is_empty()
        || !continuous.manifold_residual.is_empty()
        || !source.assignments().is_empty()
    {
        return Err(unsupported(
            "implicit, manifold or algebraic refresh owners",
            None,
        ));
    }
    if source.derivative_source().output_count() != n {
        return Err(unsupported(
            "derivative output domain is not the state domain",
            None,
        ));
    }
    let mut programs = Vec::new();
    let mut covered = vec![false; n];
    let mut cursor = 0;
    for (node, value) in source.derivative_source().source().nodes.iter().enumerate() {
        let ComputeNode::ScalarPrograms(block) = value else {
            return Err(unsupported(
                "structured derivative node requires compact constructor",
                None,
            ));
        };
        let indices = block
            .compute_block_output_indices("shared state Jacobian", node, cursor)
            .map_err(|_| {
                unsupported("invalid canonical output placement", block.program_span(0))
            })?;
        cursor = block
            .advance_compute_block_output_cursor("shared state Jacobian", node, cursor)
            .map_err(|_| {
                unsupported("canonical output placement overflow", block.program_span(0))
            })?;
        let mut ordinal = 0;
        for (index, ops) in block.programs().iter().enumerate() {
            let span = block
                .program_span(index)
                .ok_or_else(|| unsupported("program provenance missing", None))?;
            check_operations(ops, span)?;
            let count = rumoca_ir_solve::ScalarProgramBlock::program_output_count(ops);
            let outputs = indices
                .get(ordinal..ordinal + count)
                .ok_or_else(|| unsupported("incomplete output placement", Some(span)))?;
            claim_outputs(outputs, &mut covered, span)?;
            programs.push(Program {
                ops,
                outputs: outputs.to_vec(),
                span,
            });
            ordinal += count;
        }
    }
    if programs.is_empty() || covered.iter().any(|value| !value) {
        return Err(unsupported("incomplete state derivative coverage", None));
    }
    Ok(programs)
}

fn claim_outputs(
    outputs: &[usize],
    covered: &mut [bool],
    span: Span,
) -> Result<(), ModelStateJacobianError> {
    for &output in outputs {
        if output >= covered.len() || covered[output] {
            return Err(unsupported(
                "duplicate or foreign derivative output",
                Some(span),
            ));
        }
        covered[output] = true;
    }
    Ok(())
}

fn check_operations(ops: &[LinearOp], span: Span) -> Result<(), ModelStateJacobianError> {
    use rumoca_ir_solve::{BinaryOp as B, UnaryOp as U};
    for operation in ops {
        let supported = match operation {
            LinearOp::Const { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::Move { .. }
            | LinearOp::StoreOutput { .. } => true,
            LinearOp::Binary { op, .. } => matches!(op, B::Add | B::Sub | B::Mul | B::Div | B::Pow),
            LinearOp::Unary { op, .. } => matches!(
                op,
                U::Neg
                    | U::Abs
                    | U::Sqrt
                    | U::Sin
                    | U::Cos
                    | U::Tan
                    | U::Asin
                    | U::Acos
                    | U::Atan
                    | U::Sinh
                    | U::Cosh
                    | U::Tanh
                    | U::Exp
                    | U::Log
                    | U::Log10
            ),
            _ => false,
        };
        if !supported {
            return Err(unsupported(operation.kind_name(), Some(span)));
        }
    }
    Ok(())
}
