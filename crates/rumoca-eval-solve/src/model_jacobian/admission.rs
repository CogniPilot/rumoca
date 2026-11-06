mod compact;

use super::*;
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve::{ComputeNode, LinearOp, SolveRealFormat};

pub(in crate::model_jacobian) use compact::CompactNode;

pub(super) struct Program<'source> {
    pub(super) ops: std::borrow::Cow<'source, [LinearOp]>,
    pub(super) outputs: Vec<usize>,
    pub(super) span: Span,
}

/// One compact derivative node: its checked facts, its source node and
/// domain, and the derivative rows its points write in row-major point
/// order. Admission is target-independent and expands nothing; the selected
/// target decides at construction whether the compact map or its scalarized
/// programs are issued.
pub(super) struct Compact<'source> {
    pub(super) node: CompactNode<'source>,
    pub(super) source: &'source ComputeNode,
    pub(super) domain: &'source StructuredIndexDomain,
    pub(super) rows: Vec<usize>,
}

impl Compact<'_> {
    /// Operations the scalarized form issues: points × base operations, in
    /// checked arithmetic and before any row is expanded.
    pub(super) fn scalarized_operations(&self) -> Option<usize> {
        self.rows.len().checked_mul(self.node.base_ops.len())
    }
}

/// The derivative source in canonical program order. A scalar program is one
/// program identity; a compact node's points are the next `rows.len()`, as
/// the scalarized source numbers them.
pub(super) enum Unit<'source> {
    Scalar(Program<'source>),
    Compact(Compact<'source>),
}

impl Unit<'_> {
    pub(super) fn span(&self) -> Span {
        match self {
            Self::Scalar(program) => program.span,
            Self::Compact(compact) => compact.node.span,
        }
    }
}

pub(super) fn unsupported(reason: &'static str, span: Option<Span>) -> ModelStateJacobianError {
    ModelStateJacobianError::Unsupported { reason, span }
}

pub(super) fn programs<'source>(
    model: &'source SolveModel,
    source: &ContinuousNumericalSource<'source>,
) -> Result<Vec<Unit<'source>>, ModelStateJacobianError> {
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
        let block = match value {
            ComputeNode::ScalarPrograms(block) => block,
            // Compact nodes, refusing every other structured node by name.
            _ => {
                let compact = admit_compact(value, n, &mut covered)?;
                cursor = compact
                    .rows
                    .iter()
                    .map(|row| row + 1)
                    .fold(cursor, usize::max);
                programs.push(Unit::Compact(compact));
                continue;
            }
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
            programs.push(Unit::Scalar(Program {
                ops: std::borrow::Cow::Borrowed(ops.as_slice()),
                outputs: outputs.to_vec(),
                span,
            }));
            ordinal += count;
        }
    }
    if programs.is_empty() || covered.iter().any(|value| !value) {
        return Err(unsupported("incomplete state derivative coverage", None));
    }
    Ok(programs)
}

/// The selected target's expansion step for a maps-disabled selection: the
/// scalarized operations of all compact nodes together, in checked
/// arithmetic, must fit `bound`, or the whole product refuses as Unsupported
/// (directional AD remains) before any row is expanded.
pub(super) fn check_scalarized_bound(
    units: &[Unit<'_>],
    bound: usize,
) -> Result<(), ModelStateJacobianError> {
    let mut total = 0usize;
    for unit in units {
        if let Unit::Compact(compact) = unit {
            total = compact
                .scalarized_operations()
                .and_then(|operations| total.checked_add(operations))
                .filter(|total| *total <= bound)
                .ok_or_else(|| {
                    unsupported(
                        "compact nodes exceed the target's scalar expansion bound",
                        Some(compact.node.span),
                    )
                })?;
        }
    }
    Ok(())
}

/// The node's scalarized programs, one per point in row-major order, exactly
/// as the scalar reference expands it; each must write its own point's row.
/// Called only at the target step, after `check_scalarized_bound`.
pub(super) fn scalarized_programs(
    compact: &Compact<'_>,
) -> Result<Vec<Program<'static>>, ModelStateJacobianError> {
    let (node, rows, span) = (compact.source, compact.rows.as_slice(), compact.node.span);
    let refuse = |reason| unsupported(reason, Some(span));
    let single = rumoca_ir_solve::ComputeBlock {
        nodes: vec![node.clone()],
    };
    let block = crate::compute_block_scalarize::to_scalar_program_block(&single)
        .map_err(|_| refuse("compact node does not scalarize"))?;
    let indices = block
        .compute_block_output_indices("shared state Jacobian", 0, 0)
        .map_err(|_| refuse("invalid scalarized output placement"))?;
    if block.programs().len() != rows.len() || indices != rows {
        return Err(refuse(
            "scalarized compact node disagrees with its placement",
        ));
    }
    block
        .programs()
        .iter()
        .zip(rows)
        .map(|(ops, row)| {
            check_operations(ops, span)?;
            if rumoca_ir_solve::ScalarProgramBlock::program_output_count(ops) != 1 {
                return Err(refuse("scalarized compact point must store one output"));
            }
            Ok(Program {
                ops: std::borrow::Cow::Owned(ops.clone()),
                outputs: vec![*row],
                span,
            })
        })
        .collect()
}

/// Admit one compact derivative node: its checked facts, and the rows its
/// points write, claimed once. Strided constants refuse by name for now.
fn admit_compact<'source>(
    value: &'source ComputeNode,
    n: usize,
    covered: &mut [bool],
) -> Result<Compact<'source>, ModelStateJacobianError> {
    let (ComputeNode::Map {
        domain,
        output_map,
        base_ops,
        load_strides,
        const_strides,
        span,
        ..
    }
    | ComputeNode::AffineStencil {
        domain,
        output_map,
        base_ops,
        load_strides,
        const_strides,
        span,
        ..
    }) = value
    else {
        return Err(unsupported(
            "structured derivative node requires compact constructor",
            None,
        ));
    };
    if !const_strides.is_empty() {
        return Err(unsupported(
            "compact strided constants are not issued yet",
            Some(*span),
        ));
    }
    let node = compact::admit(domain, output_map, base_ops, load_strides, n, *span)?;
    // The IR's own output expansion names the rows; the admitted affine
    // output key must agree with it point by point.
    let rows = output_map
        .output_indices(domain)
        .map_err(|_| unsupported("invalid compact output placement", Some(*span)))?;
    if !compact::rows_match(&node, &rows) {
        return Err(unsupported(
            "compact output placement disagrees with its affine key",
            Some(*span),
        ));
    }
    claim_outputs(&rows, covered, *span)?;
    Ok(Compact {
        node,
        source: value,
        domain,
        rows,
    })
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
