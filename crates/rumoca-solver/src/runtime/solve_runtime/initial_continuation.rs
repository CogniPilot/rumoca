//! Certified coverage for the initialization homotopy continuation.
//!
//! `SolveLayout::initial_homotopy_parameter_index` allocates a hidden `P` slot
//! (λ) for models whose DAE carries `homotopy(actual, simplified)` (MLS 3.6
//! §3.7.4.3). λ is seeded to `1.0`, so every evaluation outside the
//! initialization continuation reads exactly `actual` — the trivial
//! implementation §3.7.4.3 sanctions. The continuation is the only thing that
//! ever moves λ: it walks λ from `0` to `1` **around the solves whose rows read
//! λ**, so the sweep can steer those unknowns off the simplified branch onto the
//! actual one, and restores `1.0` when it is done.
//!
//! That promise is only real if the driver and the checked artifact agree on
//! which solves the sweep governs. [`InitialContinuationCoverage`] is the single
//! place that answer lives: it is built from the two plans the driver re-solves
//! at every continuation value and it is what
//! `SolveRuntime::project_initial_variables` consults to decide what to
//! re-solve. A runtime that stops driving one of those solves must drop it from
//! the coverage, and models that need it are then rejected instead of silently
//! simulating whichever root a cold guess lands on (as observed on
//! `Modelica.Electrical.Analog.Examples.OpAmps.SignalGenerator`, which collapsed
//! to the trivial all-zero root because the sweep steered nothing at all).
//!
//! # One index space: the equation index
//!
//! Everything in this module is stated in **equation index** space — the row
//! index of the residual vector a system evaluates. Every value that arrives in
//! another space is translated explicitly, once, at its point of entry:
//!
//! * A [`solve::ScalarProgramBlock`] numbers its *programs* densely, but each
//!   stored output carries its own identity in `output_indices`. Program index
//!   and equation index coincide only for blocks built by
//!   `ScalarProgramBlock::with_program_spans`, which is why the unit fixtures
//!   below use real permutations: `BistableLoop` emits `[2, 1]`, `E10_Mixed`
//!   `[4, 1, 2, 3]`, `E11_Shifted` `[5, 4, 1, 2, 3]`. [`lambda_reading_equations`]
//!   is the only place a program index is turned into an equation index.
//! * `continuous.implicit_row_targets` and `initialization.row_targets()` are
//!   indexed by equation index.
//! * `InitializationProjectionBlock::rows` and `AlgebraicProjectionBlock::rows`
//!   are equation indices into the corresponding residual vector.
//! * [`AlgebraicRefreshRow::equation_index`] is an equation index;
//!   its opaque canonical scalar-program source is resolved only by the final
//!   evaluator adapter. Only `equation_index` is read here.
//!
//! [`AlgebraicRefreshRow::equation_index`]: rumoca_ir_solve::AlgebraicRefreshRow::equation_index
//!
//! # Acceptance contract (SPEC 0008 / SPEC 0036)
//!
//! Accepted:
//!
//! * No λ slot is allocated at all (the DAE carried no `homotopy`), in which
//!   case there is no continuation and no coverage object.
//! * λ is allocated and read by at least one lowered row of the model. Rows
//!   split into two groups:
//!   * **Steered**: the iteratively solved blocks λ reaches: an algebraic
//!     refresh projection block, or an initialization projection block, whose
//!     rows read λ, read a value λ determines, or start from a seed row that
//!     does. A value λ determines is the target of an exact assignment that
//!     reads λ or such a value, or an unknown of a steered block. The
//!     continuation drives these; this module proves a plan names each
//!     λ-reading algebraic row.
//!   * **Exactly assigned**: a λ-reading `continuous.implicit_rhs` row the
//!     refresh executes as an exact assignment. Its value is a function of
//!     already settled values at every λ, so continuing it selects no root.
//!     When no iterative block consumes it, it is evaluated at λ = 1 like the
//!     unsteered rows below. MLS 3.7 §3.7.4.2 locates the homotopy iteration
//!     in "the smallest iteration loop" from the first BLT block with homotopy
//!     to the last nonlinear block, and there is no such loop here.
//!   * **Unsteered** — every other row family the lowering can put a λ read in:
//!     `continuous.derivative_rhs` (`der(y) = homotopy(...)`),
//!     `discrete.rhs` (`when c then z = homotopy(...); end when;`),
//!     `events.root_conditions`, `continuous.residual`,
//!     `continuous.manifold_residual`, `initialization.update_rhs()`,
//!     `visible_value_rows`, and `initialization.residual()` rows that no
//!     projection block solves (steady-state `initial equation der(x) = 0`
//!     against a `der(x) = homotopy(...)` equation). Nothing solves these rows
//!     for an unknown during the continuation, so they are evaluated at λ = 1,
//!     which is exactly `actual` — MLS §3.7.4.3's explicitly permitted trivial
//!     implementation `homotopy(actual, simplified) = actual`. They are legal
//!     and carry no coverage requirement.
//!
//! Rejected:
//!
//! * λ is allocated but **no** lowered row anywhere in the problem reads it: the
//!   `simplified` operand disappeared between DAE and Solve, which SPEC 0036
//!   forbids doing silently.
//! * λ is allocated outside the compiled parameter vector.
//! * A λ-reading `continuous.implicit_rhs` equation whose
//!   `implicit_row_targets` entry is an algebraic slot that no algebraic refresh
//!   plan names. The sweep would report success without ever having solved that
//!   row.
//!
//! Initialization row targets are derived from the same checked plan. The
//! former independent target/row mismatch is rejected during IR construction.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_eval_solve::{EvalSolveError, PreparedScalarProgramBlock, to_scalar_program_block};
use rumoca_ir_solve as solve;

/// The exact solves the initialization homotopy continuation drives.
///
/// Present only for models that allocate a continuation parameter. Row indices
/// are equation indices — see the module documentation.
#[derive(Clone, Debug)]
pub(crate) struct InitialContinuationCoverage {
    /// Hidden `P` slot the driver advances from `0` to `1`.
    lambda_index: usize,
    /// `continuous.implicit_rhs` equations of the iteratively solved refresh
    /// blocks λ reaches. Non-empty means the sweep has to re-run that refresh
    /// at every continuation value.
    steered_implicit_equations: BTreeSet<usize>,
    /// `initialization.residual()` equations the initialization projection
    /// plan solves that read λ or a value λ determines. Non-empty means the
    /// plan itself carries part of the sweep.
    steered_initialization_equations: BTreeSet<usize>,
    /// Whether a steered initialization row reads a refresh value λ
    /// determines, so the sweep must re-run the refresh that produces it.
    initialization_reads_refresh: bool,
}

impl InitialContinuationCoverage {
    /// Scalarize the initialization residual and certify the model's homotopy
    /// continuation coverage against it.
    ///
    /// The scalarized block is returned alongside the coverage because
    /// `SolveRuntime` needs the very block that was certified, not a second
    /// scalarization of the same compute block.
    pub(super) fn certify_runtime_blocks(
        model: &solve::SolveModel,
        implicit_scalar_rhs: &PreparedScalarProgramBlock,
        algebraic_refresh: &solve::RefreshPlan,
    ) -> Result<(solve::ScalarProgramBlock, Option<Self>), EvalSolveError> {
        let initial_scalar_residual =
            to_scalar_program_block(model.problem.initialization.residual())?;
        let coverage = Self::certify(
            model,
            implicit_scalar_rhs.block(),
            &initial_scalar_residual,
            algebraic_refresh,
        )?;
        Ok((initial_scalar_residual, coverage))
    }

    /// Certify that the continuation can steer every solve λ reaches.
    ///
    /// Returns `Ok(None)` when the model allocates no continuation parameter.
    pub(crate) fn certify(
        model: &solve::SolveModel,
        implicit_block: &solve::ScalarProgramBlock,
        initial_block: &solve::ScalarProgramBlock,
        algebraic_refresh: &solve::RefreshPlan,
    ) -> Result<Option<Self>, EvalSolveError> {
        let Some(lambda_index) = model.problem.solve_layout.initial_homotopy_parameter_index else {
            return Ok(None);
        };
        let parameter_len = model.problem.solve_layout.compiled_parameter_len;
        if lambda_index >= parameter_len {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "initial homotopy parameter index {lambda_index} is outside the \
                     {parameter_len} compiled parameters"
                ),
                span: None,
            });
        }

        let initial_reads = lambda_reading_equations(initial_block, lambda_index)?;
        let implicit_reads = lambda_reading_equations(implicit_block, lambda_index)?;
        if initial_reads.is_empty()
            && implicit_reads.is_empty()
            && !model_reads_parameter(model, lambda_index)
        {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "the model allocates initial homotopy parameter slot {lambda_index} but no \
                     lowered row reads it; the homotopy simplified operand was dropped during \
                     Solve lowering"
                ),
                span: None,
            });
        }

        let refresh_equations = algebraic_refresh_equations(algebraic_refresh);
        let lambda_algebraic_rows =
            certify_implicit_rows(model, implicit_block, &implicit_reads, &refresh_equations)?;
        let flow = LambdaFlow::derive(
            implicit_block,
            algebraic_refresh,
            &implicit_reads,
            lambda_algebraic_rows,
        )?;
        let (steered_initialization_equations, initialization_reads_refresh) =
            certify_initialization_rows(model, initial_block, &initial_reads, &flow.determined)?;

        Ok(Some(Self {
            lambda_index,
            steered_implicit_equations: flow.steered,
            steered_initialization_equations,
            initialization_reads_refresh,
        }))
    }

    /// The parameter the driver sweeps, or `None` when the continuation would
    /// steer nothing.
    ///
    /// Every λ read can sit in a row no continuation-driven solve owns
    /// (`der(y) = homotopy(...)`, a `when` body, a steady-state initial
    /// equation). Sweeping around those would walk λ from `0` to `1` having
    /// steered nothing and would only cost solves, so the driver leaves λ at its
    /// seeded `1.0` and every homotopy expression reads exactly `actual` — MLS
    /// §3.7.4.3's trivial implementation.
    pub(crate) fn sweep_parameter_index(&self) -> Option<usize> {
        (!self.steered_implicit_equations.is_empty()
            || !self.steered_initialization_equations.is_empty())
        .then_some(self.lambda_index)
    }

    /// Whether the continuation must re-solve the algebraic refresh at every
    /// continuation value. False when no λ-reading implicit equation is solved
    /// during initialization, in which case the initialization projection plan
    /// carries the whole sweep on its own.
    pub(crate) fn drives_algebraic_refresh(&self) -> bool {
        !self.steered_implicit_equations.is_empty() || self.initialization_reads_refresh
    }
}

/// Select continuation rows directly from the checked initialization owner:
/// the solved rows that read λ or a refresh value λ determines. The flag
/// reports whether any solved row reads such a refresh value.
fn certify_initialization_rows(
    model: &solve::SolveModel,
    initial_block: &solve::ScalarProgramBlock,
    initial_reads: &BTreeMap<usize, usize>,
    determined: &BTreeSet<usize>,
) -> Result<(BTreeSet<usize>, bool), EvalSolveError> {
    let positions = if determined.is_empty() {
        BTreeMap::new()
    } else {
        equation_positions(initial_block)?
    };
    let mut steered = BTreeSet::new();
    let mut reads_refresh = false;
    for row in model
        .problem
        .initialization
        .projection_plan()
        .blocks
        .iter()
        .flat_map(|block| block.rows.iter().copied())
    {
        let reads_determined = !determined.is_empty()
            && reads_any(
                &equation_y_reads(initial_block, &positions, row),
                determined,
            );
        reads_refresh |= reads_determined;
        if reads_determined || initial_reads.contains_key(&row) {
            steered.insert(row);
        }
    }
    Ok((steered, reads_refresh))
}

/// How λ flows through the algebraic refresh schedule.
///
/// An exact assignment is a function of already settled values at every λ, so
/// it selects no root: it only carries λ to its target. An iteratively solved
/// block selects a root, so it is steered when a row reads λ or a value λ
/// determines, or when a seed row that does starts one of its unknowns. A
/// steered block's unknowns are values λ determines. Only the issued stage
/// schedule proves a row is executed as an exact assignment; a plan without
/// one keeps every λ-reading algebraic row steered.
struct LambdaFlow {
    /// Equations of the steered iteratively solved refresh blocks.
    steered: BTreeSet<usize>,
    /// Solver-Y coordinates whose refreshed value depends on λ.
    determined: BTreeSet<usize>,
}

enum FlowNode {
    Exact {
        equation: usize,
        target: usize,
        reads: solve::OutputYReads,
    },
    Seed {
        equation: usize,
        target: usize,
        reads: solve::OutputYReads,
    },
    Iterative {
        rows: Vec<usize>,
        unknowns: Vec<usize>,
        reads: Vec<solve::OutputYReads>,
    },
}

impl LambdaFlow {
    fn derive(
        implicit_block: &solve::ScalarProgramBlock,
        refresh: &solve::RefreshPlan,
        implicit_reads: &BTreeMap<usize, usize>,
        lambda_algebraic_rows: BTreeSet<usize>,
    ) -> Result<Self, EvalSolveError> {
        if refresh.value_stages.is_empty() {
            return Ok(Self {
                steered: lambda_algebraic_rows,
                determined: BTreeSet::new(),
            });
        }
        let nodes = flow_nodes(implicit_block, refresh)?;
        let mut flow = Self {
            steered: BTreeSet::new(),
            determined: BTreeSet::new(),
        };
        let mut seeded = BTreeSet::new();
        loop {
            let before = (flow.steered.len(), flow.determined.len(), seeded.len());
            for node in &nodes {
                flow.visit(node, implicit_reads, &mut seeded);
            }
            if before == (flow.steered.len(), flow.determined.len(), seeded.len()) {
                return Ok(flow);
            }
        }
    }

    /// Carry λ through one node; `seeded` collects the unknowns a λ-dependent
    /// seed starts.
    fn visit(
        &mut self,
        node: &FlowNode,
        implicit_reads: &BTreeMap<usize, usize>,
        seeded: &mut BTreeSet<usize>,
    ) {
        let reads_lambda = |equation: &usize| implicit_reads.contains_key(equation);
        match node {
            FlowNode::Exact {
                equation,
                target,
                reads,
            } if reads_lambda(equation) || reads_any(reads, &self.determined) => {
                self.determined.insert(*target);
            }
            FlowNode::Seed {
                equation,
                target,
                reads,
            } if reads_lambda(equation) || reads_any(reads, &self.determined) => {
                seeded.insert(*target);
            }
            FlowNode::Iterative {
                rows,
                unknowns,
                reads,
            } if rows.iter().any(reads_lambda)
                || reads.iter().any(|row| reads_any(row, &self.determined))
                || unknowns.iter().any(|unknown| seeded.contains(unknown)) =>
            {
                self.steered.extend(rows.iter().copied());
                self.determined.extend(unknowns.iter().copied());
            }
            FlowNode::Exact { .. } | FlowNode::Seed { .. } | FlowNode::Iterative { .. } => {}
        }
    }
}

/// The refresh schedule as λ-flow nodes, in stage order.
fn flow_nodes(
    implicit_block: &solve::ScalarProgramBlock,
    refresh: &solve::RefreshPlan,
) -> Result<Vec<FlowNode>, EvalSolveError> {
    let positions = equation_positions(implicit_block)?;
    let reads = |equation: usize| equation_y_reads(implicit_block, &positions, equation);
    let row_nodes = |selection: &solve::RefreshRowSelection, exact: bool| {
        refresh
            .selected_rows(selection)
            .iter()
            .map(|row| {
                let (equation, target) = (row.equation_index(), row.target_index());
                let reads = reads(equation);
                if exact {
                    FlowNode::Exact {
                        equation,
                        target,
                        reads,
                    }
                } else {
                    FlowNode::Seed {
                        equation,
                        target,
                        reads,
                    }
                }
            })
            .collect::<Vec<_>>()
    };
    let mut nodes = Vec::new();
    for stage in &refresh.value_stages {
        match stage {
            solve::RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
                ..
            } => {
                nodes.extend(row_nodes(static_rows, false));
                nodes.extend(row_nodes(dynamic_rows, false));
            }
            solve::RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
                ..
            } => {
                nodes.extend(row_nodes(static_rows, true));
                nodes.extend(row_nodes(dynamic_rows, true));
            }
            solve::RefreshStage::ProjectionBlock {
                plan, seed_rows, ..
            } => {
                nodes.extend(row_nodes(seed_rows, false));
                nodes.extend(plan.blocks.iter().map(|block| FlowNode::Iterative {
                    rows: block.rows.clone(),
                    unknowns: block.y_indices.clone(),
                    reads: block.rows.iter().map(|&row| reads(row)).collect(),
                }));
            }
        }
    }
    Ok(nodes)
}

/// The solver-Y coordinates equation `equation` of `block` reads.
fn equation_y_reads(
    block: &solve::ScalarProgramBlock,
    positions: &BTreeMap<usize, (usize, usize)>,
    equation: usize,
) -> solve::OutputYReads {
    positions
        .get(&equation)
        .and_then(|&(program_index, output_offset)| {
            Some(solve::output_y_reads(
                block.program(program_index)?,
                output_offset,
            ))
        })
        .unwrap_or(solve::OutputYReads::Absent)
}

fn reads_any(reads: &solve::OutputYReads, values: &BTreeSet<usize>) -> bool {
    match reads {
        solve::OutputYReads::Absent => false,
        solve::OutputYReads::Bounded(reads) => !reads.is_disjoint(values),
        solve::OutputYReads::Unbounded => !values.is_empty(),
    }
}

/// Reject λ-reading `continuous.implicit_rhs` equations that target an algebraic
/// slot no refresh plan names, and return the λ-reading algebraic equations
/// initialization solves.
fn certify_implicit_rows(
    model: &solve::SolveModel,
    implicit_block: &solve::ScalarProgramBlock,
    implicit_reads: &BTreeMap<usize, usize>,
    refresh_equations: &BTreeSet<usize>,
) -> Result<BTreeSet<usize>, EvalSolveError> {
    let mut steered = BTreeSet::new();
    for (&equation, &program_index) in implicit_reads {
        if !implicit_equation_is_initialization_solved(model, equation) {
            continue;
        }
        if !refresh_equations.contains(&equation) {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "continuous.implicit_rhs equation {equation} reads the homotopy continuation \
                     parameter and targets an algebraic solved during initialization, but the \
                     algebraic refresh plan does not name it; the continuation would sweep lambda \
                     without ever steering that row"
                ),
                span: implicit_block.program_span(program_index),
            });
        }
        steered.insert(equation);
    }
    Ok(steered)
}

/// Equation indices in `block` whose program reads parameter slot `index`,
/// mapped to the program index that computes them so diagnostics can point at
/// the source occurrence.
///
/// This is the single translation from program space to equation space: a
/// program's stored outputs are consumed in order against `output_indices`,
/// exactly as `rumoca_eval_solve::refresh_plan` does when it builds refresh
/// rows.
fn lambda_reading_equations(
    block: &solve::ScalarProgramBlock,
    index: usize,
) -> Result<BTreeMap<usize, usize>, EvalSolveError> {
    let reading_programs = block
        .programs()
        .iter()
        .map(|program| {
            program
                .iter()
                .any(|op| linear_op_reads_parameter(op, index))
        })
        .collect::<Vec<_>>();
    Ok(equation_positions(block)?
        .into_iter()
        .filter(|(_, (program_index, _))| reading_programs[*program_index])
        .map(|(equation, (program_index, _))| (equation, program_index))
        .collect())
}

/// Each equation index of `block` mapped to the program that computes it and
/// the output offset within that program.
fn equation_positions(
    block: &solve::ScalarProgramBlock,
) -> Result<BTreeMap<usize, (usize, usize)>, EvalSolveError> {
    let mut positions = BTreeMap::new();
    let mut ordinal = 0usize;
    for (program_index, program) in block.programs().iter().enumerate() {
        for output_offset in 0..solve::ScalarProgramBlock::program_output_count(program) {
            let Some(equation) = block.output_indices().get(ordinal).copied() else {
                return Err(EvalSolveError::ShapeContract {
                    message: format!(
                        "scalar program output ordinal {ordinal} has no output index; the block \
                         declares {} output indices",
                        block.output_indices().len()
                    ),
                    span: block.program_span(program_index),
                });
            };
            positions.insert(equation, (program_index, output_offset));
            ordinal = ordinal
                .checked_add(1)
                .ok_or_else(|| EvalSolveError::ShapeContract {
                    message: "scalar program output ordinal overflows host index limits"
                        .to_string(),
                    span: block.program_span(program_index),
                })?;
        }
    }
    if ordinal != block.output_indices().len() {
        return Err(EvalSolveError::ShapeContract {
            message: format!(
                "scalar program block has {} output indices but {ordinal} StoreOutput ops",
                block.output_indices().len()
            ),
            span: block.first_source_span(),
        });
    }
    Ok(positions)
}

fn linear_op_reads_parameter(op: &solve::LinearOp, index: usize) -> bool {
    match op {
        solve::LinearOp::LoadP { index: slot, .. } => *slot == index,
        // A runtime-indexed load can land anywhere in `base..base+count`, so
        // treat the whole run as a read of the slot.
        solve::LinearOp::LoadIndexedP { base, count, .. } => {
            (*base..base.saturating_add(*count)).contains(&index)
        }
        solve::LinearOp::TensorLoad {
            input: solve::TensorInputKind::P,
            input_start,
            count,
            ..
        } => (*input_start..input_start.saturating_add(*count)).contains(&index),
        solve::LinearOp::FunctionFold { program, .. }
        | solve::LinearOp::GuardedFunctionFold { program, .. }
        | solve::LinearOp::StoreOutputFunctionFold { program, .. } => program
            .update
            .iter()
            .any(|nested| linear_op_reads_parameter(nested, index)),
        solve::LinearOp::FunctionConditional { program, .. } => {
            program.arms.iter().any(|arm| {
                arm.condition
                    .iter()
                    .chain(&arm.result)
                    .any(|nested| linear_op_reads_parameter(nested, index))
            }) || program
                .fallback
                .iter()
                .any(|nested| linear_op_reads_parameter(nested, index))
        }
        _ => false,
    }
}

/// Whether any lowered row in the problem reads parameter slot `index`.
///
/// The continuation only steers two systems, but λ may legally be read by any of
/// them — `derivative_rhs`, `discrete.rhs`, `events.root_conditions` and the
/// rest all evaluate it at λ = 1. This walk is what separates "λ is read
/// somewhere the continuation does not steer" (legal) from "λ is read nowhere"
/// (the dropped-`simplified` bug SPEC 0036 forbids).
fn model_reads_parameter(model: &solve::SolveModel, index: usize) -> bool {
    use solve::visitor::SolveVisitor as _;

    let mut scan = ParameterReadScan {
        index,
        found: false,
    };
    let _ = scan.visit_solve_problem(&model.problem);
    if scan.found {
        return true;
    }
    let _ = scan.visit_scalar_program_block(&model.visible_value_rows);
    scan.found
}

struct ParameterReadScan {
    index: usize,
    found: bool,
}

impl solve::visitor::SolveVisitor for ParameterReadScan {
    type Error = std::convert::Infallible;

    fn visit_linear_op(
        &mut self,
        _kind: solve::visitor::LinearOpSliceKind,
        _op_index: usize,
        op: &solve::LinearOp,
    ) -> Result<(), Self::Error> {
        self.found |= linear_op_reads_parameter(op, self.index);
        Ok(())
    }
}

/// Equation indices the algebraic refresh re-solves at every continuation value.
fn algebraic_refresh_equations(plan: &solve::RefreshPlan) -> BTreeSet<usize> {
    plan.rows
        .iter()
        .chain(plan.causal_rows().iter())
        .map(|row| row.equation_index())
        .chain(
            plan.simultaneous_plan
                .blocks
                .iter()
                .chain(plan.value_projection_plan.blocks.iter())
                .flat_map(|block| block.rows.iter().copied()),
        )
        .collect()
}

/// Whether initialization solves `equation` of the continuous implicit system.
///
/// `lower_algebraic_projection` assigns `implicit_row_targets` entries only for
/// `UnknownId::Algebraic` unknowns, whose Y indices are `>= state_scalar_count`
/// by construction; every other entry stays `None`. So the real invariant is
/// "the row has an assigned algebraic target", and the `index >= state_count`
/// guard restates the lowering's own range rather than excluding a shape that
/// production emits.
fn implicit_equation_is_initialization_solved(model: &solve::SolveModel, equation: usize) -> bool {
    let state_count = model.state_scalar_count();
    matches!(
        model
            .problem
            .continuous
            .implicit_row_targets
            .get(equation)
            .copied()
            .flatten(),
        Some(solve::ScalarSlot::Y { index, .. }) if index >= state_count
    )
}

#[cfg(test)]
mod tests {
    use rumoca_core::{BytePos, SourceId, Span};

    use super::*;

    fn span() -> Span {
        Span::new(
            SourceId::from_source_name("initial_continuation.mo"),
            BytePos(0),
            BytePos(1),
        )
    }

    /// A block whose program indices and equation indices coincide.
    fn scalar_block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
        let spans = vec![span(); programs.len()];
        solve::ScalarProgramBlock::with_program_spans(programs, spans)
            .expect("fixture scalar program block is well formed")
    }

    /// A block whose equation identity is a non-identity permutation of its
    /// program order, as every multi-equation model rumoca lowers emits.
    fn permuted_block(
        programs: Vec<Vec<solve::LinearOp>>,
        output_indices: Vec<usize>,
    ) -> solve::ScalarProgramBlock {
        let spans = vec![span(); programs.len()];
        solve::ScalarProgramBlock::with_output_indices(programs, spans, output_indices)
            .expect("fixture scalar program block is well formed")
    }

    fn reads_lambda_program(slot: usize) -> Vec<solve::LinearOp> {
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadP {
                dst: 1,
                index: slot,
            },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]
    }

    #[test]
    fn nested_conditional_parameter_read_is_detected() {
        let nested = solve::FunctionConditionalProgram::checked(
            0,
            [1],
            [(
                vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::LoadP { dst: 0, index: 1 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            )],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        )
        .expect("nested conditional fixture is checked");
        let op = solve::LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(nested),
        };

        assert!(linear_op_reads_parameter(&op, 1));
        assert!(!linear_op_reads_parameter(&op, 0));
    }

    fn refresh_row(equation_index: usize, row_idx: usize) -> solve::AlgebraicRefreshRow {
        solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
            owner_id: Default::default(),
            source: solve::RefreshScalarProgramSource::checked(0, row_idx).unwrap(),
            equation_index,
            output_offset: 0,
            target_index: 0,
            assignment_target: None,
            assignment_shape: None,
            direct_assignment_certified: false,
            exact_assignment_certified: false,
        })
        .unwrap()
    }

    fn plain_program() -> Vec<solve::LinearOp> {
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    }

    fn model_with_lambda(lambda: Option<usize>) -> solve::SolveModel {
        solve::SolveModel {
            problem: solve::SolveProblem {
                solve_layout: solve::SolveLayout {
                    state_scalar_count: 0,
                    compiled_parameter_len: 2,
                    initial_homotopy_parameter_index: lambda,
                    ..Default::default()
                },
                ..Default::default()
            },
            ..Default::default()
        }
    }

    fn covered_plan() -> solve::InitializationProjectionPlan {
        solve::InitializationProjectionPlan {
            iterates_discretes: false,
            blocks: vec![solve::InitializationProjectionBlock {
                rows: vec![0],
                unknowns: vec![solve::scalar_slot_y(0)],
                scales: vec![solve::InitializationUnknownScale::Solver],
            }],
        }
    }

    #[test]
    fn coverage_is_absent_without_a_continuation_parameter() {
        let model = model_with_lambda(None);
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![plain_program()]),
            &solve::RefreshPlan::default(),
        )
        .expect("a model without homotopy certifies");

        assert!(coverage.is_none());
    }

    #[test]
    fn covered_initialization_row_certifies() {
        let mut model = model_with_lambda(Some(1));
        model.problem.initialization =
            solve::InitializationSolveSystem::construct(solve::InitializationSystemInput {
                residual: solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                    reads_lambda_program(1),
                ])),
                row_roles: vec![solve::InitializationRowRole::Solved],
                projection_plan: covered_plan(),
                ..Default::default()
            })
            .unwrap();
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![reads_lambda_program(1)]),
            &solve::RefreshPlan::default(),
        )
        .expect("a plan-covered homotopy row certifies")
        .expect("a continuation parameter yields coverage");

        assert_eq!(coverage.sweep_parameter_index(), Some(1));
        assert!(
            !coverage.drives_algebraic_refresh(),
            "no implicit row reads lambda, so the plan carries the sweep alone"
        );
    }

    #[test]
    fn refresh_covered_implicit_row_drives_the_algebraic_refresh() {
        let mut model = model_with_lambda(Some(1));
        model.problem.continuous.implicit_row_targets = vec![Some(solve::scalar_slot_y(0))];
        let refresh = solve::RefreshPlan {
            rows: vec![refresh_row(0, 0)],
            ..Default::default()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![reads_lambda_program(1)]),
            &scalar_block(vec![plain_program()]),
            &refresh,
        )
        .expect("a refresh-covered homotopy row certifies")
        .expect("a continuation parameter yields coverage");

        assert!(
            coverage.drives_algebraic_refresh(),
            "the sweep must re-solve the algebraic refresh that owns the homotopy row"
        );
    }

    /// `BistableLoop`: two implicit programs whose equation identity is
    /// `[2, 1]`. The λ row is program 1 / equation 1.
    #[test]
    fn bistable_loop_permutation_resolves_the_lambda_row_to_its_equation() {
        let mut model = model_with_lambda(Some(2));
        model.problem.solve_layout.compiled_parameter_len = 3;
        model.problem.solve_layout.state_scalar_count = 1;
        model.problem.continuous.implicit_row_targets = vec![
            None,
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
        ];
        let refresh = solve::RefreshPlan {
            simultaneous_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![1, 2],
                    y_indices: vec![1, 2],
                    tearing: None,
                    alternate_charts: Vec::new(),
                }],
            },
            ..Default::default()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &permuted_block(vec![plain_program(), reads_lambda_program(2)], vec![2, 1]),
            &scalar_block(vec![]),
            &refresh,
        )
        .expect("the BistableLoop shape certifies")
        .expect("a continuation parameter yields coverage");

        assert!(
            coverage.drives_algebraic_refresh(),
            "equation 1 is the algebraic loop head the sweep has to re-solve"
        );
    }

    /// `E10_Mixed`: `output_indices = [4, 1, 2, 3]`, so the λ-reading program 0
    /// is equation 4. Reading `implicit_row_targets` at the *program* index
    /// yields `None` and silently drops the row from the coverage requirement;
    /// reading it at the equation index yields `Y4`, an algebraic the sweep must
    /// steer.
    #[test]
    fn mixed_vector_permutation_steers_the_algebraic_row_at_its_equation_index() {
        let mut model = model_with_lambda(Some(0));
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.state_scalar_count = 1;
        model.problem.continuous.implicit_row_targets = vec![
            None,
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
            Some(solve::scalar_slot_y(3)),
            Some(solve::scalar_slot_y(4)),
        ];
        assert!(
            model.problem.continuous.implicit_row_targets[0].is_none(),
            "the program-index reading of the lambda row must resolve to None, \
             so this fixture proves the translation and not an accident"
        );
        let refresh = solve::RefreshPlan {
            simultaneous_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![
                    solve::AlgebraicProjectionBlock {
                        rows: vec![4],
                        y_indices: vec![4],
                        tearing: None,
                        alternate_charts: Vec::new(),
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                        tearing: None,
                        alternate_charts: Vec::new(),
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![2],
                        y_indices: vec![2],
                        tearing: None,
                        alternate_charts: Vec::new(),
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![3],
                        y_indices: vec![3],
                        tearing: None,
                        alternate_charts: Vec::new(),
                    },
                ],
            },
            ..Default::default()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &permuted_block(
                vec![
                    reads_lambda_program(0),
                    plain_program(),
                    plain_program(),
                    plain_program(),
                ],
                vec![4, 1, 2, 3],
            ),
            &scalar_block(vec![]),
            &refresh,
        )
        .expect("the E10_Mixed shape certifies")
        .expect("a continuation parameter yields coverage");

        assert!(
            coverage.drives_algebraic_refresh(),
            "equation 4 targets Y4, an algebraic initialization solves, so the \
             sweep must re-run the algebraic refresh"
        );
    }

    /// `E11_Shifted`: `output_indices = [5, 4, 1, 2, 3]`; the λ-reading program
    /// 1 is equation 4, not equation 1.
    #[test]
    fn shifted_permutation_steers_the_algebraic_row_at_its_equation_index() {
        let mut model = model_with_lambda(Some(2));
        model.problem.solve_layout.compiled_parameter_len = 3;
        model.problem.solve_layout.state_scalar_count = 1;
        model.problem.continuous.implicit_row_targets = vec![
            None,
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
            Some(solve::scalar_slot_y(3)),
            Some(solve::scalar_slot_y(4)),
            Some(solve::scalar_slot_y(5)),
        ];
        let refresh = solve::RefreshPlan {
            simultaneous_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![4, 5],
                    y_indices: vec![4, 5],
                    tearing: None,
                    alternate_charts: Vec::new(),
                }],
            },
            ..Default::default()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &permuted_block(
                vec![
                    plain_program(),
                    reads_lambda_program(2),
                    plain_program(),
                    plain_program(),
                    plain_program(),
                ],
                vec![5, 4, 1, 2, 3],
            ),
            &scalar_block(vec![]),
            &refresh,
        )
        .expect("the E11_Shifted shape certifies")
        .expect("a continuation parameter yields coverage");

        assert!(
            coverage.drives_algebraic_refresh(),
            "equation 4 is the loop head; a program-index reading would have \
             checked equation 1 instead"
        );
    }

    #[test]
    fn dead_continuation_parameter_is_rejected() {
        let model = model_with_lambda(Some(1));
        let error = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![plain_program()]),
            &solve::RefreshPlan::default(),
        )
        .expect_err("an allocated slot that nothing reads must be rejected");

        assert!(
            error.to_string().contains("no lowered row reads it"),
            "unexpected message: {error}"
        );
    }

    #[test]
    fn out_of_range_continuation_parameter_is_rejected() {
        let model = model_with_lambda(Some(7));
        let error = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![plain_program()]),
            &solve::RefreshPlan::default(),
        )
        .expect_err("an out-of-range continuation slot must be rejected");

        assert!(
            error
                .to_string()
                .contains("outside the 2 compiled parameters"),
            "unexpected message: {error}"
        );
    }

    /// `E1_DerHomotopy` / `E8_Closure`: the only λ read is in
    /// `continuous.derivative_rhs`. Nothing solves that row for an unknown, so
    /// the continuation owes it nothing and the model is legal — it evaluates at
    /// λ = 1, MLS §3.7.4.3's trivial implementation.
    #[test]
    fn derivative_row_lambda_read_is_legal_and_unsteered() {
        let mut model = model_with_lambda(Some(0));
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.state_scalar_count = 1;
        model.problem.continuous.derivative_rhs =
            solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                reads_lambda_program(0),
            ]));
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![]),
            &solve::RefreshPlan::default(),
        )
        .expect("der(y) = homotopy(..) is legal MLS and must not be rejected")
        .expect("a continuation parameter yields coverage");

        assert!(
            !coverage.drives_algebraic_refresh(),
            "a derivative row is not part of any solve the continuation drives"
        );
    }

    /// `E2_WhenHomotopy`: the only λ read is in `discrete.rhs`.
    #[test]
    fn discrete_row_lambda_read_is_legal_and_unsteered() {
        let mut model = model_with_lambda(Some(0));
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.discrete.rhs = scalar_block(vec![reads_lambda_program(0)]);
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![]),
            &solve::RefreshPlan::default(),
        )
        .expect("when-clause homotopy is legal MLS and must not be rejected")
        .expect("a continuation parameter yields coverage");

        assert!(!coverage.drives_algebraic_refresh());
    }

    /// `E6_SteadyState`: `initial equation der(x) = 0` against
    /// `der(x) = homotopy(x^3 - x, x + 1)` lowers to one λ-reading
    /// `initialization.residual` row with **no** row target and an empty
    /// projection plan. Initialization solves nothing through that row, so the
    /// continuation owes it no coverage and the model must be accepted.
    #[test]
    fn steady_state_initialization_row_without_a_projection_owner_certifies() {
        let mut model = model_with_lambda(Some(0));
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.state_scalar_count = 1;
        model.problem.initialization =
            solve::InitializationSolveSystem::construct(solve::InitializationSystemInput {
                residual: solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                    reads_lambda_program(0),
                ])),
                row_roles: vec![solve::InitializationRowRole::SurplusCheck],
                ..Default::default()
            })
            .unwrap();
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![reads_lambda_program(0)]),
            &solve::RefreshPlan::default(),
        )
        .expect("a steady-state initialization row no plan solves must not be rejected")
        .expect("a continuation parameter yields coverage");

        assert!(!coverage.drives_algebraic_refresh());
    }

    /// The genuine implicit hole, stated on a permuted block so the message and
    /// the check both name the equation index.
    #[test]
    fn unsteered_implicit_algebraic_row_is_rejected() {
        let mut model = model_with_lambda(Some(1));
        model.problem.solve_layout.state_scalar_count = 1;
        model.problem.continuous.implicit_row_targets = vec![
            None,
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
        ];
        let error = InitialContinuationCoverage::certify(
            &model,
            &permuted_block(vec![plain_program(), reads_lambda_program(1)], vec![2, 1]),
            &scalar_block(vec![]),
            &solve::RefreshPlan::default(),
        )
        .expect_err("an algebraic homotopy row no refresh plan solves must be rejected");

        assert!(
            error
                .to_string()
                .contains("continuous.implicit_rhs equation 1"),
            "unexpected message: {error}"
        );
    }

    /// Exact refresh row `equation` assigning solver-Y `target`.
    fn exact_row(equation: usize, target: usize) -> solve::AlgebraicRefreshRow {
        solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
            owner_id: Default::default(),
            source: solve::RefreshScalarProgramSource::checked(0, equation).unwrap(),
            equation_index: equation,
            output_offset: 0,
            target_index: target,
            assignment_target: Some(target),
            assignment_shape: Some(solve::TargetAssignmentShape::Zero {
                target_y_index: target,
                expr_eval_len: 1,
            }),
            direct_assignment_certified: false,
            exact_assignment_certified: true,
        })
        .unwrap()
    }

    /// `y1 - y0`: equation 1 reads the value equation 0 assigns.
    fn reads_first_program() -> Vec<solve::LinearOp> {
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]
    }

    /// A Limiter-like chain: equation 0 assigns `y0 = lambda` exactly and
    /// equation 1 consumes `y0`. Only the stages decide whether equation 1
    /// selects a root.
    fn exact_chain_model() -> (solve::SolveModel, solve::ScalarProgramBlock) {
        let mut model = model_with_lambda(Some(1));
        model.problem.continuous.implicit_row_targets =
            vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
        let implicit = scalar_block(vec![reads_lambda_program(1), reads_first_program()]);
        (model, implicit)
    }

    fn exact_stage(rows: &[usize]) -> solve::RefreshStage {
        solve::RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: solve::RefreshRowSelection::checked(2, []).unwrap(),
            dynamic_rows: solve::RefreshRowSelection::checked(2, rows.iter().copied()).unwrap(),
        }
    }

    /// `Modelica.Blocks.Examples.TotalHarmonicDistortion`: the Limiter's
    /// homotopy row and the Division that reads it are exact assignments, so
    /// no iterative solve depends on lambda and the sweep must not run (at
    /// lambda = 0 the Division would evaluate 0/0).
    #[test]
    fn exact_assignment_chain_steers_nothing() {
        let (model, implicit) = exact_chain_model();
        let refresh = solve::RefreshPlan {
            rows: vec![exact_row(0, 0), exact_row(1, 1)],
            value_stages: vec![exact_stage(&[0, 1])],
            ..Default::default()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &implicit,
            &scalar_block(vec![]),
            &refresh,
        )
        .expect("an exactly assigned homotopy row certifies")
        .expect("a continuation parameter yields coverage");

        assert_eq!(coverage.sweep_parameter_index(), None);
        assert!(!coverage.drives_algebraic_refresh());
    }

    /// MLS 3.7 §3.7.4.2's `w = f1(x)` with homotopy feeding the nonlinear
    /// system `0 = f2(.., w)`: the exact row carries lambda into an iterative
    /// block, which the sweep must steer.
    #[test]
    fn exact_assignment_feeding_an_iterative_block_is_steered() {
        let (model, implicit) = exact_chain_model();
        let refresh = solve::RefreshPlan {
            rows: vec![exact_row(0, 0), exact_row(1, 1)],
            value_stages: vec![
                exact_stage(&[0]),
                solve::RefreshStage::ProjectionBlock {
                    seed_sequence: Default::default(),
                    block_index: 0,
                    plan: solve::AlgebraicProjectionPlan {
                        blocks: vec![solve::AlgebraicProjectionBlock {
                            rows: vec![1],
                            y_indices: vec![1],
                            tearing: None,
                            alternate_charts: Vec::new(),
                        }],
                    },
                    seed_rows: solve::RefreshRowSelection::checked(2, []).unwrap(),
                },
            ],
            ..Default::default()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &implicit,
            &scalar_block(vec![]),
            &refresh,
        )
        .expect("a homotopy chain into an iterative block certifies")
        .expect("a continuation parameter yields coverage");

        assert_eq!(coverage.sweep_parameter_index(), Some(1));
        assert!(coverage.drives_algebraic_refresh());
    }

    /// An initialization row the projection plan solves and that reads the
    /// exactly assigned homotopy value is steered, and the sweep re-runs the
    /// refresh that produces the value.
    #[test]
    fn initialization_row_reading_an_exact_homotopy_value_is_steered() {
        let (mut model, implicit) = exact_chain_model();
        let initial = scalar_block(vec![plain_program()]);
        model.problem.initialization =
            solve::InitializationSolveSystem::construct(solve::InitializationSystemInput {
                residual: solve::ComputeBlock::from_scalar_program_block(initial.clone()),
                row_roles: vec![solve::InitializationRowRole::Solved],
                projection_plan: covered_plan(),
                ..Default::default()
            })
            .unwrap();
        let refresh = solve::RefreshPlan {
            rows: vec![exact_row(0, 0), exact_row(1, 1)],
            value_stages: vec![exact_stage(&[0, 1])],
            ..Default::default()
        };
        let coverage = InitialContinuationCoverage::certify(&model, &implicit, &initial, &refresh)
            .expect("an initialization row over a homotopy value certifies")
            .expect("a continuation parameter yields coverage");

        assert_eq!(coverage.sweep_parameter_index(), Some(1));
        assert!(coverage.drives_algebraic_refresh());
    }
}
