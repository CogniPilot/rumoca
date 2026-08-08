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
//! * `continuous.implicit_row_targets` and `initialization.row_targets` are
//!   indexed by equation index.
//! * `InitializationProjectionBlock::rows` and `AlgebraicProjectionBlock::rows`
//!   are equation indices into the corresponding residual vector.
//! * [`AlgebraicRefreshRow::equation_index`] is an equation index;
//!   `AlgebraicRefreshRow::row_idx` is a *program* index (documented at
//!   `rumoca-eval-solve/src/refresh_plan.rs`). Only `equation_index` is read
//!   here.
//!
//! [`AlgebraicRefreshRow::equation_index`]: rumoca_eval_solve::refresh_plan::AlgebraicRefreshRow::equation_index
//!
//! # Acceptance contract (SPEC 0008 / SPEC 0036)
//!
//! Accepted:
//!
//! * No λ slot is allocated at all (the DAE carried no `homotopy`), in which
//!   case there is no continuation and no coverage object.
//! * λ is allocated and read by at least one lowered row of the model. Rows
//!   split into two groups:
//!   * **Steered** — `continuous.implicit_rhs` rows that target an algebraic
//!     slot, and `initialization.residual` rows the initialization projection
//!     plan solves. The continuation drives these; this module proves a plan
//!     names each of them.
//!   * **Unsteered** — every other row family the lowering can put a λ read in:
//!     `continuous.derivative_rhs` (`der(y) = homotopy(...)`),
//!     `discrete.rhs` (`when c then z = homotopy(...); end when;`),
//!     `events.root_conditions`, `continuous.residual`,
//!     `continuous.manifold_residual`, `initialization.update_rhs`,
//!     `visible_value_rows`, and `initialization.residual` rows that no
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
//! * A λ-reading `initialization.residual` equation whose target unknown *is*
//!   claimed by an `initialization.projection_plan` block while that block omits
//!   the equation itself — the plan claims to solve the unknown from rows that
//!   exclude the one carrying λ.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_eval_solve::{
    EvalSolveError, PreparedScalarProgramBlock, refresh_plan::RefreshPlan, to_scalar_program_block,
};
use rumoca_ir_solve as solve;

/// The exact solves the initialization homotopy continuation drives.
///
/// Present only for models that allocate a continuation parameter. Row indices
/// are equation indices — see the module documentation.
#[derive(Clone, Debug)]
pub(crate) struct InitialContinuationCoverage {
    /// Hidden `P` slot the driver advances from `0` to `1`.
    lambda_index: usize,
    /// λ-reading `continuous.implicit_rhs` equations, each proven to be named by
    /// the algebraic refresh plan. Non-empty means the sweep has to re-run that
    /// refresh at every continuation value.
    steered_implicit_equations: BTreeSet<usize>,
    /// λ-reading `initialization.residual` equations the initialization
    /// projection plan solves. Non-empty means the plan itself carries part of
    /// the sweep.
    steered_initialization_equations: BTreeSet<usize>,
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
        algebraic_refresh: &RefreshPlan,
    ) -> Result<(solve::ScalarProgramBlock, Option<Self>), EvalSolveError> {
        let initial_scalar_residual =
            to_scalar_program_block(&model.problem.initialization.residual)?;
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
        algebraic_refresh: &RefreshPlan,
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

        let steered_initialization_equations =
            certify_initialization_rows(model, initial_block, &initial_reads)?;
        let refresh_equations = algebraic_refresh_equations(algebraic_refresh);
        let steered_implicit_equations =
            certify_implicit_rows(model, implicit_block, &implicit_reads, &refresh_equations)?;

        Ok(Some(Self {
            lambda_index,
            steered_implicit_equations,
            steered_initialization_equations,
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
        !self.steered_implicit_equations.is_empty()
    }
}

/// Reject λ-reading `initialization.residual` equations that a projection block
/// claims through their unknown but omits from its own rows, and return the
/// equations the projection plan steers.
fn certify_initialization_rows(
    model: &solve::SolveModel,
    initial_block: &solve::ScalarProgramBlock,
    initial_reads: &BTreeMap<usize, usize>,
) -> Result<BTreeSet<usize>, EvalSolveError> {
    let plan = &model.problem.initialization.projection_plan;
    let mut steered = BTreeSet::new();
    for (&equation, &program_index) in initial_reads {
        let Some(target) = model
            .problem
            .initialization
            .row_targets
            .get(equation)
            .copied()
            .flatten()
            .as_ref()
            .and_then(slot_key)
        else {
            // No unknown is assigned to this row, so initialization solves
            // nothing through it and the continuation owes it nothing. The
            // steady-state `initial equation der(x) = 0` shape lands here.
            continue;
        };
        let Some(owner) = plan.blocks.iter().find(|block| {
            block
                .unknowns
                .iter()
                .filter_map(slot_key)
                .any(|u| u == target)
        }) else {
            // The projection plan does not solve this row's unknown at all; the
            // row rides at λ = 1 like any other unsteered row.
            continue;
        };
        if !owner.rows.contains(&equation) {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "initialization.residual equation {equation} reads the homotopy continuation \
                     parameter and its target unknown is solved by an initialization.projection_plan \
                     block whose rows {:?} exclude it; the continuation would sweep lambda without \
                     ever steering that row",
                    owner.rows
                ),
                span: initial_block.program_span(program_index),
            });
        }
        steered.insert(equation);
    }
    Ok(steered)
}

/// Reject λ-reading `continuous.implicit_rhs` equations that target an algebraic
/// slot no refresh plan names, and return the equations the sweep steers.
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
    let mut reads = BTreeMap::new();
    let mut ordinal = 0usize;
    for (program_index, program) in block.programs().iter().enumerate() {
        let program_reads = program
            .iter()
            .any(|op| linear_op_reads_parameter(op, index));
        for _ in 0..solve::ScalarProgramBlock::program_output_count(program) {
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
            if program_reads {
                reads.insert(equation, program_index);
            }
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
    Ok(reads)
}

fn linear_op_reads_parameter(op: &solve::LinearOp, index: usize) -> bool {
    match op {
        solve::LinearOp::LoadP { index: slot, .. } => *slot == index,
        // A runtime-indexed load can land anywhere in `base..base+count`, so
        // treat the whole run as a read of the slot.
        solve::LinearOp::LoadIndexedP { base, count, .. } => {
            (*base..base.saturating_add(*count)).contains(&index)
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
fn algebraic_refresh_equations(plan: &RefreshPlan) -> BTreeSet<usize> {
    plan.rows
        .iter()
        .chain(plan.causal_seed_rows.iter())
        .map(|row| row.equation_index)
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

/// Storage-space identity of a projection unknown, ignoring byte offsets.
fn slot_key(slot: &solve::ScalarSlot) -> Option<(u8, usize)> {
    match slot {
        solve::ScalarSlot::Y { index, .. } => Some((0, *index)),
        solve::ScalarSlot::P { index, .. } => Some((1, *index)),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    }
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

    fn refresh_row(
        equation_index: usize,
        row_idx: usize,
    ) -> rumoca_eval_solve::refresh_plan::AlgebraicRefreshRow {
        rumoca_eval_solve::refresh_plan::AlgebraicRefreshRow {
            equation_index,
            row_idx,
            output_offset: 0,
            target_index: 0,
            assignment_target: None,
            assignment_shape: None,
            direct_assignment_certified: false,
            exact_assignment_certified: false,
        }
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
            blocks: vec![solve::InitializationProjectionBlock {
                rows: vec![0],
                unknowns: vec![solve::scalar_slot_y(0)],
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
            &RefreshPlan::default(),
        )
        .expect("a model without homotopy certifies");

        assert!(coverage.is_none());
    }

    #[test]
    fn covered_initialization_row_certifies() {
        let mut model = model_with_lambda(Some(1));
        model.problem.initialization.row_targets = vec![Some(solve::scalar_slot_y(0))];
        model.problem.initialization.projection_plan = covered_plan();
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![reads_lambda_program(1)]),
            &RefreshPlan::default(),
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
        let refresh = RefreshPlan {
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
        let refresh = RefreshPlan {
            simultaneous_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![1, 2],
                    y_indices: vec![1, 2],
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
        let refresh = RefreshPlan {
            simultaneous_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![
                    solve::AlgebraicProjectionBlock {
                        rows: vec![4],
                        y_indices: vec![4],
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![2],
                        y_indices: vec![2],
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![3],
                        y_indices: vec![3],
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
        let refresh = RefreshPlan {
            simultaneous_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![4, 5],
                    y_indices: vec![4, 5],
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
            &RefreshPlan::default(),
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
            &RefreshPlan::default(),
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
            &RefreshPlan::default(),
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
            &RefreshPlan::default(),
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
        model.problem.initialization.row_targets = vec![None];
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![reads_lambda_program(0)]),
            &RefreshPlan::default(),
        )
        .expect("a steady-state initialization row no plan solves must not be rejected")
        .expect("a continuation parameter yields coverage");

        assert!(!coverage.drives_algebraic_refresh());
    }

    /// The genuine initialization hole: the plan claims the row's unknown but
    /// solves it from other rows, so the sweep never steers the λ row.
    #[test]
    fn unsteered_initialization_row_is_rejected() {
        let mut model = model_with_lambda(Some(1));
        model.problem.initialization.row_targets =
            vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
        model.problem.initialization.projection_plan = solve::InitializationProjectionPlan {
            blocks: vec![solve::InitializationProjectionBlock {
                rows: vec![1],
                unknowns: vec![solve::scalar_slot_y(0)],
            }],
        };
        let error = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![reads_lambda_program(1), plain_program()]),
            &RefreshPlan::default(),
        )
        .expect_err("a homotopy row its own solve block omits must be rejected");

        assert!(
            error
                .to_string()
                .contains("initialization.residual equation 0"),
            "unexpected message: {error}"
        );
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
            &RefreshPlan::default(),
        )
        .expect_err("an algebraic homotopy row no refresh plan solves must be rejected");

        assert!(
            error
                .to_string()
                .contains("continuous.implicit_rhs equation 1"),
            "unexpected message: {error}"
        );
    }
}
