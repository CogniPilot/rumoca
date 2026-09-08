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
        algebraic_refresh: &solve::IssuedRefreshPlan,
    ) -> Result<(solve::ScalarProgramBlock, Option<Self>), EvalSolveError> {
        let initial_scalar_residual =
            to_scalar_program_block(model.problem().initialization().residual())?;
        let coverage = Self::certify_with_refresh_equations(
            model,
            implicit_scalar_rhs.block(),
            &initial_scalar_residual,
            algebraic_refresh_equations_issued(algebraic_refresh),
        )?;
        Ok((initial_scalar_residual, coverage))
    }

    /// Test seam for injecting already scalarized blocks while retaining the
    /// production-issued refresh owner.
    #[cfg(test)]
    pub(crate) fn certify(
        model: &solve::SolveModel,
        implicit_block: &solve::ScalarProgramBlock,
        initial_block: &solve::ScalarProgramBlock,
        algebraic_refresh: &solve::IssuedRefreshPlan,
    ) -> Result<Option<Self>, EvalSolveError> {
        Self::certify_with_refresh_equations(
            model,
            implicit_block,
            initial_block,
            algebraic_refresh_equations_issued(algebraic_refresh),
        )
    }

    fn certify_with_refresh_equations(
        model: &solve::SolveModel,
        implicit_block: &solve::ScalarProgramBlock,
        initial_block: &solve::ScalarProgramBlock,
        refresh_equations: BTreeSet<usize>,
    ) -> Result<Option<Self>, EvalSolveError> {
        let Some(lambda_index) = model
            .problem()
            .solve_layout()
            .initial_homotopy_parameter_index
        else {
            return Ok(None);
        };
        let parameter_len = model.problem().solve_layout().compiled_parameter_len;
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
    let plan = model.problem().initialization().projection_plan();
    let mut steered = BTreeSet::new();
    for (&equation, &program_index) in initial_reads {
        let Some(target) = model
            .problem()
            .initialization()
            .row_targets()
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
        let output_count = block
            .stored_output_count_for_program(program_index)
            .ok_or_else(|| EvalSolveError::ShapeContract {
                message: "missing retained scalar program output width".to_string(),
                span: block.program_span(program_index),
            })?;
        for _ in 0..output_count {
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
        solve::LinearOp::TensorLoad {
            input: solve::TensorInputKind::P,
            input_start,
            count,
            ..
        } => (*input_start..input_start.saturating_add(*count)).contains(&index),
        solve::LinearOp::FunctionFold { program, .. }
        | solve::LinearOp::GuardedFunctionFold { program, .. }
        | solve::LinearOp::StoreOutputFunctionFold { program, .. } => program
            .update()
            .iter()
            .any(|nested| linear_op_reads_parameter(nested, index)),
        solve::LinearOp::FunctionConditional { program, .. } => {
            program.arms().iter().any(|arm| {
                arm.condition()
                    .iter()
                    .chain(arm.result())
                    .any(|nested| linear_op_reads_parameter(nested, index))
            }) || program
                .fallback()
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
    let _ = scan.visit_solve_problem(model.problem());
    if scan.found {
        return true;
    }
    let _ = scan.visit_scalar_program_block(
        solve::visitor::ScalarProgramBlockOwner::VisibleValueRows,
        model.visible_value_rows(),
    );
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
        _owner: solve::visitor::LinearOpSliceOwner,
        _op_index: usize,
        op: &solve::LinearOp,
    ) -> Result<(), Self::Error> {
        self.found |= linear_op_reads_parameter(op, self.index);
        Ok(())
    }
}

fn algebraic_refresh_equations_issued(plan: &solve::IssuedRefreshPlan) -> BTreeSet<usize> {
    plan.rows()
        .iter()
        .map(solve::AlgebraicRefreshRow::equation_index)
        .chain(
            plan.simultaneous_plan()
                .blocks
                .iter()
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
            .problem()
            .continuous()
            .implicit_row_targets()
            .get(equation)
            .copied()
            .flatten(),
        Some(solve::ScalarSlot::Y { index }) if index >= state_count
    )
}

/// Storage-space identity of a projection unknown, ignoring byte offsets.
fn slot_key(slot: &solve::ScalarSlot) -> Option<(u8, usize)> {
    match slot {
        solve::ScalarSlot::Y { index } => Some((0, *index)),
        solve::ScalarSlot::P { index } => Some((1, *index)),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::{BytePos, SourceId, Span};

    use super::*;

    use crate::test_support::empty_binary64_first_product_model;

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

    fn plain_program() -> Vec<solve::LinearOp> {
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    }

    fn covered_plan() -> solve::InitializationProjectionPlan {
        solve::InitializationProjectionPlan {
            blocks: vec![solve::InitializationProjectionBlock {
                rows: vec![0],
                unknowns: vec![solve::scalar_slot_y(0)],
            }],
        }
    }

    struct PermutationFixtureSpec {
        algebraic_count: usize,
        parameter_count: usize,
        lambda_parameter_index: usize,
        projection_row_groups: Vec<Vec<usize>>,
        refresh_row_groups: Vec<Vec<usize>>,
    }

    struct PermutationCoverageFixture {
        model: solve::SolveModel,
    }

    fn algebraic_projection_blocks(
        row_groups: Vec<Vec<usize>>,
    ) -> Vec<solve::AlgebraicProjectionBlock> {
        row_groups
            .into_iter()
            .map(|rows| solve::AlgebraicProjectionBlock {
                y_indices: rows.clone(),
                rows,
                tearing: None,
            })
            .collect()
    }

    fn permutation_coverage_fixture(spec: PermutationFixtureSpec) -> PermutationCoverageFixture {
        let PermutationFixtureSpec {
            algebraic_count,
            parameter_count,
            lambda_parameter_index,
            projection_row_groups,
            refresh_row_groups,
        } = spec;
        let refresh_block_count = refresh_row_groups.len();
        let names = std::iter::once("x".to_string())
            .chain((0..algebraic_count).map(|index| format!("a{index}")))
            .collect();
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names,
                ..Default::default()
            },
            state_scalar_count: 1,
            algebraic_scalar_count: algebraic_count,
            compiled_parameter_len: parameter_count,
            initial_homotopy_parameter_index: Some(lambda_parameter_index),
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let identity_programs = std::iter::repeat_with(plain_program)
            .take(algebraic_count)
            .collect();
        let refresh_plan = solve::RefreshPlan {
            simultaneous_plan: solve::AlgebraicProjectionPlan {
                blocks: algebraic_projection_blocks(refresh_row_groups),
            },
            simultaneous_block_indices: (0..refresh_block_count).collect(),
            ..solve::RefreshPlan::empty()
        };
        let continuous = crate::test_support::ContinuousSystemFixture {
            implicit_rhs: solve::ComputeBlock::from_scalar_program_block(permuted_block(
                identity_programs,
                (1..=algebraic_count).collect(),
            )),
            implicit_row_targets: std::iter::once(None)
                .chain((1..=algebraic_count).map(|index| Some(solve::scalar_slot_y(index))))
                .collect(),
            algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                blocks: algebraic_projection_blocks(projection_row_groups),
            },
            derivative_rhs: crate::test_support::zero_derivative_rhs(1, span()),
            refresh_plans: Some(solve::ContinuousRefreshPlanInputs::new(
                refresh_plan,
                solve::RefreshPlan::empty(),
                solve::RefreshPlan::empty(),
                solve::RefreshPlan::empty(),
                Vec::new(),
            )),
            ..crate::test_support::ContinuousSystemFixture::empty()
        };
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(
                    Default::default(),
                    algebraic_count + 1,
                    parameter_count,
                ),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::empty(),
                discrete,
                events,
                clocks,
            )
            .expect("permutation fixture satisfies the checked root contract"),
            initial_y: vec![0.0; algebraic_count + 1],
            solver_nominals: vec![1.0; algebraic_count + 1],
            parameters: vec![0.0; parameter_count],
            ..empty_binary64_first_product_model()
        };
        PermutationCoverageFixture { model }
    }

    #[test]
    fn coverage_is_absent_without_a_continuation_parameter() {
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 0, 2),
                solve::SolveLayout {
                    compiled_parameter_len: 2,
                    initial_homotopy_parameter_index: None,
                    ..Default::default()
                },
                crate::test_support::ContinuousSystemFixture::empty(),
                solve::InitializationSolveSystem::empty(),
                solve::DiscreteSolveSystem::default(),
                solve::SolveEventPartition::default(),
                solve::SolveClockPartition::default(),
            )
            .expect("no-continuation fixture satisfies the checked root contract"),
            parameters: vec![0.0; 2],
            ..empty_binary64_first_product_model()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![plain_program()]),
            model.problem().continuous().refresh_owners().algebraic(),
        )
        .expect("a model without homotopy certifies");

        assert!(coverage.is_none());
    }

    #[test]
    fn covered_initialization_row_certifies() {
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["x".to_string()],
                ..Default::default()
            },
            state_scalar_count: 1,
            compiled_parameter_len: 2,
            initial_homotopy_parameter_index: Some(1),
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            derivative_rhs: crate::test_support::zero_derivative_rhs(1, span()),
            ..crate::test_support::ContinuousSystemFixture::empty()
        }
        .seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 1, 2),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::construct(
                    solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                        plain_program(),
                    ])),
                    vec![Some(solve::scalar_slot_y(0))],
                    vec![solve::InitializationRowRole::Solved],
                    1,
                    vec![solve::scalar_slot_y(0)],
                    covered_plan(),
                    (solve::ScalarProgramBlock::default(), Vec::new()),
                )
                .expect("the covered fixture initialization system is exactly correlated"),
                discrete,
                events,
                clocks,
            )
            .expect("covered-initialization fixture satisfies the checked root contract"),
            initial_y: vec![0.0],
            solver_nominals: vec![1.0],
            parameters: vec![0.0; 2],
            ..empty_binary64_first_product_model()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![reads_lambda_program(1)]),
            model.problem().continuous().refresh_owners().algebraic(),
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
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["a".to_string()],
                ..Default::default()
            },
            algebraic_scalar_count: 1,
            compiled_parameter_len: 2,
            initial_homotopy_parameter_index: Some(1),
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            implicit_rhs: solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                plain_program(),
            ])),
            implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
            algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    tearing: None,
                }],
            },
            ..crate::test_support::ContinuousSystemFixture::empty()
        };
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 1, 2),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::empty(),
                discrete,
                events,
                clocks,
            )
            .expect("refresh-covered fixture satisfies the checked root contract"),
            initial_y: vec![0.0],
            solver_nominals: vec![1.0],
            parameters: vec![0.0; 2],
            ..empty_binary64_first_product_model()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![reads_lambda_program(1)]),
            &scalar_block(vec![plain_program()]),
            model.problem().continuous().refresh_owners().algebraic(),
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
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["x".to_string(), "a0".to_string(), "a1".to_string()],
                ..Default::default()
            },
            state_scalar_count: 1,
            algebraic_scalar_count: 2,
            compiled_parameter_len: 3,
            initial_homotopy_parameter_index: Some(2),
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            implicit_rhs: solve::ComputeBlock::from_scalar_program_block(permuted_block(
                vec![plain_program(), plain_program()],
                vec![1, 2],
            )),
            implicit_row_targets: vec![
                None,
                Some(solve::scalar_slot_y(1)),
                Some(solve::scalar_slot_y(2)),
            ],
            algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![1, 2],
                    y_indices: vec![1, 2],
                    tearing: None,
                }],
            },
            derivative_rhs: crate::test_support::zero_derivative_rhs(1, span()),
            refresh_plans: Some(solve::ContinuousRefreshPlanInputs::new(
                solve::RefreshPlan {
                    simultaneous_plan: solve::AlgebraicProjectionPlan {
                        blocks: vec![solve::AlgebraicProjectionBlock {
                            rows: vec![1, 2],
                            y_indices: vec![1, 2],
                            tearing: None,
                        }],
                    },
                    simultaneous_block_indices: vec![0],
                    ..solve::RefreshPlan::empty()
                },
                solve::RefreshPlan::empty(),
                solve::RefreshPlan::empty(),
                solve::RefreshPlan::empty(),
                Vec::new(),
            )),
            ..crate::test_support::ContinuousSystemFixture::empty()
        };
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 3, 3),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::empty(),
                discrete,
                events,
                clocks,
            )
            .expect("BistableLoop fixture satisfies the checked root contract"),
            initial_y: vec![0.0; 3],
            solver_nominals: vec![1.0; 3],
            parameters: vec![0.0; 3],
            ..empty_binary64_first_product_model()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &permuted_block(vec![plain_program(), reads_lambda_program(2)], vec![2, 1]),
            &scalar_block(vec![]),
            model.problem().continuous().refresh_owners().algebraic(),
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
        let fixture = permutation_coverage_fixture(PermutationFixtureSpec {
            algebraic_count: 4,
            parameter_count: 1,
            lambda_parameter_index: 0,
            projection_row_groups: vec![vec![4], vec![1], vec![2], vec![3]],
            refresh_row_groups: vec![vec![4], vec![1], vec![2], vec![3]],
        });
        assert!(
            fixture.model.problem().continuous().implicit_row_targets()[0].is_none(),
            "the program-index reading of the lambda row must resolve to None, \
             so this fixture proves the translation and not an accident"
        );
        let coverage = InitialContinuationCoverage::certify(
            &fixture.model,
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
            fixture
                .model
                .problem()
                .continuous()
                .refresh_owners()
                .algebraic(),
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
        let fixture = permutation_coverage_fixture(PermutationFixtureSpec {
            algebraic_count: 5,
            parameter_count: 3,
            lambda_parameter_index: 2,
            projection_row_groups: vec![vec![4, 5], vec![1], vec![2], vec![3]],
            refresh_row_groups: vec![vec![4, 5]],
        });
        let coverage = InitialContinuationCoverage::certify(
            &fixture.model,
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
            fixture
                .model
                .problem()
                .continuous()
                .refresh_owners()
                .algebraic(),
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
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 0, 2),
                solve::SolveLayout {
                    compiled_parameter_len: 2,
                    initial_homotopy_parameter_index: Some(1),
                    ..Default::default()
                },
                crate::test_support::ContinuousSystemFixture::empty(),
                solve::InitializationSolveSystem::empty(),
                solve::DiscreteSolveSystem::default(),
                solve::SolveEventPartition::default(),
                solve::SolveClockPartition::default(),
            )
            .expect("dead-continuation fixture satisfies the checked root contract"),
            parameters: vec![0.0; 2],
            ..empty_binary64_first_product_model()
        };
        let error = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![plain_program()]),
            model.problem().continuous().refresh_owners().algebraic(),
        )
        .expect_err("an allocated slot that nothing reads must be rejected");

        assert!(
            error.to_string().contains("no lowered row reads it"),
            "unexpected message: {error}"
        );
    }

    #[test]
    fn out_of_range_continuation_parameter_is_rejected() {
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 0, 2),
                solve::SolveLayout {
                    compiled_parameter_len: 2,
                    initial_homotopy_parameter_index: Some(7),
                    ..Default::default()
                },
                crate::test_support::ContinuousSystemFixture::empty(),
                solve::InitializationSolveSystem::empty(),
                solve::DiscreteSolveSystem::default(),
                solve::SolveEventPartition::default(),
                solve::SolveClockPartition::default(),
            )
            .expect("out-of-range continuation metadata remains a runtime contract fixture"),
            parameters: vec![0.0; 2],
            ..empty_binary64_first_product_model()
        };
        let error = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![plain_program()]),
            &scalar_block(vec![plain_program()]),
            model.problem().continuous().refresh_owners().algebraic(),
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
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["x".to_string()],
                ..Default::default()
            },
            state_scalar_count: 1,
            compiled_parameter_len: 1,
            initial_homotopy_parameter_index: Some(0),
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            derivative_rhs: solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                reads_lambda_program(0),
            ])),
            ..crate::test_support::ContinuousSystemFixture::empty()
        };
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 1, 1),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::empty(),
                discrete,
                events,
                clocks,
            )
            .expect("derivative-homotopy fixture satisfies the checked root contract"),
            initial_y: vec![0.0],
            solver_nominals: vec![1.0],
            parameters: vec![0.0],
            ..empty_binary64_first_product_model()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![]),
            model.problem().continuous().refresh_owners().algebraic(),
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
        let provenance = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("discrete_homotopy_variables.mo"),
            1,
            2,
        );
        let solve_layout = solve::SolveLayout {
            variable_storage_runs: vec![solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::P(1),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteReal,
                value_kind: solve::SolveVariableValueKind::Real,
            }],
            variable_declarations: vec![solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteReal,
                solve::SolveVariableValueKind::Real,
            )],
            compiled_parameter_len: 3,
            discrete_real_scalar_names: vec!["mode".to_string()],
            initial_homotopy_parameter_index: Some(0),
            pre_param_bindings: vec![solve::PreParamBinding {
                dest_p_index: 2,
                source: solve::PreParamSource::P { index: 1 },
                clock_schedule: None,
            }],
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem {
            event_iteration_plan: solve::EventIterationPlan {
                runs: vec![solve::EventIterationRun {
                    variable: 0,
                    pre_binding_start: 0,
                    owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
                }],
            },
            rhs: scalar_block(vec![vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]]),
            update_targets: vec![solve::scalar_slot_p(1)],
            row_roles: vec![solve::DiscreteRowRole::Equation],
            pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
            observation_refresh: vec![false],
            integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
            clock_owners: vec![None],
            ..Default::default()
        };
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture::empty();
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 0, 3),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::empty(),
                discrete,
                events,
                clocks,
            )
            .expect("discrete-homotopy fixture satisfies the checked root contract"),
            parameters: vec![0.0; 3],
            visible_value_rows: solve::ScalarProgramBlock::with_source_span(
                vec![vec![
                    solve::LinearOp::LoadP { dst: 0, index: 1 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]],
                provenance
                    .require_provenance("discrete homotopy visibility fixture")
                    .expect("fixture provenance is source-backed"),
            )
            .expect("discrete homotopy visibility is computable"),
            variable_entries: crate::test_support::explicit_real_scalar_catalog_entries(vec![
                crate::test_support::RealScalarVariableFixture::discrete_real(
                    1, "mode", 1, 0.0, provenance,
                ),
            ]),
            ..empty_binary64_first_product_model()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![]),
            model.problem().continuous().refresh_owners().algebraic(),
        )
        .expect("when-clause homotopy is legal MLS and must not be rejected")
        .expect("a continuation parameter yields coverage");

        assert!(!coverage.drives_algebraic_refresh());
    }

    /// `E6_SteadyState`: `initial equation der(x) = 0` against
    /// A λ-reading `initialization.residual` row with **no** row target and an
    /// empty projection plan: a stated-value check the projection owes nothing.
    /// Initialization solves nothing through that row, so the continuation owes
    /// it no coverage and the model must be accepted.
    #[test]
    fn steady_state_initialization_row_without_a_projection_owner_certifies() {
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["x".to_string()],
                ..Default::default()
            },
            state_scalar_count: 1,
            compiled_parameter_len: 1,
            initial_homotopy_parameter_index: Some(0),
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            derivative_rhs: solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                reads_lambda_program(0),
            ])),
            ..crate::test_support::ContinuousSystemFixture::empty()
        }
        .seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 1, 1),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::construct(
                    solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                        plain_program(),
                    ])),
                    vec![None],
                    vec![solve::InitializationRowRole::StatedValueCheck],
                    0,
                    Vec::new(),
                    solve::InitializationProjectionPlan::default(),
                    (solve::ScalarProgramBlock::default(), Vec::new()),
                )
                .expect("the stated-value-check fixture initialization system is exactly correlated"),
                discrete,
                events,
                clocks,
            )
            .expect("steady-state fixture satisfies the checked root contract"),
            initial_y: vec![0.0],
            solver_nominals: vec![1.0],
            parameters: vec![0.0],
            ..empty_binary64_first_product_model()
        };
        let coverage = InitialContinuationCoverage::certify(
            &model,
            &scalar_block(vec![]),
            &scalar_block(vec![reads_lambda_program(0)]),
            model.problem().continuous().refresh_owners().algebraic(),
        )
        .expect("a steady-state initialization row no plan solves must not be rejected")
        .expect("a continuation parameter yields coverage");

        assert!(!coverage.drives_algebraic_refresh());
    }

    /// The genuine initialization hole, now closed at mint: a plan that claims
    /// a row's unknown while solving it from another row cannot reach any
    /// runtime, because the aggregate issuer refuses the miscorrelation before
    /// a `SolveProblem` exists. The sweep-coverage certificate no longer needs
    /// to catch this shape; the issuer's refusal is the stronger guard.
    #[test]
    fn unsteered_initialization_row_is_unconstructible() {
        let error = solve::InitializationSolveSystem::construct(
            solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                plain_program(),
                plain_program(),
            ])),
            vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
            vec![solve::InitializationRowRole::Solved; 2],
            2,
            vec![solve::scalar_slot_y(0)],
            solve::InitializationProjectionPlan {
                blocks: vec![solve::InitializationProjectionBlock {
                    rows: vec![1],
                    unknowns: vec![solve::scalar_slot_y(0)],
                }],
            },
            (solve::ScalarProgramBlock::default(), Vec::new()),
        )
        .expect_err("a row recorded solved with no owning block must not mint");

        assert!(
            error.to_string().contains("row 0"),
            "the refusal names the row no block claims, got: {error}"
        );
    }

    /// The genuine implicit hole, stated on a permuted block so the message and
    /// the check both name the equation index.
    #[test]
    fn unsteered_implicit_algebraic_row_is_rejected() {
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["x".to_string(), "a0".to_string(), "a1".to_string()],
                ..Default::default()
            },
            state_scalar_count: 1,
            algebraic_scalar_count: 2,
            compiled_parameter_len: 2,
            initial_homotopy_parameter_index: Some(1),
            ..Default::default()
        };
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            implicit_rhs: solve::ComputeBlock::from_scalar_program_block(scalar_block(vec![
                plain_program(),
                plain_program(),
            ])),
            implicit_row_targets: vec![
                Some(solve::scalar_slot_y(1)),
                Some(solve::scalar_slot_y(2)),
            ],
            algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                blocks: vec![
                    solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![1],
                        tearing: None,
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![2],
                        tearing: None,
                    },
                ],
            },
            derivative_rhs: crate::test_support::zero_derivative_rhs(1, span()),
            ..crate::test_support::ContinuousSystemFixture::empty()
        };
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(Default::default(), 3, 2),
                solve_layout,
                continuous,
                solve::InitializationSolveSystem::empty(),
                discrete,
                events,
                clocks,
            )
            .expect("unsteered-implicit fixture satisfies the checked root contract"),
            initial_y: vec![0.0; 3],
            solver_nominals: vec![1.0; 3],
            parameters: vec![0.0; 2],
            ..empty_binary64_first_product_model()
        };
        let error = InitialContinuationCoverage::certify(
            &model,
            &permuted_block(vec![plain_program(), reads_lambda_program(1)], vec![2, 1]),
            &scalar_block(vec![]),
            model.problem().continuous().refresh_owners().root(),
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
