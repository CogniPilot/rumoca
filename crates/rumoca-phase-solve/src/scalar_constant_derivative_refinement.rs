//! Bounded DAE-to-Solve refinement for one scalar constant derivative.
//!
//! This is the bounded C61 fact checker. The affine transition that invokes it
//! consumes the C60 checked-root carrier and checks only facts C60 deliberately
//! does not cover: the literal
//! start's bitwise transfer, the exact oriented residual's derivative meaning,
//! the exact two-operation Solve kernel, the exact tangent and visible-row
//! programs that kernel induces, the exact structural artifact set, and
//! absence of every other executable owner. The comparison is structural and
//! model-agnostic; names never enter its fixed-size fact vocabulary.
//!
//! The receipt rides the sole production lowering. `lower_solve_model` admits
//! the profile over the exact prepared DAE before any Solve construction and,
//! for an admitted profile, checks the Solve root it has just built. A
//! refused in-profile root fails that lowering, so no consumer ever holds a
//! `LoweredSolveModel` whose in-profile Solve root disagrees with its DAE. A
//! DAE outside the profile lowers exactly as any other model and carries the
//! typed refusal in place of a receipt; nothing is claimed about it. There is
//! no second lowering, fallback, or repair path behind the profile.
//!
//! In the SPEC_0037 ladder the containing production transition provides L2
//! witness gating for the live `LoweredSolveModel` inside the profile, around a
//! pure, total comparison over closed facts. The Lean pilot proves the whole
//! fact comparison and actual program-to-fact projection, not enclosing-root
//! selection, runtime semantics, or the whole C61 transition. No whole-C61 L3/L4 claim is
//! made. Bare Solve wire and `into_model` are outside this live claim.

mod diagnostics;
#[cfg(test)]
mod tests;
#[cfg(any(test, kani))]
mod verification;

pub use diagnostics::{
    DerivativePatternKind, NonIdentityMassMatrixKind, NonemptyDerivativePatternKind,
    SolveExecutableOwner, SolveMetadataField,
};

use std::collections::HashMap;

use rumoca_core::Fixity;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

const DAE_CENSUS_WIDTH: usize = 31;
const SOLVE_OWNER_WIDTH: usize = 28;
const KERNEL_OPERATION_WIDTH: usize = 2;
const FULL_JACOBIAN_OPERATION_WIDTH: usize = 3;
const VISIBLE_ROW_OPERATION_WIDTH: usize = 2;

/// Opaque evidence minted only as the successful value of the SOLVE-C61 fact
/// comparison. The affine transition is the sole caller that can install the
/// receipt beside the C60-checked root from which both inputs were obtained.
pub struct CheckedDaeSolveScalarConstantDerivativeRefinement {
    _private: (),
}

/// A DAE outside the deliberately narrow structural profile.
///
/// This is a disposition, not a lowering failure: the model is lowered
/// generically and simply carries no C61 receipt.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ScalarConstantDerivativeUnsupported {
    #[error("profile does not admit runtime start overrides")]
    RuntimeOverrides,
    #[error("profile requires exactly one DAE variable; found {actual}")]
    VariableCount { actual: usize },
    #[error("profile requires one scalar Real state with a fixed start")]
    StateDeclaration,
    #[error("profile requires a literal Real start")]
    LiteralStart,
    #[error("profile requires exactly one top-level scalar residual owner")]
    ResidualOwner,
    #[error("profile requires residual der(state)-constant or constant-der(state)")]
    ResidualShape,
    #[error("profile does not admit DAE executable owner `{owner}`; found {actual}")]
    DaeOwner { owner: &'static str, actual: usize },
}

/// A Solve projection that disagrees with the already-admitted DAE facts.
///
/// Inside the profile this refuses the production lowering itself.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ScalarConstantDerivativeMismatch {
    #[error("Solve catalog start must contain exactly one scalar")]
    CatalogStartShape,
    #[error("Solve catalog start bits differ: expected {expected:#018x}, found {actual:#018x}")]
    CatalogStartBits { expected: u64, actual: u64 },
    #[error("Solve initial_y must contain exactly one scalar")]
    InitialYShape,
    #[error("Solve initial_y bits differ: expected {expected:#018x}, found {actual:#018x}")]
    InitialYBits { expected: u64, actual: u64 },
    #[error("Solve derivative kernel must be one scalar program with output [0]")]
    KernelShape,
    #[error("Solve derivative kernel must be exactly [Const(c), StoreOutput(0)]")]
    KernelOperations,
    #[error(
        "Solve derivative constant bits differ: expected {expected:#018x}, found {actual:#018x}"
    )]
    DerivativeConstantBits { expected: u64, actual: u64 },
    #[error("Solve full Jacobian must be one scalar program with output [0]")]
    FullJacobianShape,
    #[error("Solve full Jacobian must be exactly [Const(c), Const(0.0), StoreOutput(tangent)]")]
    FullJacobianOperations,
    #[error(
        "Solve full Jacobian primal bits differ: expected {expected:#018x}, found {actual:#018x}"
    )]
    FullJacobianPrimalBits { expected: u64, actual: u64 },
    #[error("Solve full Jacobian tangent must be +0.0; found {actual:#018x}")]
    FullJacobianTangentBits { actual: u64 },
    #[error("Solve visible rows must be one scalar program with output [0]")]
    VisibleRowShape,
    #[error("Solve visible row must be exactly [LoadY(0), StoreOutput(0)]")]
    VisibleRowOperations,
    #[error("Solve executable owner `{owner}` has count {actual}, expected {expected}")]
    SolveOwner {
        owner: SolveExecutableOwner,
        expected: usize,
        actual: usize,
    },
    #[error("Solve metadata `{field}` has count {actual}, expected {expected}")]
    SolveMetadata {
        field: SolveMetadataField,
        expected: usize,
        actual: usize,
    },
    #[error("Solve mass matrix must be Identity; found {kind} with {entry_count} stored entries")]
    MassMatrix {
        kind: NonIdentityMassMatrixKind,
        entry_count: usize,
    },
    #[error("Solve derivative structural pattern must be empty 1x1; found {kind} {rows}x{columns}")]
    DerivativeStructuralPattern {
        kind: DerivativePatternKind,
        rows: u32,
        columns: u32,
    },
}

/// DAE-side facts admitted before Solve construction begins.
pub(crate) struct AdmittedScalarConstantDerivativeProfile {
    start_bits: u64,
    derivative_constant_bits: u64,
}

/// One register operation reduced to the closed vocabulary the profile can
/// name. Any operation outside that vocabulary is `Unsupported`, so a foreign
/// operation can never satisfy an exact expectation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OperationFact {
    Constant { destination: u32, bits: u64 },
    LoadY { destination: u32, index: usize },
    StoreOutput { source: u32 },
    Unsupported,
}

/// Closed projection of a complete sole program with logical outputs `[0]`.
/// An unsupported shape carries no prefix that could be mistaken for a body.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ScalarBlockFacts<const WIDTH: usize> {
    UnsupportedShape,
    Exact([OperationFact; WIDTH]),
}

struct SolveFacts {
    catalog_start_present: bool,
    catalog_start_width: usize,
    catalog_start_bits: u64,
    initial_y_width: usize,
    initial_y_bits: u64,
    kernel: ScalarBlockFacts<KERNEL_OPERATION_WIDTH>,
    full_jacobian: ScalarBlockFacts<FULL_JACOBIAN_OPERATION_WIDTH>,
    visible_rows: ScalarBlockFacts<VISIBLE_ROW_OPERATION_WIDTH>,
    owners: [usize; SOLVE_OWNER_WIDTH],
    metadata: SolveMetadataFacts,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum MassMatrixFact {
    Identity,
    Diagonal { entry_count: usize },
    Sparse { entry_count: usize },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum DerivativePatternFact {
    Absent,
    Empty {
        rows: u32,
        columns: u32,
    },
    Other {
        kind: NonemptyDerivativePatternKind,
        rows: u32,
        columns: u32,
    },
}

#[derive(Clone, Copy)]
struct SolveMetadataFacts {
    pure_call_owners: usize,
    implicit_row_targets: usize,
    algebraic_projection_blocks: usize,
    manifold_projection_blocks: usize,
    initialization_projection_unknowns: usize,
    initialization_projection_blocks: usize,
    initialization_update_targets: usize,
    discrete_update_targets: usize,
    discrete_event_iteration_runs: usize,
    discrete_runtime_assignment_targets: usize,
    discrete_runtime_assignment_roles: usize,
    discrete_post_commit_targets: usize,
    discrete_post_commit_runtime_rows: usize,
    discrete_row_roles: usize,
    discrete_pre_modes: usize,
    discrete_observation_refresh: usize,
    discrete_observation_refresh_reads_y: usize,
    discrete_integrator_history_effects: usize,
    discrete_clock_owners: usize,
    discrete_structured_updates: usize,
    discrete_guarded_assignments: usize,
    discrete_event_transactions: usize,
    discrete_clock_partition_order: usize,
    discrete_clock_intermediate_targets: usize,
    discrete_clock_intermediate_clocks: usize,
    event_root_memory_targets: usize,
    event_root_zero_domains: usize,
    event_root_refresh_roles: usize,
    event_condition_memories: usize,
    event_scheduled_roots: usize,
    event_scheduled_times: usize,
    event_dynamic_time_names: usize,
    event_actions: usize,
    event_has_terminal: usize,
    event_delay_targets: usize,
    event_delay_discrete_flags: usize,
    clock_schedules: usize,
    clock_activation_parameters: usize,
    continuous_refresh_rows: usize,
    continuous_refresh_static_parameters: usize,
    mass_matrix: MassMatrixFact,
    structural_implicit: usize,
    structural_algebraic_projection_blocks: usize,
    structural_manifold: usize,
    structural_manifold_projection_blocks: usize,
    structural_derivative: DerivativePatternFact,
    initialization_structural_residual: usize,
    initialization_structural_projection_blocks: usize,
}

/// Admit the DAE half of the profile over the exact prepared DAE.
///
/// Runtime overrides are refused first: an overridden start no longer has a
/// literal DAE origin, so the bitwise start transfer would have nothing to
/// refine against.
pub(crate) fn admit_scalar_constant_derivative_profile(
    view: dae::DaeView<'_>,
    overrides: &HashMap<String, f64>,
) -> Result<AdmittedScalarConstantDerivativeProfile, ScalarConstantDerivativeUnsupported> {
    if !overrides.is_empty() {
        return Err(ScalarConstantDerivativeUnsupported::RuntimeOverrides);
    }
    admit_dae_profile(view)
}

/// Project one Solve root and check it against an admitted profile.
///
/// The receipt is the success value rather than an adjacent construction step.
/// The sole affine C61 transition obtains both arguments from its consumed
/// C60-checked carrier and is the only operation that can install this result.
pub(crate) fn check_scalar_constant_derivative_facts(
    profile: &AdmittedScalarConstantDerivativeProfile,
    model: &solve::SolveModel,
) -> Result<CheckedDaeSolveScalarConstantDerivativeRefinement, ScalarConstantDerivativeMismatch> {
    let solve_facts = project_solve_facts(model);
    check_scalar_constant_derivative_refinement(profile, &solve_facts)
}

fn admit_dae_profile(
    view: dae::DaeView<'_>,
) -> Result<AdmittedScalarConstantDerivativeProfile, ScalarConstantDerivativeUnsupported> {
    if view.variable_count() != 1 {
        return Err(ScalarConstantDerivativeUnsupported::VariableCount {
            actual: view.variable_count(),
        });
    }
    let Some((_, variable)) = view.variables().next() else {
        return Err(ScalarConstantDerivativeUnsupported::VariableCount { actual: 0 });
    };
    let dae::VariableIdentity::State(state) = variable.identity() else {
        return Err(ScalarConstantDerivativeUnsupported::StateDeclaration);
    };
    if variable.value_type().scalar_type() != dae::ScalarType::Real
        || !variable.value_type().dimensions().is_empty()
        || variable.scalar_count() != 1
        || variable.fixed() != Fixity::Fixed
        || variable.causality() != dae::VariableCausality::Local
        || variable.variability() != dae::ExpressionVariability::Continuous
        || variable.is_tunable()
    {
        return Err(ScalarConstantDerivativeUnsupported::StateDeclaration);
    }
    let Some(start) = variable.start() else {
        return Err(ScalarConstantDerivativeUnsupported::LiteralStart);
    };
    let dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(start)) =
        view.exact_expression(start).operation()
    else {
        return Err(ScalarConstantDerivativeUnsupported::LiteralStart);
    };

    let census = project_dae_owner_census(view);
    check_dae_owner_census(&census)?;
    let Some(dae::ContinuousOwnerView::Residual { equation, .. }) = view.continuous_owner(0) else {
        return Err(ScalarConstantDerivativeUnsupported::ResidualOwner);
    };
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = view.exact_expression(equation.residual()).operation()
    else {
        return Err(ScalarConstantDerivativeUnsupported::ResidualShape);
    };
    let left = view.exact_expression(lhs).operation();
    let right = view.exact_expression(rhs).operation();
    let constant = match (left, right) {
        (
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(found)),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)),
        ) if found == state => *value,
        (
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(found)),
        ) if found == state => *value,
        _ => return Err(ScalarConstantDerivativeUnsupported::ResidualShape),
    };
    Ok(AdmittedScalarConstantDerivativeProfile {
        start_bits: start.to_bits(),
        derivative_constant_bits: constant.to_bits(),
    })
}

fn check_scalar_constant_derivative_refinement(
    dae: &AdmittedScalarConstantDerivativeProfile,
    solve: &SolveFacts,
) -> Result<CheckedDaeSolveScalarConstantDerivativeRefinement, ScalarConstantDerivativeMismatch> {
    if !solve.catalog_start_present || solve.catalog_start_width != 1 {
        return Err(ScalarConstantDerivativeMismatch::CatalogStartShape);
    }
    if solve.catalog_start_bits != dae.start_bits {
        return Err(ScalarConstantDerivativeMismatch::CatalogStartBits {
            expected: dae.start_bits,
            actual: solve.catalog_start_bits,
        });
    }
    if solve.initial_y_width != 1 {
        return Err(ScalarConstantDerivativeMismatch::InitialYShape);
    }
    if solve.initial_y_bits != dae.start_bits {
        return Err(ScalarConstantDerivativeMismatch::InitialYBits {
            expected: dae.start_bits,
            actual: solve.initial_y_bits,
        });
    }
    check_solve_owner_census(solve)?;
    check_kernel(solve.kernel, dae.derivative_constant_bits)?;
    check_full_jacobian(solve.full_jacobian, dae.derivative_constant_bits)?;
    check_visible_rows(solve.visible_rows)?;
    Ok(CheckedDaeSolveScalarConstantDerivativeRefinement { _private: () })
}

/// The derivative kernel is exactly `[Const(c), StoreOutput(0)]`, with `c`
/// bit-identical to the admitted DAE literal.
fn check_kernel(
    kernel: ScalarBlockFacts<KERNEL_OPERATION_WIDTH>,
    expected_derivative_bits: u64,
) -> Result<(), ScalarConstantDerivativeMismatch> {
    let ScalarBlockFacts::Exact(operations) = kernel else {
        return Err(ScalarConstantDerivativeMismatch::KernelShape);
    };
    let [first, second] = operations;
    let (OperationFact::Constant { destination, bits }, OperationFact::StoreOutput { source }) =
        (first, second)
    else {
        return Err(ScalarConstantDerivativeMismatch::KernelOperations);
    };
    if destination != 0 || source != 0 {
        return Err(ScalarConstantDerivativeMismatch::KernelOperations);
    }
    if bits != expected_derivative_bits {
        return Err(ScalarConstantDerivativeMismatch::DerivativeConstantBits {
            expected: expected_derivative_bits,
            actual: bits,
        });
    }
    Ok(())
}

/// The full Jacobian-vector program of a constant kernel is exactly the
/// forward-mode lowering of `[Const(c), StoreOutput(0)]` seeded over `y` and
/// `p`: the primal constant in register 0, the tangent `d/dy(c) * v = +0.0` in
/// register 1, and a store of the tangent register. Storing the primal
/// register, or a tangent other than positive zero, would make the Jacobian of
/// `der(x) = c` nonzero and is refused.
fn check_full_jacobian(
    facts: ScalarBlockFacts<FULL_JACOBIAN_OPERATION_WIDTH>,
    expected_derivative_bits: u64,
) -> Result<(), ScalarConstantDerivativeMismatch> {
    let ScalarBlockFacts::Exact(operations) = facts else {
        return Err(ScalarConstantDerivativeMismatch::FullJacobianShape);
    };
    let [first, second, third] = operations;
    let (
        OperationFact::Constant {
            destination: primal,
            bits: primal_bits,
        },
        OperationFact::Constant {
            destination: tangent,
            bits: tangent_bits,
        },
        OperationFact::StoreOutput { source },
    ) = (first, second, third)
    else {
        return Err(ScalarConstantDerivativeMismatch::FullJacobianOperations);
    };
    if primal != 0 || tangent != 1 || source != 1 {
        return Err(ScalarConstantDerivativeMismatch::FullJacobianOperations);
    }
    if primal_bits != expected_derivative_bits {
        return Err(ScalarConstantDerivativeMismatch::FullJacobianPrimalBits {
            expected: expected_derivative_bits,
            actual: primal_bits,
        });
    }
    if tangent_bits != const { 0.0_f64.to_bits() } {
        return Err(ScalarConstantDerivativeMismatch::FullJacobianTangentBits {
            actual: tangent_bits,
        });
    }
    Ok(())
}

/// The single visible row reads the state's solver storage and nothing else:
/// exactly `[LoadY(0), StoreOutput(0)]`. With one state the profile admits,
/// solver index 0 is the only storage that can carry it, so a row reading any
/// other index, a parameter, or time would trace something other than the
/// admitted state.
fn check_visible_rows(
    facts: ScalarBlockFacts<VISIBLE_ROW_OPERATION_WIDTH>,
) -> Result<(), ScalarConstantDerivativeMismatch> {
    let ScalarBlockFacts::Exact(operations) = facts else {
        return Err(ScalarConstantDerivativeMismatch::VisibleRowShape);
    };
    let [first, second] = operations;
    let (OperationFact::LoadY { destination, index }, OperationFact::StoreOutput { source }) =
        (first, second)
    else {
        return Err(ScalarConstantDerivativeMismatch::VisibleRowOperations);
    };
    if destination != 0 || index != 0 || source != 0 {
        return Err(ScalarConstantDerivativeMismatch::VisibleRowOperations);
    }
    Ok(())
}

fn project_dae_owner_census(view: dae::DaeView<'_>) -> [usize; DAE_CENSUS_WIDTH] {
    let callable = view.with_callable_source_inventory(|inventory| {
        [
            inventory.functions().len(),
            inventory.expressions().len(),
            inventory.definitions().len(),
            inventory.assertions().len(),
            inventory.conditionals().len(),
            inventory.calls().len(),
            inventory.folds().len(),
        ]
    });
    [
        view.function_count(),
        view.continuous_equation_count(),
        view.continuous_owner_count(),
        view.continuous_family_count(),
        view.initialization_family_count(),
        view.initialization_equation_count(),
        view.initialization_owner_count(),
        view.initial_discrete_value_count(),
        view.discrete_real_equation_count(),
        view.discrete_value_owner_count(),
        view.model_event_transaction_count(),
        view.relation_count(),
        view.condition_count(),
        view.root_count(),
        view.structured_root_count(),
        view.time_event_count(),
        view.event_action_count(),
        view.clock_count(),
        view.clock_ownership_count(),
        view.previous_value_count(),
        view.terminal_count(),
        view.delay_count(),
        view.runtime_quotient_owner_count(),
        callable[0],
        callable[1],
        callable[2],
        callable[3],
        callable[4],
        callable[5],
        callable[6],
        view.discrete_value_definition_count(),
    ]
}

const DAE_OWNER_NAMES: [&str; DAE_CENSUS_WIDTH] = [
    "functions",
    "continuous equations",
    "continuous owners",
    "continuous families",
    "initialization families",
    "initialization equations",
    "initialization owners",
    "initial discrete values",
    "discrete Real equations",
    "discrete value owners",
    "model event transactions",
    "relations",
    "conditions",
    "roots",
    "structured roots",
    "time events",
    "event actions",
    "clocks",
    "clock ownerships",
    "previous values",
    "terminal values",
    "delays",
    "runtime quotients",
    "callable functions",
    "callable expressions",
    "callable definitions",
    "callable assertions",
    "callable conditionals",
    "callable calls",
    "callable folds",
    "discrete value definitions",
];

fn check_dae_owner_census(
    actual: &[usize; DAE_CENSUS_WIDTH],
) -> Result<(), ScalarConstantDerivativeUnsupported> {
    let mut expected = [0; DAE_CENSUS_WIDTH];
    expected[1] = 1;
    expected[2] = 1;
    if let Err(mismatch) = check_owner_counts(actual, &expected) {
        return Err(ScalarConstantDerivativeUnsupported::DaeOwner {
            owner: DAE_OWNER_NAMES[mismatch.index],
            actual: mismatch.actual,
        });
    }
    Ok(())
}

fn project_solve_facts(model: &solve::SolveModel) -> SolveFacts {
    project_solve_values_with_owner_premises(
        model,
        project_solve_owner_census(model),
        project_solve_metadata(model),
    )
}

/// Project starts and programs from one root, forwarding the two supplied
/// owner/metadata premises unchanged. This constructor does not establish
/// either premise's provenance; the production caller derives both from model.
fn project_solve_values_with_owner_premises(
    model: &solve::SolveModel,
    owners: [usize; SOLVE_OWNER_WIDTH],
    metadata: SolveMetadataFacts,
) -> SolveFacts {
    let entries = model.variable_catalog().entries();
    let start = if entries.is_empty() {
        None
    } else {
        entries[0].start()
    };
    let (catalog_start_present, catalog_start_width, catalog_start_bits) =
        project_start_value_facts(start);
    let initial = model.initial_y();
    SolveFacts {
        catalog_start_present,
        catalog_start_width,
        catalog_start_bits,
        initial_y_width: initial.len(),
        initial_y_bits: first_value_bits(initial),
        kernel: project_kernel(model.problem().continuous().derivative_rhs()),
        full_jacobian: project_scalar_block(&model.artifacts().continuous().full_jacobian_v),
        visible_rows: project_scalar_block(model.visible_value_rows()),
        owners,
        metadata,
    }
}

fn project_start_value_facts(start: Option<&[f64]>) -> (bool, usize, u64) {
    match start {
        Some(values) => (true, values.len(), first_value_bits(values)),
        None => (false, 0, 0),
    }
}

fn first_value_bits(values: &[f64]) -> u64 {
    if values.is_empty() {
        0
    } else {
        values[0].to_bits()
    }
}

fn operation_fact(operation: &solve::LinearOp) -> OperationFact {
    match operation {
        solve::LinearOp::Const { dst, value } => OperationFact::Constant {
            destination: *dst,
            bits: value.to_bits(),
        },
        solve::LinearOp::LoadY { dst, index } => OperationFact::LoadY {
            destination: *dst,
            index: *index,
        },
        solve::LinearOp::StoreOutput { src } => OperationFact::StoreOutput { source: *src },
        _ => OperationFact::Unsupported,
    }
}

/// Borrow the entire sole program, never a retained prefix. Selection is
/// independent of instruction semantics and allocates no replacement storage.
fn exact_program<T, const WIDTH: usize>(programs: &[Vec<T>]) -> Option<&[T; WIDTH]> {
    if programs.len() != 1 {
        return None;
    }
    let Ok(operations) = <&[T; WIDTH]>::try_from(&programs[0][..]) else {
        return None;
    };
    Some(operations)
}

fn project_operations<const WIDTH: usize>(
    programs: &[Vec<solve::LinearOp>],
) -> ScalarBlockFacts<WIDTH> {
    let Some(operations) = exact_program::<_, WIDTH>(programs) else {
        return ScalarBlockFacts::UnsupportedShape;
    };
    let mut facts = [OperationFact::Unsupported; WIDTH];
    let mut index = 0;
    while index < WIDTH {
        facts[index] = operation_fact(&operations[index]);
        index += 1;
    }
    ScalarBlockFacts::Exact(facts)
}

fn project_scalar_block<const WIDTH: usize>(
    block: &solve::ScalarProgramBlock,
) -> ScalarBlockFacts<WIDTH> {
    let outputs = block.output_indices();
    if outputs.len() != 1 || outputs[0] != 0 {
        return ScalarBlockFacts::UnsupportedShape;
    }
    project_operations(block.programs())
}

fn project_kernel(block: &solve::ComputeBlock) -> ScalarBlockFacts<KERNEL_OPERATION_WIDTH> {
    if block.nodes.len() != 1 {
        return ScalarBlockFacts::UnsupportedShape;
    }
    let solve::ComputeNode::ScalarPrograms(programs) = &block.nodes[0] else {
        return ScalarBlockFacts::UnsupportedShape;
    };
    project_scalar_block(programs)
}

fn project_solve_owner_census(model: &solve::SolveModel) -> [usize; SOLVE_OWNER_WIDTH] {
    struct Census([usize; SOLVE_OWNER_WIDTH]);
    impl solve::SolveVisitor for Census {
        type Error = std::convert::Infallible;

        fn visit_scalar_program_block(
            &mut self,
            owner: solve::ScalarProgramBlockOwner,
            block: &solve::ScalarProgramBlock,
        ) -> Result<(), Self::Error> {
            // A compute node is one executable owner even if malformed public
            // IR contains an empty scalar block. Its node visitor counts it;
            // counting its programs here as well would both miss the empty
            // case and double-count valid nodes. Standalone scalar-block
            // owners retain one count per executable program.
            if !matches!(owner, solve::ScalarProgramBlockOwner::ComputeBlock(_)) {
                let index = scalar_owner_index(owner);
                self.0[index] = self.0[index].saturating_add(block.programs().len());
            }
            solve::walk_scalar_program_block(self, owner, block)
        }

        fn visit_compute_node(
            &mut self,
            owner: solve::ComputeBlockOwner,
            node_index: usize,
            node: &solve::ComputeNode,
        ) -> Result<(), Self::Error> {
            let index = scalar_owner_index(solve::ScalarProgramBlockOwner::ComputeBlock(owner));
            self.0[index] = self.0[index].saturating_add(1);
            solve::walk_compute_node(self, owner, node_index, node)
        }

        fn visit_linear_op_slice(
            &mut self,
            owner: solve::LinearOpSliceOwner,
            ops: &[solve::LinearOp],
        ) -> Result<(), Self::Error> {
            match owner {
                solve::LinearOpSliceOwner::GuardedAssignmentProgram { .. } => {
                    self.0[24] = self.0[24].saturating_add(ops.len());
                }
                solve::LinearOpSliceOwner::EventMessageValue { .. }
                | solve::LinearOpSliceOwner::EventMessageFormat { .. } => {
                    self.0[25] = self.0[25].saturating_add(ops.len());
                }
                solve::LinearOpSliceOwner::MatMulLhs { .. }
                | solve::LinearOpSliceOwner::MatMulRhs { .. }
                | solve::LinearOpSliceOwner::LinSolveSetup { .. }
                | solve::LinearOpSliceOwner::MapBase { .. }
                | solve::LinearOpSliceOwner::AffineStencilBase { .. } => {
                    self.0[26] = self.0[26].saturating_add(ops.len());
                }
                solve::LinearOpSliceOwner::ScalarProgramConstruction { .. }
                | solve::LinearOpSliceOwner::ScalarProgram { .. } => {}
            }
            Ok(())
        }

        fn visit_event_transaction_program(
            &mut self,
            _index: usize,
            _program: &solve::EventTransactionProgram,
        ) -> Result<(), Self::Error> {
            self.0[27] = self.0[27].saturating_add(1);
            Ok(())
        }
    }
    let mut census = Census([0; SOLVE_OWNER_WIDTH]);
    let result = solve::SolveVisitor::visit_solve_model(&mut census, model);
    if let Err(never) = result {
        match never {}
    }
    census.0
}

const fn scalar_owner_index(owner: solve::ScalarProgramBlockOwner) -> usize {
    match owner {
        solve::ScalarProgramBlockOwner::ComputeBlock(owner) => match owner {
            solve::ComputeBlockOwner::ContinuousImplicitRhs => 0,
            solve::ComputeBlockOwner::ContinuousResidual => 1,
            solve::ComputeBlockOwner::ContinuousManifoldResidual => 2,
            solve::ComputeBlockOwner::ContinuousDerivativeRhs => 3,
            solve::ComputeBlockOwner::InitializationResidual => 4,
            solve::ComputeBlockOwner::DiscreteStructuredRhs => 5,
            solve::ComputeBlockOwner::ContinuousImplicitJacobianV => 6,
            solve::ComputeBlockOwner::ContinuousManifoldJacobianV => 7,
            solve::ComputeBlockOwner::InitializationResidualJacobianV => 8,
        },
        solve::ScalarProgramBlockOwner::InitializationUpdateRhs => 9,
        solve::ScalarProgramBlockOwner::DiscreteRuntimeAssignmentRhs => 10,
        solve::ScalarProgramBlockOwner::DiscretePostCommitAssignmentRhs => 11,
        solve::ScalarProgramBlockOwner::DiscreteRhs => 12,
        solve::ScalarProgramBlockOwner::DiscreteClockPartitionIntermediates => 13,
        solve::ScalarProgramBlockOwner::EventRootConditions => 14,
        solve::ScalarProgramBlockOwner::EventDynamicTimeEventRhs => 15,
        solve::ScalarProgramBlockOwner::EventActionConditions => 16,
        solve::ScalarProgramBlockOwner::EventDelaySourceRhs => 17,
        solve::ScalarProgramBlockOwner::EventDelayTimeRhs => 18,
        solve::ScalarProgramBlockOwner::EventDelayMaxRhs => 19,
        solve::ScalarProgramBlockOwner::ContinuousExactRefreshAssignmentFinalProgram => 20,
        solve::ScalarProgramBlockOwner::ContinuousFullJacobianV => 21,
        solve::ScalarProgramBlockOwner::ContinuousImplicitJacobianVScalar => 22,
        solve::ScalarProgramBlockOwner::VisibleValueRows => 23,
    }
}

const SOLVE_OWNERS: [SolveExecutableOwner; SOLVE_OWNER_WIDTH] = [
    SolveExecutableOwner::ContinuousImplicit,
    SolveExecutableOwner::ContinuousResidual,
    SolveExecutableOwner::ContinuousManifold,
    SolveExecutableOwner::DerivativeKernel,
    SolveExecutableOwner::InitializationResidual,
    SolveExecutableOwner::StructuredDiscrete,
    SolveExecutableOwner::ImplicitTensorJvp,
    SolveExecutableOwner::ManifoldJvp,
    SolveExecutableOwner::InitializationJvp,
    SolveExecutableOwner::InitializationUpdates,
    SolveExecutableOwner::RuntimeAssignments,
    SolveExecutableOwner::PostCommitAssignments,
    SolveExecutableOwner::DiscreteRows,
    SolveExecutableOwner::ClockIntermediates,
    SolveExecutableOwner::RootConditions,
    SolveExecutableOwner::DynamicTimeEvents,
    SolveExecutableOwner::ActionConditions,
    SolveExecutableOwner::DelaySources,
    SolveExecutableOwner::DelayTimes,
    SolveExecutableOwner::DelayMaxima,
    SolveExecutableOwner::ExactRefreshPrograms,
    SolveExecutableOwner::FullDerivativeJvp,
    SolveExecutableOwner::ImplicitScalarJvp,
    SolveExecutableOwner::VisibleRows,
    SolveExecutableOwner::GuardedAssignments,
    SolveExecutableOwner::EventMessagePrograms,
    SolveExecutableOwner::CompactTensorSetup,
    SolveExecutableOwner::EventTransactionPrograms,
];

#[derive(Debug, PartialEq, Eq)]
struct OwnerCountMismatch {
    index: usize,
    expected: usize,
    actual: usize,
}

/// Compare the entire count domain before attaching presentation-only labels.
fn check_owner_counts<const WIDTH: usize>(
    actual: &[usize; WIDTH],
    expected: &[usize; WIDTH],
) -> Result<(), OwnerCountMismatch> {
    let mut index = 0;
    while index < WIDTH {
        if actual[index] != expected[index] {
            return Err(OwnerCountMismatch {
                index,
                expected: expected[index],
                actual: actual[index],
            });
        }
        index += 1;
    }
    Ok(())
}

/// Every executable owner is counted; the three owners the profile permits
/// (derivative kernel, its full JVP, and the visible rows) are additionally
/// inspected operation by operation by the kernel, Jacobian, and visible-row
/// checks, so a count of one never stands in for the right body.
fn check_solve_owner_census(solve: &SolveFacts) -> Result<(), ScalarConstantDerivativeMismatch> {
    let mut expected = [0; SOLVE_OWNER_WIDTH];
    expected[3] = 1;
    expected[21] = 1;
    expected[23] = 1;
    if let Err(mismatch) = check_owner_counts(&solve.owners, &expected) {
        return Err(ScalarConstantDerivativeMismatch::SolveOwner {
            owner: SOLVE_OWNERS[mismatch.index],
            expected: mismatch.expected,
            actual: mismatch.actual,
        });
    }
    check_general_metadata(solve.metadata)?;
    check_structural_metadata(solve.metadata)?;
    check_discrete_metadata(solve.metadata)?;
    check_event_metadata(solve.metadata)
}

fn check_general_metadata(
    facts: SolveMetadataFacts,
) -> Result<(), ScalarConstantDerivativeMismatch> {
    require_metadata(
        SolveMetadataField::PureCallOwners,
        0,
        facts.pure_call_owners,
    )?;
    require_metadata(
        SolveMetadataField::ImplicitRowTargets,
        0,
        facts.implicit_row_targets,
    )?;
    require_metadata(
        SolveMetadataField::AlgebraicProjectionBlocks,
        0,
        facts.algebraic_projection_blocks,
    )?;
    require_metadata(
        SolveMetadataField::ManifoldProjectionBlocks,
        0,
        facts.manifold_projection_blocks,
    )?;
    require_metadata(
        SolveMetadataField::InitializationProjectionUnknowns,
        0,
        facts.initialization_projection_unknowns,
    )?;
    require_metadata(
        SolveMetadataField::InitializationProjectionBlocks,
        0,
        facts.initialization_projection_blocks,
    )?;
    require_metadata(
        SolveMetadataField::InitializationUpdateTargets,
        0,
        facts.initialization_update_targets,
    )?;
    require_metadata(
        SolveMetadataField::ContinuousRefreshRows,
        0,
        facts.continuous_refresh_rows,
    )?;
    require_metadata(
        SolveMetadataField::ContinuousRefreshStaticParameters,
        0,
        facts.continuous_refresh_static_parameters,
    )?;
    match facts.mass_matrix {
        MassMatrixFact::Identity => Ok(()),
        MassMatrixFact::Diagonal { entry_count } => {
            Err(ScalarConstantDerivativeMismatch::MassMatrix {
                kind: NonIdentityMassMatrixKind::Diagonal,
                entry_count,
            })
        }
        MassMatrixFact::Sparse { entry_count } => {
            Err(ScalarConstantDerivativeMismatch::MassMatrix {
                kind: NonIdentityMassMatrixKind::Sparse,
                entry_count,
            })
        }
    }
}

/// Each structural artifact kind is pinned separately. The only artifact a
/// constant explicit ODE induces is the derivative Jacobian structure; an
/// implicit, manifold, projection, or initialization structure in its place
/// would describe a different system even when the total count agrees.
fn check_structural_metadata(
    facts: SolveMetadataFacts,
) -> Result<(), ScalarConstantDerivativeMismatch> {
    require_metadata(
        SolveMetadataField::StructuralImplicit,
        0,
        facts.structural_implicit,
    )?;
    require_metadata(
        SolveMetadataField::StructuralAlgebraicProjectionBlocks,
        0,
        facts.structural_algebraic_projection_blocks,
    )?;
    require_metadata(
        SolveMetadataField::StructuralManifold,
        0,
        facts.structural_manifold,
    )?;
    require_metadata(
        SolveMetadataField::StructuralManifoldProjectionBlocks,
        0,
        facts.structural_manifold_projection_blocks,
    )?;
    match facts.structural_derivative {
        DerivativePatternFact::Empty {
            rows: 1,
            columns: 1,
        } => {}
        DerivativePatternFact::Absent => {
            return Err(
                ScalarConstantDerivativeMismatch::DerivativeStructuralPattern {
                    kind: DerivativePatternKind::Absent,
                    rows: 0,
                    columns: 0,
                },
            );
        }
        DerivativePatternFact::Empty { rows, columns } => {
            return Err(
                ScalarConstantDerivativeMismatch::DerivativeStructuralPattern {
                    kind: DerivativePatternKind::Empty,
                    rows,
                    columns,
                },
            );
        }
        DerivativePatternFact::Other {
            kind,
            rows,
            columns,
        } => {
            return Err(
                ScalarConstantDerivativeMismatch::DerivativeStructuralPattern {
                    kind: DerivativePatternKind::Other(kind),
                    rows,
                    columns,
                },
            );
        }
    }
    require_metadata(
        SolveMetadataField::InitializationStructuralResidual,
        0,
        facts.initialization_structural_residual,
    )?;
    require_metadata(
        SolveMetadataField::InitializationStructuralProjectionBlocks,
        0,
        facts.initialization_structural_projection_blocks,
    )
}

fn check_discrete_metadata(
    facts: SolveMetadataFacts,
) -> Result<(), ScalarConstantDerivativeMismatch> {
    require_metadata(
        SolveMetadataField::DiscreteUpdateTargets,
        0,
        facts.discrete_update_targets,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteEventIterationRuns,
        0,
        facts.discrete_event_iteration_runs,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteRuntimeAssignmentTargets,
        0,
        facts.discrete_runtime_assignment_targets,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteRuntimeAssignmentRoles,
        0,
        facts.discrete_runtime_assignment_roles,
    )?;
    require_metadata(
        SolveMetadataField::DiscretePostCommitTargets,
        0,
        facts.discrete_post_commit_targets,
    )?;
    require_metadata(
        SolveMetadataField::DiscretePostCommitRuntimeRows,
        0,
        facts.discrete_post_commit_runtime_rows,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteRowRoles,
        0,
        facts.discrete_row_roles,
    )?;
    require_metadata(
        SolveMetadataField::DiscretePreModes,
        0,
        facts.discrete_pre_modes,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteObservationRefresh,
        0,
        facts.discrete_observation_refresh,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteObservationRefreshReadsY,
        0,
        facts.discrete_observation_refresh_reads_y,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteIntegratorHistoryEffects,
        0,
        facts.discrete_integrator_history_effects,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteClockOwners,
        0,
        facts.discrete_clock_owners,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteStructuredUpdates,
        0,
        facts.discrete_structured_updates,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteGuardedAssignments,
        0,
        facts.discrete_guarded_assignments,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteEventTransactions,
        0,
        facts.discrete_event_transactions,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteClockPartitionOrder,
        0,
        facts.discrete_clock_partition_order,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteClockIntermediateTargets,
        0,
        facts.discrete_clock_intermediate_targets,
    )?;
    require_metadata(
        SolveMetadataField::DiscreteClockIntermediateClocks,
        0,
        facts.discrete_clock_intermediate_clocks,
    )
}

fn check_event_metadata(facts: SolveMetadataFacts) -> Result<(), ScalarConstantDerivativeMismatch> {
    require_metadata(
        SolveMetadataField::EventRootMemoryTargets,
        0,
        facts.event_root_memory_targets,
    )?;
    require_metadata(
        SolveMetadataField::EventRootZeroDomains,
        0,
        facts.event_root_zero_domains,
    )?;
    require_metadata(
        SolveMetadataField::EventRootRefreshRoles,
        0,
        facts.event_root_refresh_roles,
    )?;
    require_metadata(
        SolveMetadataField::EventConditionMemories,
        0,
        facts.event_condition_memories,
    )?;
    require_metadata(
        SolveMetadataField::EventScheduledRoots,
        0,
        facts.event_scheduled_roots,
    )?;
    require_metadata(
        SolveMetadataField::EventScheduledTimes,
        0,
        facts.event_scheduled_times,
    )?;
    require_metadata(
        SolveMetadataField::EventDynamicTimeNames,
        0,
        facts.event_dynamic_time_names,
    )?;
    require_metadata(SolveMetadataField::EventActions, 0, facts.event_actions)?;
    require_metadata(
        SolveMetadataField::EventHasTerminal,
        0,
        facts.event_has_terminal,
    )?;
    require_metadata(
        SolveMetadataField::EventDelayTargets,
        0,
        facts.event_delay_targets,
    )?;
    require_metadata(
        SolveMetadataField::EventDelayDiscreteFlags,
        0,
        facts.event_delay_discrete_flags,
    )?;
    require_metadata(SolveMetadataField::ClockSchedules, 0, facts.clock_schedules)?;
    require_metadata(
        SolveMetadataField::ClockActivationParameters,
        0,
        facts.clock_activation_parameters,
    )
}

fn require_metadata(
    field: SolveMetadataField,
    expected: usize,
    actual: usize,
) -> Result<(), ScalarConstantDerivativeMismatch> {
    if actual != expected {
        return Err(ScalarConstantDerivativeMismatch::SolveMetadata {
            field,
            expected,
            actual,
        });
    }
    Ok(())
}

fn project_derivative_pattern(
    structure: Option<&solve::JacobianStructure>,
) -> DerivativePatternFact {
    let Some(structure) = structure else {
        return DerivativePatternFact::Absent;
    };
    let pattern = structure.pattern();
    let rows = pattern.rows();
    let columns = pattern.columns();
    match pattern.view() {
        solve::StructuralPatternView::Empty => DerivativePatternFact::Empty { rows, columns },
        solve::StructuralPatternView::Full => DerivativePatternFact::Other {
            kind: NonemptyDerivativePatternKind::Full,
            rows,
            columns,
        },
        solve::StructuralPatternView::Diagonal => DerivativePatternFact::Other {
            kind: NonemptyDerivativePatternKind::Diagonal,
            rows,
            columns,
        },
        solve::StructuralPatternView::Banded { .. } => DerivativePatternFact::Other {
            kind: NonemptyDerivativePatternKind::Banded,
            rows,
            columns,
        },
        solve::StructuralPatternView::Csr { .. } => DerivativePatternFact::Other {
            kind: NonemptyDerivativePatternKind::Csr,
            rows,
            columns,
        },
        solve::StructuralPatternView::Affine { .. } => DerivativePatternFact::Other {
            kind: NonemptyDerivativePatternKind::Affine,
            rows,
            columns,
        },
    }
}

fn project_solve_metadata(model: &solve::SolveModel) -> SolveMetadataFacts {
    let problem = model.problem();
    let continuous = problem.continuous();
    let refresh = continuous.refresh_owners();
    let initialization = problem.initialization();
    let discrete = problem.discrete();
    let events = problem.events();
    let delays = &events.delays;
    let artifacts = model.artifacts();
    let mass_matrix = match &artifacts.continuous().mass_matrix {
        solve::MassMatrix::Identity => MassMatrixFact::Identity,
        solve::MassMatrix::Diagonal { values } => MassMatrixFact::Diagonal {
            entry_count: values.len(),
        },
        solve::MassMatrix::Sparse { entries } => MassMatrixFact::Sparse {
            entry_count: entries.len(),
        },
    };
    let structural = &artifacts.continuous().structural;
    let structural_derivative = project_derivative_pattern(structural.derivative());
    let initialization_structural = &artifacts.initialization().structural;
    SolveMetadataFacts {
        pure_call_owners: model.pure_calls().owners().len(),
        implicit_row_targets: continuous.implicit_row_targets().len(),
        algebraic_projection_blocks: continuous.algebraic_projection_plan().blocks.len(),
        manifold_projection_blocks: continuous.manifold_projection_plan().blocks.len(),
        initialization_projection_unknowns: initialization.projection_unknowns().len(),
        initialization_projection_blocks: initialization.projection_plan().blocks.len(),
        initialization_update_targets: initialization.update_targets().len(),
        discrete_update_targets: discrete.update_targets.len(),
        discrete_event_iteration_runs: discrete.event_iteration_plan.runs.len(),
        discrete_runtime_assignment_targets: discrete.runtime_assignment_targets.len(),
        discrete_runtime_assignment_roles: discrete.runtime_assignment_roles.len(),
        discrete_post_commit_targets: discrete.post_commit_assignment_targets.len(),
        discrete_post_commit_runtime_rows: discrete.post_commit_assignment_runtime_rows.len(),
        discrete_row_roles: discrete.row_roles.len(),
        discrete_pre_modes: discrete.pre_modes.len(),
        discrete_observation_refresh: discrete.observation_refresh.len(),
        discrete_observation_refresh_reads_y: usize::from(discrete.observation_refresh_reads_y),
        discrete_integrator_history_effects: discrete.integrator_history_effects.len(),
        discrete_clock_owners: discrete.clock_owners.len(),
        discrete_structured_updates: discrete.structured_updates.len(),
        discrete_guarded_assignments: discrete.guarded_assignments.len(),
        discrete_event_transactions: discrete.event_transactions.len(),
        discrete_clock_partition_order: discrete.clock_partition_order.len(),
        discrete_clock_intermediate_targets: discrete.clock_partition_intermediate_targets.len(),
        discrete_clock_intermediate_clocks: discrete.clock_partition_intermediate_clocks.len(),
        event_root_memory_targets: events.root_relation_memory_targets.len(),
        event_root_zero_domains: events.root_zero_domains.len(),
        event_root_refresh_roles: events.root_relation_refresh_roles.len(),
        event_condition_memories: events.condition_memory_parameter_indices.len(),
        event_scheduled_roots: events.scheduled_root_conditions.len(),
        event_scheduled_times: events.scheduled_time_events.len(),
        event_dynamic_time_names: events.dynamic_time_event_names.len(),
        event_actions: events.actions.len(),
        event_has_terminal: usize::from(events.has_terminal_event),
        event_delay_targets: delays.value_parameter_indices.len(),
        event_delay_discrete_flags: delays.source_is_discrete.len(),
        clock_schedules: problem.clocks().periodic_event_schedules.len(),
        clock_activation_parameters: problem.clocks().activation_parameter_indices.len(),
        continuous_refresh_rows: refresh.algebraic().rows().len()
            + refresh.derivative().rows().len()
            + refresh.root().rows().len()
            + refresh.event().rows().len()
            + refresh.clock_events().len(),
        continuous_refresh_static_parameters: refresh.static_parameter_indices().len(),
        mass_matrix,
        structural_implicit: usize::from(structural.implicit().is_some()),
        structural_algebraic_projection_blocks: structural.algebraic_projection().len(),
        structural_manifold: usize::from(structural.manifold().is_some()),
        structural_manifold_projection_blocks: structural.manifold_projection().len(),
        structural_derivative,
        initialization_structural_residual: usize::from(
            initialization_structural.residual().is_some(),
        ),
        initialization_structural_projection_blocks: initialization_structural.projection().len(),
    }
}
