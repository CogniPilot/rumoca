//! The GALEC checked-construction boundary: structural closure followed by six
//! static analyses per SPEC_0034 "Validator Scope" (§3.2.2) and GAL-018, run
//! over a [`Block`] with ALL findings collected (never fail-fast).
//!
//! | Analysis | Module | Codes |
//! |----------|--------|-------|
//! | Structural closure | `structure` | EG001, EG004–EG009 |
//! | Name | `names` | EG002/EG003, EG010–EG013 |
//! | Type | `types` | EG014–EG021, EG042 |
//! | Dimensionality | `dims` | EG022–EG025, EG040 |
//! | Termination | `termination` | EG026–EG028 |
//! | Side-effect | `effects` | EG029–EG033 |
//! | Signals | `signals` | EG034–EG039 |
//!
//! Some Validator-Scope rules are guaranteed by AST construction rather
//! than checked here: exactly three parameter-free block-interface methods
//! (`Startup`/`Recalibrate`/`DoStep` are dedicated [`Block`] fields, trap
//! T1), matching `end` names (each name is stored once), the mandatory
//! if-expression `else` (trap T12), unary minus over references only (trap
//! T4), and methods exposing only predefined signals (§3.2.5 §1.3).
//!
//! Reporting discipline: each defect is diagnosed exactly once — the type
//! analysis is the sole reporter of resolution failures (EG014/EG015); the
//! other analyses resolve silently and skip what they cannot resolve.
//!
//! Real relational and equality operators signal `NAN` for qNaN operands
//! (SPEC_0034 D8, trap T9). Signal analysis consumes the expression types
//! proven by type analysis, so Integer and Boolean comparisons do not acquire
//! spurious escape signals.

use crate::ast::Block;
use crate::diagnostic::GalecError;

mod context;
mod dims;
mod effects;
mod locate;
mod names;
mod navigate;
mod retained;
mod signals;
mod spans;
mod structure;
mod termination;
mod types;

pub use locate::span_of;
pub use navigate::{SymbolInfo, symbol_at};
pub(crate) use retained::{
    BinderLoc, BlockDeclarationStartLiteral, BuiltinResultLoc, CallLoc, CallResultProjectionLoc,
    CallResultReceiverLoc, CalleeResultLoc, ChildRole, DeclarationClass, DeclarationLoc,
    EvaluatedLiteral, EvaluatedScalar, ExpressionKind, ExpressionLoc, FixedValueShape, FunctionLoc,
    GeneratedOrigin, MethodLoc, MethodOwner, RealMatrixMultiplyOccurrenceLoc, ReferenceLoc,
    ResolvedTarget, RetainedValidation, RetainedValidationError, StatementKind, StatementLoc,
    SubjectLoc, SubjectParent, SubjectProvenance,
};

pub(crate) enum ValidationFailure {
    Diagnostics(Vec<GalecError>),
    Index(retained::RetainedValidationError),
}

pub(crate) enum SignalClausePolicy {
    RetainAuthored,
    DeriveGenerated,
}

/// Declaration-start authority at this construction boundary.
///
/// Algorithm Code concrete syntax does not encode the manifest-bound start
/// relation (SPEC_0034 GAL-014/GAL-020), so parsed syntax closes that relation
/// with an explicit syntax-only disposition. Compiler-generated packages must
/// instead supply and prove every mandated start before construction can
/// succeed (SPEC_0042 §6, declaration/start relation).
#[derive(Clone, Copy)]
pub(super) enum DeclarationStartContract {
    ParsedSyntax,
    GeneratedPackage,
}

pub(crate) fn close(
    block: &mut Block,
    signal_clauses: SignalClausePolicy,
) -> Result<retained::RetainedValidation, ValidationFailure> {
    close_with_arithmetic_profile(
        block,
        signal_clauses,
        DeclarationStartContract::ParsedSyntax,
        None,
    )
}

pub(crate) fn close_profiled(
    block: &mut Block,
    signal_clauses: SignalClausePolicy,
    arithmetic: crate::package::AlgorithmCodeArithmeticProfile,
) -> Result<retained::RetainedValidation, ValidationFailure> {
    close_with_arithmetic_profile(
        block,
        signal_clauses,
        DeclarationStartContract::GeneratedPackage,
        Some(arithmetic),
    )
}

fn close_with_arithmetic_profile(
    block: &mut Block,
    signal_clauses: SignalClausePolicy,
    declaration_starts: DeclarationStartContract,
    arithmetic: Option<crate::package::AlgorithmCodeArithmeticProfile>,
) -> Result<retained::RetainedValidation, ValidationFailure> {
    let mut retained =
        retained::RetainedValidationBuilder::install(block, declaration_starts, arithmetic)
            .map_err(ValidationFailure::Index)?;
    let mut diagnostics = Vec::new();
    check_without_signals(block, declaration_starts, &mut retained, &mut diagnostics)
        .map_err(ValidationFailure::Index)?;
    if matches!(signal_clauses, SignalClausePolicy::DeriveGenerated) {
        let [startup, recalibrate, do_step] =
            derive_generated_signal_clauses(block, &retained).map_err(ValidationFailure::Index)?;
        block.startup.signals = startup;
        block.recalibrate.signals = recalibrate;
        block.do_step.signals = do_step;
    }
    signals::check(
        &context::BlockContext::new(block),
        &retained,
        &mut diagnostics,
    )
    .map_err(ValidationFailure::Index)?;
    if diagnostics.is_empty() {
        retained.finish().map_err(ValidationFailure::Index)
    } else {
        Err(ValidationFailure::Diagnostics(diagnostics))
    }
}

fn check_without_signals(
    block: &Block,
    declaration_starts: DeclarationStartContract,
    retained: &mut retained::RetainedValidationBuilder,
    diagnostics: &mut Vec<GalecError>,
) -> Result<(), retained::RetainedValidationError> {
    let ctx = context::BlockContext::new(block);
    structure::check(&ctx, diagnostics);
    names::check(&ctx, diagnostics);
    types::check(&ctx, declaration_starts, retained, diagnostics)?;
    dims::check(&ctx, retained, diagnostics)?;
    termination::check(&ctx, retained, diagnostics)?;
    effects::check(&ctx, retained, diagnostics)?;
    Ok(())
}

/// Close every generated signal clause before whole-block validation.
///
/// The generated projection starts with empty user-function clauses. The
/// language requires an acyclic call graph, so visiting functions callee-first
/// closes every clause exactly once. Expression types are invariant while only
/// those clauses change; proving them once avoids a quadratic sequence of
/// identical whole-block checks. Parsed source retains its authored clauses,
/// so an incorrect clause remains a construction diagnostic.
fn derive_generated_signal_clauses(
    block: &mut Block,
    retained: &retained::RetainedValidationBuilder,
) -> Result<[Vec<crate::ast::PredefinedSignal>; 3], retained::RetainedValidationError> {
    let order = generated_function_callee_first(retained)?;
    for function_index in order {
        let clause = {
            let ctx = context::BlockContext::new(block);
            signals::user_signal_clause(&ctx, retained, function_index)?
        };
        let Some(function) = block
            .protected_functions
            .iter_mut()
            .chain(&mut block.public_functions)
            .nth(function_index)
        else {
            return Err(retained::RetainedValidationError::MissingResolvedSubject);
        };
        function.signals = clause;
    }
    let ctx = context::BlockContext::new(block);
    signals::method_signal_clauses(&ctx, retained)
}

fn generated_function_callee_first(
    retained: &retained::RetainedValidationBuilder,
) -> Result<Vec<usize>, retained::RetainedValidationError> {
    let graph = retained.user_call_graph()?;
    let mut colors = vec![0_u8; graph.functions.len()];
    let mut order = Vec::with_capacity(graph.functions.len());
    for start in 0..graph.functions.len() {
        if colors[start] != 0 {
            continue;
        }
        colors[start] = 1;
        let mut frames = vec![(start, 0_usize)];
        while let Some((node, next_edge)) = frames.last_mut() {
            let Some(edge) = graph.functions[*node].get(*next_edge) else {
                colors[*node] = 2;
                order.push(*node);
                frames.pop();
                continue;
            };
            *next_edge += 1;
            let callee = edge.callee.index();
            match colors[callee] {
                0 => {
                    colors[callee] = 1;
                    frames.push((callee, 0));
                }
                // Termination analysis owns the one recursion diagnostic.
                // Skipping a gray edge here only makes clause derivation
                // terminate while that diagnostic-bearing root is rejected.
                1 | 2 => {}
                _ => {
                    return Err(retained::RetainedValidationError::InconsistentFact {
                        family: "user-call-order-color",
                        index: u32::try_from(callee)
                            .map_err(|_| retained::RetainedValidationError::LocatorOverflow)?,
                    });
                }
            }
        }
    }
    Ok(order)
}
