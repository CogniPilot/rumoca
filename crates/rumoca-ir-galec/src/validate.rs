//! The GALEC checked-construction boundary: structural closure followed by six
//! static analyses per SPEC_0034 "Validator Scope" (§3.2.2) and GAL-018, run
//! over a [`Block`] with ALL findings collected (never fail-fast).
//!
//! | Analysis | Module | Codes |
//! |----------|--------|-------|
//! | Structural closure | `structure` | EG001, EG004–EG009 |
//! | Name | `names` | EG002/EG003, EG010–EG013 |
//! | Type | `types` | EG014–EG021 |
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
use std::collections::{HashMap, HashSet};

mod context;
mod dims;
mod effects;
mod locate;
mod names;
mod navigate;
mod signals;
mod spans;
mod structure;
mod termination;
mod types;

pub use locate::span_of;
pub use navigate::{SymbolInfo, symbol_at};

/// Run all six analyses over `block`, collecting every finding.
///
/// Returns `Ok(())` for a valid block, or ALL diagnostics found (analysis
/// order: name, type, dimensionality, termination, side-effect, signals;
/// deterministic within each analysis).
///
/// # Errors
///
/// Returns the non-empty list of [`GalecError`] findings (codes EG001–EG040)
/// when the block violates structural closure or any semantic analysis.
pub fn validate(block: &Block) -> Result<(), Vec<GalecError>> {
    let ctx = context::BlockContext::new(block);
    let mut diags = Vec::new();
    structure::check(&ctx, &mut diags);
    names::check(&ctx, &mut diags);
    let expression_types = types::check(&ctx, &mut diags);
    dims::check(&ctx, &mut diags);
    termination::check(&ctx, &mut diags);
    effects::check(&ctx, &mut diags);
    signals::check(&ctx, &expression_types, &mut diags);
    if diags.is_empty() { Ok(()) } else { Err(diags) }
}

/// Close every generated signal clause before whole-block validation.
///
/// The generated projection starts with empty user-function clauses. The
/// language requires an acyclic call graph, so visiting functions callee-first
/// closes every clause exactly once. Expression types are invariant while only
/// those clauses change; proving them once avoids a quadratic sequence of
/// identical whole-block checks. Parsed source still goes through [`validate`]
/// unchanged, so an authored incorrect clause remains a diagnostic.
pub(crate) fn derive_generated_signal_clauses(
    block: &mut Block,
) -> [Vec<crate::ast::PredefinedSignal>; 3] {
    let expression_types = {
        let ctx = context::BlockContext::new(block);
        let mut diagnostics = Vec::new();
        types::check(&ctx, &mut diagnostics)
    };
    let order = generated_function_callee_first(block);
    for function_index in order {
        let clause = {
            let ctx = context::BlockContext::new(block);
            signals::user_signal_clause(&ctx, &expression_types, function_index)
        };
        block
            .protected_functions
            .iter_mut()
            .chain(&mut block.public_functions)
            .nth(function_index)
            .expect("generated function order resolves")
            .signals = clause;
    }
    let ctx = context::BlockContext::new(block);
    signals::method_signal_clauses(&ctx, &expression_types)
}

fn generated_function_callee_first(block: &Block) -> Vec<usize> {
    let ctx = context::BlockContext::new(block);
    let graph = termination::user_call_graph(&ctx);
    let names = block
        .protected_functions
        .iter()
        .chain(&block.public_functions)
        .map(|function| context::lexeme(&function.name))
        .collect::<Vec<_>>();
    let mut traversal = SignalOrder {
        names: &names,
        graph: &graph,
        visiting: HashSet::new(),
        visited: HashSet::new(),
        order: Vec::with_capacity(names.len()),
    };
    for index in 0..names.len() {
        traversal.append(index);
    }
    traversal.order
}

struct SignalOrder<'a> {
    names: &'a [String],
    graph: &'a HashMap<String, Vec<String>>,
    visiting: HashSet<usize>,
    visited: HashSet<usize>,
    order: Vec<usize>,
}

impl SignalOrder<'_> {
    fn append(&mut self, index: usize) {
        if self.visited.contains(&index) || !self.visiting.insert(index) {
            return;
        }
        let callee_indices = self
            .graph
            .get(&self.names[index])
            .into_iter()
            .flatten()
            .filter_map(|callee| self.names.iter().position(|name| name == callee))
            .collect::<Vec<_>>();
        for callee_index in callee_indices {
            self.append(callee_index);
        }
        self.visiting.remove(&index);
        if self.visited.insert(index) {
            self.order.push(index);
        }
    }
}
