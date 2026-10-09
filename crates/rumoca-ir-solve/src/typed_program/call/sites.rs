//! Static call-site inventory of a pure-call table.
//!
//! The number of `Call` operations an owner's body issues for one callee,
//! counted through every nested region, is a construction fact of the checked
//! program (SPEC_0040 SOLVE-C73): a callee evaluated once per authored
//! occurrence has one site per occurrence. Executors that run every site (the
//! Cranelift and WASM adapters) evaluate it once per executed site, so this
//! count is the cost owner a report can pin without running the program.

use super::{SolvePureCallOwnerId, SolvePureCallTable};
use crate::typed_program::{SolveOperation, TypedProgram};
use rumoca_core::Span;

/// The `Call` operations one caller owner's body issues for one callee.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SolveCallSiteCount {
    pub caller: SolvePureCallOwnerId,
    pub callee: SolvePureCallOwnerId,
    /// Source span of the callee's call occurrence.
    pub callee_provenance: Span,
    /// Static `Call` operations in the caller's body, regions included.
    pub sites: u32,
}

impl SolvePureCallTable {
    /// Every (caller, callee) pair with at least one call site, in caller
    /// then callee order.
    #[must_use]
    pub fn call_site_counts(&self) -> Vec<SolveCallSiteCount> {
        let mut counts = Vec::new();
        for caller in self.owners() {
            let mut sites = std::collections::BTreeMap::new();
            count_sites(caller.body(), &mut sites);
            counts.extend(sites.into_iter().filter_map(|(callee, sites)| {
                let callee = self.owners().get(callee as usize)?;
                Some(SolveCallSiteCount {
                    caller: caller.id(),
                    callee: callee.id(),
                    callee_provenance: callee.provenance(),
                    sites,
                })
            }));
        }
        counts
    }
}

fn count_sites(program: &TypedProgram, sites: &mut std::collections::BTreeMap<u32, u32>) {
    for spanned in program.operations() {
        match spanned.operation() {
            SolveOperation::Call { owner, .. } => *sites.entry(owner.index()).or_default() += 1,
            SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                count_sites(if_true.body(), sites);
                count_sites(if_false.body(), sites);
            }
            SolveOperation::Map { body, .. } => count_sites(body.body(), sites),
            SolveOperation::Fold {
                transition,
                continuation,
                ..
            } => {
                count_sites(transition.body(), sites);
                if let Some(continuation) = continuation {
                    count_sites(continuation.body(), sites);
                }
            }
            _ => {}
        }
    }
}
