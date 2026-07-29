use rumoca_core::SourceMap;
use rumoca_phase_resolve::ResolvedTree;
use serde::{Deserialize, Serialize};

use super::{
    CompilationResult, CompilationSummary, ModelFailureDiagnostic, PhaseResult,
    format_strict_failure_summary, requested_missing_result_message,
};

/// Report type from strict-reachable-with-recovery compilation.
///
/// The requested model remains strict: it must compile successfully for callers
/// to treat the compile as successful. Other related models are still compiled
/// so additional diagnostics can be surfaced to the user.
#[derive(Debug, Serialize, Deserialize)]
pub struct StrictCompileReport {
    pub requested_model: String,
    pub requested_result: Option<PhaseResult>,
    pub summary: CompilationSummary,
    pub failures: Vec<ModelFailureDiagnostic>,
    pub source_map: Option<SourceMap>,
}

/// Successful strict compilation paired with its exact resolved target closure.
#[derive(Debug)]
pub struct StrictCompilation {
    result: CompilationResult,
    resolved: ResolvedTree,
}

impl StrictCompilation {
    fn new(result: CompilationResult, resolved: ResolvedTree) -> Self {
        Self { result, resolved }
    }

    /// Borrow the compilation result.
    pub fn result(&self) -> &CompilationResult {
        &self.result
    }

    /// Borrow the resolved target closure used to produce the result.
    pub fn resolved(&self) -> &ResolvedTree {
        &self.resolved
    }

    /// Consume the proof-bearing compilation into its result and Resolve proof.
    pub fn into_parts(self) -> (CompilationResult, ResolvedTree) {
        (self.result, self.resolved)
    }
}

impl StrictCompileReport {
    pub(super) fn into_compilation(
        mut self,
        resolved: ResolvedTree,
    ) -> std::result::Result<StrictCompilation, Box<Self>> {
        let requested_result = self.requested_result.take();
        match requested_result {
            Some(PhaseResult::Success(result)) if self.failures.is_empty() => {
                Ok(StrictCompilation::new(*result, resolved))
            }
            requested_result => {
                self.requested_result = requested_result;
                Err(Box::new(self))
            }
        }
    }

    /// Returns true when strict compile succeeded for the requested closure.
    pub fn requested_succeeded(&self) -> bool {
        matches!(self.requested_result, Some(PhaseResult::Success(_))) && self.failures.is_empty()
    }

    /// Build a concise failure summary for user-facing diagnostics.
    pub fn failure_summary(&self, max_related: usize) -> String {
        let requested = match &self.requested_result {
            Some(PhaseResult::Success(_)) => {
                format!("{} compiled successfully", self.requested_model)
            }
            Some(PhaseResult::NeedsInner { missing_inners, .. }) => format!(
                "{} requires inner declarations: {}",
                self.requested_model,
                missing_inners.join(", ")
            ),
            Some(PhaseResult::Failed { phase, error, .. }) => {
                format!("{} failed in {}: {}", self.requested_model, phase, error)
            }
            None => requested_missing_result_message(&self.requested_model, &self.failures),
        };

        format_strict_failure_summary(
            &self.requested_model,
            requested,
            &self.failures,
            max_related,
        )
    }
}
