use rumoca_core::SourceMap;
use rumoca_ir_ast::ClassDefIndex;
use rumoca_phase_resolve::ResolvedTree;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

use crate::codegen_target::CheckedTargetArtifactStem;

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
    model_name: String,
    canonical_model_identity: CanonicalModelIdentity,
    result: CompilationResult,
    resolved: ResolvedTree,
}

/// Resolve-issued qualified model identity and its sole portable artifact stem.
#[derive(Debug)]
pub(crate) struct CanonicalModelIdentity {
    components: Box<[Box<str>]>,
    artifact_stem: Arc<CheckedTargetArtifactStem>,
}

impl CanonicalModelIdentity {
    fn construct(requested_model: &str, resolved: &ResolvedTree) -> Option<Self> {
        let index = ClassDefIndex::from_tree(resolved.inner());
        let def_id = index.def_id_by_qualified_name(requested_model)?;
        let components: Option<Box<[Box<str>]>> = index
            .def_ancestry(def_id)
            .into_iter()
            .map(|ancestor| {
                let component = index.local_name(ancestor)?;
                (!component.is_empty()).then(|| Box::<str>::from(component))
            })
            .collect();
        let components = components?;
        if components.is_empty() {
            return None;
        }
        let artifact_stem = Arc::new(CheckedTargetArtifactStem::from_model_components(
            &components,
        ));
        Some(Self {
            components,
            artifact_stem,
        })
    }

    pub(crate) fn components(&self) -> &[Box<str>] {
        &self.components
    }

    pub(crate) fn artifact_stem(&self) -> &Arc<CheckedTargetArtifactStem> {
        &self.artifact_stem
    }
}

impl StrictCompilation {
    fn new(model_name: String, result: CompilationResult, resolved: ResolvedTree) -> Option<Self> {
        let canonical_model_identity = CanonicalModelIdentity::construct(&model_name, &resolved)?;
        Some(Self {
            model_name,
            canonical_model_identity,
            result,
            resolved,
        })
    }

    /// Exact requested model identity compiled into this result.
    #[must_use]
    pub fn model_name(&self) -> &str {
        &self.model_name
    }

    /// Borrow the compilation result.
    pub fn result(&self) -> &CompilationResult {
        &self.result
    }

    /// Borrow the resolved target closure used to produce the result.
    pub fn resolved(&self) -> &ResolvedTree {
        &self.resolved
    }

    pub(crate) fn canonical_model_identity(&self) -> &CanonicalModelIdentity {
        &self.canonical_model_identity
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
                match StrictCompilation::new(self.requested_model.clone(), *result, resolved) {
                    Some(compilation) => Ok(compilation),
                    None => {
                        self.failures.push(ModelFailureDiagnostic {
                            model_name: self.requested_model.clone(),
                            phase: None,
                            error_code: None,
                            error: "strict compilation lost the Resolve-issued qualified model identity"
                                .to_string(),
                            primary_label: None,
                            secondary_labels: Vec::new(),
                            notes: vec![
                                "artifact construction requires the exact nonempty resolved component sequence"
                                    .to_string(),
                            ],
                        });
                        Err(Box::new(self))
                    }
                }
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
