use super::ModelDiagnostics;
use rumoca_core::{Diagnostic as CommonDiagnostic, SourceMap};
use rumoca_ir_ast as ast;

pub(super) fn global_resolution_failure_diagnostics(
    source_map: SourceMap,
    diagnostics: Vec<CommonDiagnostic>,
) -> ModelDiagnostics {
    ModelDiagnostics {
        diagnostics,
        source_map: Some(source_map),
        global_resolution_failure: true,
    }
}

pub(super) fn model_diagnostics_for_tree(
    tree: &ast::ClassTree,
    diagnostics: Vec<CommonDiagnostic>,
) -> ModelDiagnostics {
    ModelDiagnostics {
        diagnostics,
        source_map: Some(tree.source_map.clone()),
        global_resolution_failure: false,
    }
}

pub(super) fn synthesized_inner_warning(synthesized_inners: &[String]) -> Option<CommonDiagnostic> {
    if synthesized_inners.is_empty() {
        return None;
    }
    Some(
        CommonDiagnostic::warning(format!(
            "outer without matching inner detected ({}); synthesizing root-level inner declaration(s)",
            synthesized_inners.join(", ")
        ))
        .with_code("EI013")
        .with_note("MLS §5.4 permits default inner synthesis when no matching inner is present."),
    )
}
