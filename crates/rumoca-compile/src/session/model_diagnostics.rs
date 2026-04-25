use super::ModelDiagnostics;
use rumoca_core::{Diagnostic as CommonDiagnostic, PrimaryLabel, SourceMap};
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

pub(super) fn merge_model_diagnostics(
    mut lhs: ModelDiagnostics,
    rhs: ModelDiagnostics,
) -> ModelDiagnostics {
    lhs.diagnostics.extend(rhs.diagnostics);
    lhs.global_resolution_failure |= rhs.global_resolution_failure;
    if lhs.source_map.is_none() {
        lhs.source_map = rhs.source_map;
    }
    lhs
}

pub(super) fn synthesized_inner_warning(
    synthesized_inners: &[String],
    primary_label: PrimaryLabel,
) -> Option<CommonDiagnostic> {
    if synthesized_inners.is_empty() {
        return None;
    }
    Some(
        CommonDiagnostic::warning(
            "EI013",
            format!(
                "outer without matching inner detected ({}); synthesizing root-level inner declaration(s)",
                synthesized_inners.join(", ")
            ),
            primary_label,
        )
        .with_note("MLS §5.4 permits default inner synthesis when no matching inner is present."),
    )
}
