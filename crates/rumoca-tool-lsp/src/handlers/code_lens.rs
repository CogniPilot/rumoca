//! Code lens handler for Modelica files.

use lsp_types::{CodeLens, Url};
use rumoca_compile::parsing::ast;
use serde_json::json;

use super::workspace_symbols::collect_model_names;

/// Handle code lens request - return unresolved lenses for model declarations.
///
/// `source` is the text `ast` was parsed from; lens ranges are UTF-16 columns.
pub fn handle_code_lens(ast: &ast::StoredDefinition, source: &str, uri: &Url) -> Vec<CodeLens> {
    let model_names = collect_model_names(ast, source);
    model_names
        .into_iter()
        .map(|(name, range)| CodeLens {
            range,
            command: None,
            data: Some(json!({
                "uri": uri.as_str(),
                "modelName": name,
            })),
        })
        .collect()
}
