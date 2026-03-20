//! Code lens handler for Modelica files.

use lsp_types::{CodeLens, Url};
use rumoca_session::parsing::ast;
use serde_json::json;

use super::workspace_symbols::collect_model_names;

/// Handle code lens request - return unresolved lenses for model declarations.
pub fn handle_code_lens(ast: &ast::StoredDefinition, uri: &Url) -> Vec<CodeLens> {
    let model_names = collect_model_names(ast);
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
