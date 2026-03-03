//! Go-to-definition handler for Modelica files.

use std::path::{Path, PathBuf};

use lsp_types::{GotoDefinitionResponse, Location, Position, Range, Url};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

use crate::helpers::{get_word_at_position, resolve_at_position};

/// Handle go-to-definition request.
pub fn handle_goto_definition(
    ast: &ast::StoredDefinition,
    tree: Option<&ast::ClassTree>,
    source: &str,
    uri: &Url,
    line: u32,
    character: u32,
) -> Option<GotoDefinitionResponse> {
    let position = Position { line, character };
    let word = get_word_at_position(source, position)?;

    // Try resolved tree first for def_id-based lookup
    if let Some(tree) = tree {
        if let Some(response) = def_id_lookup(ast, tree, &word, uri) {
            return Some(response);
        }
        if let Some(response) = import_lookup(ast, tree, &word, uri) {
            return Some(response);
        }
    }

    // Fallback: scan AST for matching declarations
    ast_lookup(ast, &word, uri)
}

fn def_id_lookup(
    ast: &ast::StoredDefinition,
    tree: &ast::ClassTree,
    name: &str,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    // Find the def_id for this name using the resolved tree
    let def_id = resolve_at_position(ast, tree, name)?;

    goto_response_for_def_id(tree, def_id, uri)
}

fn import_lookup(
    ast: &ast::StoredDefinition,
    tree: &ast::ClassTree,
    name: &str,
    fallback_uri: &Url,
) -> Option<GotoDefinitionResponse> {
    for class in ast.classes.values() {
        if let Some(response) = import_lookup_in_class(class, tree, name, fallback_uri) {
            return Some(response);
        }
    }
    None
}

fn import_lookup_in_class(
    class: &ast::ClassDef,
    tree: &ast::ClassTree,
    name: &str,
    fallback_uri: &Url,
) -> Option<GotoDefinitionResponse> {
    for import in &class.imports {
        if let Some(def_id) = imported_def_id(import, tree, name)
            && let Some(response) = goto_response_for_def_id(tree, def_id, fallback_uri)
        {
            return Some(response);
        }
    }
    for nested in class.classes.values() {
        if let Some(response) = import_lookup_in_class(nested, tree, name, fallback_uri) {
            return Some(response);
        }
    }
    None
}

fn imported_def_id(import: &ast::Import, tree: &ast::ClassTree, name: &str) -> Option<DefId> {
    match import {
        ast::Import::Qualified { path, .. } => {
            let last = path.name.last()?.text.as_ref();
            if last == name {
                tree.get_def_id_by_name(&path.to_string())
            } else {
                None
            }
        }
        ast::Import::Renamed { alias, path, .. } => {
            if alias.text.as_ref() == name {
                tree.get_def_id_by_name(&path.to_string())
            } else {
                None
            }
        }
        ast::Import::Unqualified { path, .. } => {
            let qualified = format!("{}.{}", path, name);
            tree.get_def_id_by_name(&qualified)
        }
        ast::Import::Selective { path, names, .. } => {
            let matched = names.iter().find(|token| token.text.as_ref() == name)?;
            let qualified = format!("{}.{}", path, matched.text);
            tree.get_def_id_by_name(&qualified)
        }
    }
}

fn goto_response_for_def_id(
    tree: &ast::ClassTree,
    def_id: DefId,
    fallback_uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let class = tree.get_class_by_def_id(def_id)?;
    let loc = &class.name.location;
    let target_uri = target_uri_for_location(loc, fallback_uri);
    let range = Range {
        start: Position::new(
            loc.start_line.saturating_sub(1),
            loc.start_column.saturating_sub(1),
        ),
        end: Position::new(
            loc.end_line.saturating_sub(1),
            loc.end_column.saturating_sub(1),
        ),
    };
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: target_uri,
        range,
    }))
}

fn target_uri_for_location(loc: &ast::Location, fallback_uri: &Url) -> Url {
    if loc.file_name.is_empty() {
        return fallback_uri.clone();
    }
    let path = Path::new(loc.file_name.as_str());
    if path.is_absolute()
        && let Some(uri) = url_from_file_path(path)
    {
        return uri;
    }
    if let Some(base_path) = file_path_from_url(fallback_uri)
        && let Some(parent) = base_path.parent()
    {
        let candidate = parent.join(path);
        if let Some(uri) = url_from_file_path(candidate) {
            return uri;
        }
    }
    fallback_uri.clone()
}

#[cfg(not(target_arch = "wasm32"))]
fn file_path_from_url(uri: &Url) -> Option<PathBuf> {
    uri.to_file_path().ok()
}

#[cfg(target_arch = "wasm32")]
fn file_path_from_url(uri: &Url) -> Option<PathBuf> {
    if uri.scheme() != "file" {
        return None;
    }
    let path = uri.path();
    if path.is_empty() {
        return None;
    }
    Some(PathBuf::from(path))
}

#[cfg(not(target_arch = "wasm32"))]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    Url::from_file_path(path).ok()
}

#[cfg(target_arch = "wasm32")]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    let raw = path.as_ref().to_string_lossy();
    if raw.is_empty() {
        return None;
    }
    let mut normalized = raw.replace('\\', "/");
    if !normalized.starts_with('/') {
        normalized.insert(0, '/');
    }
    Url::parse(&format!("file://{}", normalized)).ok()
}

fn ast_lookup(
    ast: &ast::StoredDefinition,
    name: &str,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    // Search for component declarations with this name
    for (_, class) in &ast.classes {
        if let Some(loc) = find_declaration_in_class(class, name) {
            return Some(GotoDefinitionResponse::Scalar(loc.with_uri(uri)));
        }
    }

    // Search for class definitions with this name
    for (class_name, class) in &ast.classes {
        if class_name == name {
            let loc = &class.name.location;
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position::new(
                        loc.start_line.saturating_sub(1),
                        loc.start_column.saturating_sub(1),
                    ),
                    end: Position::new(
                        loc.end_line.saturating_sub(1),
                        loc.end_column.saturating_sub(1),
                    ),
                },
            }));
        }
        if let Some(loc) = find_class_in_class(class, name) {
            return Some(GotoDefinitionResponse::Scalar(loc.with_uri(uri)));
        }
    }

    None
}

struct FoundLocation {
    range: Range,
}

impl FoundLocation {
    fn with_uri(self, uri: &Url) -> Location {
        Location {
            uri: uri.clone(),
            range: self.range,
        }
    }
}

fn find_declaration_in_class(class: &rumoca_ir_ast::ClassDef, name: &str) -> Option<FoundLocation> {
    if let Some(comp) = class.components.get(name) {
        let loc = &comp.name_token.location;
        return Some(FoundLocation {
            range: Range {
                start: Position::new(
                    loc.start_line.saturating_sub(1),
                    loc.start_column.saturating_sub(1),
                ),
                end: Position::new(
                    loc.end_line.saturating_sub(1),
                    loc.end_column.saturating_sub(1),
                ),
            },
        });
    }

    // Search nested classes
    for (_, nested) in &class.classes {
        if let Some(loc) = find_declaration_in_class(nested, name) {
            return Some(loc);
        }
    }

    None
}

fn find_class_in_class(class: &rumoca_ir_ast::ClassDef, name: &str) -> Option<FoundLocation> {
    for (nested_name, nested) in &class.classes {
        if nested_name == name {
            let loc = &nested.name.location;
            return Some(FoundLocation {
                range: Range {
                    start: Position::new(
                        loc.start_line.saturating_sub(1),
                        loc.start_column.saturating_sub(1),
                    ),
                    end: Position::new(
                        loc.end_line.saturating_sub(1),
                        loc.end_column.saturating_sub(1),
                    ),
                },
            });
        }
        if let Some(loc) = find_class_in_class(nested, name) {
            return Some(loc);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn goto_definition_resolves_qualified_import_symbol() {
        let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  PID pid(k=100);
equation
  pid.u = 1;
end Ball;
"#;
        let library = r#"
package Modelica
  package Blocks
    package Continuous
      block PID
        Real u;
        Real y;
      equation
        y = u;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;

        let base = std::env::temp_dir().join("rumoca_lsp_goto_definition_test");
        let ball_path = base.join("ball.mo");
        let modelica_path = base.join("Modelica.mo");
        let ball_uri_path = ball_path.to_string_lossy().to_string();
        let modelica_uri_path = modelica_path.to_string_lossy().to_string();

        let mut session = rumoca_session::Session::default();
        session.update_document(&ball_uri_path, source);
        session.update_document(&modelica_uri_path, library);
        let resolved = session.resolved().expect("resolved");
        let doc = session
            .get_document(&ball_uri_path)
            .expect("main document present");
        let ast = doc.parsed.as_ref().expect("main doc parsed");
        let uri = Url::from_file_path(&ball_path).expect("uri");
        let import_line = source.lines().nth(2).expect("import line");
        let char_pos = import_line.find("PID").expect("PID token") as u32 + 1;

        let result = handle_goto_definition(ast, Some(&resolved.0), source, &uri, 2, char_pos);
        match result {
            Some(GotoDefinitionResponse::Scalar(location)) => {
                let expected = Url::from_file_path(&modelica_path)
                    .expect("expected uri")
                    .to_string();
                assert!(
                    location.uri.to_string() == expected,
                    "unexpected target uri: {}",
                    location.uri
                );
            }
            other => panic!("expected scalar goto response, got: {other:?}"),
        }
    }
}
