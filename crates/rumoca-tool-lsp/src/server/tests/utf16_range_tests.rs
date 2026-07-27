//! LSP ranges must be measured in UTF-16 code units.
//!
//! `Position.character` is a UTF-16 offset in the default `positionEncoding`,
//! while the Modelica lexer records 1-based Unicode *scalar* columns and token
//! lengths are UTF-8 byte counts. Every range-producing surface therefore has
//! to go through the document's byte span; these tests pin the surfaces that
//! previously published raw lexer columns.

use super::*;
use rumoca_lsp_position::byte_offset_to_position;

/// A line whose UTF-16 columns, character columns and byte columns all differ.
///
/// `𝔸` is 4 UTF-8 bytes / 2 UTF-16 units / 1 Unicode scalar, so a range
/// computed from character columns is one unit short and one computed from
/// bytes is two units long.
const ASTRAL_SOURCE: &str =
    "model M\n  String tag = \"𝔸\"; Real signal;\nequation\n  signal = 1;\nend M;\n";

async fn open_document(server: &ModelicaLanguageServer, path: &Path, source: &str) -> Url {
    let uri = Url::from_file_path(path).expect("file uri");
    std::fs::write(path, source).expect("write document");
    server
        .did_open(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: "modelica".to_string(),
                version: 1,
                text: source.to_string(),
            },
        })
        .await;
    uri
}

#[test]
fn rename_edits_use_utf16_columns_after_astral_character() {
    let temp = new_temp_dir("utf16-rename");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let uri = open_document(server, &temp.join("astral.mo"), ASTRAL_SOURCE).await;

        // Put the cursor on the `signal` declaration, whose UTF-16 column is
        // one greater than its lexer character column.
        let declaration = ASTRAL_SOURCE
            .find("Real signal")
            .expect("declaration present")
            + "Real ".len();
        let position = byte_offset_to_position(ASTRAL_SOURCE, declaration);

        let edit = server
            .rename(RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position,
                },
                new_name: "renamed".to_string(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .await
            .expect("rename should succeed")
            .expect("rename should produce edits");

        let changes = edit.changes.expect("rename edits");
        let edits = changes.get(&uri).expect("edits for the renamed document");
        assert!(!edits.is_empty(), "expected at least one rename edit");
        for text_edit in edits {
            let expected_line = ASTRAL_SOURCE
                .lines()
                .nth(text_edit.range.start.line as usize)
                .expect("edit line exists");
            let byte_column = rumoca_lsp_position::utf16_column_to_byte_column(
                expected_line,
                text_edit.range.start.character,
            );
            assert!(
                expected_line[byte_column..].starts_with("signal"),
                "edit at {:?} should point at `signal`, line was `{expected_line}`",
                text_edit.range
            );
        }
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn document_symbols_use_utf16_columns_after_non_ascii() {
    let temp = new_temp_dir("utf16-document-symbols");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let uri = open_document(server, &temp.join("astral.mo"), ASTRAL_SOURCE).await;

        let response = server
            .document_symbol(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("document symbols should succeed")
            .expect("document symbols should be produced");

        let DocumentSymbolResponse::Nested(symbols) = response else {
            panic!("expected nested document symbols");
        };
        let signal = find_symbol(&symbols, "signal").expect("`signal` outline entry");
        let expected = byte_offset_to_position(
            ASTRAL_SOURCE,
            ASTRAL_SOURCE.find("Real signal").expect("declaration") + "Real ".len(),
        );
        assert_eq!(signal.selection_range.start, expected);
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn document_symbol_group_range_has_nonzero_span() {
    let temp = new_temp_dir("utf16-outline-groups");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        // The parameter sits after an astral character on the same line so its
        // UTF-16 column differs from both its byte column and its lexer
        // character column.
        let source = "model M\n  String tag = \"𝔸\"; parameter Real k = 1;\n  Real x;\nequation\n  x = k;\nend M;\n";
        let uri = open_document(server, &temp.join("groups.mo"), source).await;

        let response = server
            .document_symbol(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("document symbols should succeed")
            .expect("document symbols should be produced");

        let DocumentSymbolResponse::Nested(symbols) = response else {
            panic!("expected nested document symbols");
        };
        // The synthesized "Parameters" group has no token of its own; it now
        // inherits its children's byte span, so its range is derived the same
        // way every other outline node's is.
        let parameters = find_symbol(&symbols, "Parameters").expect("Parameters group");
        let k = find_symbol(&symbols, "k").expect("`k` outline entry");
        assert_eq!(
            parameters.range.start, k.range.start,
            "the group must span its only child"
        );
        assert_eq!(
            parameters.range.start,
            byte_offset_to_position(source, source.find("Real k").expect("declaration"))
        );
        assert!(
            parameters.range.end.line > parameters.range.start.line
                || parameters.range.end.character > parameters.range.start.character,
            "group range must be non-degenerate: {:?}",
            parameters.range
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn workspace_symbols_use_target_file_utf16_columns() {
    let temp = new_temp_dir("utf16-workspace-symbols");
    run_async_test(async {
        let service = new_test_service();
        let server = service.inner();
        let target_source = "model Widget\n  String tag = \"𝔸\"; Real gain;\nend Widget;\n";
        open_document(server, &temp.join("other.mo"), "model Other\nend Other;\n").await;
        let target_uri = open_document(server, &temp.join("widget.mo"), target_source).await;

        let response = server
            .symbol(WorkspaceSymbolParams {
                query: "gain".to_string(),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("workspace symbols should succeed")
            .expect("workspace symbols should be produced");

        let gain = response
            .iter()
            .find(|symbol| symbol.name == "gain" && symbol.location.uri == target_uri)
            .expect("`gain` workspace symbol in the target file");
        let expected = byte_offset_to_position(
            target_source,
            target_source.find("Real gain").expect("declaration") + "Real ".len(),
        );
        assert_eq!(gain.location.range.start, expected);
    });
    let _ = std::fs::remove_dir_all(&temp);
}

fn find_symbol<'a>(symbols: &'a [DocumentSymbol], name: &str) -> Option<&'a DocumentSymbol> {
    for symbol in symbols {
        if symbol.name == name {
            return Some(symbol);
        }
        if let Some(children) = symbol.children.as_deref()
            && let Some(found) = find_symbol(children, name)
        {
            return Some(found);
        }
    }
    None
}
