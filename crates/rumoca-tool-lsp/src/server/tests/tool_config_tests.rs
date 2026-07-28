//! SPEC_0018: the editor honors `.rumoca_fmt.toml` / `.rumoca_lint.toml`.
//!
//! Before this, `formatting` hardcoded `FormatOptions::default()` (the Dymola
//! profile) and diagnostics hardcoded `LintOptions::default()`, so a project
//! that configured the tools got one result from `rumoca fmt`/`rumoca lint` and
//! a different one in the editor.

use super::*;

/// A model whose formatting differs between the Dymola and canonical profiles:
/// canonical normalizes the binding spacing and the structural indentation.
const UNFORMATTED_MODEL: &str = "model M\n      Real x=1;\nequation\n      x=1;\nend M;\n";

async fn open_model(server: &ModelicaLanguageServer, path: &Path, source: &str) -> Url {
    let uri = Url::from_file_path(path).expect("file uri");
    std::fs::write(path, source).expect("write model");
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

fn formatting_params(uri: &Url, tab_size: u32) -> DocumentFormattingParams {
    DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions {
            tab_size,
            insert_spaces: true,
            ..FormattingOptions::default()
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
    }
}

async fn format_document(
    server: &ModelicaLanguageServer,
    uri: &Url,
    tab_size: u32,
) -> Option<String> {
    server
        .formatting(formatting_params(uri, tab_size))
        .await
        .expect("formatting should succeed")
        .map(|edits| edits[0].new_text.clone())
}

#[test]
fn formatting_uses_rumoca_fmt_toml_profile() {
    let temp = new_temp_dir("tool-config-fmt-profile");
    run_async_test(async {
        std::fs::write(temp.join(".rumoca_fmt.toml"), "profile = \"canonical\"\n")
            .expect("write fmt config");
        let service = new_test_service();
        let server = service.inner();
        let uri = open_model(server, &temp.join("m.mo"), UNFORMATTED_MODEL).await;

        let formatted = format_document(server, &uri, 2)
            .await
            .expect("canonical profile reformats this source");
        let expected = rumoca_tool_fmt::format(
            UNFORMATTED_MODEL,
            &rumoca_tool_fmt::FormatOptions::for_profile(rumoca_tool_fmt::FormatProfile::Canonical),
        )
        .expect("canonical format");
        assert_eq!(formatted, expected);
        assert_ne!(
            formatted,
            rumoca_tool_fmt::format(
                UNFORMATTED_MODEL,
                &rumoca_tool_fmt::FormatOptions::default()
            )
            .expect("dymola format"),
            "the config profile must actually differ from the default"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn formatting_client_tab_size_is_honored_when_config_is_silent() {
    let temp = new_temp_dir("tool-config-fmt-tabsize");
    run_async_test(async {
        // The config sets only the profile, so `indent_size` stays open for the
        // client's `tab_size` to fill in.
        std::fs::write(
            temp.join(".rumoca_fmt.toml"),
            "profile = \"canonical\"\nnormalize_indentation = true\n",
        )
        .expect("write fmt config");
        let service = new_test_service();
        let server = service.inner();
        let uri = open_model(server, &temp.join("m.mo"), UNFORMATTED_MODEL).await;

        let formatted = format_document(server, &uri, 4)
            .await
            .expect("formatting should produce an edit");
        assert!(
            formatted.contains("\n    Real x = 1;"),
            "client tab_size=4 should indent by four spaces: {formatted:?}"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn formatting_config_profile_overrides_client_options() {
    let temp = new_temp_dir("tool-config-fmt-precedence");
    run_async_test(async {
        // SPEC_0018 precedence: the project's config beats the editor's own
        // preferences, so a config-pinned indent survives a differing tab_size.
        std::fs::write(
            temp.join(".rumoca_fmt.toml"),
            "profile = \"canonical\"\nnormalize_indentation = true\nindent_size = 2\n",
        )
        .expect("write fmt config");
        let service = new_test_service();
        let server = service.inner();
        let uri = open_model(server, &temp.join("m.mo"), UNFORMATTED_MODEL).await;

        let formatted = format_document(server, &uri, 8)
            .await
            .expect("formatting should produce an edit");
        assert!(
            formatted.contains("\n  Real x = 1;"),
            "config indent-size must beat the client tab_size: {formatted:?}"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn tool_config_cache_reloads_after_config_file_change() {
    let temp = new_temp_dir("tool-config-reload");
    run_async_test(async {
        let config_path = temp.join(".rumoca_fmt.toml");
        std::fs::write(&config_path, "profile = \"canonical\"\n").expect("write fmt config");
        let service = new_test_service();
        let server = service.inner();
        let uri = open_model(server, &temp.join("m.mo"), UNFORMATTED_MODEL).await;

        let canonical = format_document(server, &uri, 2)
            .await
            .expect("canonical profile reformats this source");

        std::fs::write(&config_path, "profile = \"dymola\"\n").expect("rewrite fmt config");
        let config_uri = Url::from_file_path(&config_path).expect("config uri");
        server
            .did_save(DidSaveTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: config_uri },
                text: None,
            })
            .await;

        let dymola = format_document(server, &uri, 2).await;
        assert_ne!(
            dymola,
            Some(canonical),
            "saving a new profile must take effect on the next format"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn malformed_formatter_config_fails_request_and_is_not_cached() {
    let temp = new_temp_dir("tool-config-malformed");
    run_async_test(async {
        let config_path = temp.join(".rumoca_fmt.toml");
        std::fs::write(&config_path, "profile = [\n").expect("write malformed fmt config");
        let service = new_test_service();
        let server = service.inner();
        let uri = open_model(server, &temp.join("m.mo"), UNFORMATTED_MODEL).await;

        let error = server
            .formatting(formatting_params(&uri, 2))
            .await
            .expect_err("malformed formatter config must fail the formatting request");
        assert!(
            error.to_string().contains("formatter configuration"),
            "{error:?}"
        );
        assert!(
            server.tool_config_cache.read().await.is_empty(),
            "failed config loads must not cache defaults"
        );

        std::fs::write(&config_path, "profile = \"canonical\"\n").expect("repair fmt config");
        let formatted = server
            .formatting(formatting_params(&uri, 2))
            .await
            .expect("repaired config should load")
            .expect("canonical profile should produce an edit");
        assert!(!formatted.is_empty());
        assert_eq!(server.tool_config_cache.read().await.len(), 1);
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn diagnostics_respect_rumoca_lint_toml_disabled_rules() {
    let temp = new_temp_dir("tool-config-lint");
    run_async_test(async {
        // A lowercase class name violates the naming-convention rule.
        let source = "model badName\n  Real x;\nequation\n  x = 1;\nend badName;\n";
        let path = temp.join("badname.mo");
        std::fs::write(&path, source).expect("write model");
        let uri = Url::from_file_path(&path).expect("file uri");
        let file_name = session_document_uri_key(&uri);

        let service = new_test_service();
        let server = service.inner();
        let before = {
            let options = server
                .tool_options_for_document(&file_name)
                .await
                .expect("default tool options");
            rumoca_tool_lint::lint(source, &file_name, &options.lint)
        };
        assert!(
            before.iter().any(|m| m.rule == "naming-convention"),
            "the fixture must trip naming-convention by default: {before:?}"
        );

        std::fs::write(
            temp.join(".rumoca_lint.toml"),
            "disabled_rules = [\"naming-convention\"]\n",
        )
        .expect("write lint config");
        let config_uri =
            Url::from_file_path(temp.join(".rumoca_lint.toml")).expect("lint config uri");
        server
            .did_save(DidSaveTextDocumentParams {
                text_document: TextDocumentIdentifier { uri: config_uri },
                text: None,
            })
            .await;

        let options = server
            .tool_options_for_document(&file_name)
            .await
            .expect("valid lint config");
        assert!(
            options
                .lint
                .disabled_rules
                .iter()
                .any(|rule| rule == "naming-convention"),
            "the server must pick up the project lint config"
        );
        let after = rumoca_tool_lint::lint(source, &file_name, &options.lint);
        assert!(
            !after.iter().any(|m| m.rule == "naming-convention"),
            "the disabled rule must not fire: {after:?}"
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}

#[test]
fn formatting_replacement_range_ends_at_utf16_document_end() {
    let temp = new_temp_dir("tool-config-fmt-range");
    run_async_test(async {
        // The final line carries an astral character, so its byte length and
        // UTF-16 length differ.
        let source = "model M\n      Real x=1;\nend M;\n// 𝔸\n";
        std::fs::write(temp.join(".rumoca_fmt.toml"), "profile = \"canonical\"\n")
            .expect("write fmt config");
        let service = new_test_service();
        let server = service.inner();
        let uri = open_model(server, &temp.join("m.mo"), source).await;

        let edits = server
            .formatting(formatting_params(&uri, 2))
            .await
            .expect("formatting should succeed")
            .expect("formatting should produce an edit");
        assert_eq!(
            edits[0].range.end,
            rumoca_lsp_position::byte_offset_to_position(source, source.len())
        );
    });
    let _ = std::fs::remove_dir_all(&temp);
}
