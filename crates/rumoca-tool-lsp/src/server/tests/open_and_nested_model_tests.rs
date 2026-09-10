//! Open diagnostics and package lenses retain compiler-owned results (SPEC_0008).

use super::*;
use tower_service::Service;

#[test]
fn opening_an_unbalanced_model_publishes_semantic_diagnostics_without_saving() {
    let temp = new_temp_dir("open-semantic-diagnostics");
    run_async_test(async {
        let (mut service, socket) = LspService::new(ModelicaLanguageServer::new);
        let (sender, mut receiver) = tokio::sync::mpsc::unbounded_channel();
        let mut diagnostics = socket.filter(|request| {
            std::future::ready(request.method() == "textDocument/publishDiagnostics")
        });
        tokio::spawn(async move {
            while let Some(request) = diagnostics.next().await {
                sender
                    .send(request)
                    .expect("diagnostic receiver remains live");
            }
        });
        let initialization = service
            .call(
                tower_lsp::jsonrpc::Request::build("initialize")
                    .id(1)
                    .params(serde_json::json!({"capabilities": {}}))
                    .finish(),
            )
            .await
            .expect("initialize request")
            .expect("initialize response");
        assert!(initialization.is_ok(), "LSP initialization must succeed");
        let uri = Url::from_file_path(temp.join("Unbalanced.mo")).expect("file URI");
        service
            .inner()
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text:
                        "model Unbalanced Real x; Real y; equation x=1; y=2; x+y=5; end Unbalanced;"
                            .to_string(),
                },
            })
            .await;
        let request = tokio::time::timeout(std::time::Duration::from_secs(10), receiver.recv())
            .await
            .expect("opening a model must publish diagnostics")
            .expect("diagnostic request");
        let params: PublishDiagnosticsParams = serde_json::from_value(
            request
                .params()
                .expect("published diagnostic parameters")
                .clone(),
        )
        .expect("valid LSP diagnostics");
        assert_eq!(params.uri, uri);
        assert!(
            params.diagnostics.iter().any(|diagnostic| {
                diagnostic
                    .message
                    .contains("unbalanced model: 3 equations, 2 unknowns")
            }),
            "opening must expose the compiler's balance error: {:?}",
            params.diagnostics
        );
    });
    std::fs::remove_dir_all(temp).expect("remove fixture");
}

#[test]
fn nested_lenses_resolve_within_qualified_names_and_utf16_ranges() {
    let temp = new_temp_dir("nested-model-lenses");
    run_async_test(async {
        let source = "within Examples;\npackage P\n  package Left\n    /* 𝔸 */ model M Real x=1; end M;\n  end Left;\n  package Right\n    model M Real x=1; Real y=2; end M;\n  end Right;\n  block B Real z=3; end B;\nend P;\n";
        let uri = Url::from_file_path(temp.join("P.mo")).expect("file URI");
        let service = new_test_service();
        let server = service.inner();
        server
            .session
            .write()
            .await
            .update_document(&session_document_uri_key(&uri), source);
        let lenses = server
            .code_lens(CodeLensParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .await
            .expect("code lens request")
            .expect("nested model lenses");
        let expected = [
            ("Examples.P.Left.M", "Balanced (1 unknowns, 1 eqs)"),
            ("Examples.P.Right.M", "Balanced (2 unknowns, 2 eqs)"),
            ("Examples.P.B", "Balanced (1 unknowns, 1 eqs)"),
        ];
        assert_eq!(lenses.len(), expected.len());
        let offset = source.find("model M").expect("first nested model") + "model ".len();
        assert_eq!(
            lenses[0].range.start,
            crate::text_position::byte_offset_to_position(source, offset)
        );
        for (lens, (name, title)) in lenses.into_iter().zip(expected) {
            assert_eq!(lens.data.as_ref().expect("lens data")["modelName"], name);
            let resolved = server
                .code_lens_resolve(lens)
                .await
                .expect("resolve nested lens");
            assert_eq!(
                resolved.command.expect("resolved compile status").title,
                title
            );
        }
    });
    std::fs::remove_dir_all(temp).expect("remove fixture");
}
