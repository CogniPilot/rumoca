//! A model whose full strict compile fails while instantiating a component's
//! class must surface that failure in the Problems panel, anchored in the user's
//! open document rather than in the library file the failure points into.

use super::*;
use tower_service::Service;

/// `Chassis` fails to instantiate: its conditional component `wheel` guards on a
/// free (non-parameter) variable, which MLS 4.8 forbids (EI006). The failure's
/// source span points into the library file, so the per-model semantic query
/// cannot anchor it in the open document and, before the strict-compile
/// diagnostics pass, the Problems panel stayed empty even though the compile
/// status lens reported the failure.
const CHASSIS_LIBRARY_SOURCE: &str = "model Chassis\n  Real load;\n  Real wheel if load > 0.5;\nequation\n  load = 1.0;\n  wheel = 2.0;\nend Chassis;\n";

const VEHICLE_SOURCE: &str = "model Vehicle\n  Chassis body;\nend Vehicle;\n";

#[test]
fn instantiate_failure_in_library_class_publishes_diagnostic_anchored_in_open_document() {
    let temp = new_temp_dir("strict-compile-diagnostic");
    run_async_test(async {
        let (mut service, socket) = LspService::new(ModelicaLanguageServer::new);
        let (sender, mut receiver) = tokio::sync::mpsc::unbounded_channel();
        let mut diagnostics = socket.filter(|request| {
            std::future::ready(request.method() == "textDocument/publishDiagnostics")
        });
        tokio::spawn(async move {
            while let Some(request) = diagnostics.next().await {
                let _ = sender.send(request);
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

        // Preload the library class the open document depends on.
        service
            .inner()
            .session
            .write()
            .await
            .add_document("Chassis.mo", CHASSIS_LIBRARY_SOURCE)
            .expect("preload library class");

        let uri = Url::from_file_path(temp.join("Vehicle.mo")).expect("file URI");
        service
            .inner()
            .did_open(DidOpenTextDocumentParams {
                text_document: TextDocumentItem {
                    uri: uri.clone(),
                    language_id: "modelica".to_string(),
                    version: 1,
                    text: VEHICLE_SOURCE.to_string(),
                },
            })
            .await;

        let instantiate_failure = wait_for_diagnostic(&uri, &mut receiver, |diagnostic| {
            diagnostic.code == Some(NumberOrString::String("EI006".to_string()))
        })
        .await;

        assert_eq!(
            instantiate_failure.severity,
            Some(DiagnosticSeverity::ERROR),
            "instantiate failure must be an error diagnostic"
        );
        assert!(
            instantiate_failure
                .message
                .contains("conditional component `wheel` requires parameter expression"),
            "diagnostic must carry the instantiate failure message: {instantiate_failure:?}"
        );
        assert!(
            instantiate_failure.message.contains("origin: Chassis.mo"),
            "diagnostic must record the originating library location: {instantiate_failure:?}"
        );

        // The squiggle must land on the `Vehicle` model name in the open
        // document, not inside the library file the failure points into.
        let model_name_offset = VEHICLE_SOURCE
            .find("Vehicle")
            .expect("model name present in source");
        assert_eq!(
            instantiate_failure.range.start,
            crate::text_position::byte_offset_to_position(VEHICLE_SOURCE, model_name_offset),
            "diagnostic must anchor at the enclosing model name in the user's document"
        );
    });
    std::fs::remove_dir_all(temp).expect("remove fixture");
}

/// Collect the newest diagnostic matching `predicate` for `uri`, tolerating the
/// several diagnostic publishes an open can produce (initial parse, then the
/// compiled result) by waiting until the matching diagnostic appears.
async fn wait_for_diagnostic(
    uri: &Url,
    receiver: &mut tokio::sync::mpsc::UnboundedReceiver<tower_lsp::jsonrpc::Request>,
    predicate: impl Fn(&lsp_types::Diagnostic) -> bool,
) -> lsp_types::Diagnostic {
    let deadline = std::time::Duration::from_secs(20);
    let found = tokio::time::timeout(deadline, async {
        loop {
            let request = receiver
                .recv()
                .await
                .expect("diagnostic publisher remains live");
            let params: PublishDiagnosticsParams = serde_json::from_value(
                request
                    .params()
                    .expect("published diagnostic parameters")
                    .clone(),
            )
            .expect("valid LSP diagnostics");
            if &params.uri != uri {
                continue;
            }
            if let Some(diagnostic) = params.diagnostics.into_iter().find(&predicate) {
                return diagnostic;
            }
        }
    })
    .await;
    found.expect("strict-compile instantiate failure must reach the Problems panel")
}
