use super::*;
use crate::helpers::{location_to_range_in_optional_source, location_to_range_in_source};

pub(super) async fn references(
    server: &ModelicaLanguageServer,
    params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let request_token = server.begin_analysis_request().await;
    let uri = &params.text_document_position.text_document.uri;
    let uri_path = session_document_uri_key(uri);
    let pos = params.text_document_position.position;
    let include_decl = params.context.include_declaration;
    if let Some(snapshot) = server.document_lightweight_snapshot(&uri_path).await
        && let Some(locations) =
            snapshot.navigation_references_query(&uri_path, pos.line, pos.character, include_decl)
    {
        if server.analysis_request_is_stale(request_token).await {
            return Ok(None);
        }
        let mut sources = SnapshotSourceTexts::new(&snapshot);
        return Ok(Some(navigation_locations_to_lsp(
            locations,
            uri,
            &mut sources,
        )));
    }
    Ok(None)
}

pub(super) async fn prepare_rename(
    server: &ModelicaLanguageServer,
    params: TextDocumentPositionParams,
) -> Result<Option<PrepareRenameResponse>> {
    let request_token = server.begin_analysis_request().await;
    let uri = &params.text_document.uri;
    let uri_path = session_document_uri_key(uri);
    let pos = params.position;
    if let Some((document, snapshot)) = server.document_analysis_snapshot(&uri_path).await
        && let Some(location) =
            snapshot.navigation_prepare_rename_query(&uri_path, pos.line, pos.character)
    {
        if server.analysis_request_is_stale(request_token).await {
            return Ok(None);
        }
        return Ok(Some(PrepareRenameResponse::Range(
            location_to_range_in_source(&document.content, &location),
        )));
    }
    Ok(None)
}

pub(super) async fn rename(
    server: &ModelicaLanguageServer,
    params: RenameParams,
) -> Result<Option<WorkspaceEdit>> {
    let request_token = server.begin_analysis_request().await;
    let uri = &params.text_document_position.text_document.uri;
    let uri_path = session_document_uri_key(uri);
    let pos = params.text_document_position.position;
    let new_name = &params.new_name;
    if let Some(snapshot) = server.document_lightweight_snapshot(&uri_path).await
        && let Some(locations) =
            snapshot.navigation_rename_locations_query(&uri_path, pos.line, pos.character)
    {
        if server.analysis_request_is_stale(request_token).await {
            return Ok(None);
        }
        let mut sources = SnapshotSourceTexts::new(&snapshot);
        return Ok(Some(navigation_rename_edit(
            locations,
            uri,
            new_name,
            &mut sources,
        )));
    }
    Ok(None)
}

fn navigation_locations_to_lsp(
    locations: Vec<(String, rumoca_compile::parsing::ir_core::Location)>,
    fallback_uri: &Url,
    sources: &mut SnapshotSourceTexts<'_>,
) -> Vec<Location> {
    locations
        .into_iter()
        .map(|(uri, location)| {
            let source = sources.source_for(&uri);
            Location {
                uri: navigation_location_uri(&uri, fallback_uri),
                range: location_to_range_in_optional_source(source.as_deref(), &location),
            }
        })
        .collect()
}

fn navigation_rename_edit(
    locations: Vec<(String, rumoca_compile::parsing::ir_core::Location)>,
    fallback_uri: &Url,
    new_name: &str,
    sources: &mut SnapshotSourceTexts<'_>,
) -> WorkspaceEdit {
    let mut changes = HashMap::new();
    for (uri, location) in locations {
        let source = sources.source_for(&uri);
        changes
            .entry(navigation_location_uri(&uri, fallback_uri))
            .or_insert_with(Vec::new)
            .push(TextEdit {
                range: location_to_range_in_optional_source(source.as_deref(), &location),
                new_text: new_name.to_string(),
            });
    }

    WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    }
}

fn navigation_location_uri(uri: &str, fallback_uri: &Url) -> Url {
    Url::from_file_path(uri)
        .ok()
        .unwrap_or_else(|| fallback_uri.clone())
}
