// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::Notification as NotificationTrait;
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionOptions, CompletionParams, CompletionResponse,
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, Location, MarkupContent, MarkupKind,
    Position, Range, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Uri,
    notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument},
    request::{Completion, GotoDefinition, HoverRequest},
};
use rumoca::ir::ast::{ClassDefinition, StoredDefinition};
use std::collections::HashMap;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("Starting rumoca-lsp server");

    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string()]),
            ..Default::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(lsp_types::OneOf::Left(true)),
        ..Default::default()
    })?;

    let init_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };

    let _init_params: InitializeParams = serde_json::from_value(init_params)?;
    eprintln!("Server initialized");

    main_loop(connection)?;
    io_threads.join()?;

    eprintln!("Shutting down rumoca-lsp server");
    Ok(())
}

fn main_loop(connection: Connection) -> Result<(), Box<dyn Error + Sync + Send>> {
    let mut documents: HashMap<Uri, String> = HashMap::new();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }

                let req = match cast_request::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        let result = handle_goto_definition(&documents, params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                let req = match cast_request::<Completion>(req) {
                    Ok((id, params)) => {
                        let result = handle_completion(&documents, params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(req)) => req,
                };

                match cast_request::<HoverRequest>(req) {
                    Ok((id, params)) => {
                        let result = handle_hover(&documents, params);
                        let resp = Response::new_ok(id, result);
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(_req)) => {
                        // Unknown request, ignore
                    }
                };
            }
            Message::Response(_resp) => {
                // We don't send requests, so we don't expect responses
            }
            Message::Notification(notif) => {
                let notif = match cast_notification::<DidOpenTextDocument>(notif) {
                    Ok(params) => {
                        handle_did_open(&connection, &mut documents, params)?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(notif)) => notif,
                };

                let notif = match cast_notification::<DidChangeTextDocument>(notif) {
                    Ok(params) => {
                        handle_did_change(&connection, &mut documents, params)?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(notif)) => notif,
                };

                match cast_notification::<DidCloseTextDocument>(notif) {
                    Ok(params) => {
                        handle_did_close(&mut documents, params);
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => {
                        eprintln!("JSON error: {err:?}");
                        continue;
                    }
                    Err(ExtractError::MethodMismatch(_notif)) => {
                        // Unknown notification, ignore
                    }
                };
            }
        }
    }

    Ok(())
}

fn cast_request<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_notification<N>(notif: Notification) -> Result<N::Params, ExtractError<Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    notif.extract(N::METHOD)
}

fn handle_did_open(
    connection: &Connection,
    documents: &mut HashMap<Uri, String>,
    params: DidOpenTextDocumentParams,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let uri = params.text_document.uri.clone();
    let text = params.text_document.text;
    documents.insert(uri.clone(), text.clone());

    // Run diagnostics on open
    let diagnostics = compute_diagnostics(&uri, &text);
    publish_diagnostics(connection, uri, diagnostics)?;

    Ok(())
}

fn handle_did_change(
    connection: &Connection,
    documents: &mut HashMap<Uri, String>,
    params: DidChangeTextDocumentParams,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let uri = params.text_document.uri.clone();

    // Since we use TextDocumentSyncKind::FULL, we get the full content
    if let Some(change) = params.content_changes.into_iter().next() {
        let text = change.text;
        documents.insert(uri.clone(), text.clone());

        // Run diagnostics on change
        let diagnostics = compute_diagnostics(&uri, &text);
        publish_diagnostics(connection, uri, diagnostics)?;
    }

    Ok(())
}

fn handle_did_close(documents: &mut HashMap<Uri, String>, params: DidCloseTextDocumentParams) {
    documents.remove(&params.text_document.uri);
}

fn compute_diagnostics(uri: &Uri, text: &str) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    // Try to parse the document using rumoca's compiler
    let path = uri.path().as_str();
    if path.ends_with(".mo") {
        // First, try to parse and extract the first class name for compilation
        use rumoca::modelica_grammar::ModelicaGrammar;
        use rumoca::modelica_parser::parse;

        let mut grammar = ModelicaGrammar::new();
        match parse(text, path, &mut grammar) {
            Ok(_) => {
                // Parsing succeeded, now try to compile with auto-detected model name
                if let Some(ref ast) = grammar.modelica {
                    // Get the first class name from the AST
                    if let Some(first_class_name) = ast.class_list.keys().next() {
                        // Try full compilation with the detected model name
                        match rumoca::Compiler::new()
                            .model(first_class_name)
                            .compile_str(text, path)
                        {
                            Ok(_) => {
                                // No errors
                            }
                            Err(e) => {
                                let error_msg = format!("{}", e);
                                // Try to extract line/column from the error message
                                let (line, col) =
                                    extract_error_location(&error_msg).unwrap_or((1, 1));
                                let diagnostic = Diagnostic {
                                    range: Range {
                                        start: Position {
                                            line: line.saturating_sub(1),
                                            character: col.saturating_sub(1),
                                        },
                                        end: Position {
                                            line: line.saturating_sub(1),
                                            character: col.saturating_sub(1) + 20,
                                        },
                                    },
                                    severity: Some(DiagnosticSeverity::ERROR),
                                    source: Some("rumoca".to_string()),
                                    message: error_msg,
                                    ..Default::default()
                                };
                                diagnostics.push(diagnostic);
                            }
                        }
                    }
                }
            }
            Err(e) => {
                // Parse error - extract location from debug format which has detailed info
                let error_debug = format!("{:?}", e);
                let error_msg = format!("{}", e);
                // Try to extract from debug format first (has Location struct)
                let (line, col) = extract_location_from_debug(&error_debug)
                    .or_else(|| extract_error_location(&error_msg))
                    .unwrap_or((1, 1));
                // Try to get a better error message from the debug output
                let message = extract_error_cause(&error_debug).unwrap_or(error_msg);
                let diagnostic = Diagnostic {
                    range: Range {
                        start: Position {
                            line: line.saturating_sub(1),
                            character: col.saturating_sub(1),
                        },
                        end: Position {
                            line: line.saturating_sub(1),
                            character: col.saturating_sub(1) + 20,
                        },
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    source: Some("rumoca".to_string()),
                    message,
                    ..Default::default()
                };
                diagnostics.push(diagnostic);
            }
        }
    }

    diagnostics
}

/// Extract location from ParolError debug output
/// Looks for pattern like "error_location: Location { start_line: 2, start_column: 21, ..."
fn extract_location_from_debug(debug_str: &str) -> Option<(u32, u32)> {
    // Find the first occurrence of "error_location: Location {"
    if let Some(pos) = debug_str.find("error_location: Location {") {
        let after_location = &debug_str[pos..];

        // Extract start_line
        let line_num = if let Some(line_pos) = after_location.find("start_line:") {
            let after_line = &after_location[line_pos + 11..];
            after_line
                .split(',')
                .next()
                .and_then(|s| s.trim().parse::<u32>().ok())
        } else {
            None
        };

        // Extract start_column
        let col_num = if let Some(col_pos) = after_location.find("start_column:") {
            let after_col = &after_location[col_pos + 13..];
            after_col
                .split(',')
                .next()
                .and_then(|s| s.trim().parse::<u32>().ok())
        } else {
            None
        };

        match (line_num, col_num) {
            (Some(line), Some(col)) => Some((line, col)),
            _ => None,
        }
    } else {
        None
    }
}

/// Extract the error cause from ParolError debug output
/// Looks for pattern like 'cause: "..."'
fn extract_error_cause(debug_str: &str) -> Option<String> {
    // Find the first occurrence of 'cause: "'
    if let Some(pos) = debug_str.find("cause: \"") {
        let after_cause = &debug_str[pos + 8..];
        // Find the closing quote (but handle escaped quotes)
        let mut in_escape = false;
        let mut cause_end = 0;

        for (i, ch) in after_cause.chars().enumerate() {
            if in_escape {
                in_escape = false;
                continue;
            }
            if ch == '\\' {
                in_escape = true;
                continue;
            }
            if ch == '"' {
                cause_end = i;
                break;
            }
        }

        if cause_end > 0 {
            let cause = &after_cause[..cause_end];
            // Clean up the cause - take just the first line for cleaner display
            let first_line = cause.lines().next().unwrap_or(cause);
            return Some(first_line.to_string());
        }
    }
    None
}

/// Extract line and column from error message patterns like "file.mo:14:9" or "line X, column Y"
fn extract_error_location(error_msg: &str) -> Option<(u32, u32)> {
    // Try pattern like "filename.mo:14:9" (common in rumoca errors)
    // Look for ".mo:" followed by line:column
    if let Some(mo_pos) = error_msg.find(".mo:") {
        let after_mo = &error_msg[mo_pos + 4..];
        // Find end of line number (first non-digit or colon)
        let line_end = after_mo
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(after_mo.len());
        if line_end > 0 {
            if let Ok(line) = after_mo[..line_end].parse::<u32>() {
                // Check if there's a column after the colon
                if after_mo.len() > line_end && after_mo.as_bytes()[line_end] == b':' {
                    let after_colon = &after_mo[line_end + 1..];
                    let col_end = after_colon
                        .find(|c: char| !c.is_ascii_digit())
                        .unwrap_or(after_colon.len());
                    if col_end > 0 {
                        if let Ok(col) = after_colon[..col_end].parse::<u32>() {
                            return Some((line, col));
                        }
                    }
                }
                return Some((line, 1));
            }
        }
    }

    // Try pattern like "[1:5]" (common in error messages)
    if let Some(bracket_pos) = error_msg.find('[') {
        let after_bracket = &error_msg[bracket_pos + 1..];
        if let Some(colon_pos) = after_bracket.find(':') {
            let line_str = &after_bracket[..colon_pos];
            if let Ok(line) = line_str.trim().parse::<u32>() {
                let after_colon = &after_bracket[colon_pos + 1..];
                if let Some(end_pos) = after_colon.find(']') {
                    let col_str = &after_colon[..end_pos];
                    if let Ok(col) = col_str.trim().parse::<u32>() {
                        return Some((line, col));
                    }
                }
            }
        }
    }

    // Try pattern like "line X, column Y"
    if let Some(line_pos) = error_msg.find("line ") {
        let after_line = &error_msg[line_pos + 5..];
        let line_end = after_line
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(after_line.len());
        if let Ok(line) = after_line[..line_end].parse::<u32>() {
            if let Some(col_pos) = after_line.find("column ") {
                let after_col = &after_line[col_pos + 7..];
                let col_end = after_col
                    .find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(after_col.len());
                if let Ok(col) = after_col[..col_end].parse::<u32>() {
                    return Some((line, col));
                }
            }
            return Some((line, 1));
        }
    }

    None
}

fn publish_diagnostics(
    connection: &Connection,
    uri: Uri,
    diagnostics: Vec<Diagnostic>,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let params = lsp_types::PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    let notif = Notification::new(
        <lsp_types::notification::PublishDiagnostics as NotificationTrait>::METHOD.to_string(),
        params,
    );
    connection.sender.send(Message::Notification(notif))?;
    Ok(())
}

fn handle_completion(
    _documents: &HashMap<Uri, String>,
    _params: CompletionParams,
) -> Option<CompletionResponse> {
    // Basic Modelica keyword completions
    let keywords = vec![
        ("model", "model declaration"),
        ("class", "class declaration"),
        ("connector", "connector declaration"),
        ("package", "package declaration"),
        ("function", "function declaration"),
        ("record", "record declaration"),
        ("block", "block declaration"),
        ("type", "type declaration"),
        ("parameter", "parameter variable"),
        ("constant", "constant variable"),
        ("input", "input connector"),
        ("output", "output connector"),
        ("flow", "flow variable"),
        ("stream", "stream variable"),
        ("discrete", "discrete variable"),
        ("Real", "Real number type"),
        ("Integer", "Integer type"),
        ("Boolean", "Boolean type"),
        ("String", "String type"),
        ("extends", "inheritance"),
        ("import", "import statement"),
        ("within", "within statement"),
        ("equation", "equation section"),
        ("algorithm", "algorithm section"),
        ("initial equation", "initial equation section"),
        ("initial algorithm", "initial algorithm section"),
        ("protected", "protected section"),
        ("public", "public section"),
        ("final", "final modifier"),
        ("partial", "partial class"),
        ("replaceable", "replaceable element"),
        ("redeclare", "redeclare element"),
        ("inner", "inner element"),
        ("outer", "outer element"),
        ("encapsulated", "encapsulated class"),
        ("annotation", "annotation"),
        ("if", "if statement"),
        ("then", "then clause"),
        ("else", "else clause"),
        ("elseif", "elseif clause"),
        ("for", "for loop"),
        ("loop", "loop keyword"),
        ("while", "while loop"),
        ("when", "when statement"),
        ("connect", "connect equation"),
        ("der", "derivative operator"),
        ("pre", "pre operator"),
        ("noEvent", "noEvent operator"),
        ("smooth", "smooth operator"),
        ("sample", "sample operator"),
        ("edge", "edge operator"),
        ("change", "change operator"),
        ("reinit", "reinit operator"),
        ("sin", "sine function"),
        ("cos", "cosine function"),
        ("tan", "tangent function"),
        ("exp", "exponential function"),
        ("log", "natural logarithm"),
        ("sqrt", "square root"),
        ("abs", "absolute value"),
        ("sign", "sign function"),
        ("min", "minimum function"),
        ("max", "maximum function"),
        ("sum", "sum function"),
        ("product", "product function"),
        ("time", "simulation time"),
    ];

    let items: Vec<CompletionItem> = keywords
        .into_iter()
        .map(|(label, detail)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            ..Default::default()
        })
        .collect();

    Some(CompletionResponse::Array(items))
}

fn handle_hover(documents: &HashMap<Uri, String>, params: HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = documents.get(uri)?;

    // Get the word at the cursor position
    let word = get_word_at_position(text, position)?;

    // Provide hover info for known Modelica keywords and built-ins
    let hover_text = get_hover_info(&word)?;

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: hover_text,
        }),
        range: None,
    })
}

fn get_word_at_position(text: &str, position: Position) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let line = lines.get(position.line as usize)?;
    let col = position.character as usize;

    if col > line.len() {
        return None;
    }

    // Find word boundaries
    let start = line[..col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);

    let end = line[col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| col + i)
        .unwrap_or(line.len());

    if start >= end {
        return None;
    }

    Some(line[start..end].to_string())
}

fn get_hover_info(word: &str) -> Option<String> {
    let info = match word {
        "model" => {
            "**model**\n\nA model is a class that can contain equations and may be instantiated."
        }
        "class" => {
            "**class**\n\nA general class definition. Models, connectors, and other specialized classes inherit from class."
        }
        "connector" => {
            "**connector**\n\nA connector defines the interface for connections between components."
        }
        "package" => "**package**\n\nA package is a namespace for organizing classes and models.",
        "function" => {
            "**function**\n\nA function is a class that computes outputs from inputs using algorithms."
        }
        "record" => "**record**\n\nA record is a class used as a data structure without equations.",
        "block" => "**block**\n\nA block is a class with fixed causality for inputs and outputs.",
        "type" => "**type**\n\nDefines a type alias or derived type.",
        "parameter" => {
            "**parameter**\n\nA parameter is a variable that remains constant during simulation but can be changed between simulations."
        }
        "constant" => "**constant**\n\nA constant is a variable whose value cannot be changed.",
        "input" => "**input**\n\nDeclares an input connector variable.",
        "output" => "**output**\n\nDeclares an output connector variable.",
        "flow" => "**flow**\n\nDeclares a flow variable (summed to zero in connections).",
        "stream" => "**stream**\n\nDeclares a stream variable for bidirectional flow.",
        "Real" => {
            "**Real**\n\nFloating-point number type.\n\nAttributes: `unit`, `displayUnit`, `min`, `max`, `start`, `fixed`, `nominal`, `stateSelect`"
        }
        "Integer" => {
            "**Integer**\n\nInteger number type.\n\nAttributes: `min`, `max`, `start`, `fixed`"
        }
        "Boolean" => {
            "**Boolean**\n\nBoolean type with values `true` and `false`.\n\nAttributes: `start`, `fixed`"
        }
        "String" => "**String**\n\nString type for text.\n\nAttributes: `start`",
        "der" => {
            "**der(x)**\n\nTime derivative of variable x.\n\n```modelica\nder(x) = v;  // dx/dt = v\n```"
        }
        "pre" => {
            "**pre(x)**\n\nThe value of x immediately before an event.\n\nUsed in when-clauses to access previous values."
        }
        "time" => "**time**\n\nBuilt-in variable representing simulation time.",
        "equation" => {
            "**equation**\n\nSection containing equations that define the mathematical relationships."
        }
        "algorithm" => "**algorithm**\n\nSection containing sequential assignment statements.",
        "connect" => "**connect(a, b)**\n\nCreates a connection between connectors a and b.",
        "when" => {
            "**when**\n\nEvent-triggered section. Equations inside are active only when condition becomes true."
        }
        "if" => "**if**\n\nConditional expression or statement.",
        "for" => "**for**\n\nLoop construct for iteration.",
        "extends" => "**extends**\n\nInheritance from a base class.",
        "import" => "**import**\n\nImports classes from other packages.",
        "within" => "**within**\n\nSpecifies the package this file belongs to.",
        "annotation" => "**annotation**\n\nMetadata for documentation, icons, experiments, etc.",
        "sin" => "**sin(x)**\n\nSine function. Returns the sine of x (in radians).",
        "cos" => "**cos(x)**\n\nCosine function. Returns the cosine of x (in radians).",
        "tan" => "**tan(x)**\n\nTangent function. Returns the tangent of x (in radians).",
        "exp" => "**exp(x)**\n\nExponential function. Returns e^x.",
        "log" => "**log(x)**\n\nNatural logarithm. Returns ln(x).",
        "sqrt" => "**sqrt(x)**\n\nSquare root. Returns √x.",
        "abs" => "**abs(x)**\n\nAbsolute value. Returns |x|.",
        "min" => "**min(a, b)**\n\nReturns the minimum of a and b.",
        "max" => "**max(a, b)**\n\nReturns the maximum of a and b.",
        _ => return None,
    };
    Some(info.to_string())
}

fn handle_goto_definition(
    documents: &HashMap<Uri, String>,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    // Get the word at cursor position
    let word = get_word_at_position(text, position)?;

    // Try to parse the document and find definitions
    if let Ok(result) = rumoca::Compiler::new().compile_str(text, path) {
        // Search for the definition in the AST
        if let Some(location) = find_definition_in_ast(&result.def, &word) {
            let target_uri = uri.clone();
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: target_uri,
                range: Range {
                    start: Position {
                        line: location.0.saturating_sub(1),
                        character: location.1.saturating_sub(1),
                    },
                    end: Position {
                        line: location.0.saturating_sub(1),
                        character: location.1.saturating_sub(1) + word.len() as u32,
                    },
                },
            }));
        }
    }

    None
}

/// Find a definition in the AST, returning (line, column) if found
fn find_definition_in_ast(def: &StoredDefinition, name: &str) -> Option<(u32, u32)> {
    // Search in all class definitions
    for class in def.class_list.values() {
        if let Some(loc) = find_definition_in_class(class, name) {
            return Some(loc);
        }
    }
    None
}

/// Recursively search for a definition in a class
fn find_definition_in_class(class: &ClassDefinition, name: &str) -> Option<(u32, u32)> {
    // Check if this class matches the name
    if class.name.text == name {
        return Some((
            class.name.location.start_line,
            class.name.location.start_column,
        ));
    }

    // Check components (variables)
    for (comp_name, comp) in &class.components {
        if comp_name == name {
            // Use the type_name's first token location if available
            if let Some(first_token) = comp.type_name.name.first() {
                return Some((
                    first_token.location.start_line,
                    first_token.location.start_column,
                ));
            }
        }
    }

    // Check nested classes (functions, types, etc.)
    for nested_class in class.classes.values() {
        if let Some(loc) = find_definition_in_class(nested_class, name) {
            return Some(loc);
        }
    }

    None
}
