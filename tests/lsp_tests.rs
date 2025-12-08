// Allow mutable_key_type - Uri has interior mutability but we use it correctly as a key
#![allow(clippy::mutable_key_type)]

//! Comprehensive LSP tests for the Rumoca Modelica compiler.
//!
//! Tests all LSP features including:
//! - Document symbols
//! - Hover
//! - Go to definition
//! - Completion
//! - Signature help
//! - References
//! - Rename
//! - Folding
//! - Code actions
//! - Inlay hints
//! - Semantic tokens
//! - Workspace symbols
//! - Formatting
//! - Code lenses
//! - Call hierarchy
//! - Document links

mod common;

use lsp_types::{
    CallHierarchyPrepareParams, CodeActionContext, CodeActionParams, CodeLensParams,
    CompletionParams, CompletionTriggerKind, DocumentFormattingParams, DocumentLinkParams,
    DocumentSymbolParams, FoldingRangeParams, FormattingOptions, GotoDefinitionParams,
    HoverContents, HoverParams, InlayHintParams, Position, Range, ReferenceContext,
    ReferenceParams, SemanticTokensParams, SignatureHelpParams, TextDocumentIdentifier,
    TextDocumentPositionParams, Uri, WorkspaceSymbolParams,
};

use rumoca::lsp::{
    WorkspaceState, compute_diagnostics, create_documents, get_semantic_token_legend,
    handle_code_action, handle_code_lens, handle_completion_workspace, handle_document_links,
    handle_document_symbols, handle_folding_range, handle_formatting, handle_goto_definition,
    handle_hover, handle_inlay_hints, handle_prepare_call_hierarchy, handle_references,
    handle_semantic_tokens, handle_signature_help, handle_workspace_symbol,
};

// Use common LSP test utilities
use common::lsp::test_uri;

// ============================================================================
// Diagnostics Tests
// ============================================================================

#[test]
fn test_diagnostics_valid_model() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
equation
  der(x) = 1;
end Test;"#;

    let mut workspace = WorkspaceState::new();
    let diagnostics = compute_diagnostics(&uri, text, &mut workspace);
    // Valid model should have no errors
    assert!(
        diagnostics.is_empty(),
        "Expected no diagnostics for valid model"
    );
}

#[test]
fn test_diagnostics_syntax_error() {
    let uri = test_uri();
    let text = "model Test\n  Real x\nend Test;"; // Missing semicolon

    let mut workspace = WorkspaceState::new();
    let diagnostics = compute_diagnostics(&uri, text, &mut workspace);
    assert!(
        !diagnostics.is_empty(),
        "Expected diagnostics for syntax error"
    );
}

#[test]
fn test_diagnostics_inherited_variables() {
    // Test that inherited variables from extends clause are recognized
    // This is a regression test for the fix where F_e was reported as undefined
    // in SportCub even though it extends RigidBody which defines F_e
    let uri = test_uri();
    let text = r#"
block SportCub
    extends RigidBody;
    parameter Real g = 9.81;
equation
    F_e = {0, 0, -m*g};
    M_b = {0, 0, 0};
end SportCub;

model RigidBody
    parameter Real m = 1.0;
    Real F_e[3] "external force";
    Real M_b[3] "external moment";
equation
    F_e = m * {0, 0, 0};
    M_b = {0, 0, 0};
end RigidBody;
"#;

    let mut workspace = WorkspaceState::new();
    let diagnostics = compute_diagnostics(&uri, text, &mut workspace);

    // Filter to only "undefined" errors
    let undefined_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.message.contains("Undefined")
                && (d.message.contains("F_e")
                    || d.message.contains("M_b")
                    || d.message.contains("m"))
        })
        .collect();

    assert!(
        undefined_errors.is_empty(),
        "F_e, M_b, and m should be recognized as inherited from RigidBody, but got: {:?}",
        undefined_errors
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_diagnostics_inherited_components_not_unused() {
    // Test that inherited components don't trigger "unused variable" warnings
    // This is a regression test: inherited components like J are used in the base
    // class's equations, so they shouldn't be flagged as unused in derived classes
    let uri = test_uri();
    let text = r#"
block SportCub
    extends RigidBody;
    parameter Real g = 9.81;
equation
    F_e = {0, 0, -m*g};
    M_b = {0, 0, 0};
end SportCub;

model RigidBody
    parameter Real m = 1.0;
    Real F_e[3] "external force";
    Real M_b[3] "external moment";
    Real J[3,3] "inertia matrix";  // Used in RigidBody's equations but not directly in SportCub
equation
    J = {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}};
    F_e = m * {0, 0, 0};
    M_b = J * {0, 0, 0};  // J is used here in base class
end RigidBody;
"#;

    let mut workspace = WorkspaceState::new();
    let diagnostics = compute_diagnostics(&uri, text, &mut workspace);

    // Check that J is NOT flagged as unused in SportCub
    let j_unused_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("'J'") && d.message.contains("unused"))
        .collect();

    assert!(
        j_unused_warnings.is_empty(),
        "Inherited component J should NOT be flagged as unused in SportCub: {:?}",
        j_unused_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

#[test]
fn test_diagnostics_array_indexing_type_inference() {
    // Test that array indexing correctly reduces dimensions in type checking
    // e.g., q[3] where q is Real[4] should be Real, not Real[4]
    // This prevents false "type mismatch" errors like "Real is not compatible with Real[4]"
    let uri = test_uri();
    let text = r#"
function quatToRot
    input Real q[4] "quaternion";
    output Real R[3,3] "rotation matrix";
algorithm
    R[1,1] := 1 - 2*(q[3]^2 + q[4]^2);
    R[1,2] := 2*(q[2]*q[3] - q[1]*q[4]);
end quatToRot;
"#;

    let mut workspace = WorkspaceState::new();
    let diagnostics = compute_diagnostics(&uri, text, &mut workspace);

    // Filter to type mismatch errors related to array types
    let type_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("not compatible") && d.message.contains("Real["))
        .collect();

    assert!(
        type_errors.is_empty(),
        "Array indexing should reduce type from Real[4] to Real, but got type errors: {:?}",
        type_errors.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

// ============================================================================
// Document Symbols Tests
// ============================================================================

#[test]
fn test_document_symbols_model() {
    let uri = test_uri();
    let text = r#"model Test
  parameter Real k = 1.0;
  Real x(start = 0);
equation
  der(x) = k * x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_document_symbols(&documents, params);
    assert!(result.is_some(), "Expected document symbols");

    if let Some(lsp_types::DocumentSymbolResponse::Nested(symbols)) = result {
        assert!(!symbols.is_empty(), "Expected at least one symbol");
        // Should have the model "Test"
        assert!(
            symbols.iter().any(|s| s.name == "Test"),
            "Expected Test model symbol"
        );
    }
}

#[test]
fn test_document_symbols_nested_classes() {
    let uri = test_uri();
    let text = r#"package MyPackage
  model Inner
    Real x;
  end Inner;
end MyPackage;"#;

    let documents = create_documents(&uri, text);
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_document_symbols(&documents, params);
    assert!(result.is_some());
}

/// Helper to recursively validate that selectionRange is contained within range for all symbols
fn validate_symbol_ranges(symbol: &lsp_types::DocumentSymbol) -> Result<(), String> {
    // Check that selection_range is contained within range
    let range = &symbol.range;
    let sel_range = &symbol.selection_range;

    // Start of selection must be >= start of range
    let start_ok = sel_range.start.line > range.start.line
        || (sel_range.start.line == range.start.line
            && sel_range.start.character >= range.start.character);

    // End of selection must be <= end of range
    let end_ok = sel_range.end.line < range.end.line
        || (sel_range.end.line == range.end.line && sel_range.end.character <= range.end.character);

    if !start_ok || !end_ok {
        return Err(format!(
            "Symbol '{}': selectionRange {:?} is not contained in range {:?}",
            symbol.name, sel_range, range
        ));
    }

    // Recursively check children
    if let Some(children) = &symbol.children {
        for child in children {
            validate_symbol_ranges(child)?;
        }
    }

    Ok(())
}

#[test]
fn test_document_symbols_range_containment() {
    let uri = test_uri();
    // Use circuit.mo style content that was causing the bug
    let text = r#"class SimpleCircuit
  Resistor R1(R=10);
  Capacitor C(C=0.01);
  Resistor R2(R=100);
  Inductor L1(L=0.1);
  VsourceAC AC;
  Ground G;
equation
  connect(AC.p, R1.p);
  connect(R1.n, C.p);
  connect(C.n, AC.n);
  connect(R1.p, R2.p);
  connect(R2.n, L1.p);
  connect(L1.n, C.n);
  connect(AC.n, G.p);
end SimpleCircuit;"#;

    let documents = create_documents(&uri, text);
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_document_symbols(&documents, params);
    assert!(result.is_some(), "Expected document symbols");

    if let Some(lsp_types::DocumentSymbolResponse::Nested(symbols)) = result {
        for symbol in &symbols {
            if let Err(e) = validate_symbol_ranges(symbol) {
                panic!("Range validation failed: {}", e);
            }
        }
    }
}

// ============================================================================
// Hover Tests
// ============================================================================

#[test]
fn test_hover_on_type() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: 3,
            }, // "Real"
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_hover(&documents, params);
    assert!(result.is_some(), "Expected hover information for type");
}

#[test]
fn test_hover_on_variable() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: 7,
            }, // "x"
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_hover(&documents, params);
    assert!(result.is_some(), "Expected hover information for variable");
}

#[test]
fn test_hover_on_inherited_variable() {
    let uri = test_uri();
    // Test hover on a variable inherited via extends
    // TwoPin defines v and i, Capacitor extends TwoPin and uses v in an equation
    // Lines (0-indexed):
    // 0: partial class TwoPin
    // 1:   Real v;
    // 2:   Real i;
    // 3: equation
    // 4:   v = 1;
    // 5: end TwoPin;
    // 6: (empty)
    // 7: class Capacitor
    // 8:   extends TwoPin;
    // 9:   parameter Real C = 1.0;
    // 10: equation
    // 11:   C * der(v) = i;
    // 12: end Capacitor;
    let text = r#"partial class TwoPin
  Real v;
  Real i;
equation
  v = 1;
end TwoPin;

class Capacitor
  extends TwoPin;
  parameter Real C = 1.0;
equation
  C * der(v) = i;
end Capacitor;"#;

    let documents = create_documents(&uri, text);

    // Hover over 'v' in the equation "C * der(v) = i" (line 11, character 10 is inside der(v))
    // Line 11 is:   C * der(v) = i;
    // Chars:      0123456789...
    // 'v' is at character 10
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 11,
                character: 10,
            }, // "v" inside der(v)
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_hover(&documents, params);
    assert!(
        result.is_some(),
        "Expected hover information for inherited variable 'v'"
    );

    if let Some(hover) = result {
        if let HoverContents::Markup(markup) = hover.contents {
            // Should show the variable info with type Real
            assert!(
                markup.value.contains("v") && markup.value.contains("Real"),
                "Hover should show 'v: Real', got: {}",
                markup.value
            );
            // Should indicate it's inherited from TwoPin
            assert!(
                markup.value.contains("TwoPin"),
                "Hover should indicate inheritance from TwoPin, got: {}",
                markup.value
            );
        } else {
            panic!("Expected Markup hover contents");
        }
    }
}

#[test]
fn test_hover_on_class_shows_flattened_content() {
    // Test that hovering on a class name shows flattened content including inherited members
    let uri = test_uri();
    let text = r#"model RigidBody
    parameter Real m = 1.0;
    Real F_e[3] "external force";
    Real M_b[3] "external moment";
equation
    F_e = m * {0, 0, 0};
    M_b = {0, 0, 0};
end RigidBody;

block SportCub
    extends RigidBody;
    parameter Real g = 9.81;
equation
    F_e = {0, 0, -m*g};
    M_b = {0, 0, 0};
end SportCub;"#;

    let documents = create_documents(&uri, text);

    // Hover over 'SportCub' class name (line 9, character 6)
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 9,
                character: 8,
            }, // "SportCub"
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_hover(&documents, params);
    assert!(
        result.is_some(),
        "Expected hover information for class SportCub"
    );

    if let Some(hover) = result {
        if let HoverContents::Markup(markup) = hover.contents {
            eprintln!("Hover content:\n{}", markup.value);

            // Should show inherited components from RigidBody: F_e, M_b, m
            assert!(
                markup.value.contains("F_e"),
                "Hover should show inherited F_e from RigidBody, got:\n{}",
                markup.value
            );
            assert!(
                markup.value.contains("M_b"),
                "Hover should show inherited M_b from RigidBody, got:\n{}",
                markup.value
            );
            assert!(
                markup.value.contains("m"),
                "Hover should show inherited parameter m from RigidBody, got:\n{}",
                markup.value
            );

            // Should show inherited equations from RigidBody
            // After flattening, the equations from both classes should be present
        } else {
            panic!("Expected Markup hover contents");
        }
    }
}

// ============================================================================
// Go to Definition Tests
// ============================================================================

#[test]
fn test_goto_definition_variable() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
equation
  der(x) = 1;
end Test;"#;

    let documents = create_documents(&uri, text);

    // Test going to definition from the declaration position
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: 7,
            }, // "x" in "Real x;"
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_goto_definition(&documents, params);
    // Go to definition finds the type location for a variable, or may return None
    // This is testing that the handler doesn't crash, actual functionality depends on implementation
    // The current implementation finds components by name and returns their type location
    let _ = result; // Result may or may not be Some depending on implementation
}

#[test]
fn test_goto_definition_class() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 0,
                character: 6,
            }, // "Test" in "model Test"
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_goto_definition(&documents, params);
    // Go to definition for the class name may or may not find the definition
    // depending on whether the compile_str succeeds with fake paths
    // This test ensures the handler doesn't crash
    let _ = result;
}

// ============================================================================
// Completion Tests
// ============================================================================

#[test]
fn test_completion_keywords() {
    let uri = test_uri();
    let text = "mod"; // Partial keyword

    let _documents = create_documents(&uri, text);
    let mut workspace = WorkspaceState::new();
    workspace.update_document(uri.clone(), text.to_string());
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 0,
                character: 3,
            },
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: Some(lsp_types::CompletionContext {
            trigger_kind: CompletionTriggerKind::INVOKED,
            trigger_character: None,
        }),
    };

    let result = handle_completion_workspace(&mut workspace, params);
    assert!(result.is_some(), "Expected completion items");

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        // Should include "model" keyword
        assert!(
            items.iter().any(|i| i.label == "model"),
            "Expected 'model' keyword in completions"
        );
    }
}

#[test]
fn test_completion_builtin_functions() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
equation
  x = sin
end Test;"#;

    let _documents = create_documents(&uri, text);
    let mut workspace = WorkspaceState::new();
    workspace.update_document(uri.clone(), text.to_string());
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 3,
                character: 9,
            }, // after "sin"
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: None,
    };

    let result = handle_completion_workspace(&mut workspace, params);
    assert!(result.is_some(), "Expected completion items");

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        // Should include "sin" function
        assert!(
            items.iter().any(|i| i.label == "sin"),
            "Expected 'sin' function in completions"
        );
    }
}

#[test]
fn test_completion_modifiers() {
    let uri = test_uri();
    // Test that typing inside parentheses after a type declaration shows modifier completions
    let text = r#"model Test
  Real h(
end Test;"#;

    let _documents = create_documents(&uri, text);
    let mut workspace = WorkspaceState::new();
    workspace.update_document(uri.clone(), text.to_string());
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: 9,
            }, // after "Real h("
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: None,
    };

    let result = handle_completion_workspace(&mut workspace, params);
    assert!(result.is_some(), "Expected completion items for modifiers");

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Should include common modifiers
        assert!(
            labels.contains(&"start"),
            "Expected 'start' modifier in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"fixed"),
            "Expected 'fixed' modifier in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"min"),
            "Expected 'min' modifier in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"max"),
            "Expected 'max' modifier in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"unit"),
            "Expected 'unit' modifier in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"stateSelect"),
            "Expected 'stateSelect' modifier in completions, got: {:?}",
            labels
        );

        // Should NOT include keywords (we're in modifier context)
        assert!(
            !labels.contains(&"model"),
            "Should NOT have 'model' keyword in modifier completions"
        );
    }
}

#[test]
fn test_completion_modifiers_after_comma() {
    let uri = test_uri();
    // Test that typing after a comma in modifiers shows completions
    let text = r#"model Test
  Real h(start=10.0,
end Test;"#;

    let _documents = create_documents(&uri, text);
    let mut workspace = WorkspaceState::new();
    workspace.update_document(uri.clone(), text.to_string());
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: 21,
            }, // after "Real h(start=10.0, "
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: None,
    };

    let result = handle_completion_workspace(&mut workspace, params);
    assert!(
        result.is_some(),
        "Expected completion items for modifiers after comma"
    );

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Should still show modifier completions
        assert!(
            labels.contains(&"fixed"),
            "Expected 'fixed' modifier in completions after comma, got: {:?}",
            labels
        );
    }
}

#[test]
fn test_completion_member_access() {
    let uri = test_uri();
    // Test that typing "ball." after declaring a component shows its members
    // Use a simpler structure where the model is in the same scope
    // Note: The document must be syntactically valid for parsing to succeed
    let text = r#"model BouncingBall
  parameter Real g = 9.81;
  Real h(start = 10.0);
  Real v(start = 0.0);
equation
  der(h) = v;
  der(v) = -g;
end BouncingBall;

model B
  BouncingBall ball;
equation
  ball.h = 1;
end B;"#;

    let _documents = create_documents(&uri, text);
    let mut workspace = WorkspaceState::new();
    workspace.update_document(uri.clone(), text.to_string());
    // Position cursor after "ball." on line "  ball.h = 1;"
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 12,
                character: 7,
            }, // after "ball."
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: Some(lsp_types::CompletionContext {
            trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
            trigger_character: Some(".".to_string()),
        }),
    };

    let result = handle_completion_workspace(&mut workspace, params);
    assert!(result.is_some(), "Expected completion items for ball.");

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        // Should include members of BouncingBall: g, h, v
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
        assert!(
            labels.contains(&"g"),
            "Expected 'g' parameter in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"h"),
            "Expected 'h' variable in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"v"),
            "Expected 'v' variable in completions, got: {:?}",
            labels
        );
    }
}

// ============================================================================
// Signature Help Tests
// ============================================================================

#[test]
fn test_signature_help_builtin_function() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
equation
  x = sin(
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = SignatureHelpParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 3,
                character: 10,
            }, // inside sin(
        },
        work_done_progress_params: Default::default(),
        context: None,
    };

    let result = handle_signature_help(&documents, params);
    assert!(result.is_some(), "Expected signature help for sin()");

    if let Some(sig_help) = result {
        assert!(
            !sig_help.signatures.is_empty(),
            "Expected at least one signature"
        );
    }
}

// ============================================================================
// References Tests
// ============================================================================

#[test]
fn test_references_variable() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
  Real y;
equation
  der(x) = y;
  y = x + 1;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = ReferenceParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 1,
                character: 7,
            }, // "x" declaration
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: ReferenceContext {
            include_declaration: true,
        },
    };

    let result = handle_references(&documents, params);
    assert!(result.is_some(), "Expected references");

    if let Some(refs) = result {
        // Should find at least 3 references (declaration + 2 usages)
        assert!(refs.len() >= 2, "Expected multiple references to x");
    }
}

// ============================================================================
// Folding Range Tests
// ============================================================================

#[test]
fn test_folding_range_model() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
  Real y;
equation
  der(x) = 1;
  y = x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = FoldingRangeParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_folding_range(&documents, params);
    assert!(result.is_some(), "Expected folding ranges");

    if let Some(ranges) = result {
        assert!(!ranges.is_empty(), "Expected at least one folding range");
    }
}

#[test]
fn test_folding_range_comments() {
    let uri = test_uri();
    let text = r#"// This is a comment
// spanning multiple
// lines
model Test
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = FoldingRangeParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_folding_range(&documents, params);
    assert!(result.is_some());
}

// ============================================================================
// Code Actions Tests
// ============================================================================

#[test]
fn test_code_action_on_diagnostic() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 9,
            },
        },
        context: CodeActionContext {
            diagnostics: vec![],
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_code_action(&documents, params);
    // May or may not have code actions depending on context
    assert!(result.is_some());
}

// ============================================================================
// Inlay Hints Tests
// ============================================================================

#[test]
fn test_inlay_hints_function_params() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
equation
  x = sin(3.14);
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = InlayHintParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 4,
                character: 9,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_inlay_hints(&documents, params);
    assert!(result.is_some());
}

// ============================================================================
// Semantic Tokens Tests
// ============================================================================

#[test]
fn test_semantic_tokens_legend() {
    let legend = get_semantic_token_legend();
    assert!(
        !legend.token_types.is_empty(),
        "Expected token types in legend"
    );
}

#[test]
fn test_semantic_tokens_model() {
    let uri = test_uri();
    let text = r#"model Test
  parameter Real k = 1.0;
  Real x;
equation
  der(x) = k * x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = SemanticTokensParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_semantic_tokens(&documents, params);
    assert!(result.is_some(), "Expected semantic tokens");

    if let Some(lsp_types::SemanticTokensResult::Tokens(tokens)) = result {
        assert!(!tokens.data.is_empty(), "Expected token data");
    }
}

// ============================================================================
// Workspace Symbols Tests
// ============================================================================

#[test]
fn test_workspace_symbols_search() {
    let uri = test_uri();
    let text = r#"model TestModel
  Real x;
end TestModel;

function TestFunction
  input Real x;
  output Real y;
algorithm
  y := x * 2;
end TestFunction;"#;

    let documents = create_documents(&uri, text);
    let params = WorkspaceSymbolParams {
        query: "Test".to_string(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_workspace_symbol(&documents, params);
    assert!(result.is_some());

    if let Some(symbols) = result {
        assert!(
            symbols.iter().any(|s| s.name.contains("Test")),
            "Expected symbols matching 'Test'"
        );
    }
}

#[test]
fn test_workspace_symbols_empty_query() {
    let uri = test_uri();
    let text = r#"model MyModel
end MyModel;"#;

    let documents = create_documents(&uri, text);
    let params = WorkspaceSymbolParams {
        query: "".to_string(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_workspace_symbol(&documents, params);
    assert!(result.is_some());
}

// ============================================================================
// Formatting Tests
// ============================================================================

#[test]
fn test_formatting_indentation() {
    let uri = test_uri();
    let text = "model Test\nReal x;\nend Test;";

    let documents = create_documents(&uri, text);
    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions {
            tab_size: 2,
            insert_spaces: true,
            ..Default::default()
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_formatting(&documents, params);
    assert!(result.is_some(), "Expected formatting result");

    if let Some(edits) = result
        && !edits.is_empty()
    {
        // Check that the edit adds proper indentation
        let new_text = &edits[0].new_text;
        assert!(
            new_text.contains("  Real x;"),
            "Expected proper indentation"
        );
    }
}

#[test]
fn test_formatting_operators() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
equation
  x=1+2*3;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = DocumentFormattingParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        options: FormattingOptions {
            tab_size: 2,
            insert_spaces: true,
            ..Default::default()
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_formatting(&documents, params);
    assert!(result.is_some());

    if let Some(edits) = result
        && !edits.is_empty()
    {
        let new_text = &edits[0].new_text;
        // Should have spaces around operators
        assert!(
            new_text.contains("x = 1 + 2 * 3"),
            "Expected spaces around operators"
        );
    }
}

// ============================================================================
// Code Lens Tests
// ============================================================================

#[test]
fn test_code_lens_model() {
    let uri = test_uri();
    let text = r#"model Test
  Real x;
  Real y;
equation
  der(x) = 1;
  y = x;
end Test;"#;

    let mut workspace = WorkspaceState::new();
    workspace.open_document(uri.clone(), text.to_string());
    // Compute diagnostics first to populate balance cache
    compute_diagnostics(&uri, text, &mut workspace);

    let params = CodeLensParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_code_lens(&workspace, params);
    assert!(result.is_some());

    if let Some(lenses) = result {
        // Should have lenses for component count, equation count
        assert!(!lenses.is_empty(), "Expected code lenses");
    }
}

#[test]
fn test_code_lens_extends() {
    let uri = test_uri();
    let text = r#"model Base
  Real x;
end Base;

model Derived
  extends Base;
  Real y;
end Derived;"#;

    let mut workspace = WorkspaceState::new();
    workspace.open_document(uri.clone(), text.to_string());
    // Compute diagnostics first to populate balance cache
    compute_diagnostics(&uri, text, &mut workspace);

    let params = CodeLensParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_code_lens(&workspace, params);
    assert!(result.is_some());
}

// ============================================================================
// Call Hierarchy Tests
// ============================================================================

#[test]
fn test_call_hierarchy_prepare() {
    let uri = test_uri();
    let text = r#"function myFunc
  input Real x;
  output Real y;
algorithm
  y := x * 2;
end myFunc;

model Test
  Real z;
equation
  z = myFunc(1.0);
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = CallHierarchyPrepareParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 0,
                character: 10,
            }, // on "myFunc"
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_prepare_call_hierarchy(&documents, params);
    // Function definitions should be found
    assert!(result.is_some());
}

// ============================================================================
// Document Links Tests
// ============================================================================

#[test]
fn test_document_links_imports() {
    let uri = test_uri();
    // Model with a file path string that should be detected as a link
    let text = r#"model Test
  annotation(Icon(graphics={Bitmap(fileName="resources/icon.svg")}));
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = DocumentLinkParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_document_links(&documents, params);
    assert!(result.is_some(), "Expected document links result");

    if let Some(links) = result {
        // Should have a link for the fileName
        assert!(
            !links.is_empty(),
            "Expected document links for fileName annotation"
        );
    }
}

#[test]
fn test_document_links_within() {
    let uri = test_uri();
    let text = r#"within MyPackage;

model Test
  Real x;
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = DocumentLinkParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_document_links(&documents, params);
    // Result should be Some even if empty (valid model that parsed)
    assert!(result.is_some(), "Expected document links result");
}

#[test]
fn test_document_links_urls() {
    let uri = test_uri();
    let text = r#"model Test "Test model"
  annotation(Documentation(info="<html>
    <p>See <a href=\"https://example.com\">docs</a></p>
  </html>"));
end Test;"#;

    let documents = create_documents(&uri, text);
    let params = DocumentLinkParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    let result = handle_document_links(&documents, params);
    assert!(result.is_some());
}

// ============================================================================
// Workspace State Tests
// ============================================================================

#[test]
fn test_workspace_state_document_management() {
    let mut ws = WorkspaceState::new();
    let uri: Uri = "file:///tmp/test.mo".parse().unwrap();
    let text = "model Test end Test;";

    // Open document
    ws.open_document(uri.clone(), text.to_string());
    assert!(ws.get_document(&uri).is_some());
    assert_eq!(ws.get_document(&uri).unwrap(), text);

    // Update document
    let new_text = "model Test Real x; end Test;";
    ws.update_document(uri.clone(), new_text.to_string());
    assert_eq!(ws.get_document(&uri).unwrap(), new_text);

    // Close document
    ws.close_document(&uri);
    assert!(ws.get_document(&uri).is_none());
}

#[test]
fn test_workspace_state_symbol_indexing() {
    let mut ws = WorkspaceState::new();
    let uri: Uri = "file:///tmp/test.mo".parse().unwrap();
    let text = r#"model TestModel
  Real x;
end TestModel;

function TestFunction
  input Real x;
  output Real y;
algorithm
  y := x * 2;
end TestFunction;"#;

    ws.open_document(uri.clone(), text.to_string());

    // Search for symbols
    let symbols = ws.find_symbols("Test");
    assert!(
        !symbols.is_empty(),
        "Expected symbols matching 'Test' query"
    );
}

// ============================================================================
// Edge Cases and Error Handling Tests
// ============================================================================

#[test]
fn test_empty_document() {
    let uri = test_uri();
    let text = "";

    let documents = create_documents(&uri, text);

    // Document symbols on empty doc
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let result = handle_document_symbols(&documents, params);
    // Should not crash, may return None or empty
    assert!(
        result.is_none()
            || matches!(result, Some(lsp_types::DocumentSymbolResponse::Nested(v)) if v.is_empty())
    );
}

#[test]
fn test_nonexistent_document() {
    let uri: Uri = "file:///nonexistent.mo".parse().unwrap();
    let other_uri: Uri = "file:///other.mo".parse().unwrap();
    let documents = create_documents(&other_uri, "model Test end Test;");

    // Try to get symbols for non-existent document
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let result = handle_document_symbols(&documents, params);
    assert!(result.is_none());
}

#[test]
fn test_position_out_of_bounds() {
    let uri = test_uri();
    let text = "model Test end Test;";
    let documents = create_documents(&uri, text);

    // Position way beyond document
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position: Position {
                line: 100,
                character: 100,
            },
        },
        work_done_progress_params: Default::default(),
    };

    let result = handle_hover(&documents, params);
    // Should not crash, may return None
    assert!(result.is_none());
}

#[test]
fn test_completion_member_access_with_syntax_error() {
    // Test that member completion works even when the document has syntax errors
    // This simulates typing "ball." mid-expression, which causes a parse error
    let uri = test_uri();

    // First, open the document in a VALID state (this caches the AST)
    let valid_text = r#"model BouncingBall
  parameter Real g = 9.81;
  Real h(start = 10.0);
  Real v(start = 0.0);
equation
  der(h) = v;
  der(v) = -g;
end BouncingBall;

model B
  BouncingBall ball;
equation
  ball.h = 1;
end B;"#;

    let mut ws = WorkspaceState::new();
    ws.open_document(uri.clone(), valid_text.to_string());

    // Now simulate the user typing - document becomes invalid with "ball."
    let invalid_text = r#"model BouncingBall
  parameter Real g = 9.81;
  Real h(start = 10.0);
  Real v(start = 0.0);
equation
  der(h) = v;
  der(v) = -g;
end BouncingBall;

model B
  BouncingBall ball;
equation
  ball.
end B;"#;
    // Note: "ball." is incomplete syntax - will cause parse error

    // Update the document (simulates typing)
    ws.update_document(uri.clone(), invalid_text.to_string());

    // Use the handle_completion_workspace function
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 12,
                character: 7,
            }, // after "ball."
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: Some(lsp_types::CompletionContext {
            trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
            trigger_character: Some(".".to_string()),
        }),
    };

    let result = rumoca::lsp::handle_completion_workspace(&mut ws, params);
    assert!(
        result.is_some(),
        "Expected completion items for ball. even with syntax error"
    );

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        // Should include members of BouncingBall: g, h, v
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Should NOT have keywords (that would indicate fallback to general completion)
        assert!(
            !labels.contains(&"model"),
            "Should NOT have keywords in dot completion: {:?}",
            labels
        );
        assert!(
            !labels.contains(&"parameter"),
            "Should NOT have keywords in dot completion: {:?}",
            labels
        );

        // Should have class members
        assert!(
            labels.contains(&"g"),
            "Expected 'g' parameter in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"h"),
            "Expected 'h' variable in completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"v"),
            "Expected 'v' variable in completions, got: {:?}",
            labels
        );
    }
}

#[test]
fn test_completion_class_instance_modifiers() {
    // Test that typing after opening paren for a class instance shows member modifiers
    // We use a valid document so the parser can succeed and we can look up class members
    let uri = test_uri();
    // Valid document - we simulate the user typing after opening paren by positioning the cursor
    // The document needs to be syntactically valid for parsing to succeed
    let text = r#"model BouncingBall
  parameter Real g = 9.81;
  Real h(start = 10.0);
  Real v(start = 0.0);
equation
  der(h) = v;
  der(v) = -g;
end BouncingBall;

model B
  BouncingBall ball(g = 1);
end B;"#;

    let _documents = create_documents(&uri, text);
    let mut workspace = WorkspaceState::new();
    workspace.update_document(uri.clone(), text.to_string());
    // Position cursor right after "ball(" - character 20 is after the opening paren
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 10,
                character: 20,
            }, // after "BouncingBall ball("
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: Some(lsp_types::CompletionContext {
            trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
            trigger_character: Some("(".to_string()),
        }),
    };

    let result = handle_completion_workspace(&mut workspace, params);
    assert!(
        result.is_some(),
        "Expected completion items for class instance modifiers"
    );

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Should include members of BouncingBall that can be modified: g, h, v
        assert!(
            labels.contains(&"g"),
            "Expected 'g' parameter in instance modifier completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"h"),
            "Expected 'h' variable in instance modifier completions, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"v"),
            "Expected 'v' variable in instance modifier completions, got: {:?}",
            labels
        );

        // Should NOT include primitive type modifiers like 'start', 'fixed'
        // (those are for primitive types, not class instances)
        assert!(
            !labels.contains(&"start"),
            "Should NOT have primitive modifiers like 'start' for class instance, got: {:?}",
            labels
        );

        // Should include general modifiers like 'each', 'redeclare', 'final'
        assert!(
            labels.contains(&"each"),
            "Expected 'each' modifier for class instance, got: {:?}",
            labels
        );
    }
}

#[test]
fn test_malformed_modelica() {
    let uri = test_uri();
    let text = "this is not valid modelica {{{{";

    let documents = create_documents(&uri, text);

    // Should handle gracefully
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };
    let result = handle_document_symbols(&documents, params);
    // Should not crash
    let _ = result;

    // Diagnostics should report errors
    let mut workspace = WorkspaceState::new();
    let diags = compute_diagnostics(&uri, text, &mut workspace);
    assert!(!diags.is_empty(), "Expected diagnostics for invalid code");
}

#[test]
fn test_completion_imported_type_members() {
    // Test that member completion works for components whose type is imported from another file
    // This tests the import resolution in get_member_completions

    // File 1: Define a class with some members
    let lib_uri: Uri = "file:///test/MyLib.mo".parse().unwrap();
    let lib_text = r#"package MyLib
  model Controller
    parameter Real gain = 1.0 "Controller gain";
    Real u "Input signal";
    Real y "Output signal";
  equation
    y = gain * u;
  end Controller;
end MyLib;"#;

    // File 2: Import and use the class
    let main_uri: Uri = "file:///test/Main.mo".parse().unwrap();
    let main_text = r#"model Main
  import MyLib.Controller;
  Controller ctrl(gain = 2.0);
equation
  ctrl.u = time;
end Main;"#;

    // Set up workspace with both files
    let mut ws = WorkspaceState::new();
    ws.open_document(lib_uri.clone(), lib_text.to_string());
    ws.open_document(main_uri.clone(), main_text.to_string());

    // Request completion after "ctrl." on line "  ctrl.u = time;"
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: main_uri.clone(),
            },
            position: Position {
                line: 4,
                character: 7,
            }, // after "ctrl."
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: Some(lsp_types::CompletionContext {
            trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
            trigger_character: Some(".".to_string()),
        }),
    };

    let result = rumoca::lsp::handle_completion_workspace(&mut ws, params);
    assert!(
        result.is_some(),
        "Expected completion items for ctrl. (imported type)"
    );

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Should include members from MyLib.Controller: gain, u, y
        assert!(
            labels.contains(&"gain"),
            "Expected 'gain' parameter from imported Controller, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"u"),
            "Expected 'u' variable from imported Controller, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"y"),
            "Expected 'y' variable from imported Controller, got: {:?}",
            labels
        );
    }
}

#[test]
fn test_completion_inherited_members() {
    // Test that member completion includes members from base classes via extends
    // This is a regression test for the inheritance resolution in get_member_completions

    // File 1: Define a base class with some members (similar to Modelica.Blocks.Interfaces.SISO)
    let base_uri: Uri = "file:///test/Interfaces.mo".parse().unwrap();
    let base_text = r#"within MyLib;
package Interfaces
  partial block SISO "Single Input Single Output block"
    Real u "Input signal";
    Real y "Output signal";
  end SISO;
end Interfaces;"#;

    // File 2: Define a derived class that extends the base (similar to PID extending SISO)
    let derived_uri: Uri = "file:///test/Controllers.mo".parse().unwrap();
    let derived_text = r#"within MyLib;
package Controllers
  block PID "PID Controller"
    extends Interfaces.SISO;
    parameter Real K = 1.0 "Gain";
    parameter Real Ti = 0.5 "Integral time";
    parameter Real Td = 0.1 "Derivative time";
  end PID;
end Controllers;"#;

    // File 3: Main file that uses the derived class
    let main_uri: Uri = "file:///test/Main.mo".parse().unwrap();
    let main_text = r#"model Main
  import MyLib.Controllers.PID;
  PID pid1(K = 2.0);
equation
  pid1.u = time;
end Main;"#;

    // Set up workspace with all files
    let mut ws = WorkspaceState::new();
    ws.open_document(base_uri.clone(), base_text.to_string());
    ws.open_document(derived_uri.clone(), derived_text.to_string());
    ws.open_document(main_uri.clone(), main_text.to_string());

    // Request completion after "pid1." on line "  pid1.u = time;"
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: main_uri.clone(),
            },
            position: Position {
                line: 4,
                character: 7,
            }, // after "pid1."
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: Some(lsp_types::CompletionContext {
            trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
            trigger_character: Some(".".to_string()),
        }),
    };

    let result = rumoca::lsp::handle_completion_workspace(&mut ws, params);
    assert!(
        result.is_some(),
        "Expected completion items for pid1. (with inheritance)"
    );

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Should include direct members from PID: K, Ti, Td
        assert!(
            labels.contains(&"K"),
            "Expected 'K' parameter from PID, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"Ti"),
            "Expected 'Ti' parameter from PID, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"Td"),
            "Expected 'Td' parameter from PID, got: {:?}",
            labels
        );

        // Should also include inherited members from SISO: u, y
        assert!(
            labels.contains(&"u"),
            "Expected inherited 'u' from SISO base class, got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"y"),
            "Expected inherited 'y' from SISO base class, got: {:?}",
            labels
        );
    }
}

#[test]
fn test_completion_inherited_members_relative_path() {
    // Test inheritance resolution when extends uses a relative path that needs
    // to be resolved by walking up the package hierarchy.
    // This tests the resolve_type_name_candidates function.

    // File 1: Define a base class in a sibling package (like Modelica.Blocks.Interfaces.SISO)
    let interfaces_uri: Uri = "file:///test/Blocks/Interfaces.mo".parse().unwrap();
    let interfaces_text = r#"within Modelica.Blocks;
package Interfaces
  partial block SISO "Single Input Single Output block"
    Real u "Input signal";
    Real y "Output signal";
  end SISO;
end Interfaces;"#;

    // File 2: Define a derived class that extends using relative path (like PID extends Interfaces.SISO)
    let continuous_uri: Uri = "file:///test/Blocks/Continuous.mo".parse().unwrap();
    let continuous_text = r#"within Modelica.Blocks;
package Continuous
  block PID "PID Controller"
    extends Interfaces.SISO;
    parameter Real k = 1.0 "Gain";
  end PID;
end Continuous;"#;

    // File 3: Main file that uses the derived class
    let main_uri: Uri = "file:///test/Main.mo".parse().unwrap();
    let main_text = r#"model Main
  import Modelica.Blocks.Continuous.PID;
  PID controller(k = 2.0);
equation
  controller.u = time;
end Main;"#;

    // Set up workspace with all files
    let mut ws = WorkspaceState::new();
    ws.open_document(interfaces_uri.clone(), interfaces_text.to_string());
    ws.open_document(continuous_uri.clone(), continuous_text.to_string());
    ws.open_document(main_uri.clone(), main_text.to_string());

    // Request completion after "controller." on line "  controller.u = time;"
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier {
                uri: main_uri.clone(),
            },
            position: Position {
                line: 4,
                character: 13,
            }, // after "controller."
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: Some(lsp_types::CompletionContext {
            trigger_kind: CompletionTriggerKind::TRIGGER_CHARACTER,
            trigger_character: Some(".".to_string()),
        }),
    };

    let result = rumoca::lsp::handle_completion_workspace(&mut ws, params);
    assert!(
        result.is_some(),
        "Expected completion items for controller. (with relative inheritance)"
    );

    if let Some(lsp_types::CompletionResponse::Array(items)) = result {
        let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();

        // Should include direct members from PID: k
        assert!(
            labels.contains(&"k"),
            "Expected 'k' parameter from PID, got: {:?}",
            labels
        );

        // Should also include inherited members from SISO: u, y
        // This specifically tests that Interfaces.SISO is resolved to
        // Modelica.Blocks.Interfaces.SISO (not Modelica.Blocks.Continuous.Interfaces.SISO)
        assert!(
            labels.contains(&"u"),
            "Expected inherited 'u' from SISO (via relative extends path), got: {:?}",
            labels
        );
        assert!(
            labels.contains(&"y"),
            "Expected inherited 'y' from SISO (via relative extends path), got: {:?}",
            labels
        );
    }
}
