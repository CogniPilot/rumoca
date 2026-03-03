//! LEX (Lexical) contract tests - MLS §2
//!
//! Tests for the 13 lexical contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{expect_parse_err_with_code, expect_parse_ok};
use rumoca_contracts::{ContractCategory, TestRunner, create_registry};
use rumoca_phase_parse::parse_to_ast;
use rumoca_session::{Session, SessionConfig};

/// Helper to check whether a source is rejected by parse or compile.
///
/// For lexical contracts, rejection may happen during parse or a later semantic phase.
fn expect_rejected(source: &str, model: &str) {
    if parse_to_ast(source, "test.mo").is_err() {
        return;
    }
    let mut session = Session::new(SessionConfig::default());
    if session.add_document("test.mo", source).is_err() {
        return;
    }
    if session.compile_model(model).is_ok() {
        panic!("Expected source to be rejected by parse or compile for model {model}");
    }
}

fn parses_ok(source: &str) -> bool {
    parse_to_ast(source, "test.mo").is_ok()
}

fn parses_err(source: &str) -> bool {
    parse_to_ast(source, "test.mo").is_err()
}

fn register_parse_ok_case(
    runner: &mut TestRunner,
    contract_id: &'static str,
    source: &'static str,
    failure_message: &'static str,
) {
    runner.register_test(contract_id, move || {
        if parses_ok(source) {
            Ok(())
        } else {
            Err(failure_message.into())
        }
    });
}

fn register_parse_err_case(
    runner: &mut TestRunner,
    contract_id: &'static str,
    source: &'static str,
    failure_message: &'static str,
) {
    runner.register_test(contract_id, move || {
        if parses_err(source) {
            Ok(())
        } else {
            Err(failure_message.into())
        }
    });
}

// =============================================================================
// LEX-001: ASCII identifiers
// "Restricted to Unicode characters corresponding to 7-bit ASCII for identifiers"
// =============================================================================

#[test]
fn lex_001_ascii_identifiers_valid() {
    // Valid ASCII identifiers should parse
    expect_parse_ok("model Test end Test;");
    expect_parse_ok("model ABC123 end ABC123;");
    expect_parse_ok("model _underscore end _underscore;");
}

#[test]
fn lex_001_ascii_identifiers_non_ascii() {
    // MLS contract: non-ASCII identifiers must be rejected by the toolchain.
    expect_rejected("model Tëst end Tëst;", "Tëst");
}

// =============================================================================
// LEX-002: No token whitespace
// "Whitespace cannot occur inside tokens"
// =============================================================================

#[test]
fn lex_002_no_token_whitespace() {
    // Whitespace inside tokens should fail
    // e.g., "1 . 5" is not the same as "1.5"
    expect_parse_err_with_code("model Test Real x = 1 . 5; end Test;", "EP001");

    // Normal spacing is fine
    expect_parse_ok("model Test Real x = 1.5; end Test;");
}

// =============================================================================
// LEX-003: No nested comments
// "Delimited Modelica comments do not nest"
// =============================================================================

#[test]
fn lex_003_no_nested_comments() {
    // Simple comment works
    expect_parse_ok("model Test /* comment */ end Test;");

    // Non-nested comments work
    expect_parse_ok("model Test /* a */ /* b */ end Test;");

    // Nested comment should NOT work as nested (outer closes at first */)
    // "/* outer /* inner */ still outer */" - the "still outer */" is invalid
    let source = "model Test /* outer /* inner */ still outer */ end Test;";
    // This should fail because "still outer */" is not valid syntax
    // The first /* ... */ closes at the first */
    expect_parse_err_with_code(source, "EP001");
}

// =============================================================================
// LEX-004: Case sensitivity
// "Case is significant, i.e., Inductor and inductor are different"
// =============================================================================

#[test]
fn lex_004_case_sensitivity() {
    // Different case = different identifiers
    let source = r#"
model Test
    Real Inductor;
    Real inductor;
end Test;
"#;
    let ast = parse_to_ast(source, "test.mo")
        .expect("Both Inductor and inductor should be valid distinct identifiers");
    let model = ast.classes.get("Test").unwrap();
    // Should have 2 different components
    assert_eq!(model.components.len(), 2);
}

// =============================================================================
// LEX-005: Reserved keywords
// "Keywords are reserved words that cannot be used where IDENT is expected"
// =============================================================================

#[test]
fn lex_005_reserved_keywords() {
    for source in [
        "model Test Real model; end Test;",
        "model Test Real equation; end Test;",
        "model Test Real end; end Test;",
        "model Test Real if; end Test;",
        "model Test Real when; end Test;",
        "model Test Real der; end Test;",
    ] {
        expect_parse_err_with_code(source, "EP001");
    }
}

// =============================================================================
// LEX-006: Reserved type names
// "Not allowed to declare element or enumeration literal with reserved names"
// =============================================================================

#[test]
fn lex_006_reserved_type_names() {
    // Cannot use 'Real' as a variable name (it's a built-in type).
    // Rejection can occur in parsing or later semantic checks.
    expect_rejected("model Test Real Real; end Test;", "Test");
}

// =============================================================================
// LEX-007: Quoted distinct
// "Single quotes are part of identifier: 'x' and x are distinct identifiers"
// =============================================================================

#[test]
fn lex_007_quoted_distinct() {
    // 'x' and x should be different identifiers
    let source = r#"
model Test
    Real x;
    Real 'x';
end Test;
"#;
    let ast = parse_to_ast(source, "test.mo").expect("'x' and x should parse as distinct names");
    let model = ast.classes.get("Test").unwrap();
    assert_eq!(model.components.len(), 2, "'x' and x should be distinct");
}

#[test]
fn lex_007_quoted_with_spaces() {
    // Quoted identifiers can contain spaces
    let source = r#"
model Test
    Real 'my variable';
end Test;
"#;
    expect_parse_ok(source);
}

// =============================================================================
// LEX-008: String concat explicit
// "Concatenation of string literals requires binary expression (+)"
// =============================================================================

#[test]
fn lex_008_string_concat_explicit() {
    // C-style adjacent string concatenation should NOT work
    let source = r#"model Test "desc1" "desc2" end Test;"#;
    expect_parse_err_with_code(source, "EP001");

    // Explicit + concatenation in expressions should work
    let source = r#"
model Test
    String s = "hello" + " " + "world";
end Test;
"#;
    expect_parse_ok(source);
}

// =============================================================================
// LEX-009: Semantic parser check
// "Parsers must implement semantic validation for equation-or-procedure"
// =============================================================================

#[test]
fn lex_009_semantic_parser_check() {
    // This is about semantic validation - equations vs statements
    // := should only appear in algorithms, not equations
    let source = r#"
model Test
    Real x;
equation
    x := 1;  // Assignment in equation section - should fail
end Test;
"#;
    expect_rejected(source, "Test");
}

// =============================================================================
// LEX-010: Float range minimum
// "At least IEEE double precision range"
// =============================================================================

#[test]
fn lex_010_float_range() {
    // Very large float should parse
    let source = "model Test parameter Real x = 1e308; end Test;";
    expect_parse_ok(source);

    // Very small float should parse
    let source = "model Test parameter Real x = 1e-308; end Test;";
    expect_parse_ok(source);

    // Standard floats
    let source = "model Test parameter Real x = 3.14159265358979; end Test;";
    expect_parse_ok(source);
}

// =============================================================================
// LEX-011: Integer range minimum
// =============================================================================

#[test]
fn lex_011_integer_range() {
    // Large integers should parse
    let source = "model Test parameter Integer x = 2147483647; end Test;";
    expect_parse_ok(source);

    // Negative integers
    let source = "model Test parameter Integer x = -2147483648; end Test;";
    expect_parse_ok(source);
}

// =============================================================================
// LEX-012: Boolean literals only
// "Only true and false permitted as Boolean literals"
// =============================================================================

#[test]
fn lex_012_boolean_literals() {
    // true and false should work
    let source = "model Test parameter Boolean a = true; parameter Boolean b = false; end Test;";
    expect_parse_ok(source);

    // Non-literal Boolean identifiers (e.g. `True`) should be rejected.
    expect_rejected("model Test Boolean b = True; end Test;", "Test");
}

// =============================================================================
// LEX-013: String escapes required
// "Backslash escape sequences required"
// =============================================================================

#[test]
fn lex_013_string_escapes() {
    // Basic escapes should work
    let source = r#"model Test String s = "hello\nworld"; end Test;"#;
    expect_parse_ok(source);

    // Tab escape
    let source = r#"model Test String s = "col1\tcol2"; end Test;"#;
    expect_parse_ok(source);

    // Quote escape
    let source = r#"model Test String s = "say \"hello\""; end Test;"#;
    expect_parse_ok(source);

    // Backslash escape
    let source = r#"model Test String s = "path\\file"; end Test;"#;
    expect_parse_ok(source);
}

// =============================================================================
// Integration test: Run all LEX contracts through the test runner
// =============================================================================

#[test]
fn test_lex_contracts_runner() {
    let registry = create_registry();
    let mut runner = TestRunner::new(registry);

    // Register all LEX contract tests.
    register_parse_ok_case(
        &mut runner,
        "LEX-001",
        "model Test end Test;",
        "ASCII identifiers should parse",
    );
    register_parse_err_case(
        &mut runner,
        "LEX-002",
        "model Test Real x = 1 . 5; end Test;",
        "Whitespace in tokens should fail",
    );
    register_parse_ok_case(
        &mut runner,
        "LEX-003",
        "model Test /* comment */ end Test;",
        "Simple comments should work",
    );

    runner.register_test("LEX-004", || {
        let source = "model Test Real Inductor; Real inductor; end Test;";
        let Ok(ast) = parse_to_ast(source, "test.mo") else {
            return Err("Case sensitivity not working".into());
        };
        if ast.classes.get("Test").map(|m| m.components.len()) == Some(2) {
            return Ok(());
        }
        Err("Case sensitivity not working".into())
    });

    register_parse_err_case(
        &mut runner,
        "LEX-005",
        "model Test Real model; end Test;",
        "Reserved keywords should not be usable as identifiers",
    );
    register_parse_ok_case(
        &mut runner,
        "LEX-010",
        "model Test parameter Real x = 1e308; end Test;",
        "Large floats should parse",
    );
    register_parse_ok_case(
        &mut runner,
        "LEX-011",
        "model Test parameter Integer x = 2147483647; end Test;",
        "Large integers should parse",
    );
    register_parse_ok_case(
        &mut runner,
        "LEX-012",
        "model Test parameter Boolean a = true; parameter Boolean b = false; end Test;",
        "Boolean literals should parse",
    );
    register_parse_ok_case(
        &mut runner,
        "LEX-013",
        r#"model Test String s = "hello\nworld"; end Test;"#,
        "String escapes should work",
    );

    // Run tests for the Lexical category
    runner.run_category(ContractCategory::Lexical);

    // Report results
    println!(
        "LEX contracts: {} passed, {} failed",
        runner.passed_count(),
        runner.failed_count()
    );

    assert_eq!(
        runner.results().len(),
        runner.test_count(),
        "All registered LEX runner tests should execute"
    );
    assert_eq!(
        runner.failed_count(),
        0,
        "LEX runner test failures detected"
    );
    assert_eq!(
        runner.passed_count(),
        runner.test_count(),
        "All registered LEX runner tests must pass"
    );
}
