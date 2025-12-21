//! Common test utilities for rumoca integration tests.
//!
//! This module provides standardized helpers for:
//! - Parsing test files from fixtures
//! - Creating DAEs from models
//! - Setting up LSP test environments
//! - Common assertions and test data

// Allow dead_code - this is a shared utility module where different tests use different subsets
#![allow(dead_code)]

use anyhow::Result;
use rumoca::ir::ast::StoredDefinition;
use rumoca::modelica_grammar::ModelicaGrammar;
use rumoca::modelica_parser::parse;
use std::fs;

// =============================================================================
// Fixture Path Helpers
// =============================================================================

/// Get the path to a fixture file
pub fn fixture_path(name: &str) -> String {
    format!("tests/fixtures/{}.mo", name)
}

/// Get the path to a fixture file with custom extension
pub fn fixture_path_ext(name: &str, ext: &str) -> String {
    format!("tests/fixtures/{}.{}", name, ext)
}

// =============================================================================
// Parsing Helpers
// =============================================================================

/// Parse a test file from the fixtures directory
pub fn parse_test_file(name: &str) -> Result<StoredDefinition> {
    let path = fixture_path(name);
    let input = fs::read_to_string(&path)
        .map_err(|e| anyhow::anyhow!("Failed to read test file {}: {}", path, e))?;

    let mut grammar = ModelicaGrammar::new();
    parse(&input, &path, &mut grammar)?;

    grammar
        .modelica
        .ok_or_else(|| anyhow::anyhow!("Parser succeeded but produced no AST for {}", path))
}

/// Parse Modelica source code directly (for inline test models)
pub fn parse_source(source: &str) -> Result<StoredDefinition> {
    let mut grammar = ModelicaGrammar::new();
    parse(source, "<test>", &mut grammar)?;

    grammar
        .modelica
        .ok_or_else(|| anyhow::anyhow!("Parser succeeded but produced no AST"))
}

// =============================================================================
// Compiler Helpers
// =============================================================================

/// Compile a fixture file with the given model name
pub fn compile_fixture(
    fixture_name: &str,
    model_name: &str,
) -> Result<rumoca::compiler::CompilationResult> {
    let path = fixture_path(fixture_name);
    rumoca::Compiler::new()
        .model(model_name)
        .compile_file(&path)
        .map_err(|e| anyhow::anyhow!("Failed to compile {}: {}", fixture_name, e))
}

/// Compile inline source code with the given model name
pub fn compile_source(
    source: &str,
    model_name: &str,
) -> Result<rumoca::compiler::CompilationResult> {
    rumoca::Compiler::new()
        .model(model_name)
        .compile_str(source, "<test>")
        .map_err(|e| anyhow::anyhow!("Failed to compile source: {}", e))
}

/// Compile an MSL model (requires MODELICAPATH to be set)
///
/// This compiles a model from the Modelica Standard Library by:
/// 1. Including the Modelica package from MODELICAPATH
/// 2. Compiling a minimal "within;" file with the specified model
pub fn compile_msl_model(model_name: &str) -> Result<rumoca::compiler::CompilationResult> {
    rumoca::Compiler::new()
        .model(model_name)
        .include_from_modelica_path("Modelica")?
        .compile_str("within;", "<msl_entry>")
        .map_err(|e| anyhow::anyhow!("Failed to compile MSL model {}: {}", model_name, e))
}

/// Flatten a fixture file and return the flat class for inspection
pub fn flatten_fixture(
    fixture_name: &str,
    model_name: &str,
) -> Result<rumoca::ir::ast::ClassDefinition> {
    use rumoca::ir::transform::flatten::flatten;
    let def = parse_test_file(fixture_name)?;
    flatten(&def, Some(model_name)).map_err(|e| anyhow::anyhow!("Flatten failed: {}", e))
}

// =============================================================================
// DAE Helpers
// =============================================================================

/// Create a DAE from a fixture file
pub fn create_dae_from_fixture(
    fixture_name: &str,
    model_name: &str,
) -> Result<rumoca::dae::ast::Dae> {
    use rumoca::ir::structural::create_dae::create_dae;
    use rumoca::ir::transform::flatten::flatten;

    let def = parse_test_file(fixture_name)?;
    let mut fclass = flatten(&def, Some(model_name))?;
    create_dae(&mut fclass)
}

// =============================================================================
// LSP Test Helpers
// =============================================================================

pub mod lsp {
    use lsp_types::Uri;

    /// Create a test URI for LSP tests
    pub fn test_uri() -> Uri {
        "file:///tmp/test.mo".parse().unwrap()
    }

    /// Create a test URI with a custom filename
    pub fn test_uri_named(name: &str) -> Uri {
        format!("file:///tmp/{}.mo", name).parse().unwrap()
    }

    /// Create a workspace state with a single document
    pub fn create_workspace_with_doc(
        uri: &Uri,
        content: &str,
    ) -> rumoca::lsp::workspace::WorkspaceState {
        let mut workspace = rumoca::lsp::workspace::WorkspaceState::new();
        workspace.open_document(uri.clone(), content.to_string());
        workspace
    }

    /// Helper to create TextDocumentIdentifier
    pub fn text_document_id(uri: &Uri) -> lsp_types::TextDocumentIdentifier {
        lsp_types::TextDocumentIdentifier { uri: uri.clone() }
    }

    /// Helper to create TextDocumentPositionParams
    pub fn position_params(
        uri: &Uri,
        line: u32,
        character: u32,
    ) -> lsp_types::TextDocumentPositionParams {
        lsp_types::TextDocumentPositionParams {
            text_document: text_document_id(uri),
            position: lsp_types::Position { line, character },
        }
    }
}

// =============================================================================
// Test Assertion Helpers
// =============================================================================

/// Assert that a model parses successfully
pub fn assert_parses(fixture_name: &str) {
    parse_test_file(fixture_name)
        .unwrap_or_else(|e| panic!("Failed to parse {}: {}", fixture_name, e));
}

/// Assert that a model compiles successfully
pub fn assert_compiles(fixture_name: &str, model_name: &str) {
    compile_fixture(fixture_name, model_name)
        .unwrap_or_else(|e| panic!("Failed to compile {}/{}: {}", fixture_name, model_name, e));
}

/// Assert that a model is balanced
pub fn assert_balanced(fixture_name: &str, model_name: &str) {
    let result = compile_fixture(fixture_name, model_name)
        .unwrap_or_else(|e| panic!("Failed to compile {}/{}: {}", fixture_name, model_name, e));
    assert!(
        result.is_balanced(),
        "{}/{} should be balanced: {}",
        fixture_name,
        model_name,
        result.balance_status()
    );
}

// =============================================================================
// Test Data - Common Model Snippets
// =============================================================================

/// A minimal valid model for quick tests
pub const MINIMAL_MODEL: &str = r#"model Test
  Real x;
equation
  der(x) = 1;
end Test;"#;

/// A model with parameters
pub const MODEL_WITH_PARAMS: &str = r#"model TestParams
  parameter Real k = 1.0;
  Real x;
equation
  der(x) = k;
end TestParams;"#;

/// A model with a when clause
pub const MODEL_WITH_WHEN: &str = r#"model TestWhen
  Real x(start = 0);
  discrete Real y;
equation
  der(x) = 1;
  when x > 1 then
    y = pre(y) + 1;
  end when;
end TestWhen;"#;

/// A model with nested classes
pub const MODEL_WITH_NESTED: &str = r#"package TestPackage
  model Inner
    Real x;
  equation
    der(x) = 1;
  end Inner;

  model Outer
    Inner inner;
  end Outer;
end TestPackage;"#;

// =============================================================================
// List of Standard Test Fixtures
// =============================================================================

/// List of all standard test fixture names (without .mo extension)
pub const STANDARD_FIXTURES: &[&str] = &[
    "integrator",
    "bouncing_ball",
    "rover",
    "quadrotor",
    "simple_circuit",
    "nightvapor",
];

/// List of fixtures that should parse successfully
pub const PARSEABLE_FIXTURES: &[&str] = &[
    "integrator",
    "bouncing_ball",
    "rover",
    "quadrotor",
    "simple_circuit",
    "nightvapor",
    "for_equation",
    "if_expression",
    "imports",
    "initial_equation",
    "matrix_test",
    "noevent_smooth",
    "packages",
    "scoping_test",
];
