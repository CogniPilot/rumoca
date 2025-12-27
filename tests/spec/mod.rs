//! Modelica Language Specification Conformance Test Suite
//!
//! This module provides comprehensive tests for validating rumoca's conformance
//! to the Modelica Language Specification (MLS) Version 3.7-dev.
//!
//! # Reference
//!
//! - Spec: https://specification.modelica.org/master/
//! - GitHub: https://github.com/modelica/ModelicaSpecification/tree/master
//!
//! # Structure
//!
//! Tests are organized to mirror the MLS chapter structure:
//! - `chapter_01_scope` - Scope and Objectives (MLS Chapter 1)
//! - `chapter_02_lexical` - Lexical Structure (MLS Chapter 2)
//! - `chapter_03_operators` - Operators and Expressions (MLS Chapter 3)
//! - `chapter_04_classes` - Classes, Predefined Types, Declarations (MLS Chapter 4)
//! - `chapter_05_scoping` - Scoping, Name Lookup, Flattening (MLS Chapter 5)
//! - `chapter_07_inheritance` - Inheritance, Modification, Redeclaration (MLS Chapter 7)
//! - `chapter_08_equations` - Equations (MLS Chapter 8)
//! - `chapter_09_connectors` - Connectors and Connections (MLS Chapter 9)
//! - `chapter_10_arrays` - Arrays (MLS Chapter 10)
//! - `chapter_11_algorithms` - Statements and Algorithm Sections (MLS Chapter 11)
//! - `chapter_12_functions` - Functions (MLS Chapter 12)
//!
//! # Test Naming Convention
//!
//! Tests follow the pattern: `mls_<section>_<description>`
//! Example: `mls_2_3_1_basic_identifier` for MLS §2.3.1
//!
//! # Running Tests
//!
//! ```bash
//! # Run all specification tests
//! cargo test --test specification_tests
//!
//! # Run specific chapter
//! cargo test --test specification_tests chapter_02
//!
//! # Run specific section
//! cargo test --test specification_tests mls_2_3
//! ```

// Allow dead_code for shared utilities
#![allow(dead_code)]

use rumoca::Compiler;
use rumoca::ir::ast::ClassType;

// ============================================================================
// TEST UTILITIES
// ============================================================================

/// Compile a model and expect success
pub fn expect_success(source: &str, model: &str) {
    let result = Compiler::new().model(model).compile_str(source, "test.mo");
    assert!(
        result.is_ok(),
        "Expected '{}' to compile successfully.\nError: {:?}\nSource:\n{}",
        model,
        result.err(),
        source
    );
}

/// Compile a model and expect failure (semantic error)
pub fn expect_failure(source: &str, model: &str) {
    let result = Compiler::new().model(model).compile_str(source, "test.mo");
    assert!(
        result.is_err(),
        "Expected '{}' to fail compilation, but it succeeded.\nSource:\n{}",
        model,
        source
    );
}

/// Parse source and expect success (syntax only, no compilation)
pub fn expect_parse_success(source: &str) {
    let result = rumoca::parse_source(source, "test.mo");
    assert!(
        result.is_ok(),
        "Expected source to parse successfully.\nError: {:?}\nSource:\n{}",
        result.err(),
        source
    );
}

/// Parse source and expect failure (syntax error)
pub fn expect_parse_failure(source: &str) {
    let result = rumoca::parse_source(source, "test.mo");
    assert!(
        result.is_err(),
        "Expected source to fail parsing, but it succeeded.\nSource:\n{}",
        source
    );
}

/// Check that a model is balanced (equal equations and unknowns)
pub fn expect_balanced(source: &str, model: &str) {
    let result = Compiler::new()
        .model(model)
        .compile_str(source, "test.mo")
        .expect("Failed to compile");
    assert!(
        result.is_balanced(),
        "Expected '{}' to be balanced. Status: {}",
        model,
        result.balance_status()
    );
}

/// Check that a model is unbalanced
pub fn expect_unbalanced(source: &str, model: &str) {
    let result = Compiler::new()
        .model(model)
        .compile_str(source, "test.mo")
        .expect("Failed to compile");
    assert!(
        !result.is_balanced(),
        "Expected '{}' to be unbalanced, but it was balanced.",
        model
    );
}

// ============================================================================
// PHASE 10: ENHANCED TEST UTILITIES
// ============================================================================

/// Check that a model is balanced with specific equation and unknown counts
///
/// This is useful for verifying that equation counting is correct for specific
/// model structures.
///
/// # Arguments
///
/// * `source` - The Modelica source code
/// * `model` - The model name to compile
/// * `expected_eq` - Expected number of equations
/// * `expected_unk` - Expected number of unknowns
pub fn expect_balanced_with_count(
    source: &str,
    model: &str,
    expected_eq: usize,
    expected_unk: usize,
) {
    let result = Compiler::new()
        .model(model)
        .compile_str(source, "test.mo")
        .expect("Failed to compile");

    let balance = result.dae().check_balance();

    assert!(
        result.is_balanced(),
        "Expected '{}' to be balanced. Status: {}",
        model,
        result.balance_status()
    );

    assert_eq!(
        balance.num_equations, expected_eq,
        "Expected {} equations but got {} for model '{}'",
        expected_eq, balance.num_equations, model
    );

    assert_eq!(
        balance.num_unknowns, expected_unk,
        "Expected {} unknowns but got {} for model '{}'",
        expected_unk, balance.num_unknowns, model
    );
}

/// Check that a model has specific equation and unknown counts (without requiring balance)
///
/// This is useful for testing partial models or verifying counts independently
/// of balance status.
pub fn expect_counts(source: &str, model: &str, expected_eq: usize, expected_unk: usize) {
    let result = Compiler::new()
        .model(model)
        .compile_str(source, "test.mo")
        .expect("Failed to compile");

    let balance = result.dae().check_balance();

    assert_eq!(
        balance.num_equations,
        expected_eq,
        "Expected {} equations but got {} for model '{}'.\nBalance: {}",
        expected_eq,
        balance.num_equations,
        model,
        result.balance_status()
    );

    assert_eq!(
        balance.num_unknowns,
        expected_unk,
        "Expected {} unknowns but got {} for model '{}'.\nBalance: {}",
        expected_unk,
        balance.num_unknowns,
        model,
        result.balance_status()
    );
}

/// Compile a model and expect failure with a specific error message substring
///
/// This is useful for verifying that error messages are helpful and descriptive.
///
/// # Arguments
///
/// * `source` - The Modelica source code
/// * `model` - The model name to compile
/// * `expected_message` - A substring that should appear in the error message
pub fn expect_failure_with_message(source: &str, model: &str, expected_message: &str) {
    let result = Compiler::new().model(model).compile_str(source, "test.mo");

    match result {
        Ok(_) => panic!(
            "Expected '{}' to fail compilation, but it succeeded.\nSource:\n{}",
            model, source
        ),
        Err(e) => {
            let error_msg = format!("{:?}", e);
            assert!(
                error_msg.contains(expected_message),
                "Expected error message to contain '{}' but got:\n{}\nSource:\n{}",
                expected_message,
                error_msg,
                source
            );
        }
    }
}

/// Parse source and expect failure with a specific error message substring
pub fn expect_parse_failure_with_message(source: &str, expected_message: &str) {
    let result = rumoca::parse_source(source, "test.mo");

    match result {
        Ok(_) => panic!(
            "Expected source to fail parsing, but it succeeded.\nSource:\n{}",
            source
        ),
        Err(e) => {
            let error_msg = format!("{:?}", e);
            assert!(
                error_msg.contains(expected_message),
                "Expected error message to contain '{}' but got:\n{}\nSource:\n{}",
                expected_message,
                error_msg,
                source
            );
        }
    }
}

// ============================================================================
// AST VERIFICATION UTILITIES
// ============================================================================

/// Compile a model and return the result for detailed AST inspection
pub fn compile(source: &str, model: &str) -> rumoca::compiler::CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(source, "test.mo")
        .unwrap_or_else(|e| {
            panic!(
                "Failed to compile '{}'.\nError: {:?}\nSource:\n{}",
                model, e, source
            )
        })
}

/// Parse source and return the StoredDefinition for AST inspection
pub fn parse(source: &str) -> rumoca::ir::ast::StoredDefinition {
    rumoca::parse_source(source, "test.mo").unwrap_or_else(|e| {
        panic!(
            "Failed to parse source.\nError: {:?}\nSource:\n{}",
            e, source
        )
    })
}

/// Compile and verify class type of the top-level class
pub fn expect_class_type(source: &str, model: &str, expected_type: ClassType) {
    let def = parse(source);
    let class = def
        .class_list
        .get(model)
        .unwrap_or_else(|| panic!("Class '{}' not found in parsed AST", model));
    assert_eq!(
        class.class_type, expected_type,
        "Expected class type {:?} but got {:?} for model '{}'",
        expected_type, class.class_type, model
    );
}

/// Compile and verify that a component exists with the expected type
pub fn expect_component(source: &str, model: &str, component_name: &str, expected_type: &str) {
    let result = compile(source, model);
    let class = &result.expanded_class;
    let component = class.components.get(component_name).unwrap_or_else(|| {
        panic!(
            "Component '{}' not found in flattened class '{}'",
            component_name, model
        )
    });
    assert_eq!(
        component.type_name.to_string(),
        expected_type,
        "Expected component '{}' to have type '{}' but got '{}'",
        component_name,
        expected_type,
        component.type_name
    );
}

/// Compile and verify the number of equations
pub fn expect_equation_count(source: &str, model: &str, expected_count: usize) {
    let result = compile(source, model);
    let actual_count = result.expanded_class.equations.len();
    assert_eq!(
        actual_count, expected_count,
        "Expected {} equations but got {} for model '{}'",
        expected_count, actual_count, model
    );
}

/// Compile and verify the number of components
pub fn expect_component_count(source: &str, model: &str, expected_count: usize) {
    let result = compile(source, model);
    let actual_count = result.expanded_class.components.len();
    assert_eq!(
        actual_count, expected_count,
        "Expected {} components but got {} for model '{}'",
        expected_count, actual_count, model
    );
}

/// Compile and verify that a component does NOT exist (for testing removal)
pub fn expect_no_component(source: &str, model: &str, component_name: &str) {
    let result = compile(source, model);
    assert!(
        !result
            .expanded_class
            .components
            .contains_key(component_name),
        "Expected component '{}' to NOT exist in flattened class '{}' but it was found",
        component_name,
        model
    );
}

/// Multi-file test: compile multiple source files together
///
/// This enables testing of:
/// - Import cycle detection
/// - Cross-file references
/// - Package structure
///
/// # Arguments
///
/// * `files` - Array of (filename, source) tuples
/// * `main_model` - The main model to compile
///
/// # Example
///
/// ```ignore
/// expect_multi_file_success(&[
///     ("Package.mo", "package P constant Real x = 1; end P;"),
///     ("Model.mo", "model M import P; Real y = P.x; end M;"),
/// ], "M");
/// ```
pub fn expect_multi_file_success(files: &[(&str, &str)], main_model: &str) {
    use std::fs;
    use tempfile::TempDir;

    // Create a temporary directory for the test files
    let temp_dir = TempDir::new().expect("Failed to create temp directory");

    // Write all files to the temp directory
    let mut file_paths: Vec<String> = Vec::new();
    for (filename, source) in files {
        let path = temp_dir.path().join(filename);
        fs::write(&path, source).expect("Failed to write test file");
        file_paths.push(path.to_string_lossy().to_string());
    }

    // Compile with all files
    let path_refs: Vec<&str> = file_paths.iter().map(|s| s.as_str()).collect();
    let result = Compiler::new().model(main_model).compile_files(&path_refs);

    assert!(
        result.is_ok(),
        "Expected multi-file compilation to succeed for model '{}'.\nError: {:?}",
        main_model,
        result.err()
    );
}

/// Multi-file test: expect compilation failure
pub fn expect_multi_file_failure(files: &[(&str, &str)], main_model: &str) {
    use std::fs;
    use tempfile::TempDir;

    let temp_dir = TempDir::new().expect("Failed to create temp directory");

    let mut file_paths: Vec<String> = Vec::new();
    for (filename, source) in files {
        let path = temp_dir.path().join(filename);
        fs::write(&path, source).expect("Failed to write test file");
        file_paths.push(path.to_string_lossy().to_string());
    }

    let path_refs: Vec<&str> = file_paths.iter().map(|s| s.as_str()).collect();
    let result = Compiler::new().model(main_model).compile_files(&path_refs);

    assert!(
        result.is_err(),
        "Expected multi-file compilation to fail for model '{}', but it succeeded.",
        main_model
    );
}

// ============================================================================
// CHAPTER MODULES (folder-based for maintainability)
// ============================================================================

pub mod chapter_01;
pub mod chapter_02;
pub mod chapter_03;
pub mod chapter_04;
pub mod chapter_05;
pub mod chapter_06;
pub mod chapter_07;
pub mod chapter_08;
pub mod chapter_09;
pub mod chapter_10;
pub mod chapter_11;
pub mod chapter_12;
pub mod chapter_13;
pub mod chapter_14;
pub mod chapter_15;
pub mod chapter_16;
pub mod chapter_17;
pub mod chapter_18;
pub mod integration;
