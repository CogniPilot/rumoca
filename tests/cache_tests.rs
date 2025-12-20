//! Tests for DAE cache invalidation.
//!
//! Verifies that cached DAE results are properly invalidated when source files change.

use std::fs;
use std::io::Write;
use tempfile::NamedTempFile;

use rumoca::compiler::pipeline::{check_balance_only, clear_dae_cache};
use rumoca::compiler::{Compiler, parse_source};
use rumoca::ir::transform::flatten::clear_caches;

/// Test that the DAE cache correctly invalidates when a source file changes.
#[test]
fn test_dae_cache_invalidation() {
    // Clear all caches to start fresh
    clear_caches();
    clear_dae_cache();

    // Create a temporary file with a simple balanced model
    let mut temp_file = NamedTempFile::new().expect("Failed to create temp file");
    let initial_model = r#"
model TestModel
    Real x(start = 0);
equation
    der(x) = 1;
end TestModel;
"#;
    temp_file
        .write_all(initial_model.as_bytes())
        .expect("Failed to write initial model");
    temp_file.flush().expect("Failed to flush");

    let path = temp_file.path().to_str().unwrap();

    // Parse the model
    let def1 = parse_source(initial_model, path).expect("Failed to parse initial model");

    // First call - should compute and cache
    let result1 = check_balance_only(&def1, Some("TestModel")).expect("First balance check failed");
    assert!(result1.is_balanced(), "Initial model should be balanced");
    assert_eq!(result1.num_states, 1, "Should have 1 state");
    assert_eq!(result1.num_equations, 1, "Should have 1 equation");

    // Second call with same content - should use cache
    let result2 =
        check_balance_only(&def1, Some("TestModel")).expect("Second balance check failed");
    assert_eq!(
        result1.num_states, result2.num_states,
        "Cached result should match"
    );

    // Now modify the file to have a different model (2 states instead of 1)
    let modified_model = r#"
model TestModel
    Real x(start = 0);
    Real y(start = 0);
equation
    der(x) = 1;
    der(y) = x;
end TestModel;
"#;

    // Write the modified content
    fs::write(temp_file.path(), modified_model).expect("Failed to write modified model");

    // Clear the file hash cache so it re-reads the file
    clear_caches();

    // Parse the modified model
    let def2 = parse_source(modified_model, path).expect("Failed to parse modified model");

    // This should NOT use the old cache because the file changed
    let result3 = check_balance_only(&def2, Some("TestModel")).expect("Third balance check failed");

    assert!(result3.is_balanced(), "Modified model should be balanced");
    assert_eq!(
        result3.num_states, 2,
        "Modified model should have 2 states, got {}",
        result3.num_states
    );
    assert_eq!(
        result3.num_equations, 2,
        "Modified model should have 2 equations"
    );

    // Verify the results are different
    assert_ne!(
        result1.num_states, result3.num_states,
        "Cache should have been invalidated - states should differ"
    );
}

/// Test that cache invalidation works with the full Compiler API
#[test]
fn test_compiler_cache_invalidation() {
    // Clear all caches
    clear_caches();
    clear_dae_cache();

    // Create initial model
    let mut temp_file = NamedTempFile::new().expect("Failed to create temp file");
    let initial_model = r#"
model CacheTest
    parameter Real k = 1.0;
    Real x(start = 0);
equation
    der(x) = k;
end CacheTest;
"#;
    temp_file
        .write_all(initial_model.as_bytes())
        .expect("Failed to write");
    temp_file.flush().expect("Failed to flush");

    let path = temp_file.path().to_str().unwrap();

    // Compile initial model
    let result1 = Compiler::new()
        .model("CacheTest")
        .compile_str(initial_model, path)
        .expect("First compile failed");

    assert!(result1.is_balanced());
    assert_eq!(result1.balance.num_states, 1);
    assert_eq!(result1.balance.num_parameters, 1);

    // Modify to add another parameter
    let modified_model = r#"
model CacheTest
    parameter Real k = 1.0;
    parameter Real m = 2.0;
    Real x(start = 0);
equation
    der(x) = k * m;
end CacheTest;
"#;

    fs::write(temp_file.path(), modified_model).expect("Failed to write modified");
    clear_caches();

    // Compile modified model
    let result2 = Compiler::new()
        .model("CacheTest")
        .compile_str(modified_model, path)
        .expect("Second compile failed");

    assert!(result2.is_balanced());
    assert_eq!(result2.balance.num_states, 1);
    assert_eq!(
        result2.balance.num_parameters, 2,
        "Should have 2 parameters after modification"
    );
}

/// Test that FileDependencies correctly detects file changes
#[test]
fn test_file_dependencies_validation() {
    use rumoca::ir::transform::flatten::FileDependencies;

    // Create a temp file
    let mut temp_file = NamedTempFile::new().expect("Failed to create temp file");
    temp_file
        .write_all(b"initial content")
        .expect("Failed to write");
    temp_file.flush().expect("Failed to flush");

    let path = temp_file.path().to_str().unwrap();

    // Clear file hash cache to ensure fresh computation
    clear_caches();

    // Create dependencies with the current file hash
    let mut deps = FileDependencies::new();

    // Read current content and compute hash
    let content = fs::read(temp_file.path()).expect("Failed to read");
    let hash = format!("{:x}", chksum_md5::hash(&content));
    deps.record(path, &hash);

    // Dependencies should be valid
    assert!(deps.is_valid(), "Dependencies should be valid initially");

    // Modify the file
    fs::write(temp_file.path(), "modified content").expect("Failed to write modified");

    // Clear file hash cache so is_valid() recomputes
    clear_caches();

    // Dependencies should now be invalid
    assert!(
        !deps.is_valid(),
        "Dependencies should be invalid after file modification"
    );
}

/// Test that missing files are detected as invalid
#[test]
fn test_file_dependencies_missing_file() {
    use rumoca::ir::transform::flatten::FileDependencies;

    clear_caches();

    let mut deps = FileDependencies::new();
    deps.record("/nonexistent/path/to/file.mo", "somehash");

    // Should be invalid because file doesn't exist
    assert!(
        !deps.is_valid(),
        "Dependencies with missing file should be invalid"
    );
}
