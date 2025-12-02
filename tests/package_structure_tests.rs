use rumoca::Compiler;
use std::path::Path;

#[test]
fn test_discover_package_files() {
    use rumoca::ir::transform::multi_file::{discover_package_files, parse_package_order};

    let package_dir = Path::new("tests/fixtures/package_structure/MyLib");

    // First check that package.order is parsed correctly
    let order = parse_package_order(&package_dir.join("package.order")).unwrap();
    assert_eq!(order, vec!["Types", "Functions", "Examples"]);

    // Discover all files in the package
    let files = discover_package_files(package_dir).unwrap();

    // Should have: package.mo, Types.mo, Functions.mo, Examples/package.mo, Examples/SimpleModel.mo, Examples/AdvancedModel.mo
    assert_eq!(files.len(), 6, "Expected 6 files, got: {:?}", files);

    // Check ordering - package.mo should be first
    assert!(
        files[0].ends_with("package.mo"),
        "First file should be package.mo"
    );

    // Types should come before Functions (per package.order)
    let types_idx = files.iter().position(|f| f.ends_with("Types.mo")).unwrap();
    let functions_idx = files
        .iter()
        .position(|f| f.ends_with("Functions.mo"))
        .unwrap();
    assert!(
        types_idx < functions_idx,
        "Types.mo should come before Functions.mo"
    );

    // Examples should come after Functions
    let examples_pkg_idx = files
        .iter()
        .position(|f| f.ends_with("Examples/package.mo"))
        .unwrap();
    assert!(
        functions_idx < examples_pkg_idx,
        "Functions.mo should come before Examples/"
    );
}

#[test]
fn test_compile_package_simple_model() {
    let result = Compiler::new()
        .model("MyLib.Examples.SimpleModel")
        .compile_package("tests/fixtures/package_structure/MyLib");

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    // SimpleModel should have one state variable x
    assert!(result.dae.x.contains_key("x"), "Should have state x");
}

#[test]
fn test_compile_package_advanced_model() {
    let result = Compiler::new()
        .model("MyLib.Examples.AdvancedModel")
        .compile_package("tests/fixtures/package_structure/MyLib");

    assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

    let result = result.unwrap();
    // AdvancedModel should have two state variables x and y
    assert!(result.dae.x.contains_key("x"), "Should have state x");
    assert!(result.dae.x.contains_key("y"), "Should have state y");
}

#[test]
fn test_package_order_respected() {
    use rumoca::ir::transform::multi_file::discover_package_files;

    let examples_dir = Path::new("tests/fixtures/package_structure/MyLib/Examples");
    let files = discover_package_files(examples_dir).unwrap();

    // The package.order specifies: SimpleModel, AdvancedModel
    let simple_idx = files
        .iter()
        .position(|f| f.ends_with("SimpleModel.mo"))
        .unwrap();
    let advanced_idx = files
        .iter()
        .position(|f| f.ends_with("AdvancedModel.mo"))
        .unwrap();

    assert!(
        simple_idx < advanced_idx,
        "SimpleModel.mo should come before AdvancedModel.mo (per package.order)"
    );
}

#[test]
fn test_modelica_path_lookup() {
    use rumoca::ir::transform::multi_file::find_package_in_modelica_path;

    // Save current MODELICAPATH
    let original = std::env::var("MODELICAPATH").ok();

    // SAFETY: Test is single-threaded, no concurrent env var access
    unsafe {
        std::env::set_var("MODELICAPATH", "tests/fixtures/package_structure");
    }

    // Should find MyLib
    let found = find_package_in_modelica_path("MyLib");
    assert!(found.is_some(), "Should find MyLib in MODELICAPATH");

    let path = found.unwrap();
    assert!(path.ends_with("MyLib"), "Path should end with MyLib");

    // Should not find NonExistent
    let not_found = find_package_in_modelica_path("NonExistent");
    assert!(not_found.is_none(), "Should not find NonExistent");

    // Restore original MODELICAPATH
    // SAFETY: Test is single-threaded, no concurrent env var access
    unsafe {
        if let Some(val) = original {
            std::env::set_var("MODELICAPATH", val);
        } else {
            std::env::remove_var("MODELICAPATH");
        }
    }
}
