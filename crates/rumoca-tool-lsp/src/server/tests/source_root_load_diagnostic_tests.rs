//! Source-root load failures must reach the editor as plain LSP diagnostics
//! keyed by the offending file, with the load warning deferring to them instead
//! of repeating rendered miette output.

use super::*;

#[test]
fn package_layout_errors_map_to_plain_lsp_diagnostics() {
    let temp = new_temp_dir("package-layout-diag");
    let lib = temp.join("Modelica");
    std::fs::create_dir_all(&lib).expect("mkdir");
    std::fs::write(lib.join("package.mo"), "package Modelica end Modelica;")
        .expect("write package");
    std::fs::write(lib.join("A.mo"), "model A end A;").expect("write child");

    let err = parse_source_root_with_cache(&lib).expect_err("missing within must fail");
    let layout = err
        .downcast_ref::<PackageLayoutError>()
        .expect("package layout error type must be preserved");
    let diagnostics = source_root_load_diagnostics_for_package_layout_error(layout);
    let file_key = canonical_path_key(&lib.join("A.mo").to_string_lossy());
    let file_diagnostics = diagnostics
        .get(&file_key)
        .expect("source-backed package-layout diagnostic should be keyed by file");
    assert!(
        file_diagnostics
            .iter()
            .any(|diag| diag.code.as_ref() == Some(&NumberOrString::String("PKG-009".to_string()))),
        "expected PKG-009 diagnostic for child file: {file_diagnostics:?}"
    );
    assert!(
        file_diagnostics
            .iter()
            .all(|diag| diag.source.as_deref() == Some("rumoca")),
        "expected standard LSP diagnostics, not rendered miette output: {file_diagnostics:?}"
    );
}

#[test]
fn package_layout_source_root_load_warning_is_concise_when_file_diagnostics_exist() {
    let temp = new_temp_dir("package-layout-warning");
    let lib = temp.join("Modelica");
    std::fs::create_dir_all(&lib).expect("mkdir");
    std::fs::write(lib.join("package.mo"), "package Modelica end Modelica;")
        .expect("write package");
    std::fs::write(lib.join("A.mo"), "model A end A;").expect("write child");

    let err = parse_source_root_with_cache(&lib).expect_err("missing within must fail");
    let message = source_root_load_error_message(&lib.to_string_lossy(), &err);
    assert!(
        message.contains("see diagnostics"),
        "source-backed package-layout failures should defer details to diagnostics: {message}"
    );
    assert!(
        !message.contains("PKG-009"),
        "warning should not repeat the full diagnostic summary: {message}"
    );
}
