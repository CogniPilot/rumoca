use std::fs;
use std::path::{Path, PathBuf};

fn corpus_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/broken_modelica")
}

fn expected_root_class_name(path: &Path) -> &'static str {
    match path.file_stem().and_then(|stem| stem.to_str()) {
        Some("incomplete_class") => "IncompleteClass",
        Some("missing_equation_rhs") => "MissingEquationRhs",
        Some("missing_package_end") => "MissingPackageEnd",
        Some("missing_semicolon") => "MissingSemicolon",
        Some("unbalanced_braces") => "UnbalancedBraces",
        Some(other) => panic!("missing expected recovered class mapping for {other}"),
        None => panic!("corpus path has no file stem: {}", path.display()),
    }
}

#[test]
fn broken_modelica_corpus_recovers_without_panicking() {
    let mut entries = fs::read_dir(corpus_dir())
        .expect("read broken Modelica corpus")
        .collect::<Result<Vec<_>, _>>()
        .expect("read corpus entries");
    entries.sort_by_key(|entry| entry.path());

    assert!(
        !entries.is_empty(),
        "broken Modelica recovery corpus must not be empty"
    );

    for entry in entries {
        let path = entry.path();
        if path.extension().and_then(|ext| ext.to_str()) != Some("mo") {
            continue;
        }
        let source = fs::read_to_string(&path).expect("read broken corpus case");
        let file_name = path.display().to_string();
        let syntax = rumoca_phase_parse::parse_to_syntax(&source, &file_name);

        assert!(
            syntax.recovered().is_some() || syntax.parsed().is_some(),
            "{file_name} should produce a best-effort syntax tree"
        );
        assert!(
            !syntax.parse_errors().is_empty() || syntax.parse_error().is_some(),
            "{file_name} should exercise recovery after a parse error"
        );
        assert!(
            !syntax.best_effort().classes.is_empty(),
            "{file_name} should preserve at least one class shell"
        );
        let expected_class = expected_root_class_name(&path);
        assert!(
            syntax.best_effort().classes.contains_key(expected_class),
            "{file_name} should preserve recovered root class {expected_class}"
        );
    }
}
