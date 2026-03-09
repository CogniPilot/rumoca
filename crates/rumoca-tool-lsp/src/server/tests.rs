use super::*;

#[test]
fn import_prefix_detection_handles_partial_qualified_name() {
    let source = "model M\n  import Mo\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  import Mo".len() as u32,
    };
    assert_eq!(
        extract_import_completion_prefix(source, pos),
        Some("Mo".to_string())
    );

    let source = "model M\n  import Modelica.Blocks.Co\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  import Modelica.Blocks.Co".len() as u32,
    };
    assert_eq!(
        extract_import_completion_prefix(source, pos),
        Some("Modelica.Blocks.Co".to_string())
    );
}

#[test]
fn import_prefix_detection_ignores_non_import_lines() {
    let source = "model M\n  Real x;\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  Real x".len() as u32,
    };
    assert_eq!(extract_import_completion_prefix(source, pos), None);
}

#[test]
fn completion_context_requires_resolved_session_for_import_root_prefix() {
    let source = "model M\n  import Mo\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  import Mo".len() as u32,
    };
    let import_prefix = extract_import_completion_prefix(source, pos);
    assert!(completion_context_needs_resolved_session(
        source,
        pos,
        import_prefix.as_deref()
    ));
}

#[test]
fn completion_context_does_not_require_resolved_session_for_plain_identifier() {
    let source = "model M\n  Re\nend M;\n";
    let pos = Position {
        line: 1,
        character: "  Re".len() as u32,
    };
    let import_prefix = extract_import_completion_prefix(source, pos);
    assert!(!completion_context_needs_resolved_session(
        source,
        pos,
        import_prefix.as_deref()
    ));
}

#[test]
fn reserve_library_load_rejects_loaded_loading_and_stale_epoch() {
    let mut loaded = HashSet::new();
    loaded.insert("library::modelica".to_string());
    let mut loading = HashSet::new();
    loading.insert("library::vendor".to_string());

    assert!(!should_reserve_library_load(
        &loaded,
        &loading,
        "library::modelica",
        5,
        5
    ));
    assert!(!should_reserve_library_load(
        &loaded,
        &loading,
        "library::vendor",
        5,
        5
    ));
    assert!(!should_reserve_library_load(
        &loaded,
        &loading,
        "library::new",
        4,
        5
    ));
    assert!(should_reserve_library_load(
        &loaded,
        &loading,
        "library::new",
        5,
        5
    ));
}

#[test]
fn apply_library_load_requires_fresh_epoch_and_unloaded_path() {
    let mut loaded = HashSet::new();
    loaded.insert("library::modelica".to_string());

    assert!(!should_apply_library_load(
        &loaded,
        "library::modelica",
        8,
        8
    ));
    assert!(!should_apply_library_load(&loaded, "library::new", 7, 8));
    assert!(should_apply_library_load(&loaded, "library::new", 8, 8));
}
