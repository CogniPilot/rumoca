use crate::compile::{
    Session, SessionConfig, SourceRootKind, reset_session_cache_stats, session_cache_stats,
};
use crate::parsing::parse_source_to_ast;
use std::collections::HashSet;

fn namespace_class_names(session: &mut Session) -> Vec<String> {
    let mut stack = vec![String::new()];
    let mut seen = HashSet::new();
    let mut names = Vec::new();

    while let Some(prefix) = stack.pop() {
        let entries = session
            .namespace_index_query(&prefix)
            .expect("query namespace completion cache");
        for (_, full_name, has_children) in entries {
            if !seen.insert(full_name.clone()) {
                continue;
            }
            names.push(full_name.clone());
            if has_children {
                stack.push(format!("{full_name}."));
            }
        }
    }

    names.sort_unstable();
    names
}

#[test]
fn session_cache_stats_track_session_events() {
    let before = session_cache_stats();
    let mut session = Session::new(SessionConfig::default());

    session
        .add_document("local.mo", "model Local\n  Real x;\nend Local;\n")
        .expect("local document should parse");

    let parse_error = session.update_document("local.mo", "model Local\n  Real x\nend Local;\n");
    assert!(
        parse_error.is_some(),
        "invalid update should report parse error"
    );
    let parse_error = session.update_document("local.mo", "model Local\n  Real x;\nend Local;\n");
    assert!(
        parse_error.is_none(),
        "valid update should clear parse error"
    );

    let lib = r#"
package Lib
  package Electrical
    model Resistor
      Real v;
    equation
      der(v) = 1;
    end Resistor;
  end Electrical;
end Lib;
"#;
    let parsed = parse_source_to_ast(lib, "Lib/package.mo").expect("parse source root");
    let inserted = session.replace_parsed_source_set(
        "external::Lib",
        SourceRootKind::External,
        vec![("Lib/package.mo".to_string(), parsed)],
        None,
    );
    assert_eq!(inserted, 1, "external source-root should be inserted");

    let first = namespace_class_names(&mut session);
    assert!(
        first.iter().any(|name| name == "Lib.Electrical.Resistor"),
        "expected nested external source-root class in completion cache"
    );
    let second = namespace_class_names(&mut session);
    assert_eq!(first, second, "cache hit should preserve class names");

    let _ = session
        .all_class_names()
        .expect("resolved build should succeed for local model and external source root");

    let stats = session_cache_stats().delta_since(before);
    assert!(stats.document_parse_calls >= 2);
    assert!(stats.document_parse_error_calls >= 1);
    // `parsed_file_parse_calls` is process-global and also records parallel file-cache parses
    // from unrelated tests. This session exercises only direct document parsing plus an
    // in-memory `replace_parsed_source_set`, so an exact-zero assertion is race-prone under the
    // default multi-threaded test runner.
    assert!(stats.namespace_completion_cache_misses >= 1);
    assert!(stats.namespace_completion_cache_hits >= 1);
    assert!(stats.standard_resolved_builds >= 1);
    assert!(stats.resolved_state_invalidations >= 1);
    assert!(stats.document_mutation_invalidations >= 1);
    assert!(stats.source_set_mutation_invalidations >= 1);
}

#[test]
fn session_cache_stats_track_query_caches() {
    let before = {
        reset_session_cache_stats();
        session_cache_stats()
    };
    let mut session = Session::new(SessionConfig::default());

    session
        .add_document(
            "workspace/model.mo",
            "model One\n  Real x(start=0);\nequation\n  der(x) = -x;\nend One;\n",
        )
        .expect("local model should parse");
    session
        .add_document(
            "external/package.mo",
            "package Lib\n  model Two\n  end Two;\nend Lib;\n",
        )
        .expect("external package should parse");

    assert!(session.parsed_file_query("workspace/model.mo").is_some());
    assert!(session.parsed_file_query("workspace/model.mo").is_some());
    assert!(session.recovered_file_query("workspace/model.mo").is_some());
    assert!(session.recovered_file_query("workspace/model.mo").is_some());

    assert!(
        !session
            .file_item_index_query("workspace/model.mo")
            .is_empty(),
        "file should contain at least one workspace symbol candidate"
    );
    assert!(
        !session
            .file_item_index_query("workspace/model.mo")
            .is_empty(),
        "second file-item query should come from cache"
    );
    assert!(
        session
            .document_symbol_query("workspace/model.mo")
            .is_some(),
        "document symbol query should return cached outline entries"
    );
    assert!(
        session
            .document_symbol_query("workspace/model.mo")
            .is_some(),
        "second document symbol query should come from cache"
    );

    let namespace_model = parse_source_to_ast(
        "package Lib\n  package Electrical\n    model R\n    end R;\n  end Electrical;\nend Lib;\n",
        "lib/package.mo",
    )
    .expect("parse external source-root model");
    session.replace_parsed_source_set(
        "external::Lib",
        SourceRootKind::External,
        vec![("lib/package.mo".to_string(), namespace_model)],
        None,
    );

    assert!(
        !session.workspace_symbol_query("One").is_empty(),
        "workspace query should include parsed workspace class"
    );
    assert!(
        !session.workspace_symbol_query("One").is_empty(),
        "second call should hit workspace query cache"
    );

    assert!(
        !session.namespace_index_query("Lib.").unwrap().is_empty(),
        "namespace query should include Lib namespace"
    );
    assert!(
        !session.namespace_index_query("Lib.").unwrap().is_empty(),
        "second namespace query should use merged cache"
    );

    let _ = session.compile_model_diagnostics("One");
    let _ = session.compile_model_diagnostics("One");

    let stats = session_cache_stats().delta_since(before);
    assert!(stats.parsed_file_query_misses >= 1);
    assert!(stats.parsed_file_query_hits >= 1);
    assert!(stats.recovered_file_query_misses >= 1);
    assert!(stats.recovered_file_query_hits >= 1);
    assert!(stats.file_item_index_query_misses >= 1);
    assert!(stats.file_item_index_query_hits >= 1);
    assert!(stats.workspace_symbol_query_misses >= 1);
    assert!(stats.workspace_symbol_query_hits >= 1);
    assert!(stats.namespace_index_query_misses >= 1);
    assert!(stats.namespace_index_query_hits >= 1);
    assert!(stats.document_symbol_query_misses >= 1);
    assert!(stats.document_symbol_query_hits >= 1);
    assert!(stats.interface_semantic_diagnostics_cache_misses >= 1);
    assert!(stats.interface_semantic_diagnostics_builds >= 1);
    assert!(stats.interface_semantic_diagnostics_cache_hits >= 1);
    assert!(stats.body_semantic_diagnostics_cache_misses >= 1);
    assert!(stats.body_semantic_diagnostics_builds >= 1);
    assert!(stats.body_semantic_diagnostics_cache_hits >= 1);
    assert!(stats.model_stage_semantic_diagnostics_cache_misses >= 1);
    assert!(stats.model_stage_semantic_diagnostics_builds >= 1);
    assert!(stats.model_stage_semantic_diagnostics_cache_hits >= 1);
}
