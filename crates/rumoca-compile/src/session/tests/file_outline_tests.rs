use super::*;
use crate::compile::{reset_session_cache_stats, session_cache_stats};

fn top_level_outline_names(session: &mut Session, uri: &str) -> Vec<String> {
    session
        .file_outline_query(uri)
        .expect("file outline should exist")
        .document_symbols()
        .iter()
        .map(|symbol| symbol.name.clone())
        .collect()
}

fn outline_child_detail<'a>(
    symbols: &'a [DocumentSymbol],
    class_name: &str,
    child_name: &str,
) -> &'a str {
    symbols
        .iter()
        .find(|symbol| symbol.name == class_name)
        .expect("class outline should exist")
        .children
        .iter()
        .find(|child| child.name == child_name)
        .and_then(|child| child.detail.as_deref())
        .expect("outline child detail should exist")
}

#[test]
fn file_outline_query_remains_available_for_recovered_body_errors() {
    let mut session = Session::default();
    let source_v1 = r#"
model M
  Real x;
equation
  x = 1;
end M;
"#;
    let source_v2 = r#"
model M
  Real x;
equation
  x = 1
end M;
"#;

    session
        .add_document("input.mo", source_v1)
        .expect("initial source should parse");
    assert_eq!(top_level_outline_names(&mut session, "input.mo"), vec!["M"]);

    let parse_error = session.update_document("input.mo", source_v2);
    assert!(
        parse_error.is_some(),
        "updated source should keep a recoverable parse error"
    );

    assert_eq!(
        top_level_outline_names(&mut session, "input.mo"),
        vec!["M"],
        "recovered body errors should still produce a syntax outline"
    );
}

#[test]
fn file_outline_query_rebuilds_after_outline_shape_change() {
    let mut session = Session::default();
    let source_v1 = r#"
model M
  Real x;
end M;
"#;
    let source_v2 = r#"
model M
  Real x;
  model Inner
  end Inner;
end M;
"#;

    session
        .add_document("input.mo", source_v1)
        .expect("initial source should parse");
    let initial = session
        .file_outline_query("input.mo")
        .expect("initial outline should exist")
        .document_symbols()
        .to_vec();
    let initial_file_id = *session
        .file_ids
        .get("input.mo")
        .expect("file id should be assigned");
    let initial_fingerprint = session
        .get_document("input.mo")
        .expect("initial document should exist")
        .outline_fingerprint();
    assert_eq!(initial[0].name, "M");
    assert!(
        initial[0]
            .children
            .iter()
            .all(|child| child.name != "Inner"),
        "initial outline should not include nested class"
    );

    let parse_error = session.update_document("input.mo", source_v2);
    assert!(parse_error.is_none(), "updated source should remain valid");

    let updated = session
        .file_outline_query("input.mo")
        .expect("updated outline should exist")
        .document_symbols()
        .to_vec();
    let cached = session
        .query_state
        .ast
        .file_outline_cache
        .get(&initial_file_id)
        .expect("outline cache should be stored");
    assert_ne!(
        cached.fingerprint, initial_fingerprint,
        "outline cache should track the new outline fingerprint"
    );
    assert!(
        updated[0]
            .children
            .iter()
            .any(|child| child.name == "Inner"),
        "outline should rebuild when nested class structure changes"
    );
}

#[test]
fn file_outline_query_tracks_body_section_count_changes() {
    let mut session = Session::default();
    let source_v1 = r#"
model M
  Real x;
equation
  x = 1;
algorithm
  x := 2;
end M;
"#;
    let source_v2 = r#"
model M
  Real x;
equation
  x = 1;
  x = 2;
algorithm
  x := 2;
algorithm
  x := 3;
end M;
"#;

    session
        .add_document("input.mo", source_v1)
        .expect("initial source should parse");
    let initial = session
        .file_outline_query("input.mo")
        .expect("initial outline should exist")
        .document_symbols()
        .to_vec();
    assert_eq!(
        outline_child_detail(&initial, "M", "Equations"),
        "1 equations"
    );
    assert_eq!(
        outline_child_detail(&initial, "M", "Algorithms"),
        "1 algorithm sections"
    );

    let parse_error = session.update_document("input.mo", source_v2);
    assert!(parse_error.is_none(), "updated source should remain valid");

    let updated = session
        .file_outline_query("input.mo")
        .expect("updated outline should exist")
        .document_symbols()
        .to_vec();
    assert_eq!(
        outline_child_detail(&updated, "M", "Equations"),
        "2 equations"
    );
    assert_eq!(
        outline_child_detail(&updated, "M", "Algorithms"),
        "2 algorithm sections"
    );
}

#[test]
fn document_symbol_query_stays_warm_for_same_span_body_edits() {
    let _guard = session_stats_test_guard();
    let mut session = Session::default();
    let source_v1 = r#"
model M
  Real x;
  Real y;
equation
  x = y;
end M;
"#;
    let source_v2 = r#"
model M
  Real x;
  Real y;
equation
  y = x;
end M;
"#;

    session
        .add_document("input.mo", source_v1)
        .expect("initial source should parse");
    let outline_before = session
        .get_document("input.mo")
        .expect("initial document should exist")
        .outline_fingerprint();

    reset_session_cache_stats();
    let before_first = session_cache_stats();
    let first = session
        .document_symbol_query("input.mo")
        .expect("initial document symbols should exist");
    let first_delta = session_cache_stats().delta_since(before_first);
    assert_eq!(first[0].name, "M");
    assert!(
        first_delta.document_symbol_query_misses >= 1,
        "first document symbol query should build outline state"
    );

    let parse_error = session.update_document("input.mo", source_v2);
    assert!(parse_error.is_none(), "body-only edit should remain valid");
    assert_eq!(
        session
            .get_document("input.mo")
            .expect("updated document should exist")
            .outline_fingerprint(),
        outline_before,
        "same-span body edits should keep the outline fingerprint stable"
    );

    let before_second = session_cache_stats();
    let second = session
        .document_symbol_query("input.mo")
        .expect("updated document symbols should exist");
    let second_delta = session_cache_stats().delta_since(before_second);
    assert_eq!(second[0].name, "M");
    assert!(
        second_delta.document_symbol_query_hits >= 1,
        "same-span body edits should keep the outline cache warm"
    );
    assert_eq!(
        second_delta.document_symbol_query_misses, 0,
        "same-span body edits should not rebuild the outline cache"
    );
}
