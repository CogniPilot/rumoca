use super::*;

fn summary_for(session: &mut Session, uri: &str) -> FileSummary {
    session
        .file_summary_query(uri)
        .expect("file summary should exist")
        .clone()
}

#[test]
fn file_summary_is_stable_across_body_only_edits() {
    let mut session = Session::default();
    let source_v1 = r#"
within Demo;
model Outer
  import Lib.Helpers.*;
  extends Base;
  parameter Real gain = 1;
  model Inner
    Real x;
  equation
    x = gain;
  end Inner;
equation
  gain = 2;
end Outer;
"#;
    let source_v2 = r#"
within Demo;
model Outer
  import Lib.Helpers.*;
  extends Base;
  parameter Real gain = 3;
  model Inner
    Real x;
  algorithm
    x := gain + 1;
  end Inner;
algorithm
  gain := 4;
end Outer;
"#;

    session
        .add_document("input.mo", source_v1)
        .expect("initial source should parse");
    let summary_before = summary_for(&mut session, "input.mo");

    let parse_error = session.update_document("input.mo", source_v2);
    assert!(parse_error.is_none(), "body edit should remain valid");
    let summary_after = summary_for(&mut session, "input.mo");

    assert_eq!(
        summary_before, summary_after,
        "equation/algorithm body edits should not change the file summary"
    );
    assert_eq!(summary_after.within_path(), Some("Demo"));
    assert_eq!(
        summary_after.item_key_for_name("Demo.Outer"),
        summary_before.item_key_for_name("Demo.Outer"),
        "body-only edits should keep stable class keys"
    );
}

#[test]
fn file_summary_changes_when_structural_headers_change() {
    let mut session = Session::default();
    let source_v1 = r#"
within Demo;
model Outer
  import Lib.Helpers.*;
  Real x;
end Outer;
"#;
    let source_v2 = r#"
within Demo;
model Outer
  import Lib.Components.*;
  Real x;
  Real y;
end Outer;
"#;

    session
        .add_document("input.mo", source_v1)
        .expect("initial source should parse");
    let summary_before = summary_for(&mut session, "input.mo");

    let parse_error = session.update_document("input.mo", source_v2);
    assert!(parse_error.is_none(), "structural edit should remain valid");
    let summary_after = summary_for(&mut session, "input.mo");

    assert_ne!(
        summary_before, summary_after,
        "imports and component signature edits should invalidate the file summary"
    );

    let outer = summary_after
        .class("Demo.Outer")
        .expect("outer class should still exist");
    assert!(
        outer.components.contains_key("y"),
        "new component declarations should appear in the updated file summary"
    );
}

#[test]
fn file_summary_remains_available_for_recovered_body_errors() {
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
        .add_document("broken.mo", source_v1)
        .expect("initial source should parse");
    let summary_before = summary_for(&mut session, "broken.mo");

    let parse_error = session.update_document("broken.mo", source_v2);
    assert!(
        parse_error.is_some(),
        "updated source should keep a recoverable parse error"
    );

    let summary = summary_for(&mut session, "broken.mo");
    assert_eq!(
        summary.item_key_for_name("M"),
        summary_before.item_key_for_name("M"),
        "recoverable body errors should keep stable class ownership keys"
    );
    assert!(
        summary.class("M").is_some(),
        "recoverable body errors should still produce a top-level file summary"
    );
}
