use super::*;
use rumoca_session::compile::SourceRootKind;
use rumoca_session::parsing::parse_source_to_ast;

fn parse_ast(source: &str) -> ast::StoredDefinition {
    parse_source_to_ast(source, "input.mo").expect("parse should succeed")
}

#[test]
fn modifier_completion_suggests_imported_type_members() {
    let lib = r#"
package Modelica
  package Blocks
    package Continuous
      model PID
        parameter Real kp = 1.0;
        Real y;
        Real u;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;
    let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  Real x(start=0);
  PID pid();
end Ball;
"#;
    let mut session = Session::default();
    session.add_document("Lib.mo", lib).expect("lib parses");
    session
        .add_document("input.mo", source)
        .expect("source parses");

    let line = 4;
    let character = "  PID pid(".len() as u32;
    let items = handle_completion(
        source,
        None,
        Some(&mut session),
        Some("input.mo"),
        line,
        character,
    );
    assert!(
        items.iter().any(|i| i.label == "kp"),
        "expected PID member `kp` in completions: {:?}",
        items.iter().map(|i| i.label.clone()).collect::<Vec<_>>()
    );
    assert!(
        !session.has_resolved_cached(),
        "query-backed modifier completion should not need a resolved session"
    );
}

#[test]
fn dot_completion_scopes_to_component_type_members() {
    let lib = r#"
package Modelica
  package Blocks
    package Continuous
      model PID
        parameter Real kp = 1.0;
        Real y;
        Real u;
      end PID;
    end Continuous;
  end Blocks;
end Modelica;
"#;
    let source = r#"
model Ball
  import Modelica.Blocks.Continuous.PID;
  Real x(start=0);
  PID pid();
equation
  pid.u = 1;
end Ball;
"#;
    let mut session = Session::default();
    session.add_document("Lib.mo", lib).expect("lib parses");
    session
        .add_document("input.mo", source)
        .expect("source parses");

    let ast = parse_ast(source);
    let line = 6;
    let character = "  pid.".len() as u32;
    let items = handle_completion(
        source,
        Some(&ast),
        Some(&mut session),
        Some("input.mo"),
        line,
        character,
    );
    let labels = items.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
    assert!(
        labels.iter().any(|label| label == "kp"),
        "expected PID member completions, got: {:?}",
        labels
    );
    assert!(
        !labels.iter().any(|label| label == "x"),
        "dot-completion on `pid.` should not include Ball-scoped names: {:?}",
        labels
    );
    assert!(
        !session.has_resolved_cached(),
        "query-backed dot completion should not need a resolved session"
    );
}

#[test]
fn dot_completion_uses_session_for_local_model_component_members() {
    let source = r#"
model Plane
  Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;

model Sim
  Plane p1, p2;
equation
  p1.x = 1;
end Sim;
"#;
    let mut session = Session::default();
    session
        .add_document("input.mo", source)
        .expect("source parses");

    let line = 12;
    let character = "  p1.".len() as u32;
    let items = handle_completion(
        source,
        None,
        Some(&mut session),
        Some("input.mo"),
        line,
        character,
    );
    let labels = items.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
    assert!(
        labels.iter().any(|label| label == "x"),
        "expected Plane member completions for `p1.`, got: {:?}",
        labels
    );
    assert!(
        labels.iter().any(|label| label == "theta"),
        "expected Plane member `theta` completion for `p1.`, got: {:?}",
        labels
    );
    assert!(
        !session.has_resolved_cached(),
        "local model member completion should stay on the query path"
    );
}

#[test]
fn dot_completion_falls_back_to_ast_for_incomplete_member_edit() {
    let valid_source = r#"
operator record SE2
  Real x;
  Real y;
  Real theta;
end SE2;

model Test2
  SE2 pose;
equation
  der(pose.x) = 1;
  der(pose.y) = 0;
  pose.x = 0;
end Test2;
"#;
    let invalid_source = r#"
operator record SE2
  Real x;
  Real y;
  Real theta;
end SE2;

model Test2
  SE2 pose;
equation
  der(pose.x) = 1;
  der(pose.y) = 0;
  pose.
end Test2;
"#;
    let stale_ast = parse_ast(valid_source);
    let mut session = Session::default();
    session
        .add_document("input.mo", valid_source)
        .expect("valid source should parse");
    let parse_error = session.update_document("input.mo", invalid_source);
    assert!(
        parse_error.is_some(),
        "incomplete member edit should leave a recoverable parse error"
    );

    let line = 12;
    let character = "  pose.".len() as u32;
    let items = handle_completion(
        invalid_source,
        Some(&stale_ast),
        Some(&mut session),
        Some("input.mo"),
        line,
        character,
    );
    let labels = items.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
    assert!(
        labels.iter().any(|label| label == "x"),
        "expected SE2 member `x` completion during incomplete edit, got: {:?}",
        labels
    );
    assert!(
        labels.iter().any(|label| label == "theta"),
        "expected SE2 member `theta` completion during incomplete edit, got: {:?}",
        labels
    );
}

#[test]
fn dot_completion_resolves_recursive_member_path_from_ast_and_owner_scope() {
    let valid_source = r#"
operator record SE2
  Real x;
  Real y;
  Real theta;
end SE2;

model Test2
  import Pose = SE2;
  Pose pose;
end Test2;

model Sim
  Test2 test;
equation
  test.pose.x = 1;
end Sim;
"#;
    let invalid_source = r#"
operator record SE2
  Real x;
  Real y;
  Real theta;
end SE2;

model Test2
  import Pose = SE2;
  Pose pose;
end Test2;

model Sim
  Test2 test;
equation
  test.pose.
end Sim;
"#;
    let stale_ast = parse_ast(valid_source);
    let mut session = Session::default();
    session
        .add_document("input.mo", valid_source)
        .expect("valid source should parse");
    let parse_error = session.update_document("input.mo", invalid_source);
    assert!(
        parse_error.is_some(),
        "nested member edit should keep a recoverable parse error"
    );

    let line = 15;
    let character = "  test.pose.".len() as u32;
    let items = handle_completion(
        invalid_source,
        Some(&stale_ast),
        Some(&mut session),
        Some("input.mo"),
        line,
        character,
    );
    let labels = items
        .iter()
        .map(|item| item.label.clone())
        .collect::<Vec<_>>();
    assert!(
        labels.iter().any(|label| label == "x"),
        "expected recursive member completion `x`, got: {:?}",
        labels
    );
    assert!(
        labels.iter().any(|label| label == "theta"),
        "expected recursive member completion `theta`, got: {:?}",
        labels
    );
    assert!(
        !session.has_resolved_cached(),
        "recursive member completion should stay on the query path"
    );
}

#[test]
fn local_completion_uses_session_scope_query_when_available() {
    let stale_source = r#"
model Sim
end Sim;
"#;
    let source = r#"
model Sim
  parameter Real kp;
  model Inner
  end Inner;
  
end Sim;
"#;
    let mut session = Session::default();
    session
        .add_document("input.mo", source)
        .expect("source parses");

    let stale_ast = parse_ast(stale_source);
    let character = 2;
    let items = handle_completion(
        source,
        Some(&stale_ast),
        Some(&mut session),
        Some("input.mo"),
        5,
        character,
    );
    let labels = items
        .iter()
        .map(|item| item.label.clone())
        .collect::<Vec<_>>();
    assert!(
        labels.iter().any(|label| label == "kp"),
        "session-backed local completion should surface current local members, got: {:?}",
        labels
    );
    assert!(
        !session.has_resolved_cached(),
        "session-backed local completion should stay off resolved caches"
    );
}

#[test]
fn dot_completion_uses_ast_for_local_imported_alias_members() {
    let source = r#"
package Lib
  model Helper
    parameter Real gain = 1;
    output Real y;
  equation
    y = gain;
  end Helper;
end Lib;

model M
  import Alias = Lib.Helper;
  Alias helperInst;
equation
  helperInst.y = sin(helperInst.gain);
end M;
"#;
    let ast = parse_ast(source);
    let line = 14;
    let character = "  helperInst.".len() as u32;
    let items = handle_completion(source, Some(&ast), None, None, line, character);
    let labels = items.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
    assert!(
        labels.iter().any(|label| label == "gain"),
        "expected local alias member `gain` completion, got: {:?}",
        labels
    );
    assert!(
        labels.iter().any(|label| label == "y"),
        "expected local alias member `y` completion, got: {:?}",
        labels
    );
}

#[test]
fn builtin_modifier_completion_remains_available_for_builtin_types() {
    let source = r#"
model M
  Real x(start = 0);
end M;
"#;
    let ast = parse_ast(source);
    let line = 2;
    let character = "  Real x(".len() as u32;
    let items = handle_completion(source, Some(&ast), None, None, line, character);
    assert!(
        items.iter().any(|i| i.label == "start"),
        "expected builtin modifier `start` completion"
    );
}

#[test]
fn keyword_completion_includes_operator() {
    let items = keyword_completions("op");
    assert!(
        items.iter().any(|i| i.label == "operator"),
        "expected `operator` keyword completion"
    );
}

#[test]
fn external_source_root_completion_uses_namespace_cache_after_local_edit() {
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
    let mut session = Session::default();
    let parsed = parse_source_to_ast(lib, "Lib/package.mo").expect("parse source root");
    let inserted = session.replace_parsed_source_set(
        "external::Lib",
        SourceRootKind::External,
        vec![("Lib/package.mo".to_string(), parsed)],
        None,
    );
    assert_eq!(inserted, 1, "expected external source root to load");

    let source = "model Active\n  Real x;\nend Active;\n";
    session
        .add_document("input.mo", source)
        .expect("source parses");

    let namespace_root_children = session
        .namespace_index_query("")
        .expect("build namespace completion cache");
    assert!(
        namespace_root_children
            .iter()
            .any(|(_, full_name, _)| full_name == "Lib"),
        "expected root source-root namespace cache entry: {namespace_root_children:?}"
    );
    let lib_namespace_children = session
        .namespace_index_query("Lib.")
        .expect("load namespace children under Lib");
    assert!(
        lib_namespace_children
            .iter()
            .any(|(_, full_name, _)| full_name == "Lib.Electrical"),
        "expected Lib children namespace cache entry: {lib_namespace_children:?}"
    );
    let electrical_namespace_children = session
        .namespace_index_query("Lib.Electrical.")
        .expect("load namespace children under Lib.Electrical");
    assert!(
        electrical_namespace_children
            .iter()
            .any(|(_, full_name, _)| full_name == "Lib.Electrical.Resistor"),
        "expected nested external source-root class in namespace cache: {electrical_namespace_children:?}"
    );
    assert!(
        !session.has_resolved_cached(),
        "priming the source-root cache should not build the full resolved session"
    );
    assert_eq!(
        session
            .namespace_index_query("Lib.")
            .expect("expected namespace children under Lib"),
        vec![("Electrical".to_string(), "Lib.Electrical".to_string(), true)],
        "namespace cache should expose immediate children"
    );

    let edited_source = "model Active\n  Real x;\n  Real y;\nend Active;\n";
    session.update_document("input.mo", edited_source);
    assert!(
        !session.has_resolved_cached(),
        "editing a local document should invalidate the full resolved session"
    );
    assert!(
        !session
            .namespace_index_query("Lib.")
            .expect("expected source-root namespace after local edit")
            .is_empty(),
        "editing a local document should preserve the source-root-only completion cache"
    );

    let completion_source = "model Active\n  Lib.\nend Active;\n";
    let items = handle_completion(
        completion_source,
        None,
        Some(&mut session),
        Some("input.mo"),
        1,
        6,
    );
    let labels = items.iter().map(|i| i.label.clone()).collect::<Vec<_>>();
    assert!(
        labels.iter().any(|label| label == "Electrical"),
        "expected completion from cached source-root class names, got: {labels:?}"
    );
    assert!(
        !session.has_resolved_cached(),
        "source-root completion should not rebuild the full resolved session"
    );
}
