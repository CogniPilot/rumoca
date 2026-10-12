//! Checked-source browser previews retain target and artifact obligations.

use super::*;

const SOURCE: &str = r#"
model BrowserPreviewCounter
  constant Real samplePeriod = 0.1;
  parameter Real gain = 2.0;
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    y = gain * (pre(y) + 1.0);
  end when;
end BrowserPreviewCounter;
"#;

fn custom_preview(
    path: &str,
    template: &str,
    events: bool,
) -> Result<Vec<RenderedTargetFile>, WasmError> {
    let manifest = format!(
        r#"version = 1
ir = "algorithm-code"
name = "source-preview"
[[files]]
path = "{path}"
template = "source.jinja"
[capabilities]
continuous_states = false
events = {events}
clocks = true
"#
    );
    let templates = serde_json::to_string(&BTreeMap::from([("source.jinja", template)]))
        .expect("serialize template map");
    render_target_in_session(
        &mut rumoca_compile::Session::default(),
        SOURCE,
        "BrowserPreviewCounter",
        "source-preview",
        &manifest,
        &templates,
    )
}

#[test]
fn builtin_algorithm_preview_emits_the_actual_checked_model() {
    let files = render_target_in_session(
        &mut rumoca_compile::Session::default(),
        SOURCE,
        "BrowserPreviewCounter",
        "galec",
        "",
        "{}",
    )
    .expect("fixed-sample counter has an Algorithm Code source preview");
    assert_eq!(files.len(), 1, "packaging metadata is not a source preview");
    assert_eq!(files[0].path, "AlgorithmCode/BrowserPreviewCounter.alg");
    assert!(files[0].content.contains("BrowserPreviewCounter"));
    assert!(files[0].content.contains("DoStep"));
    assert!(files[0].content.contains("2.0"));
}

#[test]
fn browser_production_source_retains_checked_source_locations() {
    let mut session = rumoca_compile::Session::default();
    let files = render_target_in_session(
        &mut session,
        SOURCE,
        "BrowserPreviewCounter",
        "embedded-c-galec",
        "",
        "{}",
    )
    .expect("the checked counter has a Production C source preview");
    let implementation = files
        .iter()
        .find(|file| file.path == "BrowserPreviewCounter.c")
        .expect("Production C source is present");
    assert!(
        implementation
            .content
            .contains("Modelica trace: input.mo:8:5"),
        "the y update must retain its actual source line/column: {}",
        implementation.content
    );
}

#[test]
fn browser_workspace_preview_retains_declaration_source_identity() {
    let mut session = rumoca_compile::Session::default();
    let counter = SOURCE.replace("BrowserPreviewCounter", "Counter");
    let workspace = format!("within ;\npackage Workshop\n{counter}\nend Workshop;");
    session.update_document("Workshop/package.mo", &workspace);
    let source = "model BrowserPreviewCounter extends Workshop.Counter; end BrowserPreviewCounter;";
    let files = render_target_in_session(
        &mut session,
        source,
        "BrowserPreviewCounter",
        "embedded-c-galec",
        "",
        "{}",
    )
    .expect("workspace declarations are compiled in the same checked session");
    let implementation = files
        .iter()
        .find(|file| file.path == "BrowserPreviewCounter.c")
        .expect("Production C source is present");
    // The two package header lines move the inherited y update to line 10.
    assert!(
        implementation
            .content
            .contains("Modelica trace: package.mo:10:5"),
        "the inherited equation must retain its declaration source: {}",
        implementation.content
    );
    assert!(
        !implementation
            .content
            .contains("Modelica trace: input.mo:10:5"),
        "the extending model must not acquire its inherited equation's location"
    );
}

#[test]
fn algorithm_preview_honors_declared_event_capability() {
    let error = custom_preview("Counter.alg", "{{ model_name }}", false)
        .expect_err("a preview cannot bypass the custom target's event refusal");
    assert!(
        error.message().contains("unsupported-feature:events"),
        "{error:?}"
    );
}

#[test]
fn algorithm_preview_refuses_an_empty_source_output_set() {
    let error = custom_preview("manifest.xml", "{{ model_name }}", true)
        .expect_err("metadata-only files cannot become a successful empty preview");
    assert!(error.message().contains("source files"), "{error:?}");
}

#[test]
fn algorithm_preview_does_not_invent_artifact_evidence() {
    let files = custom_preview("Counter.alg", "{{ model_name }}", true)
        .expect("the same custom target renders its available source context");
    assert_eq!(files.len(), 1);
    assert_eq!(files[0].content, "BrowserPreviewCounter");
    for template in [
        "{{ artifact.generated_at }}",
        "{{ artifact.generation_tool }}",
        "{{ artifact.identities | length }}",
        "{{ artifact.checksums | length }}",
    ] {
        let error = custom_preview("Counter.alg", template, true)
            .expect_err("source previews have no packaging artifact evidence");
        assert!(
            error
                .message()
                .contains("template render error: undefined value"),
            "{template}: {error:?}"
        );
    }
}

#[test]
fn algorithm_preview_classifies_the_rendered_output_path() {
    let expected = custom_preview("BrowserPreviewCounter.c", "{{ model_name }}", true)
        .expect("literal C filename remains a source preview");
    let rendered = custom_preview("{{ model_name ~ '.c' }}", "{{ model_name }}", true)
        .expect("the checked renderer produces the same actual C output path");
    assert_eq!(rendered.len(), expected.len());
    assert_eq!(rendered[0].path, expected[0].path);
    assert_eq!(rendered[0].content, expected[0].content);

    let error = custom_preview("{{ model_name ~ '.xml' }}", "{{ model_name }}", true)
        .expect_err("a rendered metadata filename cannot become a source preview");
    assert!(error.message().contains("source files"), "{error:?}");
}

#[test]
fn algorithm_preview_requires_a_safe_rendered_relative_path() {
    for path in ["{{ '../Counter.c' }}", "{{ '/Counter.c' }}", "{{ '' }}"] {
        let error = custom_preview(path, "{{ model_name }}", true)
            .expect_err("the actual rendered filename must remain within the target root");
        assert!(
            error.message().contains("Target manifest path"),
            "{path}: {error:?}"
        );
    }
}
