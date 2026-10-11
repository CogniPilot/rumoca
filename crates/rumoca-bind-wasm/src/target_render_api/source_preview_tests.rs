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
    for template in [
        "{{ artifact.generated_at }}",
        "{{ artifact.generation_tool }}",
        "{{ artifact.identities | length }}",
        "{{ artifact.checksums | length }}",
    ] {
        let error = custom_preview("Counter.alg", template, true)
            .expect_err("source previews have no packaging artifact evidence");
        assert!(
            error.message().contains("artifact"),
            "{template}: {error:?}"
        );
    }
}
