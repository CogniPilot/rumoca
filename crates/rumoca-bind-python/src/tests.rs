//! Rust-side unit tests for the pure compile/diagnostics helpers. The full
//! typed-surface behaviour (Model, Result, views) is covered by the Python
//! contract tests in `tests/api_test.py`.

use super::*;
use rumoca_compile::SessionConfig;
use rumoca_compile::codegen::targets::RenderedTargetFile;

const FIXED_WING_OUTER_LOOP_SOURCE: &str = r#"
record CubEstimate
  Real flightPathAngle;
  Real speedChange;
end CubEstimate;

record CubGuidance
  CubEstimate estimate;
end CubGuidance;

model FixedWingOuterLoop
  constant Real samplePeriod = 0.02;
  parameter Real courseDeadband = 0.01;
  parameter Real courseErrorGain = 2.0;
  discrete CubEstimate estimator;
  discrete CubGuidance guidance;
  discrete Real course(start = 0.0);
  discrete Real courseError(start = 0.0);
  discrete Real desiredCourseRate(start = 0.0);
algorithm
  when sample(0.0, samplePeriod) then
    estimator.flightPathAngle := 0.5;
    estimator.speedChange := 1.0;
    guidance.estimate := estimator;
    courseError := 0.5 - pre(course);
    if abs(courseError) < courseDeadband then
      courseError := 0.0;
    end if;
    desiredCourseRate := courseErrorGain * courseError;
  end when;
end FixedWingOuterLoop;
"#;

#[test]
fn version_is_non_empty() {
    assert!(!version().is_empty());
}

#[test]
fn diagnostics_report_syntax_error() {
    // Missing `;` after `Real x` is a syntax error.
    let diags = diagnostics_for_source("model M Real x end M;", "input.mo");
    assert!(
        diags.iter().any(|d| d.level == "error"),
        "expected an error diagnostic, got {diags:?}"
    );
}

#[test]
fn diagnostics_clean_source_has_no_syntax_error() {
    let diags = diagnostics_for_source("model M Real x; end M;", "input.mo");
    assert!(
        diags
            .iter()
            .all(|d| d.rule.as_deref() != Some("syntax-error")),
        "valid source should not report a syntax error, got {diags:?}"
    );
}

#[test]
fn builtin_targets_include_sympy() {
    let ids: Vec<String> = targets::list_targets().into_iter().map(|t| t.id).collect();
    assert!(
        ids.iter().any(|id| id == "sympy"),
        "expected a sympy target, got {ids:?}"
    );
}

#[test]
fn solver_listing_has_known_families() {
    let solvers = targets::list_solvers();
    assert!(
        solvers
            .iter()
            .any(|s| s.id == "rk-like" && s.family == "explicit")
    );
    assert!(
        solvers
            .iter()
            .any(|s| s.id == "bdf" && s.family == "implicit")
    );
}

fn compile_fixed_wing_outer_loop() -> HighLevelCompilationResult {
    let mut session = Session::new(SessionConfig::default());
    let (result, model_name) = compile_source_in_session(
        &mut session,
        FIXED_WING_OUTER_LOOP_SOURCE,
        Some("FixedWingOuterLoop"),
        "FixedWingOuterLoop.mo",
        &[],
    )
    .expect("FixedWingOuterLoop fixture should compile through the Python binding session path");
    assert_eq!(model_name, "FixedWingOuterLoop");
    result
}

fn rendered_pairs(files: Vec<RenderedTargetFile>) -> Vec<(String, String)> {
    files
        .into_iter()
        .map(|file| (file.path, normalize_dynamic_manifest_content(&file.content)))
        .collect()
}

fn normalize_dynamic_manifest_content(content: &str) -> String {
    let without_uuids = replace_braced_uuids(content);
    let without_time =
        replace_xml_attr_value(&without_uuids, "generationDateAndTime", "{GENERATION_TIME}");
    replace_xml_attr_value(&without_time, "checksum", "{SHA1}")
}

fn replace_xml_attr_value(content: &str, attr: &str, replacement: &str) -> String {
    let needle = format!("{attr}=\"");
    let mut out = String::with_capacity(content.len());
    let mut rest = content;
    while let Some(start) = rest.find(&needle) {
        let value_start = start + needle.len();
        out.push_str(&rest[..value_start]);
        out.push_str(replacement);
        let Some(value_end) = rest[value_start..].find('"') else {
            out.push_str(&rest[value_start..]);
            return out;
        };
        rest = &rest[value_start + value_end..];
    }
    out.push_str(rest);
    out
}

fn replace_braced_uuids(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    let mut rest = content;
    while let Some(start) = rest.find('{') {
        out.push_str(&rest[..start]);
        let Some(end) = rest[start + 1..].find('}') else {
            out.push_str(&rest[start..]);
            return out;
        };
        let candidate = &rest[start + 1..start + 1 + end];
        if is_uuid_body(candidate) {
            out.push_str("{UUID}");
        } else {
            out.push('{');
            out.push_str(candidate);
            out.push('}');
        }
        rest = &rest[start + end + 2..];
    }
    out.push_str(rest);
    out
}

fn is_uuid_body(value: &str) -> bool {
    value.len() == 36
        && value.chars().all(|ch| ch == '-' || ch.is_ascii_hexdigit())
        && value.chars().filter(|&ch| ch == '-').count() == 4
}

fn assert_binding_codegen_matches_cli_dispatch(target: &str) {
    let result = compile_fixed_wing_outer_loop();
    let binding_files = render_target_files(&result, "FixedWingOuterLoop", target)
        .unwrap_or_else(|err| panic!("binding render for {target} failed: {}", err.0));
    let cli_dispatch_files =
        ::rumoca::render_target_files(&result, "FixedWingOuterLoop", target, None)
            .unwrap_or_else(|err| panic!("CLI dispatch render for {target} failed: {err:#}"));
    assert_eq!(
        rendered_pairs(binding_files),
        rendered_pairs(cli_dispatch_files),
        "Python binding codegen must use the same target dispatcher as rumoca compile"
    );
}

#[test]
fn fixed_wing_outer_loop_embedded_c_galec_matches_cli_dispatch() {
    assert_binding_codegen_matches_cli_dispatch("embedded-c-galec");
}

#[test]
fn fixed_wing_outer_loop_galec_production_matches_cli_dispatch() {
    assert_binding_codegen_matches_cli_dispatch("galec-production");
}
