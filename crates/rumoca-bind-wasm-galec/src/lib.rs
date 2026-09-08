//! Lazy GALEC / eFMI codegen addon for the rumoca WASM package.
//!
//! This is a SEPARATE `cdylib` sibling of `rumoca-bind-wasm`: the core
//! rumoca WASM binary (Modelica / template / simulation workflows) must NOT
//! grow the GALEC → eFMI Algorithm Code projection, so this
//! module carries it on its own and is loaded on demand only when a user
//! selects a GALEC codegen target. It mirrors the repo's lazy-diffsol-addon
//! (`rumoca-bind-wasm-diffsol`) and the layered core/rumoca/viz/live
//! packaging direction.
//!
//! It is a thin wasm boundary: [`render_galec`] compiles Modelica in-memory and
//! delegates the checked target to `StrictCompilation::render_target`. This
//! crate neither lowers IR nor renders templates.

use std::collections::BTreeMap;

use lsp_types::{Position, Url};
use rumoca_compile::codegen::targets::{
    ArtifactGenerationInstant, ArtifactIdentitySeed, ArtifactSessionInput, TargetBundle,
};
use rumoca_compile::{Session, SessionConfig};
use rumoca_core::{PhaseError, SourceMap};
use rumoca_phase_codegen::CodegenError;
use rumoca_phase_galec::GalecTargetErrors;
use rumoca_tool_lsp_galec::{compute_diagnostics, navigation};
use serde_json::{Value, json};
use wasm_bindgen::prelude::*;

const GALEC_TARGET: &str = "galec";

/// Initialize the panic hook for readable console errors (mirrors the core
/// binding and the diffsol addon).
#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Compile the workspace Modelica sources, project the model named
/// `model_name` to GALEC, and return the rendered artifacts as a JSON string.
///
/// `workspace_sources` is a JSON object mapping each document path to its
/// Modelica text (`{ "<path>": "<content>", … }`) — the SAME map the core
/// binding compiles with, so a model spanning several files (imports, a
/// library, a non-active file) projects to GALEC exactly as it compiles for
/// every other target. `target` is exactly `galec`; GALEC never emits C.
///
/// Success shape:
/// ```json
/// { "ok": true, "target": "<target>", "alg": "<.alg text>" }
/// ```
/// Failure shape: `{ "ok": false, "error": "<msg>" }`.
#[wasm_bindgen]
pub fn render_galec(workspace_sources: &str, model_name: &str, target: &str) -> String {
    let value = match render_galec_impl(workspace_sources, model_name, target) {
        Ok(value) => value,
        Err(error) => json!({ "ok": false, "error": error }),
    };
    serde_json::to_string(&value).expect("a JSON value always serializes")
}

/// Compute GALEC `.alg` LSP diagnostics and return them as JSON.
#[wasm_bindgen]
pub fn galec_diagnostics(source: &str, file_name: &str) -> String {
    match compute_diagnostics(source, file_name) {
        Ok(diagnostics) => serialize_language_response(&diagnostics),
        Err(error) => serialize_language_response(&json!({ "error": error.to_string() })),
    }
}

/// Return GALEC hover information for a UTF-16 LSP position, or `null`.
#[wasm_bindgen]
pub fn galec_hover(source: &str, file_name: &str, line: u32, character: u32) -> String {
    let hover = navigation::hover(source, file_name, Position { line, character });
    serialize_language_response(&hover)
}

/// Return the GALEC definition location for a UTF-16 LSP position, or `null`.
#[wasm_bindgen]
pub fn galec_definition(
    source: &str,
    file_name: &str,
    uri: &str,
    line: u32,
    character: u32,
) -> String {
    let definition = Url::parse(uri).ok().and_then(|url| {
        navigation::goto_definition(source, file_name, url, Position { line, character })
    });
    serialize_language_response(&definition)
}

fn serialize_language_response<T: serde::Serialize>(value: &T) -> String {
    serde_json::to_string(value).expect("GALEC language responses are JSON-serializable")
}

fn render_galec_impl(
    workspace_sources: &str,
    model_name: &str,
    target: &str,
) -> Result<Value, String> {
    // 1. Load every workspace document into an in-memory Session, then compile
    //    the requested (resolved) model across all of them — a model defined in
    //    or importing a non-active file compiles just as the core binding's
    //    workspace compile does.
    let documents: BTreeMap<String, String> = serde_json::from_str(workspace_sources)
        .map_err(|error| format!("invalid workspace sources JSON: {error}"))?;
    if documents.is_empty() {
        return Err("no Modelica sources were provided".to_owned());
    }
    let mut session = Session::new(SessionConfig::default());
    for (path, content) in &documents {
        session
            .add_document(path, content)
            .map_err(|error| format!("failed to load `{path}`: {error}"))?;
    }
    let compilation = session
        .compile_model_strict(model_name)
        .map_err(|report| format!("compilation error: {}", report.failure_summary(8)))?;

    if !is_galec_target(target) {
        return Err(unknown_target_error(target));
    }
    let checked = TargetBundle::builtin(target)
        .ok_or_else(|| format!("missing built-in target `{target}`"))?
        .check()
        .map_err(|error| error.to_string())?;
    let input = ArtifactSessionInput::construct(
        "1970-01-01T00:00:00Z"
            .parse::<ArtifactGenerationInstant>()
            .expect("the pinned UTC instant is canonical"),
        "00000000-0000-0000-0000-000000000001"
            .parse::<ArtifactIdentitySeed>()
            .expect("the pinned UUID seed is canonical"),
    );
    let artifact = compilation.render_target(checked, input).map_err(|error| {
        galec_target_diagnostic_message(&error, compilation.result().dae.source_map())
            .unwrap_or_else(|| format!("GALEC target rendering failed: {error:#}"))
    })?;
    let files = artifact.into_rendered_files();
    let alg = files
        .iter()
        .find(|file| file.path() == "AlgorithmCode/model.alg")
        .ok_or_else(|| {
            "checked GALEC artifact lost its standard AlgorithmCode/model.alg member".to_owned()
        })?
        .content()
        .to_owned();

    Ok(json!({
        "ok": true,
        "target": target,
        "alg": alg,
    }))
}

fn galec_target_diagnostic_message(
    error: &anyhow::Error,
    source_map: &SourceMap,
) -> Option<String> {
    let details = if let Some(errors) = error.downcast_ref::<GalecTargetErrors>() {
        errors
            .iter()
            .map(|error| {
                let diagnostic = error.to_diagnostic();
                diagnostic_line(
                    diagnostic.code.as_deref().unwrap_or("GALEC"),
                    &diagnostic.message,
                    diagnostic.labels.first().map(|label| label.span),
                    source_map,
                )
            })
            .collect::<Vec<_>>()
            .join("\n")
    } else {
        let typed = error.downcast_ref::<CodegenError>()?;
        let CodegenError::UnsupportedTargetFeature { span, .. } = typed else {
            return None;
        };
        diagnostic_line("EC009", &typed.to_string(), *span, source_map)
    };
    Some(format!("GALEC target rendering failed:\n{details}"))
}

fn diagnostic_line(
    code: &str,
    message: &str,
    span: Option<rumoca_core::Span>,
    source_map: &SourceMap,
) -> String {
    let location =
        span.and_then(|span| rumoca_compile::compile::source_span_location(source_map, span));
    match location {
        Some(location) => format!(
            "[{code}] {message} ({}:{}:{})",
            location.file_name,
            location.start.line + 1,
            location.start.character + 1,
        ),
        None => format!("[{code}] {message}"),
    }
}

/// Whether `target` is the one GALEC codegen target. GALEC never emits C.
fn is_galec_target(target: &str) -> bool {
    target == GALEC_TARGET
}

/// The SPEC_0008-shaped refusal for a target this addon does not project:
/// name the rejected value AND the admissible set, so a caller can fix the
/// request without reading the source.
fn unknown_target_error(target: &str) -> String {
    format!("'{target}' is not a GALEC codegen target (expected {GALEC_TARGET})")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Fixed-sample discrete model admissible for GALEC projection (mirrors
    /// the `rumoca-compile` galec facade fixture).
    const DISCRETE_SOURCE: &str = r#"
model GalecWasmDemo
  constant Real samplePeriod = 0.001;
  parameter Real gain = 2.0;
  discrete Integer count(start = 0);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = gain * count;
  end when;
end GalecWasmDemo;
"#;

    fn parse(json: &str) -> Value {
        serde_json::from_str(json).expect("render_galec must return valid JSON")
    }

    /// A single-document workspace-sources map (the JSON object `render_galec`
    /// takes): `{ "<path>": "<content>" }`.
    fn workspace(path: &str, source: &str) -> String {
        json!({ path: source }).to_string()
    }

    fn line_character_for(source: &str, needle: &str, offset_in_needle: usize) -> (u32, u32) {
        let offset = source.find(needle).expect("needle present") + offset_in_needle;
        let prefix = &source[..offset];
        let line = prefix.bytes().filter(|byte| *byte == b'\n').count() as u32;
        let character = prefix
            .rsplit_once('\n')
            .map_or(prefix.len(), |(_, tail)| tail.len()) as u32;
        (line, character)
    }

    #[test]
    fn galec_target_returns_alg_only() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            GALEC_TARGET,
        ));
        assert_eq!(value["ok"], true, "{value}");
        assert_eq!(value["target"], GALEC_TARGET);
        assert!(
            value["alg"]
                .as_str()
                .is_some_and(|alg| alg.contains("DoStep")),
            "alg should carry the DoStep method: {value}"
        );
        assert!(value.get("c_header").is_none(), "{value}");
        assert!(value.get("c_source").is_none(), "{value}");
    }

    #[test]
    fn unknown_target_is_a_loud_error() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            "not-a-galec-target",
        ));
        assert_eq!(value["ok"], false);
        assert!(
            value["error"]
                .as_str()
                .is_some_and(|error| error.contains("not a GALEC codegen target")),
            "{value}"
        );
    }

    #[test]
    fn galec_lsp_diagnostics_reports_parse_errors() {
        let value = parse(&galec_diagnostics("block Bad\nend Other;\n", "bad.alg"));
        let diagnostics = value.as_array().expect("diagnostics array");
        assert_eq!(diagnostics.len(), 1, "{value}");
        assert_eq!(diagnostics[0]["source"], "rumoca-galec");
        assert!(
            diagnostics[0]["message"]
                .as_str()
                .is_some_and(|message| !message.is_empty()),
            "{value}"
        );
    }

    #[test]
    fn galec_lsp_hover_and_definition_are_json() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            GALEC_TARGET,
        ));
        let alg = value["alg"].as_str().expect("alg string");
        assert!(
            parse(&galec_diagnostics(alg, "GalecWasmDemo.alg"))
                .as_array()
                .is_some_and(Vec::is_empty),
            "generated GALEC must diagnose cleanly"
        );
        let (line, character) = line_character_for(alg, "self.count :=", "self.".len());

        let hover = parse(&galec_hover(alg, "GalecWasmDemo.alg", line, character));
        assert!(
            hover["contents"].to_string().contains("Integer"),
            "hover should describe the protected count state: {hover}"
        );

        let definition = parse(&galec_definition(
            alg,
            "GalecWasmDemo.alg",
            "file:///GalecWasmDemo.alg",
            line,
            character,
        ));
        assert!(
            definition["range"].is_object(),
            "definition should return a scalar LSP location: {definition}"
        );
    }

    /// A model spanning several workspace files projects to GALEC exactly as it
    /// compiles for every other target — the addon loads all documents, not
    /// just one (regression for the single-active-document gap).
    #[test]
    fn model_spanning_multiple_files_projects() {
        let library = r#"
within Demo;
model Gain
  parameter Real k = 2.0;
end Gain;
"#;
        let top = r#"
within Demo;
model Counter
  extends Demo.Gain;
  constant Real samplePeriod = 0.001;
  discrete Integer count(start = 0);
  discrete output Real y(start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    count = pre(count) + 1;
    y = k * count;
  end when;
end Counter;
"#;
        let sources = json!({
            "Demo/Gain.mo": library,
            "Demo/Counter.mo": top,
        })
        .to_string();
        let value = parse(&render_galec(&sources, "Demo.Counter", GALEC_TARGET));
        assert_eq!(value["ok"], true, "multi-file model must project: {value}");
        assert!(value.get("model_identifier").is_none(), "{value}");
        assert!(
            value["alg"]
                .as_str()
                .is_some_and(|alg| alg.contains("DoStep")),
            "{value}"
        );
    }

    #[test]
    fn empty_workspace_is_a_loud_error() {
        let value = parse(&render_galec("{}", "GalecWasmDemo", GALEC_TARGET));
        assert_eq!(value["ok"], false);
        assert!(
            value["error"]
                .as_str()
                .is_some_and(|error| error.contains("no Modelica sources")),
            "{value}"
        );
    }

    #[test]
    fn continuous_model_is_rejected_at_the_target_capability_layer() {
        let source = r#"
model ContinuousDemo
  Real x(start = 1.0);
  parameter Real k = 2.0;
equation
  der(x) = -k * x;
end ContinuousDemo;
"#;
        let value = parse(&render_galec(
            &workspace("input.mo", source),
            "ContinuousDemo",
            GALEC_TARGET,
        ));
        assert_eq!(value["ok"], false);
        assert!(
            value["error"].as_str().is_some_and(|error| {
                error.contains("[EC009]")
                    && error.contains("unsupported-feature:continuous_states")
                    && error.contains("Target 'galec'")
                    && error.contains("input.mo:3:3")
                    && !error.contains("[EGT001]")
                    && !error.contains("[EGT005]")
            }),
            "{value}"
        );
    }

    #[test]
    fn projection_diagnostic_keeps_code_and_source_location() {
        let source = "model A\n  Real x;\nend A;\n";
        let mut source_map = SourceMap::new();
        let source_id = source_map.add("models/A.mo", source);
        let errors = GalecTargetErrors::from(vec![
            rumoca_phase_galec::GalecTargetError::UnsupportedFeature {
                feature: "test-feature".to_owned(),
                detail: "test refusal".to_owned(),
                span: Some(rumoca_core::Span::from_offsets(source_id, 10, 14)),
            },
        ]);

        let chained = anyhow::Error::new(errors).context("compile target rendering context");
        let rendered = galec_target_diagnostic_message(&chained, &source_map)
            .expect("typed GALEC target errors remain in the adapter chain");
        assert!(rendered.contains("[EGT017]"), "{rendered}");
        assert!(rendered.contains("models/A.mo:2:3"), "{rendered}");
    }
}
