//! Lazy GALEC / eFMI codegen addon for the rumoca WASM package.
//!
//! This is a SEPARATE `cdylib` sibling of `rumoca-bind-wasm`: the core
//! rumoca WASM binary (Modelica / template / simulation workflows) must NOT
//! grow the GALEC → eFMI Algorithm Code + embedded-C projection, so this
//! module carries it on its own and is loaded on demand only when a user
//! selects a GALEC codegen target. It mirrors the repo's lazy-diffsol-addon
//! (`rumoca-bind-wasm-diffsol`) and the layered core/rumoca/viz/live
//! packaging direction.
//!
//! It is a thin wasm boundary: [`render_galec`] compiles Modelica in-memory to
//! the canonical DAE + Flat model, then delegates to the shared
//! the checked `rumoca-phase-galec` projection and the generic
//! `rumoca-phase-codegen` Algorithm Code template surface used by the CLI and
//! LSP.

use std::collections::BTreeMap;

use lsp_types::{Position, Url};
use rumoca_compile::codegen::targets::{TargetBundle, TargetTemplateSource};
use rumoca_compile::{Session, SessionConfig};
use rumoca_phase_parse_galec::parse as parse_galec;
use rumoca_tool_lsp_galec::{compute_diagnostics, navigation};
use serde_json::{Value, json};
use wasm_bindgen::prelude::*;

const GALEC_TARGET: &str = "galec";
const GALEC_PRODUCTION_TARGET: &str = "galec-production";
const EMBEDDED_C_GALEC_TARGET: &str = "embedded-c-galec";

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
/// every other target. `target` is one of `galec`, `galec-production`,
/// `embedded-c-galec`.
///
/// Success shape:
/// ```json
/// { "ok": true, "target": "<target>", "model_identifier": "<id>",
///   "alg": "<.alg text>", "c_header": "<.h text or empty>",
///   "c_source": "<.c text or empty>" }
/// ```
/// The `c_header`/`c_source` fields are empty strings for the `galec` target
/// (Algorithm Code only). Failure shape: `{ "ok": false, "error": "<msg>" }`.
#[wasm_bindgen]
pub fn render_galec(workspace_sources: &str, model_name: &str, target: &str) -> String {
    let value = match render_galec_impl(workspace_sources, model_name, target) {
        Ok(value) => value,
        Err(error) => json!({ "ok": false, "error": error }),
    };
    // A `serde_json::Value` built from strings always serializes; fall back to
    // a hand-built error string on the impossible failure so the contract
    // (always a JSON string) still holds.
    serde_json::to_string(&value).unwrap_or_else(|error| {
        format!("{{\"ok\":false,\"error\":\"response serialization failed: {error}\"}}")
    })
}

/// Compute GALEC `.alg` LSP diagnostics and return them as JSON.
#[wasm_bindgen]
pub fn galec_diagnostics(source: &str, file_name: &str) -> String {
    serialize_language_response(&compute_diagnostics(source, file_name))
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

/// Parse an edited GALEC `.alg` block and render GALEC-derived C files.
///
/// This is the editor-owned second step for the docs/playground flow:
/// Modelica projection produces editable `.alg`; this function consumes the
/// current `.alg` text and emits `.h`/`.c` without re-reading the Modelica
/// source. It is intentionally source-only: the eFMI container and Production
/// Code manifests remain the native CLI packaging step.
#[wasm_bindgen]
pub fn render_galec_c_from_alg(
    alg_source: &str,
    file_name: &str,
    model_name: &str,
    target: &str,
) -> String {
    let value = match render_galec_c_from_alg_impl(alg_source, file_name, model_name, target) {
        Ok(value) => value,
        Err(error) => json!({ "ok": false, "error": error }),
    };
    serde_json::to_string(&value).unwrap_or_else(|error| {
        format!("{{\"ok\":false,\"error\":\"response serialization failed: {error}\"}}")
    })
}

fn serialize_language_response<T: serde::Serialize>(value: &T) -> String {
    serde_json::to_string(value)
        .unwrap_or_else(|error| format!("{{\"error\":\"JSON serialization failed: {error}\"}}"))
}

fn render_galec_c_from_alg_impl(
    alg_source: &str,
    file_name: &str,
    model_name: &str,
    target: &str,
) -> Result<Value, String> {
    let checked = parse_galec(alg_source, file_name)
        .map_err(|error| format!("GALEC parse error: {error}"))?;
    if !matches!(target, EMBEDDED_C_GALEC_TARGET | GALEC_PRODUCTION_TARGET) {
        return Err(format!("target `{target}` does not emit C"));
    }
    let model_id = model_name.replace('.', "_");
    let bundle = TargetBundle::builtin(EMBEDDED_C_GALEC_TARGET)
        .ok_or_else(|| "missing built-in embedded-c-galec target".to_owned())?;
    let artifact = SourceArtifactFacts {
        generated_at: "1970-01-01T00:00:00Z",
        generation_tool: "rumoca wasm edited Algorithm Code",
        identities: BTreeMap::new(),
        checksums: BTreeMap::new(),
    };
    let header = bundle
        .template_source("model.h.jinja")
        .map_err(|error| error.to_string())?;
    let source = bundle
        .template_source("model.c.jinja")
        .map_err(|error| error.to_string())?;
    let c_header =
        rumoca_phase_codegen::render_checked_algorithm_block_template_with_artifact(
            &checked,
            &artifact,
            header.as_ref(),
            &model_id,
        )
        .map_err(|error| error.to_string())?;
    let c_source =
        rumoca_phase_codegen::render_checked_algorithm_block_template_with_artifact(
            &checked,
            &artifact,
            source.as_ref(),
            &model_id,
        )
        .map_err(|error| error.to_string())?;
    Ok(json!({
        "ok": true,
        "target": target,
        "model_identifier": model_id,
        "c_header": c_header,
        "c_source": c_source,
    }))
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
    let result = session
        .compile_model(model_name)
        .map_err(|error| format!("compilation error: {error}"))?;

    // 2. Delegate to the shared identity-free renderer (validates the target,
    //    projects to GALEC, and renders the .alg + C with the target's
    //    conformance header). GALEC identifiers/C names cannot contain dots.
    let model_id = model_name.replace('.', "_");
    let sources = render_checked_sources(&result.dae, &model_id, target)?;

    Ok(json!({
        "ok": true,
        "target": target,
        // The file-system-safe identifier the projection and the C `#include`
        // both use (dots -> underscores). The web layer names the .alg/.h/.c
        // files with THIS so the generated `#include "<id>.h"` resolves; naming
        // them by the bare model leaf breaks C compilation for a package-
        // qualified model (e.g. `MyLib.Demo` -> include `MyLib_Demo.h`).
        "model_identifier": model_id,
        "alg": sources.alg,
        "c_header": sources.c_header,
        "c_source": sources.c_source,
    }))
}

#[derive(serde::Serialize)]
struct SourceArtifactFacts {
    generated_at: &'static str,
    generation_tool: &'static str,
    identities: BTreeMap<String, String>,
    checksums: BTreeMap<String, String>,
}

struct RenderedSources {
    alg: String,
    c_header: String,
    c_source: String,
}

fn render_checked_sources(
    dae: &rumoca_compile::compile::Dae,
    model_id: &str,
    target: &str,
) -> Result<RenderedSources, String> {
    if !matches!(
        target,
        GALEC_TARGET | GALEC_PRODUCTION_TARGET | EMBEDDED_C_GALEC_TARGET
    ) {
        return Err(format!("unknown GALEC target `{target}`"));
    }
    let bundle =
        TargetBundle::builtin(target).ok_or_else(|| format!("missing built-in target `{target}`"))?;
    let manifest = bundle.parse_manifest().map_err(|error| error.to_string())?;
    let package = rumoca_phase_galec::lower_to_algorithm_code(
        &rumoca_phase_galec::GalecInput::new(dae, model_id),
        &rumoca_phase_galec::GalecOptions::default(),
    )
    .map_err(|diagnostics| {
        diagnostics
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("; ")
    })?;
    let artifact = SourceArtifactFacts {
        generated_at: "1970-01-01T00:00:00Z",
        generation_tool: "rumoca wasm source preview",
        identities: BTreeMap::new(),
        checksums: BTreeMap::new(),
    };
    let mut rendered = RenderedSources {
        alg: String::new(),
        c_header: String::new(),
        c_source: String::new(),
    };
    for file in &manifest.files {
        let extension = std::path::Path::new(&file.path)
            .extension()
            .and_then(|value| value.to_str());
        if !matches!(extension, Some("alg" | "h" | "c")) {
            continue;
        }
        let source = bundle
            .template_source(&file.template)
            .map_err(|error| error.to_string())?;
        let content = rumoca_phase_codegen::render_algorithm_code_template_with_artifact(
            &package,
            &artifact,
            source.as_ref(),
            model_id,
        )
        .map_err(|error| error.to_string())?;
        match extension {
            Some("alg") => rendered.alg = content,
            Some("h") => rendered.c_header = content,
            Some("c") => rendered.c_source = content,
            _ => {}
        }
    }
    Ok(rendered)
}

#[cfg(test)]
mod tests {
    use super::*;
    const EMBEDDED_C_GALEC_CONFORMANCE_LINES: &[&str] =
        &["GALEC-derived embedded C export"];
    const PRODUCTION_CONFORMANCE_LINES: &[&str] = &["eFMI Production Code export"];
    const PRODUCTION_CONFORMANCE_SUMMARY: &str = "eFMI Production Code export";

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
        assert_eq!(value["c_header"], "");
        assert_eq!(value["c_source"], "");
    }

    #[test]
    fn embedded_c_target_renders_c_with_not_a_container_header() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            EMBEDDED_C_GALEC_TARGET,
        ));
        assert_eq!(value["ok"], true, "{value}");
        let header = value["c_header"].as_str().expect("c_header string");
        let source = value["c_source"].as_str().expect("c_source string");
        assert!(header.contains("GalecWasmDemoState"), "{header}");
        assert!(source.contains("_dostep("), "{source}");
        assert!(
            header.contains(EMBEDDED_C_GALEC_CONFORMANCE_LINES[0]),
            "embedded-c header must self-describe as NOT a container: {header}"
        );
    }

    #[test]
    fn production_target_renders_c_with_production_conformance_header() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            GALEC_PRODUCTION_TARGET,
        ));
        assert_eq!(value["ok"], true, "{value}");
        let header = value["c_header"].as_str().expect("c_header string");
        let source = value["c_source"].as_str().expect("c_source string");
        assert!(
            header.contains(PRODUCTION_CONFORMANCE_LINES[0]),
            "production header must claim the PC representation: {header}"
        );
        assert!(
            source.contains(PRODUCTION_CONFORMANCE_SUMMARY),
            "production source must carry the PC summary: {source}"
        );
        assert!(
            !header.contains("NOT an eFMI Production Code container"),
            "the embedded-c NOT-a-container claim must not leak into production: {header}"
        );
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

    #[test]
    fn edited_alg_text_renders_c_without_modelica_source() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            GALEC_TARGET,
        ));
        let alg = value["alg"].as_str().expect("alg string");
        let c = parse(&render_galec_c_from_alg(
            alg,
            "GalecWasmDemo.alg",
            "GalecWasmDemo",
            EMBEDDED_C_GALEC_TARGET,
        ));
        assert_eq!(c["ok"], true, "{c}");
        assert!(
            c["c_header"]
                .as_str()
                .is_some_and(|header| header.contains("GalecWasmDemoState")),
            "{c}"
        );
        assert!(
            c["c_source"]
                .as_str()
                .is_some_and(|source| source.contains("_dostep(")),
            "{c}"
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
        assert_eq!(value["model_identifier"], "Demo_Counter");
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
    fn continuous_model_is_rejected_with_projection_diagnostics() {
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
            value["error"]
                .as_str()
                .is_some_and(|error| error.contains("projection rejected")),
            "{value}"
        );
    }
}
