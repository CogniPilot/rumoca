//! Lazy GALEC / eFMI codegen addon for the rumoca WASM package.
//!
//! This is a SEPARATE `cdylib` sibling of `rumoca-bind-wasm`: the core
//! rumoca WASM binary (Modelica / template / simulation workflows) must NOT
//! grow the GALEC projection, so this module carries it on its own and is
//! loaded on demand only when a user selects a GALEC codegen target. It
//! mirrors the repo's lazy-diffsol-addon (`rumoca-bind-wasm-diffsol`) and
//! the layered core/rumoca/viz/live packaging direction.
//!
//! It is a thin wasm boundary: [`render_galec`] compiles Modelica in-memory to
//! the canonical DAE, then delegates to `rumoca_galec` (analyze → transform →
//! render) — the same pipeline the CLI's `embedded-c-galec` and
//! `embedded-rust-galec` targets use — identity-free (no eFMU container /
//! UUID / clock, so it is safe on `wasm32-unknown-unknown`).

use std::collections::BTreeMap;

use rumoca_compile::{Session, SessionConfig};
use serde_json::{Value, json};
use wasm_bindgen::prelude::*;

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
/// every other target. `target` is one of `embedded-c-galec`,
/// `embedded-rust-galec`.
///
/// Success shape:
/// ```json
/// { "ok": true, "target": "<target>", "model_identifier": "<id>",
///   "alg": "<.alg text>", "c_header": "<.h text or empty>",
///   "c_source": "<.c/.rs text or empty>" }
/// ```
/// The `c_header`/`c_source` fields are empty strings for targets that don't
/// emit them. Failure shape: `{ "ok": false, "error": "<msg>" }`.
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
///
/// Pending: port `rumoca-tool-galec-lsp` to the new `rumoca-galec` IR.
/// Returns an empty diagnostics array until the port is complete.
#[wasm_bindgen]
pub fn galec_diagnostics(_source: &str, _file_name: &str) -> String {
    "[]".to_owned()
}

/// Return GALEC hover information for a UTF-16 LSP position, or `null`.
///
/// Pending: port `rumoca-tool-galec-lsp` to the new `rumoca-galec` IR.
/// Always returns `null` until the port is complete.
#[wasm_bindgen]
pub fn galec_hover(_source: &str, _file_name: &str, _line: u32, _character: u32) -> String {
    "null".to_owned()
}

/// Return the GALEC definition location for a UTF-16 LSP position, or `null`.
///
/// Pending: port `rumoca-tool-galec-lsp` to the new `rumoca-galec` IR.
/// Always returns `null` until the port is complete.
#[wasm_bindgen]
pub fn galec_definition(
    _source: &str,
    _file_name: &str,
    _uri: &str,
    _line: u32,
    _character: u32,
) -> String {
    "null".to_owned()
}

/// Parse an edited GALEC `.alg` block and render GALEC-derived C files.
///
/// This is the editor-owned second step for the docs/playground flow:
/// Modelica projection produces editable `.alg`; this function consumes the
/// current `.alg` text and emits `.h`/`.c` without re-reading the Modelica
/// source. Pending: bridging the `.alg` parser output to the new `rumoca-galec`
/// IR so rendering can be re-driven from edited text.
#[cfg(any())]
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

/// Pending bridge from the `rumoca-ir-galec` block parser to the new
/// `rumoca-galec` IR; kept here for reference until that path is restored.
#[cfg(any())]
fn render_galec_c_from_alg_impl(
    alg_source: &str,
    file_name: &str,
    model_name: &str,
    _target: &str,
) -> Result<Value, String> {
    let _block = parse_galec(alg_source, file_name)
        .map_err(|error| format!("GALEC parse error: {error}"))?;
    let _model_id = model_name.replace('.', "_");
    Err(
        "render_galec_c_from_alg is pending re-implementation on the new rumoca-galec pipeline"
            .to_owned(),
    )
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

    // 2. Analyze and transform through the new rumoca-galec pipeline.
    let model_id = model_name.replace('.', "_");
    let analysis = rumoca_galec::analysis::analyze(&result.dae).map_err(|errors| {
        format!(
            "GALEC projection rejected the model: {}",
            errors
                .iter()
                .map(|e| format!("{e:?}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    })?;
    let galec = rumoca_galec::transformation::transform(
        rumoca_galec::transformation::TransformationInput {
            dae: &result.dae,
            analysis,
            model_name: model_id.clone(),
        },
    )
    .map_err(|e| format!("GALEC transformation failed: {e}"))?;

    // 3. Render the .alg block (always) then target-specific sources.
    let alg = rumoca_galec::render::render(&galec)
        .map_err(|e| format!("GALEC .alg render failed: {e}"))?;

    let (c_header, c_source) = match target {
        "embedded-c-galec" => {
            let h = rumoca_galec::render::render_c_header(&galec)
                .map_err(|e| format!("GALEC C header render failed: {e}"))?;
            let c = rumoca_galec::render::render_c_source(&galec)
                .map_err(|e| format!("GALEC C source render failed: {e}"))?;
            (h, c)
        }
        "embedded-rust-galec" => {
            let rs = rumoca_galec::render::render_rust(&galec)
                .map_err(|e| format!("GALEC Rust render failed: {e}"))?;
            (String::new(), rs)
        }
        _ => {
            return Err(format!(
                "'{target}' is not a supported GALEC target \
                 (expected embedded-c-galec or embedded-rust-galec)"
            ));
        }
    };

    Ok(json!({
        "ok": true,
        "target": target,
        "model_identifier": model_id,
        "alg": alg,
        "c_header": c_header,
        "c_source": c_source,
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Fixed-sample discrete model admissible for GALEC projection.
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

    #[cfg(any())]
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
    fn embedded_c_target_renders_c_with_state_struct_and_dostep() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            "embedded-c-galec",
        ));
        assert_eq!(value["ok"], true, "{value}");
        assert_eq!(value["target"], "embedded-c-galec");
        let header = value["c_header"].as_str().expect("c_header string");
        let source = value["c_source"].as_str().expect("c_source string");
        assert!(header.contains("GalecWasmDemoState"), "{header}");
        assert!(source.contains("_dostep("), "{source}");
    }

    #[test]
    fn embedded_rust_target_renders_rs() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            "embedded-rust-galec",
        ));
        assert_eq!(value["ok"], true, "{value}");
        assert_eq!(value["target"], "embedded-rust-galec");
        assert_eq!(value["c_header"], "");
        let rs = value["c_source"].as_str().expect("c_source string");
        assert!(rs.contains("GalecWasmDemoState"), "{rs}");
    }

    #[test]
    fn unknown_target_is_a_loud_error() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            "fmi3",
        ));
        assert_eq!(value["ok"], false);
        assert!(
            value["error"]
                .as_str()
                .is_some_and(|error| error.contains("not a supported GALEC target")),
            "{value}"
        );
    }

    #[cfg(any())]
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

    #[cfg(any())]
    #[test]
    fn galec_lsp_hover_and_definition_are_json() {
        let value = parse(&render_galec(
            &workspace("input.mo", DISCRETE_SOURCE),
            "GalecWasmDemo",
            "embedded-c-galec",
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
        let value = parse(&render_galec(&sources, "Demo.Counter", "embedded-c-galec"));
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
        let value = parse(&render_galec("{}", "GalecWasmDemo", "embedded-c-galec"));
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
            "embedded-c-galec",
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
