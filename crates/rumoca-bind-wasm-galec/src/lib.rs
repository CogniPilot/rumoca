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
//! [`rumoca_compile::galec::render_galec_sources`] — the one renderer behind
//! the CLI, the LSP, and this addon — which projects to GALEC (`.alg`) and, for
//! the C targets, the embedded C, identity-free (no eFMU container / UUID /
//! clock, so it is safe on `wasm32-unknown-unknown`).

use std::collections::BTreeMap;

use rumoca_compile::galec::render_galec_sources;
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
    let sources = render_galec_sources(&result.dae, &result.flat, &model_id, target)
        .map_err(|error| error.to_string())?;

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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_compile::galec::{
        EMBEDDED_C_GALEC_CONFORMANCE_LINES, EMBEDDED_C_GALEC_TARGET, GALEC_PRODUCTION_TARGET,
        GALEC_TARGET, PRODUCTION_CONFORMANCE_LINES, PRODUCTION_CONFORMANCE_SUMMARY,
    };

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
            "fmi3",
        ));
        assert_eq!(value["ok"], false);
        assert!(
            value["error"]
                .as_str()
                .is_some_and(|error| error.contains("not a GALEC codegen target")),
            "{value}"
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
