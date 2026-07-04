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

/// Compile Modelica `source`, project the model named `model_name` to GALEC,
/// and return the rendered artifacts as a JSON string.
///
/// `target` is one of `galec`, `galec-production`, `embedded-c-galec`.
///
/// Success shape:
/// ```json
/// { "ok": true, "target": "<target>", "alg": "<.alg text>",
///   "c_header": "<.h text or empty>", "c_source": "<.c text or empty>" }
/// ```
/// The `c_header`/`c_source` fields are empty strings for the `galec` target
/// (Algorithm Code only). Failure shape: `{ "ok": false, "error": "<msg>" }`.
#[wasm_bindgen]
pub fn render_galec(source: &str, model_name: &str, target: &str) -> String {
    let value = match render_galec_impl(source, model_name, target) {
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

fn render_galec_impl(source: &str, model_name: &str, target: &str) -> Result<Value, String> {
    // 1. Compile the source in-memory to the canonical DAE + Flat model
    //    (mirror the sibling core binding's in-memory Session path).
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("input.mo", source)
        .map_err(|error| format!("failed to load source: {error}"))?;
    let requested = qualify_input_model_name(&session, model_name);
    let result = session
        .compile_model(&requested)
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
        "alg": sources.alg,
        "c_header": sources.c_header,
        "c_source": sources.c_source,
    }))
}

/// Qualify a bare model name against the in-memory document's `within` prefix
/// (mirrors `rumoca-bind-wasm`'s `qualify_input_model_name`): a dotted name is
/// already qualified, and a bare top-level class in a `within P;` file resolves
/// to `P.<name>`.
fn qualify_input_model_name(session: &Session, model_name: &str) -> String {
    if model_name.contains('.') {
        return model_name.to_string();
    }
    let Some(doc) = session.get_document("input.mo") else {
        return model_name.to_string();
    };
    let Some(parsed) = doc.parsed().or_else(|| doc.recovered()) else {
        return model_name.to_string();
    };
    if !parsed.classes.contains_key(model_name) {
        return model_name.to_string();
    }
    let within = parsed
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|prefix| !prefix.is_empty());
    within.map_or_else(
        || model_name.to_string(),
        |prefix| format!("{prefix}.{model_name}"),
    )
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

    #[test]
    fn galec_target_returns_alg_only() {
        let value = parse(&render_galec(
            DISCRETE_SOURCE,
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
            DISCRETE_SOURCE,
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
            DISCRETE_SOURCE,
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
        let value = parse(&render_galec(DISCRETE_SOURCE, "GalecWasmDemo", "fmi3"));
        assert_eq!(value["ok"], false);
        assert!(
            value["error"]
                .as_str()
                .is_some_and(|error| error.contains("not a GALEC codegen target")),
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
        let value = parse(&render_galec(source, "ContinuousDemo", GALEC_TARGET));
        assert_eq!(value["ok"], false);
        assert!(
            value["error"]
                .as_str()
                .is_some_and(|error| error.contains("projection rejected")),
            "{value}"
        );
    }
}
