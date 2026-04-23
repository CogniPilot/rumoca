//! Compose a physics model with an in-process Modelica controller.
//!
//! When the config has both `[physics]` and `[controller]` sections, we
//! synthesize a wrapper Modelica model at load time that instantiates
//! both as sub-components and wires them per the `actuate` (controller
//! output → physics input) and `sense` (physics output → controller
//! input) tables. Top-level inputs of the wrapper are the remaining
//! controller inputs (those not satisfied by `sense`), which the runtime
//! input engine drives via `[signals.stepper_inputs]`.
//!
//! The wrapper is returned as a single Modelica source string — just
//! `physics_source ++ controller_source ++ wrapper`. The caller passes
//! that through the existing compile pipeline unchanged.

use std::collections::{BTreeSet, HashMap};

use anyhow::Result;

/// Name of the synthesized wrapper model. Callers pass this as the
/// compiler's top-level model name when composition is active.
pub(crate) const WRAPPER_MODEL_NAME: &str = "ComposedModel";

/// Build the combined Modelica source: physics + controller + synthesized
/// wrapper. Returns the source string and the wrapper model name for the
/// compiler.
pub(crate) fn synthesize(
    physics_source: &str,
    physics_name: &str,
    controller_source: &str,
    controller_name: &str,
    actuate: &HashMap<String, String>,
    sense: &HashMap<String, String>,
) -> Result<String> {
    let controller_inputs = extract_input_names(controller_source);
    let sense_receivers: BTreeSet<&str> = sense.values().map(String::as_str).collect();

    // Top-level wrapper inputs = controller inputs not fed by `sense`.
    let top_inputs: Vec<&str> = controller_inputs
        .iter()
        .filter(|name| !sense_receivers.contains(name.as_str()))
        .map(String::as_str)
        .collect();

    // Top-level wrapper outputs = every public (non-protected, non-input)
    // Real variable declared in the physics model. Passed through as
    // `name = physics.name` so user configs keep using `stepper:px` etc.
    // without caring that physics is now a sub-component.
    let physics_outputs = extract_public_real_names(physics_source, physics_name);

    let mut wrapper = String::new();
    wrapper.push_str(&format!(
        "// Synthesized wrapper composing {physics_name} + {controller_name}.\n"
    ));
    wrapper.push_str(&format!("model {WRAPPER_MODEL_NAME}\n"));

    // Top-level inputs (passed through to controller).
    for name in &top_inputs {
        wrapper.push_str(&format!("  input Real {name}(start = 0);\n"));
    }

    // Top-level outputs (passthroughs from physics sub-component).
    for name in &physics_outputs {
        wrapper.push_str(&format!("  output Real {name};\n"));
    }

    // Sub-components.
    wrapper.push_str(&format!("  {physics_name} physics;\n"));
    wrapper.push_str(&format!("  {controller_name} controller;\n"));

    wrapper.push_str("equation\n");

    // Top-level inputs → controller.
    for name in &top_inputs {
        wrapper.push_str(&format!("  controller.{name} = {name};\n"));
    }

    // Sense: physics output → controller input.
    for (phys_var, ctrl_input) in sense_sorted(sense) {
        wrapper.push_str(&format!(
            "  controller.{ctrl_input} = physics.{phys_var};\n"
        ));
    }

    // Actuate: controller output → physics input.
    for (ctrl_output, phys_input) in actuate_sorted(actuate) {
        wrapper.push_str(&format!(
            "  physics.{phys_input} = controller.{ctrl_output};\n"
        ));
    }

    // Physics output passthroughs.
    for name in &physics_outputs {
        wrapper.push_str(&format!("  {name} = physics.{name};\n"));
    }

    wrapper.push_str(&format!("end {WRAPPER_MODEL_NAME};\n"));

    let mut combined = String::with_capacity(
        physics_source.len() + controller_source.len() + wrapper.len() + 128,
    );
    combined.push_str(physics_source);
    ensure_trailing_newline(&mut combined);
    combined.push_str(controller_source);
    ensure_trailing_newline(&mut combined);
    combined.push_str(&wrapper);

    Ok(combined)
}

fn ensure_trailing_newline(s: &mut String) {
    if !s.ends_with('\n') {
        s.push('\n');
    }
}

fn sense_sorted(sense: &HashMap<String, String>) -> Vec<(&str, &str)> {
    let mut v: Vec<(&str, &str)> = sense
        .iter()
        .map(|(k, v)| (k.as_str(), v.as_str()))
        .collect();
    v.sort();
    v
}

fn actuate_sorted(actuate: &HashMap<String, String>) -> Vec<(&str, &str)> {
    let mut v: Vec<(&str, &str)> = actuate
        .iter()
        .map(|(k, v)| (k.as_str(), v.as_str()))
        .collect();
    v.sort();
    v
}

/// Scan a Modelica source for top-level `input Real <name>` declarations.
///
/// Very pragmatic — matches lines that look like input declarations in
/// the common shape. Does not parse the full grammar; specifically doesn't
/// try to support nested scopes, `inner/outer`, `connector`, etc. That's
/// fine for the controllers we're composing: they use plain
/// `input Real <name>(start = …) "...";` syntax.
fn extract_input_names(source: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut in_block_comment = false;
    for raw in source.lines() {
        let mut line = raw;
        // Block-comment bookkeeping.
        if in_block_comment {
            if let Some(idx) = line.find("*/") {
                line = &line[idx + 2..];
                in_block_comment = false;
            } else {
                continue;
            }
        }
        // Strip inline line comments.
        let line = if let Some(idx) = line.find("//") {
            &line[..idx]
        } else {
            line
        };
        // Track block-comment starts that span the end of the line.
        let line_owned: String;
        let line = if let Some(start) = line.find("/*") {
            if let Some(end_rel) = line[start..].find("*/") {
                line_owned = format!("{}{}", &line[..start], &line[start + end_rel + 2..]);
                line_owned.as_str()
            } else {
                in_block_comment = true;
                &line[..start]
            }
        } else {
            line
        };
        let trimmed = line.trim_start();
        let Some(rest) = trimmed.strip_prefix("input") else {
            continue;
        };
        let rest = rest.trim_start();
        let Some(rest) = rest.strip_prefix("Real") else {
            continue;
        };
        let rest = rest.trim_start();
        // rest starts with "<name>(...)" or "<name>=..." or "<name>;"
        let name: String = rest
            .chars()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect();
        if !name.is_empty() {
            out.push(name);
        }
    }
    out
}

/// Scan a Modelica source for public (non-protected) Real variable
/// declarations inside the named model. Excludes `input`, `parameter`,
/// and anything inside the `protected` section. Used by the synthesizer
/// to auto-expose physics outputs as top-level passthroughs so user
/// configs keep `stepper:px` working after composition.
fn extract_public_real_names(source: &str, model_name: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen: BTreeSet<String> = BTreeSet::new();
    let mut in_target_model = false;
    let mut depth_protected = 0;
    let mut equation_seen = false;
    let model_header = format!("model {model_name}");
    let end_marker = format!("end {model_name}");

    for raw in source.lines() {
        let trimmed = raw.trim();
        if trimmed.starts_with("//") {
            continue;
        }
        if !in_target_model {
            if trimmed.starts_with(&model_header) {
                in_target_model = true;
            }
            continue;
        }
        if trimmed.starts_with(&end_marker) {
            break;
        }
        if trimmed == "protected" {
            depth_protected = 1;
            continue;
        }
        if trimmed == "equation" || trimmed == "algorithm" {
            equation_seen = true;
            continue;
        }
        if depth_protected > 0 || equation_seen {
            continue;
        }
        // We're in the public declaration body. Match lines that look like
        // `[output] Real <name>` (optionally preceded by nothing else).
        let line = trimmed;
        let line = line.strip_prefix("output").map(str::trim_start).unwrap_or(line);
        // Skip input/parameter/constant.
        if line.starts_with("input")
            || line.starts_with("parameter")
            || line.starts_with("constant")
            || line.starts_with("discrete")
        {
            continue;
        }
        let Some(rest) = line.strip_prefix("Real") else {
            continue;
        };
        let rest = rest.trim_start();
        // One declaration per line is the common case. Handle the
        // `Real a, b, c;` shape too.
        // Grab names until `(`, `=`, `"`, or `;`.
        let mut chunk = String::new();
        for ch in rest.chars() {
            if ch == '(' || ch == '=' || ch == '"' || ch == ';' || ch == '\n' {
                break;
            }
            chunk.push(ch);
        }
        for piece in chunk.split(',') {
            let name: String = piece
                .trim()
                .chars()
                .take_while(|c| c.is_alphanumeric() || *c == '_')
                .collect();
            if !name.is_empty() && seen.insert(name.clone()) {
                out.push(name);
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_input_names_from_typical_modelica() {
        let src = r#"
model M
  input Real x(start = 0);
  input Real y (start=1) "pitch";
  // input Real z_commented;
  /* input Real w_also_commented; */
  input Real q;
equation
  // nothing
end M;
"#;
        let names = extract_input_names(src);
        assert_eq!(names, vec!["x", "y", "q"]);
    }

    #[test]
    fn extracts_public_reals_excluding_protected_and_inputs() {
        let src = r#"
model P
  parameter Real k = 1.0;
  input Real u(start = 0);
  Real x(start = 0);
  Real y;
  output Real accel_x;
protected
  Real internal;
equation
  der(x) = u;
  y = x + 1;
  accel_x = der(x);
  internal = 0;
end P;
"#;
        let names = extract_public_real_names(src, "P");
        assert_eq!(names, vec!["x", "y", "accel_x"]);
    }

    #[test]
    fn synthesizes_wrapper_with_routes() {
        let physics = "model P\n  input Real u;\n  Real x;\nequation\n  der(x) = u;\nend P;\n";
        let ctrl = "model C\n  input Real y;\n  input Real r;\n  Real v;\nequation\n  v = r - y;\nend C;\n";
        let actuate: HashMap<String, String> =
            [("v".into(), "u".into())].into_iter().collect();
        let sense: HashMap<String, String> =
            [("x".into(), "y".into())].into_iter().collect();
        let out = synthesize(physics, "P", ctrl, "C", &actuate, &sense).unwrap();
        assert!(out.contains("model ComposedModel"));
        assert!(out.contains("input Real r(start = 0)"));
        assert!(!out.contains("input Real y(start"), "sensed inputs must not be top-level");
        assert!(out.contains("controller.r = r"));
        assert!(out.contains("controller.y = physics.x"));
        assert!(out.contains("physics.u = controller.v"));
    }
}
