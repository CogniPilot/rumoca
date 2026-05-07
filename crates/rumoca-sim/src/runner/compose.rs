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
use std::sync::Arc;

use anyhow::{Context, Result};
use rumoca_compile::compile::{
    AstCausality, AstComponent, AstComponentRefPart, AstComponentReference, AstExpression,
    AstForIndex, AstSubscript, AstToken, AstVariability, Session,
};

/// Name of the synthesized wrapper model. Callers pass this as the
/// compiler's top-level model name when composition is active.
pub const WRAPPER_MODEL_NAME: &str = "ComposedModel";

#[derive(Debug, Clone, PartialEq, Eq)]
struct PublicRealDecl {
    name: String,
    dimensions: String,
    start_modifier: Option<String>,
}

/// Build the combined Modelica source: physics + controller + synthesized
/// wrapper. Returns the source string and the wrapper model name for the
/// compiler.
pub fn synthesize(
    physics_source: &str,
    physics_name: &str,
    controller_source: &str,
    controller_name: &str,
    actuate: &HashMap<String, String>,
    sense: &HashMap<String, String>,
) -> Result<String> {
    let physics_components = class_components_from_source(physics_source, physics_name, "physics")?;
    let controller_components =
        class_components_from_source(controller_source, controller_name, "controller")?;
    let sense_receivers: BTreeSet<&str> = sense.values().map(String::as_str).collect();

    // Top-level wrapper inputs = controller inputs not fed by `sense`.
    let top_inputs: Vec<String> = controller_components
        .iter()
        .filter(|component| {
            is_real_component(component)
                && matches!(&component.causality, AstCausality::Input(_))
                && !sense_receivers.contains(component.name.as_str())
        })
        .map(|component| component.name.clone())
        .collect();

    // Top-level wrapper outputs = actual public output Real variables declared
    // in the physics model. Passed through as `name = physics.name` so user
    // configs keep using `stepper:px` etc. without caring that physics is now
    // a sub-component.
    let physics_component_names: BTreeSet<String> = physics_components
        .iter()
        .map(|component| component.name.clone())
        .collect();
    let physics_outputs = physics_components
        .iter()
        .filter_map(|component| public_real_decl(component, &physics_component_names))
        .collect::<Vec<_>>();

    let mut wrapper = String::new();
    wrapper.push_str(&format!(
        "// Synthesized wrapper composing {physics_name} + {controller_name}.\n"
    ));
    wrapper.push_str(&format!("model {WRAPPER_MODEL_NAME}\n"));

    // Top-level inputs (passed through to controller).
    for name in &top_inputs {
        wrapper.push_str(&format!("  input Real {name}(start = 0, fixed = true);\n"));
    }

    // Top-level outputs (passthroughs from physics sub-component).
    for decl in &physics_outputs {
        wrapper.push_str(&format!(
            "  output Real {}{}{};\n",
            decl.name,
            decl.dimensions,
            decl.start_modifier.as_deref().unwrap_or("")
        ));
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
    for decl in &physics_outputs {
        wrapper.push_str(&format!("  {0} = physics.{0};\n", decl.name));
    }

    wrapper.push_str(&format!("end {WRAPPER_MODEL_NAME};\n"));

    let mut combined =
        String::with_capacity(physics_source.len() + controller_source.len() + wrapper.len() + 128);
    combined.push_str(physics_source);
    ensure_trailing_newline(&mut combined);
    combined.push_str(controller_source);
    ensure_trailing_newline(&mut combined);
    combined.push_str(&wrapper);

    Ok(combined)
}

fn class_components_from_source(
    source: &str,
    class_name: &str,
    role: &str,
) -> Result<Vec<AstComponent>> {
    let mut session = Session::default();
    let uri = format!("{role}.mo");
    session
        .add_document(&uri, source)
        .with_context(|| format!("Parse {role} Modelica source"))?;
    session
        .class_components_query(&uri, class_name)
        .with_context(|| format!("Find {role} model class `{class_name}`"))
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

fn public_real_decl(
    component: &AstComponent,
    physics_component_names: &BTreeSet<String>,
) -> Option<PublicRealDecl> {
    if !is_real_component(component)
        || component.is_protected
        || !matches!(&component.causality, AstCausality::Output(_))
        || matches!(
            &component.variability,
            AstVariability::Parameter(_)
                | AstVariability::Constant(_)
                | AstVariability::Discrete(_)
        )
    {
        return None;
    }

    Some(PublicRealDecl {
        name: component.name.clone(),
        dimensions: component_dimensions(component),
        start_modifier: output_start_modifier(component, physics_component_names),
    })
}

fn output_start_modifier(
    component: &AstComponent,
    physics_component_names: &BTreeSet<String>,
) -> Option<String> {
    component.start_is_modification.then(|| {
        let start = qualify_physics_start_expr(&component.start, physics_component_names);
        let each = if component.start_has_each {
            "each "
        } else {
            ""
        };
        format!("({each}start = {start})")
    })
}

fn qualify_physics_start_arc(
    expr: &AstExpression,
    physics_component_names: &BTreeSet<String>,
) -> Arc<AstExpression> {
    Arc::new(qualify_physics_start_expr(expr, physics_component_names))
}

fn qualify_physics_start_exprs(
    elements: &[AstExpression],
    physics_component_names: &BTreeSet<String>,
) -> Vec<AstExpression> {
    elements
        .iter()
        .map(|element| qualify_physics_start_expr(element, physics_component_names))
        .collect()
}

fn qualify_physics_start_branches(
    branches: &[(AstExpression, AstExpression)],
    physics_component_names: &BTreeSet<String>,
) -> Vec<(AstExpression, AstExpression)> {
    branches
        .iter()
        .map(|(cond, value)| {
            (
                qualify_physics_start_expr(cond, physics_component_names),
                qualify_physics_start_expr(value, physics_component_names),
            )
        })
        .collect()
}

fn qualify_physics_start_indices(
    indices: &[AstForIndex],
    physics_component_names: &BTreeSet<String>,
) -> Vec<AstForIndex> {
    indices
        .iter()
        .map(|index| AstForIndex {
            ident: index.ident.clone(),
            range: qualify_physics_start_expr(&index.range, physics_component_names),
        })
        .collect()
}

fn qualify_physics_start_expr(
    expr: &AstExpression,
    physics_component_names: &BTreeSet<String>,
) -> AstExpression {
    match expr {
        AstExpression::Range { start, step, end } => AstExpression::Range {
            start: qualify_physics_start_arc(start, physics_component_names),
            step: step
                .as_ref()
                .map(|expr| qualify_physics_start_arc(expr, physics_component_names)),
            end: qualify_physics_start_arc(end, physics_component_names),
        },
        AstExpression::Unary { op, rhs } => AstExpression::Unary {
            op: op.clone(),
            rhs: qualify_physics_start_arc(rhs, physics_component_names),
        },
        AstExpression::Binary { op, lhs, rhs } => AstExpression::Binary {
            op: op.clone(),
            lhs: qualify_physics_start_arc(lhs, physics_component_names),
            rhs: qualify_physics_start_arc(rhs, physics_component_names),
        },
        AstExpression::ComponentReference(reference) => AstExpression::ComponentReference(
            qualify_physics_component_ref(reference, physics_component_names),
        ),
        AstExpression::FunctionCall { comp, args } => AstExpression::FunctionCall {
            comp: comp.clone(),
            args: qualify_physics_start_exprs(args, physics_component_names),
        },
        AstExpression::ClassModification {
            target,
            modifications,
        } => AstExpression::ClassModification {
            target: target.clone(),
            modifications: qualify_physics_start_exprs(modifications, physics_component_names),
        },
        AstExpression::NamedArgument { name, value } => AstExpression::NamedArgument {
            name: name.clone(),
            value: qualify_physics_start_arc(value, physics_component_names),
        },
        AstExpression::Modification { target, value } => AstExpression::Modification {
            target: target.clone(),
            value: qualify_physics_start_arc(value, physics_component_names),
        },
        AstExpression::Array {
            elements,
            is_matrix,
        } => AstExpression::Array {
            elements: qualify_physics_start_exprs(elements, physics_component_names),
            is_matrix: *is_matrix,
        },
        AstExpression::Tuple { elements } => AstExpression::Tuple {
            elements: qualify_physics_start_exprs(elements, physics_component_names),
        },
        AstExpression::If {
            branches,
            else_branch,
        } => AstExpression::If {
            branches: qualify_physics_start_branches(branches, physics_component_names),
            else_branch: qualify_physics_start_arc(else_branch, physics_component_names),
        },
        AstExpression::Parenthesized { inner } => AstExpression::Parenthesized {
            inner: qualify_physics_start_arc(inner, physics_component_names),
        },
        AstExpression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => AstExpression::ArrayComprehension {
            expr: qualify_physics_start_arc(expr, physics_component_names),
            indices: qualify_physics_start_indices(indices, physics_component_names),
            filter: filter
                .as_ref()
                .map(|expr| qualify_physics_start_arc(expr, physics_component_names)),
        },
        AstExpression::ArrayIndex { base, subscripts } => AstExpression::ArrayIndex {
            base: qualify_physics_start_arc(base, physics_component_names),
            subscripts: subscripts
                .iter()
                .map(|subscript| qualify_physics_subscript(subscript, physics_component_names))
                .collect(),
        },
        AstExpression::FieldAccess { base, field } => AstExpression::FieldAccess {
            base: qualify_physics_start_arc(base, physics_component_names),
            field: field.clone(),
        },
        AstExpression::Empty | AstExpression::Terminal { .. } => expr.clone(),
    }
}

fn qualify_physics_component_ref(
    reference: &AstComponentReference,
    physics_component_names: &BTreeSet<String>,
) -> AstComponentReference {
    let Some(first) = reference.parts.first() else {
        return reference.clone();
    };
    if reference.local || first.ident.text.as_ref() == "physics" {
        return reference.clone();
    }

    let mut parts = Vec::with_capacity(reference.parts.len() + 1);
    parts.push(AstComponentRefPart {
        ident: AstToken {
            text: Arc::from("physics"),
            ..AstToken::default()
        },
        subs: None,
    });
    parts.extend(reference.parts.iter().cloned().map(|mut part| {
        part.subs = part.subs.map(|subs| {
            subs.iter()
                .map(|subscript| qualify_physics_subscript(subscript, physics_component_names))
                .collect()
        });
        part
    }));

    AstComponentReference {
        local: false,
        parts,
        def_id: None,
    }
}

fn qualify_physics_subscript(
    subscript: &AstSubscript,
    physics_component_names: &BTreeSet<String>,
) -> AstSubscript {
    match subscript {
        AstSubscript::Expression(expr) => {
            AstSubscript::Expression(qualify_physics_start_expr(expr, physics_component_names))
        }
        AstSubscript::Empty | AstSubscript::Range { .. } => subscript.clone(),
    }
}

fn is_real_component(component: &AstComponent) -> bool {
    component.type_name.to_string() == "Real"
}

fn component_dimensions(component: &AstComponent) -> String {
    if !component.shape_expr.is_empty() {
        return format!(
            "[{}]",
            component
                .shape_expr
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        );
    }
    if component.shape.is_empty() {
        return String::new();
    }
    format!(
        "[{}]",
        component
            .shape
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn synthesizes_wrapper_with_routes() {
        let physics = "model P\n  input Real u;\n  Real x;\nequation\n  der(x) = u;\nend P;\n";
        let ctrl = "model C\n  input Real y;\n  input Real r;\n  output Real v;\nequation\n  v = r - y;\nend C;\n";
        let actuate: HashMap<String, String> = [("v".into(), "u".into())].into_iter().collect();
        let sense: HashMap<String, String> = [("x".into(), "y".into())].into_iter().collect();
        let out = synthesize(physics, "P", ctrl, "C", &actuate, &sense).unwrap();
        assert!(out.contains("model ComposedModel"));
        assert!(out.contains("input Real r(start = 0, fixed = true)"));
        assert!(
            !out.contains("input Real y(start"),
            "sensed inputs must not be top-level"
        );
        assert!(out.contains("controller.r = r"));
        assert!(out.contains("controller.y = physics.x"));
        assert!(out.contains("physics.u = controller.v"));
    }

    #[test]
    fn synthesizer_uses_ast_components_and_ignores_nested_class_members() {
        let physics = r#"
model P
  model Motor
    output Real thrust;
  equation
    thrust = 1;
  end Motor;

  input Real u;
  parameter Real p_start[3] = {0, 0, 0.12};
  output Real position[3](start = p_start);
  Real x;
protected
  Real hidden;
equation
  x = u;
end P;
"#;
        let ctrl = "model C\n  input Real r;\n  output Real v;\nequation\n  v = r;\nend C;\n";
        let actuate: HashMap<String, String> = [("v".into(), "u".into())].into_iter().collect();
        let sense = HashMap::new();

        let out = synthesize(physics, "P", ctrl, "C", &actuate, &sense).unwrap();
        let wrapper = out
            .split("// Synthesized wrapper")
            .nth(1)
            .expect("wrapper should be appended");
        assert!(wrapper.contains("output Real position[3](start = physics.p_start)"));
        assert!(!wrapper.contains("output Real x"));
        assert!(!wrapper.contains("output Real thrust"));
        assert!(!wrapper.contains("thrust = physics.thrust"));
        assert!(!wrapper.contains("output Real hidden"));
    }

    #[test]
    fn compiled_wrapper_does_not_expose_nested_physics_inputs() {
        let physics = r#"
model P
  model Motor
    input Real omega_cmd;
    output Real omega(start = 0);
  equation
    der(omega) = omega_cmd - omega;
  end Motor;

  input Real omega_cmd[2];
  output Real omega[2];
protected
  Motor motor[2];
equation
  motor.omega_cmd = omega_cmd;
  omega = motor.omega;
end P;
"#;
        let ctrl = r#"
model C
  input Real stick;
  output Real motor_cmd_0;
  output Real motor_cmd_1;
equation
  motor_cmd_0 = stick;
  motor_cmd_1 = stick;
end C;
"#;
        let actuate: HashMap<String, String> = [
            ("motor_cmd_0".into(), "omega_cmd[1]".into()),
            ("motor_cmd_1".into(), "omega_cmd[2]".into()),
        ]
        .into_iter()
        .collect();
        let sense = HashMap::new();
        let source = synthesize(physics, "P", ctrl, "C", &actuate, &sense).unwrap();

        let mut session = Session::default();
        session
            .add_document("composed.mo", &source)
            .expect("composed source should parse");
        let result = session
            .compile_model(WRAPPER_MODEL_NAME)
            .expect("composed wrapper should compile");
        let input_names = result
            .dae
            .inputs
            .keys()
            .map(ToString::to_string)
            .collect::<Vec<_>>();

        assert_eq!(input_names, vec!["stick"]);
    }
}
