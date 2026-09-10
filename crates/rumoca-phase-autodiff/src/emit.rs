//! Generated Modelica: the tangent function and the Jacobian wrapper.
//!
//! Synthesis emits plain Modelica text and the compiler parses it back, so the
//! expanded program the compiler sees and the program
//! `--emit-standard-modelica` writes are the same artifact rather than two
//! renderings that could drift. Every generated class carries a description
//! string naming the call site it was minted for.

use crate::engine::{TANGENT_FUNCTION_SUFFIX, TangentBuilder};
use crate::model::{Dimension, FunctionModel, Port, PortRole, ValueKind};
use crate::refusal::{Refusable, Refusal, Rule, Site};
use crate::scope::FunctionScope;

/// The output name of a generated Jacobian wrapper.
pub(crate) const JACOBIAN_OUTPUT: &str = "J_ad";

/// A synthesized Modelica function.
#[derive(Debug, Clone)]
pub(crate) struct Generated {
    /// Declared name of the generated function.
    pub(crate) name: String,
    /// Complete `function … end name;` text.
    pub(crate) text: String,
    /// Functions whose tangent this text calls.
    pub(crate) requested: Vec<String>,
}

/// The generated name of a function's tangent.
pub(crate) fn tangent_name(function: &str) -> String {
    format!("{function}{TANGENT_FUNCTION_SUFFIX}")
}

/// Escape a provenance string for embedding in a Modelica string literal.
///
/// The provenance carries the raw source text of the differentiated call, which
/// can contain a String literal argument whose quotes would otherwise close the
/// description string early and produce unparseable output. Modelica string
/// literals escape a backslash as `\\` and a double quote as `\"`; the backslash
/// pass runs first so an escaped quote is not doubly escaped.
fn escape_modelica_string(text: &str) -> String {
    text.replace('\\', "\\\\").replace('"', "\\\"")
}

/// The generated name of a Jacobian wrapper.
pub(crate) fn wrapper_name(function: &str, formal: &str) -> String {
    format!("{function}_jacobian_{formal}")
}

/// Emit the forward tangent function of `model`.
pub(crate) fn tangent_function(
    model: &FunctionModel,
    scope: &FunctionScope<'_>,
    site: &Site,
    provenance: &str,
) -> Refusable<Generated> {
    if model.statements.is_empty() {
        return Err(Refusal::new(
            Rule::Signature,
            site.clone(),
            format!("`{}` has no algorithm section", model.name),
        ));
    }
    let mut builder = TangentBuilder::new(model, scope, site.clone());
    // Declaration bindings run before the algorithm does, so their tangents
    // are stated in that same place and in that same order.
    let bound = builder.declaration_tangents(0)?;
    let body = builder.body(&model.statements, 0)?;
    let requested = builder.requested().to_vec();

    let name = tangent_name(&model.name);
    let mut lines = vec![format!(
        "function {name} \"{}\"",
        escape_modelica_string(provenance)
    )];
    for port in model.ports_with(PortRole::Input) {
        lines.push(format!("  input {};", port.primal_declaration()));
    }
    for port in differentiable(model, PortRole::Input) {
        lines.push(format!("  input {};", port.tangent_declaration()));
    }
    let outputs: Vec<&Port> = differentiable(model, PortRole::Output).collect();
    if outputs.is_empty() {
        return Err(Refusal::new(
            Rule::Signature,
            site.clone(),
            format!("`{}` has no Real output to differentiate", model.name),
        ));
    }
    for port in &outputs {
        lines.push(format!("  output {};", port.tangent_declaration()));
    }
    lines.push("protected".to_string());
    for port in model.ports_with(PortRole::Output) {
        lines.push(format!("  {};", port.primal_declaration()));
    }
    for port in model.ports_with(PortRole::Local) {
        lines.push(format!("  {};", port.primal_declaration()));
    }
    for port in differentiable(model, PortRole::Local) {
        lines.push(format!("  {};", port.tangent_declaration()));
    }
    lines.push("algorithm".to_string());
    lines.extend(bound);
    lines.extend(body);
    lines.push(format!("end {name};"));
    Ok(Generated {
        name,
        text: lines.join("\n"),
        requested,
    })
}

/// Emit the Jacobian wrapper of `model` with respect to the input `formal`.
pub(crate) fn jacobian_wrapper(
    model: &FunctionModel,
    formal: &Port,
    site: &Site,
    provenance: &str,
) -> Refusable<Generated> {
    let columns = seed_count(formal, site)?;
    let output = single_output(model, site)?;
    let rows = row_count(output, site)?;
    check_wrapper_names(model, site)?;

    let name = wrapper_name(&model.name, &formal.name);
    let mut lines = vec![format!(
        "function {name} \"{}\"",
        escape_modelica_string(provenance)
    )];
    for port in model.ports_with(PortRole::Input) {
        lines.push(format!("  input {};", port.primal_declaration()));
    }
    lines.push(format!(
        "  output Real {JACOBIAN_OUTPUT}[{rows}, {columns}];"
    ));
    lines.push("algorithm".to_string());
    let row_selector = if output.rank() == 0 { "1" } else { ":" };
    for column in 1..=columns {
        let arguments = wrapper_arguments(model, formal, column, columns);
        lines.push(format!(
            "  {JACOBIAN_OUTPUT}[{row_selector}, {column}] := {}({});",
            tangent_name(&model.name),
            arguments.join(", ")
        ));
    }
    lines.push(format!("end {name};"));
    Ok(Generated {
        name,
        text: lines.join("\n"),
        requested: vec![model.name.clone()],
    })
}

/// The primal arguments followed by the seeded tangent arguments.
fn wrapper_arguments(
    model: &FunctionModel,
    formal: &Port,
    column: usize,
    columns: usize,
) -> Vec<String> {
    let mut arguments: Vec<String> = model
        .ports_with(PortRole::Input)
        .map(|port| port.name.clone())
        .collect();
    for port in differentiable(model, PortRole::Input) {
        if port.name == formal.name {
            arguments.push(seed_text(port, column, columns));
        } else {
            arguments.push(format!("0.0*({})", port.name));
        }
    }
    arguments
}

/// The unit seed of column `column`, in the shape the formal declares.
fn seed_text(formal: &Port, column: usize, columns: usize) -> String {
    if formal.rank() == 0 {
        return "1.0".to_string();
    }
    let entries: Vec<&str> = (1..=columns)
        .map(|index| if index == column { "1.0" } else { "0.0" })
        .collect();
    format!("{{{}}}", entries.join(", "))
}

/// The number of Jacobian columns the differentiated input states.
fn seed_count(formal: &Port, site: &Site) -> Refusable<usize> {
    match formal.dims.as_slice() {
        [] => Ok(1),
        [Dimension::Stated(text)] => text.trim().parse::<usize>().map_err(|_| {
            Refusal::new(
                Rule::Signature,
                site.clone(),
                format!(
                    "`{}` is declared `[{text}]`; the differentiated input needs a literal \
                     integer dimension",
                    formal.name
                ),
            )
        }),
        _ => Err(Refusal::new(
            Rule::Signature,
            site.clone(),
            format!(
                "`{}` has rank {}; the differentiated input must be a scalar or a vector",
                formal.name,
                formal.rank()
            ),
        )),
    }
}

/// The number of Jacobian rows the output states.
fn row_count(output: &Port, site: &Site) -> Refusable<String> {
    match output.dims.as_slice() {
        [] => Ok("1".to_string()),
        [Dimension::Stated(text)] => Ok(text.clone()),
        _ => Err(Refusal::new(
            Rule::Signature,
            site.clone(),
            format!(
                "`{}` has an output shape the wrapper cannot state",
                output.name
            ),
        )),
    }
}

fn single_output<'a>(model: &'a FunctionModel, site: &Site) -> Refusable<&'a Port> {
    let outputs: Vec<&Port> = model.ports_with(PortRole::Output).collect();
    let [output] = outputs.as_slice() else {
        return Err(Refusal::new(
            Rule::Signature,
            site.clone(),
            format!(
                "`{}` has {} outputs; a Jacobian is taken of one",
                model.name,
                outputs.len()
            ),
        ));
    };
    if output.kind != ValueKind::Differentiable {
        return Err(Refusal::new(
            Rule::Signature,
            site.clone(),
            format!("`{}` does not return Real", model.name),
        ));
    }
    Ok(output)
}

/// The wrapper mints one name of its own, which must be free.
fn check_wrapper_names(model: &FunctionModel, site: &Site) -> Refusable<()> {
    if model.port(JACOBIAN_OUTPUT).is_some() {
        return Err(Refusal::new(
            Rule::NameCollision,
            site.clone(),
            format!("`{}` declares `{JACOBIAN_OUTPUT}`", model.name),
        ));
    }
    Ok(())
}

fn differentiable(model: &FunctionModel, role: PortRole) -> impl Iterator<Item = &Port> {
    model
        .ports_with(role)
        .filter(|port| port.kind == ValueKind::Differentiable)
}
