//! This module provides functionality for working with the `Dae` structure,
//! which is part of the Abstract Syntax Tree (AST) representation in the
//! Differential-Algebraic Equation (DAE) system. The `Dae` structure is used
//! to model and manipulate DAE-related data within the application.
use crate::dae::ast::Dae;
use anyhow::{Context, Result};
use minijinja::{Environment, context};
use std::fs;

pub fn panic(msg: &str) {
    panic!("{:?}", msg);
}

pub fn warn(msg: &str) {
    eprintln!("{:?}", msg);
}

pub fn render_template(dae: &Dae, template_file: &str) -> Result<()> {
    let template_txt = fs::read_to_string(template_file)
        .with_context(|| format!("Can't read file {}", template_file))?;

    let mut env = Environment::new();
    env.add_function("panic", panic);
    env.add_function("warn", warn);
    env.add_template("template", &template_txt)?;
    let tmpl = env.get_template("template")?;
    let txt = tmpl.render(context!(dae => dae)).unwrap();
    println!("{}", txt);
    Ok(())
}

/// Render a template from a string directly (for WASM/editor use).
/// Returns the rendered output as a string.
pub fn render_template_str(dae: &Dae, template_str: &str) -> Result<String> {
    let mut env = Environment::new();
    env.add_function("panic", panic);
    env.add_function("warn", warn);
    env.add_template("template", template_str)?;
    let tmpl = env.get_template("template")?;
    let txt = tmpl
        .render(context!(dae => dae))
        .with_context(|| "Template rendering failed")?;
    Ok(txt)
}
