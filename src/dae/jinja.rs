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

pub fn render_template(def: Dae, template_file: &str) -> Result<()> {
    let template_txt = fs::read_to_string(template_file)
        .with_context(|| format!("Can't read file {}", template_file))?;

    let mut env = Environment::new();
    env.add_function("panic", panic);
    env.add_function("warn", warn);
    env.add_template("template", &template_txt)?;
    let tmpl = env.get_template("template")?;
    let txt = tmpl.render(context!(def => def)).unwrap();
    println!("{}", txt);
    Ok(())
}
