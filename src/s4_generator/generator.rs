use crate::s2_analyzer::ast;
use minijinja::{context, Environment};
use ordermap::OrderMap;

pub fn panic(msg: &str) {
    panic!("{:?}", msg);
}

pub fn generate(
    classes: &OrderMap<String, ast::Class>,
    template_file: &str,
) -> Result<String, Box<dyn std::error::Error>> {
    let template_text = std::fs::read_to_string(template_file)?;
    let mut env = Environment::new();
    env.add_function("panic", panic);
    env.add_template("template", &template_text)?;
    let tmpl = env.get_template("template")?;
    let txt = tmpl.render(context!(classes => classes)).unwrap();
    Ok(txt)
}
