use crate::ast;

use tera::{Context, Tera};

pub fn generate(def: &ast::StoredDefinition) {
    let template = std::fs::read_to_string("src/generators/templates/casadi_sx.tera").
        expect("failed to read template");
    let mut tera = Tera::default();
    tera.add_raw_template("casadi_sx", &template).expect("failed to add template");

    let mut context = Context::new();
    context.insert("def", def);
    println!("{}", tera.render("casadi_sx", &context).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generators::parse_file;

    #[test]
    fn test_generate_casadi_sx() {
        let def = parse_file("src/model.mo").expect("failed to parse");
        generate(&def);
    }
}
