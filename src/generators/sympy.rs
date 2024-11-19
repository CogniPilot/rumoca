use crate::ast;
use tera::{Context, Tera};

static SYMPY_TEMPLATE: &str = r#"
import sympy
{% for class in def.classes %}
class {{ class.name }}:

    def __init__(self):
        {% for comp in class.components -%}
        self.{{ comp.name }} = sympy.symbols('{{ comp.name }}');
        {% endfor -%}
{% endfor %}
"#;

pub fn generate(def: &ast::StoredDefinition) {
    let mut tera = Tera::default();

    tera.add_raw_template("sympy", SYMPY_TEMPLATE).unwrap();

    let mut context = Context::new();
    context.insert("def", def);
    println!("{}", tera.render("sympy", &context).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generators::parse_file;

    #[test]
    fn test_generate_sympy() {
        let def: ast::StoredDefinition = parse_file("src/model.mo").expect("failed to parse");
        generate(&def);
    }
}
