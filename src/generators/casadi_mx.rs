use crate::ast;

use tera::{Context, Tera};

static CASADI_SX_TEMPLATE: &str = r#"
import casadi as ca
{% for class in def.classes %}
class {{ class.name }}:

    def __init__(self):
        {% for comp in class.components -%}
        self.{{ comp.name }} = ca.MX.sym('{{ comp.name }}');
        {% endfor -%}
{% endfor %}
"#;

pub fn generate(def: &ast::StoredDefinition) {
    let mut tera = Tera::default();
    tera.add_raw_template("template", CASADI_SX_TEMPLATE)
        .unwrap();
    let mut context = Context::new();
    context.insert("def", def);
    println!("{}", tera.render("template", &context).unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generators::parse_file;

    #[test]
    fn test_generate_casadi_mx() {
        let def = parse_file("src/model.mo");
        generate(&def);
    }
}
