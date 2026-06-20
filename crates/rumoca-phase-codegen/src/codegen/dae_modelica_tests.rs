use super::*;

fn cfg_template(body: &str) -> String {
    format!(
        r#"
{{%- set cfg = {{
  "power": "^",
  "and_op": "and",
  "or_op": "or",
  "not_op": "not ",
  "true_val": "true",
  "false_val": "false",
  "array_start": "{{",
  "array_end": "}}",
  "if_style": "modelica",
  "sanitize_dots": false,
  "one_based_index": true,
  "modelica_builtins": true
}} -%}}
{body}
"#
    )
}

fn index(value: i64) -> serde_json::Value {
    serde_json::json!({"Index": {"value": value}})
}

fn var_ref(name: &str, subscripts: Vec<serde_json::Value>) -> serde_json::Value {
    serde_json::json!({"VarRef": {"name": name, "subscripts": subscripts}})
}

fn der(expr: serde_json::Value) -> serde_json::Value {
    serde_json::json!({"BuiltinCall": {"function": "Der", "args": [expr]}})
}

fn real(value: f64) -> serde_json::Value {
    serde_json::json!({"Literal": {"value": {"Real": value}}})
}

fn residual(lhs: serde_json::Value, rhs: serde_json::Value) -> serde_json::Value {
    serde_json::json!({
        "rhs": {"Binary": {"op": "Sub", "lhs": lhs, "rhs": rhs}},
        "origin": "test"
    })
}

fn family(
    first_equation_index: usize,
    equation_counts: Vec<usize>,
    lower: i64,
    upper: i64,
) -> serde_json::Value {
    serde_json::json!({
        "domain": {
            "binders": [{
                "id": 0,
                "display_name": "i",
                "lower": lower,
                "upper": upper,
                "step": 1
            }]
        },
        "first_equation_index": first_equation_index,
        "equation_counts": equation_counts,
        "origin": "test"
    })
}

#[test]
fn dae_modelica_renders_structured_vector_equation_as_slice() {
    let dae_json = serde_json::json!({
        "f_x": [
            residual(der(var_ref("u", vec![index(1)])), var_ref("w", vec![index(1)])),
            residual(der(var_ref("u", vec![index(2)])), var_ref("w", vec![index(2)])),
            residual(der(var_ref("u", vec![index(3)])), var_ref("w", vec![index(3)]))
        ],
        "structured_equations": [family(0, vec![1, 1, 1], 1, 3)]
    });
    let template = cfg_template(r#"{{ render_dae_equations(dae, "f_x", cfg) }}"#);

    let rendered = render_template_with_dae_json(&dae_json, &template).unwrap();

    assert_eq!(rendered.trim(), "der(u[1:3]) = w[1:3];");
}

#[test]
fn dae_modelica_renders_structured_boundary_slice_with_literal_rhs() {
    let dae_json = serde_json::json!({
        "f_x": [
            residual(der(var_ref("w", vec![index(1), index(2)])), real(0.0)),
            residual(der(var_ref("w", vec![index(1), index(3)])), real(0.0))
        ],
        "structured_equations": [family(0, vec![1, 1], 2, 3)]
    });
    let template = cfg_template(r#"{{ render_dae_equations(dae, "f_x", cfg) }}"#);

    let rendered = render_template_with_dae_json(&dae_json, &template).unwrap();

    assert_eq!(rendered.trim(), "der(w[1, 2:3]) = {0.0, 0.0};");
}
