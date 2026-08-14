//! Exact-identity regression coverage for `arr.member` where `arr` is an array
//! of components.
//!
//! `cells.value` with `Cell cells[3]` denotes the array of the three elements'
//! `value` members (MLS §10.5). The flat model owns one variable per element
//! leaf and none named `cells.value`, so the projection has to be expanded into
//! the elements the model does own.
//!
//! Flatten expands it by walking the occurrence graph: the unindexed part
//! selects the array declaration, each element occurrence is its own node, and
//! the remaining parts are selected inside each element. Nothing here depends on
//! how a variable's path is rendered, and nothing depends on
//! `FlattenOptions::simplify_variable_names` — the expansion is a fact about the
//! model, not about how its names are printed.

use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<component_array_member_projection>";
const SOURCE: &str = r#"
model Cell
    Real value;
end Cell;

model Bank
    Cell cells[3];
    Real values[3];
    Real total;
equation
    values = cells.value;
    total = sum(cells.value);
    cells[1].value = 1.0;
    cells[2].value = 2.0;
    cells[3].value = 3.0;
end Bank;

model Nested
    Bank bank;
    Real echo;
equation
    echo = sum(bank.cells.value);
end Nested;

model Single
    Cell cells[1];
    Real values[1];
equation
    values = cells.value;
    cells[1].value = 1.0;
end Single;

model Grid
    Cell cells[2, 2];
    Real total;
equation
    total = sum(cells.value);
    cells[1, 1].value = 1.0;
    cells[1, 2].value = 2.0;
    cells[2, 1].value = 3.0;
    cells[2, 2].value = 4.0;
end Grid;
"#;

fn flatten_model_with(
    model_name: &str,
    options: rumoca_phase_flatten::FlattenOptions,
) -> rumoca_ir_flat::Model {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model_name).expect("model instantiates");
    rumoca_phase_flatten::flatten_ref_with_options(
        instanced.inner(),
        instanced.overlay(),
        model_name,
        options,
    )
    .expect("model flattens")
}

fn flatten_model(model_name: &str) -> rumoca_ir_flat::Model {
    flatten_model_with(model_name, rumoca_phase_flatten::FlattenOptions::default())
}

/// Every reference reachable from the model's equations, including the ones in
/// the structured families' comprehension templates.
///
/// The template is a peer copy of the scalar residual that downstream phases
/// read directly, so a projection left there is exactly as unresolvable as one
/// left in the residual.
fn equation_references(model: &rumoca_ir_flat::Model) -> Vec<String> {
    let mut names = Vec::new();
    for equation in model.equations.iter().chain(model.initial_equations.iter()) {
        collect_references(&equation.residual, &mut names);
    }
    for family in model
        .structured_equations
        .iter()
        .chain(model.initial_structured_equations.iter())
    {
        let Some(template) = family.template.as_ref() else {
            continue;
        };
        for body in &template.body {
            collect_references(body, &mut names);
        }
    }
    for variable in model.variables.values() {
        if let Some(binding) = &variable.binding {
            collect_references(binding, &mut names);
        }
    }
    names
}

fn collect_references(expr: &rumoca_core::Expression, names: &mut Vec<String>) {
    struct Collector<'a> {
        names: &'a mut Vec<String>,
    }
    impl rumoca_core::ExpressionRewriter for Collector<'_> {
        fn rewrite_var_ref_expression(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
            span: rumoca_core::Span,
        ) -> rumoca_core::Expression {
            self.names.push(name.as_str().to_string());
            rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            }
        }
    }
    use rumoca_core::ExpressionRewriter as _;
    Collector { names }.rewrite_expression(expr);
}

/// The array expression a projection was expanded into, by the element names it
/// reads in order.
fn projected_element_names(expr: &rumoca_core::Expression) -> Vec<String> {
    let rumoca_core::Expression::Array { elements, .. } = expr else {
        panic!("expected the projection to be an array expression, got {expr:?}");
    };
    elements
        .iter()
        .map(|element| match element {
            rumoca_core::Expression::VarRef { name, .. } => name.as_str().to_string(),
            other => panic!("expected a projected element reference, got {other:?}"),
        })
        .collect()
}

fn array_equation_rhs(model: &rumoca_ir_flat::Model, lhs_name: &str) -> rumoca_core::Expression {
    for equation in &model.equations {
        let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = &equation.residual else {
            continue;
        };
        if *op != rumoca_core::OpBinary::Sub {
            continue;
        }
        let rumoca_core::Expression::VarRef { name, .. } = lhs.as_ref() else {
            continue;
        };
        if name.as_str() == lhs_name {
            return rhs.as_ref().clone();
        }
    }
    panic!("flat model has no residual with {lhs_name} on the left");
}

#[test]
fn component_array_member_projection_expands_to_the_element_variables() {
    let model = flatten_model("Bank");

    assert_eq!(
        projected_element_names(&array_equation_rhs(&model, "values")),
        vec!["cells[1].value", "cells[2].value", "cells[3].value"],
        "the projection reads every element, in element order"
    );
}

#[test]
fn expansion_does_not_depend_on_the_name_shortening_option() {
    // Simplification is a rendering choice. Running it must not be what decides
    // whether the model's equations name variables that exist.
    let expanded = flatten_model_with(
        "Bank",
        rumoca_phase_flatten::FlattenOptions {
            simplify_variable_names: true,
            ..rumoca_phase_flatten::FlattenOptions::default()
        },
    );

    let owned: Vec<String> = expanded
        .variables
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    let elements = projected_element_names(&array_equation_rhs(&expanded, "values"));
    assert_eq!(
        elements.len(),
        3,
        "the projection stays expanded under shortened names"
    );
    for element in &elements {
        assert!(
            owned.contains(element),
            "projected element {element} must name a variable the model owns; owns {owned:?}"
        );
    }
}

#[test]
fn no_equation_reference_survives_as_an_unowned_projection() {
    for model_name in ["Bank", "Nested", "Single"] {
        let model = flatten_model(model_name);
        let owned: Vec<&str> = model.variables.keys().map(|name| name.as_str()).collect();
        for reference in equation_references(&model) {
            assert!(
                !reference.ends_with("cells.value"),
                "{model_name} left the unexpanded projection `{reference}` behind"
            );
            assert!(
                owned.contains(&reference.as_str()) || reference == "time",
                "{model_name} names `{reference}`, which is not a variable it owns"
            );
        }
    }
}

#[test]
fn projection_through_an_enclosing_component_expands() {
    // `bank.cells.value` has a leading part that is not the array, so the walk
    // has to reach the array declaration before it fans out.
    let model = flatten_model("Nested");
    let echo = array_equation_rhs(&model, "echo");
    let mut summed = Vec::new();
    collect_references(&echo, &mut summed);

    assert_eq!(
        summed,
        vec![
            "bank.cells[1].value",
            "bank.cells[2].value",
            "bank.cells[3].value"
        ],
        "the reduction over the nested projection reads every element, got {echo:?}"
    );
}

#[test]
fn single_element_component_array_projection_expands() {
    // A one-element array is the shape `Modelica.StateGraph.PartialCompositeStep`
    // uses (`suspend[nSuspend]` with `nSuspend = 1`). It is still an array, and
    // `suspend.reset` is still an array expression, not a scalar reference.
    let model = flatten_model("Single");

    assert_eq!(
        projected_element_names(&array_equation_rhs(&model, "values")),
        vec!["cells[1].value"],
    );
}

#[test]
fn multidimensional_component_array_projection_is_left_for_the_dae_phase() {
    // Accepted above: a one-dimensional component array, whose projection is a
    // vector this pass can build. `Cell cells[2,2]` projects to a 2x2 matrix,
    // which a flat element list would silently reshape into a 4-vector. Rather
    // than guess, flatten leaves the reference exactly as written so the DAE
    // phase rejects it against its own source span (ED008).
    let model = flatten_model("Grid");
    let references = equation_references(&model);

    assert!(
        references.iter().any(|name| name == "cells.value"),
        "the unsupported projection must survive verbatim, not be reshaped; read {references:?}"
    );
    assert!(
        !model
            .variables
            .keys()
            .any(|name| name.as_str() == "cells.value"),
        "and it must not have been invented as a variable"
    );
}
