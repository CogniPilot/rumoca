//! Exact-identity regression coverage for modifier bindings that name a
//! component of the enclosing instance.
//!
//! `Leaf leaf[m](final offset = offset)` binds every element of `leaf` to the
//! enclosing component's `offset` array. The reference on the right-hand side
//! is written in the enclosing class scope, so flattening must spell it as the
//! enclosing *instance* member (`mid.offset`) — the flat variable that actually
//! holds the value after any outer modification. Recovering the outer value
//! from the declaration's own default binding is wrong the moment an outer
//! modification replaces it.
//!
//! MLS §7.2.4 makes that a scope fact, not only a spelling one: the modifier's
//! right-hand side is interpreted in the class body that *wrote* the modifier,
//! not in the body of the component being modified. `Probe probe(reading =
//! sum(cell.y))` names the `cell` array that `Rack` owns, so the occurrence
//! identity carried by that reference must be `Rack`'s body. Scoping it to
//! `Probe`'s body instead leaves a reference whose scope owns nothing it names,
//! and every occurrence-graph walk over it — member projection through the
//! component array, constant resolution — correctly refuses.

use rumoca_ir_ast as ast;

const SOURCE_NAME: &str = "<instance_scoped_modifier_bindings>";
const SOURCE: &str = r#"
model Leaf
    parameter Real offset = 0;
    parameter Real phase = 0;
    Real y;
equation
    y = offset + phase * time;
end Leaf;

model Mid
    parameter Integer m = 3;
    parameter Real offset[m] = zeros(m);
    parameter Real phase[m] = fill(0.5, m);
    Leaf leaf[m](final offset = offset, final phase = phase);
end Mid;

model Top
    Mid mid(offset = {10, 20, 30});
end Top;

model Siblings
    Mid first(offset = {10, 20, 30});
    Mid second(offset = {40, 50, 60});
end Siblings;

model Probe
    Real reading;
end Probe;

model Rack
    Leaf cell[2](phase = {1, 2});
    Probe probe(reading = sum(cell.y));
    parameter Real declaredOffsetSum = sum(cell.offset);
end Rack;

model Plant
    Rack rack;
end Plant;

model TwoRacks
    Rack left;
    Rack right;
end TwoRacks;
"#;

fn flatten_model(model_name: &str) -> rumoca_ir_flat::Model {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model_name).expect("model instantiates");
    rumoca_phase_flatten::flatten_ref(instanced.inner(), instanced.overlay(), model_name)
        .expect("model flattens")
}

fn flatten_source() -> rumoca_ir_flat::Model {
    flatten_model("Top")
}

fn binding_of(model: &rumoca_ir_flat::Model, name: &str) -> rumoca_core::Expression {
    model
        .variables
        .get(&rumoca_core::VarName::new(name))
        .unwrap_or_else(|| panic!("flat model owns {name}"))
        .binding
        .clone()
        .unwrap_or_else(|| panic!("{name} keeps its modifier binding"))
}

#[test]
fn array_component_modifier_binds_the_enclosing_instance_member() {
    let model = flatten_source();

    for index in 1..=3 {
        for member in ["offset", "phase"] {
            let binding = binding_of(&model, &format!("mid.leaf[{index}].{member}"));
            let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = &binding
            else {
                panic!(
                    "mid.leaf[{index}].{member} must stay a reference to the enclosing member, got {binding:?}"
                );
            };
            assert_eq!(
                name.as_str(),
                format!("mid.{member}"),
                "the modifier reference must name the enclosing instance member"
            );
            assert_eq!(
                subscripts.len(),
                1,
                "the element modifier keeps its element subscript"
            );
            assert!(
                name.parts().iter().all(|part| part.def_id.index() != 0),
                "every reference part carries an exact identity"
            );
        }
    }
}

#[test]
fn outer_modification_reaches_the_inner_array_elements() {
    let model = flatten_source();
    let outer = binding_of(&model, "mid.offset");
    let rumoca_core::Expression::Array { elements, .. } = &outer else {
        panic!("the outer modification supplies an array literal, got {outer:?}");
    };
    assert_eq!(
        elements.len(),
        3,
        "the outer modification supplies 3 values"
    );
}

#[test]
fn sibling_instances_of_one_class_each_bind_their_own_member() {
    // `first` and `second` instantiate the same `Mid`, so the modifier
    // reference inside `Mid` has one declaration identity for both. Only the
    // enclosing instance separates them, and each element must read the array
    // its own instance owns.
    let model = flatten_model("Siblings");

    for owner in ["first", "second"] {
        for index in 1..=3 {
            let binding = binding_of(&model, &format!("{owner}.leaf[{index}].offset"));
            let rumoca_core::Expression::VarRef { name, .. } = &binding else {
                panic!("{owner}.leaf[{index}].offset must stay a reference, got {binding:?}");
            };
            assert_eq!(
                name.as_str(),
                format!("{owner}.offset"),
                "each sibling reads the array its own instance owns"
            );
        }
    }
}

/// Elements of a `sum(<array of components>.<member>)` binding, in order.
///
/// The projection resolves only when the reference is scoped to the class body
/// that owns the component array, so an unexpanded `sum(cell.y)` is the exact
/// symptom this file guards against and is reported as such.
fn summed_projection_elements(
    model: &rumoca_ir_flat::Model,
    name: &str,
) -> Vec<rumoca_core::Reference> {
    let binding = binding_of(model, name);
    let rumoca_core::Expression::BuiltinCall { args, .. } = &binding else {
        panic!("{name} keeps its `sum(...)` modifier binding, got {binding:?}");
    };
    let [rumoca_core::Expression::Array { elements, .. }] = args.as_slice() else {
        panic!(
            "{name} must project the component array element-wise; \
             it is still unexpanded: {args:?}"
        );
    };
    elements
        .iter()
        .map(|element| {
            let rumoca_core::Expression::VarRef { name, .. } = element else {
                panic!("each projected element is a reference, got {element:?}");
            };
            name.clone()
        })
        .collect()
}

/// Assert that `references` name exactly the flat variables `expected`, by the
/// occurrence identity each of those variables carries — not by spelling.
fn assert_names_occurrences(
    model: &rumoca_ir_flat::Model,
    references: &[rumoca_core::Reference],
    expected: &[String],
) {
    let spelled: Vec<&str> = references
        .iter()
        .map(rumoca_core::Reference::as_str)
        .collect();
    assert_eq!(spelled, expected, "projection names the wrong variables");
    for (reference, name) in references.iter().zip(expected) {
        let variable = model
            .variables
            .get(&rumoca_core::VarName::new(name))
            .unwrap_or_else(|| panic!("flat model owns {name}"));
        assert_eq!(
            reference.instance_id(),
            Some(variable.instance_id),
            "{name} must be named by the occurrence the model allocated for it"
        );
    }
}

#[test]
fn modifier_binding_resolves_in_the_body_that_wrote_the_modifier() {
    // `Probe probe(reading = sum(cell.y))` is written in `Rack`'s body, so
    // `cell` is `Rack`'s component array. Scoped to `Probe`'s body instead,
    // the projection has nothing to walk and survives to the DAE phase as an
    // unresolved `rack.cell.y`.
    let model = flatten_model("Plant");

    let elements = summed_projection_elements(&model, "rack.probe.reading");
    assert_names_occurrences(
        &model,
        &elements,
        &["rack.cell[1].y".to_string(), "rack.cell[2].y".to_string()],
    );
}

#[test]
fn declaration_binding_still_resolves_in_the_declaring_body() {
    // The unchanged half of the contract: `parameter Real declaredOffsetSum =
    // sum(cell.offset)` is a declaration binding, whose references are written
    // in the body that declares the variable, so that body stays its scope.
    let model = flatten_model("Plant");

    let elements = summed_projection_elements(&model, "rack.declaredOffsetSum");
    assert_names_occurrences(
        &model,
        &elements,
        &[
            "rack.cell[1].offset".to_string(),
            "rack.cell[2].offset".to_string(),
        ],
    );
}

#[test]
fn sibling_racks_each_project_their_own_component_array() {
    // `left` and `right` instantiate one `Rack`, so the modifier's references
    // carry one declaration identity for both. Only the writing body's
    // occurrence separates them.
    let model = flatten_model("TwoRacks");

    for owner in ["left", "right"] {
        let elements = summed_projection_elements(&model, &format!("{owner}.probe.reading"));
        assert_names_occurrences(
            &model,
            &elements,
            &[format!("{owner}.cell[1].y"), format!("{owner}.cell[2].y")],
        );
    }
}
