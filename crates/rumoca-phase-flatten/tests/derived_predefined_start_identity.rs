//! Regression coverage for unbound parameters of derived predefined types.
//!
//! A `start` attribute is an initialization guess, not the parameter's value.
//! Constant extraction must therefore retain references to an unbound derived
//! scalar/array parameter instead of fabricating a record constructor such as
//! `Frequency(start=50)`.

use rumoca_core::ExpressionVisitor;
use rumoca_ir_ast as ast;
use rumoca_ir_flat::{FunctionCallCollector, VarRefCollector};

const SOURCE_NAME: &str = "<derived_predefined_start_identity>";
const SOURCE: &str = r#"
package P
    type Frequency = Real(quantity="Frequency", unit="Hz");

    record Settings
        Real gain;
    end Settings;

    partial model ScalarBase
        parameter Frequency f(start=50);
        Real x;
    end ScalarBase;

    model Scalar
        extends ScalarBase(x(start=1/f));
    equation
        x = 1/f;
    end Scalar;

    partial model ArrayBase
        parameter Frequency f[2](start={50, 60});
        Real x[2];
    end ArrayBase;

    model ArrayValue
        extends ArrayBase(x(start={1/f[1], 1/f[2]}));
    equation
        x = {1/f[1], 1/f[2]};
    end ArrayValue;

    model RecordControl
        parameter Settings settings(gain=2);
        Real x(start=settings.gain);
    equation
        x = settings.gain;
    end RecordControl;
end P;
"#;

fn typed_flat_model(model_name: &str) -> (rumoca_ir_flat::Model, rumoca_core::SourceMap) {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model_name).expect("model instantiates");
    let ast::InstancedTree { tree, mut overlay } = instanced;
    rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, model_name)
        .expect("instanced model typechecks");
    let source_map = tree.source_map.clone();
    let flat = rumoca_phase_flatten::flatten_ref(&tree, &overlay, model_name)
        .expect("typed model flattens");
    (flat, source_map)
}

fn variable<'model>(
    model: &'model rumoca_ir_flat::Model,
    name: &str,
) -> &'model rumoca_ir_flat::Variable {
    model
        .variables
        .get(&rumoca_core::VarName::new(name))
        .unwrap_or_else(|| panic!("flat model owns `{name}`"))
}

fn assert_start_reads_only(
    model: &rumoca_ir_flat::Model,
    variable_name: &str,
    expected_reference: &str,
) {
    let start = variable(model, variable_name)
        .start
        .as_ref()
        .unwrap_or_else(|| panic!("`{variable_name}` retains its start expression"));

    let mut calls = FunctionCallCollector::new();
    calls.visit_expression(start);
    assert!(
        calls.names().is_empty(),
        "a derived predefined start must not become a constructor call: {:?}",
        calls.names()
    );

    let mut references = VarRefCollector::new();
    references.visit_expression(start);
    let spelled: Vec<_> = references.vars().iter().map(|name| name.as_str()).collect();
    assert_eq!(spelled, [expected_reference]);
}

fn assert_dae_constructs(flat: &rumoca_ir_flat::Model, source_map: rumoca_core::SourceMap) {
    rumoca_phase_dae::to_dae(flat, source_map)
        .expect("the symbolic derived-predefined reference lowers to DAE");
}

#[test]
fn scalar_start_does_not_supply_an_unbound_derived_parameter_value() {
    let (flat, source_map) = typed_flat_model("P.Scalar");

    assert_start_reads_only(&flat, "x", "f");
    assert_dae_constructs(&flat, source_map);
}

#[test]
fn array_shape_and_indexed_references_survive_without_scalar_identity_fallback() {
    let (flat, source_map) = typed_flat_model("P.ArrayValue");

    let frequency = variable(&flat, "f");
    assert_eq!(frequency.dims, [2]);
    assert_eq!(flat.effective_types[&frequency.type_id].dimensions(), [2]);
    assert_start_reads_only(&flat, "x", "f");
    assert_dae_constructs(&flat, source_map);
}

#[test]
fn resolved_record_modifications_remain_record_constant_values() {
    let (flat, source_map) = typed_flat_model("P.RecordControl");

    let start = variable(&flat, "x")
        .start
        .as_ref()
        .expect("record field start is retained");
    let mut calls = FunctionCallCollector::new();
    calls.visit_expression(start);
    assert!(
        calls.names().is_empty(),
        "record field projection is resolved before DAE: {:?}",
        calls.names()
    );
    assert_dae_constructs(&flat, source_map);
}
