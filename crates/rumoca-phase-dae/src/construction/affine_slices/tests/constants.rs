//! Exact settled declaration values must not inherit lexical binder authority.

use super::*;
use rumoca_core::{ComponentRefPart, ComponentReference, DefId, EffectiveType, InstanceId, TypeId};

fn declaration(declaration: u32, instance: u32) -> (flat::Model, flat::Variable, Reference) {
    let path = ComponentReference::construct(
        false,
        span(),
        vec![ComponentRefPart {
            ident: "radius".into(),
            span: span(),
            subs: Vec::new(),
            def_id: DefId::new(declaration),
        }],
    )
    .unwrap();
    let integer_type = TypeId::new(9);
    let mut flat = flat::Model::default();
    flat.predefined_types.integer = integer_type;
    flat.effective_types.insert(
        integer_type,
        EffectiveType::new(integer_type, integer_type, Vec::new()).unwrap(),
    );
    let variable = flat::Variable {
        name: VarName::new("radius"),
        instance_id: InstanceId::new(instance),
        component_ref: Some(path.clone()),
        type_id: integer_type,
        variability: Variability::Constant(Default::default()),
        binding: Some(integer(3)),
        ..flat::Variable::empty_with_span(span())
    };
    let reference =
        Reference::with_component_reference("radius", path).with_instance_id(variable.instance_id);
    (flat, variable, reference)
}

fn named_window(reference: Reference) -> Expression {
    let radius = Expression::VarRef {
        name: reference,
        subscripts: Vec::new(),
        span: span(),
    };
    range(
        binary(OpBinary::Sub, super::reference("i"), radius.clone()),
        1,
        binary(OpBinary::Add, super::reference("i"), radius),
    )
}

#[test]
fn settled_integer_declarations_require_exact_occurrence_and_path_identity() {
    let (flat, variable, reference) = declaration(116, 4);
    for variability in [
        Variability::Constant(Default::default()),
        Variability::Parameter(Default::default()),
    ] {
        let mut variable = variable.clone();
        variable.variability = variability;
        let mut values = EvalContext::new();
        values.add_instance_parameter(variable.instance_id, "radius", EvalValue::Integer(3));
        let mut shapes = scope(1, 90);
        shapes.bind_slice_constant(&flat, &variable, &values);
        assert_eq!(
            plan(&named_window(reference.clone()), &shapes)
                .unwrap()
                .extent,
            7
        );
        let (_, _, wrong_instance) = declaration(116, 5);
        let (_, _, wrong_declaration) = declaration(117, 4);
        for wrong in [wrong_instance, wrong_declaration, Reference::new("radius")] {
            assert!(plan(&named_window(wrong), &shapes).is_none());
        }
    }
}

#[test]
fn settled_slice_values_refuse_name_defaults_real_runtime_array_and_unfixed_parameter() {
    let (mut flat, variable, reference) = declaration(116, 4);
    let mut values = EvalContext::new();
    values.add_parameter("radius", EvalValue::Integer(3));
    let mut shapes = scope(1, 90);
    shapes.bind_slice_constant(&flat, &variable, &values);
    assert!(plan(&named_window(reference.clone()), &shapes).is_none());
    values.add_instance_parameter(variable.instance_id, "radius", EvalValue::Integer(3));
    let mut runtime = variable.clone();
    runtime.variability = Variability::Empty;
    let mut array = variable.clone();
    array.dims = vec![1];
    let mut unfixed = variable.clone();
    unfixed.variability = Variability::Parameter(Default::default());
    unfixed.fixed = Some(vec![false]);
    for variable in [runtime, array, unfixed] {
        shapes.bind_slice_constant(&flat, &variable, &values);
        assert!(plan(&named_window(reference.clone()), &shapes).is_none());
    }
    flat.predefined_types.integer = TypeId::new(10);
    shapes.bind_slice_constant(&flat, &variable, &values);
    assert!(plan(&named_window(reference.clone()), &shapes).is_none());
    values.add_instance_parameter(variable.instance_id, "radius", EvalValue::Real(3.0));
    flat.predefined_types.integer = variable.type_id;
    shapes.bind_slice_constant(&flat, &variable, &values);
    assert!(plan(&named_window(reference), &shapes).is_none());
}

#[test]
fn settled_slice_values_clear_on_shadowing_and_keep_original_overflow_refusal() {
    let (flat, variable, reference) = declaration(116, 4);
    let mut values = EvalContext::new();
    values.add_instance_parameter(variable.instance_id, "radius", EvalValue::Integer(3));
    for clear in [0, 1, 2] {
        let mut shapes = scope(1, 90);
        shapes.bind_slice_constant(&flat, &variable, &values);
        match clear {
            0 => shapes.insert(variable.name.clone(), Vec::new()),
            1 => shapes.bind_scalar_value(variable.name.clone(), EvalValue::Integer(3)),
            _ => shapes.bind_integer_bounds(variable.name.clone(), 3, 3),
        }
        assert!(plan(&named_window(reference.clone()), &shapes).is_none());
    }
    let mut shapes = scope(i64::MAX, i64::MAX);
    shapes.bind_slice_constant(&flat, &variable, &values);
    assert!(plan(&named_window(reference), &shapes).is_none());
}
