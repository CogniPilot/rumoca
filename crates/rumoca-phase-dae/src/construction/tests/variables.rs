use rumoca_core::{Reference, TypeId};

use super::super::*;
use super::support::*;

#[test]
fn production_range_lowering_retains_each_bound_occurrence() {
    let source = TestSource::new(
        "model M parameter Integer a[3] = 1:3; \
         parameter Integer b[3] = 4:1:6; end M;",
    );
    let omitted_at = source.span("1:3", 0);
    let explicit_at = source.span("4:1:6", 0);
    let omitted_bounds = (source.span("1", 0), source.span("3", 1));
    let explicit_bounds = (
        source.span("4", 0),
        source.span("1", 1),
        source.span("6", 0),
    );
    let mut model = test_model();
    add_range_parameter(
        &mut model,
        "a",
        TypeId::new(30),
        source.span("parameter Integer a[3]", 0),
        Expression::Range {
            start: Box::new(Expression::Literal {
                value: Literal::Integer(1),
                span: omitted_bounds.0,
            }),
            step: None,
            end: Box::new(Expression::Literal {
                value: Literal::Integer(3),
                span: omitted_bounds.1,
            }),
            span: omitted_at,
        },
    );
    add_range_parameter(
        &mut model,
        "b",
        TypeId::new(31),
        source.span("parameter Integer b[3]", 0),
        Expression::Range {
            start: Box::new(Expression::Literal {
                value: Literal::Integer(4),
                span: explicit_bounds.0,
            }),
            step: Some(Box::new(Expression::Literal {
                value: Literal::Integer(1),
                span: explicit_bounds.1,
            })),
            end: Box::new(Expression::Literal {
                value: Literal::Integer(6),
                span: explicit_bounds.2,
            }),
            span: explicit_at,
        },
    );

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let omitted = view
            .expression(
                view.variable(view.variable_id(0).unwrap())
                    .unwrap()
                    .binding()
                    .unwrap(),
            )
            .unwrap();
        let dae::ExpressionOperation::Range(omitted_range) = omitted.operation() else {
            unreachable!("parameter binding is a checked range")
        };
        assert_eq!(omitted.provenance().span(), omitted_at);
        assert_eq!(omitted_range.start().provenance().span(), omitted_bounds.0);
        assert_eq!(omitted_range.stop().provenance().span(), omitted_bounds.1);
        assert!(omitted_range.explicit_step().is_none());

        let explicit = view
            .expression(
                view.variable(view.variable_id(1).unwrap())
                    .unwrap()
                    .binding()
                    .unwrap(),
            )
            .unwrap();
        let dae::ExpressionOperation::Range(explicit_range) = explicit.operation() else {
            unreachable!("parameter binding is a checked range")
        };
        assert_eq!(explicit.provenance().span(), explicit_at);
        assert_eq!(
            explicit_range.start().provenance().span(),
            explicit_bounds.0
        );
        assert_eq!(
            explicit_range.explicit_step().unwrap().provenance().span(),
            explicit_bounds.1
        );
        assert_eq!(explicit_range.stop().provenance().span(), explicit_bounds.2);
    });
}

fn add_range_parameter(
    model: &mut flat::Model,
    name: &str,
    type_id: TypeId,
    declaration: Span,
    binding: Expression,
) {
    let mut variable = flat::Variable::empty_with_span(declaration);
    variable.name = VarName::new(name);
    variable.instance_id = test_instance_id(name);
    variable.type_id = type_id;
    variable.dims = vec![3];
    variable.variability = Variability::Parameter(Default::default());
    variable.is_primitive = true;
    variable.binding = Some(binding);
    register_test_integer_type(model, variable.type_id, &variable.dims);
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new(name), "Integer".to_string());
}

#[test]
fn input_ownership_requires_resolved_component_identity() {
    let source = TestSource::new("model M input Real u; end M;");
    let declaration = source.span("input Real u", 0);
    let mut model = test_model();
    let mut variable = flat::Variable::empty_with_span(declaration);
    variable.name = VarName::new("u");
    variable.instance_id = test_instance_id("u");
    variable.type_id = TypeId::new(8);
    variable.variability = Variability::Continuous(Default::default());
    variable.causality = Causality::Input(Default::default());
    variable.is_primitive = true;
    register_test_real_type(&mut model, variable.type_id, &variable.dims);
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new("u"), "Real".to_string());
    model.top_level_input_components.insert("u".to_string());

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "input ownership" && span == declaration
    ));
}

#[test]
fn zero_extent_parameter_binding_takes_its_element_type_from_the_declaration() {
    let source = TestSource::new("Real x; parameter Real p[0] = {}; equation x - 1.0;");
    let mut model = scalar_real_model(&source);
    add_empty_array_parameter(&mut model, &source, vec![0]);
    let dae = construct(&model, source.map).unwrap();

    dae.inspect(|view| {
        let parameter = view.variable(view.variable_id(1).unwrap()).unwrap();
        assert_eq!(parameter.role(), dae::VariableRole::Parameter);
        assert_eq!(parameter.scalar_count(), 0);
        let binding = parameter.binding().expect("empty binding is retained");
        let expression = view.expression(binding).expect("checked binding resolves");
        assert_eq!(expression.value_type().dimensions(), [0]);
        assert_eq!(
            expression.value_type().scalar_type(),
            dae::ScalarType::Real,
            "the declaration proves the element type of the empty literal"
        );
    });
}

#[test]
fn empty_array_binding_without_a_zero_extent_declaration_is_still_rejected() {
    let source = TestSource::new("Real x; parameter Real p[2] = {}; equation x - 1.0;");
    let literal = source.span("{}", 0);
    let mut model = scalar_real_model(&source);
    add_empty_array_parameter(&mut model, &source, vec![2]);

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "empty array" && span == literal
    ));
}

fn add_empty_array_parameter(model: &mut flat::Model, source: &TestSource, dims: Vec<i64>) {
    add_primitive_variable(model, source, "p", "parameter Real p", 14, dims, false);
    let variable = model.variables.get_mut(&VarName::new("p")).unwrap();
    variable.variability = Variability::Parameter(Default::default());
    variable.binding = Some(Expression::Array {
        elements: Vec::new(),
        is_matrix: false,
        span: source.span("{}", 0),
    });
}

#[test]
fn primitive_arrays_parameters_and_discrete_values_keep_checked_owners() {
    let source = TestSource::new(
        "Real x[2]; Real y; parameter Real p[2] = {1.0,2.0}; \
         discrete Boolean m = true; equation x = p; y = x[2];",
    );
    let model = array_and_discrete_model(&source);
    let dae = construct(&model, source.map).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.variable_count(), 4);
        assert_eq!(
            view.variable(view.variable_id(0).unwrap())
                .unwrap()
                .scalar_count(),
            2
        );
        let parameter = view.variable(view.variable_id(2).unwrap()).unwrap();
        assert_eq!(parameter.role(), dae::VariableRole::Parameter);
        assert!(parameter.binding().is_some());
        assert_eq!(
            view.variable(view.variable_id(3).unwrap()).unwrap().role(),
            dae::VariableRole::DiscreteValue
        );
        assert_eq!(view.discrete_value_definition_count(), 1);
        assert_eq!(view.continuous_owner_count(), 2);
        assert!(matches!(
            view.continuous_owner(0),
            Some(dae::ContinuousOwnerView::Structured { family, .. })
                if family.scalar_rows() == 2
        ));
        assert!(matches!(
            view.continuous_owner(1),
            Some(dae::ContinuousOwnerView::Residual { .. })
        ));
        let domain = view.domain(view.domain_id(0).unwrap()).unwrap();
        assert_eq!(
            domain.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::ArrayEquationProjection)
        );
    });
}

#[test]
fn variable_identity_pass_preserves_order_forward_and_function_attributes() {
    let source = TestSource::new(
        "parameter Real P=1; parameter Real A=B; parameter Real B=2; \
         parameter Real C=f(); function f output Real y; algorithm y := 4; end f;",
    );
    let mut model = test_model();
    for (name, declaration, type_id, binding) in [
        (
            "P",
            "parameter Real P=1",
            20,
            Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1", 0),
            },
        ),
        (
            "A",
            "parameter Real A=B",
            21,
            variable_reference(&source, "B", "B", 0, Vec::new()),
        ),
        (
            "B",
            "parameter Real B=2",
            22,
            Expression::Literal {
                value: Literal::Real(2.0),
                span: source.span("2", 0),
            },
        ),
        (
            "C",
            "parameter Real C=f()",
            23,
            Expression::FunctionCall {
                name: Reference::new("f"),
                args: Vec::new(),
                is_constructor: false,
                span: source.span("f()", 0),
            },
        ),
    ] {
        add_primitive_variable(
            &mut model,
            &source,
            name,
            declaration,
            type_id,
            Vec::new(),
            false,
        );
        let variable = model.variables.get_mut(&VarName::new(name)).unwrap();
        variable.variability = Variability::Parameter(Default::default());
        variable.binding = Some(binding);
    }

    let function_span = source.span("function f", 0);
    let output_span = source.span("output Real y", 0);
    let assignment_span = source.span("y := 4", 0);
    let mut function = rumoca_core::Function::new("f", function_span);
    function.add_output(real_function_param("y", Vec::new(), output_span));
    function.body = vec![rumoca_core::Statement::Assignment {
        comp: test_component_reference("y", assignment_span),
        value: Expression::Literal {
            value: Literal::Real(4.0),
            span: source.span("4", 0),
        },
        span: assignment_span,
    }];
    model.add_function(function);
    model.is_partial = true;

    let forward_use = source.span("B", 0);
    let function_call = source.span("f()", 0);
    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let names = view
            .variables()
            .map(|(_, variable)| variable.name().to_string())
            .collect::<Vec<_>>();
        assert_eq!(names, ["P", "A", "B", "C"]);

        let forward = view.variable(view.variable_id(1).unwrap()).unwrap();
        let forward_binding = view.expression(forward.binding().unwrap()).unwrap();
        assert_eq!(forward_binding.provenance().span(), forward_use);
        assert!(matches!(
            forward_binding.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id))
                if id.index() == 2
        ));

        let function_dependent = view.variable(view.variable_id(3).unwrap()).unwrap();
        let call = view
            .expression(function_dependent.binding().unwrap())
            .unwrap();
        assert_eq!(call.provenance().span(), function_call);
        assert!(matches!(
            call.operation(),
            dae::ExpressionOperation::Call { .. }
        ));
    });
}

#[test]
fn ordinary_forward_attribute_cycle_retains_both_use_occurrences() {
    let source = TestSource::new("parameter Real A(start=B); parameter Real B(start=A);");
    let mut model = test_model();
    for (name, declaration, type_id, dependency, occurrence) in [
        ("A", "parameter Real A(start=B)", 24, "B", 0),
        ("B", "parameter Real B(start=A)", 25, "A", 1),
    ] {
        add_primitive_variable(
            &mut model,
            &source,
            name,
            declaration,
            type_id,
            Vec::new(),
            false,
        );
        let variable = model.variables.get_mut(&VarName::new(name)).unwrap();
        variable.variability = Variability::Parameter(Default::default());
        variable.start = Some(variable_reference(
            &source,
            dependency,
            dependency,
            occurrence,
            Vec::new(),
        ));
    }
    model.is_partial = true;

    let a_start = source.span("B", 0);
    let b_start = source.span("A", 1);
    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let a = view.variable(view.variable_id(0).unwrap()).unwrap();
        let b = view.variable(view.variable_id(1).unwrap()).unwrap();
        assert_eq!(
            view.expression(a.start().unwrap())
                .unwrap()
                .provenance()
                .span(),
            a_start
        );
        assert_eq!(
            view.expression(b.start().unwrap())
                .unwrap()
                .provenance()
                .span(),
            b_start
        );
    });
}

fn array_and_discrete_model(source: &TestSource) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(&mut model, source, "x", "Real x[2]", 10, vec![2], false);
    add_primitive_variable(&mut model, source, "y", "Real y", 11, Vec::new(), false);
    add_parameter_with_array_binding(&mut model, source);
    add_discrete_boolean_with_binding(&mut model, source);
    let x = variable_reference(source, "x", "x = p", 0, Vec::new());
    let p = variable_reference(source, "p", "p", 1, Vec::new());
    model.add_equation(array_equation(
        source,
        "x = p",
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(x),
            rhs: Box::new(p),
            span: source.span("x = p", 0),
        },
        2,
    ));
    let y = variable_reference(source, "y", "y = x[2]", 0, Vec::new());
    let x_indexed = variable_reference(
        source,
        "x",
        "x[2]",
        0,
        vec![Subscript::Index {
            value: 2,
            span: source.span("2", 3),
        }],
    );
    model.add_equation(array_equation(
        source,
        "y = x[2]",
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(y),
            rhs: Box::new(x_indexed),
            span: source.span("y = x[2]", 0),
        },
        1,
    ));
    model
}

fn add_parameter_with_array_binding(model: &mut flat::Model, source: &TestSource) {
    add_primitive_variable(
        model,
        source,
        "p",
        "parameter Real p[2]",
        12,
        vec![2],
        false,
    );
    let variable = model.variables.get_mut(&VarName::new("p")).unwrap();
    variable.variability = Variability::Parameter(Default::default());
    variable.binding = Some(Expression::Array {
        elements: vec![
            Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            },
            Expression::Literal {
                value: Literal::Real(2.0),
                span: source.span("2.0", 0),
            },
        ],
        is_matrix: false,
        span: source.span("{1.0,2.0}", 0),
    });
}

fn add_discrete_boolean_with_binding(model: &mut flat::Model, source: &TestSource) {
    add_primitive_variable(
        model,
        source,
        "m",
        "discrete Boolean m",
        13,
        Vec::new(),
        true,
    );
    model.variables.get_mut(&VarName::new("m")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    });
}

fn array_equation(
    source: &TestSource,
    owner: &str,
    residual: Expression,
    scalar_count: usize,
) -> flat::Equation {
    flat::Equation::new_array(
        residual,
        source.span(owner, 0),
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
        scalar_count,
    )
}
