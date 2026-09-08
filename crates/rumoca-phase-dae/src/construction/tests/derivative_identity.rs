//! Proof that Flat-to-DAE derivative ownership follows occurrence identity.
//!
//! Both coordinates below instantiate the same array component and leaf
//! declarations. Their complete `DefId` path is therefore deliberately equal;
//! semantic component subscripts and occurrence identities distinguish the two
//! runtime states.

use rumoca_core::{ComponentRefPart, ComponentReference, DefId, InstanceId, Reference};

use super::super::*;
use super::support::*;

const LEFT_INSTANCE: InstanceId = InstanceId(70_001);
const RIGHT_INSTANCE: InstanceId = InstanceId(70_002);
const SHARED_BLOCKS: DefId = DefId(71_001);
const SHARED_X: DefId = DefId(71_003);

#[test]
fn same_declaration_derivatives_lower_to_their_exact_state_occurrences() {
    let (model, source, _) = nested_derivative_model();
    let dae = construct(&model, source.map).expect("the two exact state occurrences are admitted");

    dae.dae().inspect(|view| {
        let states = view
            .variables()
            .map(|(_, variable)| {
                let dae::VariableIdentity::State(state) = variable.identity() else {
                    panic!("every fixture variable is a state");
                };
                (variable.source_occurrence().instance_id(), state)
            })
            .collect::<HashMap<_, _>>();
        assert_eq!(states.len(), 2);

        for (row, occurrence) in [(0, LEFT_INSTANCE), (1, RIGHT_INSTANCE)] {
            let residual = view
                .continuous_equation(row)
                .expect("fixture equation exists")
                .residual();
            let dae::ExpressionOperation::Binary { lhs, .. } = view
                .expression(residual)
                .expect("fixture residual exists")
                .operation()
            else {
                panic!("fixture equation is a subtraction residual");
            };
            let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(found)) = view
                .expression(lhs)
                .expect("derivative operand exists")
                .operation()
            else {
                panic!("left residual operand is a state derivative");
            };
            assert_eq!(found, states[&occurrence]);
        }
    });
}

#[test]
fn changing_only_the_derivative_instance_identity_is_rejected() {
    let (mut model, source, first_derivative_span) = nested_derivative_model();
    let name = first_derivative_reference_mut(&mut model);
    *name = name.clone().with_instance_id(RIGHT_INSTANCE);

    let error = construct(&model, source.map)
        .expect_err("an occurrence identity cannot contradict its cached name and component path");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "derivative target identity"
            && detail == "the operand's cached name or component reference contradicts its occurrence identity"
            && span == first_derivative_span
    ));
}

#[test]
fn a_derivative_without_an_occurrence_identity_is_rejected_at_analysis() {
    let (mut model, source, first_derivative_span) = nested_derivative_model();
    let name = first_derivative_reference_mut(&mut model);
    *name = Reference::with_component_reference(
        "blocks[1].x",
        component_reference("blocks[1].x", 1, first_derivative_span),
    );

    assert_identity_error(
        construct(&model, source.map)
            .expect_err("a derivative operand cannot omit its Flat occurrence identity"),
        "the operand has no allocated Flat occurrence identity",
        first_derivative_span,
    );
}

#[test]
fn a_derivative_with_a_foreign_occurrence_identity_is_rejected_at_analysis() {
    let (mut model, source, first_derivative_span) = nested_derivative_model();
    let name = first_derivative_reference_mut(&mut model);
    *name = name.clone().with_instance_id(InstanceId(79_999));

    assert_identity_error(
        construct(&model, source.map)
            .expect_err("a derivative operand cannot name an occurrence outside the Flat root"),
        "the operand occurrence is not present in this Flat model",
        first_derivative_span,
    );
}

#[test]
fn a_non_state_derivative_is_rejected_before_dae_construction() {
    let (mut model, source, first_derivative_span) = nested_derivative_model();
    let variable = model
        .variables
        .get_mut(&VarName::new("blocks[1].x"))
        .expect("the fixture owns the first occurrence");
    let Variability::Continuous(token) = &variable.variability else {
        panic!("the fixture starts with a continuous coordinate");
    };
    variable.variability = Variability::Parameter(token.clone());

    let error = construct(&model, source.map)
        .expect_err("der(parameter) must fail at derivative-role admission");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "derivative expression"
            && detail == "der(...) must name a primitive continuous Real state coordinate"
            && span == first_derivative_span
    ));
}

#[test]
fn derivative_identity_is_checked_in_assertion_owned_expressions() {
    let (mut model, source, _) = nested_derivative_model();
    let assertion_span = source.span("der(blocks[2].x)", 0);
    let Expression::Binary { lhs, .. } = &model.equations[1].residual else {
        panic!("fixture equation is a subtraction residual");
    };
    let mut forged = lhs.as_ref().clone();
    let Expression::BuiltinCall { args, .. } = &mut forged else {
        panic!("fixture residual starts with der(...)");
    };
    let Expression::VarRef { name, .. } = &mut args[0] else {
        panic!("fixture derivative has one reference operand");
    };
    *name = name.clone().with_instance_id(LEFT_INSTANCE);
    model.assert_equations.push(flat::AssertEquation::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: assertion_span,
        },
        forged,
        None,
        assertion_span,
        flat::EquationOrigin::ComponentEquation {
            component: "blocks[2]".to_owned(),
        },
    ));

    assert_identity_error(
        construct(&model, source.map)
            .expect_err("assertion-owned derivatives use the complete owner traversal"),
        "the operand's cached name or component reference contradicts its occurrence identity",
        assertion_span,
    );
}

#[test]
fn derivative_identity_is_checked_in_every_variable_expression_attribute() {
    type SetAttribute = fn(&mut flat::Variable, Expression);
    for set_attribute in [
        (|variable: &mut flat::Variable, value| variable.start = Some(value)) as SetAttribute,
        |variable: &mut flat::Variable, value| variable.min = Some(value),
        |variable: &mut flat::Variable, value| variable.max = Some(value),
        |variable: &mut flat::Variable, value| variable.nominal = Some(value),
        |variable: &mut flat::Variable, value| variable.binding = Some(value),
    ] {
        let (mut model, source, _) = nested_derivative_model();
        let attribute_span = source.span("der(blocks[2].x)", 0);
        let Expression::Binary { lhs, .. } = &model.equations[1].residual else {
            panic!("fixture equation is a subtraction residual");
        };
        let mut forged = lhs.as_ref().clone();
        let Expression::BuiltinCall { args, .. } = &mut forged else {
            panic!("fixture residual starts with der(...)");
        };
        let Expression::VarRef { name, .. } = &mut args[0] else {
            panic!("fixture derivative has one reference operand");
        };
        *name = name.clone().with_instance_id(LEFT_INSTANCE);
        let variable = model
            .variables
            .get_mut(&VarName::new("blocks[1].x"))
            .expect("the fixture owns the first occurrence");
        set_attribute(variable, forged);

        assert_identity_error(
            construct(&model, source.map)
                .expect_err("every expression-valued variable attribute is a derivative owner"),
            "the operand's cached name or component reference contradicts its occurrence identity",
            attribute_span,
        );
    }
}

#[test]
fn a_derivative_in_a_function_owner_is_rejected_before_function_lowering() {
    let source = TestSource::new("function f input Real u = der(1.0); end f;");
    let derivative_span = source.span("der(1.0)", 0);
    let mut model = test_model();
    let mut input = real_function_param("u", Vec::new(), source.span("Real u", 0));
    input.default = Some(Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        }],
        span: derivative_span,
    });
    let mut function =
        rumoca_core::Function::new("f", DefId::new(71_100), source.span("function f", 0));
    function.add_input(input);
    model.add_function(function);

    let error = construct(&model, source.map)
        .expect_err("MLS §12.2 prohibits der in every function-owned expression");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedRuntimeOperator {
            operator,
            detail,
            span,
        } if operator == "der"
            && detail == "der is prohibited in a function semantic owner by SPEC_0022 FUNC-010"
            && span == derivative_span
    ));
}

fn first_derivative_reference_mut(model: &mut flat::Model) -> &mut Reference {
    let Expression::Binary { lhs, .. } = &mut model.equations[0].residual else {
        panic!("fixture equation is a subtraction residual");
    };
    let Expression::BuiltinCall { args, .. } = lhs.as_mut() else {
        panic!("fixture residual starts with der(...)");
    };
    let Expression::VarRef { name, .. } = &mut args[0] else {
        panic!("fixture derivative has one reference operand");
    };
    name
}

fn assert_identity_error(error: ToDaeError, detail_expected: &str, span_expected: Span) {
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "derivative target identity"
            && detail == detail_expected
            && span == span_expected
    ));
}

fn nested_derivative_model() -> (flat::Model, TestSource, Span) {
    let source = TestSource::new(
        "model Leaf Real x; end Leaf; model M Leaf blocks[2]; equation \
         der(blocks[1].x)=1.0; der(blocks[2].x)=2.0; end M;",
    );
    let mut model = test_model();

    for (name, instance, index) in [
        ("blocks[1].x", LEFT_INSTANCE, 1),
        ("blocks[2].x", RIGHT_INSTANCE, 2),
    ] {
        let declaration = source.span("Real x", 0);
        let reference_span = source.span(name, 0);
        let component = component_reference(name, index, reference_span);
        add_primitive_variable(
            &mut model,
            &source,
            name,
            "Real x",
            72_001,
            Vec::new(),
            false,
        );
        let variable = model
            .variables
            .get_mut(&VarName::new(name))
            .expect("the fixture helper inserts the variable");
        variable.source_span = declaration;
        variable.instance_id = instance;
        variable.component_ref = Some(component);
    }

    for (name, instance, index, literal) in [
        ("blocks[1].x", LEFT_INSTANCE, 1, 1.0),
        ("blocks[2].x", RIGHT_INSTANCE, 2, 2.0),
    ] {
        let call_text = format!("der({name})");
        let call_span = source.span(&call_text, 0);
        let reference_span = source.span(name, 0);
        let equation_text = format!("{call_text}={literal:.1}");
        let equation_span = source.span(&equation_text, 0);
        model.add_equation(flat::Equation::new(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args: vec![Expression::VarRef {
                        name: Reference::with_component_reference(
                            name,
                            component_reference(name, index, reference_span),
                        )
                        .with_instance_id(instance),
                        subscripts: Vec::new(),
                        span: reference_span,
                    }],
                    span: call_span,
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Real(literal),
                    span: source.span(&format!("{literal:.1}"), 0),
                }),
                span: equation_span,
            },
            equation_span,
            flat::EquationOrigin::ComponentEquation {
                component: name.split_once('.').expect("nested name").0.to_owned(),
            },
        ));
    }

    let first_derivative_span = source.span("der(blocks[1].x)", 0);
    (model, source, first_derivative_span)
}

fn component_reference(name: &str, index: i64, span: Span) -> ComponentReference {
    let (owner, leaf) = name.split_once('.').expect("fixture name is nested");
    let owner = owner.split_once('[').expect("fixture owner is indexed").0;
    ComponentReference::construct(
        false,
        span,
        vec![
            ComponentRefPart {
                ident: owner.to_owned(),
                span,
                subs: vec![Subscript::index(index, span)],
                def_id: SHARED_BLOCKS,
            },
            ComponentRefPart {
                ident: leaf.to_owned(),
                span,
                subs: Vec::new(),
                def_id: SHARED_X,
            },
        ],
    )
    .expect("fixture component reference has exact declaration identities")
}
