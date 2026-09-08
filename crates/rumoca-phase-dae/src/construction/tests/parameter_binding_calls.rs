//! Parameter bindings that call functions at translation time.
//!
//! `constant_context` folds every settled `parameter` binding before shape
//! analysis, so the evaluator decides which bindings this phase may settle. A
//! binding it refuses for a reason that proves nothing wrong with the model
//! stops a well-formed model here; a callee it must never execute has to stay
//! refused by that callee's own owner afterwards, never folded into a value.

use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, Reference, ResolvedFunctionReference, TypeId,
};

use super::super::*;
use super::support::*;

/// A read of an iterator as Flat delivers it from a resolved source: the
/// iterator's declaration identity is issued to the reads alone, so the
/// reference carries a root `DefId` that selects no function declaration and
/// no occurrence identity. `Modelica.Electrical.Polyphase.Functions.symmetricOrientation`
/// reads `k` in `orientation := {(k - 1)*2*pi/m for k in 1:m}` exactly so.
fn resolved_iterator_read(name: &str, declaration: DefId, span: Span) -> Expression {
    Expression::VarRef {
        name: Reference::with_component_reference(
            name,
            ComponentReference::construct(
                false,
                span,
                vec![ComponentRefPart {
                    ident: name.to_owned(),
                    span,
                    subs: Vec::new(),
                    def_id: declaration,
                }],
            )
            .expect("the iterator read has an exact resolved identity"),
        ),
        subscripts: Vec::new(),
        span,
    }
}

fn resolved_call(
    model: &flat::Model,
    function: &str,
    args: Vec<Expression>,
    span: Span,
) -> Expression {
    let instance = model.functions[&VarName::new(function)]
        .instance_id
        .expect("Flat assigns the selected function an exact instance");
    Expression::FunctionCall {
        name: Reference::new(function).with_resolved_function(ResolvedFunctionReference {
            instance_id: instance,
            base_part_count: 1,
            transitively_non_replaceable: true,
        }),
        args,
        is_constructor: false,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    }
}

#[test]
fn a_parameter_binding_folds_an_iterator_read_carrying_its_resolved_identity() {
    let source = TestSource::new(
        "function f output Real y[3]; algorithm y := {(k - 1)*2.0 for k in 1:3}; end f; \
         parameter Real phase[3] = f();",
    );
    let comprehension_span = source.span("{(k - 1)*2.0 for k in 1:3}", 0);
    let assignment_span = source.span("y := {(k - 1)*2.0 for k in 1:3}", 0);
    let mut function =
        rumoca_core::Function::new("f", DefId::new(63_130), source.span("function f", 0));
    function.def_id = Some(DefId::new(63_130));
    function.pure = true;
    function.transitively_non_replaceable = true;
    function.add_output(real_function_param(
        "y",
        vec![3],
        source.span("output Real y[3]", 0),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: test_component_reference("y", assignment_span),
        value: Expression::ArrayComprehension {
            expr: Box::new(Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(Expression::Binary {
                    op: OpBinary::Sub,
                    lhs: Box::new(resolved_iterator_read(
                        "k",
                        DefId::new(63_131),
                        source.span("k", 1),
                    )),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: source.span("1", 0),
                    }),
                    span: source.span("(k - 1)", 0),
                }),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Real(2.0),
                    span: source.span("2.0", 0),
                }),
                span: source.span("(k - 1)*2.0", 0),
            }),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "k".to_owned(),
                range: Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: source.span("1", 1),
                    }),
                    step: None,
                    end: Box::new(Expression::Literal {
                        value: Literal::Integer(3),
                        span: source.span("3", 0),
                    }),
                    span: source.span("1:3", 0),
                },
            }],
            filter: None,
            span: comprehension_span,
        },
        span: assignment_span,
    });

    let mut model = test_model();
    model.add_function(function);
    add_primitive_variable(
        &mut model,
        &source,
        "phase",
        "parameter Real phase[3]",
        907,
        vec![3],
        false,
    );
    let call_span = source.span("f()", 0);
    let binding = resolved_call(&model, "f", Vec::new(), call_span);
    let phase = model.variables.get_mut(&VarName::new("phase")).unwrap();
    phase.variability = Variability::Parameter(Default::default());
    phase.binding = Some(binding);

    let dae = construct(&model, source.map)
        .expect("the iterator read is bound by its comprehension index and the binding folds");
    dae.dae().inspect(|view| {
        let phase = view
            .variables()
            .map(|(_, variable)| variable)
            .find(|variable| variable.name().as_str() == "phase")
            .expect("the folded parameter keeps its DAE owner");
        assert!(matches!(
            phase.identity(),
            dae::VariableIdentity::Parameter(_)
        ));
        let binding = view
            .expression(phase.binding().expect("the binding is retained"))
            .unwrap();
        assert_eq!(binding.value_type().dimensions(), [3]);
    });
}

fn external_object_constructor(source: &TestSource) -> rumoca_core::Function {
    let function_span = source.span("function Handle", 0);
    let mut constructor = rumoca_core::Function::new("Handle", DefId::new(63_132), function_span);
    constructor.def_id = Some(DefId::new(63_132));
    constructor.transitively_non_replaceable = true;
    constructor.add_input(real_function_param(
        "seed",
        Vec::new(),
        source.span("input Real seed", 0),
    ));
    constructor.add_output(function_param(
        "handle",
        "Handle",
        TypeId::new(900),
        TypeId::new(900),
        Vec::new(),
        source.span("output Handle handle", 0),
    ));
    constructor.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("make_handle".to_string()),
        output_name: Some("handle".to_string()),
        args: vec![Expression::VarRef {
            name: Reference::new("seed"),
            subscripts: Vec::new(),
            span: source.span("seed", 1),
        }],
        annotations: Vec::new(),
    });
    constructor
}

#[test]
fn an_external_object_constructor_in_a_parameter_binding_is_still_refused() {
    let source = TestSource::new(
        "function Handle input Real seed; output Handle handle; \
         external \"C\" handle = make_handle(seed); end Handle; \
         parameter Handle table = Handle(1.0);",
    );
    let output_span = source.span("output Handle handle", 0);
    let mut model = test_model();
    model.add_function(external_object_constructor(&source));
    let declaration = source.span("parameter Handle table", 0);
    let mut table = flat::Variable::empty_with_span(declaration);
    table.name = VarName::new("table");
    table.instance_id = test_instance_id("table");
    table.component_ref = Some(test_component_reference("table", declaration));
    table.type_id = TypeId::new(900);
    table.variability = Variability::Parameter(Default::default());
    table.is_primitive = true;
    register_test_effective_type(&mut model, table.type_id, table.type_id, &table.dims);
    let call_span = source.span("Handle(1.0)", 0);
    table.binding = Some(resolved_call(
        &model,
        "Handle",
        vec![Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        }],
        call_span,
    ));
    model.add_variable(table.name.clone(), table);
    model
        .variable_type_names
        .insert(VarName::new("table"), "Handle".to_string());

    // The evaluator never executes an external interface, so the binding is
    // left unsettled and the constructor reaches the owner that names what an
    // `ExternalObject` lacks: a checked DAE value type.
    let error = construct(&model, source.map)
        .expect_err("an ExternalObject constructor cannot fold to a parameter value");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "function value type"
            && detail == "`Handle.handle` has unsupported type `Handle`"
            && span == output_span
    ));
}

#[test]
fn an_impure_function_in_a_parameter_binding_is_never_folded() {
    let source = TestSource::new(
        "impure function f input Real p0; output Real y0; \
         external \"C\" y0 = my_random(p0); end f; \
         parameter Real seed = f(2.5);",
    );
    let mut function =
        rumoca_core::Function::new("f", DefId::new(63_133), source.span("function f", 0));
    function.def_id = Some(DefId::new(63_133));
    function.pure = false;
    function.purity_declared = true;
    function.transitively_non_replaceable = true;
    function.add_input(real_function_param(
        "p0",
        Vec::new(),
        source.span("input Real p0", 0),
    ));
    function.add_output(real_function_param(
        "y0",
        Vec::new(),
        source.span("output Real y0", 0),
    ));
    let annotation_span = source.span("my_random", 0);
    function.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("my_random".to_string()),
        output_name: Some("y0".to_string()),
        args: vec![Expression::VarRef {
            name: Reference::new("p0"),
            subscripts: Vec::new(),
            span: source.span("p0", 1),
        }],
        annotations: vec![rumoca_core::ExternalFunctionAnnotation {
            name: vec!["Library".to_string()],
            value: Expression::Literal {
                value: Literal::String("ModelicaExternalC".to_string()),
                span: annotation_span,
            },
            span: annotation_span,
        }],
    });

    let mut model = test_model();
    model.add_function(function);
    add_primitive_variable(
        &mut model,
        &source,
        "seed",
        "parameter Real seed",
        908,
        Vec::new(),
        false,
    );
    let call_span = source.span("f(2.5)", 0);
    let binding = resolved_call(
        &model,
        "f",
        vec![Expression::Literal {
            value: Literal::Real(2.5),
            span: source.span("2.5", 0),
        }],
        call_span,
    );
    let seed = model.variables.get_mut(&VarName::new("seed")).unwrap();
    seed.variability = Variability::Parameter(Default::default());
    seed.binding = Some(binding);

    // MLS §12.3 admits an impure call in a parameter binding, so the model is
    // constructible; the evaluator refuses to execute the impure interface,
    // and the binding therefore stays the call for initialization to make.
    let dae = construct(&model, source.map)
        .expect("an impure parameter binding is deferred to initialization, not folded");
    dae.dae().inspect(|view| {
        let seed = view
            .variables()
            .map(|(_, variable)| variable)
            .find(|variable| variable.name().as_str() == "seed")
            .expect("the deferred parameter keeps its DAE owner");
        let binding = view
            .expression(seed.binding().expect("the binding is retained"))
            .unwrap();
        assert!(
            matches!(binding.operation(), dae::ExpressionOperation::Call { .. }),
            "the impure call must not fold to a value"
        );
    });
}
