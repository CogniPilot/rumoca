//! Tests for Flat-owned invariants and analysis.

use super::*;
use rumoca_core::{ComponentRefPart, ProvenanceSpan, Token, extract_algorithm_outputs};

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("flat_ir_test.mo"),
        1,
        2,
    )
}

fn test_provenance() -> ProvenanceSpan {
    test_span()
        .require_provenance("Flat IR test")
        .expect("test span has provenance")
}

fn component_reference(name: &str, def_id: DefId, subs: Vec<Subscript>) -> ComponentReference {
    ComponentReference::construct(
        false,
        test_span(),
        vec![ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs,
            def_id,
        }],
    )
    .expect("test reference is nonempty and resolved")
}

#[test]
fn when_chain_is_nonempty_and_preserves_source_order_and_provenance() {
    let source = rumoca_core::SourceId::from_source_name("when_chain_ir_test.mo");
    let owner_span = Span::from_offsets(source, 1, 80);
    let first_span = Span::from_offsets(source, 6, 15);
    let second_span = Span::from_offsets(source, 35, 45);
    let first = WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: first_span,
        },
        first_span,
    );
    let second = WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(false),
            span: second_span,
        },
        second_span,
    );
    let mut chain = WhenChain::new(first, owner_span);
    chain.push_else_when(second);

    assert_eq!(chain.span(), owner_span);
    assert_eq!(chain.first().span, first_span);
    assert_eq!(chain.branch_count(), 2);
    assert_eq!(
        chain
            .branches()
            .map(|branch| branch.span)
            .collect::<Vec<_>>(),
        [first_span, second_span]
    );
}

#[test]
fn when_chain_deserialization_requires_its_first_branch() {
    let span = test_span();
    let chain = WhenChain::new(
        WhenBranch::new(
            Expression::Literal {
                value: Literal::Boolean(true),
                span,
            },
            span,
        ),
        span,
    );
    let mut value = serde_json::to_value(chain).expect("serialize nonempty when chain");
    assert!(
        value
            .as_object_mut()
            .expect("when chain serializes as a record")
            .remove("first")
            .is_some()
    );

    let error = serde_json::from_value::<WhenChain>(value)
        .expect_err("serialized when chain requires its mandatory first branch");
    assert!(error.to_string().contains("missing field `first`"));
}

#[test]
fn flat_variable_and_clock_constructors_preserve_owner_span() {
    assert_eq!(
        Variable::empty_with_span(test_span()).source_span,
        test_span()
    );
    assert_eq!(BaseClock::inferred(test_span()).source_span(), test_span());
    assert_eq!(
        BaseClock::periodic(0.1, test_span())
            .expect("positive periodic clock")
            .source_span(),
        test_span()
    );
    assert_eq!(
        SubClock::identity(test_provenance()).source_span(),
        test_span()
    );
    assert_eq!(
        SubClock::sub_sample(2, test_provenance())
            .expect("positive sub-sample factor")
            .source_span(),
        test_span()
    );
    assert_eq!(
        SubClock::super_sample(2, test_provenance())
            .expect("positive super-sample factor")
            .source_span(),
        test_span()
    );
}

fn make_parameter_var(name: &str, fixed: Option<bool>, has_binding: bool) -> Variable {
    Variable {
        name: VarName::new(name),
        variability: Variability::Parameter(Token::default()),
        fixed,
        binding: has_binding.then_some(Expression::Literal {
            value: Literal::Real(1.0),
            span: test_span(),
        }),
        ..Variable::empty_with_span(test_span())
    }
}

#[test]
fn unbound_fixed_parameters_detect_missing_bindings() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("p_missing"),
        make_parameter_var("p_missing", None, false),
    );
    flat.add_variable(
        VarName::new("p_bound"),
        make_parameter_var("p_bound", None, true),
    );
    flat.add_variable(
        VarName::new("p_not_fixed"),
        make_parameter_var("p_not_fixed", Some(false), false),
    );

    assert!(flat.has_unbound_fixed_parameters());
    assert_eq!(
        flat.unbound_fixed_parameters(),
        vec![VarName::new("p_missing")]
    );
}

#[test]
fn unbound_fixed_parameters_ignore_zero_sized_parameters() {
    let mut flat = Model::new();
    let mut empty = make_parameter_var("p_empty", None, false);
    empty.dims = vec![0, 3];
    flat.add_variable(VarName::new("p_empty"), empty);

    assert!(!flat.has_unbound_fixed_parameters());
    assert!(flat.unbound_fixed_parameters().is_empty());

    let mut nonempty = make_parameter_var("p_nonempty", None, false);
    nonempty.dims = vec![1];
    flat.add_variable(VarName::new("p_nonempty"), nonempty);
    assert!(flat.has_unbound_fixed_parameters());
    assert_eq!(
        flat.unbound_fixed_parameters(),
        vec![VarName::new("p_nonempty")]
    );
}

#[test]
fn algorithm_outputs_drop_assignment_subscripts() {
    let statements = vec![Statement::Assignment {
        comp: component_reference(
            "x",
            DefId::new(1),
            vec![Subscript::generated_index(1, test_span())],
        ),
        value: Expression::Literal {
            value: Literal::Real(1.0),
            span: test_span(),
        },
        span: test_span(),
    }];

    let outputs = extract_algorithm_outputs(&statements);
    assert_eq!(outputs.len(), 1);
    assert_eq!(outputs[0].as_str(), "x");
    assert!(outputs[0].has_structure());
}

#[test]
fn algorithm_outputs_keep_function_call_targets() {
    let statements = vec![Statement::FunctionCall {
        comp: component_reference("f", DefId::new(2), Vec::new()),
        args: Vec::new(),
        outputs: vec![Some(component_reference(
            "y",
            DefId::new(3),
            vec![Subscript::generated_index(2, test_span())],
        ))],
        span: test_span(),
    }];

    let outputs = extract_algorithm_outputs(&statements);
    assert_eq!(outputs.len(), 1);
    assert_eq!(outputs[0].as_str(), "y");
    assert!(outputs[0].has_structure());
}
