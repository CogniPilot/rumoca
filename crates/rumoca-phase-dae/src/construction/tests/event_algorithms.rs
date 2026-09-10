use rumoca_core::{Reference, TypeId};

use super::super::*;
use super::support::*;

/// MLS §11.1: the statements of a model algorithm section that are not inside a
/// `when` run whenever the section runs. A discrete assignment written there is
/// therefore not a statement without an activation — its activation is the
/// section's own unconditional one, and the transaction step it produces must
/// carry that `Always` condition rather than borrowing an enclosing branch that
/// does not exist.
/// Declare one `Real` whose variability is discrete, so it lands on a
/// `DiscreteReal` coordinate rather than the B.1c discrete-value arena.
fn add_discrete_real_variable(
    model: &mut flat::Model,
    source: &TestSource,
    name: &str,
    declaration: &str,
    type_id: u32,
) {
    let mut variable = flat::Variable::empty_with_span(source.span(declaration, 0));
    variable.name = VarName::new(name);
    variable.instance_id = test_instance_id(name);
    variable.type_id = TypeId::new(type_id);
    variable.variability = Variability::Discrete(Default::default());
    variable.is_primitive = true;
    register_test_real_type(model, variable.type_id, &[]);
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new(name), "Real".to_string());
}

#[test]
fn model_algorithm_assignment_outside_when_activates_unconditionally() {
    let source = TestSource::new(
        "model M discrete Real y; algorithm y := 0.0; when true then y := 1.0; end when; end M;",
    );
    let mut model = test_model();
    add_discrete_real_variable(&mut model, &source, "y", "discrete Real y", 63);
    let unguarded_span = source.span("y := 0.0", 0);
    let guarded_span = source.span("y := 1.0", 0);
    let when_span = source.span("when true then y := 1.0; end when", 0);
    model.algorithms.push(flat::Algorithm::new(
        vec![
            rumoca_core::Statement::Assignment {
                comp: test_component_reference("y", unguarded_span),
                value: Expression::Literal {
                    value: Literal::Real(0.0),
                    span: source.span("0.0", 0),
                },
                span: unguarded_span,
            },
            rumoca_core::Statement::When {
                blocks: vec![rumoca_core::StatementBlock {
                    cond: Expression::Literal {
                        value: Literal::Boolean(true),
                        span: source.span("true", 0),
                    },
                    stmts: vec![rumoca_core::Statement::Assignment {
                        comp: test_component_reference("y", guarded_span),
                        value: Expression::Literal {
                            value: Literal::Real(1.0),
                            span: source.span("1.0", 0),
                        },
                        span: guarded_span,
                    }],
                }],
                span: when_span,
            },
        ],
        source.span("algorithm", 0),
        "algorithm section",
    ));
    model.is_partial = true;

    let dae = construct(&model, source.map)
        .expect("a statement written outside every when lowers on the section activation");
    dae.inspect(|view| {
        assert_eq!(view.model_event_transaction_count(), 1);
        let transaction = view
            .model_event_transaction(view.model_event_transaction_id(0).unwrap())
            .unwrap();
        assert_eq!(transaction.steps().len(), 2);
        let unguarded = transaction.steps().next().unwrap();
        for condition in [unguarded.trigger(), unguarded.guard()] {
            assert!(
                matches!(
                    view.condition(condition).unwrap().operation(),
                    dae::ConditionOperation::Always
                ),
                "an unbranched section statement runs on the section's own activation"
            );
        }
        assert_eq!(unguarded.definitions().len(), 1);
        assert_eq!(
            unguarded.definitions().next().unwrap().provenance().span(),
            unguarded_span
        );
        // The `when` beside it keeps its own event activation, so the section
        // activation never leaks into a guarded step.
        let guarded = transaction.steps().nth(1).unwrap();
        assert!(!matches!(
            view.condition(guarded.guard()).unwrap().operation(),
            dae::ConditionOperation::Always
        ));
    });
}

#[test]
fn model_event_algorithm_sequential_read_after_write_uses_new_value() {
    let source = TestSource::new(
        "model M discrete Boolean x; discrete Boolean y; algorithm when true then x := true; y := x; end when; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "x",
        "discrete Boolean x",
        51,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "y",
        "discrete Boolean y",
        52,
        Vec::new(),
        true,
    );
    let first_span = source.span("x := true", 0);
    let second_span = source.span("y := x", 0);
    let when_span = source.span("when true then x := true; y := x; end when", 0);
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![
                    rumoca_core::Statement::Assignment {
                        comp: test_component_reference("x", first_span),
                        value: Expression::Literal {
                            value: Literal::Boolean(true),
                            span: source.span("true", 1),
                        },
                        span: first_span,
                    },
                    rumoca_core::Statement::Assignment {
                        comp: test_component_reference("y", second_span),
                        value: variable_reference(&source, "x", "y := x", 0, Vec::new()),
                        span: second_span,
                    },
                ],
            }],
            span: when_span,
        }],
        source.span("algorithm when true then x := true; y := x; end when", 0),
        "algorithm section",
    ));
    model.is_partial = true;

    let dae = construct(&model, source.map)
        .expect("the event transition carries the first assignment into the second RHS");
    dae.inspect(|view| {
        assert_eq!(view.model_event_transaction_count(), 1);
        let transaction = view
            .model_event_transaction(view.model_event_transaction_id(0).unwrap())
            .unwrap();
        assert_eq!(transaction.targets().len(), 2);
        assert_eq!(transaction.steps().len(), 2);
        let second_step = transaction.steps().nth(1).unwrap();
        let second_definition = second_step.definitions().next().unwrap();
        assert_eq!(second_definition.provenance().span(), second_span);
        assert!(matches!(
            view.expression(second_definition.value())
                .unwrap()
                .operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(true))
        ));
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(owner.targets().len(), 2);
        assert_eq!(owner.targets().get(0).unwrap().index(), 0);
        assert_eq!(owner.targets().get(1).unwrap().index(), 1);
        let branch = owner.branches().get(0).unwrap();
        let (value, provenance) = branch.values().get(1).unwrap();
        assert_eq!(provenance.span(), second_span);
        assert!(matches!(
            view.expression(value).unwrap().operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(true))
        ));
    });
}

#[test]
fn model_event_algorithm_partial_assignment_retains_entry_value() {
    let source = TestSource::new(
        "model M discrete Boolean x; discrete Boolean y; algorithm when true then if false then x := true; end if; y := x; end when; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "x",
        "discrete Boolean x",
        151,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "y",
        "discrete Boolean y",
        152,
        Vec::new(),
        true,
    );
    let assignment_span = source.span("x := true", 0);
    let if_span = source.span("if false then x := true; end if", 0);
    let y_span = source.span("y := x", 0);
    let when_span = source.span(
        "when true then if false then x := true; end if; y := x; end when",
        0,
    );
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![
                    rumoca_core::Statement::If {
                        cond_blocks: vec![rumoca_core::StatementBlock {
                            cond: Expression::Literal {
                                value: Literal::Boolean(false),
                                span: source.span("false", 0),
                            },
                            stmts: vec![rumoca_core::Statement::Assignment {
                                comp: test_component_reference("x", assignment_span),
                                value: Expression::Literal {
                                    value: Literal::Boolean(true),
                                    span: source.span("true", 1),
                                },
                                span: assignment_span,
                            }],
                        }],
                        else_block: None,
                        span: if_span,
                    },
                    rumoca_core::Statement::Assignment {
                        comp: test_component_reference("y", y_span),
                        value: variable_reference(&source, "x", "y := x", 0, Vec::new()),
                        span: y_span,
                    },
                ],
            }],
            span: when_span,
        }],
        source.span("algorithm", 0),
        "algorithm section",
    ));
    model.is_partial = true;

    construct(&model, source.map)
        .expect("an unassigned event branch retains the target's event-entry value");
}

#[test]
fn model_event_algorithm_if_guard_uses_sequential_new_value() {
    let source = TestSource::new(
        "model M discrete Boolean x; discrete Boolean y; algorithm when true then x := true; if x then y := true; else y := false; end if; end when; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "x",
        "discrete Boolean x",
        53,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "y",
        "discrete Boolean y",
        54,
        Vec::new(),
        true,
    );
    let first_span = source.span("x := true", 0);
    let if_span = source.span("if x then y := true; else y := false; end if", 0);
    let condition_span = source.span("if x then", 0);
    let when_span = source.span(
        "when true then x := true; if x then y := true; else y := false; end if; end when",
        0,
    );
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![
                    rumoca_core::Statement::Assignment {
                        comp: test_component_reference("x", first_span),
                        value: Expression::Literal {
                            value: Literal::Boolean(true),
                            span: source.span("true", 1),
                        },
                        span: first_span,
                    },
                    rumoca_core::Statement::If {
                        cond_blocks: vec![rumoca_core::StatementBlock {
                            cond: variable_reference(&source, "x", "if x then", 0, Vec::new()),
                            stmts: vec![rumoca_core::Statement::Assignment {
                                comp: test_component_reference(
                                    "y",
                                    source.span("y := true", 0),
                                ),
                                value: Expression::Literal {
                                    value: Literal::Boolean(true),
                                    span: source.span("true", 2),
                                },
                                span: source.span("y := true", 0),
                            }],
                        }],
                        else_block: Some(vec![rumoca_core::Statement::Assignment {
                            comp: test_component_reference("y", source.span("y := false", 0)),
                            value: Expression::Literal {
                                value: Literal::Boolean(false),
                                span: source.span("false", 0),
                            },
                            span: source.span("y := false", 0),
                        }]),
                        span: if_span,
                    },
                ],
            }],
            span: when_span,
        }],
        source.span(
            "algorithm when true then x := true; if x then y := true; else y := false; end if; end when",
            0,
        ),
        "algorithm section",
    ));
    model.is_partial = true;

    let dae = construct(&model, source.map)
        .expect("the event-local if guard reads the checked sequential environment");
    dae.inspect(|view| {
        let condition = (0..view.condition_count())
            .filter_map(|index| view.condition(view.condition_id(index)?))
            .find(|condition| condition.provenance().span() == condition_span)
            .expect("the if guard retains its exact source owner");
        let dae::ConditionOperation::Discrete(value) = condition.operation() else {
            panic!("an event-local if owns one discrete Boolean guard");
        };
        assert!(matches!(
            view.expression(value).unwrap().operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(true))
        ));
    });
}

#[test]
fn model_event_algorithm_total_element_loop_stays_one_tensor_map() {
    let source = TestSource::new(
        "model M discrete Boolean x[3]; algorithm when true then for i in 1:3 loop x[i] := true; end for; end when; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "x",
        "discrete Boolean x[3]",
        53,
        vec![3],
        true,
    );
    let assignment_span = source.span("x[i] := true", 0);
    let index_span = source.span("i", 1);
    let component = rumoca_core::ComponentReference::construct(
        false,
        assignment_span,
        vec![rumoca_core::ComponentRefPart {
            ident: "x".to_string(),
            span: assignment_span,
            subs: vec![Subscript::Expr {
                expr: Box::new(Expression::VarRef {
                    name: Reference::new("i"),
                    subscripts: Vec::new(),
                    span: index_span,
                }),
                span: index_span,
            }],
            def_id: rumoca_core::DefId::new(test_instance_id("x").index()),
        }],
    )
    .unwrap();
    let range_span = source.span("1:3", 0);
    let loop_span = source.span("for i in 1:3 loop x[i] := true; end for", 0);
    let when_span = source.span(
        "when true then for i in 1:3 loop x[i] := true; end for; end when",
        0,
    );
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "i".to_string(),
                        range: Expression::Range {
                            start: Box::new(Expression::Literal {
                                value: Literal::Integer(1),
                                span: source.span("1", 0),
                            }),
                            step: None,
                            end: Box::new(Expression::Literal {
                                value: Literal::Integer(3),
                                span: source.span("3", 1),
                            }),
                            span: range_span,
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: component,
                        value: Expression::Literal {
                            value: Literal::Boolean(true),
                            span: source.span("true", 1),
                        },
                        span: assignment_span,
                    }],
                    span: loop_span,
                }],
            }],
            span: when_span,
        }],
        source.span(
            "algorithm when true then for i in 1:3 loop x[i] := true; end for; end when",
            0,
        ),
        "algorithm section",
    ));

    let dae = construct(&model, source.map).expect("the total loop has a checked tensor owner");
    dae.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let value = owner.branches().get(0).unwrap().values().get(0).unwrap().0;
        let expression = view.expression(value).unwrap();
        assert_eq!(expression.value_type().dimensions(), &[3]);
        assert!(matches!(
            expression.operation(),
            dae::ExpressionOperation::Comprehension { .. }
        ));
    });
}

fn identity_boolean_function(
    source: &TestSource,
    boolean: rumoca_core::TypeId,
) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new("f", source.span("function f", 0));
    function.add_input(function_param(
        "u",
        "Boolean",
        boolean,
        boolean,
        Vec::new(),
        source.span("input Boolean u", 0),
    ));
    function.add_output(function_param(
        "y",
        "Boolean",
        boolean,
        boolean,
        Vec::new(),
        source.span("output Boolean y", 0),
    ));
    let function_assignment = source.span("y := u", 0);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: test_component_reference("y", function_assignment),
        value: Expression::VarRef {
            name: Reference::new("u"),
            subscripts: Vec::new(),
            span: source.span("u", 1),
        },
        span: function_assignment,
    });
    function
}

fn function_call_loop_algorithm(source: &TestSource) -> flat::Algorithm {
    let index_span = source.span("i", 1);
    let index_expression = || Expression::VarRef {
        name: Reference::new("i"),
        subscripts: Vec::new(),
        span: index_span,
    };
    let assignment_span = source.span("x[i] := f(source[i])", 0);
    let target = rumoca_core::ComponentReference::construct(
        false,
        assignment_span,
        vec![rumoca_core::ComponentRefPart {
            ident: "x".to_string(),
            span: source.span("x[i]", 0),
            subs: vec![Subscript::Expr {
                expr: Box::new(index_expression()),
                span: index_span,
            }],
            def_id: rumoca_core::DefId::new(test_instance_id("x").index()),
        }],
    )
    .unwrap();
    let call_span = source.span("f(source[i])", 0);
    let value = Expression::FunctionCall {
        name: Reference::new("f"),
        args: vec![Expression::VarRef {
            name: test_reference("source"),
            subscripts: vec![Subscript::Expr {
                expr: Box::new(index_expression()),
                span: index_span,
            }],
            span: source.span("source[i]", 0),
        }],
        is_constructor: false,
        span: call_span,
    };
    let range_span = source.span("1:3", 0);
    let loop_span = source.span("for i in 1:3 loop x[i] := f(source[i]); end for", 0);
    let when_span = source.span(
        "when true then for i in 1:3 loop x[i] := f(source[i]); end for; end when",
        0,
    );
    flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "i".to_string(),
                        range: Expression::Range {
                            start: Box::new(Expression::Literal {
                                value: Literal::Integer(1),
                                span: source.span("1", 0),
                            }),
                            step: None,
                            end: Box::new(Expression::Literal {
                                value: Literal::Integer(3),
                                span: source.span("3", 1),
                            }),
                            span: range_span,
                        },
                    }],
                    equations: vec![rumoca_core::Statement::Assignment {
                        comp: target,
                        value,
                        span: assignment_span,
                    }],
                    span: loop_span,
                }],
            }],
            span: when_span,
        }],
        source.span("algorithm", 1),
        "algorithm section",
    )
}

#[test]
fn model_event_tensor_loop_function_call_uses_scalar_binder_shape() {
    let source = TestSource::new(
        "function f input Boolean u; output Boolean y; algorithm y := u; end f; \
         model M discrete Boolean x[3]; discrete Boolean source[3]; algorithm \
         when true then for i in 1:3 loop x[i] := f(source[i]); end for; end when; end M;",
    );
    let mut model = test_model();
    for (name, declaration, type_id) in [
        ("x", "discrete Boolean x[3]", 57),
        ("source", "discrete Boolean source[3]", 58),
    ] {
        add_primitive_variable(
            &mut model,
            &source,
            name,
            declaration,
            type_id,
            vec![3],
            true,
        );
    }
    model.add_function(identity_boolean_function(
        &source,
        model.predefined_types.boolean,
    ));
    model.algorithms.push(function_call_loop_algorithm(&source));
    model.is_partial = true;

    let dae = construct(&model, source.map)
        .expect("a function call in a compact loop shares the loop's scalar binder proof");
    dae.inspect(|view| {
        assert!((0..view.expression_count()).any(|index| {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            matches!(
                expression.operation(),
                dae::ExpressionOperation::Comprehension { .. }
            )
        }));
    });
}

#[test]
fn sampled_algorithm_clock_ownership_is_independent_of_producer_order() {
    let source = TestSource::new(
        "model M discrete Real y; discrete Real source; algorithm \
         when sample(0.0, 0.1) then y := source + pre(y); end when; algorithm \
         when sample(0.0, 0.1) then source := 1.0; end when; end M;",
    );
    let mut model = test_model();
    for (name, declaration, type_id) in [
        ("y", "discrete Real y", 59),
        ("source", "discrete Real source", 60),
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
        model
            .variables
            .get_mut(&VarName::new(name))
            .unwrap()
            .variability = Variability::Discrete(Default::default());
    }

    let sample_condition = |occurrence: usize| {
        let span = source.span("sample(0.0, 0.1)", occurrence);
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![
                Expression::Literal {
                    value: Literal::Real(0.0),
                    span: source.span("0.0", occurrence),
                },
                Expression::Literal {
                    value: Literal::Real(0.1),
                    span: source.span("0.1", occurrence),
                },
            ],
            span,
        }
    };
    let first_assignment = source.span("y := source + pre(y)", 0);
    let first_when = source.span(
        "when sample(0.0, 0.1) then y := source + pre(y); end when",
        0,
    );
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: sample_condition(0),
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: test_component_reference("y", first_assignment),
                    value: Expression::Binary {
                        op: OpBinary::Add,
                        lhs: Box::new(variable_reference(
                            &source,
                            "source",
                            "source",
                            1,
                            Vec::new(),
                        )),
                        rhs: Box::new(Expression::BuiltinCall {
                            function: BuiltinFunction::Pre,
                            args: vec![variable_reference(&source, "y", "pre(y)", 0, Vec::new())],
                            span: source.span("pre(y)", 0),
                        }),
                        span: source.span("source + pre(y)", 0),
                    },
                    span: first_assignment,
                }],
            }],
            span: first_when,
        }],
        source.span("algorithm", 0),
        "consumer algorithm",
    ));

    let second_assignment = source.span("source := 1.0", 0);
    let second_when = source.span("when sample(0.0, 0.1) then source := 1.0; end when", 0);
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: sample_condition(1),
                stmts: vec![rumoca_core::Statement::Assignment {
                    comp: test_component_reference("source", second_assignment),
                    value: Expression::Literal {
                        value: Literal::Real(1.0),
                        span: source.span("1.0", 0),
                    },
                    span: second_assignment,
                }],
            }],
            span: second_when,
        }],
        source.span("algorithm", 1),
        "producer algorithm",
    ));
    model.is_partial = true;

    construct(&model, source.map)
        .expect("all clock owners are claimed before an earlier consumer is lowered");
}

#[test]
fn model_event_algorithm_indexed_writes_form_one_tensor_ssa_value() {
    let source = TestSource::new(
        "model M discrete Boolean x[2]; algorithm when true then x[1] := true; x[2] := x[1]; end when; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "x",
        "discrete Boolean x[2]",
        55,
        vec![2],
        true,
    );
    let indexed_target = |index: i64, span: Span| {
        rumoca_core::ComponentReference::construct(
            false,
            span,
            vec![rumoca_core::ComponentRefPart {
                ident: "x".to_string(),
                span,
                subs: vec![Subscript::Index { value: index, span }],
                def_id: rumoca_core::DefId::new(test_instance_id("x").index()),
            }],
        )
        .unwrap()
    };
    let first_span = source.span("x[1] := true", 0);
    let second_span = source.span("x[2] := x[1]", 0);
    let when_span = source.span("when true then x[1] := true; x[2] := x[1]; end when", 0);
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![
                    rumoca_core::Statement::Assignment {
                        comp: indexed_target(1, first_span),
                        value: Expression::Literal {
                            value: Literal::Boolean(true),
                            span: source.span("true", 1),
                        },
                        span: first_span,
                    },
                    rumoca_core::Statement::Assignment {
                        comp: indexed_target(2, second_span),
                        value: variable_reference(
                            &source,
                            "x",
                            "x[2] := x[1]",
                            0,
                            vec![Subscript::Index {
                                value: 1,
                                span: source.span("1", 1),
                            }],
                        ),
                        span: second_span,
                    },
                ],
            }],
            span: when_span,
        }],
        source.span(
            "algorithm when true then x[1] := true; x[2] := x[1]; end when",
            0,
        ),
        "algorithm section",
    ));

    let dae = construct(&model, source.map)
        .expect("indexed writes remain one checked tensor-valued transition");
    dae.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let value = owner.branches().get(0).unwrap().values().get(0).unwrap().0;
        let outer = view.expression(value).unwrap();
        assert_eq!(outer.value_type().dimensions(), &[2]);
        let dae::ExpressionOperation::ArrayUpdate { base, value, .. } = outer.operation() else {
            panic!("the final indexed assignment must be a tensor ArrayUpdate");
        };
        assert!(matches!(
            view.expression(base).unwrap().operation(),
            dae::ExpressionOperation::ArrayUpdate { .. }
        ));
        assert!(matches!(
            view.expression(value).unwrap().operation(),
            dae::ExpressionOperation::Index { .. }
        ));
    });
}

#[test]
fn sampled_model_algorithm_assertion_keeps_the_when_activation() {
    let source = TestSource::new(
        "model M algorithm when true then assert(false, \"failed\"); end when; end M;",
    );
    let mut model = test_model();
    let assertion_span = source.span("assert(false, \"failed\")", 0);
    let when_span = source.span("when true then assert(false, \"failed\"); end when", 0);
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![rumoca_core::Statement::Assert {
                    condition: Expression::Literal {
                        value: Literal::Boolean(false),
                        span: source.span("false", 0),
                    },
                    message: Box::new(Expression::Literal {
                        value: Literal::String("failed".to_string()),
                        span: source.span("\"failed\"", 0),
                    }),
                    level: None,
                    span: assertion_span,
                }],
            }],
            span: when_span,
        }],
        source.span("algorithm", 0),
        "algorithm section",
    ));
    model.is_partial = true;

    let dae = construct(&model, source.map)
        .expect("an algorithm assertion is an action owned by its when activation");
    dae.inspect(|view| {
        assert_eq!(view.model_event_transaction_count(), 0);
        assert_eq!(view.event_action_count(), 1);
        let action = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        assert_eq!(action.provenance().span(), assertion_span);
        assert!(matches!(
            action.operation(),
            dae::EventActionOperation::Assert { .. }
        ));
    });
}

fn two_stage_tensor_loop(source: &TestSource, second_index: Expression) -> flat::Algorithm {
    let index_span = source.span("i", 1);
    let target = |name: &str, occurrence: usize| {
        let span = source.span(&format!("{name}[i]"), occurrence);
        rumoca_core::ComponentReference::construct(
            false,
            span,
            vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span,
                subs: vec![Subscript::Expr {
                    expr: Box::new(Expression::VarRef {
                        name: Reference::new("i"),
                        subscripts: Vec::new(),
                        span: index_span,
                    }),
                    span: index_span,
                }],
                def_id: rumoca_core::DefId::new(test_instance_id(name).index()),
            }],
        )
        .unwrap()
    };
    let first_span = source.span("a[i] := 1.0", 0);
    let second_span = source.span("b[i] := a[i]", 0);
    let loop_span = source.span("for i in 1:3 loop a[i] := 1.0; b[i] := a[i]; end for", 0);
    let when_span = source.span(
        "when true then for i in 1:3 loop a[i] := 1.0; b[i] := a[i]; end for; end when",
        0,
    );
    flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                stmts: vec![rumoca_core::Statement::For {
                    indices: vec![rumoca_core::ForIndex {
                        ident: "i".to_string(),
                        range: Expression::Range {
                            start: Box::new(Expression::Literal {
                                value: Literal::Integer(1),
                                span: source.span("1", 0),
                            }),
                            step: None,
                            end: Box::new(Expression::Literal {
                                value: Literal::Integer(3),
                                span: source.span("3", 0),
                            }),
                            span: source.span("1:3", 0),
                        },
                    }],
                    equations: vec![
                        rumoca_core::Statement::Assignment {
                            comp: target("a", 0),
                            value: Expression::Literal {
                                value: Literal::Real(1.0),
                                span: source.span("1.0", 0),
                            },
                            span: first_span,
                        },
                        rumoca_core::Statement::Assignment {
                            comp: target("b", 0),
                            value: Expression::VarRef {
                                name: test_reference("a"),
                                subscripts: vec![Subscript::Expr {
                                    expr: Box::new(second_index),
                                    span: index_span,
                                }],
                                span: source.span("a[i]", 1),
                            },
                            span: second_span,
                        },
                    ],
                    span: loop_span,
                }],
            }],
            span: when_span,
        }],
        source.span("algorithm", 0),
        "algorithm section",
    )
}

fn two_tensor_model(source: &TestSource) -> flat::Model {
    let mut model = test_model();
    for (name, declaration, type_id) in [
        ("a", "discrete Real a[3]", 62),
        ("b", "discrete Real b[3]", 63),
    ] {
        add_primitive_variable(
            &mut model,
            source,
            name,
            declaration,
            type_id,
            vec![3],
            false,
        );
        model
            .variables
            .get_mut(&VarName::new(name))
            .unwrap()
            .variability = Variability::Discrete(Default::default());
    }
    model.is_partial = true;
    model
}

#[test]
fn event_tensor_loop_allows_acyclic_same_element_dependency() {
    let source = TestSource::new(
        "model M discrete Real a[3]; discrete Real b[3]; algorithm when true then for i in 1:3 loop a[i] := 1.0; b[i] := a[i]; end for; end when; end M;",
    );
    let mut model = two_tensor_model(&source);
    model.algorithms.push(two_stage_tensor_loop(
        &source,
        Expression::VarRef {
            name: Reference::new("i"),
            subscripts: Vec::new(),
            span: source.span("i", 2),
        },
    ));

    construct(&model, source.map)
        .expect("same-index dependencies form an ordered pair of compact tensor maps");
}

#[test]
fn event_tensor_loop_rejects_cross_element_dependency() {
    let source = TestSource::new(
        "model M discrete Real a[3]; discrete Real b[3]; algorithm when true then for i in 1:3 loop a[i] := 1.0; b[i] := a[i]; end for; end when; end M;",
    );
    let mut model = two_tensor_model(&source);
    model.algorithms.push(two_stage_tensor_loop(
        &source,
        Expression::Literal {
            value: Literal::Integer(1),
            span: source.span("1", 1),
        },
    ));

    let error = construct(&model, source.map)
        .expect_err("cross-index reads are a recurrence, not a pointwise tensor map");
    assert!(
        error
            .to_string()
            .contains("reads a different tensor element")
    );
}

fn sampled_step_function(source: &TestSource, model: &flat::Model) -> rumoca_core::Function {
    let mut step = rumoca_core::Function::new("step", source.span("function step", 0));
    step.add_input(real_function_param(
        "u",
        Vec::new(),
        source.span("input Real u", 0),
    ));
    step.add_output(real_function_param(
        "x",
        Vec::new(),
        source.span("output Real x", 0),
    ));
    step.add_output(function_param(
        "ok",
        "Boolean",
        model.predefined_types.boolean,
        model.predefined_types.boolean,
        Vec::new(),
        source.span("output Boolean ok", 0),
    ));
    let function_x = source.span("x := u", 0);
    let function_ok = source.span("ok := true", 0);
    step.body = vec![
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("x", function_x),
            value: Expression::VarRef {
                name: Reference::new("u"),
                subscripts: Vec::new(),
                span: source.span("u", 1),
            },
            span: function_x,
        },
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("ok", function_ok),
            value: Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            },
            span: function_ok,
        },
    ];
    step
}

fn add_sampled_result_variables(model: &mut flat::Model, source: &TestSource) {
    for (name, declaration, type_id, discrete_type) in [
        ("x", "discrete Real x", 71, false),
        ("y", "discrete Real y", 72, false),
        ("ok", "discrete Boolean ok", 73, true),
    ] {
        add_primitive_variable(
            model,
            source,
            name,
            declaration,
            type_id,
            Vec::new(),
            discrete_type,
        );
        model
            .variables
            .get_mut(&VarName::new(name))
            .unwrap()
            .variability = Variability::Discrete(Default::default());
    }
}

fn sampled_step_call(source: &TestSource, model: &flat::Model) -> rumoca_core::Statement {
    let call_span = source.span("(x, ok) := step(1.0)", 0);
    let step_instance = model.functions[&VarName::new("step")]
        .instance_id
        .expect("Flat gives the callee an exact instance");
    rumoca_core::Statement::FunctionCall {
        comp: Reference::from_component_reference(test_component_reference("step", call_span))
            .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                instance_id: step_instance,
                base_part_count: 1,
                transitively_non_replaceable: true,
            }),
        args: vec![Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        }],
        outputs: vec![
            Some(test_component_reference("x", call_span)),
            Some(test_component_reference("ok", call_span)),
        ],
        span: call_span,
    }
}

fn sampled_result_algorithm(source: &TestSource, call: rumoca_core::Statement) -> flat::Algorithm {
    let y_span = source.span("y := x", 0);
    let sample_span = source.span("sample(0.0, 0.1)", 0);
    let when_span = source.span(
        "when sample(0.0, 0.1) then (x, ok) := step(1.0); y := x; end when",
        0,
    );
    flat::Algorithm::new(
        vec![rumoca_core::Statement::When {
            blocks: vec![rumoca_core::StatementBlock {
                cond: Expression::BuiltinCall {
                    function: BuiltinFunction::Sample,
                    args: vec![
                        Expression::Literal {
                            value: Literal::Real(0.0),
                            span: source.span("0.0", 0),
                        },
                        Expression::Literal {
                            value: Literal::Real(0.1),
                            span: source.span("0.1", 0),
                        },
                    ],
                    span: sample_span,
                },
                stmts: vec![
                    call,
                    rumoca_core::Statement::Assignment {
                        comp: test_component_reference("y", y_span),
                        value: variable_reference(source, "x", "y := x", 0, Vec::new()),
                        span: y_span,
                    },
                ],
            }],
            span: when_span,
        }],
        source.span("algorithm when", 0),
        "sampled mixed transaction",
    )
}

#[test]
fn sampled_mixed_result_call_stays_one_ordered_model_event_transaction() {
    let source = TestSource::new(
        "function step input Real u; output Real x; output Boolean ok; algorithm \
         x := u; ok := true; end step; model M discrete Real x; discrete Real y; \
         discrete Boolean ok; algorithm when sample(0.0, 0.1) then \
         (x, ok) := step(1.0); y := x; end when; end M;",
    );
    let mut model = test_model();
    model.add_function(sampled_step_function(&source, &model));
    add_sampled_result_variables(&mut model, &source);
    let call = sampled_step_call(&source, &model);
    model
        .algorithms
        .push(sampled_result_algorithm(&source, call));
    model.is_partial = true;

    let dae = construct(&model, source.map)
        .expect("the mixed sampled algorithm is one checked transaction");
    dae.inspect(|view| {
        assert_eq!(view.model_event_transaction_count(), 1);
        let transaction = view
            .model_event_transaction(view.model_event_transaction_id(0).unwrap())
            .unwrap();
        assert_eq!(transaction.targets().len(), 3);
        assert_eq!(transaction.steps().len(), 2);
        assert!(transaction.steps().all(|step| step.clock().is_some()));
        let definitions = transaction
            .steps()
            .flat_map(|step| step.definitions())
            .collect::<Vec<_>>();
        assert_eq!(definitions.len(), 3);
        let call_owners = definitions
            .iter()
            .filter_map(|definition| {
                let expression = view.expression(definition.value()).unwrap();
                match expression.operation() {
                    dae::ExpressionOperation::Call { owner, .. } => Some(owner),
                    _ => None,
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(call_owners.len(), 3);
        assert!(call_owners.windows(2).all(|owners| owners[0] == owners[1]));
    });
}
