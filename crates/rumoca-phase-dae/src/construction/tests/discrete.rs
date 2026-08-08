use rumoca_core::Reference;

use super::super::*;
use super::support::*;

/// `time > <threshold>`, with both spans taken from the caller's source text.
fn chain_time_relation(source: &TestSource, text: &str, threshold: f64) -> Expression {
    Expression::Binary {
        op: OpBinary::Gt,
        lhs: Box::new(Expression::VarRef {
            name: Reference::new("time"),
            subscripts: Vec::new(),
            span: source.span("time", 0),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Real(threshold),
            span: source.span(text, 0),
        }),
        span: source.span(text, 0),
    }
}

/// `when time > 0.5 then m = true; elsewhen time > 0.5 then m = false;`
///
/// Both branches are *relations*, and deliberately the same one. A literal
/// `true` would not do: MLS §8.3.5.1 starts its activation buffer at the
/// condition's own value, so it has no rising edge at all and therefore cannot
/// exhibit the simultaneous-edge case this fixture exists to pin. Two spellings
/// of one threshold do become true at the same instant, which is exactly when
/// §8.3.5's branch priority has anything to resolve.
fn source_priority_when_model(source: &TestSource) -> flat::Model {
    let first_condition_span = source.span("0.5", 0);
    let second_condition_span = source.span("0.5", 1);
    let first_assignment_span = source.span("m = true", 0);
    let second_assignment_span = source.span("m = false", 0);
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );
    let mut first = flat::WhenBranch::new(
        chain_time_relation(source, "0.5", 0.5),
        first_condition_span,
    );
    first.add_equation(flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(true),
            span: source.span("true", 0),
        },
        first_assignment_span,
        "first branch",
    ));
    let mut second = flat::WhenBranch::new(
        Expression::Binary {
            op: OpBinary::Gt,
            lhs: Box::new(Expression::VarRef {
                name: Reference::new("time"),
                subscripts: Vec::new(),
                span: source.span("time", 1),
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Real(0.5),
                span: second_condition_span,
            }),
            span: second_condition_span,
        },
        second_condition_span,
    );
    second.add_equation(flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(false),
            span: source.span("false", 0),
        },
        second_assignment_span,
        "second branch",
    ));
    let mut chain = flat::WhenChain::new(
        first,
        source.span(
            "when time > 0.5 then m = true; elsewhen time > 0.5 then m = false; end when",
            0,
        ),
    );
    chain.push_else_when(second);
    model.when_chains.push(chain);
    model
}

fn duplicate_when_model(source: &TestSource, nested: bool) -> flat::Model {
    let condition_span = source.span("when true", 0);
    let first_span = source.span("m = true", 0);
    let second_span = source.span("m = false", 0);
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(true),
            span: source.span("true", 1),
        },
        first_span,
        "first definition",
    ));
    let second = flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(false),
            span: source.span("false", 0),
        },
        second_span,
        "second definition",
    );
    if nested {
        branch.add_equation(flat::WhenEquation::conditional(
            vec![(
                Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("if true", 0),
                },
                vec![second],
            )],
            None,
            source.span("if true then m = false; end if", 0),
            "nested second definition",
        ));
    } else {
        branch.add_equation(second);
    }
    model
        .when_chains
        .push(flat::WhenChain::new(branch, source.span("when true", 0)));
    model
}

fn atomic_when_dependency_model(source: &TestSource, cyclic: bool) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        source,
        "a",
        "discrete Boolean a",
        8,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        source,
        "z",
        "discrete Boolean z",
        9,
        Vec::new(),
        true,
    );
    let condition_span = source.span("true", 0);
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: condition_span,
        },
        condition_span,
    );
    let a_action = source.span("a = z", 0);
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("a"),
        Expression::VarRef {
            name: Reference::new("z"),
            subscripts: Vec::new(),
            span: Span::from_offsets(source.source, a_action.end.0 - 1, a_action.end.0),
        },
        a_action,
        "consumer action",
    ));
    let z_action = if cyclic {
        source.span("z = a", 0)
    } else {
        source.span("z = true", 0)
    };
    let z_value = if cyclic {
        Expression::VarRef {
            name: Reference::new("a"),
            subscripts: Vec::new(),
            span: Span::from_offsets(source.source, z_action.end.0 - 1, z_action.end.0),
        }
    } else {
        Expression::Literal {
            value: Literal::Boolean(true),
            span: source.span("true", 1),
        }
    };
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("z"),
        z_value,
        z_action,
        "producer action",
    ));
    model
        .when_chains
        .push(flat::WhenChain::new(branch, source.span("when true", 0)));
    model
}

/// Every branch of a `when`/`elsewhen` chain is activated by its own condition
/// and nothing else, and the branches reach the owner in source order.
///
/// MLS §8.3.5 activates the equations of a when-equation *"only at the instant
/// when the scalar expression or any of the elements of the vector expression
/// becomes true"*, and §8.3.5.1 writes the chain as one if-expression per
/// assigned variable whose arms are `edge(b1)`, `edge(b2)`, … over one `Boolean
/// bi` per branch condition. An `elsewhen` guard is therefore its own condition,
/// unqualified: nothing about the earlier branches belongs in it.
///
/// Source priority is the *order* of those arms, which is why this test pins the
/// branch order and the exact assignment each branch carries. §8.3.5 scopes the
/// priority to the same thing: the chain *"can be used to resolve assignment
/// conflicts since the first of the when/elsewhen parts are given higher
/// priority than later ones"*, and a conflict is two arms selected at one
/// instant — resolved by taking the earlier arm.
///
/// The guard used to be `cond_i and not (cond_1 or …)`. That subtracts the
/// earlier branch's *level*, not its edge, so a first condition that stays true
/// suppressed every later branch permanently: `when time > 0.3 then y = 1;
/// elsewhen time > 0.7 then y = 2;` held `y = 1` for the whole run where
/// OpenModelica reaches `y = 2` at `t = 0.7`. Both branches here are the same
/// relation `time > 0.5` — the case where the two edges really are simultaneous
/// — so branch order is the only thing that can resolve it, and the order is
/// what is asserted.
#[test]
fn when_chain_activates_each_branch_by_its_own_condition_in_source_order() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when time > 0.5 then m = true; elsewhen time > 0.5 then m = false; end when; end M;",
    );
    let first_condition_span = source.span("0.5", 0);
    let second_condition_span = source.span("0.5", 1);
    let model = source_priority_when_model(&source);
    let dae = construct(&model, source.map).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.event_action_count(), 0);
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let first = owner.branches().get(0).unwrap();
        let second = owner.branches().get(1).unwrap();
        assert_eq!(
            view.source_text(first.values().get(0).unwrap().1),
            Some("m = true"),
            "the higher-priority arm must stay first"
        );
        assert_eq!(
            view.source_text(second.values().get(0).unwrap().1),
            Some("m = false"),
            "and the elsewhen arm must stay second"
        );
        let dae::DiscreteBranchActivation::When {
            trigger: first_trigger,
            guard: first_guard,
        } = first.activation()
        else {
            panic!("source when branch must remain condition-owned");
        };
        let dae::DiscreteBranchActivation::When {
            trigger: second_trigger,
            guard: second_guard,
        } = second.activation()
        else {
            panic!("source elsewhen branch must remain condition-owned");
        };
        assert_eq!(
            first_guard, first_trigger,
            "the first branch is activated by its own edge"
        );
        assert_eq!(
            second_guard, second_trigger,
            "and so is the elsewhen branch: MLS §8.3.5.1 gives it `edge(b2)`, not \
             `edge(b2) and not b1`"
        );
        assert_ne!(
            first_trigger, second_trigger,
            "each branch condition is its own `Boolean bi`, even when the two are \
             written with the same text"
        );

        let first_condition = view.condition(first_trigger).unwrap();
        assert_eq!(first_condition.provenance().span(), first_condition_span);
        assert_eq!(
            first_condition.provenance().origin(),
            dae::DaeProvenanceOrigin::Source
        );
        let second_condition = view.condition(second_trigger).unwrap();
        assert_eq!(second_condition.provenance().span(), second_condition_span);
        assert_eq!(
            second_condition.provenance().origin(),
            dae::DaeProvenanceOrigin::Source
        );
    });
}

#[test]
fn when_discrete_real_lowers_to_condition_owned_b1b_residual() {
    let source = TestSource::new(
        "model M discrete Real z; equation \
         when true then z = 1.0; end when; end M;",
    );
    let assignment_span = source.span("z = 1.0", 0);
    let condition_span = source.span("true", 0);
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "z",
        "discrete Real z",
        8,
        Vec::new(),
        false,
    );
    model
        .variables
        .get_mut(&VarName::new("z"))
        .unwrap()
        .variability = Variability::Discrete(Default::default());
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("z"),
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        assignment_span,
        "discrete Real assignment",
    ));
    model.when_chains.push(flat::WhenChain::new(
        branch,
        source.span("when true then z = 1.0; end when", 0),
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.event_action_count(), 0);
        assert_eq!(view.discrete_real_equation_count(), 1);
        let equation = view.discrete_real_equation(0).unwrap();
        assert_eq!(equation.provenance().span(), assignment_span);
        assert!(matches!(
            equation.activation(),
            dae::DiscreteRealActivation::When { .. }
        ));
        assert_eq!(
            view.expression(equation.residual())
                .unwrap()
                .provenance()
                .origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::SyntheticResidual)
        );
    });
}

#[test]
fn discrete_real_connection_keeps_continuous_pass_through_equation() {
    let source = TestSource::new(
        "model M output Real source; output Real forwarded; equation \
         when true then source = 1.0; end when; connect(source, forwarded); end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "source",
        "output Real source",
        8,
        Vec::new(),
        false,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "forwarded",
        "output Real forwarded",
        9,
        Vec::new(),
        false,
    );
    for name in ["source", "forwarded"] {
        let variable = model.variables.get_mut(&VarName::new(name)).unwrap();
        variable.causality = Causality::Output(Default::default());
        variable.component_ref = Some(test_component_reference(
            name,
            source.span(&format!("output Real {name}"), 0),
        ));
    }
    let condition_span = source.span("true", 0);
    let assignment_span = source.span("source = 1.0", 0);
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assign(
        VarName::new("source"),
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
        assignment_span,
        "sampled source",
    ));
    model.when_chains.push(flat::WhenChain::new(
        branch,
        source.span("when true then source = 1.0; end when", 0),
    ));
    let connection_span = source.span("connect(source, forwarded)", 0);
    model.add_equation(connection_equation(
        scalar_connection_reference(&source, "source", 2),
        scalar_connection_reference(&source, "forwarded", 1),
        connection_span,
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.discrete_real_equation_count(), 1);
        assert_eq!(view.continuous_owner_count(), 1);
        let owner = view.continuous_owner(0).unwrap();
        let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
            panic!("a scalar connection remains one residual owner");
        };
        assert_eq!(equation.provenance().span(), connection_span);
    });
}

#[test]
fn array_discrete_real_binding_keeps_one_target_shaped_b1b_owner() {
    let source = TestSource::new("model M discrete Real z[3] = {1.0, 2.0, 3.0}; end M;");
    let declaration_span = source.span("discrete Real z[3]", 0);
    let binding_span = source.span("{1.0, 2.0, 3.0}", 0);
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "z",
        "discrete Real z[3]",
        8,
        vec![3],
        false,
    );
    let variable = model.variables.get_mut(&VarName::new("z")).unwrap();
    variable.variability = Variability::Discrete(Default::default());
    variable.binding = Some(Expression::Array {
        elements: [1.0, 2.0, 3.0]
            .into_iter()
            .map(|value| Expression::Literal {
                value: Literal::Real(value),
                span: binding_span,
            })
            .collect(),
        is_matrix: false,
        span: binding_span,
    });
    variable.source_span = declaration_span;

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.discrete_real_equation_count(), 1);
        let equation = view.discrete_real_equation(0).unwrap();
        assert!(matches!(
            equation.activation(),
            dae::DiscreteRealActivation::Always
        ));
        assert_eq!(
            view.expression(equation.residual())
                .unwrap()
                .value_type()
                .dimensions(),
            &[3]
        );
    });
}

#[test]
fn b1c_topology_orders_producers_before_declaration_order_consumers() {
    let source =
        TestSource::new("model M discrete Boolean a = z; discrete Boolean z = true; end M;");
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "a",
        "discrete Boolean a",
        8,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "z",
        "discrete Boolean z",
        9,
        Vec::new(),
        true,
    );
    model.variables.get_mut(&VarName::new("a")).unwrap().binding = Some(Expression::VarRef {
        name: Reference::new("z"),
        subscripts: Vec::new(),
        span: source.span("z", 0),
    });
    model.variables.get_mut(&VarName::new("z")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    });

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 2);
        let producer = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let consumer = view
            .discrete_value_owner(view.discrete_value_owner_id(1).unwrap())
            .unwrap();
        assert_eq!(producer.targets().get(0).unwrap().index(), 1);
        assert_eq!(consumer.targets().get(0).unwrap().index(), 0);
    });
}

#[test]
fn b1c_connection_assigns_the_exact_input_from_its_output_owner() {
    let source = TestSource::new(
        "model M output Boolean source; input Boolean sink; equation \
         source = true; connect(source, sink); end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "source",
        "output Boolean source",
        8,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "sink",
        "input Boolean sink",
        9,
        Vec::new(),
        true,
    );
    model
        .variables
        .get_mut(&VarName::new("source"))
        .unwrap()
        .causality = Causality::Output(Default::default());
    model
        .variables
        .get_mut(&VarName::new("sink"))
        .unwrap()
        .causality = Causality::Input(Default::default());
    model
        .variables
        .get_mut(&VarName::new("sink"))
        .unwrap()
        .component_ref = Some(test_component_reference(
        "sink",
        source.span("input Boolean sink", 0),
    ));

    let definition_span = source.span("source = true", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: Reference::new("source"),
                subscripts: Vec::new(),
                span: source.span("source", 1),
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            }),
            span: definition_span,
        },
        definition_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    let connection_span = source.span("connect(source, sink)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: Reference::new("source"),
                subscripts: Vec::new(),
                span: source.span("source", 2),
            }),
            rhs: Box::new(Expression::VarRef {
                name: Reference::new("sink"),
                subscripts: Vec::new(),
                span: source.span("sink", 1),
            }),
            span: connection_span,
        },
        connection_span,
        flat::EquationOrigin::Connection {
            lhs: "source".to_string(),
            rhs: "sink".to_string(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 2);
        let source_owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let sink_owner = view
            .discrete_value_owner(view.discrete_value_owner_id(1).unwrap())
            .unwrap();
        assert_eq!(source_owner.targets().get(0).unwrap().index(), 0);
        assert_eq!(sink_owner.targets().get(0).unwrap().index(), 1);
        assert_eq!(sink_owner.provenance().span(), connection_span);
    });
}

#[test]
fn b1c_connection_keeps_indexed_output_value_when_output_is_written_first() {
    assert_indexed_output_connection_value(true);
}

#[test]
fn b1c_connection_keeps_indexed_output_value_when_input_is_written_first() {
    assert_indexed_output_connection_value(false);
}

fn assert_indexed_output_connection_value(output_first: bool) {
    let connection = if output_first {
        "connect(source[1], sink)"
    } else {
        "connect(sink, source[1])"
    };
    let text = format!(
        "model M output Boolean source[2] = {{true, false}}; input Boolean sink; equation {connection}; end M;"
    );
    let source = TestSource::new(&text);
    let connection_span = source.span(connection, 0);
    let mut model = indexed_output_connection_model(&source, output_first);
    model
        .variables
        .get_mut(&VarName::new("source"))
        .unwrap()
        .binding = Some(Expression::Array {
        elements: vec![
            Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            },
            Expression::Literal {
                value: Literal::Boolean(false),
                span: source.span("false", 0),
            },
        ],
        is_matrix: false,
        span: source.span("{true, false}", 0),
    });

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let owner = (0..view.discrete_value_owner_count())
            .filter_map(|index| view.discrete_value_owner(view.discrete_value_owner_id(index)?))
            .find(|owner| owner.provenance().span() == connection_span)
            .expect("the connection owns the scalar input definition");
        let branch = owner.branches().get(0).expect("the owner is unconditional");
        let value = view
            .expression(branch.values().get(0).expect("the branch assigns sink").0)
            .expect("the assigned value resolves");
        let dae::ExpressionOperation::Index { base, subscripts } = value.operation() else {
            panic!("the output element remains a checked Index expression");
        };
        assert_eq!(subscripts.len(), 1);
        assert!(matches!(
            view.expression(base).unwrap().operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(_))
        ));
        assert_eq!(value.value_type().scalar_type(), dae::ScalarType::Boolean);
        assert!(value.value_type().dimensions().is_empty());
    });
}

#[test]
fn b1c_connection_rejects_a_subscripted_input_target() {
    let source = TestSource::new(
        "model M output Boolean source = true; input Boolean sink[2]; equation connect(source, sink[1]); end M;",
    );
    let connection_span = source.span("connect(source, sink[1])", 0);
    let mut model = test_model();
    add_connection_endpoint(
        &mut model,
        &source,
        "source",
        "output Boolean source",
        8,
        Vec::new(),
        Causality::Output(Default::default()),
    );
    add_connection_endpoint(
        &mut model,
        &source,
        "sink",
        "input Boolean sink[2]",
        9,
        vec![2],
        Causality::Input(Default::default()),
    );
    model
        .variables
        .get_mut(&VarName::new("source"))
        .unwrap()
        .binding = Some(Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    });
    model.add_equation(connection_equation(
        Expression::VarRef {
            name: test_reference("source"),
            subscripts: Vec::new(),
            span: source.span("source", 1),
        },
        Expression::VarRef {
            name: test_reference("sink"),
            subscripts: vec![Subscript::Index {
                value: 1,
                span: source.span("sink[1]", 0),
            }],
            span: source.span("sink[1]", 0),
        },
        connection_span,
    ));

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::DiscreteSolvedFormViolation { detail, span })
            if detail.contains("cover a discrete coordinate exactly once")
                && span == connection_span
    ));
}

#[test]
fn b1c_connections_coalesce_complete_element_coverage_into_one_array_owner() {
    let source = TestSource::new(
        "model M output Boolean a = true; output Boolean b = false; input Boolean sink[2]; \
         equation connect(a, sink[1]); connect(b, sink[2]); end M;",
    );
    let model = complete_array_connection_model(&source, false);
    let dae = construct(&model, source.map).unwrap();

    dae.inspect(|view| {
        let owner = (0..view.discrete_value_owner_count())
            .filter_map(|index| view.discrete_value_owner(view.discrete_value_owner_id(index)?))
            .find(|owner| {
                owner
                    .targets()
                    .get(0)
                    .is_some_and(|target| target.index() == 2)
            })
            .expect("the two element edges form one array-coordinate owner");
        let branch = owner.branches().get(0).unwrap();
        let value = view.expression(branch.values().get(0).unwrap().0).unwrap();
        let dae::ExpressionOperation::Array(elements) = value.operation() else {
            panic!("the complete element coverage is represented as one array value");
        };
        assert_eq!(elements.len(), 2);
        assert_eq!(value.value_type().dimensions(), &[2]);
    });
}

#[test]
fn b1c_connections_reject_overlapping_element_coverage() {
    let source = TestSource::new(
        "model M output Boolean a = true; output Boolean b = false; input Boolean sink[2]; \
         equation connect(a, sink[1]); connect(b, sink[1]); end M;",
    );
    let duplicate_span = source.span("connect(b, sink[1])", 0);
    let model = complete_array_connection_model(&source, true);

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::DiscreteSolvedFormViolation { detail, span })
            if detail.contains("overlapping element assignments") && span == duplicate_span
    ));
}

#[test]
fn b1c_element_assignments_construct_one_ordered_array_owner() {
    let source = TestSource::new(
        "model M discrete Boolean auxiliary[2]; equation auxiliary[1] = true; \
         auxiliary[2] = auxiliary[1]; end M;",
    );
    let model = ordered_array_assignment_model(&source, false);
    let dae = construct(&model, source.map)
        .expect("exact element coverage with a strictly backward recurrence constructs one owner");

    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let value = view
            .expression(owner.branches().get(0).unwrap().values().get(0).unwrap().0)
            .unwrap();
        assert!(matches!(
            value.operation(),
            dae::ExpressionOperation::Array(elements) if elements.len() == 2
        ));
        assert_eq!(value.value_type().dimensions(), &[2]);
    });
}

#[test]
fn b1c_element_assignments_reject_a_forward_array_recurrence() {
    let source = TestSource::new(
        "model M discrete Boolean auxiliary[2]; equation auxiliary[1] = auxiliary[2]; \
         auxiliary[2] = true; end M;",
    );
    let model = ordered_array_assignment_model(&source, true);

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::DiscreteSolvedFormViolation { detail, .. })
            if detail.contains("identity 0 reads discrete-value identity 0")
    ));
}

fn ordered_array_assignment_model(source: &TestSource, forward: bool) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        source,
        "auxiliary",
        "discrete Boolean auxiliary[2]",
        8,
        vec![2],
        true,
    );
    for index in 1..=2 {
        let lhs_text = format!("auxiliary[{index}]");
        let lhs_occurrence = usize::from(index == 2 && !forward);
        let rhs = if (index == 1) == forward {
            Expression::VarRef {
                name: test_reference("auxiliary"),
                subscripts: vec![Subscript::Index {
                    value: if forward { 2 } else { 1 },
                    span: source.span(
                        if forward {
                            "auxiliary[2]"
                        } else {
                            "auxiliary[1]"
                        },
                        1,
                    ),
                }],
                span: source.span(
                    if forward {
                        "auxiliary[2]"
                    } else {
                        "auxiliary[1]"
                    },
                    1,
                ),
            }
        } else {
            Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            }
        };
        let equation_text = if forward && index == 1 {
            "auxiliary[1] = auxiliary[2]"
        } else if !forward && index == 2 {
            "auxiliary[2] = auxiliary[1]"
        } else if index == 1 {
            "auxiliary[1] = true"
        } else {
            "auxiliary[2] = true"
        };
        let span = source.span(equation_text, 0);
        model.add_equation(flat::Equation::new(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(Expression::VarRef {
                    name: test_reference("auxiliary"),
                    subscripts: vec![Subscript::Index {
                        value: index,
                        span: source.span(&lhs_text, lhs_occurrence),
                    }],
                    span: source.span(&lhs_text, lhs_occurrence),
                }),
                rhs: Box::new(rhs),
                span,
            },
            span,
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        ));
    }
    model
}

fn complete_array_connection_model(source: &TestSource, duplicate: bool) -> flat::Model {
    let mut model = test_model();
    for (name, declaration, type_id, value) in [
        ("a", "output Boolean a", 8, true),
        ("b", "output Boolean b", 9, false),
    ] {
        add_connection_endpoint(
            &mut model,
            source,
            name,
            declaration,
            type_id,
            Vec::new(),
            Causality::Output(Default::default()),
        );
        model
            .variables
            .get_mut(&VarName::new(name))
            .unwrap()
            .binding = Some(Expression::Literal {
            value: Literal::Boolean(value),
            span: source.span(if value { "true" } else { "false" }, 0),
        });
    }
    add_connection_endpoint(
        &mut model,
        source,
        "sink",
        "input Boolean sink[2]",
        10,
        vec![2],
        Causality::Input(Default::default()),
    );
    for (member, (source_name, sink_index)) in [("a", 1), ("b", if duplicate { 1 } else { 2 })]
        .into_iter()
        .enumerate()
    {
        let rendered = format!("sink[{sink_index}]");
        let connection = format!("connect({source_name}, {rendered})");
        let span = source.span(&connection, 0);
        let sink_occurrence = usize::from(duplicate && member == 1);
        model.add_equation(connection_equation(
            scalar_connection_reference(source, source_name, 1),
            Expression::VarRef {
                name: test_reference("sink"),
                subscripts: vec![Subscript::Index {
                    value: sink_index,
                    span: source.span(&rendered, sink_occurrence),
                }],
                span: source.span(&rendered, sink_occurrence),
            },
            span,
        ));
    }
    model
}

#[test]
fn b1c_classification_keeps_discrete_controlled_real_equations_continuous() {
    let source = TestSource::new(
        "model M Boolean m = true; Real y; equation if m then y = 1.0; else y = 2.0; end if; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "m", "Boolean m", 8, Vec::new(), true);
    model.variables.get_mut(&VarName::new("m")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    });
    add_primitive_variable(&mut model, &source, "y", "Real y", 9, Vec::new(), false);
    let owner = source.span("if m then y = 1.0; else y = 2.0; end if", 0);
    model.add_equation(flat::Equation::new(
        Expression::If {
            branches: vec![(
                Expression::VarRef {
                    name: test_reference("m"),
                    subscripts: Vec::new(),
                    span: source.span("m", 1),
                },
                real_assignment_residual(&source, "1.0", 1.0, owner),
            )],
            else_branch: Box::new(real_assignment_residual(&source, "2.0", 2.0, owner)),
            span: owner,
        },
        owner,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.continuous_equation_count(), 1);
        assert_eq!(view.discrete_value_owner_count(), 1);
        let dae::ContinuousOwnerView::Residual { equation, .. } =
            view.continuous_owners().next().unwrap()
        else {
            panic!("the scalar equation must remain a scalar owner");
        };
        let residual = view.expression(equation.residual()).unwrap();
        let dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } = residual.operation()
        else {
            panic!("a branch-invariant target must form one causal residual");
        };
        assert!(matches!(
            view.expression(lhs).unwrap().operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(_))
        ));
        assert!(matches!(
            view.expression(rhs).unwrap().operation(),
            dae::ExpressionOperation::Conditional(_)
        ));
    });
}

#[test]
fn b1c_classification_ignores_discrete_guards_inside_a_continuous_value() {
    let source = TestSource::new(
        "model M Boolean m = true; Real y; equation (if m then y else 2.0) = 1.0; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "m", "Boolean m", 8, Vec::new(), true);
    model.variables.get_mut(&VarName::new("m")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    });
    add_primitive_variable(&mut model, &source, "y", "Real y", 9, Vec::new(), false);
    let owner = source.span("(if m then y else 2.0) = 1.0", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::If {
                branches: vec![(
                    Expression::VarRef {
                        name: test_reference("m"),
                        subscripts: Vec::new(),
                        span: source.span("m", 1),
                    },
                    Expression::VarRef {
                        name: test_reference("y"),
                        subscripts: Vec::new(),
                        span: source.span("y", 1),
                    },
                )],
                else_branch: Box::new(Expression::Literal {
                    value: Literal::Real(2.0),
                    span: source.span("2.0", 0),
                }),
                span: owner,
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }),
            span: owner,
        },
        owner,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| assert_eq!(view.continuous_equation_count(), 1));
}

#[test]
fn b1c_classification_rejects_mixed_discrete_and_continuous_if_branches() {
    let source = TestSource::new(
        "model M Boolean m; Real y; equation if true then m = true; else y = 2.0; end if; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(&mut model, &source, "m", "Boolean m", 8, Vec::new(), true);
    add_primitive_variable(&mut model, &source, "y", "Real y", 9, Vec::new(), false);
    let owner = source.span("if true then m = true; else y = 2.0; end if", 0);
    model.add_equation(flat::Equation::new(
        Expression::If {
            branches: vec![(
                Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 0),
                },
                Expression::Binary {
                    op: OpBinary::Sub,
                    lhs: Box::new(Expression::VarRef {
                        name: test_reference("m"),
                        subscripts: Vec::new(),
                        span: source.span("m", 1),
                    }),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Boolean(true),
                        span: source.span("true", 1),
                    }),
                    span: owner,
                },
            )],
            else_branch: Box::new(real_assignment_residual(&source, "2.0", 2.0, owner)),
            span: owner,
        },
        owner,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::DiscreteSolvedFormViolation { detail, span })
            if detail.contains("all branches") && span == owner
    ));
}

fn real_assignment_residual(
    source: &TestSource,
    literal: &str,
    value: f64,
    span: Span,
) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(Expression::VarRef {
            name: test_reference("y"),
            subscripts: Vec::new(),
            span: source.span("y", 1),
        }),
        rhs: Box::new(Expression::Literal {
            value: Literal::Real(value),
            span: source.span(literal, 0),
        }),
        span,
    }
}

#[test]
fn b1c_connection_orients_output_forwarders_from_the_exact_source_owner() {
    let source = TestSource::new(
        "model M output Boolean producer; output Boolean forwarded[1]; output Boolean boundary; \
         input Boolean sink; equation producer = true; connect(forwarded[1], producer); \
         connect(producer, boundary); connect(boundary, sink); end M;",
    );
    let (model, aggregate_span, boundary_span, sink_span) =
        output_forwarder_connection_model(&source);

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 4);
        let aggregate_owner = (0..view.discrete_value_owner_count())
            .filter_map(|index| view.discrete_value_owner(view.discrete_value_owner_id(index)?))
            .find(|owner| owner.provenance().span() == aggregate_span)
            .expect("the singleton aggregate connection has one exact owner");
        let value = view
            .expression(
                aggregate_owner
                    .branches()
                    .get(0)
                    .unwrap()
                    .values()
                    .get(0)
                    .unwrap()
                    .0,
            )
            .unwrap();
        let dae::ExpressionOperation::Array(elements) = value.operation() else {
            panic!("the full singleton selection is reconstructed as one aggregate value");
        };
        assert_eq!(elements.len(), 1);
        assert_eq!(value.value_type().dimensions(), &[1]);
        assert!(matches!(
            view.expression(elements.get(0).unwrap())
                .unwrap()
                .operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(_))
        ));
        assert!(
            (0..view.discrete_value_owner_count())
                .filter_map(|index| view.discrete_value_owner(view.discrete_value_owner_id(index)?))
                .any(|owner| owner.provenance().span() == boundary_span)
        );
        assert!(
            (0..view.discrete_value_owner_count())
                .filter_map(|index| view.discrete_value_owner(view.discrete_value_owner_id(index)?))
                .any(|owner| owner.provenance().span() == sink_span)
        );
    });
}

fn output_forwarder_connection_model(source: &TestSource) -> (flat::Model, Span, Span, Span) {
    let mut model = test_model();
    add_connection_endpoint(
        &mut model,
        source,
        "producer",
        "output Boolean producer",
        8,
        Vec::new(),
        Causality::Output(Default::default()),
    );
    add_connection_endpoint(
        &mut model,
        source,
        "forwarded",
        "output Boolean forwarded[1]",
        9,
        vec![1],
        Causality::Output(Default::default()),
    );
    add_connection_endpoint(
        &mut model,
        source,
        "boundary",
        "output Boolean boundary",
        10,
        Vec::new(),
        Causality::Output(Default::default()),
    );
    add_connection_endpoint(
        &mut model,
        source,
        "sink",
        "input Boolean sink",
        11,
        Vec::new(),
        Causality::Input(Default::default()),
    );
    add_boolean_source_equation(&mut model, source, "producer", "producer = true");
    let aggregate_span = source.span("connect(forwarded[1], producer)", 0);
    model.add_equation(connection_equation(
        indexed_connection_reference(source, "forwarded", "forwarded[1]", 0),
        scalar_connection_reference(source, "producer", 2),
        aggregate_span,
    ));
    let boundary_span = source.span("connect(producer, boundary)", 0);
    model.add_equation(connection_equation(
        scalar_connection_reference(source, "producer", 3),
        scalar_connection_reference(source, "boundary", 1),
        boundary_span,
    ));
    let sink_span = source.span("connect(boundary, sink)", 0);
    model.add_equation(connection_equation(
        scalar_connection_reference(source, "boundary", 2),
        scalar_connection_reference(source, "sink", 1),
        sink_span,
    ));
    (model, aggregate_span, boundary_span, sink_span)
}

fn add_boolean_source_equation(
    model: &mut flat::Model,
    source: &TestSource,
    target: &str,
    equation: &str,
) {
    let span = source.span(equation, 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(scalar_connection_reference(source, target, 1)),
            rhs: Box::new(Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            }),
            span,
        },
        span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
}

fn scalar_connection_reference(source: &TestSource, name: &str, occurrence: usize) -> Expression {
    Expression::VarRef {
        name: test_reference(name),
        subscripts: Vec::new(),
        span: source.span(name, occurrence),
    }
}

fn indexed_connection_reference(
    source: &TestSource,
    name: &str,
    rendered: &str,
    occurrence: usize,
) -> Expression {
    Expression::VarRef {
        name: test_reference(name),
        subscripts: vec![Subscript::Index {
            value: 1,
            span: source.span(rendered, occurrence),
        }],
        span: source.span(rendered, occurrence),
    }
}

#[test]
fn b1c_connection_does_not_invent_an_owner_between_two_producers() {
    let source = TestSource::new(
        "model M output Boolean a = true; output Boolean b = false; equation connect(a, b); end M;",
    );
    let mut model = test_model();
    add_connection_endpoint(
        &mut model,
        &source,
        "a",
        "output Boolean a",
        8,
        Vec::new(),
        Causality::Output(Default::default()),
    );
    add_connection_endpoint(
        &mut model,
        &source,
        "b",
        "output Boolean b",
        9,
        Vec::new(),
        Causality::Output(Default::default()),
    );
    model.variables.get_mut(&VarName::new("a")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Boolean(true),
        span: source.span("true", 0),
    });
    model.variables.get_mut(&VarName::new("b")).unwrap().binding = Some(Expression::Literal {
        value: Literal::Boolean(false),
        span: source.span("false", 0),
    });
    let connection_span = source.span("connect(a, b)", 0);
    model.add_equation(connection_equation(
        Expression::VarRef {
            name: test_reference("a"),
            subscripts: Vec::new(),
            span: connection_span,
        },
        Expression::VarRef {
            name: test_reference("b"),
            subscripts: Vec::new(),
            span: connection_span,
        },
        connection_span,
    ));

    assert!(matches!(
        construct(&model, source.map),
        Err(ToDaeError::DiscreteSolvedFormViolation { detail, .. })
            if detail.contains("more than one semantic definition owner")
    ));
}

fn indexed_output_connection_model(source: &TestSource, output_first: bool) -> flat::Model {
    let mut model = test_model();
    add_connection_endpoint(
        &mut model,
        source,
        "source",
        "output Boolean source[2]",
        8,
        vec![2],
        Causality::Output(Default::default()),
    );
    add_connection_endpoint(
        &mut model,
        source,
        "sink",
        "input Boolean sink",
        9,
        Vec::new(),
        Causality::Input(Default::default()),
    );
    let output = Expression::VarRef {
        name: test_reference("source"),
        subscripts: vec![Subscript::Index {
            value: 1,
            span: source.span("source[1]", 0),
        }],
        span: source.span("source[1]", 0),
    };
    let input = Expression::VarRef {
        name: test_reference("sink"),
        subscripts: Vec::new(),
        span: source.span("sink", 1),
    };
    let (lhs, rhs) = if output_first {
        (output, input)
    } else {
        (input, output)
    };
    let connection_span = if output_first {
        source.span("connect(source[1], sink)", 0)
    } else {
        source.span("connect(sink, source[1])", 0)
    };
    model.add_equation(connection_equation(lhs, rhs, connection_span));
    model
}

fn add_connection_endpoint(
    model: &mut flat::Model,
    source: &TestSource,
    name: &str,
    declaration: &str,
    type_id: u32,
    dimensions: Vec<i64>,
    causality: Causality,
) {
    add_primitive_variable(model, source, name, declaration, type_id, dimensions, true);
    let variable = model.variables.get_mut(&VarName::new(name)).unwrap();
    variable.causality = causality;
    variable.component_ref = Some(test_component_reference(name, source.span(declaration, 0)));
}

fn connection_equation(lhs: Expression, rhs: Expression, span: Span) -> flat::Equation {
    flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        },
        span,
        flat::EquationOrigin::Connection {
            lhs: String::new(),
            rhs: String::new(),
        },
    )
}

#[test]
fn b1c_topology_orders_targets_inside_one_atomic_owner() {
    let source = TestSource::new(
        "model M discrete Boolean a; discrete Boolean z; equation \
         when true then a = z; z = true; end when; end M;",
    );
    let model = atomic_when_dependency_model(&source, false);

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(owner.targets().len(), 2);
        assert_eq!(owner.targets().get(0).unwrap().index(), 1);
        assert_eq!(owner.targets().get(1).unwrap().index(), 0);
    });
}

#[test]
fn b1c_topology_rejects_internal_atomic_owner_cycles_at_action_span() {
    let source = TestSource::new(
        "model M discrete Boolean a; discrete Boolean z; equation \
         when true then a = z; z = a; end when; end M;",
    );
    let first_action = source.span("a = z", 0);
    let model = atomic_when_dependency_model(&source, true);

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if detail.contains("internal current-value dependency cycle")
                && span == first_action
    ));
}

#[test]
fn b1c_topology_rejects_current_value_cycles_before_construction() {
    let source = TestSource::new("model M discrete Boolean a = z; discrete Boolean z = a; end M;");
    let mut model = test_model();
    for (name, declaration, type_id, dependency, occurrence) in [
        ("a", "discrete Boolean a", 8, "z", 0),
        ("z", "discrete Boolean z", 9, "a", 1),
    ] {
        add_primitive_variable(
            &mut model,
            &source,
            name,
            declaration,
            type_id,
            Vec::new(),
            true,
        );
        model
            .variables
            .get_mut(&VarName::new(name))
            .unwrap()
            .binding = Some(Expression::VarRef {
            name: Reference::new(dependency),
            subscripts: Vec::new(),
            span: source.span(dependency, occurrence),
        });
    }

    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, .. }
            if detail.contains("current-value dependency cycle")
    ));
}

#[test]
fn unassigned_discrete_value_has_explicit_generated_hold_owner() {
    let source = TestSource::new("model M discrete Boolean m; end M;");
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(
            owner.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::DiscreteUpdate)
        );
        let branch = owner.branches().get(0).unwrap();
        assert!(matches!(
            branch.activation(),
            dae::DiscreteBranchActivation::Always
        ));
        let value = view.expression(branch.values().get(0).unwrap().0).unwrap();
        assert!(matches!(
            value.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::PreDiscreteValue(_))
        ));
    });
}

#[test]
fn initial_pre_equation_uses_checked_discrete_initial_value_owner() {
    let source =
        TestSource::new("model M discrete Boolean m; initial equation pre(m) = true; end M;");
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );
    let equation_span = source.span("pre(m) = true", 0);
    model.initial_equations.push(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![variable_reference(&source, "m", "m", 1, Vec::new())],
                span: source.span("pre(m)", 0),
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.initialization_owner_count(), 0);
        assert_eq!(view.initial_discrete_value_count(), 1);
        let definition = view.initial_discrete_value(0).unwrap();
        assert_eq!(definition.target().index(), 0);
        assert_eq!(definition.provenance().span(), equation_span);
        assert!(matches!(
            view.expression(definition.value()).unwrap().operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(true))
        ));
    });
}

#[test]
fn coupled_initial_discrete_real_definition_remains_a_numeric_initialization_row() {
    let source = TestSource::new(
        "model M discrete Real d; input Real u; initial equation d = u; equation d = u; end M;",
    );
    let mut model = test_model();
    let mut target = flat::Variable::empty_with_span(source.span("discrete Real d", 0));
    target.name = VarName::new("d");
    target.instance_id = test_instance_id("d");
    target.type_id = rumoca_core::TypeId::new(8);
    target.variability = Variability::Discrete(Default::default());
    target.is_primitive = true;
    register_test_real_type(&mut model, target.type_id, &target.dims);
    model.add_variable(target.name.clone(), target);
    model
        .variable_type_names
        .insert(VarName::new("d"), "Real".to_string());
    add_primitive_variable(
        &mut model,
        &source,
        "u",
        "input Real u",
        9,
        Vec::new(),
        false,
    );
    let input = model.variables.get_mut(&VarName::new("u")).unwrap();
    input.causality = Causality::Input(Default::default());
    input.component_ref = Some(test_component_reference(
        "u",
        source.span("input Real u", 0),
    ));
    model.top_level_input_components.insert("u".to_string());
    let equation = |occurrence| {
        let span = source.span("d = u", occurrence);
        flat::Equation::new(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(variable_reference(
                    &source,
                    "d",
                    "d = u",
                    occurrence,
                    Vec::new(),
                )),
                rhs: Box::new(variable_reference(
                    &source,
                    "u",
                    "d = u",
                    occurrence,
                    Vec::new(),
                )),
                span,
            },
            span,
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        )
    };
    model.initial_equations.push(equation(0));
    model.equations.push(equation(1));

    let dae = construct(&model, source.map).unwrap();
    dae.inspect(|view| {
        assert_eq!(view.initial_discrete_value_count(), 0);
        assert_eq!(view.initialization_owner_count(), 1);
    });
}

#[test]
fn malformed_flat_when_branch_rejects_direct_duplicate_at_second_definition() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; m = false; end when; end M;",
    );
    let second_span = source.span("m = false", 0);
    let model = duplicate_when_model(&source, false);
    let error = construct(&model, source.map).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == second_span && detail.contains("`m`")
    ));
}

#[test]
fn malformed_flat_when_branch_rejects_nested_duplicate_at_inner_definition() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; if true then m = false; end if; end when; end M;",
    );
    let second_span = source.span("m = false", 0);
    let model = duplicate_when_model(&source, true);
    let error = construct(&model, source.map).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == second_span && detail.contains("`m`")
    ));
}

#[test]
fn malformed_flat_rejects_second_independent_when_owner_for_one_target() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; end when; \
         when false then m = false; end when; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "m",
        "discrete Boolean m",
        8,
        Vec::new(),
        true,
    );
    for (condition, assignment, owner, value) in [
        (
            "when true",
            "m = true",
            "when true then m = true; end when",
            true,
        ),
        (
            "when false",
            "m = false",
            "when false then m = false; end when",
            false,
        ),
    ] {
        let condition_span = source.span(condition, 0);
        let assignment_span = source.span(assignment, 0);
        let mut branch = flat::WhenBranch::new(
            Expression::Literal {
                value: Literal::Boolean(value),
                span: condition_span,
            },
            condition_span,
        );
        branch.add_equation(flat::WhenEquation::assign(
            VarName::new("m"),
            Expression::Literal {
                value: Literal::Boolean(value),
                span: assignment_span,
            },
            assignment_span,
            assignment,
        ));
        model
            .when_chains
            .push(flat::WhenChain::new(branch, source.span(owner, 0)));
    }
    let second_owner = source.span("when false then m = false; end when", 0);
    let error = construct(&model, source.map).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == second_owner && detail.contains("`m`")
    ));
}

#[test]
fn nested_duplicate_diagnostic_follows_source_insertion_order() {
    let source = TestSource::new(
        "model M discrete Boolean z; discrete Boolean a; equation \
         when true then z = true; a = true; \
         if true then z = false; a = false; end if; end when; end M;",
    );
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        &source,
        "z",
        "discrete Boolean z",
        8,
        Vec::new(),
        true,
    );
    add_primitive_variable(
        &mut model,
        &source,
        "a",
        "discrete Boolean a",
        9,
        Vec::new(),
        true,
    );
    let owner_span = source.span("when true", 0);
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: owner_span,
        },
        owner_span,
    );
    for (target, assignment) in [("z", "z = true"), ("a", "a = true")] {
        let span = source.span(assignment, 0);
        branch.add_equation(flat::WhenEquation::assign(
            VarName::new(target),
            Expression::Literal {
                value: Literal::Boolean(true),
                span,
            },
            span,
            assignment,
        ));
    }
    let z_second = source.span("z = false", 0);
    let a_second = source.span("a = false", 0);
    branch.add_equation(flat::WhenEquation::conditional(
        vec![(
            Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("if true", 0),
            },
            vec![
                flat::WhenEquation::assign(
                    VarName::new("z"),
                    Expression::Literal {
                        value: Literal::Boolean(false),
                        span: z_second,
                    },
                    z_second,
                    "z second",
                ),
                flat::WhenEquation::assign(
                    VarName::new("a"),
                    Expression::Literal {
                        value: Literal::Boolean(false),
                        span: a_second,
                    },
                    a_second,
                    "a second",
                ),
            ],
        )],
        None,
        source.span("if true then z = false; a = false; end if", 0),
        "source-order duplicates",
    ));
    model
        .when_chains
        .push(flat::WhenChain::new(branch, owner_span));
    let error = construct(&model, source.map).unwrap_err();

    assert!(matches!(
        error,
        ToDaeError::DiscreteSolvedFormViolation { detail, span }
            if span == z_second && detail.contains("`z`")
    ));
}

#[test]
fn when_assert_level_reaches_checked_event_action_with_exact_provenance() {
    let source = TestSource::new(
        "model M equation when true then assert(false, \"failed\", 2); end when; end M;",
    );
    let condition_span = source.span("true", 0);
    let assertion_span = source.span("assert(false, \"failed\", 2)", 0);
    let level_span = source.span("2", 0);
    let mut branch = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
            span: condition_span,
        },
        condition_span,
    );
    branch.add_equation(flat::WhenEquation::assert(
        Expression::Literal {
            value: Literal::Boolean(false),
            span: source.span("false", 0),
        },
        Expression::Literal {
            value: Literal::String("failed".to_string()),
            span: source.span("\"failed\"", 0),
        },
        Some(Expression::Literal {
            value: Literal::Integer(2),
            span: level_span,
        }),
        assertion_span,
        "assert in when-clause",
    ));
    let chain = flat::WhenChain::new(
        branch,
        source.span("when true then assert(false, \"failed\", 2); end when", 0),
    );
    let mut model = test_model();
    model.when_chains.push(chain);
    let dae = construct(&model, source.map).unwrap();

    dae.inspect(|view| {
        assert_eq!(view.event_action_count(), 1);
        let action = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        assert_eq!(action.provenance().span(), assertion_span);
        assert_eq!(
            view.source_text(action.provenance()),
            Some("assert(false, \"failed\", 2)")
        );
        let dae::EventActionOperation::Assert {
            message,
            level: Some(level),
        } = action.operation()
        else {
            panic!("checked event assertion must own its optional level");
        };
        assert_eq!(
            view.source_text(view.expression(message).unwrap().provenance()),
            Some("\"failed\"")
        );
        let level = view.expression(level).unwrap();
        assert_eq!(level.provenance().span(), level_span);
        assert_eq!(view.source_text(level.provenance()), Some("2"));
        assert!(matches!(
            level.operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(2))
        ));
    });
}
