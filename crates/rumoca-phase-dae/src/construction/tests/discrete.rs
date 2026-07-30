use rumoca_core::Reference;

use super::super::*;
use super::support::*;

fn source_priority_when_model(source: &TestSource) -> flat::Model {
    let first_condition_span = source.span("true", 0);
    let second_condition_span = source.span("true", 2);
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
        Expression::Literal {
            value: Literal::Boolean(true),
            span: first_condition_span,
        },
        first_condition_span,
    );
    first.add_equation(flat::WhenEquation::assign(
        VarName::new("m"),
        Expression::Literal {
            value: Literal::Boolean(true),
            span: source.span("true", 1),
        },
        first_assignment_span,
        "first branch",
    ));
    let mut second = flat::WhenBranch::new(
        Expression::Literal {
            value: Literal::Boolean(true),
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
            "when true then m = true; elsewhen true then m = false; end when",
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

#[test]
fn when_chain_lowers_source_priority_with_exact_branch_provenance() {
    let source = TestSource::new(
        "model M discrete Boolean m; equation \
         when true then m = true; elsewhen true then m = false; end when; end M;",
    );
    let second_condition_span = source.span("true", 2);
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
            Some("m = true")
        );
        assert_eq!(
            view.source_text(second.values().get(0).unwrap().1),
            Some("m = false")
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
        assert_eq!(first_guard, first_trigger);
        assert_ne!(second_guard, second_trigger);

        let guard = view.condition(second_guard).unwrap();
        assert_eq!(guard.provenance().span(), second_condition_span);
        assert_eq!(
            guard.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::ConditionLowering)
        );
        let dae::ConditionOperation::And(branch_trigger, no_previous) = guard.operation() else {
            panic!("later branch guard must combine its trigger with source priority");
        };
        assert_eq!(branch_trigger, second_trigger);
        let negated = view.condition(no_previous).unwrap();
        assert_eq!(negated.provenance().span(), second_condition_span);
        assert_eq!(
            negated.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::ConditionLowering)
        );
        assert!(matches!(
            negated.operation(),
            dae::ConditionOperation::Not(previous) if previous == first_trigger
        ));
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
