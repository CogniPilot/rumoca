use rumoca_core::{ClockLattice, ClockRational, SourceId, SourceMap, Span, TypeId, VarName};

use super::*;

fn at(source: SourceId, text: &str, needle: &str) -> dae::DaeProvenance {
    let start = text.find(needle).expect("test source contains snippet");
    dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
        .expect("test provenance is exact")
}

fn enclosed(source: SourceId, text: &str, first: &str, last: &str) -> dae::DaeProvenance {
    let start = text.find(first).expect("test source contains owner start");
    let end = text.find(last).expect("test source contains owner end") + last.len();
    dae::DaeProvenance::source(Span::from_offsets(source, start, end))
        .expect("test owner provenance is exact")
}

fn project(model: &dae::Dae) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let classified =
            classify_variables(view, &definitions).expect("test variables are classifiable");
        let by_id = classified
            .iter()
            .map(|variable| (variable.id.index(), variable.clone()))
            .collect::<HashMap<_, _>>();
        let clock = (0..view.clock_count())
            .filter_map(|index| view.clock_id(index))
            .find(|clock| {
                matches!(
                    view.clock(*clock).map(dae::ClockView::operation),
                    Some(dae::ClockOperation::Periodic(_))
                )
            })
            .expect("test has one periodic clock");
        let pre_names = HashMap::new();
        lower_clocked_assignments(
            BlockLowering {
                view,
                definitions: &definitions,
                by_id: &by_id,
                pre_names: &pre_names,
                policy: EmissionPolicy::reviewable(),
            },
            clock,
        )
        .map(|assignments| assignments.statements)
    })
}

fn periodic_clock<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ClockId<'dae>, dae::DaeConstructionError> {
    dae.clocks(|clocks| {
        clocks.periodic(
            ClockLattice::new(ClockRational::ONE, ClockRational::ZERO)
                .expect("test lattice is valid"),
            provenance,
        )
    })
    .map(Into::into)
}

fn define_real_equation<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    provenance: dae::DaeProvenance,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    dae.discrete(|discrete| {
        discrete.real_equation(provenance, |equation| {
            equation.equal(lhs, rhs)?;
            Ok(())
        })
    })?;
    Ok(())
}

fn define_when_real_equation<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    trigger: dae::ConditionId<'dae>,
    guard: dae::ConditionId<'dae>,
    provenance: dae::DaeProvenance,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    dae.discrete(|discrete| {
        discrete.when_real_equation(trigger, guard, provenance, |equation| {
            equation.equal(lhs, rhs)?;
            Ok(())
        })
    })?;
    Ok(())
}

fn assignment_target(statement: &gast::Spanned<gast::Statement>) -> &str {
    let gast::Statement::Assignment {
        target: gast::Reference::State(parts),
        ..
    } = &statement.node
    else {
        panic!("expected one state assignment")
    };
    parts
        .first()
        .expect("checked state reference is nonempty")
        .name
        .lexeme()
}

#[derive(Clone, Copy)]
struct AtomicOwnerSpans {
    m_declaration: dae::DaeProvenance,
    n_declaration: dae::DaeProvenance,
    owner: dae::DaeProvenance,
    first_branch: dae::DaeProvenance,
    second_branch: dae::DaeProvenance,
    m_true: dae::DaeProvenance,
    n_false: dae::DaeProvenance,
    m_false: dae::DaeProvenance,
    n_true: dae::DaeProvenance,
}

struct AtomicOwnerValues<'dae> {
    first_condition: dae::ExprId<'dae>,
    second_condition: dae::ExprId<'dae>,
    first_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
    second_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
}

struct AtomicOwnerDefinition<'dae> {
    targets: [dae::DiscreteValueId<'dae>; 2],
    first: dae::ConditionId<'dae>,
    second: dae::ConditionId<'dae>,
    first_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
    second_values: [(dae::ExprId<'dae>, dae::DaeProvenance); 2],
    spans: AtomicOwnerSpans,
}

fn atomic_owner_spans(source: SourceId, text: &str) -> AtomicOwnerSpans {
    AtomicOwnerSpans {
        m_declaration: at(source, text, "discrete Boolean m"),
        n_declaration: at(source, text, "discrete Boolean n"),
        owner: enclosed(source, text, "when sample(0, 1)", "end when"),
        first_branch: at(source, text, "sample(0, 1) and a"),
        second_branch: at(source, text, "sample(0, 1) and b"),
        m_true: at(source, text, "m = true"),
        n_false: at(source, text, "n = false"),
        m_false: at(source, text, "m = false"),
        n_true: at(source, text, "n = true"),
    }
}

fn atomic_owner_values<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    spans: AtomicOwnerSpans,
) -> Result<AtomicOwnerValues<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        Ok(AtomicOwnerValues {
            first_condition: expressions
                .at(spans.first_branch)
                .literal(dae::DaeLiteral::Boolean(true))?,
            second_condition: expressions
                .at(spans.second_branch)
                .literal(dae::DaeLiteral::Boolean(true))?,
            first_values: [
                (
                    expressions
                        .at(spans.m_true)
                        .literal(dae::DaeLiteral::Boolean(true))?,
                    spans.m_true,
                ),
                (
                    expressions
                        .at(spans.n_false)
                        .literal(dae::DaeLiteral::Boolean(false))?,
                    spans.n_false,
                ),
            ],
            second_values: [
                (
                    expressions
                        .at(spans.m_false)
                        .literal(dae::DaeLiteral::Boolean(false))?,
                    spans.m_false,
                ),
                (
                    expressions
                        .at(spans.n_true)
                        .literal(dae::DaeLiteral::Boolean(true))?,
                    spans.n_true,
                ),
            ],
        })
    })
}

fn atomic_owner_conditions<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    clock: dae::ClockId<'dae>,
    values: &AtomicOwnerValues<'dae>,
    spans: AtomicOwnerSpans,
) -> Result<(dae::ConditionId<'dae>, dae::ConditionId<'dae>), dae::DaeConstructionError> {
    dae.conditions(|conditions| {
        let tick = conditions.reserve(spans.first_branch)?;
        conditions.define(tick, dae::ConditionInput::Clock(clock), spans.first_branch)?;
        let first_condition = conditions.reserve(spans.first_branch)?;
        conditions.define(
            first_condition,
            dae::ConditionInput::Discrete(values.first_condition),
            spans.first_branch,
        )?;
        let second_condition = conditions.reserve(spans.second_branch)?;
        conditions.define(
            second_condition,
            dae::ConditionInput::Discrete(values.second_condition),
            spans.second_branch,
        )?;
        let first = conditions.reserve(spans.first_branch)?;
        conditions.define(
            first,
            dae::ConditionInput::And(tick, first_condition),
            spans.first_branch,
        )?;
        let second = conditions.reserve(spans.second_branch)?;
        conditions.define(
            second,
            dae::ConditionInput::And(tick, second_condition),
            spans.second_branch,
        )?;
        Ok((first, second))
    })
}

fn define_atomic_owner<'dae>(
    topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
    definition: AtomicOwnerDefinition<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    topology.owner(definition.spans.owner, definition.targets, |owner| {
        owner.when(
            definition.first,
            definition.first,
            definition.spans.first_branch,
            definition.first_values,
        )?;
        owner.when(
            definition.second,
            definition.second,
            definition.spans.second_branch,
            definition.second_values,
        )
    })?;
    Ok(())
}

fn atomic_owner_model(
    sources: SourceMap,
    spans: AtomicOwnerSpans,
) -> Result<dae::Dae, dae::DaeConstructionError> {
    dae::Dae::construct(sources, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                spans.m_declaration,
            )
        })?;
        let (m, n) = dae.variables(|variables| {
            Ok((
                variables.discrete_value(
                    VarName::new("m"),
                    boolean,
                    spans.m_declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("n"),
                    boolean,
                    spans.n_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let values = atomic_owner_values(dae, spans)?;
        let clock = periodic_clock(dae, spans.first_branch)?;
        let (first, second) = atomic_owner_conditions(dae, clock, &values, spans)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_value(clock, m, spans.owner)?;
            clocks.own_discrete_value(clock, n, spans.owner)?;
            Ok(())
        })?;
        dae.b1c([m, n], |topology| {
            define_atomic_owner(
                topology,
                AtomicOwnerDefinition {
                    targets: [m, n],
                    first,
                    second,
                    first_values: values.first_values,
                    second_values: values.second_values,
                    spans,
                },
            )
        })
    })
}

fn assert_atomic_owner_projection(
    statements: &[gast::Spanned<gast::Statement>],
    spans: AtomicOwnerSpans,
) {
    assert_eq!(statements.len(), 1);
    assert_eq!(statements[0].span, spans.owner.span());
    let gast::Statement::If(conditional) = &statements[0].node else {
        panic!("conditional B.1c owner must remain atomic")
    };
    assert_eq!(conditional.branches.len(), 2);
    assert_eq!(conditional.branches[0].span, spans.first_branch.span());
    assert_eq!(conditional.branches[1].span, spans.second_branch.span());
    assert_eq!(
        conditional.branches[0]
            .body
            .iter()
            .map(assignment_target)
            .collect::<Vec<_>>(),
        ["m", "n"]
    );
    assert_eq!(
        conditional.branches[0]
            .body
            .iter()
            .map(|statement| statement.span)
            .collect::<Vec<_>>(),
        [spans.m_true.span(), spans.n_false.span()]
    );
    assert_eq!(
        conditional.branches[1]
            .body
            .iter()
            .map(|statement| statement.span)
            .collect::<Vec<_>>(),
        [spans.m_false.span(), spans.n_true.span()]
    );
}

fn define_always_owner<'dae>(
    topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
    target: dae::DiscreteValueId<'dae>,
    value: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    topology.owner(provenance, [target], |owner| {
        owner.always(provenance, [(value, provenance)])
    })?;
    Ok(())
}

fn define_when_owner<'dae>(
    topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
    target: dae::DiscreteValueId<'dae>,
    condition: dae::ConditionId<'dae>,
    branch: dae::DaeProvenance,
    value: dae::ExprId<'dae>,
    action: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    topology.owner(branch, [target], |owner| {
        owner.when(condition, condition, branch, [(value, action)])
    })?;
    Ok(())
}

#[test]
fn unconditional_owner_lowers_directly_with_action_provenance() {
    let text = "discrete Boolean m; m = true; sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("unconditional.mo", text);
    let declaration = at(source, text, "discrete Boolean m");
    let action = at(source, text, "m = true");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let m = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let value = dae.expressions(|expressions| {
            expressions
                .at(action)
                .literal(dae::DaeLiteral::Boolean(true))
        })?;
        periodic_clock(dae, clock_at)?;
        dae.b1c([m], |topology| {
            topology.owner(action, [m], |owner| owner.always(action, [(value, action)]))?;
            Ok(())
        })
    })
    .expect("checked unconditional B.1c fixture");

    let statements = project(&model).expect("unconditional owner projects");
    assert_eq!(statements.len(), 1);
    assert_eq!(statements[0].span, action.span());
    assert_eq!(assignment_target(&statements[0]), "m");
    assert!(matches!(
        &statements[0].node,
        gast::Statement::Assignment {
            value: gast::Expression::Bool(true),
            ..
        }
    ));
}

struct MaterializedGuardDefinition<'dae> {
    target: dae::DiscreteValueId<'dae>,
    first: dae::ConditionId<'dae>,
    second: dae::ConditionId<'dae>,
    true_value: dae::ExprId<'dae>,
    false_value: dae::ExprId<'dae>,
    owner: dae::DaeProvenance,
    first_branch: dae::DaeProvenance,
    second_branch: dae::DaeProvenance,
    first_action: dae::DaeProvenance,
    second_action: dae::DaeProvenance,
}

fn define_materialized_guard_owner<'dae>(
    topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
    definition: MaterializedGuardDefinition<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    topology.owner(definition.owner, [definition.target], |owner| {
        owner.when(
            definition.first,
            definition.first,
            definition.first_branch,
            [(definition.true_value, definition.first_action)],
        )?;
        owner.when(
            definition.second,
            definition.second,
            definition.second_branch,
            [(definition.false_value, definition.second_action)],
        )
    })?;
    Ok(())
}

fn assert_materialized_guard_projection(statements: &[gast::Spanned<gast::Statement>]) {
    assert_eq!(statements.len(), 2);
    assert!(matches!(statements[0].node, gast::Statement::If(_)));
    let gast::Statement::If(first_owner_branch) = &statements[1].node else {
        panic!("the first materialized guard must precede its owner branch")
    };
    let first_else = first_owner_branch
        .else_body
        .as_ref()
        .expect("the second guard is evaluated only after the first is false");
    assert_eq!(first_else.len(), 2);
    assert!(matches!(first_else[0].node, gast::Statement::If(_)));
    assert!(matches!(first_else[1].node, gast::Statement::If(_)));
}

fn materialized_guard_branches(source: SourceId, text: &str) -> [dae::DaeProvenance; 2] {
    [
        at(
            source,
            text,
            "sample(0, 1) and (if true then true else false)",
        ),
        at(
            source,
            text,
            "sample(0, 1) and (if false then true else true)",
        ),
    ]
}

#[test]
fn materialized_elsewhen_guards_dominate_their_uses_lazily() {
    let text = "discrete Boolean m; when sample(0, 1) and (if true then true else false) then m = true; elsewhen sample(0, 1) and (if false then true else true) then m = false; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("materialized-guard.mo", text);
    let declaration = at(source, text, "discrete Boolean m");
    let [first_branch, second_branch] = materialized_guard_branches(source, text);
    let first_action = at(source, text, "m = true");
    let second_action = at(source, text, "m = false");
    let owner_span = enclosed(source, text, "when sample(0, 1)", "end when");
    let model = dae::Dae::construct(sources, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let target = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (first_guard, second_guard, true_value, false_value) =
            dae.expressions(|expressions| {
                let true_value = expressions
                    .at(first_branch)
                    .literal(dae::DaeLiteral::Boolean(true))?;
                let false_value = expressions
                    .at(second_branch)
                    .literal(dae::DaeLiteral::Boolean(false))?;
                let first_guard = expressions
                    .at(first_branch)
                    .conditional([(true_value, true_value)], false_value)?;
                let second_guard = expressions
                    .at(second_branch)
                    .conditional([(false_value, true_value)], true_value)?;
                Ok((first_guard, second_guard, true_value, false_value))
            })?;
        let clock = periodic_clock(dae, first_branch)?;
        let (first, second) = dae.conditions(|conditions| {
            let tick = conditions.reserve(first_branch)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), first_branch)?;
            let first_guard_condition = conditions.reserve(first_branch)?;
            conditions.define(
                first_guard_condition,
                dae::ConditionInput::Discrete(first_guard),
                first_branch,
            )?;
            let second_guard_condition = conditions.reserve(second_branch)?;
            conditions.define(
                second_guard_condition,
                dae::ConditionInput::Discrete(second_guard),
                second_branch,
            )?;
            let first = conditions.reserve(first_branch)?;
            conditions.define(
                first,
                dae::ConditionInput::And(tick, first_guard_condition),
                first_branch,
            )?;
            let second = conditions.reserve(second_branch)?;
            conditions.define(
                second,
                dae::ConditionInput::And(tick, second_guard_condition),
                second_branch,
            )?;
            Ok((first, second))
        })?;
        dae.clocks(|clocks| {
            clocks.own_discrete_value(clock, target, owner_span)?;
            Ok(())
        })?;
        dae.b1c([target], |topology| {
            define_materialized_guard_owner(
                topology,
                MaterializedGuardDefinition {
                    target,
                    first,
                    second,
                    true_value,
                    false_value,
                    owner: owner_span,
                    first_branch,
                    second_branch,
                    first_action,
                    second_action,
                },
            )
        })
    })
    .expect("checked materialized-guard B.1c fixture");

    let statements = project(&model).expect("materialized guards project");
    assert_materialized_guard_projection(&statements);
}

#[test]
fn ordered_multi_target_owner_is_atomic_and_wire_stable() {
    let text = "discrete Boolean m; discrete Boolean n; when sample(0, 1) and a then m = true; n = false; elsewhen sample(0, 1) and b then m = false; n = true; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("elsewhen.mo", text);
    let spans = atomic_owner_spans(source, text);
    let model = atomic_owner_model(sources, spans).expect("checked multi-target B.1c fixture");

    let statements = project(&model).expect("multi-target owner projects");
    assert_atomic_owner_projection(&statements, spans);
    let encoded = serde_json::to_string(&model).expect("wire encoding succeeds");
    let decoded: dae::Dae = serde_json::from_str(&encoded).expect("wire decoding succeeds");
    assert_eq!(
        statements,
        project(&decoded).expect("wire-decoded owner projects identically")
    );
}

#[test]
fn current_discrete_value_dependencies_keep_topological_owner_order() {
    let text = "discrete Boolean a; discrete Boolean b; a = true; b = a; sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("topology.mo", text);
    let a_declaration = at(source, text, "discrete Boolean a");
    let b_declaration = at(source, text, "discrete Boolean b");
    let a_action = at(source, text, "a = true");
    let b_action = at(source, text, "b = a");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                a_declaration,
            )
        })?;
        let (a, b) = dae.variables(|variables| {
            Ok((
                variables.discrete_value(
                    VarName::new("a"),
                    boolean,
                    a_declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("b"),
                    boolean,
                    b_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (true_value, a_value) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(a_action)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(b_action)
                    .coordinate(dae::CoordinateInput::DiscreteValue(a))?,
            ))
        })?;
        periodic_clock(dae, clock_at)?;
        dae.b1c([a, b], |topology| {
            define_always_owner(topology, a, true_value, a_action)?;
            define_always_owner(topology, b, a_value, b_action)?;
            Ok(())
        })
    })
    .expect("checked topological B.1c fixture");

    let statements = project(&model).expect("topological owners project");
    assert_eq!(
        statements.iter().map(assignment_target).collect::<Vec<_>>(),
        ["a", "b"]
    );
    assert_eq!(
        statements
            .iter()
            .map(|statement| statement.span)
            .collect::<Vec<_>>(),
        [a_action.span(), b_action.span()]
    );
}

#[test]
fn explicit_clocked_b1b_definition_lowers_with_equation_provenance() {
    let text = "discrete Real z; when sample(0, 1) then z = 1.0; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("clocked-real.mo", text);
    let declaration = at(source, text, "discrete Real z");
    let clock_at = at(source, text, "sample(0, 1)");
    let assignment = at(source, text, "z = 1.0");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let z = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (lhs, rhs) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(z))?,
                expressions
                    .at(assignment)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        let clock = periodic_clock(dae, clock_at)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock, z, declaration)?;
            Ok(())
        })?;
        let tick = dae.conditions(|conditions| {
            let tick = conditions.reserve(clock_at)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), clock_at)?;
            Ok(tick)
        })?;
        define_when_real_equation(dae, tick, tick, assignment, lhs, rhs)?;
        Ok(())
    })
    .expect("checked conditional B.1b fixture");

    let statements = project(&model).expect("explicit B.1b definition projects");
    assert_eq!(statements.len(), 1);
    assert_eq!(statements[0].span, assignment.span());
    assert_eq!(assignment_target(&statements[0]), "z");
    assert!(matches!(
        statements[0].node,
        gast::Statement::Assignment {
            value: gast::Expression::Real(value),
            ..
        } if value == 1.0
    ));
}

#[test]
fn conditional_real_value_work_remains_inside_its_runtime_guard() {
    let text = "discrete Real z; when sample(0, 1) and false then z = if true then 1.0 else 2.0; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("guarded-clocked-real.mo", text);
    let declaration = at(source, text, "discrete Real z");
    let clock_at = at(source, text, "sample(0, 1)");
    let guard_at = at(source, text, "sample(0, 1) and false");
    let assignment = at(source, text, "z = if true then 1.0 else 2.0");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let z = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (lhs, rhs, false_guard) = dae.expressions(|expressions| {
            let lhs = expressions
                .at(assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(z))?;
            let condition = expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Boolean(true))?;
            let one = expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let two = expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let rhs = expressions
                .at(assignment)
                .conditional([(condition, one)], two)?;
            let false_guard = expressions
                .at(guard_at)
                .literal(dae::DaeLiteral::Boolean(false))?;
            Ok((lhs, rhs, false_guard))
        })?;
        let clock = periodic_clock(dae, clock_at)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock, z, declaration)?;
            Ok(())
        })?;
        let guard = dae.conditions(|conditions| {
            let tick = conditions.reserve(clock_at)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), clock_at)?;
            let predicate = conditions.reserve(guard_at)?;
            conditions.define(
                predicate,
                dae::ConditionInput::Discrete(false_guard),
                guard_at,
            )?;
            let guard = conditions.reserve(guard_at)?;
            conditions.define(guard, dae::ConditionInput::And(tick, predicate), guard_at)?;
            Ok(guard)
        })?;
        define_when_real_equation(dae, guard, guard, assignment, lhs, rhs)
    })
    .expect("checked guarded B.1b fixture");

    let statements = project(&model).expect("guarded B.1b definition projects");
    assert_eq!(statements.len(), 1);
    let gast::Statement::If(guarded) = &statements[0].node else {
        panic!("a runtime B.1b guard must dominate all value work")
    };
    assert_eq!(guarded.branches[0].body.len(), 2);
    assert!(matches!(
        guarded.branches[0].body[0].node,
        gast::Statement::If(_)
    ));
    assert!(matches!(
        guarded.branches[0].body[1].node,
        gast::Statement::Assignment { .. }
    ));
}

#[test]
fn generated_connection_alias_does_not_compete_with_its_clocked_real_owner() {
    let text = "discrete Real z; Real connectorValue; when sample(0, 1) then z = 1.0; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("clocked-real-connection.mo", text);
    let declaration = at(source, text, "discrete Real z");
    let connector_declaration = at(source, text, "Real connectorValue");
    let clock_at = at(source, text, "sample(0, 1)");
    let assignment = at(source, text, "z = 1.0");
    let connection = dae::DaeProvenance::generated(
        dae::DaeGeneration::ConnectionEquation,
        connector_declaration.span(),
    )
    .expect("connection provenance is source-backed");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let (z, connector_value) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("z"),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("connectorValue"),
                    real,
                    connector_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (z_connection, connector, z_assignment, one) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(connection)
                    .coordinate(dae::CoordinateInput::DiscreteReal(z))?,
                expressions
                    .at(connection)
                    .coordinate(dae::CoordinateInput::Algebraic(connector_value))?,
                expressions
                    .at(assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(z))?,
                expressions
                    .at(assignment)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        define_real_equation(dae, connection, z_connection, connector)?;
        let clock = periodic_clock(dae, clock_at)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock, z, declaration)?;
            Ok(())
        })?;
        let tick = dae.conditions(|conditions| {
            let tick = conditions.reserve(clock_at)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), clock_at)?;
            Ok(tick)
        })?;
        define_when_real_equation(dae, tick, tick, assignment, z_assignment, one)
    })
    .expect("checked clocked connection fixture");

    let statements = project(&model).expect("connection alias is eliminated causally");
    assert_eq!(statements.len(), 1);
    assert_eq!(statements[0].span, assignment.span());
}

#[test]
fn periodic_interval_coordinate_lowers_to_owning_lattice_period() {
    let text = "discrete Real z; z = interval(u); Clock(1, 8);";
    let mut sources = SourceMap::new();
    let source = sources.add("clock-interval.mo", text);
    let declaration = at(source, text, "discrete Real z");
    let equation_at = at(source, text, "z = interval(u)");
    let interval_at = at(source, text, "interval(u)");
    let clock_at = at(source, text, "Clock(1, 8)");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let z = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = dae.clocks(|clocks| {
            clocks.periodic(
                ClockLattice::new(
                    ClockRational::new(1, 8).expect("fixture period is exact"),
                    ClockRational::ZERO,
                )
                .expect("fixture lattice is valid"),
                clock_at,
            )
        })?;
        let (lhs, rhs) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(equation_at)
                    .coordinate(dae::CoordinateInput::DiscreteReal(z))?,
                expressions
                    .at(interval_at)
                    .coordinate(dae::CoordinateInput::ClockInterval(clock))?,
            ))
        })?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock.into(), z, declaration)?;
            Ok(())
        })?;
        define_real_equation(dae, equation_at, lhs, rhs)
    })
    .expect("checked interval fixture constructs");

    let statements = project(&model).expect("periodic interval projects");
    assert_eq!(statements.len(), 1);
    assert_eq!(statements[0].span, equation_at.span());
    assert_eq!(assignment_target(&statements[0]), "z");
    assert!(matches!(
        statements[0].node,
        gast::Statement::Assignment {
            value: gast::Expression::Real(value),
            ..
        } if value == 0.125
    ));
}

#[test]
fn b1b_residual_pre_reads_materialize_the_previous_state() {
    let text = "discrete Real z; z = pre(z); sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("previous-real.mo", text);
    let declaration = at(source, text, "discrete Real z");
    let equation_at = at(source, text, "z = pre(z)");
    let previous_at = at(source, text, "pre(z)");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let z = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = periodic_clock(dae, clock_at)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock, z, declaration)?;
            Ok(())
        })?;
        let (lhs, rhs) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(equation_at)
                    .coordinate(dae::CoordinateInput::DiscreteReal(z))?,
                expressions
                    .at(previous_at)
                    .coordinate(dae::CoordinateInput::PreDiscreteReal(z))?,
            ))
        })?;
        define_real_equation(dae, equation_at, lhs, rhs)?;
        Ok(())
    })
    .expect("checked previous-value B.1b fixture");

    model.inspect(|view| {
        let referenced = referenced_pre_variables(view).expect("B.1b previous read is supported");
        assert_eq!(referenced.len(), 1);
        assert_eq!(referenced[0].index(), 0);
    });
}

#[test]
fn unowned_explicit_b1b_definition_fails_before_galec_lowering() {
    let text = "discrete Real z; z = 1.0; sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("unowned-real.mo", text);
    let declaration = at(source, text, "discrete Real z");
    let equation_at = at(source, text, "z = 1.0");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let z = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (lhs, rhs) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(equation_at)
                    .coordinate(dae::CoordinateInput::DiscreteReal(z))?,
                expressions
                    .at(equation_at)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        periodic_clock(dae, clock_at)?;
        define_real_equation(dae, equation_at, lhs, rhs)?;
        Ok(())
    })
    .expect("checked unowned B.1b fixture");

    let error = project(&model).expect_err("GALEC requires explicit clock ownership");
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "clock-domain" && span == equation_at.span()
    ));
}

#[test]
fn clocked_local_to_output_alias_is_oriented_and_ordered() {
    let text = "discrete Real source; discrete Real filtered; output discrete Real y; when sample(0, 1) then source = 1.0; filtered = source; y = filtered; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("output-alias.mo", text);
    let source_declaration = at(source, text, "discrete Real source");
    let filtered_declaration = at(source, text, "discrete Real filtered");
    let output_declaration = at(source, text, "output discrete Real y");
    let clock_at = at(source, text, "sample(0, 1)");
    let source_assignment = at(source, text, "source = 1.0");
    let filtered_assignment = at(source, text, "filtered = source");
    let output_assignment = at(source, text, "y = filtered");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                source_declaration,
            )
        })?;
        let (source_value, filtered, output) = dae.variables(|variables| {
            let source_value = variables.discrete_real(
                VarName::new("source"),
                real,
                source_declaration,
                dae::VariableAttributes::default(),
            )?;
            let filtered = variables.discrete_real(
                VarName::new("filtered"),
                real,
                filtered_declaration,
                dae::VariableAttributes::default(),
            )?;
            let output = variables.discrete_real(
                VarName::new("y"),
                real,
                output_declaration,
                dae::VariableAttributes {
                    causality: dae::VariableCausality::Output,
                    ..dae::VariableAttributes::default()
                },
            )?;
            Ok((source_value, filtered, output))
        })?;
        let (source_lhs, one, filtered_lhs, filtered_rhs, output_lhs, output_rhs) = dae
            .expressions(|expressions| {
                Ok((
                    expressions
                        .at(source_assignment)
                        .coordinate(dae::CoordinateInput::DiscreteReal(source_value))?,
                    expressions
                        .at(source_assignment)
                        .literal(dae::DaeLiteral::Real(1.0))?,
                    expressions
                        .at(filtered_assignment)
                        .coordinate(dae::CoordinateInput::DiscreteReal(filtered))?,
                    expressions
                        .at(filtered_assignment)
                        .coordinate(dae::CoordinateInput::DiscreteReal(source_value))?,
                    expressions
                        .at(output_assignment)
                        .coordinate(dae::CoordinateInput::DiscreteReal(output))?,
                    expressions
                        .at(output_assignment)
                        .coordinate(dae::CoordinateInput::DiscreteReal(filtered))?,
                ))
            })?;
        let clock = periodic_clock(dae, clock_at)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock, source_value, source_declaration)?;
            clocks.own_discrete_real(clock, filtered, filtered_declaration)?;
            clocks.own_discrete_real(clock, output, output_declaration)?;
            Ok(())
        })?;
        let tick = dae.conditions(|conditions| {
            let tick = conditions.reserve(clock_at)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), clock_at)?;
            Ok(tick)
        })?;
        define_when_real_equation(dae, tick, tick, source_assignment, source_lhs, one)?;
        define_when_real_equation(
            dae,
            tick,
            tick,
            filtered_assignment,
            filtered_lhs,
            filtered_rhs,
        )?;
        define_when_real_equation(dae, tick, tick, output_assignment, output_lhs, output_rhs)?;
        Ok(())
    })
    .expect("checked output alias fixture");

    let statements = project(&model).expect("output causality orients the boundary alias");
    assert_eq!(
        statements.iter().map(assignment_target).collect::<Vec<_>>(),
        ["source", "filtered", "y"]
    );
}

#[test]
fn coupled_b1b_residual_fails_closed_at_equation_provenance() {
    let text = "discrete Real z; discrete Real w; z + w = 1.0; sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("coupled-real.mo", text);
    let z_declaration = at(source, text, "discrete Real z");
    let w_declaration = at(source, text, "discrete Real w");
    let equation_at = at(source, text, "z + w = 1.0");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                z_declaration,
            )
        })?;
        let (z, w) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("z"),
                    real,
                    z_declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_real(
                    VarName::new("w"),
                    real,
                    w_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (lhs, rhs) = dae.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(z))?;
            let w = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(w))?;
            Ok((
                expressions
                    .at(equation_at)
                    .binary(dae::BinaryOperator::Add, z, w)?,
                expressions
                    .at(equation_at)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        periodic_clock(dae, clock_at)?;
        define_real_equation(dae, equation_at, lhs, rhs)?;
        Ok(())
    })
    .expect("checked coupled B.1b fixture");

    let error = project(&model).expect_err("coupled B.1b is not an assignment");
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "coupled-discrete-real-equation" && span == equation_at.span()
    ));
}

#[test]
fn unclocked_conditional_owner_is_rejected_at_its_source_span() {
    let text = "discrete Boolean m; when a then m = true; end when; sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("unclocked.mo", text);
    let declaration = at(source, text, "discrete Boolean m");
    let branch = at(source, text, "when a");
    let action = at(source, text, "m = true");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let m = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (condition_value, assigned_value) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(branch)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(action)
                    .literal(dae::DaeLiteral::Boolean(true))?,
            ))
        })?;
        let condition = dae.conditions(|conditions| {
            let condition = conditions.reserve(branch)?;
            conditions.define(
                condition,
                dae::ConditionInput::Discrete(condition_value),
                branch,
            )?;
            Ok(condition)
        })?;
        periodic_clock(dae, clock_at)?;
        dae.b1c([m], |topology| {
            define_when_owner(topology, m, condition, branch, assigned_value, action)?;
            Ok(())
        })
    })
    .expect("checked unclocked B.1c fixture");

    let error = project(&model).expect_err("unclocked condition is outside DoStep");
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "runtime-event-trigger" && span == branch.span()
    ));
}

/// Count `MultiAssignment` call statements naming `function`, at any nesting
/// depth, so a guarded projection counts the same as a bare one.
fn multi_assignment_call_sites(
    statements: &[gast::Spanned<gast::Statement>],
    function: &str,
) -> usize {
    statements
        .iter()
        .map(|statement| match &statement.node {
            gast::Statement::MultiAssignment { call, .. } => {
                usize::from(call.function.lexeme() == function)
            }
            gast::Statement::If(branching) => {
                branching
                    .branches
                    .iter()
                    .map(|branch| multi_assignment_call_sites(&branch.body, function))
                    .sum::<usize>()
                    + branching
                        .else_body
                        .as_ref()
                        .map_or(0, |body| multi_assignment_call_sites(body, function))
            }
            _ => 0,
        })
        .sum()
}

fn define_scalar_pair<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("pair"), [real], [real, real], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("input"), 0, provenance)
            })?;
            let first = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("first"), 0, provenance)
            })?;
            let second = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("second"), 1, provenance)
            })?;
            let input = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            let two = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(2.0))
            })?;
            let doubled = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Multiply, input, two)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            dae.functions(|functions| {
                functions.assign(&mut body, first, input, provenance)?;
                functions.assign(&mut body, second, doubled, provenance)?;
                functions.define(body, provenance)
            })
        },
    )
    .map(|(function, _)| function)
}

fn multi_output_clocked_fixture() -> dae::Dae {
    let text = "discrete Real a, b, c; when sample(0, 1) then c = 1.0; (a, b) = pair(c); end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("multi-output-clocked.mo", text);
    let declaration = at(source, text, "discrete Real a, b, c");
    let clock_at = at(source, text, "sample(0, 1)");
    let seed_assignment = at(source, text, "c = 1.0");
    let assignment = at(source, text, "(a, b) = pair(c)");
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let pair = define_scalar_pair(dae, real, declaration)?;
        let a = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("a"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let b = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("b"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let c = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("c"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let argument = dae.expressions(|expressions| {
            expressions
                .at(assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(c))
        })?;
        let results = dae.expressions(|expressions| {
            expressions
                .at(assignment)
                .call_results(pair, [0, 1], [argument])
        })?;
        let (a_ref, b_ref) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(a))?,
                expressions
                    .at(assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(b))?,
            ))
        })?;
        let (c_ref, seed) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(seed_assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(c))?,
                expressions
                    .at(seed_assignment)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        let clock = periodic_clock(dae, clock_at)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock, a, declaration)?;
            clocks.own_discrete_real(clock, b, declaration)?;
            clocks.own_discrete_real(clock, c, declaration)?;
            Ok(())
        })?;
        let tick = dae.conditions(|conditions| {
            let tick = conditions.reserve(clock_at)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), clock_at)?;
            Ok(tick)
        })?;
        define_when_real_equation(dae, tick, tick, seed_assignment, c_ref, seed)?;
        define_when_real_equation(dae, tick, tick, assignment, a_ref, results[0])?;
        define_when_real_equation(dae, tick, tick, assignment, b_ref, results[1])?;
        Ok(())
    })
    .expect("checked multi-output clocked fixture")
}

/// DAE-C21: the two result projections of one issued call owner are one
/// invocation. They must reach GALEC as one call binding both results, not as
/// one call per consumed output.
///
/// The call deliberately reads `c`, a discrete target this same `DoStep`
/// writes. That makes it ineligible for the domain-entry preamble hoist (a
/// hoisted call would read the previous tick's `c`), so the only thing that can
/// collapse the two projections is grouping them into one emission group. A
/// fixture whose call reads nothing would be hoisted instead and would stay
/// green with grouping disabled.
#[test]
fn two_projections_of_one_call_owner_emit_one_clocked_call() {
    let model = multi_output_clocked_fixture();
    let statements = project(&model).expect("multi-output clocked definitions project");
    assert_eq!(
        multi_assignment_call_sites(&statements, "pair"),
        1,
        "two projections of one issued call owner must emit one call"
    );
}

/// Every state assignment written by `statements`, as
/// `(target name, subscript count, source name)`.
///
/// `source name` is the referenced name for a bare reference and `<computed>`
/// otherwise, which is all these assertions need to tell one storage object
/// from another.
fn state_assignment_shapes(
    statements: &[gast::Spanned<gast::Statement>],
) -> Vec<(String, usize, String)> {
    let mut shapes = Vec::new();
    for statement in statements {
        match &statement.node {
            gast::Statement::Assignment {
                target: gast::Reference::State(parts),
                value,
            } => {
                let part = parts.first().expect("checked state reference is nonempty");
                let source = match value {
                    gast::Expression::Ref(gast::Reference::Local(source)) => {
                        source.name.lexeme().to_owned()
                    }
                    gast::Expression::Ref(gast::Reference::State(source)) => source
                        .first()
                        .expect("checked state source is nonempty")
                        .name
                        .lexeme()
                        .to_owned(),
                    _ => "<computed>".to_owned(),
                };
                shapes.push((part.name.lexeme().to_owned(), part.subscripts.len(), source));
            }
            gast::Statement::If(branching) => {
                for branch in &branching.branches {
                    shapes.extend(state_assignment_shapes(&branch.body));
                }
                if let Some(body) = &branching.else_body {
                    shapes.extend(state_assignment_shapes(body));
                }
            }
            _ => {}
        }
    }
    shapes
}

fn shapes_for<'a>(
    shapes: &'a [(String, usize, String)],
    target: &str,
) -> Vec<&'a (String, usize, String)> {
    shapes.iter().filter(|shape| shape.0 == target).collect()
}

#[derive(Clone, Copy)]
struct WholeArraySpans {
    parameter_declaration: dae::DaeProvenance,
    declaration: dae::DaeProvenance,
    clock: dae::DaeProvenance,
    seed: dae::DaeProvenance,
    pair: dae::DaeProvenance,
    echo: dae::DaeProvenance,
    alias: dae::DaeProvenance,
    widened: dae::DaeProvenance,
}

#[derive(Clone, Copy)]
struct WholeArrayVariables<'dae> {
    seed: dae::DiscreteRealId<'dae>,
    a: dae::DiscreteRealId<'dae>,
    b: dae::DiscreteRealId<'dae>,
    echo: dae::DiscreteRealId<'dae>,
    alias: dae::DiscreteRealId<'dae>,
    widened: dae::DiscreteRealId<'dae>,
    counts: dae::ParameterId<'dae>,
}

#[derive(Clone, Copy)]
struct WholeArrayExpressions<'dae> {
    seed_ref: dae::ExprId<'dae>,
    seed_value: dae::ExprId<'dae>,
    a_ref: dae::ExprId<'dae>,
    a_value: dae::ExprId<'dae>,
    b_ref: dae::ExprId<'dae>,
    b_value: dae::ExprId<'dae>,
    echo_ref: dae::ExprId<'dae>,
    echo_value: dae::ExprId<'dae>,
    alias_ref: dae::ExprId<'dae>,
    alias_value: dae::ExprId<'dae>,
    widened_ref: dae::ExprId<'dae>,
    widened_value: dae::ExprId<'dae>,
}

fn define_array_pair<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("pair"), [vector], [vector, vector], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let alpha = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("alpha"), 0, provenance)
            })?;
            let beta = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("beta"), 1, provenance)
            })?;
            let input = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            dae.functions(|functions| {
                functions.assign(&mut body, alpha, input, provenance)?;
                functions.assign(&mut body, beta, input, provenance)?;
                functions.define(body, provenance)
            })
        },
    )
    .map(|(function, _)| function)
}

fn define_whole_array_variables<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    spans: WholeArraySpans,
) -> Result<WholeArrayVariables<'dae>, dae::DaeConstructionError> {
    let mut discrete_real = |name| {
        dae.variables(|variables| {
            variables.discrete_real(
                VarName::new(name),
                vector,
                spans.declaration,
                dae::VariableAttributes::default(),
            )
        })
    };
    let seed = discrete_real("seed")?;
    let a = discrete_real("a")?;
    let b = discrete_real("b")?;
    let echo = discrete_real("echo")?;
    let alias = discrete_real("alias")?;
    let widened = discrete_real("widened")?;
    let integer_vector = dae.types(|types| {
        types.intern(
            TypeId::new(1),
            dae::ValueType::array(dae::ScalarType::Integer, [2]),
            spans.parameter_declaration,
        )
    })?;
    let counts = dae.variables(|variables| {
        variables.parameter(
            VarName::new("counts"),
            integer_vector,
            spans.parameter_declaration,
            dae::VariableAttributes::default(),
        )
    })?;
    Ok(WholeArrayVariables {
        seed,
        a,
        b,
        echo,
        alias,
        widened,
        counts,
    })
}

fn permuted_echo_value<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    variable: dae::DiscreteRealId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        let read = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
        let second = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(2))?;
        let first = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(1))?;
        let high = expressions.at(provenance).index(
            read,
            [dae::Subscript::Index {
                expression: second,
                provenance,
            }],
        )?;
        let low = expressions.at(provenance).index(
            read,
            [dae::Subscript::Index {
                expression: first,
                provenance,
            }],
        )?;
        expressions.at(provenance).array([high, low])
    })
}

fn define_whole_array_expressions<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    pair: dae::FunctionId<'dae>,
    variables: WholeArrayVariables<'dae>,
    spans: WholeArraySpans,
) -> Result<WholeArrayExpressions<'dae>, dae::DaeConstructionError> {
    let seed_read = dae.expressions(|expressions| {
        expressions
            .at(spans.pair)
            .coordinate(dae::CoordinateInput::DiscreteReal(variables.seed))
    })?;
    let results = dae.expressions(|expressions| {
        expressions
            .at(spans.pair)
            .call_results(pair, [0, 1], [seed_read])
    })?;
    let mut discrete_ref = |variable, provenance| {
        dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))
        })
    };
    let seed_ref = discrete_ref(variables.seed, spans.seed)?;
    let a_ref = discrete_ref(variables.a, spans.pair)?;
    let b_ref = discrete_ref(variables.b, spans.pair)?;
    let echo_ref = discrete_ref(variables.echo, spans.echo)?;
    let alias_ref = discrete_ref(variables.alias, spans.alias)?;
    let alias_value = discrete_ref(variables.a, spans.alias)?;
    let widened_ref = discrete_ref(variables.widened, spans.widened)?;
    let widened_value = dae.expressions(|expressions| {
        expressions
            .at(spans.widened)
            .coordinate(dae::CoordinateInput::Parameter(variables.counts))
    })?;
    let seed_value = dae.expressions(|expressions| {
        let one = expressions
            .at(spans.seed)
            .literal(dae::DaeLiteral::Real(1.0))?;
        let two = expressions
            .at(spans.seed)
            .literal(dae::DaeLiteral::Real(2.0))?;
        expressions.at(spans.seed).array([one, two])
    })?;
    Ok(WholeArrayExpressions {
        seed_ref,
        seed_value,
        a_ref,
        a_value: results[0],
        b_ref,
        b_value: results[1],
        echo_ref,
        echo_value: permuted_echo_value(dae, variables.b, spans.echo)?,
        alias_ref,
        alias_value,
        widened_ref,
        widened_value,
    })
}

fn define_whole_array_clock<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    variables: WholeArrayVariables<'dae>,
    expressions: WholeArrayExpressions<'dae>,
    spans: WholeArraySpans,
) -> Result<(), dae::DaeConstructionError> {
    let clock = periodic_clock(dae, spans.clock)?;
    dae.clocks(|clocks| {
        for variable in [
            variables.seed,
            variables.a,
            variables.b,
            variables.echo,
            variables.alias,
            variables.widened,
        ] {
            clocks.own_discrete_real(clock, variable, spans.declaration)?;
        }
        Ok(())
    })?;
    let tick = dae.conditions(|conditions| {
        let tick = conditions.reserve(spans.clock)?;
        conditions.define(tick, dae::ConditionInput::Clock(clock), spans.clock)?;
        Ok(tick)
    })?;
    for (provenance, lhs, rhs) in [
        (spans.seed, expressions.seed_ref, expressions.seed_value),
        (spans.pair, expressions.a_ref, expressions.a_value),
        (spans.pair, expressions.b_ref, expressions.b_value),
        (spans.echo, expressions.echo_ref, expressions.echo_value),
        (spans.alias, expressions.alias_ref, expressions.alias_value),
        (
            spans.widened,
            expressions.widened_ref,
            expressions.widened_value,
        ),
    ] {
        define_when_real_equation(dae, tick, tick, provenance, lhs, rhs)?;
    }
    Ok(())
}

/// A clocked definition whose value already denotes one whole array must reach
/// GALEC as ONE array assignment; a definition that only looks array-shaped
/// must keep one assignment per coordinate.
///
/// The fixture is deliberately not the easy case:
///
/// * `a` and `b` are two array results of ONE issued call, so an emitter that
///   collapsed a definition onto "the call's result" without selecting the
///   right output still emits two whole-array assignments and still compiles —
///   the assertion that `a` reads the `alpha` temporary and `b` the `beta` one
///   is what separates "compiles" from "computes the right thing".
/// * `echo` is `{b[2], b[1]}`: the same extent, the same element type and one
///   single source object, exactly like a collapsible copy, but a PERMUTED
///   correspondence. It must stay at one assignment per coordinate.
/// * `seed` is an array constructor of literals — complete and in order, but
///   not a reference to storage, so there is nothing to copy from.
fn whole_array_clocked_fixture() -> dae::Dae {
    let text = "parameter Integer counts[2]; discrete Real seed[2], a[2], b[2], \
                echo[2], alias[2], widened[2]; when sample(0, 1) then \
                seed = {1.0, 2.0}; (a, b) = pair(seed); echo = {b[2], b[1]}; alias = a; \
                widened = counts; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("whole-array-clocked.mo", text);
    let spans = WholeArraySpans {
        parameter_declaration: at(source, text, "parameter Integer counts[2]"),
        declaration: at(source, text, "discrete Real seed[2], a[2], b[2]"),
        clock: at(source, text, "sample(0, 1)"),
        seed: at(source, text, "seed = {1.0, 2.0}"),
        pair: at(source, text, "(a, b) = pair(seed)"),
        echo: at(source, text, "echo = {b[2], b[1]}"),
        alias: at(source, text, "alias = a"),
        widened: at(source, text, "widened = counts"),
    };
    dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                spans.declaration,
            )
        })?;
        let pair = define_array_pair(dae, vector, spans.declaration)?;
        let variables = define_whole_array_variables(dae, vector, spans)?;
        let expressions = define_whole_array_expressions(dae, pair, variables, spans)?;
        define_whole_array_clock(dae, variables, expressions, spans)
    })
    .expect("checked whole-array clocked fixture")
}

fn assert_whole_array_assignment_shapes(
    statements: &[gast::Spanned<gast::Statement>],
    shapes: &[(String, usize, String)],
) {
    let a_shapes = shapes_for(shapes, "a");
    let b_shapes = shapes_for(shapes, "b");
    assert_eq!(
        a_shapes.len(),
        1,
        "`a` must be one whole-array copy: {shapes:?}"
    );
    assert_eq!(
        b_shapes.len(),
        1,
        "`b` must be one whole-array copy: {shapes:?}"
    );
    assert_eq!(
        a_shapes[0].1, 0,
        "a whole-array target carries no subscript"
    );
    assert_eq!(
        b_shapes[0].1, 0,
        "a whole-array target carries no subscript"
    );
    assert!(
        a_shapes[0].2.contains("alpha") && !a_shapes[0].2.contains("beta"),
        "`a` must copy the `alpha` result temporary: {shapes:?}"
    );
    assert!(
        b_shapes[0].2.contains("beta") && !b_shapes[0].2.contains("alpha"),
        "`b` must copy the `beta` result temporary: {shapes:?}"
    );
    assert_eq!(
        multi_assignment_call_sites(statements, "pair"),
        1,
        "collapsing to whole-array copies must not duplicate the call"
    );

    let echo_shapes = shapes_for(shapes, "echo");
    assert_eq!(
        echo_shapes.len(),
        2,
        "a permuted definition keeps one assignment per coordinate: {shapes:?}"
    );
    assert!(
        echo_shapes.iter().all(|shape| shape.1 == 1),
        "each permuted coordinate is subscripted: {shapes:?}"
    );
    let seed_shapes = shapes_for(shapes, "seed");
    assert_eq!(
        seed_shapes.len(),
        2,
        "an array constructor keeps one assignment per coordinate: {shapes:?}"
    );
    let alias_shapes = shapes_for(shapes, "alias");
    assert_eq!(
        alias_shapes.len(),
        1,
        "`alias` must be one whole-array copy: {shapes:?}"
    );
    assert_eq!(
        alias_shapes[0].1, 0,
        "a whole-array target has no subscript"
    );
    assert_eq!(
        alias_shapes[0].2, "a",
        "`alias` must copy the whole `a` storage: {shapes:?}"
    );
    let widened_shapes = shapes_for(shapes, "widened");
    assert_eq!(
        widened_shapes.len(),
        2,
        "a widening copy keeps one assignment per coordinate: {shapes:?}"
    );
    assert!(
        widened_shapes
            .iter()
            .all(|shape| shape.1 == 1 && shape.2 == "<computed>"),
        "each widened coordinate is a subscripted conversion: {shapes:?}"
    );
}

#[test]
fn whole_array_clocked_definitions_collapse_only_under_proven_correspondence() {
    let model = whole_array_clocked_fixture();
    let statements = project(&model).expect("whole-array clocked definitions project");
    let shapes = state_assignment_shapes(&statements);
    assert_whole_array_assignment_shapes(&statements, &shapes);
}
