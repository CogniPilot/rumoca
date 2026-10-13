use super::*;

mod nested_partial;

fn partial_branch_function() -> Dae {
    partial_branch_with_read(0).expect("complete output and explicit partial local construct")
}

fn partial_branch_with_read(mode: u8) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new(
        "function choose if flag then y := 1; spare := 1; else y := 2; end if; end choose;",
    );
    let at = source.source("function choose", 0);
    let branch_at = source.source("y := 1; spare := 1;", 0);
    let fallback_at = source.source("y := 2;", 0);
    let conditional_at = source.source("if flag then", 0);
    Dae::construct(source.map, |dae| {
        let (integer, boolean) = dae.types(|types| {
            Ok((
                types.derived(ValueType::scalar(ScalarType::Integer), at)?,
                types.derived(ValueType::scalar(ScalarType::Boolean), at)?,
            ))
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("choose"), [boolean], [integer], at),
            |dae, reservation| {
                let (flag, target, spare) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("flag"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("spare"), integer, at)?,
                    ))
                })?;
                let (condition, one, two, truth, message) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(conditional_at).function_parameter(flag)?,
                        expressions.at(branch_at).literal(DaeLiteral::Integer(1))?,
                        expressions
                            .at(fallback_at)
                            .literal(DaeLiteral::Integer(2))?,
                        expressions.at(at).literal(DaeLiteral::Boolean(true))?,
                        expressions
                            .at(at)
                            .literal(DaeLiteral::String("unrelated assertion".into()))?,
                    ))
                })?;
                let parent = dae.functions(|functions| functions.begin(reservation, at))?;
                let (mut child, finish) =
                    dae.functions(|functions| functions.begin_branch(parent, branch_at))?;
                dae.functions(|functions| {
                    functions.assign_all(&mut child, &[(target, one), (spare, one)], branch_at)
                })?;
                let (parent, branch) =
                    dae.functions(|functions| functions.finish_branch(finish, child))?;
                let (mut child, finish) =
                    dae.functions(|functions| functions.begin_branch(parent, fallback_at))?;
                dae.functions(|functions| functions.assign(&mut child, target, two, fallback_at))?;
                let (mut parent, fallback) =
                    dae.functions(|functions| functions.finish_branch(finish, child))?;
                dae.functions(|functions| {
                    functions.finish_conditional(
                        &mut parent,
                        vec![(condition, branch)],
                        fallback,
                        conditional_at,
                    )?;
                    assert!(matches!(
                        functions.current_definition_id(&parent, spare, at),
                        Err(DaeConstructionError::IncompleteDefinition {
                            kind: "function value",
                            ..
                        })
                    ));
                    Ok(())
                })?;
                append_mode_statements(
                    dae,
                    &mut parent,
                    mode,
                    (target, spare),
                    (one, truth, message),
                    at,
                )?;
                dae.functions(|functions| functions.define(parent, at))
            },
        )?;
        Ok(())
    })
}

/// The statements each `partial_branch_with_read` mode appends after the conditional.
fn append_mode_statements<'dae>(
    dae: &mut DaeConstruction<'dae>,
    parent: &mut FunctionBody<'dae>,
    mode: u8,
    (target, spare): (FunctionValueId<'dae>, FunctionValueId<'dae>),
    (one, truth, message): (ExprId<'dae>, ExprId<'dae>, ExprId<'dae>),
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    match mode {
        1 => dae.functions(|functions| {
            functions.assertion(parent, truth, message, at)?;
            functions.read(parent, spare, at).map(|_| ())
        }),
        2 => dae.functions(|functions| {
            functions.require_assigned(parent, spare, at)?;
            let rhs = functions.read(parent, spare, at)?;
            functions.assign(parent, target, rhs, at)
        }),
        3 => dae.functions(|functions| {
            functions.assign(parent, spare, one, at)?;
            functions.require_assigned(parent, spare, at).map(|_| ())
        }),
        _ => Ok(()),
    }
}

fn check_partial_branch(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let mut values = function.values();
    let target = values.next().unwrap().id();
    let spare = values.next().unwrap().id();
    let statements = function.statements().collect::<Vec<_>>();
    let [FunctionStatementView::Conditional { conditional }] = &statements[..] else {
        panic!("one canonical statement owns both ordered branches");
    };
    assert_eq!(conditional.conditions().len(), 1);
    let branch = conditional.branches().next().unwrap();
    assert_eq!(
        branch.incoming(target),
        Some(FunctionAssignedState::Unassigned)
    );
    assert!(matches!(
        branch.outgoing(spare),
        Some(FunctionAssignedState::Assigned(_))
    ));
    assert_eq!(
        conditional.fallback().outgoing(spare),
        Some(FunctionAssignedState::Unassigned)
    );
    assert_eq!(
        conditional.outgoing(spare),
        Some(FunctionAssignedState::Conditional {
            owner: conditional.id(),
            value: spare
        })
    );
    let Some(FunctionAssignedState::Assigned(definition)) = conditional.outgoing(target) else {
        panic!("all arms assign the complete output");
    };
    assert_eq!(
        branch.body_scope().relation(definition),
        Some(FunctionScopeRelation::Enclosing)
    );
    assert_eq!(
        conditional
            .outputs()
            .map(|output| output.id())
            .collect::<Vec<_>>(),
        vec![definition]
    );
    let rhs = view.function_definition(definition).unwrap().rhs();
    assert!(
        matches!(view.expression(rhs).unwrap().operation(), ExpressionOperation::FunctionConditionalOutput { conditional: owner, value, .. } if owner == conditional.id() && value == target)
    );
    assert!(matches!(
        branch.statements().next(),
        Some(FunctionStatementView::AssignmentGroup { .. })
    ));
    assert!(matches!(
        conditional.fallback().statements().next(),
        Some(FunctionStatementView::Assignment { .. })
    ));
}

#[test]
fn ordered_branch_owner_retains_complete_and_partial_assigned_states() {
    partial_branch_function().inspect(check_partial_branch);
}

#[test]
fn current_wire_replays_scoped_prefix_and_derives_outgoing_states() {
    let dae = partial_branch_function();
    let json = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&json).unwrap();
    replayed.inspect(check_partial_branch);
    assert_eq!(serde_json::to_string(&replayed).unwrap(), json);
}

#[test]
fn ordered_branch_schema_rejects_superseded_version() {
    let dae = partial_branch_function();
    let mut wire = serde_json::to_value(&dae).unwrap();
    wire["schema_version"] = serde_json::json!(DAE_SCHEMA_VERSION - 1);
    assert!(serde_json::from_value::<Dae>(wire).is_err());
}

#[test]
fn ordinal_wire_replays_constructor_issued_conditional_projection() {
    let dae = partial_branch_function();
    let binary = bincode::serialize(&dae).unwrap();
    let replayed: Dae = bincode::deserialize(&binary).unwrap();
    replayed.inspect(check_partial_branch);
    assert_eq!(bincode::serialize(&replayed).unwrap(), binary);
}

fn mutate_projection(mut mutation: impl FnMut(&mut serde_json::Value)) -> serde_json::Value {
    let mut wire = serde_json::to_value(partial_branch_function()).unwrap();
    let marker = wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut("function_conditional_output"))
        .expect("one constructor-generated output marker");
    mutation(marker);
    wire
}

#[test]
fn generated_projection_marker_rejects_a_foreign_function() {
    let wire = mutate_projection(|marker| marker["function"] = serde_json::json!(1));
    assert!(serde_json::from_value::<Dae>(wire).is_err());
}

#[test]
fn generated_projection_marker_cannot_supply_derived_owner_coordinates() {
    let wire = mutate_projection(|marker| marker["conditional"] = serde_json::json!(0));
    assert!(serde_json::from_value::<Dae>(wire).is_err());
}

fn loop_branch_function(forbidden: bool) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new(
        "function loop_branch for i in 1:2 loop if true then y := 1; else y := 2; end if; end for; end loop_branch;",
    );
    let at = source.source("function loop_branch", 0);
    Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        dae.function(
            FunctionSignature::new(VarName::new("loop_branch"), [], [integer], at),
            |dae, reservation| construct_loop_branch(dae, reservation, integer, at, forbidden),
        )?;
        Ok(())
    })
}

fn construct_loop_branch<'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservation: FunctionReservation<'_, 'dae>,
    integer: ValueTypeId<'dae>,
    at: DaeProvenance,
    forbidden: bool,
) -> Result<(), DaeConstructionError> {
    let (target, local) = dae.functions(|functions| {
        Ok((
            functions.output(&reservation, VarName::new("y"), 0, at)?,
            functions.local(&reservation, VarName::new("z"), integer, at)?,
        ))
    })?;
    let (zero, one, two, condition) = dae.expressions(|expressions| {
        Ok((
            expressions.at(at).literal(DaeLiteral::Integer(0))?,
            expressions.at(at).literal(DaeLiteral::Integer(1))?,
            expressions.at(at).literal(DaeLiteral::Integer(2))?,
            expressions.at(at).literal(DaeLiteral::Boolean(true))?,
        ))
    })?;
    let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
    dae.functions(|functions| functions.assign(&mut body, target, zero, at))?;
    let domain = dae.domains(|domains| {
        domains.structured(
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id: 0,
                    display_name: "i".into(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                }],
            },
            at,
        )
    })?;
    let loop_body = dae.functions(|functions| functions.begin_loop(body, domain, [target], at))?;
    let (mut child, finish) =
        dae.functions(|functions| functions.begin_branch_loop(loop_body, at))?;
    dae.functions(|functions| {
        functions.assign(&mut child, if forbidden { local } else { target }, one, at)
    })?;
    let (loop_body, branch) =
        dae.functions(|functions| functions.finish_branch_loop(finish, child))?;
    let (mut child, finish) =
        dae.functions(|functions| functions.begin_branch_loop(loop_body, at))?;
    dae.functions(|functions| functions.assign(&mut child, target, two, at))?;
    let (mut loop_body, fallback) =
        dae.functions(|functions| functions.finish_branch_loop(finish, child))?;
    dae.functions(|functions| {
        functions.finish_conditional_loop(&mut loop_body, vec![(condition, branch)], fallback, at)
    })?;
    let body = dae.functions(|functions| functions.finish_loop(loop_body, at))?;
    dae.functions(|functions| functions.define(body, at))
}

#[test]
fn scoped_branch_reuses_the_existing_compact_loop_owner_and_wire() {
    let dae = loop_branch_function(false).unwrap();
    let encoded = bincode::serialize(&dae).unwrap();
    let replayed: Dae = bincode::deserialize(&encoded).unwrap();
    assert_eq!(bincode::serialize(&replayed).unwrap(), encoded);
    replayed.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(function.fold_count(), 1);
        let Some(FunctionStatementView::For { statements, .. }) = function.statements().nth(1)
        else {
            panic!("root retains its one compact loop");
        };
        assert!(matches!(
            statements.last(),
            Some(FunctionStatementView::Conditional { .. })
        ));
    });
}

#[test]
fn branch_body_cannot_bypass_loop_carried_target_admission() {
    let error = loop_branch_function(true).unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "function loop target",
            ..
        }
    ));
}

fn assertion_only_branch_function() -> Dae {
    let source = TestSource::new(
        "function effects if false then assert(false, \"inactive\"); else assert(true, \"selected\"); end if; end effects;",
    );
    let at = source.source("function effects", 0);
    Dae::construct(source.map, |dae| {
        dae.function(
            FunctionSignature::new(VarName::new("effects"), [], [], at),
            |dae, reservation| {
                let (no, yes, inactive, selected) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(DaeLiteral::Boolean(false))?,
                        expressions.at(at).literal(DaeLiteral::Boolean(true))?,
                        expressions
                            .at(at)
                            .literal(DaeLiteral::String("inactive".into()))?,
                        expressions
                            .at(at)
                            .literal(DaeLiteral::String("selected".into()))?,
                    ))
                })?;
                let parent = dae.functions(|functions| functions.begin(reservation, at))?;
                let (mut child, finish) =
                    dae.functions(|functions| functions.begin_branch(parent, at))?;
                dae.functions(|functions| functions.assertion(&mut child, no, inactive, at))?;
                let (parent, branch) =
                    dae.functions(|functions| functions.finish_branch(finish, child))?;
                let (mut child, finish) =
                    dae.functions(|functions| functions.begin_branch(parent, at))?;
                dae.functions(|functions| functions.assertion(&mut child, yes, selected, at))?;
                let (mut parent, fallback) =
                    dae.functions(|functions| functions.finish_branch(finish, child))?;
                dae.functions(|functions| {
                    functions.finish_conditional(&mut parent, vec![(no, branch)], fallback, at)?;
                    functions.define(parent, at)
                })
            },
        )?;
        Ok(())
    })
    .expect("effect-only branches retain their canonical statement")
}

fn check_assertion_only_branches(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = function.statements().collect::<Vec<_>>();
    let [FunctionStatementView::Conditional { conditional }] = &statements[..] else {
        panic!("zero projections must not erase the selected-effect statement");
    };
    assert_eq!(conditional.outputs().len(), 0);
    for (branch, expected) in [
        (conditional.branches().next().unwrap(), false),
        (conditional.fallback(), true),
    ] {
        let statements = branch.statements().collect::<Vec<_>>();
        let [FunctionStatementView::Assertion { condition, .. }] = &statements[..] else {
            panic!("both activation-owned assertions remain in their own branch");
        };
        assert!(
            matches!(view.expression(*condition).unwrap().operation(), ExpressionOperation::Literal(DaeLiteral::Boolean(value)) if *value == expected)
        );
    }
}

#[test]
fn zero_output_conditional_retains_selected_and_inactive_assertion_owners() {
    assertion_only_branch_function().inspect(check_assertion_only_branches);
}

#[test]
fn zero_output_conditional_current_wire_retains_effect_only_prefixes() {
    let dae = assertion_only_branch_function();
    let binary = bincode::serialize(&dae).unwrap();
    let replayed: Dae = bincode::deserialize(&binary).unwrap();
    replayed.inspect(check_assertion_only_branches);
    assert_eq!(bincode::serialize(&replayed).unwrap(), binary);
}

#[test]
fn require_assigned_issues_an_ordered_check_before_the_partial_projection() {
    let dae = partial_branch_with_read(2).unwrap();
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        let [FunctionStatementView::Conditional { conditional }, FunctionStatementView::RequireAssigned { conditional: checked, value, definition, .. }, FunctionStatementView::Assignment { .. }] = &statements[..] else { panic!("partial use retains its ordered check and projection"); };
        assert_eq!(conditional.id(), checked.id());
        assert_eq!(conditional.outputs().len(), 1);
        assert!(matches!(conditional.outgoing(*value), Some(FunctionAssignedState::Conditional { .. })));
        assert!(matches!(view.expression(definition.rhs()).unwrap().operation(), ExpressionOperation::FunctionConditionalOutput { conditional: owner, value: target, .. } if owner == checked.id() && target == *value));
        let mut inputs = Vec::new();
        assert!(conditional.visit_value_inputs(*value, |input| inputs.push(input)));
        assert_eq!(inputs.len(), 2, "one condition and one actually assigned source; missing path has no operand");
    });
}

#[test]
fn require_assigned_current_wire_replays_exact_state_and_generated_projection() {
    let dae = partial_branch_with_read(2).unwrap();
    let bytes = bincode::serialize(&dae).unwrap();
    let replayed: Dae = bincode::deserialize(&bytes).unwrap();
    assert_eq!(bincode::serialize(&replayed).unwrap(), bytes);
}

#[test]
fn ordinary_boolean_assertion_cannot_promote_an_unchecked_partial_state() {
    assert!(matches!(
        partial_branch_with_read(1),
        Err(DaeConstructionError::IncompleteDefinition {
            kind: "function value",
            ..
        })
    ));
}

#[test]
fn require_assigned_cannot_reuse_a_partial_owner_after_current_state_advances() {
    assert!(matches!(
        partial_branch_with_read(3),
        Err(DaeConstructionError::IncompleteDefinition {
            kind: "function partial assigned state",
            ..
        })
    ));
}

#[test]
fn require_assigned_wire_rejects_retargeting_to_an_already_assigned_value() {
    let mut wire = serde_json::to_value(partial_branch_with_read(2).unwrap()).unwrap();
    wire["storage"]["functions"][0]["statements"][1]["require_assigned"]["target"] =
        serde_json::json!(0);
    assert!(serde_json::from_value::<Dae>(wire).is_err());
}

#[test]
fn require_assigned_wire_rejects_an_output_marker_without_its_ordered_action() {
    let mut wire = serde_json::to_value(partial_branch_with_read(2).unwrap()).unwrap();
    wire["storage"]["functions"][0]["statements"]
        .as_array_mut()
        .unwrap()
        .remove(1);
    assert!(serde_json::from_value::<Dae>(wire).is_err());
}

#[test]
fn require_assigned_wire_cannot_supply_a_foreign_derived_owner() {
    let mut wire = serde_json::to_value(partial_branch_with_read(2).unwrap()).unwrap();
    wire["storage"]["functions"][0]["statements"][1]["require_assigned"]["owner"] =
        serde_json::json!(1);
    assert!(serde_json::from_value::<Dae>(wire).is_err());
}

#[test]
fn require_assigned_rejects_a_genuine_foreign_function_target() {
    let source = TestSource::new("function a end a; function b end b;");
    let at = source.source("function a", 0);
    let error = Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), at))?;
        let first = FunctionSignature::new(VarName::new("a"), [], [], at);
        let second = FunctionSignature::new(VarName::new("b"), [], [], at);
        dae.recursive_functions(first, [second], |dae, reservations| {
            let mut reservations = reservations.into_iter();
            let first = reservations.next().unwrap();
            let second = reservations.next().unwrap();
            let foreign = dae
                .functions(|functions| functions.local(&second, VarName::new("z"), integer, at))?;
            let mut body = dae.functions(|functions| functions.begin(first, at))?;
            dae.functions(|functions| functions.require_assigned(&mut body, foreign, at))?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::InvalidFunctionScope {
            expected_function: Some(0),
            found_function: 1,
            ..
        }
    ));
}

/// A local only one conditional arm assigns, checked inside a later loop that
/// does not carry it.
fn partial_local_checked_in_loop() -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new(
        "function loop_partial if flag then y := 1; spare := 1; else y := 2; end if; for i in 1:2 loop y := y + spare; end for; end loop_partial;",
    );
    let at = source.source("function loop_partial", 0);
    Dae::construct(source.map, |dae| {
        let (integer, boolean) = dae.types(|types| {
            Ok((
                types.derived(ValueType::scalar(ScalarType::Integer), at)?,
                types.derived(ValueType::scalar(ScalarType::Boolean), at)?,
            ))
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("loop_partial"), [boolean], [integer], at),
            |dae, reservation| {
                let (flag, target, spare) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("flag"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("spare"), integer, at)?,
                    ))
                })?;
                let (condition, one, two) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).function_parameter(flag)?,
                        expressions.at(at).literal(DaeLiteral::Integer(1))?,
                        expressions.at(at).literal(DaeLiteral::Integer(2))?,
                    ))
                })?;
                let parent = dae.functions(|functions| functions.begin(reservation, at))?;
                let (mut child, finish) =
                    dae.functions(|functions| functions.begin_branch(parent, at))?;
                dae.functions(|functions| {
                    functions.assign_all(&mut child, &[(target, one), (spare, one)], at)
                })?;
                let (parent, branch) =
                    dae.functions(|functions| functions.finish_branch(finish, child))?;
                let (mut child, finish) =
                    dae.functions(|functions| functions.begin_branch(parent, at))?;
                dae.functions(|functions| functions.assign(&mut child, target, two, at))?;
                let (mut body, fallback) =
                    dae.functions(|functions| functions.finish_branch(finish, child))?;
                dae.functions(|functions| {
                    functions.finish_conditional(&mut body, vec![(condition, branch)], fallback, at)
                })?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                provenance: rumoca_core::IndexBinderProvenance::Generated,
                                id: 0,
                                display_name: "i".into(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let mut looped =
                    dae.functions(|functions| functions.begin_loop(body, domain, [target], at))?;
                dae.functions(|functions| functions.require_assigned_loop(&mut looped, spare, at))?;
                let current =
                    dae.functions(|functions| functions.read(looped.body(), target, at))?;
                let read = dae.functions(|functions| functions.read(looped.body(), spare, at))?;
                let sum = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(BinaryOperator::Add, current, read)
                })?;
                dae.functions(|functions| functions.assign_loop(&mut looped, target, sum, at))?;
                let body = dae.functions(|functions| functions.finish_loop(looped, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
}

#[test]
fn a_loop_checks_a_local_a_preceding_conditional_assigned_on_one_path() {
    let dae = partial_local_checked_in_loop().unwrap();
    let encoded = bincode::serialize(&dae).unwrap();
    let replayed: Dae = bincode::deserialize(&encoded).unwrap();
    assert_eq!(bincode::serialize(&replayed).unwrap(), encoded);
}
