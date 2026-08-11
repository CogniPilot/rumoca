use super::*;
use crate::{
    ProgramSlot, SolveBinaryOperator, SolveCompareOperator, SolveConversionOperator,
    SolveIntegerDomain, SolvePredefinedSignal, SolvePureCallTable, SolveRealFormat,
    SolveRoundingMode, SolveScalarType, SolveSignal, SolveValue, TypedProgramBuilder,
};
use rumoca_core::{SourceId, StructuredIndexBinder, StructuredIndexDomain};

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("solve_method.alg"),
        start,
        start + 1,
    )
}

fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveRoundingMode::NearestTiesToEven,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

fn real() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::real(profile()))
}

fn vector() -> SolveValueType {
    SolveValueType::tensor(SolveScalarType::real(profile()), vec![3]).unwrap()
}

fn overflow() -> SolveSignalSet {
    SolveSignalSet::construct(&[SolveSignal::Predefined(SolvePredefinedSignal::Overflow)])
}

fn nan() -> SolveSignalSet {
    SolveSignalSet::construct(&[SolveSignal::Predefined(SolvePredefinedSignal::Nan)])
}

fn universe() -> SolveSignalSet {
    overflow().union(nan())
}

fn pure_calls() -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |_table| Ok(())).expect("an empty call table")
}

fn unit_range() -> SolveValueRange {
    SolveValueRange::construct(
        SolveValue::real(profile(), -1.0),
        SolveValue::real(profile(), 1.0),
    )
    .expect("a checked saturation range")
}

fn integer() -> SolveValueType {
    SolveValueType::scalar(SolveScalarType::integer(profile()))
}

/// One `true` literal usable as a branch condition; it raises nothing.
fn boolean_constant(
    method: &SolveMethodBuilder<'_>,
    provenance: Span,
) -> Result<SolveValueProgram, SolveActionConstructionError> {
    method.boolean_program(&[], provenance, |program, _inputs, outputs| {
        let value = program.constant(SolveValue::boolean(true), provenance)?;
        program.store(outputs[0], value, provenance)
    })
}

/// One Real relational test over `cell`; SPEC_0042 T9 makes it raise NAN.
fn compare_parameter<'method>(
    method: &SolveMethodBuilder<'method>,
    cell: MethodCell<'method>,
    provenance: Span,
) -> Result<SolveValueProgram, SolveActionConstructionError> {
    method.boolean_program(&[cell], provenance, |program, inputs, outputs| {
        let lhs = program.load(inputs[0], provenance)?;
        let rhs = program.load(inputs[0], provenance)?;
        let test = program.compare(SolveCompareOperator::Greater, lhs, rhs, provenance)?;
        program.store(outputs[0], test, provenance)
    })
}

fn table(
    build: impl FnOnce(&mut SolveMethodTableBuilder) -> Result<(), SolveActionConstructionError>,
) -> Result<SolveMethodTable, SolveActionConstructionError> {
    SolveMethodTable::construct(profile(), universe(), pure_calls(), build)
}

fn ranged_state() -> SolveMethodBinding {
    SolveMethodBinding::construct(
        real(),
        SolveStorageClass::PersistentState,
        SolveSlotAccess::ReadWrite,
        Some(unit_range()),
    )
}

fn tunable() -> SolveMethodBinding {
    SolveMethodBinding::construct(
        real(),
        SolveStorageClass::TunableParameter,
        SolveSlotAccess::ReadOnly,
        None,
    )
}

fn copy_program<'program>(
    program: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
) -> Result<(), SolveProgramConstructionError> {
    let value = program.load(inputs[0], span(90))?;
    program.store(outputs[0], value, span(91))
}

fn sum_program<'program>(
    program: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
) -> Result<(), SolveProgramConstructionError> {
    let lhs = program.load(inputs[0], span(92))?;
    let rhs = program.load(inputs[1], span(93))?;
    let sum = program.binary(SolveBinaryOperator::Add, lhs, rhs, span(94))?;
    program.store(outputs[0], sum, span(95))
}

fn constant_program<'program>(
    program: &mut TypedProgramBuilder<'program>,
    _inputs: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
) -> Result<(), SolveProgramConstructionError> {
    let value = program.constant(SolveValue::real(profile(), 0.5), span(96))?;
    program.store(outputs[0], value, span(97))
}

/// `parameter`, `result`, ranged persistent state, and one tunable parameter.
fn controller_interface(escapes: SolveSignalSet) -> SolveMethodInterface {
    SolveMethodInterface::construct(
        vec![real()],
        vec![real()],
        vec![ranged_state(), tunable()],
        escapes,
    )
}

fn loop_domain(upper: i64, step: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_owned(),
            lower: 1,
            upper,
            step,
        }],
    }
}

/// One stateful controller step exercising locals, loops, branches, limits,
/// and one raised signal in source order.
fn controller_table() -> Result<SolveMethodTable, SolveActionConstructionError> {
    table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            // SPEC_0042 T9: the Real comparison below raises NAN beside the
            // explicitly raised Overflow.
            controller_interface(universe()),
            span(0),
            |method, cells| {
                let parameter = cells.parameters()[0];
                let result = cells.results()[0];
                let state = cells.bindings()[0];
                let gain = cells.bindings()[1];
                let accumulator = method.declare_local(real(), None, span(1))?;
                method.assign(&[accumulator], &[parameter], span(2), copy_program)?;
                method.iterate(loop_domain(3, 1), span(3), |body, binders| {
                    assert_eq!(binders.len(), 1);
                    body.assign(&[accumulator], &[accumulator, gain], span(4), sum_program)
                })?;
                let condition = method.boolean_program(
                    &[accumulator, gain],
                    span(5),
                    |program, inputs, outputs| {
                        let lhs = program.load(inputs[0], span(6))?;
                        let rhs = program.load(inputs[1], span(7))?;
                        let test =
                            program.compare(SolveCompareOperator::Greater, lhs, rhs, span(8))?;
                        program.store(outputs[0], test, span(9))
                    },
                )?;
                method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(10),
                    |arm, _closure| arm.raise(overflow(), &[], span(11)),
                    |arm| arm.limit(&[SolveLimitTargetSpec::Cell(state)], span(12)),
                )?;
                method.assign(&[result], &[accumulator], span(13), copy_program)?;
                method.assign(&[state], &[accumulator], span(14), copy_program)
            },
        )?;
        Ok(())
    })
}

#[test]
fn ordered_actions_own_lexical_scopes_and_explicit_effects() {
    let table = controller_table().expect("the checked controller method constructs");
    let [method] = table.methods() else {
        panic!("exactly one method is issued");
    };
    assert_eq!(method.kind(), SolveMethodKind::Stateful);
    assert_eq!(method.body().locals().len(), 1);
    let actions = method.body().actions();
    assert_eq!(actions.len(), 5);
    assert!(matches!(actions[0].action(), SolveAction::Assign { .. }));
    assert!(matches!(actions[1].action(), SolveAction::Loop { .. }));
    assert!(matches!(actions[2].action(), SolveAction::Branch { .. }));
    assert_eq!(actions[3].provenance(), span(13));
    assert_eq!(actions[4].provenance(), span(14));

    let SolveAction::Loop {
        domain,
        binders,
        body,
    } = actions[1].action()
    else {
        panic!("the second action is the bounded loop");
    };
    assert_eq!(domain.validate().expect("a finite domain"), 3);
    assert_eq!(binders.len(), 1);
    assert_eq!(body.actions().len(), 1);

    let SolveAction::Branch {
        if_true, if_false, ..
    } = actions[2].action()
    else {
        panic!("the third action is the branch");
    };
    assert!(matches!(
        if_true.actions()[0].action(),
        SolveAction::Signal { .. }
    ));
    assert!(matches!(
        if_false.actions()[0].action(),
        SolveAction::Limit { .. }
    ));
    // Root, loop body, and both arms each own one lexical scope.
    assert_eq!(method.scopes().len(), 4);
    assert_eq!(method.scopes()[0].parent(), None);
    assert_eq!(
        method.scopes()[1].parent(),
        Some(method.scopes()[0].id()),
        "the loop scope nests inside the method root scope"
    );
    // The binder and the accumulator are lexical; nothing else was created.
    assert_eq!(method.cells().len(), 6);
    assert!(
        method
            .cells()
            .iter()
            .filter(|cell| !cell.is_block_storage())
            .all(|cell| cell.storage() == SolveStorageClass::MethodLocal)
    );
}

#[test]
fn controller_table_replays_through_its_constructors() {
    let table = controller_table().expect("the checked controller method constructs");
    let json = serde_json::to_string(&table).expect("the method table serializes");
    let replayed: SolveMethodTable =
        serde_json::from_str(&json).expect("the method table replays through construction");
    assert_eq!(replayed, table);
}

#[test]
fn call_abi_is_derived_from_the_checked_interface() {
    let table = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(
                vec![vector()],
                vec![real()],
                Vec::new(),
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| method.assign(&[cells.results()[0]], &[], span(1), constant_program),
        )?;
        methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(vec![real()], vec![real()], Vec::new(), nan()),
            span(2),
            |method, cells| {
                method.assign(&[cells.results()[0]], &[], span(3), constant_program)?;
                method.raise(nan(), &[], span(4))
            },
        )?;
        Ok(())
    })
    .expect("both interfaces construct");

    let pure = table.methods()[0].abi();
    assert_eq!(
        pure.parameters(),
        [SolveParameterPassing::ConstantReference],
        "aggregates cross the boundary by constant reference"
    );
    assert_eq!(pure.results(), [SolveResultPassing::ReturnValue]);
    assert_eq!(pure.status(), SolveStatusPassing::None);

    let escaping = table.methods()[1].abi();
    assert_eq!(escaping.parameters(), [SolveParameterPassing::Value]);
    assert_eq!(
        escaping.results(),
        [SolveResultPassing::OutputBuffer],
        "an escaping method spends its return value on the status word"
    );
    assert_eq!(escaping.status(), SolveStatusPassing::ErrorSignalReturn);
}

#[test]
fn bounded_acyclic_calls_reach_only_previously_issued_methods() {
    let issued = table(|methods| {
        let leaf = methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(
                vec![real()],
                vec![real()],
                Vec::new(),
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.assign(
                    &[cells.results()[0]],
                    &[cells.parameters()[0]],
                    span(1),
                    copy_program,
                )
            },
        )?;
        methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(
                vec![real()],
                vec![real()],
                Vec::new(),
                SolveSignalSet::EMPTY,
            ),
            span(2),
            |method, cells| {
                method.invoke(
                    leaf,
                    &[cells.parameters()[0]],
                    &[cells.results()[0]],
                    span(3),
                )
            },
        )?;
        Ok(())
    })
    .expect("a leaf method and its caller construct");
    assert!(matches!(
        issued.methods()[1].body().actions()[0].action(),
        SolveAction::Invoke { method, .. } if *method == issued.methods()[0].id()
    ));

    let error = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(
                vec![real()],
                vec![real()],
                Vec::new(),
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.invoke(
                    SolveMethodId::from_index(0),
                    &[cells.parameters()[0]],
                    &[cells.results()[0]],
                    span(1),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("a method cannot call itself");
    assert_eq!(
        error,
        SolveActionConstructionError::UnknownMethod {
            provenance: span(1)
        }
    );
}

#[test]
fn stateless_methods_cannot_write_state_or_call_stateful_methods() {
    let write = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(
                vec![real()],
                vec![real()],
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.assign(
                    &[cells.bindings()[0]],
                    &[cells.parameters()[0]],
                    span(1),
                    copy_program,
                )
            },
        )?;
        Ok(())
    })
    .expect_err("a stateless function cannot write persistent state");
    assert_eq!(
        write,
        SolveActionConstructionError::StatefulEffectInStatelessMethod {
            provenance: span(1)
        }
    );

    let call = table(|methods| {
        let stateful = methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| method.assign(&[cells.bindings()[0]], &[], span(1), constant_program),
        )?;
        methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(
                Vec::new(),
                vec![real()],
                Vec::new(),
                SolveSignalSet::EMPTY,
            ),
            span(2),
            |method, cells| {
                method.assign(&[cells.results()[0]], &[], span(3), constant_program)?;
                method.invoke(stateful, &[], &[], span(4))
            },
        )?;
        Ok(())
    })
    .expect_err("a stateless function cannot call a stateful method");
    assert_eq!(
        call,
        SolveActionConstructionError::StatefulEffectInStatelessMethod {
            provenance: span(4)
        }
    );
}

#[test]
fn a_catching_check_binds_one_lexical_closure_and_discharges_its_signals() {
    let caught = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.raise(nan(), &[], span(1))?;
                method.branch(
                    SolveBranchConditionSpec::Signal {
                        test: Some(SolveSignalTest::construct(false, nan(), span(2))?),
                        capture_closure: true,
                        fallback: None,
                    },
                    span(3),
                    |arm, _closure| {
                        arm.assign(&[cells.bindings()[0]], &[], span(4), constant_program)
                    },
                    |arm| arm.limit(&[SolveLimitTargetSpec::RangedState], span(5)),
                )
            },
        )?;
        Ok(())
    })
    .expect("catching the raised signal discharges the escape set");
    let method = &caught.methods()[0];
    assert_eq!(method.interface().escapes(), SolveSignalSet::EMPTY);
    let [closure] = method.closures() else {
        panic!("the catching check binds exactly one closure");
    };
    assert_eq!(closure.caught(), nan());
    assert_eq!(
        closure.scope(),
        method.scopes()[1].id(),
        "a closure lives only in the arm that caught it"
    );

    let reraised = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(Vec::new(), Vec::new(), vec![ranged_state()], nan()),
            span(0),
            |method, cells| {
                method.raise(nan(), &[], span(1))?;
                method.branch(
                    SolveBranchConditionSpec::Signal {
                        test: Some(SolveSignalTest::construct(false, nan(), span(2))?),
                        capture_closure: true,
                        fallback: None,
                    },
                    span(3),
                    |arm, closure| {
                        let closure = closure.expect("the check captured a closure");
                        arm.raise(SolveSignalSet::EMPTY, &[closure], span(4))
                    },
                    |arm| arm.assign(&[cells.bindings()[0]], &[], span(5), constant_program),
                )
            },
        )?;
        Ok(())
    })
    .expect("re-raising a caught closure propagates its signals");
    assert_eq!(reraised.methods()[0].interface().escapes(), nan());

    let json = serde_json::to_string(&reraised).expect("the signal method serializes");
    let replayed: SolveMethodTable =
        serde_json::from_str(&json).expect("the signal method replays through construction");
    assert_eq!(replayed, reraised);
}

#[test]
fn a_declared_escape_set_must_equal_the_constructed_one() {
    let error = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, _cells| method.raise(nan(), &[], span(1)),
        )?;
        Ok(())
    })
    .expect_err("an undeclared escaping signal is rejected");
    assert_eq!(
        error,
        SolveActionConstructionError::EscapeSetMismatch {
            provenance: span(0)
        }
    );
}

#[test]
fn effects_name_only_declared_signals_and_ranged_entities() {
    let undeclared = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, _cells| {
                method.raise(
                    SolveSignalSet::construct(&[SolveSignal::Predefined(
                        SolvePredefinedSignal::UnspecifiedError,
                    )]),
                    &[],
                    span(1),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("a signal outside the declared universe is rejected");
    assert_eq!(
        undeclared,
        SolveActionConstructionError::UndeclaredSignal {
            provenance: span(1)
        }
    );

    let unranged = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![SolveMethodBinding::construct(
                    real(),
                    SolveStorageClass::PersistentState,
                    SolveSlotAccess::ReadWrite,
                    None,
                )],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.limit(&[SolveLimitTargetSpec::Cell(cells.bindings()[0])], span(1))
            },
        )?;
        Ok(())
    })
    .expect_err("limiting an unranged entity is rejected");
    assert_eq!(
        unranged,
        SolveActionConstructionError::UnrangedLimitTarget {
            provenance: span(1)
        }
    );
}

#[test]
fn lexical_locals_cannot_escape_their_scope_or_the_method_abi() {
    let error = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let mut escaped = None;
                let condition = method.boolean_program(&[], span(1), |program, _, outputs| {
                    let value = program.constant(SolveValue::boolean(true), span(2))?;
                    program.store(outputs[0], value, span(3))
                })?;
                method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(4),
                    |arm, _closure| {
                        let inner = arm.declare_local(real(), None, span(5))?;
                        escaped = Some(inner);
                        arm.assign(&[inner], &[], span(6), constant_program)
                    },
                    |arm| arm.assign(&[cells.results()[0]], &[], span(7), constant_program),
                )?;
                let inner = escaped.expect("the arm declared one lexical local");
                method.assign(&[cells.results()[0]], &[inner], span(8), copy_program)
            },
        )?;
        Ok(())
    })
    .expect_err("an arm-local cell is invisible after its scope closes");
    assert_eq!(
        error,
        SolveActionConstructionError::CellOutOfScope {
            provenance: span(8)
        }
    );

    let late = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                method.assign(&[cells.results()[0]], &[], span(1), constant_program)?;
                method.declare_local(real(), None, span(2))?;
                Ok(())
            },
        )?;
        Ok(())
    })
    .expect_err("locals are declared at the head of their lexical block");
    assert_eq!(
        late,
        SolveActionConstructionError::DeclarationAfterAction {
            provenance: span(2)
        }
    );

    let bound = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                vec![real()],
                vec![SolveMethodBinding::construct(
                    real(),
                    SolveStorageClass::MethodLocal,
                    SolveSlotAccess::ReadWrite,
                    None,
                )],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| method.assign(&[cells.results()[0]], &[], span(1), constant_program),
        )?;
        Ok(())
    })
    .expect_err("a lexical local cannot be bound as block storage");
    assert_eq!(
        bound,
        SolveActionConstructionError::LocalStorageEscape {
            provenance: span(0)
        }
    );
}

#[test]
fn reads_and_results_require_dominating_definitions() {
    let undefined = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let local = method.declare_local(real(), None, span(1))?;
                method.assign(&[cells.results()[0]], &[local], span(2), copy_program)
            },
        )?;
        Ok(())
    })
    .expect_err("a local read must be dominated by a definition");
    assert_eq!(
        undefined,
        SolveActionConstructionError::UndefinedRead {
            provenance: span(2)
        }
    );

    let partial = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let condition = method.boolean_program(&[], span(1), |program, _, outputs| {
                    let value = program.constant(SolveValue::boolean(true), span(2))?;
                    program.store(outputs[0], value, span(3))
                })?;
                method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(4),
                    |arm, _closure| {
                        arm.assign(&[cells.results()[0]], &[], span(5), constant_program)
                    },
                    |arm| arm.limit(&[SolveLimitTargetSpec::Cell(cells.bindings()[0])], span(6)),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("a result defined on one arm only is rejected");
    assert_eq!(
        partial,
        SolveActionConstructionError::UndefinedResult {
            provenance: span(0)
        }
    );

    let complete = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let condition = method.boolean_program(&[], span(1), |program, _, outputs| {
                    let value = program.constant(SolveValue::boolean(true), span(2))?;
                    program.store(outputs[0], value, span(3))
                })?;
                method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(4),
                    |arm, _closure| {
                        arm.assign(&[cells.results()[0]], &[], span(5), constant_program)
                    },
                    |arm| {
                        arm.assign(
                            &[cells.results()[0]],
                            &[cells.parameters()[0]],
                            span(6),
                            copy_program,
                        )
                    },
                )
            },
        )?;
        Ok(())
    })
    .expect("a result defined on both arms is complete");
    assert_eq!(complete.methods()[0].body().actions().len(), 1);
}

/// One method whose only structured action is a loop over `domain`.
fn looping_table(
    domain: &StructuredIndexDomain,
) -> Result<SolveMethodTable, SolveActionConstructionError> {
    table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                method.assign(&[cells.results()[0]], &[], span(1), constant_program)?;
                method.iterate(domain.clone(), span(2), |body, _binders| {
                    body.assign(&[cells.results()[0]], &[], span(3), constant_program)
                })
            },
        )?;
        Ok(())
    })
}

#[test]
fn bounded_loops_require_finite_domains_and_admit_zero_trips() {
    let bounded = looping_table(&loop_domain(3, 1)).expect("a finite non-empty domain constructs");
    assert!(matches!(
        bounded.methods()[0].body().actions()[1].action(),
        SolveAction::Loop { .. }
    ));
    // A checked finite GALEC range may execute zero trips (Codex 20:35): the
    // statically-empty domain constructs and the loop body simply never runs.
    let zero_trip =
        looping_table(&loop_domain(0, 1)).expect("a statically empty domain executes zero trips");
    assert!(matches!(
        zero_trip.methods()[0].body().actions()[1].action(),
        SolveAction::Loop { .. }
    ));
    let error = looping_table(&loop_domain(3, 0))
        .expect_err("a zero-step domain is invalid, not zero-trip");
    assert_eq!(
        error,
        SolveActionConstructionError::InvalidLoopDomain {
            provenance: span(2)
        }
    );
}

#[test]
fn branches_require_complete_arms_and_typed_boolean_conditions() {
    let empty = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                method.assign(&[cells.results()[0]], &[], span(1), constant_program)?;
                let condition = method.boolean_program(&[], span(2), |program, _, outputs| {
                    let value = program.constant(SolveValue::boolean(true), span(3))?;
                    program.store(outputs[0], value, span(4))
                })?;
                method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(5),
                    |_arm, _closure| Ok(()),
                    |_arm| Ok(()),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("a branch with two empty arms is not an action");
    assert_eq!(
        empty,
        SolveActionConstructionError::EmptyBranch {
            provenance: span(5)
        }
    );

    let untyped = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let condition =
                    method.value_program(&[], vec![real()], span(1), constant_program)?;
                method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(2),
                    |arm, _closure| {
                        arm.assign(&[cells.results()[0]], &[], span(3), constant_program)
                    },
                    |arm| arm.assign(&[cells.results()[0]], &[], span(4), constant_program),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("only a typed Boolean scalar can drive a branch");
    assert_eq!(
        untyped,
        SolveActionConstructionError::InvalidCondition {
            provenance: span(2)
        }
    );
}

#[test]
fn one_action_cannot_write_the_same_cell_twice() {
    let error = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let result = cells.results()[0];
                method.assign(&[result, result], &[], span(1), constant_program)
            },
        )?;
        Ok(())
    })
    .expect_err("one evaluation cannot define one cell twice");
    assert_eq!(
        error,
        SolveActionConstructionError::DuplicateTarget {
            provenance: span(1)
        }
    );
}

#[test]
fn dummy_provenance_never_commits_a_method() {
    let dummy = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            Span::DUMMY,
            |method, cells| method.assign(&[cells.results()[0]], &[], span(1), constant_program),
        )?;
        Ok(())
    })
    .expect_err("dummy provenance never commits a method");
    assert_eq!(dummy, SolveActionConstructionError::MissingProvenance);
}

/// Every fact the constructors issue is absent from the wire, so bytes cannot
/// even name it: the derived ABI, the scope/cell/closure arenas, the method
/// identity, the loop binders, the caught set, and the issued closure id all
/// fail as unknown fields instead of being accepted and cross-checked.
#[test]
fn wire_carries_constructor_inputs_and_denies_every_derived_fact() {
    let table = controller_table().expect("the checked controller method constructs");
    let json = serde_json::to_value(&table).expect("the method table serializes");
    let method = &json["methods"][0];
    for derived in ["id", "identity", "abi", "scopes", "cells", "closures"] {
        assert!(
            method.get(derived).is_none(),
            "{derived} is a construction result, not a wire input"
        );
    }
    assert!(
        method["body"].get("scope").is_none(),
        "a block scope is issued when the block is opened"
    );
    let loop_action = &method["body"]["actions"][1]["action"];
    assert_eq!(loop_action["action"], serde_json::json!("loop"));
    assert!(
        loop_action.get("binders").is_none(),
        "loop binders are issued by the loop"
    );

    for (pointer, forged) in [
        ("/methods/0/abi", serde_json::json!({ "status": "none" })),
        ("/methods/0/identity", serde_json::json!(7)),
        ("/methods/0/cells", serde_json::json!([])),
        (
            "/methods/0/body/actions/1/action/binders",
            serde_json::json!([0]),
        ),
    ] {
        let mut forged_json = json.clone();
        insert(&mut forged_json, pointer, forged);
        let error = serde_json::from_value::<SolveMethodTable>(forged_json)
            .expect_err("a derived fact cannot be smuggled onto the wire");
        assert!(error.to_string().contains("unknown field"), "{error}");
    }
}

/// Writes `value` at a JSON pointer whose parent already exists.
fn insert(json: &mut serde_json::Value, pointer: &str, value: serde_json::Value) {
    let (parent, key) = pointer.rsplit_once('/').expect("an absolute pointer");
    json.pointer_mut(parent)
        .expect("the parent object exists")
        .as_object_mut()
        .expect("the parent is an object")
        .insert(key.to_owned(), value);
}

/// One method that raises NAN, catches it with a signal check, and limits one
/// explicitly named ranged cell in the arm that did not catch.
fn caught_table() -> Result<SolveMethodTable, SolveActionConstructionError> {
    table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.raise(nan(), &[], span(1))?;
                method.branch(
                    SolveBranchConditionSpec::Signal {
                        test: Some(SolveSignalTest::construct(false, nan(), span(2))?),
                        capture_closure: false,
                        fallback: None,
                    },
                    span(3),
                    |arm, _closure| {
                        arm.assign(&[cells.bindings()[0]], &[], span(4), constant_program)
                    },
                    |arm| arm.limit(&[SolveLimitTargetSpec::Cell(cells.bindings()[0])], span(5)),
                )
            },
        )?;
        Ok(())
    })
}

#[test]
fn wire_rejects_a_forged_caught_signal_set() {
    let caught = caught_table().expect("catching the raised signal discharges the escape set");

    let mut json = serde_json::to_value(&caught).expect("the method table serializes");
    let check = "/methods/0/body/actions/1/action/condition/check";
    assert!(
        json.pointer(&format!("{check}/caught")).is_none(),
        "the caught set is derived from the test and the declared universe"
    );
    insert(&mut json, &format!("{check}/caught"), serde_json::json!({}));
    let error = serde_json::from_value::<SolveMethodTable>(json)
        .expect_err("a forged caught set cannot replay");
    assert!(error.to_string().contains("unknown field"), "{error}");
}

/// A derived fact cannot be smuggled in beside the record that carries its
/// input either: the branch-condition and limit-target records are closed, so
/// caught bits written next to the condition tag and a scope written next to a
/// cell target are unknown fields rather than silently ignored bytes.
#[test]
fn wire_denies_a_derived_fact_smuggled_beside_a_condition_or_limit_target() {
    let caught = caught_table().expect("catching the raised signal discharges the escape set");
    let json = serde_json::to_value(&caught).expect("the method table serializes");
    let branch = "/methods/0/body/actions/1/action";
    let condition = format!("{branch}/condition");
    let target = format!("{branch}/if_false/actions/0/action/targets/0");
    assert_eq!(
        json.pointer(&condition)
            .and_then(|condition| condition.get("condition")),
        Some(&serde_json::json!("signal")),
        "the branch is driven by one catching signal check"
    );
    assert_eq!(
        json.pointer(&target)
            .and_then(|target| target.get("target")),
        Some(&serde_json::json!("cell")),
        "the arm that did not catch limits one named ranged cell"
    );

    // A closed record reports the smuggled key itself: an adjacently tagged
    // one names it as neither its tag nor its content, a struct as an unknown
    // field. Either way the bytes are rejected instead of quietly ignored.
    for (pointer, key, forged) in [
        (&condition, "caught", serde_json::json!({ "bits": 4 })),
        (&target, "scope", serde_json::json!(0)),
    ] {
        let mut forged_json = json.clone();
        insert(&mut forged_json, &format!("{pointer}/{key}"), forged);
        let error = serde_json::from_value::<SolveMethodTable>(forged_json)
            .expect_err("a derived fact cannot be smuggled beside its record");
        assert!(error.to_string().contains(key), "{error}");
    }
}

/// The wire addresses cells, closures, and callees by wire-local ordinal, and
/// an ordinal carries no authority: it selects an entity the constructors
/// already issued, or the replay fails. Solve identities themselves have no
/// decoder at all — the `compile_fail` doctests on `SolveMethodId`,
/// `SolveCellId`, `SolveScopeId`, and `SolveSignalClosureId` prove that the
/// probe `serde_json::from_str::<SolveMethodId>("7")` cannot even be written.
#[test]
fn wire_ordinals_select_issued_entities_and_never_name_them() {
    let caught = caught_table().expect("catching the raised signal discharges the escape set");
    let json = serde_json::to_value(&caught).expect("the method table serializes");
    let target = "/methods/0/body/actions/1/action/if_false/actions/0/action/targets/0/cell";
    assert_eq!(
        json.pointer(target),
        Some(&serde_json::json!(0)),
        "the limited cell is named by its wire-local ordinal"
    );

    let mut forged = json.clone();
    insert(&mut forged, target, serde_json::json!(9));
    let error = serde_json::from_value::<SolveMethodTable>(forged)
        .expect_err("an ordinal cannot address a cell the method never issued");
    assert!(
        error
            .to_string()
            .contains("cell is not owned by this method"),
        "{error}"
    );

    let replayed: SolveMethodTable =
        serde_json::from_value(json).expect("issued ordinals replay through construction");
    assert_eq!(replayed, caught);
}

/// Commits one action into the arm and then fails, leaving the arm open.
fn abandon_arm<'method>(
    arm: &mut SolveMethodBuilder<'method>,
    result: MethodCell<'method>,
) -> Result<(), SolveActionConstructionError> {
    arm.assign(&[result], &[], span(5), constant_program)?;
    Err(SolveActionConstructionError::MissingProvenance)
}

/// SPEC_0036: a nested block left open by a swallowed action error is a typed
/// construction error, never an assertion. With assertions compiled out the
/// abandoned arm would otherwise be popped and published as the method body.
#[test]
fn a_swallowed_arm_error_cannot_become_the_method_body() {
    let root = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let condition = boolean_constant(method, span(1))?;
                let swallowed = method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(4),
                    |arm, _closure| abandon_arm(arm, cells.results()[0]),
                    |_arm| Ok(()),
                );
                assert_eq!(
                    swallowed,
                    Err(SolveActionConstructionError::MissingProvenance)
                );
                method.assign(&[cells.results()[0]], &[], span(6), constant_program)
            },
        )?;
        Ok(())
    })
    .expect_err("an abandoned arm cannot silently become the body");
    assert_eq!(
        root,
        SolveActionConstructionError::UnclosedBlock {
            provenance: span(0)
        }
    );

    let nested = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let outer = boolean_constant(method, span(1))?;
                method.branch(
                    SolveBranchConditionSpec::Value(outer),
                    span(2),
                    |arm, _closure| {
                        let inner = boolean_constant(arm, span(3))?;
                        let swallowed = arm.branch(
                            SolveBranchConditionSpec::Value(inner),
                            span(4),
                            |inner_arm, _closure| abandon_arm(inner_arm, cells.results()[0]),
                            |_inner_arm| Ok(()),
                        );
                        assert!(swallowed.is_err());
                        Ok(())
                    },
                    |arm| arm.assign(&[cells.results()[0]], &[], span(6), constant_program),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("an abandoned inner arm cannot be committed as its parent");
    assert_eq!(
        nested,
        SolveActionConstructionError::UnclosedBlock {
            provenance: span(2)
        }
    );
}

/// A method table is a reusable vocabulary, never a GALEC lifecycle root.
///
/// Vocabulary validity and root completeness are separate obligations: an empty
/// table and an arbitrary one-helper table are both valid vocabularies, and
/// neither exposes a lifecycle role, so no consumer can read a root out of one.
/// The lifecycle roles, their cardinality, and their interfaces belong to the
/// future checked `SolveAlgorithmBlock::construct`, whose absence is why the
/// negative obligation is that no lifecycle is *presentable* here. The
/// compile-fail doctest on `SolveMethodTable` enforces that surface.
#[test]
fn a_method_table_is_a_vocabulary_and_presents_no_lifecycle_root() {
    let empty = table(|_methods| Ok(())).expect("an empty vocabulary is a valid vocabulary");
    assert!(empty.methods().is_empty());
    let json = serde_json::to_string(&empty).expect("the empty table serializes");
    let replayed: SolveMethodTable =
        serde_json::from_str(&json).expect("the empty table replays through construction");
    assert_eq!(replayed, empty);

    let arbitrary = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateless,
            SolveMethodInterface::construct(
                vec![real()],
                vec![real()],
                Vec::new(),
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.assign(
                    &[cells.results()[0]],
                    &[cells.parameters()[0]],
                    span(1),
                    copy_program,
                )
            },
        )?;
        Ok(())
    })
    .expect("one arbitrary helper is a valid vocabulary too");
    assert_eq!(arbitrary.methods().len(), 1);

    // Neither table carries any lifecycle role, so the two are distinguished
    // only by the methods they define.
    for table in [&empty, &arbitrary] {
        let json = serde_json::to_value(table).expect("the table serializes");
        let object = json.as_object().expect("the table is an object");
        assert_eq!(
            object.keys().collect::<Vec<_>>(),
            ["arithmetic", "signals", "calls", "methods"],
            "a table records a vocabulary, never a lifecycle claim"
        );
    }
}

/// SPEC_0042 T9/T10/T14: a Real comparison signals NAN and is therefore the
/// only reason a `in Nan` guard is admissible; a guard with no raising path is
/// a construction error, and IEEE-754 division stays silent.
#[test]
fn signal_checks_may_only_test_signals_that_can_be_active() {
    let unraisable = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.branch(
                    SolveBranchConditionSpec::Signal {
                        test: Some(SolveSignalTest::construct(false, nan(), span(1))?),
                        capture_closure: false,
                        fallback: None,
                    },
                    span(2),
                    |arm, _closure| {
                        arm.assign(&[cells.bindings()[0]], &[], span(3), constant_program)
                    },
                    |arm| arm.limit(&[SolveLimitTargetSpec::RangedState], span(4)),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("no path can raise NAN before this guard");
    assert_eq!(
        unraisable,
        SolveActionConstructionError::UnsettableSignalTest {
            provenance: span(2)
        }
    );

    let settled = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                vec![real()],
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                let comparison = compare_parameter(method, cells.parameters()[0], span(1))?;
                method.branch(
                    SolveBranchConditionSpec::Value(comparison),
                    span(2),
                    |arm, _closure| {
                        arm.assign(&[cells.bindings()[0]], &[], span(3), constant_program)
                    },
                    |arm| arm.limit(&[SolveLimitTargetSpec::RangedState], span(4)),
                )?;
                method.branch(
                    SolveBranchConditionSpec::Signal {
                        test: Some(SolveSignalTest::construct(false, nan(), span(6))?),
                        capture_closure: false,
                        fallback: None,
                    },
                    span(7),
                    |arm, _closure| {
                        arm.assign(&[cells.bindings()[0]], &[], span(8), constant_program)
                    },
                    |arm| arm.limit(&[SolveLimitTargetSpec::RangedState], span(9)),
                )
            },
        )?;
        Ok(())
    });
    let settled = settled.expect("a Real comparison makes NAN settable, and the guard catches it");
    assert_eq!(
        settled.methods()[0].interface().escapes(),
        SolveSignalSet::EMPTY,
        "the guard discharges exactly the NAN the comparison raised"
    );
}

/// One block that guards on NAN and only afterwards raises it, so the guard is
/// settable exactly when some earlier trip already ran this block.
fn check_then_raise_nan<'method>(
    block: &mut SolveMethodBuilder<'method>,
    state: MethodCell<'method>,
) -> Result<(), SolveActionConstructionError> {
    block.branch(
        SolveBranchConditionSpec::Signal {
            test: Some(SolveSignalTest::construct(false, nan(), span(2))?),
            capture_closure: false,
            fallback: None,
        },
        span(3),
        |arm, _closure| arm.assign(&[state], &[], span(4), constant_program),
        |arm| arm.limit(&[SolveLimitTargetSpec::RangedState], span(5)),
    )?;
    block.raise(nan(), &[], span(6))
}

/// One method whose loop body checks NAN before the only action that raises
/// it, over a domain of exactly `trips` iterations.
fn looping_signal_check(
    trips: i64,
    escapes: SolveSignalSet,
) -> Result<SolveMethodTable, SolveActionConstructionError> {
    table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(Vec::new(), Vec::new(), vec![ranged_state()], escapes),
            span(0),
            |method, cells| {
                let state = cells.bindings()[0];
                method.iterate(loop_domain(trips, 1), span(1), |body, _binders| {
                    check_then_raise_nan(body, state)
                })
            },
        )?;
        Ok(())
    })
}

/// SPEC_0042 T10 inside a loop: the check is proved against exactly the states
/// the declared trip count can reach.
///
/// A one-trip body can only ever start from the loop entry, so a guard whose
/// only raise follows it tests a signal that cannot be active and is a typed
/// construction error — the conservative fixpoint used to excuse it with a
/// second iteration that never runs. Two trips genuinely reach the guard with
/// the previous iteration's signal active, so the same body is admissible
/// there and escapes exactly what it raised.
#[test]
fn a_signal_check_in_a_one_trip_loop_cannot_test_a_later_raise() {
    let single = looping_signal_check(1, nan())
        .expect_err("a single-trip body never reaches its guard with NAN active");
    assert_eq!(
        single,
        SolveActionConstructionError::UnsettableSignalTest {
            provenance: span(3)
        }
    );

    let repeated = looping_signal_check(2, nan())
        .expect("a second trip reaches the guard with the first trip's NAN active");
    assert_eq!(repeated.methods()[0].interface().escapes(), nan());

    let uncaught = looping_signal_check(2, SolveSignalSet::EMPTY)
        .expect_err("the guard catches one trip's NAN, and the last trip's still escapes");
    assert_eq!(
        uncaught,
        SolveActionConstructionError::EscapeSetMismatch {
            provenance: span(0)
        }
    );
}

/// One method that raises NAN and then catches it inside a loop of `trips`
/// iterations, declaring that nothing escapes.
fn loop_catches_earlier_raise(
    trips: i64,
) -> Result<SolveMethodTable, SolveActionConstructionError> {
    table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                Vec::new(),
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                let state = cells.bindings()[0];
                method.raise(nan(), &[], span(1))?;
                method.iterate(loop_domain(trips, 1), span(2), |body, _binders| {
                    body.branch(
                        SolveBranchConditionSpec::Signal {
                            test: Some(SolveSignalTest::construct(false, nan(), span(3))?),
                            capture_closure: false,
                            fallback: None,
                        },
                        span(4),
                        |arm, _closure| arm.assign(&[state], &[], span(5), constant_program),
                        |arm| arm.limit(&[SolveLimitTargetSpec::RangedState], span(6)),
                    )
                })
            },
        )?;
        Ok(())
    })
}

/// The same declared trip count decides what the loop leaves active.
///
/// A loop the domain proves runs at least once leaves exactly what its last
/// trip left, so a body that catches the signal raised before the loop
/// discharges it. Re-joining the loop entry state would instead force this
/// method to declare an escape it cannot have — the false rejection dual of
/// the false acceptance above.
#[test]
fn a_loop_that_catches_discharges_the_signal_raised_before_it() {
    for trips in [1, 3] {
        let caught = loop_catches_earlier_raise(trips)
            .expect("a loop body that catches the raised signal discharges it");
        assert_eq!(
            caught.methods()[0].interface().escapes(),
            SolveSignalSet::EMPTY,
            "the {trips}-trip loop leaves exactly what its last trip left"
        );
    }
}

/// SPEC_0042 T9: every Real relational operator is an effect, so a method that
/// compares Reals escapes NAN unless it catches it.
#[test]
fn a_real_comparison_escapes_nan() {
    let error = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                vec![real()],
                Vec::new(),
                vec![ranged_state()],
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                let condition = compare_parameter(method, cells.parameters()[0], span(1))?;
                method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(5),
                    |arm, _closure| {
                        arm.assign(&[cells.bindings()[0]], &[], span(6), constant_program)
                    },
                    |arm| arm.limit(&[SolveLimitTargetSpec::RangedState], span(7)),
                )
            },
        )?;
        Ok(())
    })
    .expect_err("a Real comparison raises NAN on a qNaN operand");
    assert_eq!(
        error,
        SolveActionConstructionError::EscapeSetMismatch {
            provenance: span(0)
        }
    );
}

/// SPEC_0042 T14: `integer()` signals, but Solve owns no adjudicated signal row
/// for it, so construction fails closed instead of under-approximating the
/// escape set with a guessed signal.
#[test]
fn a_signalling_conversion_without_a_named_signal_row_fails_closed() {
    let error = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            SolveMethodInterface::construct(
                vec![real()],
                vec![integer()],
                Vec::new(),
                SolveSignalSet::EMPTY,
            ),
            span(0),
            |method, cells| {
                method.assign(
                    &[cells.results()[0]],
                    &[cells.parameters()[0]],
                    span(1),
                    |program, inputs, outputs| {
                        let value = program.load(inputs[0], span(2))?;
                        let truncated = program.convert(
                            SolveConversionOperator::RealToIntegerTowardZero,
                            value,
                            span(3),
                        )?;
                        program.store(outputs[0], truncated, span(4))
                    },
                )
            },
        )?;
        Ok(())
    })
    .expect_err("an unnamed signal row cannot be silently dropped");
    assert_eq!(
        error,
        SolveActionConstructionError::UnnamedSignalEffect {
            provenance: span(1)
        }
    );
}

/// Codex 20:35 stop-ship red fixture: both arms close SUCCESSFULLY after
/// declaring only locals, so the arenas already hold their scopes when the
/// post-close `EmptyBranch` validation fires. A caller that swallows that
/// error leaves the block stack balanced; `finish` must still fail because
/// the body cannot reach the orphaned arm scopes.
#[test]
fn a_swallowed_empty_branch_cannot_publish_orphan_scopes() {
    let root = table(|methods| {
        methods.add_method(
            SolveMethodKind::Stateful,
            controller_interface(SolveSignalSet::EMPTY),
            span(0),
            |method, cells| {
                let condition = boolean_constant(method, span(1))?;
                let swallowed = method.branch(
                    SolveBranchConditionSpec::Value(condition),
                    span(4),
                    |arm, _closure| arm.declare_local(real(), None, span(5)).map(|_| ()),
                    |arm| arm.declare_local(real(), None, span(6)).map(|_| ()),
                );
                assert_eq!(
                    swallowed,
                    Err(SolveActionConstructionError::EmptyBranch {
                        provenance: span(4)
                    })
                );
                method.assign(&[cells.results()[0]], &[], span(7), constant_program)
            },
        )?;
        Ok(())
    })
    .expect_err("swallowed empty-branch arenas must not publish");
    assert_eq!(
        root,
        SolveActionConstructionError::OrphanedConstruction {
            provenance: span(0)
        }
    );
}
