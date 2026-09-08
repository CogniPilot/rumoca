//! Owner-marked wire replay of dynamic runtime quotients.
//!
//! A model quotient's generated indicator batch, relation, activation
//! definition, and root are absent from the wire: one owner-marked
//! expression record plus three positional stream markers carry only
//! semantic inputs, and the staged token regenerates every produced
//! identity through the same checked constructors. These pins prove the
//! round trip is dense and idempotent, that a function-owned quotient
//! replays through its open body without any event surface, and that every
//! marker adversary — duplicate, missing, wrong kind, forged activation —
//! is an exact typed rejection with no partially consumed owner reaching a
//! finalized DAE.

use super::*;

/// A model DAE with one owned dynamic quotient, one generic condition
/// definition, and one ordinary post-batch expression referenced by an
/// initialization equation.
fn model_owner_fixture() -> Dae {
    let source = TestSource::new("Real x; mod(x, 2); initial x; x + 1");
    let declaration = source.source("Real x", 0);
    let x_at = source.source("x", 1);
    let two_at = source.source("2", 0);
    let mod_at = source.source("mod(x, 2)", 0);
    let initial_at = source.source("initial x", 0);
    let post_x_at = source.source("x", 3);
    let one_at = source.source("1", 0);
    let sum_at = source.source("x + 1", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x_use, two) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_at)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions.at(two_at).literal(DaeLiteral::Integer(2))?,
            ))
        })?;
        dae.runtime_quotient(PureBuiltin::Mod, [x_use, two], mod_at)?;
        // A generic reserved-and-defined condition alongside the owner's.
        dae.conditions(|conditions| {
            let generic = conditions.reserve(initial_at)?;
            conditions.define(generic, ConditionInput::Initial, initial_at)
        })?;
        // An ordinary post-batch expression referenced by an equation, so
        // the round trip proves later source ordinals remap onto ordinary
        // target nodes across the widened batch.
        let sum = dae.expressions(|expressions| {
            let post_x = expressions
                .at(post_x_at)
                .coordinate(CoordinateInput::Algebraic(x))?;
            let one = expressions.at(one_at).literal(DaeLiteral::Real(1.0))?;
            expressions
                .at(sum_at)
                .binary(BinaryOperator::Add, post_x, one)
        })?;
        dae.initialization(|initialization| {
            initialization
                .equation(sum_at, |equation| equation.residual(sum))
                .map(|_| ())
        })?;
        Ok(())
    })
    .expect("a model quotient owner constructs completely")
}

fn function_owner_fixture() -> Dae {
    let source = TestSource::new(
        "function f\n input Real u;\n output Real y;\nalgorithm\n y := mod(u, 2);\nend f;",
    );
    let function_at = source.source("function f", 0);
    let parameter_at = source.source("input Real u", 0);
    let output_at = source.source("output Real y", 0);
    let assignment_at = source.source("y := mod(u, 2)", 0);
    let parameter_use = source.source("u", 1);
    let two_at = source.source("2", 0);
    let quotient_at = source.source("mod(u, 2)", 0);
    Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let u = dae.expressions(|expressions| {
                    expressions.at(parameter_use).function_parameter(parameter)
                })?;
                let two = dae.expressions(|expressions| {
                    expressions.at(two_at).literal(DaeLiteral::Integer(2))
                })?;
                let quotient =
                    dae.function_runtime_quotient(&body, PureBuiltin::Mod, [u, two], quotient_at)?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, quotient, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("a function quotient owner constructs completely")
}

fn assert_model_owner_shape(dae: &Dae) {
    dae.inspect(|view| {
        assert_eq!(view.root_count(), 1);
        assert_eq!(view.relation_count(), 1);
        assert_eq!(view.condition_count(), 2);
        assert_eq!(view.runtime_quotient_owner_count(), 1);
        let owner = view
            .runtime_quotient_owner_at(0)
            .expect("the owner registry survives");
        assert_eq!(owner.builtin(), PureBuiltin::Mod);
        let RuntimeQuotientOwnerKind::ModelEvent {
            generated,
            relation,
            activation,
            root,
        } = owner.kind()
        else {
            panic!("the model owner keeps its event kind");
        };
        // Batch contiguity and dense identity.
        let quotient = owner.quotient().index();
        for (offset, id) in generated.into_iter().enumerate() {
            assert_eq!(id.index(), quotient + 1 + offset as u32);
        }
        let viewed_root = view
            .root(view.root_id(0).expect("dense root"))
            .expect("root");
        assert_eq!(viewed_root.relation(), relation);
        assert_eq!(viewed_root.activation(), activation);
        assert_eq!(root.index(), 0);
        assert!(matches!(
            view.condition(activation).expect("activation").operation(),
            ConditionOperation::Always
        ));
        // The post-batch reference reached its ordinary target node.
        let last = view
            .expression_id(view.expression_count() - 1)
            .expect("dense expressions");
        assert!(matches!(
            view.expression(last).expect("sum").operation(),
            ExpressionOperation::Binary {
                operator: BinaryOperator::Add,
                ..
            }
        ));
    });
}

#[test]
fn model_owner_round_trips_densely_and_idempotently() {
    let dae = model_owner_fixture();
    assert_model_owner_shape(&dae);
    let encoded = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    assert_model_owner_shape(&replayed);
    dae.inspect(|source_view| {
        replayed.inspect(|replayed_view| {
            assert_eq!(
                source_view.expression_count(),
                replayed_view.expression_count()
            );
        });
    });
    let re_encoded = serde_json::to_string(&replayed).unwrap();
    assert_eq!(
        encoded, re_encoded,
        "owner replay re-serializes identically"
    );
}

#[test]
fn function_owner_round_trips_event_free() {
    let dae = function_owner_fixture();
    let encoded = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    replayed.inspect(|view| {
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.condition_count(), 0);
        assert_eq!(view.runtime_quotient_owner_count(), 1);
        let owner = view
            .runtime_quotient_owner_at(0)
            .expect("the function owner registry survives");
        assert!(matches!(
            owner.kind(),
            RuntimeQuotientOwnerKind::FunctionBody { .. }
        ));
    });
    let re_encoded = serde_json::to_string(&replayed).unwrap();
    assert_eq!(encoded, re_encoded);
}

fn model_owner_wire() -> serde_json::Value {
    serde_json::to_value(model_owner_fixture()).expect("checked DAE serializes")
}

fn expect_rejected(wire: serde_json::Value) -> String {
    serde_json::from_value::<Dae>(wire)
        .expect_err("a forged owner wire cannot replay")
        .to_string()
}

#[test]
fn forged_activation_reservation_rejects() {
    let mut wire = model_owner_wire();
    // Point the owner record's activation input at the generically defined
    // condition; the marker no longer sits at the named reservation.
    let nodes = &mut wire["storage"]["expressions"]["nodes"];
    let owner = nodes
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find(|node| node.get("runtime_quotient_owner").is_some())
        .expect("the wire carries one owner record");
    owner["runtime_quotient_owner"]["kind"]["model"]["activation"] = serde_json::Value::from(1_u32);
    let message = expect_rejected(wire);
    assert!(message.contains("conditions.quotient_owner"), "{message}");
}

#[test]
fn duplicate_root_marker_rejects() {
    let mut wire = model_owner_wire();
    let roots = wire["storage"]["roots"].as_array_mut().unwrap();
    let marker = roots[0].clone();
    roots.push(marker);
    let message = expect_rejected(wire);
    assert!(message.contains("replay stage"), "{message}");
}

#[test]
fn missing_root_marker_leaves_the_token_unconsumed_and_rejects() {
    let mut wire = model_owner_wire();
    wire["storage"]["roots"].as_array_mut().unwrap().clear();
    let message = expect_rejected(wire);
    assert!(message.contains("never completed"), "{message}");
}

#[test]
fn marker_for_a_function_owner_rejects() {
    let mut wire = serde_json::to_value(function_owner_fixture()).unwrap();
    // A function owner stages no token; any stream marker naming it is
    // forged.
    wire["storage"]["relations"]
        .as_array_mut()
        .unwrap()
        .push(serde_json::json!({ "quotient_owner": { "owner": 0 } }));
    let message = expect_rejected(wire);
    assert!(message.contains("quotient_owner"), "{message}");
}

#[test]
fn record_inserted_into_the_batch_range_rejects() {
    let mut wire = model_owner_wire();
    // Duplicate the ordinary post-batch coordinate record directly after
    // the owner record: every following source ordinal shifts into the
    // batch range and the dense identity checks reject.
    let nodes = wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap();
    let owner_index = nodes
        .iter()
        .position(|node| node.get("runtime_quotient_owner").is_some())
        .expect("the wire carries one owner record");
    let forged = nodes[owner_index + 1].clone();
    nodes.insert(owner_index + 1, forged);
    let message = expect_rejected(wire);
    assert!(message.contains("expressions"), "{message}");
}

#[test]
fn an_unfinished_replay_cannot_reach_a_finalized_dae() {
    let source = TestSource::new("Real x; mod(x, 2)");
    let declaration = source.source("Real x", 0);
    let x_at = source.source("x", 1);
    let two_at = source.source("2", 0);
    let mod_at = source.source("mod(x, 2)", 0);
    let error = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x_use, two) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_at)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions.at(two_at).literal(DaeLiteral::Integer(2))?,
            ))
        })?;
        let _token = dae.begin_quotient_replay(PureBuiltin::Mod, [x_use, two], mod_at)?;
        Ok(())
    })
    .expect_err("a begun replay must finish before the DAE finalizes");
    assert!(matches!(
        error,
        DaeConstructionError::UnconsumedQuotientReplay { .. }
    ));
}

#[test]
fn replay_stages_enforce_source_order() {
    let source = TestSource::new("Real x; mod(x, 2)");
    let declaration = source.source("Real x", 0);
    let x_at = source.source("x", 1);
    let two_at = source.source("2", 0);
    let mod_at = source.source("mod(x, 2)", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x_use, two) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_at)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions.at(two_at).literal(DaeLiteral::Integer(2))?,
            ))
        })?;
        let mut token = dae.begin_quotient_replay(PureBuiltin::Mod, [x_use, two], mod_at)?;
        // Root before relation/activation violates the protocol.
        assert!(matches!(
            dae.replay_quotient_root(&mut token),
            Err(DaeConstructionError::InvalidQuotientReplayStage { stage: "root", .. })
        ));
        let relation = dae.replay_quotient_relation(&mut token)?;
        // The relation stage consumes exactly once.
        assert!(matches!(
            dae.replay_quotient_relation(&mut token),
            Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "relation",
                ..
            })
        ));
        // The reservation must carry the token's exact canonical generated
        // provenance; the source-span reservation is rejected.
        let source_reserved = dae.conditions(|conditions| conditions.reserve(mod_at))?;
        assert!(matches!(
            dae.replay_quotient_activation(&mut token, source_reserved),
            Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "activation",
                ..
            })
        ));
        dae.conditions(|conditions| {
            conditions.define(source_reserved, ConditionInput::Initial, mod_at)
        })?;
        let reserved_at = token.provenance();
        let activation = dae.conditions(|conditions| conditions.reserve(reserved_at))?;
        dae.replay_quotient_activation(&mut token, activation)?;
        let root = dae.replay_quotient_root(&mut token)?;
        assert_eq!(relation.index(), 0);
        assert_eq!(root.index(), 0);
        dae.finish_quotient_replay(token)
    })
    .expect("a completed staged replay finalizes");
}

#[test]
fn tampered_activation_reservation_span_rejects_on_the_wire() {
    // The [100] adversary: the owner activation's reserved provenance is an
    // owner-produced fact the wire must not vary. Shifting its span makes
    // the staged activation replay reject before defining.
    let mut wire = model_owner_wire();
    let conditions = wire["storage"]["conditions"].as_array_mut().unwrap();
    let marker_entry = conditions
        .iter_mut()
        .find(|entry| entry["node"].get("quotient_owner").is_some())
        .expect("the wire carries the owner activation marker");
    let start = marker_entry["provenance"]["span"]["start"]
        .as_u64()
        .expect("reservation span start");
    marker_entry["provenance"]["span"]["start"] = serde_json::Value::from(start + 1);
    let message = expect_rejected(wire);
    assert!(message.contains("replay stage"), "{message}");
}

/// One DAE holding a function-owned quotient (owner ordinal 0) and a later
/// model-owned quotient (owner ordinal 1).
fn mixed_owner_fixture() -> Dae {
    let source = TestSource::new(
        "function f\n input Real u;\n output Real y;\nalgorithm\n y := mod(u, 2);\nend f;\nReal x; mod(x, 2)",
    );
    let function_at = source.source("function f", 0);
    let parameter_at = source.source("input Real u", 0);
    let output_at = source.source("output Real y", 0);
    let assignment_at = source.source("y := mod(u, 2)", 0);
    let parameter_use = source.source("u", 1);
    let function_two_at = source.source("2", 0);
    let function_mod_at = source.source("mod(u, 2)", 0);
    let declaration = source.source("Real x", 0);
    let x_at = source.source("x", 1);
    let model_two_at = source.source("2", 1);
    let model_mod_at = source.source("mod(x, 2)", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let u = dae.expressions(|expressions| {
                    expressions.at(parameter_use).function_parameter(parameter)
                })?;
                let two = dae.expressions(|expressions| {
                    expressions
                        .at(function_two_at)
                        .literal(DaeLiteral::Integer(2))
                })?;
                let quotient = dae.function_runtime_quotient(
                    &body,
                    PureBuiltin::Mod,
                    [u, two],
                    function_mod_at,
                )?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, quotient, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x_use, two) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_at)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions
                    .at(model_two_at)
                    .literal(DaeLiteral::Integer(2))?,
            ))
        })?;
        dae.runtime_quotient(PureBuiltin::Mod, [x_use, two], model_mod_at)?;
        Ok(())
    })
    .expect("a mixed-kind owner DAE constructs completely")
}

fn assert_mixed_owner_shape(dae: &Dae) {
    dae.inspect(|view| {
        assert_eq!(view.runtime_quotient_owner_count(), 2);
        let first = view.runtime_quotient_owner_at(0).expect("function owner");
        let second = view.runtime_quotient_owner_at(1).expect("model owner");
        assert!(matches!(
            first.kind(),
            RuntimeQuotientOwnerKind::FunctionBody { .. }
        ));
        let RuntimeQuotientOwnerKind::ModelEvent { generated, .. } = second.kind() else {
            panic!("the later quotient keeps its model owner");
        };
        assert!(first.quotient().index() < second.quotient().index());
        let quotient = second.quotient().index();
        for (offset, id) in generated.into_iter().enumerate() {
            assert_eq!(id.index(), quotient + 1 + offset as u32);
        }
        assert_eq!(view.root_count(), 1);
    });
}

#[test]
fn mixed_owner_kinds_round_trip_with_one_global_ordinal() {
    let dae = mixed_owner_fixture();
    assert_mixed_owner_shape(&dae);
    let encoded = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    assert_mixed_owner_shape(&replayed);
    let re_encoded = serde_json::to_string(&replayed).unwrap();
    assert_eq!(encoded, re_encoded, "mixed owners re-serialize identically");
}

#[test]
fn marker_forged_onto_the_function_owner_ordinal_rejects() {
    // The [97] adversary: the model owner's relation marker (naming global
    // ordinal 1) is forged to name the function owner's ordinal 0, which
    // stages no token.
    let mut wire = serde_json::to_value(mixed_owner_fixture()).unwrap();
    let relations = wire["storage"]["relations"].as_array_mut().unwrap();
    let marker = relations
        .iter_mut()
        .find(|entry| entry.get("quotient_owner").is_some())
        .expect("the wire carries the model relation marker");
    marker["quotient_owner"]["owner"] = serde_json::Value::from(0_u32);
    let message = expect_rejected(wire);
    assert!(message.contains("quotient_owner"), "{message}");
}

/// One DAE holding a model-owned quotient FIRST and a function-owned
/// quotient later, so replay records the function owner during expression
/// reconstruction while the earlier model owner finishes only after roots.
fn model_before_function_fixture() -> Dae {
    let source = TestSource::new(
        "Real x; mod(x, 2)\nfunction f\n input Real u;\n output Real y;\nalgorithm\n y := mod(u, 3);\nend f;",
    );
    let declaration = source.source("Real x", 0);
    let x_at = source.source("x", 1);
    let model_two_at = source.source("2", 0);
    let model_mod_at = source.source("mod(x, 2)", 0);
    let function_at = source.source("function f", 0);
    let parameter_at = source.source("input Real u", 0);
    let output_at = source.source("output Real y", 0);
    let assignment_at = source.source("y := mod(u, 3)", 0);
    let parameter_use = source.source("u", 1);
    let function_three_at = source.source("3", 0);
    let function_mod_at = source.source("mod(u, 3)", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x_use, two) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_at)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions
                    .at(model_two_at)
                    .literal(DaeLiteral::Integer(2))?,
            ))
        })?;
        dae.runtime_quotient(PureBuiltin::Mod, [x_use, two], model_mod_at)?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let u = dae.expressions(|expressions| {
                    expressions.at(parameter_use).function_parameter(parameter)
                })?;
                let three = dae.expressions(|expressions| {
                    expressions
                        .at(function_three_at)
                        .literal(DaeLiteral::Integer(3))
                })?;
                let quotient = dae.function_runtime_quotient(
                    &body,
                    PureBuiltin::Mod,
                    [u, three],
                    function_mod_at,
                )?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, quotient, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )?;
        Ok(())
    })
    .expect("a model-then-function owner DAE constructs completely")
}

fn assert_model_then_function_order(dae: &Dae) {
    dae.inspect(|view| {
        assert_eq!(view.runtime_quotient_owner_count(), 2);
        let first = view.runtime_quotient_owner_at(0).expect("first owner");
        let second = view.runtime_quotient_owner_at(1).expect("second owner");
        assert!(
            matches!(first.kind(), RuntimeQuotientOwnerKind::ModelEvent { .. }),
            "the earlier quotient keeps public ordinal 0"
        );
        assert!(matches!(
            second.kind(),
            RuntimeQuotientOwnerKind::FunctionBody { .. }
        ));
        assert!(first.quotient().index() < second.quotient().index());
        // The expression lookup answers both exact entries.
        assert!(
            view.runtime_quotient_owner(first.quotient())
                .is_some_and(|owner| owner.quotient() == first.quotient())
        );
        assert!(
            view.runtime_quotient_owner(second.quotient())
                .is_some_and(|owner| owner.quotient() == second.quotient())
        );
    });
}

#[test]
fn model_before_function_owner_keeps_canonical_public_order() {
    // The [102] adversary: replay records the function owner during
    // expression reconstruction and the earlier model owner only after its
    // root marker; the public registry order must not depend on that
    // marker timing.
    let dae = model_before_function_fixture();
    assert_model_then_function_order(&dae);
    let encoded = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    assert_model_then_function_order(&replayed);
    let re_encoded = serde_json::to_string(&replayed).unwrap();
    assert_eq!(encoded, re_encoded);
}

#[test]
fn two_live_replays_finish_out_of_order_by_exact_slot_identity() {
    // The [95] adversary: two tokens live at once, finished in reverse
    // begin order. Each finish settles exactly its own construction-issued
    // pending slot — never a stack pop — so finalization proves both owners
    // completed and records both registry entries.
    let source = TestSource::new("Real x; mod(x, 2); mod(x, 3)");
    let declaration = source.source("Real x", 0);
    let x_at = source.source("x", 1);
    let two_at = source.source("2", 0);
    let three_at = source.source("3", 0);
    let first_at = source.source("mod(x, 2)", 0);
    let second_at = source.source("mod(x, 3)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (x_use, two, three) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(x_at)
                    .coordinate(CoordinateInput::Algebraic(x))?,
                expressions.at(two_at).literal(DaeLiteral::Integer(2))?,
                expressions.at(three_at).literal(DaeLiteral::Integer(3))?,
            ))
        })?;
        let mut first = dae.begin_quotient_replay(PureBuiltin::Mod, [x_use, two], first_at)?;
        let mut second = dae.begin_quotient_replay(PureBuiltin::Mod, [x_use, three], second_at)?;
        // A reservation whose stored provenance is not this token's exact
        // canonical generated provenance must be rejected before defining.
        let foreign = dae.conditions(|conditions| conditions.reserve(first_at))?;
        dae.replay_quotient_relation(&mut second)?;
        assert!(matches!(
            dae.replay_quotient_activation(&mut second, foreign),
            Err(DaeConstructionError::InvalidQuotientReplayStage {
                stage: "activation",
                ..
            })
        ));
        // Settle the foreign reservation generically so construction closes.
        dae.conditions(|conditions| conditions.define(foreign, ConditionInput::Initial, first_at))?;
        dae.replay_quotient_relation(&mut first)?;
        for token in [&mut second, &mut first] {
            let reserved_at = token.provenance();
            let activation = dae.conditions(|conditions| conditions.reserve(reserved_at))?;
            dae.replay_quotient_activation(token, activation)?;
            dae.replay_quotient_root(token)?;
        }
        dae.finish_quotient_replay(second)?;
        dae.finish_quotient_replay(first)
    })
    .expect("out-of-order finishes settle their exact slots");
    let assert_canonical = |dae: &Dae| {
        dae.inspect(|view| {
            assert_eq!(view.runtime_quotient_owner_count(), 2);
            assert_eq!(view.root_count(), 2);
            // Public ordinals follow quotient-expression order, not the
            // reversed finish order.
            let first = view.runtime_quotient_owner_at(0).expect("first owner");
            let second = view.runtime_quotient_owner_at(1).expect("second owner");
            assert!(first.quotient().index() < second.quotient().index());
            assert!(
                view.runtime_quotient_owner(first.quotient())
                    .is_some_and(|owner| owner.quotient() == first.quotient())
            );
            assert!(
                view.runtime_quotient_owner(second.quotient())
                    .is_some_and(|owner| owner.quotient() == second.quotient())
            );
        });
    };
    assert_canonical(&dae);
    let encoded = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    assert_canonical(&replayed);
}
