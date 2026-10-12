use super::*;

mod guard_dependencies;
mod operand_scope;
mod structured_relations;
mod view_dependencies;

fn conditional_fixture() -> Dae {
    let source = TestSource::new("if c then 1 else 2");
    let at = source.source("if c then 1 else 2", 0);
    Dae::construct(source.map, |dae| {
        let condition = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let owner = dae.begin_model_conditional(condition, at)?;
        let first = dae.model_conditional_arm(&owner, true, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(1.0)))
        })?;
        let second = dae.model_conditional_arm(&owner, false, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(2.0)))
        })?;
        dae.finish_model_conditional(owner, [first, second])?;
        Ok(())
    })
    .expect("checked arms close once")
}

fn assert_closed_guard_operations(dae: &Dae) {
    dae.inspect(|view| {
        for index in 0..view.model_conditional_count() {
            let owner = view.model_conditional_at(index).unwrap();
            let operations: Vec<_> = owner.guard_operations().collect();
            assert!(
                matches!(view.expression(operations[0]).unwrap().operation(),
                ExpressionOperation::Unary { operator: UnaryOperator::Not, operand }
                    if operand == owner.condition())
            );
            let expected_tail = if owner.parent_arm().is_some() {
                owner.guards().to_vec()
            } else {
                Vec::new()
            };
            assert_eq!(&operations[1..], expected_tail.as_slice());
            assert!(matches!(
                view.expression(owner.expression()).unwrap().operation(),
                ExpressionOperation::Conditional(_)
            ));
        }
    });
}

#[test]
fn unfinished_active_model_arm_reports_its_issued_source() {
    let source = TestSource::new("if c then 1 else 2");
    let at = source.source("if c then 1 else 2", 0);
    let unfinished = Dae::construct(source.map, |dae| {
        let condition = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        dae.begin_model_conditional(condition, at)?;
        Ok(())
    });
    assert!(matches!(unfinished,
        Err(DaeConstructionError::IncompleteDefinition { kind: "model conditional", index: 0, span })
            if span == at.span()));

    let source = TestSource::new("if c then 1 else 2");
    let at = source.source("if c then 1 else 2", 0);
    let result = Dae::construct(source.map, |dae| {
        let condition = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let owner = dae.begin_model_conditional(condition, at)?;
        dae.replay_model_arm_context(Some(owner.arm(dae, true)), at)?;
        Ok(())
    });
    assert!(matches!(result,
        Err(DaeConstructionError::InvalidModelActivationScope { span })
            if span == at.span() && !span.is_dummy()
    ));
}

#[test]
fn model_arm_cannot_escape_or_borrow_a_sibling_value() {
    let source = TestSource::new("if c then 1 else 2");
    let at = source.source("if c then 1 else 2", 0);
    Dae::construct(source.map, |dae| {
        let condition = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let owner = dae.begin_model_conditional(condition, at)?;
        let first = dae.model_conditional_arm(&owner, true, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(1.0)))
        })?;
        assert!(matches!(
            dae.expressions(|e| e.at(at).unary(UnaryOperator::Plus, first)),
            Err(DaeConstructionError::InvalidModelActivationScope { .. })
        ));
        let second = dae.model_conditional_arm(&owner, false, |dae| {
            assert!(matches!(
                dae.expressions(|e| e.at(at).unary(UnaryOperator::Plus, first)),
                Err(DaeConstructionError::InvalidModelActivationScope { .. })
            ));
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(2.0)))
        })?;
        let value = dae.finish_model_conditional(owner, [first, second])?;
        dae.expressions(|e| e.at(at).unary(UnaryOperator::Plus, value))?;
        Ok(())
    })
    .expect("discharged value is visible, sibling and undischarged values are not");
}

#[test]
fn model_conditional_replay_refuses_a_foreign_guard() {
    let source = TestSource::new("if c then 1 else 2");
    let at = source.source("if c then 1 else 2", 0);
    Dae::construct(source.map, |dae| {
        let (condition, foreign, not) = dae.expressions(|e| {
            let condition = e.at(at).literal(DaeLiteral::Boolean(true))?;
            let foreign = e.at(at).literal(DaeLiteral::Boolean(false))?;
            let not = e.at(at).unary(UnaryOperator::Not, condition)?;
            Ok((condition, foreign, not))
        })?;
        assert!(matches!(
            dae.replay_model_conditional(condition, [foreign, not], at),
            Err(DaeConstructionError::InvalidModelActivationScope { .. })
        ));
        Ok(())
    })
    .expect("failed authority admission inserts no open owner");
}

#[test]
fn replay_model_conditional_preserves_every_entry_invariant() {
    let source = TestSource::new("if c then 1 else 2; function f end f");
    let at = source.source("if c then 1 else 2", 0);
    let foreign = DaeProvenance::source(Span::from_offsets(
        SourceId::from_source_name("foreign.mo"),
        0,
        1,
    ))
    .unwrap();
    Dae::construct(source.map, |dae| {
        let (condition, not, tensor, not_tensor) = dae.expressions(|e| {
            let condition = e.at(at).literal(DaeLiteral::Boolean(true))?;
            let not = e.at(at).unary(UnaryOperator::Not, condition)?;
            let tensor = e.at(at).array([condition, condition])?;
            let not_tensor = e.at(at).unary(UnaryOperator::Not, tensor)?;
            Ok((condition, not, tensor, not_tensor))
        })?;
        assert!(matches!(
            dae.replay_model_conditional(tensor, [tensor, not_tensor], at),
            Err(DaeConstructionError::InvalidExpressionForm { .. })
        ));
        assert!(matches!(
            dae.replay_model_conditional(condition, [condition, not], foreign),
            Err(DaeConstructionError::UnknownSource { .. })
        ));
        let owner = dae.begin_model_conditional(condition, at)?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [], at),
            |dae, reservation| {
                assert!(matches!(
                    dae.replay_model_conditional(condition, [condition, not], at),
                    Err(DaeConstructionError::InvalidModelActivationScope { .. })
                ));
                assert!(matches!(
                    dae.replay_model_arm_context(Some(owner.arm(dae, true)), at),
                    Err(DaeConstructionError::InvalidModelActivationScope { .. })
                ));
                let body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        let first = dae.model_conditional_arm(&owner, true, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(1.0)))
        })?;
        let second = dae.model_conditional_arm(&owner, false, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(2.0)))
        })?;
        dae.finish_model_conditional(owner, [first, second])?;
        Ok(())
    })
    .expect("replay failures do not issue partial authority or disturb its parent");
}

#[test]
fn model_conditional_wire_reissues_authority_and_rejects_context_tampering() {
    let original = conditional_fixture();
    assert_closed_guard_operations(&original);
    let value = serde_json::to_value(&original).expect("serialize checked DAE");
    let decoded: Dae = serde_json::from_value(value.clone()).expect("current operations replay");
    decoded.inspect(|view| assert_eq!(view.model_conditional_count(), 1));
    assert_closed_guard_operations(&decoded);
    assert_eq!(serde_json::to_value(&decoded).unwrap(), value);
    let last = value["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .len()
        - 1;
    let mut missing = value.clone();
    missing["storage"]["expressions"]
        .as_object_mut()
        .unwrap()
        .remove("model_contexts");
    assert!(serde_json::from_value::<Dae>(missing).is_err());
    let mut swapped = value.clone();
    let contexts = swapped["storage"]["expressions"]["model_contexts"]
        .as_array_mut()
        .unwrap();
    let index = contexts
        .iter()
        .position(|value| value.as_u64() == Some(0))
        .expect("then-arm operation");
    contexts[index] = serde_json::json!(1);
    assert!(serde_json::from_value::<Dae>(swapped).is_err());
    let mut missing_parent = value.clone();
    missing_parent["storage"]["model_conditionals"][0]
        .as_object_mut()
        .unwrap()
        .remove("parent");
    assert!(serde_json::from_value::<Dae>(missing_parent).is_err());
    let mut foreign_close = value;
    foreign_close["storage"]["model_conditionals"][0]["expression"] = serde_json::json!(last - 1);
    assert!(serde_json::from_value::<Dae>(foreign_close).is_err());
}

fn shared_identity_function_with_dead_tail<'dae>(
    dae: &mut DaeConstruction<'dae>,
    real: ValueTypeId<'dae>,
    at: DaeProvenance,
) -> Result<(FunctionId<'dae>, ()), DaeConstructionError> {
    dae.function(
        FunctionSignature::new(VarName::new("f"), [real], [real], at),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let output = dae
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let value = dae.expressions(|e| e.at(at).function_parameter(input))?;
            let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
            dae.functions(|functions| functions.assign(&mut body, output, value, at))?;
            dae.functions(|functions| functions.define(body, at))?;
            // A dead closed body expression must remain within the
            // actual global function suspension, not an inferred
            // dependency-only extent.
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(99.0)))?;
            Ok(())
        },
    )
}

#[test]
fn nested_model_arm_first_creates_a_shared_global_function() {
    let source = TestSource::new("if c then if d then f(3) else 4 else 5; f(6)");
    let at = source.source("if c then if d then f(3) else 4 else 5; f(6)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real = dae
            .types(|types| types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), at))?;
        let condition = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let outer = dae.begin_model_conditional(condition, at)?;
        let (first, function) = dae.model_conditional_arm(&outer, true, |dae| {
            let inner = dae.begin_model_conditional(condition, at)?;
            let (value, function) = dae.model_conditional_arm(&inner, true, |dae| {
                let argument = dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(3.0)))?;
                let (function, ()) = shared_identity_function_with_dead_tail(dae, real, at)?;
                let value = dae.expressions(|e| e.at(at).call(function, 0, [argument]))?;
                Ok((value, function))
            })?;
            let fallback = dae.model_conditional_arm(&inner, false, |dae| {
                dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(4.0)))
            })?;
            Ok((
                dae.finish_model_conditional(inner, [value, fallback])?,
                function,
            ))
        })?;
        let fallback = dae.model_conditional_arm(&outer, false, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(5.0)))
        })?;
        dae.finish_model_conditional(outer, [first, fallback])?;
        let six = dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(6.0)))?;
        dae.expressions(|e| e.at(at).call(function, 0, [six]))?;
        Ok(())
    })
    .expect("one global function, two arm-local and global occurrences");
    assert_closed_guard_operations(&dae);
    let binary = bincode::serialize(&dae).expect("serialize nested construction operations");
    let decoded: Dae =
        bincode::deserialize(&binary).expect("function suspension and exact arms replay");
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
    assert_closed_guard_operations(&decoded);
    let json = serde_json::to_value(&dae).unwrap();
    serde_json::from_value::<Dae>(json.clone()).expect("JSON reissues the same boundaries");
    let end = json["storage"]["functions"][0]["construction_end"]
        .as_u64()
        .unwrap();
    for wrong_end in [end - 1, end + 1] {
        let mut wrong = json.clone();
        wrong["storage"]["functions"][0]["construction_end"] = serde_json::json!(wrong_end);
        assert!(
            serde_json::from_value::<Dae>(wrong).is_err(),
            "the global suspension must cover exactly its body, including its dead tail"
        );
    }
    let mut missing_end = json.clone();
    missing_end["storage"]["functions"][0]
        .as_object_mut()
        .unwrap()
        .remove("construction_end");
    assert!(serde_json::from_value::<Dae>(missing_end).is_err());
    let mut earlier = json;
    earlier["storage"]["functions"][0]["construction_start"] = serde_json::json!(0);
    assert!(
        serde_json::from_value::<Dae>(earlier).is_err(),
        "a function cannot consume a model prefix as its body"
    );
}

#[test]
fn arm_relation_binds_its_exact_guard_and_replay_requires_closed_authority() {
    let source = TestSource::new("if c then time>0 else false");
    let at = source.source("if c then time>0 else false", 0);
    let model = Dae::construct(source.map, |dae| {
        let condition = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let owner = dae.begin_model_conditional(condition, at)?;
        let first = dae.model_conditional_arm(&owner, true, |dae| {
            let expression = dae.expressions(|e| {
                let time = e.at(at).coordinate(CoordinateInput::Time)?;
                let zero = e.at(at).literal(DaeLiteral::Real(0.0))?;
                e.at(at).binary(BinaryOperator::Greater, time, zero)
            })?;
            assert_eq!(dae.model_activation_guard(expression, at)?, Some(condition));
            dae.conditions(|c| c.relation(expression, at))?;
            Ok(expression)
        })?;
        assert!(matches!(
            dae.model_activation_guard(first, at),
            Err(DaeConstructionError::InvalidModelActivationScope { .. })
        ));
        assert!(matches!(
            dae.conditions(|c| c.relation(first, at)),
            Err(DaeConstructionError::InvalidModelActivationScope { .. })
        ));
        let second = dae.model_conditional_arm(&owner, false, |dae| {
            assert!(matches!(
                dae.model_activation_guard(first, at),
                Err(DaeConstructionError::InvalidModelActivationScope { .. })
            ));
            assert!(matches!(
                dae.conditions(|c| c.relation(first, at)),
                Err(DaeConstructionError::InvalidModelActivationScope { .. })
            ));
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(false)))
        })?;
        dae.finish_model_conditional(owner, [first, second])?;
        assert_eq!(dae.model_activation_guard(first, at)?, Some(condition));
        let relation = dae.conditions(|c| c.relation(first, at))?;
        assert_eq!(
            relation.index(),
            0,
            "closed replay reuses the exact relation"
        );
        let activation = dae.conditions(|c| c.reserve(at))?;
        dae.conditions(|c| c.define(activation, ConditionInput::Relation(relation), at))?;
        dae.conditions(|c| c.root(relation, activation, at))?;
        Ok(())
    })
    .expect("relation is observed only under its own issued arm guard");
    let check = |view: DaeView<'_>| {
        assert_eq!(view.relation_count(), 1);
        let relation = view.relation(view.relation_id(0).unwrap()).unwrap();
        let guard = view.model_activation_guard(relation.expression()).unwrap();
        assert_eq!(guard, view.model_conditional_at(0).unwrap().guards()[0]);
        assert_eq!(view.root_count(), 1);
    };
    model.inspect(check);
    let json: Dae = serde_json::from_slice(&serde_json::to_vec(&model).unwrap()).unwrap();
    json.inspect(check);
    let bytes = bincode::serialize(&model).unwrap();
    let binary: Dae = bincode::deserialize(&bytes).unwrap();
    binary.inspect(check);
}

#[test]
fn borrowed_conditional_owner_restores_errors_and_keeps_closed_arms_closed() {
    let source = TestSource::new("if c then 1 else 2");
    let at = source.source("if c then 1 else 2", 0);
    Dae::construct(source.map, |dae| {
        let condition = dae.expressions(|e| e.at(at).literal(DaeLiteral::Boolean(true)))?;
        let owner = dae.expressions(|e| e.begin_model_conditional(condition, at))?;
        let failed: Result<(), DaeConstructionError> =
            dae.model_conditional_arm(&owner, true, |_| {
                Err(DaeConstructionError::InvalidExpressionForm { span: at.span() })
            });
        assert!(matches!(failed, Err(DaeConstructionError::InvalidExpressionForm { span }) if span == at.span()));
        dae.expressions(|e| {
            assert!(e.model_arm_context().is_none());
            Ok(())
        })?;
        let first = dae.model_conditional_arm(&owner, true, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(1.0)))
        })?;
        let second = dae.model_conditional_arm(&owner, false, |dae| {
            dae.expressions(|e| e.at(at).literal(DaeLiteral::Real(2.0)))
        })?;
        let first_arm = owner.arm(dae, true);
        let value = dae.expressions(|e| e.finish_model_conditional(owner, [first, second]))?;
        assert!(matches!(
            dae.replay_model_arm_context(Some(first_arm), at),
            Err(DaeConstructionError::InvalidModelActivationScope { .. })
        ));
        dae.expressions(|e| {
            assert!(e.model_arm_of(value, at)?.is_none());
            e.at(at).unary(UnaryOperator::Plus, value)?;
            Ok(())
        })
    })
    .expect("borrowed owner uses the same scope and close rules");
}
