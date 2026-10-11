//! Shared function callbacks retain their issued model-scope suspension.
use super::*;

fn suspended_identity_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new("f"), [real], [real], at),
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
            dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(99.0)))?;
            Ok(())
        },
    )?;
    Ok(function)
}

fn nested_function_model() -> dae::Dae {
    let text = "if c then if d then f(3) else 4 else 5; f(6)";
    let mut sources = SourceMap::new();
    let source = sources.add("nested_function.mo", text);
    let at = source_provenance(source, text, text);
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                rumoca_core::TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let condition = dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Boolean(true)))?;
        let outer = dae.begin_model_conditional(condition, at)?;
        let (first, function) = dae.model_conditional_arm(&outer, true, |dae| {
            let inner = dae.begin_model_conditional(condition, at)?;
            let (value, function) = dae.model_conditional_arm(&inner, true, |dae| {
                let argument = dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(3.0)))?;
                let function = suspended_identity_function(dae, real, at)?;
                let value = dae.expressions(|e| e.at(at).call(function, 0, [argument]))?;
                Ok((value, function))
            })?;
            let fallback = dae.model_conditional_arm(&inner, false, |dae| {
                dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(4.0)))
            })?;
            Ok((
                dae.finish_model_conditional(inner, [value, fallback])?,
                function,
            ))
        })?;
        let fallback = dae.model_conditional_arm(&outer, false, |dae| {
            dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(5.0)))
        })?;
        dae.finish_model_conditional(outer, [first, fallback])?;
        let six = dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(6.0)))?;
        dae.expressions(|e| e.at(at).call(function, 0, [six]))?;
        Ok(())
    })
    .expect("one global function, two arm-local and global occurrences")
}

#[test]
fn nested_model_arm_function_interval_survives_structural_replay() {
    let dae = nested_function_model();
    let snapshot = |view: dae::DaeView<'_>| {
        let f = view.function(view.function_id(0).unwrap()).unwrap();
        let range = f.construction_expression_range();
        assert!(range.end > range.start);
        assert!((range.clone()).any(|index| matches!(
            view.expression(view.expression_id(index as usize).unwrap()).unwrap().operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(99.0))
        )));
        (view.function_count(), view.model_conditional_count())
    };
    assert_eq!(dae.inspect(snapshot), (1, 2));
    let plan = dae.inspect(|view| vec![None; view.expression_count()]);
    let first = reconstruction::rebuild_literal_expressions(&dae, &plan)
        .expect("closed body tail rebuilt inside the actual function suspension");
    assert_eq!(first.inspect(snapshot), (1, 2));
    let second_plan = first.inspect(|view| vec![None; view.expression_count()]);
    let second = reconstruction::rebuild_literal_expressions(&first, &second_plan)
        .expect("the issued interval survives repeated structural reconstruction");
    assert_eq!(second.inspect(snapshot), (1, 2));
    let wire = serde_json::to_vec(&second).unwrap();
    let replay: dae::Dae = serde_json::from_slice(&wire).unwrap();
    assert_eq!(replay.inspect(snapshot), (1, 2));
}

#[test]
fn model_guard_operations_survive_a_settled_literal_plan() {
    let original = nested_function_model();
    let plan = original.inspect(|view| {
        let mut plan = vec![None; view.expression_count()];
        let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(view);
        for owner in 0..view.model_conditional_count() {
            let owner = view.model_conditional_at(owner).unwrap();
            for expression in owner.guard_operations() {
                let values = evaluator.expression(expression).unwrap();
                plan[expression.index() as usize] =
                    Some(std::sync::Arc::new(evaluable_parameters::FoldedValue {
                        scalar: dae::ScalarType::Boolean,
                        dimensions: Box::new([]),
                        values: values.into(),
                    }));
            }
        }
        plan
    });
    let rebuilt = reconstruction::rebuild_literal_expressions(&original, &plan)
        .expect("settled values do not replace construction-issued guard operations");
    rebuilt.inspect(|view| {
        assert_eq!(view.model_conditional_count(), 2);
        for owner in 0..2 {
            let owner = view.model_conditional_at(owner).unwrap();
            let operations: Vec<_> = owner.guard_operations().collect();
            assert_eq!(
                operations.len(),
                if owner.parent_arm().is_some() { 3 } else { 1 }
            );
            assert!(matches!(
                view.expression(operations[0]).unwrap().operation(),
                dae::ExpressionOperation::Unary {
                    operator: dae::UnaryOperator::Not,
                    ..
                }
            ));
        }
    });
    let replay: dae::Dae = serde_json::from_slice(&serde_json::to_vec(&rebuilt).unwrap()).unwrap();
    assert_eq!(replay.inspect(|view| view.model_conditional_count()), 2);
}

#[test]
fn ordinary_conditional_still_settles_to_its_selected_value() {
    let mut sources = SourceMap::new();
    let text = "if true then 3 else 4";
    let source = sources.add("ordinary_conditional.mo", text);
    let at = source_provenance(source, text, text);
    let original = dae::Dae::construct(sources, |dae| {
        dae.expressions(|e| {
            let condition = e.at(at).literal(dae::DaeLiteral::Boolean(true))?;
            let taken = e.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            let fallback = e.at(at).literal(dae::DaeLiteral::Real(4.0))?;
            e.at(at).conditional([(condition, taken)], fallback)?;
            Ok(())
        })
    })
    .unwrap();
    let plan = original.inspect(|view| vec![None; view.expression_count()]);
    let rebuilt = reconstruction::rebuild_literal_expressions(&original, &plan).unwrap();
    rebuilt.inspect(|view| {
        assert_eq!(view.model_conditional_count(), 0);
        assert!((0..view.expression_count()).all(|index| !matches!(
            view.expression(view.expression_id(index).unwrap()).unwrap().operation(),
            dae::ExpressionOperation::Conditional(_)
        )));
    });
}
