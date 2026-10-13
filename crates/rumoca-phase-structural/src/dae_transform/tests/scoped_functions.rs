use super::*;

fn scoped_function_model() -> dae::Dae {
    let text = "function guarded input Boolean flag; output Integer y; protected Integer spare; algorithm if flag then (y, spare) := (1, 1); assert(flag, \"selected\"); if flag then y := 2; else y := 3; end if; else y := 4; end if; y := spare; end guarded;";
    let mut map = SourceMap::new();
    let source = map.add("scoped_function.mo", text);
    let at = source_provenance(source, text, "function guarded");
    let outer_at = source_provenance_occurrence(source, text, "if flag then", 0);
    let inner_at = source_provenance_occurrence(source, text, "if flag then", 1);
    let tuple_at = source_provenance(source, text, "(y, spare) := (1, 1)");
    let check_at = source_provenance(source, text, "y := spare");
    dae::Dae::construct(map, |model| {
        let (integer, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        model.function(
            dae::FunctionSignature::new(VarName::new("guarded"), [boolean], [integer], at),
            |model, reservation| {
                let (flag, output, spare) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("flag"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("spare"), integer, at)?,
                    ))
                })?;
                let (condition, one, message) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(outer_at).function_parameter(flag)?,
                        expressions
                            .at(tuple_at)
                            .literal(dae::DaeLiteral::Integer(1))?,
                        expressions
                            .at(tuple_at)
                            .literal(dae::DaeLiteral::String("selected".into()))?,
                    ))
                })?;
                let body = model.functions(|functions| functions.begin(reservation, at))?;
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch(body, outer_at))?;
                model.functions(|functions| {
                    functions.assign_all(&mut child, &[(output, one), (spare, one)], tuple_at)?;
                    functions.assertion(&mut child, condition, message, tuple_at)
                })?;
                let child = append_complete_conditional(model, child, condition, output, inner_at)?;
                let (body, selected) =
                    model.functions(|functions| functions.finish_branch(finish, child))?;
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch(body, outer_at))?;
                let four = model.expressions(|expressions| {
                    expressions
                        .at(outer_at)
                        .literal(dae::DaeLiteral::Integer(4))
                })?;
                model
                    .functions(|functions| functions.assign(&mut child, output, four, outer_at))?;
                let (mut body, fallback) =
                    model.functions(|functions| functions.finish_branch(finish, child))?;
                model.functions(|functions| {
                    functions.finish_conditional(
                        &mut body,
                        vec![(condition, selected)],
                        fallback,
                        outer_at,
                    )?;
                    functions.require_assigned(&mut body, spare, check_at)?;
                    let value = functions.read(&body, spare, check_at)?;
                    functions.assign(&mut body, output, value, check_at)?;
                    functions.define(body, at)
                })
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn append_complete_conditional<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    body: dae::FunctionBody<'dae>,
    condition: dae::ExprId<'dae>,
    output: dae::FunctionValueId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let (mut child, finish) = model.functions(|functions| functions.begin_branch(body, at))?;
    let two =
        model.expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(2)))?;
    model.functions(|functions| functions.assign(&mut child, output, two, at))?;
    let (body, selected) = model.functions(|functions| functions.finish_branch(finish, child))?;
    let (mut child, finish) = model.functions(|functions| functions.begin_branch(body, at))?;
    let three =
        model.expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(3)))?;
    model.functions(|functions| functions.assign(&mut child, output, three, at))?;
    let (mut body, fallback) =
        model.functions(|functions| functions.finish_branch(finish, child))?;
    model.functions(|functions| {
        functions.finish_conditional(&mut body, vec![(condition, selected)], fallback, at)
    })?;
    Ok(body)
}

fn check_scoped_function(view: dae::DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = function.statements().collect::<Vec<_>>();
    let [
        dae::FunctionStatementView::Conditional { conditional },
        dae::FunctionStatementView::RequireAssigned {
            conditional: checked,
            value,
            definition,
            ..
        },
        dae::FunctionStatementView::Assignment { .. },
    ] = &statements[..]
    else {
        panic!("conditional prefix and the exact assigned-state action survive");
    };
    assert_eq!(checked.id(), conditional.id());
    assert_eq!(conditional.outputs().len(), 1);
    assert_eq!(value.ordinal(), 1);
    assert_eq!(definition.target(), *value);
    let branch = conditional.branches().next().unwrap();
    let prefix = branch.statements().collect::<Vec<_>>();
    let [
        dae::FunctionStatementView::AssignmentGroup { definitions, .. },
        dae::FunctionStatementView::Assertion { .. },
        dae::FunctionStatementView::Conditional { conditional: inner },
    ] = &prefix[..]
    else {
        panic!("the whole tuple and assertion precede the nested conditional");
    };
    assert_eq!(definitions.len(), 2);
    assert_eq!(inner.outputs().len(), 1);
    assert_eq!(inner.branches().next().unwrap().statements().count(), 1);
    assert_eq!(inner.fallback().statements().count(), 1);
    assert!(matches!(
        conditional.fallback().outgoing(*value),
        Some(dae::FunctionAssignedState::Unassigned)
    ));
    assert!(
        matches!(view.expression(definition.rhs()).unwrap().operation(), dae::ExpressionOperation::FunctionConditionalOutput { conditional: owner, value: target, .. } if owner == checked.id() && target == *value)
    );
    let mut inputs = Vec::new();
    assert!(conditional.visit_value_inputs(*value, |input| inputs.push(input)));
    assert_eq!(inputs.len(), 2, "the missing path creates no expression");
}

#[test]
fn structural_reconstruction_preserves_scoped_tuple_prefix_and_partial_read() {
    let source = scoped_function_model();
    source.inspect(check_scoped_function);
    let (rebuilt, _) =
        super::super::reconstruction::rebuild_derivative_aliases(&source, &[], &[]).unwrap();
    rebuilt.inspect(check_scoped_function);
    let json = serde_json::to_string(&rebuilt).unwrap();
    let decoded: dae::Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(check_scoped_function);
}
