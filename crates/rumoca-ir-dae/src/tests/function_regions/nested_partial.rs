use super::*;

fn nested_partial_function() -> Dae {
    let source = TestSource::new(
        "function nested input Boolean first; input Boolean second; output Integer y; protected Integer t; algorithm if first then t := 1; end if; if second then t := 2; end if; y := t; end nested;",
    );
    let at = source.source("function nested", 0);
    let first_at = source.source("if first then", 0);
    let second_at = source.source("if second then", 0);
    let read_at = source.source("y := t;", 0);
    Dae::construct(source.map, |dae| {
        let (integer, boolean) = dae.types(|types| {
            Ok((
                types.derived(ValueType::scalar(ScalarType::Integer), at)?,
                types.derived(ValueType::scalar(ScalarType::Boolean), at)?,
            ))
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("nested"), [boolean, boolean], [integer], at),
            |dae, reservation| {
                let (first, second, output, target) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("first"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("second"), 1, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("t"), integer, at)?,
                    ))
                })?;
                let (first, second, one, two) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(first_at).function_parameter(first)?,
                        expressions.at(second_at).function_parameter(second)?,
                        expressions.at(first_at).literal(DaeLiteral::Integer(1))?,
                        expressions.at(second_at).literal(DaeLiteral::Integer(2))?,
                    ))
                })?;
                let body = dae.functions(|functions| functions.begin(reservation, at))?;
                let body = append_partial_branch(dae, body, first, target, one, first_at)?;
                let mut body = append_partial_branch(dae, body, second, target, two, second_at)?;
                dae.functions(|functions| {
                    functions.require_assigned(&mut body, target, read_at)?;
                    let rhs = functions.read(&body, target, read_at)?;
                    functions.assign(&mut body, output, rhs, read_at)?;
                    functions.define(body, at)
                })
            },
        )?;
        Ok(())
    })
    .expect("nested partial state has an exact checked read, with no missing operand")
}

fn append_partial_branch<'dae>(
    dae: &mut DaeConstruction<'dae>,
    body: FunctionBody<'dae>,
    condition: ExprId<'dae>,
    target: FunctionValueId<'dae>,
    value: ExprId<'dae>,
    at: DaeProvenance,
) -> Result<FunctionBody<'dae>, DaeConstructionError> {
    let (mut child, finish) = dae.functions(|functions| functions.begin_branch(body, at))?;
    dae.functions(|functions| functions.assign(&mut child, target, value, at))?;
    let (body, branch) = dae.functions(|functions| functions.finish_branch(finish, child))?;
    let (child, finish) = dae.functions(|functions| functions.begin_branch(body, at))?;
    let (mut body, fallback) = dae.functions(|functions| functions.finish_branch(finish, child))?;
    dae.functions(|functions| {
        functions.finish_conditional(&mut body, vec![(condition, branch)], fallback, at)
    })?;
    Ok(body)
}

fn check_nested_partial(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = function.statements().collect::<Vec<_>>();
    let [
        FunctionStatementView::Conditional { conditional: first },
        FunctionStatementView::Conditional {
            conditional: second,
        },
        FunctionStatementView::RequireAssigned {
            conditional: checked,
            value,
            ..
        },
        FunctionStatementView::Assignment { .. },
    ] = &statements[..]
    else {
        panic!("two partial owners precede the exact checked read");
    };
    let prior = FunctionAssignedState::Conditional {
        owner: first.id(),
        value: *value,
    };
    assert_eq!(first.outgoing(*value), Some(prior));
    assert_eq!(
        second.branches().next().unwrap().incoming(*value),
        Some(prior)
    );
    assert_eq!(second.fallback().outgoing(*value), Some(prior));
    assert_eq!(checked.id(), second.id());
    assert_eq!(first.outputs().len(), 0);
    assert_eq!(second.outputs().len(), 0);
    let mut inputs = Vec::new();
    assert!(second.visit_value_inputs(*value, |input| inputs.push(input)));
    assert_eq!(
        inputs.len(),
        4,
        "two selection conditions and exactly the two assigned sources"
    );
    check_pruned_inputs(
        *first,
        *second,
        *value,
        function.values().next().unwrap().id(),
        &inputs,
    );
}

fn check_pruned_inputs<'dae>(
    first: FunctionConditionalBodyView<'dae>,
    second: FunctionConditionalBodyView<'dae>,
    value: FunctionValueId<'dae>,
    output: FunctionValueId<'dae>,
    inputs: &[ExprId<'dae>],
) {
    let mut unpruned = Vec::new();
    assert!(second.visit_value_inputs_pruned(value, |_, _| false, |input| unpruned.push(input)));
    assert_eq!(
        unpruned, inputs,
        "default query retains the same ordered inputs"
    );

    let mut remaining = Vec::new();
    assert!(second.visit_value_inputs_pruned(
        value,
        |owner, target| owner == first.id() && target == value,
        |input| remaining.push(input),
    ));
    let assignments = second
        .branches()
        .next()
        .unwrap()
        .statements()
        .collect::<Vec<_>>();
    let [FunctionStatementView::Assignment { definition }] = assignments.as_slice() else {
        panic!("the newer source branch owns its one assigned value");
    };
    assert_eq!(
        remaining,
        [second.conditions().next().unwrap(), definition.rhs()],
        "a predecessor cut retains the newer guard and payload without old reads",
    );

    let mut wrong_value = Vec::new();
    assert!(second.visit_value_inputs_pruned(
        value,
        |owner, target| owner == first.id() && target == output,
        |input| wrong_value.push(input),
    ));
    assert_eq!(
        wrong_value, inputs,
        "another value cannot select the predecessor cut"
    );

    let mut bound_owner_inputs = Vec::new();
    assert!(second.visit_value_inputs_pruned(
        value,
        |owner, target| owner == second.id() && target == value,
        |input| bound_owner_inputs.push(input),
    ));
    assert!(
        bound_owner_inputs.is_empty(),
        "a bound owner does not reopen its selection"
    );
}

#[test]
fn nested_partial_incoming_owner_is_preserved_until_the_current_checked_read() {
    nested_partial_function().inspect(check_nested_partial);
}

#[test]
fn nested_partial_current_wire_replays_prior_state_without_fabricated_values() {
    let dae = nested_partial_function();
    let bytes = bincode::serialize(&dae).unwrap();
    let replayed: Dae = bincode::deserialize(&bytes).unwrap();
    replayed.inspect(check_nested_partial);
    assert_eq!(bincode::serialize(&replayed).unwrap(), bytes);
}

fn check_expression_state_boundaries(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let Some(FunctionStatementView::Conditional { conditional: first }) =
        function.statements().next()
    else {
        panic!("the predecessor is an original checked conditional");
    };
    let Some(FunctionStatementView::RequireAssigned {
        conditional: second,
        value,
        definition,
        ..
    }) = function.statements().nth(2)
    else {
        panic!("the current source read owns its ordered check");
    };
    let root = definition.rhs();
    let mut traversal = ExpressionTraversal::new();
    let mut original = Vec::new();
    traversal.visit_pruned(view, [root], |id, _| {
        original.push(id);
        true
    });
    let default = expression_inputs(view, root, |_, _| false);
    assert_eq!(
        default, original,
        "general queries retain the original traversal"
    );
    let cut = expression_inputs(view, root, |owner, target| {
        owner == first.id() && target == value
    });
    assert!(
        cut.contains(&root),
        "the check projection itself remains visible"
    );
    assert!(
        cut.contains(&second.conditions().next().unwrap()),
        "the unbound owner's guard remains required"
    );
    assert!(
        !cut.contains(&first.conditions().next().unwrap()),
        "a transported predecessor does not reopen its selection"
    );
    let Some(FunctionStatementView::Assignment {
        definition: assigned,
    }) = second.branches().next().unwrap().statements().next()
    else {
        panic!("the newer arm retains its original assigned payload");
    };
    assert!(cut.contains(&assigned.rhs()));
    let other = function.values().next().unwrap().id();
    let wrong = expression_inputs(view, root, |owner, target| {
        owner == first.id() && target == other
    });
    assert_eq!(
        wrong, original,
        "a different value cannot select the state boundary"
    );
    let bound = expression_inputs(view, root, |owner, target| {
        owner == second.id() && target == value
    });
    assert_eq!(
        bound,
        [root],
        "the available state retains only its checked projection node"
    );
}

fn expression_inputs<'dae>(
    view: DaeView<'dae>,
    root: ExprId<'dae>,
    prune: impl FnMut(FunctionConditionalId<'dae>, FunctionValueId<'dae>) -> bool,
) -> Vec<ExprId<'dae>> {
    let mut inputs = Vec::new();
    ExpressionTraversal::new().visit_pruned_with_conditional_states(
        view,
        [root],
        prune,
        |id, _| {
            inputs.push(id);
            true
        },
    );
    inputs
}

#[test]
fn expression_traversal_preserves_exact_nested_assigned_state_boundaries() {
    nested_partial_function().inspect(check_expression_state_boundaries);
}
