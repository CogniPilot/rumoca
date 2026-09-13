use super::super::{EvaluationMode, InvocationScope, eval_owner_in_scope};
use super::*;

fn leaf_table(count: u64) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        for index in 1..=count {
            table.add_owner(
                identity(index),
                vec![real.clone()],
                vec![SolvePureCallOutput::result(real.clone())],
                span(1200),
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span(1201))?;
                    builder.store(outputs[0], input, span(1202))
                },
            )?;
        }
        Ok(())
    })
    .unwrap()
}

fn real_value(value: f64) -> TypedValue {
    let arithmetic = profile(SolveRealFormat::Binary64);
    TypedValue::construct(
        SolveValueType::scalar(SolveScalarType::real(arithmetic)),
        vec![real_kind(SolveRealFormat::Binary64, value)],
    )
    .unwrap()
}

#[test]
fn leaf_invocation_storage_is_independent_of_unrelated_owners() {
    let table = leaf_table(1024);
    let owner = table.owners().last().unwrap();
    let mut scope = InvocationScope::new(&table);
    let outputs = eval_owner_in_scope(
        &table,
        owner,
        &[real_value(3.0)],
        &mut scope,
        EvaluationMode::Primal,
    )
    .unwrap();
    assert_eq!(outputs, [real_value(3.0)]);
    assert_eq!(
        scope.calls.len(),
        0,
        "a leaf invocation must not allocate result slots for unrelated owners"
    );
    assert_eq!(scope.calls.capacity(), 0);
}

#[test]
fn sparse_invocations_retain_exact_ids_and_reject_duplicate_or_unknown_owners() {
    let table = leaf_table(1024);
    let outside_table = leaf_table(1025);
    let last = table.owners().last().unwrap().id();
    let first = table.owners()[0].id();
    let outside = outside_table.owners().last().unwrap().id();
    let mut scope = InvocationScope::new(&table);

    scope
        .insert(last, vec![real_value(7.0)], span(1203))
        .unwrap();
    assert_eq!(scope.calls.len(), 1);
    assert_eq!(scope.get(last).unwrap(), [real_value(7.0)]);
    assert_eq!(scope.get(first), None);
    assert!(matches!(
        scope.insert(last, vec![real_value(9.0)], span(1204)),
        Err(TypedProgramEvalError::InvalidCheckedProgram {
            operation: "redefine pure-call invocation",
            ..
        })
    ));
    assert_eq!(scope.get(last).unwrap(), [real_value(7.0)]);
    assert_eq!(
        scope.insert(outside, vec![real_value(8.0)], span(1205)),
        Err(TypedProgramEvalError::UnknownOwner { owner: outside })
    );
    assert_eq!(scope.calls.len(), 1);
    assert_eq!(scope.get(outside), None);
}
