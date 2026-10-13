use super::*;
use crate::SolveBinaryOperator;

fn table(operator: SolveBinaryOperator) -> SolvePureCallTable {
    let real = SolveValueType::scalar(SolveScalarType::real(profile()));
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real)],
            span(0),
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], span(1))?;
                let value = builder.binary(operator, value, value, span(2))?;
                builder.store(outputs[0], value, span(3))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn original_eq(left: &SolvePureCallTable, right: &SolvePureCallTable) -> bool {
    left.arithmetic == right.arithmetic && left.owners == right.owners
}

#[test]
fn immutable_table_clone_shares_only_exact_complete_lineage() {
    let original = table(SolveBinaryOperator::Add);
    let cloned = original.clone();
    assert!(original.shares_table_owner(&cloned));
    assert!(!original.shares_table_owner(&table(SolveBinaryOperator::Add)));
    let replay: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&original).unwrap()).unwrap();
    assert!(!original.shares_table_owner(&replay));
    assert!(Arc::ptr_eq(
        &original.immutable_lineage,
        &cloned.immutable_lineage
    ));
    assert_ne!(original.owners.as_ptr(), cloned.owners.as_ptr());
    assert_eq!(original == cloned, original_eq(&original, &cloned));
}

#[test]
fn independent_and_replayed_tables_use_full_structural_equality() {
    let original = table(SolveBinaryOperator::Add);
    let independent = table(SolveBinaryOperator::Add);
    let wire = serde_json::to_string(&original).unwrap();
    assert!(!wire.contains("immutable_lineage"));
    let replayed: SolvePureCallTable = serde_json::from_str(&wire).unwrap();
    for other in [&independent, &replayed] {
        assert!(!Arc::ptr_eq(
            &original.immutable_lineage,
            &other.immutable_lineage
        ));
        assert_eq!(original == *other, original_eq(&original, other));
        assert_eq!(serde_json::to_string(other).unwrap(), wire);
    }
    let default = SolvePureCallTable::default();
    let other_default = SolvePureCallTable::default();
    assert!(!Arc::ptr_eq(
        &default.immutable_lineage,
        &other_default.immutable_lineage
    ));
    assert_eq!(
        default == other_default,
        original_eq(&default, &other_default)
    );
}

#[test]
fn identical_owner_identity_does_not_accept_changed_helper_body() {
    let add = table(SolveBinaryOperator::Add);
    let subtract = table(SolveBinaryOperator::Subtract);
    assert_eq!(add.owners[0].id, subtract.owners[0].id);
    assert_eq!(add.owners[0].identity, subtract.owners[0].identity);
    assert!(!original_eq(&add, &subtract));
    assert_ne!(add, subtract);
    let replayed: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&subtract).unwrap()).unwrap();
    assert_eq!(add == replayed, original_eq(&add, &replayed));
}

#[test]
fn shared_lineage_does_not_bypass_arithmetic_profile_guard() {
    let original = table(SolveBinaryOperator::Add);
    // Internal adversarial fixture changes only the independent profile field.
    // No public owner constructor can reuse a lineage for changed bodies.
    let mut altered = original.clone();
    altered.arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(-10, 10).unwrap(),
    );
    assert!(Arc::ptr_eq(
        &original.immutable_lineage,
        &altered.immutable_lineage
    ));
    assert_eq!(original == altered, original_eq(&original, &altered));
    assert_ne!(original, altered);
}
