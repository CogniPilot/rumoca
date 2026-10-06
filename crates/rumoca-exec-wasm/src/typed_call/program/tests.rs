//! Relocation refuses missing/duplicate imports before mutating linked ownership.
use super::*;
use rumoca_core::{SourceId, Span};
use std::num::NonZeroU64;

fn power_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let span = Span::from_offsets(SourceId::from_source_name("math_relocation.mo"), 0, 1);
    let mut table = solve::SolvePureCallTable::builder(arithmetic);
    let owner = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![scalar.clone()],
            vec![solve::SolvePureCallOutput::result(scalar)],
            span,
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span)?;
                let power = b.binary(solve::SolveBinaryOperator::Power, value, value, span)?;
                b.store(outputs[0], power, span)
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn exact_math_relocation_refuses_missing_duplicate_catalog_without_partial_mutation() {
    let (table, site) = power_table();
    for imports in [
        vec![],
        vec![MathImport::Sin],
        vec![MathImport::Pow, MathImport::Pow],
    ] {
        let mut helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
        let before = helpers.linked.functions.clone();
        let indices = helpers.linked.math_indices.clone();
        assert!(helpers.relocate(&imports).is_err());
        assert_eq!(helpers.linked.functions, before);
        assert_eq!(helpers.linked.math_indices, indices);
    }
    let mut helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
    assert_eq!(helpers.math_imports(), [MathImport::Pow]);
    helpers
        .relocate(&[MathImport::Sin, MathImport::Pow])
        .unwrap();
    assert_eq!(helpers.layout(&site).unwrap().function, 2);
    assert_eq!(helpers.linked.math_indices[&MathImport::Pow], 1);
}

#[test]
fn math_relocation_overflow_and_repeated_assignment_preserve_all_linked_state() {
    let (table, site) = power_table();
    let mut helpers = ProgramHelpers::new(&table, std::slice::from_ref(&site)).unwrap();
    let original = helpers.linked.functions.clone();
    helpers.linked.functions.push(Some(u32::MAX));
    let before = helpers.linked.functions.clone();
    let indices = helpers.linked.math_indices.clone();
    let imports = [MathImport::Sin, MathImport::Pow];
    assert_eq!(
        helpers.relocate(&imports),
        Err(TypedCallCompileError::SizeLimit)
    );
    assert_eq!(helpers.linked.functions, before);
    assert_eq!(helpers.linked.math_indices, indices);
    assert!(!helpers.relocated);
    helpers.linked.functions = original;
    helpers.relocate(&imports).unwrap();
    let before = helpers.linked.functions.clone();
    let indices = helpers.linked.math_indices.clone();
    assert!(helpers.relocated);
    assert_eq!(
        helpers.relocate(&imports),
        Err(TypedCallCompileError::SiteMismatch)
    );
    assert_eq!(helpers.linked.functions, before);
    assert_eq!(helpers.linked.math_indices, indices);
    assert_eq!(helpers.layout(&site).unwrap().function, 2);
}
