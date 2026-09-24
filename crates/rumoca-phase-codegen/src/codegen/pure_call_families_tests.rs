use std::num::NonZeroU64;

use rumoca_core::{SourceId, Span};
use rumoca_ir_solve as solve;
use serde_json::Value as Json;

use super::PureCallFamilies;

fn span(start: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("pure_call_families.mo"),
        start,
        start + 1,
    )
}

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    )
}

fn vector() -> solve::SolveValueType {
    solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![3]).unwrap()
}

fn identity(value: u64) -> solve::SolvePureCallIdentity {
    solve::SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
}

type Builder = solve::SolvePureCallTableBuilder;
type Owner = Result<solve::SolvePureCallOwnerId, solve::SolveProgramConstructionError>;

/// `y = x`, issued at a distinct identity and source position.
fn passthrough(table: &mut Builder, call: u64, at: usize) -> Owner {
    table.add_owner(
        identity(call),
        vec![vector()],
        vec![solve::SolvePureCallOutput::result(vector())],
        span(at),
        |builder, inputs, outputs| {
            let value = builder.load(inputs[0], span(at + 1))?;
            builder.store(outputs[0], value, span(at + 2))
        },
    )
}

/// `y = -x`.
fn negation(table: &mut Builder, call: u64, at: usize) -> Owner {
    table.add_owner(
        identity(call),
        vec![vector()],
        vec![solve::SolvePureCallOutput::result(vector())],
        span(at),
        |builder, inputs, outputs| {
            let value = builder.load(inputs[0], span(at + 1))?;
            let negated = builder.unary(solve::SolveUnaryOperator::Negate, value, span(at + 2))?;
            builder.store(outputs[0], negated, span(at + 3))
        },
    )
}

/// `y = callee(x)`.
fn forwarding(
    table: &mut Builder,
    call: u64,
    callee: solve::SolvePureCallOwnerId,
    at: usize,
) -> Owner {
    table.add_owner(
        identity(call),
        vec![vector()],
        vec![solve::SolvePureCallOutput::result(vector())],
        span(at),
        |builder, inputs, outputs| {
            let argument = builder.load(inputs[0], span(at + 1))?;
            let results = builder.call(callee, &[argument], span(at + 2))?;
            builder.store(outputs[0], results[0], span(at + 3))
        },
    )
}

fn nested_callees(json: &Json, found: &mut Vec<u64>) {
    match json {
        Json::Object(fields) => {
            if fields.get("operation").and_then(Json::as_str) == Some("call") {
                found.extend(fields.get("owner").and_then(Json::as_u64));
            }
            fields
                .values()
                .for_each(|value| nested_callees(value, found));
        }
        Json::Array(items) => items.iter().for_each(|value| nested_callees(value, found)),
        _ => {}
    }
}

#[test]
fn structurally_identical_owners_share_one_emitted_family() {
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        let first = passthrough(table, 1, 0)?;
        let second = passthrough(table, 2, 10)?;
        forwarding(table, 3, first, 20)?;
        forwarding(table, 4, second, 30)?;
        let negated = negation(table, 5, 40)?;
        forwarding(table, 6, negated, 50)?;
        Ok(())
    })
    .unwrap();
    assert!(
        table
            .owners()
            .iter()
            .all(|owner| owner.directional().is_some()),
        "every Real owner carries a directional body that joins its family key"
    );

    let families = PureCallFamilies::new(&table).unwrap();

    // Owners 0/1 differ only in identity and provenance; owners 2/3 differ
    // only in which member of that family they call. Owner 4 computes
    // something else, so owner 5, which forwards to it, stays distinct too.
    assert_eq!(families.symbols(), &[0, 0, 2, 2, 4, 5]);
    let emitted: Vec<_> = families
        .owners()
        .iter()
        .map(|owner| owner["id"].as_u64().unwrap())
        .collect();
    assert_eq!(emitted, [0, 2, 4, 5]);

    let mut callees = Vec::new();
    families
        .owners()
        .iter()
        .for_each(|owner| nested_callees(owner, &mut callees));
    assert_eq!(
        callees,
        [0, 4],
        "nested calls name their family representative"
    );
}

#[test]
fn distinct_owners_keep_their_own_symbols() {
    let table = solve::SolvePureCallTable::construct(profile(), |table| {
        passthrough(table, 1, 0)?;
        negation(table, 2, 10)?;
        Ok(())
    })
    .unwrap();
    let families = PureCallFamilies::new(&table).unwrap();
    assert_eq!(families.symbols(), &[0, 1]);
    assert_eq!(families.owners().len(), 2);
}
