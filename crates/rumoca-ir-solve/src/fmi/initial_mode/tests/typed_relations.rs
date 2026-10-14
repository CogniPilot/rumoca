use super::*;
use crate::{
    SolveCompareOperator, SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable,
    SolveScalarType, SolveUnaryOperator, SolveValue, SolveValueType,
};
use rumoca_core::{SourceId, Span};
use std::num::NonZeroU64;

#[derive(Clone, Copy)]
enum Returned {
    Comparison,
    Negation,
    Constant,
    TwoOutputs,
    ForeignInterface,
}

fn table(returned: Returned) -> SolvePureCallTable {
    let arithmetic = SolvePureCallTable::default().arithmetic();
    let span = Span::from_offsets(SourceId::from_source_name("integer_relation.mo"), 0, 20);
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let mut outputs = vec![SolvePureCallOutput::result(boolean.clone())];
    if matches!(returned, Returned::TwoOutputs) {
        outputs.push(SolvePureCallOutput::result(boolean));
    }
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let inputs = vec![
        integer;
        if matches!(returned, Returned::ForeignInterface) {
            2
        } else {
            1
        }
    ];
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            inputs,
            outputs,
            span,
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span)?;
                let zero = builder.constant(SolveValue::integer(arithmetic, 0).unwrap(), span)?;
                let comparison = builder.compare(SolveCompareOperator::Less, input, zero, span)?;
                let value = match returned {
                    Returned::Comparison | Returned::TwoOutputs | Returned::ForeignInterface => {
                        comparison
                    }
                    Returned::Negation => {
                        builder.unary(SolveUnaryOperator::Not, comparison, span)?
                    }
                    Returned::Constant => builder.constant(SolveValue::boolean(true), span)?,
                };
                for &output in outputs {
                    builder.store(output, value, span)?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn raw(calls: &SolvePureCallTable) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::PureCall {
            dst_start: 1,
            input_starts: vec![0].into_boxed_slice(),
            site: calls.owners()[0].call_site(),
        },
        LinearOp::StoreOutput { src: 1 },
    ]
}

#[test]
fn only_the_issued_singleton_boolean_comparison_publication_is_a_relation() {
    for (returned, expected) in [
        (Returned::Comparison, true),
        (Returned::Negation, false),
        (Returned::Constant, false),
        (Returned::TwoOutputs, false),
    ] {
        let calls = table(returned);
        let wrapper = conditional(load(7), raw(&calls), load(8));
        assert_eq!(admitted_with_calls(&[wrapper], &calls), expected);
    }
}

#[test]
fn a_typed_relation_cannot_hide_unregistered_memory_or_additional_flag_reads() {
    let calls = table(Returned::Comparison);
    let raw = raw(&calls);
    assert!(!admitted_with_calls(
        &[conditional(load(7), raw.clone(), load(6))],
        &calls
    ));
    let mut extra_mode = raw.clone();
    extra_mode.insert(2, LinearOp::LoadP { dst: 2, index: 7 });
    assert!(!admitted_with_calls(
        &[conditional(load(7), extra_mode, load(8))],
        &calls
    ));
    let mut overwritten = raw;
    overwritten.insert(
        2,
        LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count: 2,
            lanes: 1,
        },
    );
    assert!(!admitted_with_calls(
        &[conditional(load(7), overwritten, load(8))],
        &calls
    ));
}

#[test]
fn a_call_without_its_issued_owner_cannot_establish_a_relation() {
    let calls = table(Returned::Comparison);
    let wrapper = conditional(load(7), raw(&calls), load(8));
    assert!(!admitted(&[wrapper]));
}

#[test]
fn returned_register_and_call_site_must_match_the_retained_owner() {
    let calls = table(Returned::Comparison);
    let ops = raw(&calls);
    assert!(super::super::typed_relation::comparison(&ops[1], 1, &calls));
    assert!(!super::super::typed_relation::comparison(
        &ops[1], 2, &calls
    ));
    let foreign = table(Returned::ForeignInterface);
    let op = LinearOp::PureCall {
        dst_start: 1,
        input_starts: vec![0, 0].into_boxed_slice(),
        site: foreign.owners()[0].call_site(),
    };
    assert!(!super::super::typed_relation::comparison(&op, 1, &calls));
}
