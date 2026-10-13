//! The interpreter runs every operation and every loop pass the program
//! states. The unroll, demand and fault-class properties name only passes and
//! operations whose omission or expansion changes no result, so the interpreter
//! ignores them and its results are the ones a backend honouring them must
//! match (SPEC_0040 SOLVE-C102).
use super::*;

#[test]
fn a_loop_carrying_an_unroll_property_evaluates_every_pass() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let squares = SolveValueType::tensor(integer.element_type(), vec![4]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(701),
            Vec::new(),
            vec![SolvePureCallOutput::result(squares.clone())],
            span(2000),
            |builder, _, outputs| {
                let mapped = builder.map(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            provenance: rumoca_core::IndexBinderProvenance::Generated,
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 4,
                            step: 1,
                        }],
                    },
                    &[],
                    integer.clone(),
                    span(2001),
                    |region, _, binders, output| {
                        let i = region.load(binders[0], span(2002))?;
                        let square =
                            region.binary(SolveBinaryOperator::Multiply, i, i, span(2003))?;
                        region.store(output, square, span(2004))
                    },
                )?;
                builder.store(outputs[0], mapped, span(2005))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let body = owner.body();
    assert_eq!(body.unrolled_passes(0, false), Some(4));
    assert!(body.is_prunable(0));
    let outputs = eval_pure_call(&table, owner.id(), &[]).unwrap();
    let expected = [1, 4, 9, 16].map(SolveValueKind::Integer);
    assert_eq!(outputs[0].elements(), expected);
}
