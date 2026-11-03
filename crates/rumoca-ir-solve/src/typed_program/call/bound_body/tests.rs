use super::*;
use crate::{SolveReductionOperator, SolveUnaryOperator};

fn span() -> Span {
    crate::source_span_from_offsets(17, 3, 9)
}

fn table() -> SolvePureCallTable {
    let arithmetic = SolvePureCallTable::default().arithmetic();
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        let inner = table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span())?;
                let y = b.unary(crate::SolveUnaryOperator::Sin, x, span())?;
                b.store(outputs[0], y, span())
            },
        )?;
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(2).unwrap()),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real)],
            span(),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span())?;
                let y = b.call(inner, &[x], span())?;
                b.store(outputs[0], y[0], span())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn bindings_retain_exact_body_interface_and_child_namespace() {
    let table = table();
    let outer = &table.owners()[1];
    let original = table.bind_original_body(outer.id(), span()).unwrap();
    let directional = table.bind_directional_body(outer.id(), span()).unwrap();
    assert!(std::ptr::eq(original.body(), outer.body()));
    assert!(std::ptr::eq(
        directional.body(),
        outer.directional().unwrap().body()
    ));
    assert_eq!(original.inputs().len(), 1);
    assert_eq!(original.outputs().len(), 1);
    assert_eq!(directional.inputs().len(), 2);
    assert_eq!(directional.outputs().len(), 2);
    for binding in [original, directional] {
        let child_id = binding
            .body()
            .operations()
            .iter()
            .find_map(|op| match op.operation() {
                SolveOperation::Call { owner, .. } => Some(*owner),
                _ => None,
            })
            .unwrap();
        let child = binding.resolve_child(child_id, span()).unwrap();
        assert_eq!(child.inputs().len(), binding.inputs().len());
        assert_eq!(child.outputs().len(), binding.outputs().len());
        assert!(child.body().operations().iter().any(|op| matches!(
            op.operation(),
            SolveOperation::Unary {
                operator: SolveUnaryOperator::Sin,
                ..
            }
        )));
    }
}

#[test]
fn cloned_source_shares_binding_but_equal_reconstruction_and_other_form_do_not() {
    let table = table();
    let clone = table.clone();
    let replay: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
    assert_eq!(table, replay);
    let id = table.owners()[0].id();
    let source = table.bind_original_body(id, span()).unwrap();
    assert!(source.shares_binding(clone.bind_original_body(id, span()).unwrap()));
    assert!(!source.shares_binding(replay.bind_original_body(id, span()).unwrap()));
    assert!(!source.shares_binding(table.bind_directional_body(id, span()).unwrap()));
    assert!(
        !source.shares_binding(
            table
                .bind_original_body(table.owners()[1].id(), span())
                .unwrap()
        )
    );
}

#[test]
fn unavailable_directional_body_refuses_eagerly_with_occurrence_span() {
    let arithmetic = SolvePureCallTable::default().arithmetic();
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let vector = SolveValueType::tensor(real.element_type(), vec![3]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(3).unwrap()),
            vec![vector],
            vec![SolvePureCallOutput::result(real)],
            span(),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span())?;
                let y = b.reduce(SolveReductionOperator::Product, x, span())?;
                b.store(outputs[0], y, span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let id = table.owners()[0].id();
    assert!(table.bind_original_body(id, span()).is_ok());
    assert!(table.owners()[0].directional().is_none());
    for result in [
        table.bind_directional_body(id, span()),
        table.bind_original_body(SolvePureCallOwnerId::LIFECYCLE_METHOD, span()),
    ] {
        assert!(
            matches!(result, Err(SolveProgramConstructionError::UnknownCallOwner { provenance }) if provenance == span())
        );
    }
}
