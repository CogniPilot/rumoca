//! A residual row keeps the component occurrence it was constructed with,
//! through the JSON and binary wires and back, and a row without one keeps its
//! JSON bytes unchanged.

use rumoca_core::{ClassEquation, ComponentOccurrence, InstanceId, ScopeId, SourceMap, Span};
use rumoca_ir_dae::{Dae, DaeLiteral, DaeProvenance};

fn occurrence(span: Span, instance: u32) -> ComponentOccurrence {
    ComponentOccurrence {
        class_equation: ClassEquation {
            scope: ScopeId(4),
            span,
        },
        instance: InstanceId(instance),
        view: 0,
    }
}

fn rows(with_occurrence: bool) -> (Dae, Span) {
    let mut source_map = SourceMap::new();
    let source = source_map.add("occurrence_rows.mo", "initial equation 0; equation 0;");
    let span = Span::from_offsets(source, 0, 18);
    let owner = DaeProvenance::source(span).expect("real source span");
    let tag = |instance| with_occurrence.then(|| occurrence(span, instance));
    let dae = Dae::construct(source_map, |dae| {
        let residual =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
        dae.continuous(|continuous| continuous.component_value_equation(owner, residual, tag(7)))?;
        dae.initialization(|initialization| {
            initialization.component_value_equation(owner, residual, tag(8))
        })?;
        Ok(())
    })
    .expect("scalar rows with and without occurrences construct");
    (dae, span)
}

#[test]
fn residual_rows_keep_their_component_occurrence_through_the_wire() {
    let (dae, span) = rows(true);
    let encoded = serde_json::to_string(&dae).unwrap();
    assert_eq!(encoded.matches("\"occurrence\":").count(), 2);
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    for dae in [&dae, &decoded] {
        dae.inspect(|view| {
            let continuous = view.continuous_equation(0).expect("continuous row");
            let initialization = view.initialization_equation(0).expect("initial row");
            assert_eq!(continuous.occurrence(), Some(occurrence(span, 7)));
            assert_eq!(initialization.occurrence(), Some(occurrence(span, 8)));
        });
    }
}

#[test]
fn rows_without_an_occurrence_serialize_without_the_field() {
    let (dae, _) = rows(false);
    let encoded = serde_json::to_string(&dae).unwrap();
    assert!(!encoded.contains("\"occurrence\":"));
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.continuous_equation(0).unwrap().occurrence(), None);
        assert_eq!(view.initialization_equation(0).unwrap().occurrence(), None);
    });
}

#[test]
fn positional_binary_wire_keeps_the_field_with_and_without_an_occurrence() {
    for with_occurrence in [true, false] {
        let (dae, span) = rows(with_occurrence);
        let binary = bincode::serialize(&dae).unwrap();
        let decoded: Dae = bincode::deserialize(&binary).expect("binary rows replay");
        assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
        decoded.inspect(|view| {
            let expected = |instance| with_occurrence.then(|| occurrence(span, instance));
            assert_eq!(
                view.continuous_equation(0).unwrap().occurrence(),
                expected(7)
            );
            assert_eq!(
                view.initialization_equation(0).unwrap().occurrence(),
                expected(8)
            );
        });
    }
}

#[test]
fn a_row_records_its_occurrence_once() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("occurrence_twice.mo", "equation 0;");
    let span = Span::from_offsets(source, 0, 11);
    let owner = DaeProvenance::source(span).expect("real source span");
    let result = Dae::construct(source_map, |dae| {
        let residual =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
        dae.continuous(|continuous| {
            continuous.equation(owner, |equation| {
                equation.residual(residual)?;
                equation.occurrence(occurrence(span, 1))?;
                equation.occurrence(occurrence(span, 1))
            })
        })?;
        Ok(())
    });
    assert!(result.is_err());
}
