use rumoca_core::{SourceId, Span};

use crate::{DaeConstructionError, DaeGeneration, DaeProvenance, DaeProvenanceOrigin};

fn source_span() -> Span {
    Span::from_offsets(SourceId::from_source_name("provenance.mo"), 12, 20)
}

#[test]
fn source_provenance_preserves_the_owner_span() {
    let provenance = DaeProvenance::source(source_span()).expect("test span is source-backed");

    assert_eq!(provenance.origin(), DaeProvenanceOrigin::Source);
    assert_eq!(provenance.span(), source_span());
}

#[test]
fn generated_provenance_is_typed() {
    let provenance = DaeProvenance::generated(DaeGeneration::ConnectionEquation, source_span())
        .expect("test span is source-backed");

    assert_eq!(
        provenance.origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::ConnectionEquation)
    );
}

#[test]
fn dummy_span_cannot_enter_checked_provenance() {
    assert_eq!(
        DaeProvenance::generated(DaeGeneration::PreValueLowering, Span::DUMMY),
        Err(DaeConstructionError::MissingProvenance {
            origin: DaeProvenanceOrigin::Generated(DaeGeneration::PreValueLowering),
            attempted_span: Span::DUMMY,
        })
    );
}
