//! Source provenance survives compact domain creation and nested-axis lifting.

use super::*;
use rumoca_core::{DefId, IndexBinderProvenance, SourceId, Span, Token};

fn index(provenance: Option<IndexBinderProvenance>) -> ast::ForIndex {
    let span = Span::from_offsets(SourceId::from_source_name("binder.mo"), 0, 1);
    ast::ForIndex {
        binder: provenance,
        ident: Token {
            text: "i".into(),
            ..Token::default()
        },
        range: ast::Expression::Range {
            start: std::sync::Arc::new(integer(1, span)),
            step: None,
            end: std::sync::Arc::new(integer(2, span)),
            span,
        },
    }
}

fn integer(value: i64, span: Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: Token {
            text: value.to_string().into(),
            ..Token::default()
        },
        span,
    }
}

fn iterations() -> Vec<SourceStructuredIteration> {
    [1, 2]
        .into_iter()
        .map(|value| SourceStructuredIteration {
            index_values: vec![value],
            equation_count: 1,
        })
        .collect()
}

#[test]
fn compact_domain_preserves_issued_source_identity_when_axes_are_lifted() {
    let span = Span::from_offsets(SourceId::from_source_name("binder.mo"), 0, 1);
    let provenance = IndexBinderProvenance::Source(DefId::new(23));
    let domain =
        compact_domain_from_iterations(&[index(Some(provenance))], &iterations(), span).unwrap();
    assert_eq!(domain.index_tuples().unwrap(), vec![vec![1], vec![2]]);
    assert_eq!(domain.binders[0].provenance, provenance);
    let reference = domain.binders[0].provenance.reference("i", span).unwrap();
    assert_eq!(reference.target_def_id(), Some(DefId::new(23)));
    let lifted = offset_child_binders(&domain.binders, 2);
    assert_eq!(lifted[0].id, 2);
    assert_eq!(lifted[0].provenance, provenance);
    assert_eq!(
        (lifted[0].lower, lifted[0].upper, lifted[0].step),
        (1, 2, 1)
    );
}

#[test]
fn compact_domain_requires_provenance_and_keeps_generated_axes_explicit() {
    let span = Span::from_offsets(SourceId::from_source_name("binder.mo"), 0, 1);
    assert!(compact_domain_from_iterations(&[index(None)], &iterations(), span).is_err());
    let domain = compact_domain_from_iterations(
        &[index(Some(IndexBinderProvenance::Generated))],
        &iterations(),
        span,
    )
    .unwrap();
    assert_eq!(
        domain.binders[0].provenance,
        IndexBinderProvenance::Generated
    );
    assert_eq!(domain.index_tuples().unwrap(), vec![vec![1], vec![2]]);
}
