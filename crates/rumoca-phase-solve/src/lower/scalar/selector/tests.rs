//! Exact static selection through a checked nested Integer comprehension.

use super::*;
use rumoca_core::{SourceMap, StructuredIndexBinder, StructuredIndexDomain};

fn domain(name: &str, lower: i64, upper: i64, step: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: name.into(),
            lower,
            upper,
            step,
        }],
    }
}

fn model(lower: i64, upper: i64, step: i64) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "{{100*i+k for k in 3:-1:1} for i in -2:-2:-6}";
    let source = sources.add("integer_comprehension.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let parent =
            model.domains(|domains| domains.structured(domain("i", lower, upper, step), at))?;
        let child = model.domains(|domains| domains.nested(parent, domain("k", 3, 1, -1), at))?;
        let (i, k) = model.domains(|domains| {
            Ok((
                domains.binder(parent, 0, at)?,
                domains.binder(child, 0, at)?,
            ))
        })?;
        model.expressions(|expressions| {
            let i = expressions.at(at).binder(i)?;
            let k = expressions.at(at).binder(k)?;
            let hundred = expressions.at(at).literal(dae::DaeLiteral::Integer(100))?;
            let scaled = expressions
                .at(at)
                .binary(dae::BinaryOperator::Multiply, hundred, i)?;
            let body = expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, scaled, k)?;
            let child = expressions.at(at).comprehension(child, body)?;
            expressions.at(at).comprehension(parent, child)?;
            Ok(())
        })
    })
    .unwrap()
}

fn outer<'dae>(view: dae::DaeView<'dae>) -> (dae::DomainId<'dae>, dae::ExprId<'dae>) {
    (0..view.expression_count())
        .rev()
        .find_map(|index| {
            let expression = view.expression_id(index).unwrap();
            match view.expression(expression).unwrap().operation() {
                dae::ExpressionOperation::Comprehension { domain, body } => Some((domain, body)),
                _ => None,
            }
        })
        .unwrap()
}

#[test]
fn checked_affine_window_integer_comprehension_preserves_exact_parent_and_descending_child_points()
{
    model(-2, -6, -2).inspect(|view| {
        let (parent, child) = outer(view);
        for parent_value in [-2, -4, -6] {
            let selector = ScalarSelector::from_points(view, &[(parent, vec![parent_value])]);
            for (scalar, child_value) in [3, 2, 1].into_iter().enumerate() {
                assert_eq!(
                    selector.integer(child, scalar).unwrap(),
                    parent_value * 100 + child_value
                );
            }
            assert!(selector.integer(child, 3).is_err());
        }
    });
}

#[test]
fn checked_affine_window_integer_comprehension_preserves_missing_parent_and_overflow_refusals() {
    model(i64::MAX, i64::MAX, 1).inspect(|view| {
        let (parent, child) = outer(view);
        let missing = ScalarSelector::new(view, None)
            .integer(child, 0)
            .unwrap_err();
        let expected_span = view.expression(child).unwrap().provenance().span();
        let LowerError::NonComputable { reason, span } = missing else {
            panic!("{missing:?}");
        };
        assert_eq!(reason, "binder-valued subscript has no active domain");
        assert_eq!(span, expected_span);
        let active = ScalarSelector::from_points(view, &[(parent, vec![i64::MAX])]);
        let overflow = active.integer(child, 0).unwrap_err();
        let LowerError::ContractViolation { reason, span } = overflow else {
            panic!("{overflow:?}");
        };
        assert_eq!(reason, "integer evaluation overflow");
        assert_eq!(span, expected_span);
    });
}
