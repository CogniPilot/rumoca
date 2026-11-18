//! Exact full-array definitions retained by a structured equation family.

use crate::residual_normalization::equation_sides;
use rumoca_ir_dae as dae;

/// Read a structured family body of the exact shape `target[binders] = value`,
/// where `target` is a whole algebraic array indexed by every binder of the
/// family's own domain, so the family defines each element of the array.
pub(crate) fn family_element_definition<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, dae::ExprId<'dae>)> {
    if family.scalar_view() != rumoca_core::ComprehensionScalarView::BinderSubstitution {
        return None;
    }
    let mut bodies = family.bodies().iter();
    let body = bodies.next()?;
    if bodies.next().is_some() {
        return None;
    }
    let (lhs, rhs) = equation_sides(view, body)?;
    family_indexed_target(view, family, lhs)
        .map(|target| (target, rhs))
        .or_else(|| family_indexed_target(view, family, rhs).map(|target| (target, lhs)))
}

/// The algebraic array a family body indexes with exactly its domain binders.
fn family_indexed_target<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::AlgebraicId<'dae>> {
    let dae::ExpressionOperation::Index { base, subscripts } =
        view.expression(expression)?.operation()
    else {
        return None;
    };
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(target)) =
        view.expression(base)?.operation()
    else {
        return None;
    };
    let dimensions = view
        .variable(dae::VariableId::from(target))?
        .value_type()
        .dimensions();
    let domain = view.domain(family.domain())?.structured();
    if subscripts.len() != dimensions.len() || domain.binders.len() != dimensions.len() {
        return None;
    }
    subscripts
        .iter()
        .zip(dimensions)
        .enumerate()
        .all(|(axis, (subscript, dimension))| {
            let binder = &domain.binders[axis];
            binder.lower == 1
                && binder.upper == i64::from(*dimension)
                && binder.step == 1
                && family_domain_binder(view, family, subscript, axis)
        })
        .then_some(target)
}

/// Whether a subscript is a binder of the family's own domain.
fn family_domain_binder<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    subscript: dae::SubscriptView<'dae>,
    axis: usize,
) -> bool {
    let dae::SubscriptView::Index { expression, .. } = subscript else {
        return false;
    };
    matches!(
        view.expression(expression).map(|node| node.operation()),
        Some(dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)))
            if binder.domain() == family.domain() && usize::try_from(binder.ordinal()).ok() == Some(axis)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{
        SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName,
    };

    fn family_model(bounds: [[i64; 3]; 2], selections: [usize; 2], copies: usize) -> dae::Dae {
        let mut sources = SourceMap::new();
        let text = "Real a[2,2]; equation for i in 1:2, j in 1:2 loop a[i,j]=5; end for;";
        let source = sources.add("family-definition.mo", text);
        let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        dae::Dae::construct(sources, |model| {
            let vector = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [2, 2]),
                    at,
                )
            })?;
            let domain = model.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: bounds
                            .iter()
                            .enumerate()
                            .map(|(axis, &[lower, upper, step])| StructuredIndexBinder {
                                provenance: rumoca_core::IndexBinderProvenance::Generated,
                                id: axis,
                                display_name: format!("axis{axis}"),
                                lower,
                                upper,
                                step,
                            })
                            .collect(),
                    },
                    at,
                )
            })?;
            let binders = model.domains(|domains| {
                Ok([
                    domains.binder(domain, 0, at)?,
                    domains.binder(domain, 1, at)?,
                ])
            })?;
            let target = model.variables(|variables| {
                variables.algebraic(VarName::new("a"), vector, at, Default::default())
            })?;
            let body = model.expressions(|expressions| {
                let target = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(target))?;
                let indices = selections
                    .map(|axis| expressions.at(at).binder(binders[axis]))
                    .into_iter()
                    .collect::<Result<Vec<_>, _>>()?;
                let selected = expressions.at(at).index(
                    target,
                    indices.into_iter().map(|expression| dae::Subscript::Index {
                        expression,
                        provenance: at,
                    }),
                )?;
                let value = expressions.at(at).literal(dae::DaeLiteral::Real(5.0))?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, selected, value)
            })?;
            for _ in 0..copies {
                model.continuous(|rows| {
                    rows.structured_family(
                        at,
                        domain,
                        rumoca_core::ComprehensionScalarView::BinderSubstitution,
                        |family| family.body(body),
                    )
                })?;
            }
            Ok(())
        })
        .unwrap()
    }

    #[test]
    fn complete_row_major_family_preserves_its_definition() {
        family_model([[1, 2, 1], [1, 2, 1]], [0, 1], 1).inspect(|view| {
            let dae::ContinuousOwnerView::Structured { family, .. } =
                view.continuous_owner(0).unwrap()
            else {
                panic!("retained source family");
            };
            assert!(family_element_definition(view, family).is_some());
        });
    }

    #[test]
    fn partial_stepped_and_noncanonical_binders_do_not_define_a_whole_array() {
        for (bounds, selections) in [
            ([[1, 1, 1], [1, 2, 1]], [0, 1]),
            ([[1, 2, 2], [1, 2, 1]], [0, 1]),
            ([[1, 2, 1], [1, 2, 1]], [1, 0]),
            ([[1, 2, 1], [1, 2, 1]], [0, 0]),
        ] {
            family_model(bounds, selections, 1).inspect(|view| {
                assert!(
                    matches!(
                        view.continuous_owner(0),
                        Some(dae::ContinuousOwnerView::Structured { family, .. })
                            if family_element_definition(view, family).is_none()
                    ),
                    "{bounds:?}, {selections:?}"
                );
            });
        }
    }
    #[test]
    fn duplicate_family_writers_do_not_issue_an_exact_definition() {
        family_model([[1, 2, 1], [1, 2, 1]], [0, 1], 2).inspect(|view| {
            let plan = crate::CausalDiscretePlan::derive(view).unwrap();
            let definitions = crate::SameTickDefinitions::derive(view, &plan).unwrap();
            let target = view.variables().next().unwrap().0;
            assert!(definitions.definition(target.index()).is_none());
            assert!(
                definitions
                    .definition_domain(view, target.index())
                    .is_none()
            );
        });
    }
}
