use super::*;
use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};

#[derive(Clone, Copy, PartialEq)]
enum Case {
    Complete,
    Reversed,
    Partial,
    Cycle,
    Duplicate,
    DuplicateElement,
    OutputArray,
    ArrayDefined,
    AliasChain,
    ContinuousChain,
}

fn model(case: Case) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "Real a[2]; output Real y1; output Real y2; equation y1=1; y2=2; a[1]=y1; a[2]=y2;";
    let source = sources.add("indexed-alias-definitions.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let (real, array) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    at,
                )?,
            ))
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_owned(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                at,
            )
        })?;
        let (a, y1, y2) = model.variables(|variables| {
            Ok((
                if case == Case::OutputArray {
                    variables.output(VarName::new("a"), array, at, Default::default())?
                } else {
                    variables.algebraic(VarName::new("a"), array, at, Default::default())?
                },
                variables.output(VarName::new("y1"), real, at, Default::default())?,
                variables.output(VarName::new("y2"), real, at, Default::default())?,
            ))
        })?;
        let residuals =
            model.expressions(|expressions| residuals(expressions, [a, y1, y2], case, at))?;
        model.continuous(|continuous| {
            let mut residuals = residuals.into_iter();
            if case == Case::ArrayDefined {
                let residual = residuals.next().unwrap();
                continuous.structured_family(
                    at,
                    domain,
                    rumoca_core::ComprehensionScalarView::RowMajorProjection,
                    |family| family.body(residual),
                )?;
            }
            for residual in residuals {
                continuous.equation(at, |equation| equation.residual(residual))?;
            }
            Ok(())
        })
    })
    .unwrap()
}

fn residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: [dae::AlgebraicId<'dae>; 3],
    case: Case,
    at: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let [a, y1, y2] = variables;
    let a = expressions
        .at(at)
        .coordinate(dae::CoordinateInput::Algebraic(a))?;
    let y1 = expressions
        .at(at)
        .coordinate(dae::CoordinateInput::Algebraic(y1))?;
    let y2 = expressions
        .at(at)
        .coordinate(dae::CoordinateInput::Algebraic(y2))?;
    let mut elements = Vec::new();
    for index in [1, 2] {
        let expression = expressions
            .at(at)
            .literal(dae::DaeLiteral::Integer(index))?;
        elements.push(expressions.at(at).index(
            a,
            [dae::Subscript::Index {
                expression,
                provenance: at,
            }],
        )?);
    }
    let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
    let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
    let (first, second) = if case == Case::Cycle {
        (
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, elements[1], one)?,
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, elements[0], one)?,
        )
    } else if case == Case::ContinuousChain {
        (
            expressions.at(at).coordinate(dae::CoordinateInput::Time)?,
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, y1, one)?,
        )
    } else if case == Case::AliasChain {
        (one, y1)
    } else {
        (one, two)
    };
    let mut pairs = vec![(y1, first), (y2, second), (elements[0], y1)];
    if case != Case::Partial {
        pairs.push((elements[1], y2));
    }
    if case == Case::Duplicate {
        pairs.push((y1, two));
    }
    if case == Case::DuplicateElement {
        pairs.push((elements[0], y2));
    }
    if case == Case::ArrayDefined {
        let value = expressions.at(at).array([one, two])?;
        pairs = vec![(a, value), (elements[0], y1), (elements[1], y2)];
    }
    pairs
        .into_iter()
        .map(|(lhs, rhs)| {
            let (lhs, rhs) = if case == Case::Reversed {
                (rhs, lhs)
            } else {
                (lhs, rhs)
            };
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })
        .collect()
}

#[test]
fn indexed_aliases_preserve_independent_output_definitions() {
    for case in [
        Case::Complete,
        Case::Reversed,
        Case::OutputArray,
        Case::AliasChain,
    ] {
        model(case).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            let a = view
                .variables()
                .find(|(_, v)| v.name().as_str() == "a")
                .unwrap()
                .0;
            assert_eq!(
                proof.order().len(),
                2,
                "array aliases cannot redefine the constant producers"
            );
            assert!(proof.fully_defines_variable(a));
            assert!(proof.event_holds_variable(a));
            assert!(proof.scalar_definition_for_variable(a, 0).is_some());
            assert!(proof.scalar_definition_for_variable(a, 1).is_some());
            assert_eq!(proof.remaining_owner_count(), 0);
        });
    }
}

#[test]
fn partial_indexed_aliases_keep_the_missing_coordinate_unproved() {
    model(Case::Partial).inspect(|view| {
        let proof = CausalDefinitions::derive(view);
        let a = view
            .variables()
            .find(|(_, v)| v.name().as_str() == "a")
            .unwrap()
            .0;
        assert!(!proof.fully_defines_variable(a));
        assert_eq!(proof.order().len(), 2);
        assert_eq!(proof.remaining_owner_count(), 1);
    });
}

#[test]
fn indexed_alias_dependencies_cannot_close_a_cycle_or_hide_duplicate_sources() {
    for case in [Case::Cycle, Case::Duplicate, Case::DuplicateElement] {
        model(case).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            let a = view
                .variables()
                .find(|(_, v)| v.name().as_str() == "a")
                .unwrap()
                .0;
            assert!(!proof.fully_defines_variable(a));
            assert!(!proof.event_holds_variable(a));
            assert!(proof.remaining_owner_count() >= 2);
        });
    }
}

#[test]
fn independently_defined_array_can_supply_scalar_outputs() {
    model(Case::ArrayDefined).inspect(|view| {
        let proof = CausalDefinitions::derive(view);
        assert_eq!(proof.order().len(), 3);
        assert_eq!(proof.remaining_owner_count(), 0);
        assert!(
            view.variables()
                .all(|(id, _)| proof.event_holds_variable(id))
        );
    });
}

#[test]
fn transitive_continuous_array_values_are_never_event_held() {
    model(Case::ContinuousChain).inspect(|view| {
        let proof = CausalDefinitions::derive(view);
        let a = view
            .variables()
            .find(|(_, v)| v.name().as_str() == "a")
            .unwrap()
            .0;
        assert!(proof.fully_defines_variable(a));
        assert_eq!(proof.remaining_owner_count(), 0);
        assert!(
            view.variables()
                .all(|(id, _)| !proof.event_holds_variable(id))
        );
    });
}
