use super::*;
use rumoca_core::{
    ComprehensionScalarView, SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId,
    VarName,
};

fn coordinate_model(structured: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "coordinate-proof.mo",
        "Real x[2],y[2]; equation x-x+y=zeros(2);",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 35)).unwrap();
    dae::Dae::construct(sources, |model| {
        let array = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                at,
            )
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".into(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                at,
            )
        })?;
        let (x, y) = model.variables(|vars| {
            Ok((
                vars.algebraic(VarName::new("x"), array, at, Default::default())?,
                vars.algebraic(VarName::new("y"), array, at, Default::default())?,
            ))
        })?;
        let residuals = model.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            let y = e.at(at).coordinate(dae::CoordinateInput::Algebraic(y))?;
            if structured {
                let cancelled = e.at(at).binary(dae::BinaryOperator::Subtract, x, x)?;
                return Ok(vec![e.at(at).binary(
                    dae::BinaryOperator::Add,
                    cancelled,
                    y,
                )?]);
            }
            let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let two = e.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let x1 = e.at(at).index(
                x,
                [dae::Subscript::Index {
                    expression: one,
                    provenance: at,
                }],
            )?;
            let x2 = e.at(at).index(
                x,
                [dae::Subscript::Index {
                    expression: two,
                    provenance: at,
                }],
            )?;
            let y1 = e.at(at).index(
                y,
                [dae::Subscript::Index {
                    expression: one,
                    provenance: at,
                }],
            )?;
            let alias = e.at(at).binary(dae::BinaryOperator::Subtract, x1, x2)?;
            let cancelled = e.at(at).binary(dae::BinaryOperator::Subtract, x1, x1)?;
            let negative = e.at(at).binary(dae::BinaryOperator::Add, cancelled, y1)?;
            Ok(vec![alias, negative])
        })?;
        model.continuous(|continuous| {
            if structured {
                return continuous
                    .structured_family(
                        at,
                        domain,
                        ComprehensionScalarView::RowMajorProjection,
                        |family| family.body(residuals[0]),
                    )
                    .map(|_| ());
            }
            for residual in residuals {
                continuous.equation(at, |equation| equation.residual(residual))?;
            }
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn indexed_alias_proof_distinguishes_coordinates_and_rejects_same_element_cancellation() {
    coordinate_model(false).inspect(|view| {
        let incidence = build_incidence(view).unwrap();
        let proof = incidence.causal_candidates.as_ref().unwrap();
        assert_eq!(proof.row(0), &[0, 1]);
        assert_eq!(proof.row(1), &[2]);
        assert_eq!(incidence.eq_unknowns.row(1), &[0, 2]);
    });
}

#[test]
fn structured_scalar_views_do_not_admit_cancelled_or_other_point_coordinates() {
    coordinate_model(true).inspect(|view| {
        let incidence = build_incidence(view).unwrap();
        let proof = incidence.causal_candidates.as_ref().unwrap();
        assert_eq!(proof.row(0), &[2]);
        assert_eq!(proof.row(1), &[3]);
        assert_eq!(incidence.eq_unknowns.row(0), &[0, 2]);
        assert_eq!(incidence.eq_unknowns.row(1), &[1, 3]);
        let saved = ReusableIncidence::from_incidence(&incidence);
        let copied = build_incidence_reusing(view, IncidenceReuse::new(&saved, &[false])).unwrap();
        assert_eq!(copied.causal_candidates, incidence.causal_candidates);
    });
}
