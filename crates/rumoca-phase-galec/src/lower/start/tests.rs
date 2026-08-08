use rumoca_core::VarName;

use super::*;

fn shape<'a>(variable: &'a VarName, dimensions: &[usize]) -> StartShape<'a> {
    StartShape {
        dimensions: dimensions.to_vec(),
        scalar_count: dimensions.iter().product(),
        variable,
    }
}

#[test]
fn scalar_start_broadcast_retains_declared_rank() {
    let variable = VarName::new("x");
    let values = StartValues::evaluated(vec![1.0], true, &variable, Span::DUMMY).unwrap();
    let start = shape(&variable, &[2, 3]).real(values).unwrap();
    assert_eq!(
        start,
        gast::Expression::Array(vec![
            gast::Expression::Array(vec![
                gast::Expression::Real(1.0),
                gast::Expression::Real(1.0),
                gast::Expression::Real(1.0),
            ]),
            gast::Expression::Array(vec![
                gast::Expression::Real(1.0),
                gast::Expression::Real(1.0),
                gast::Expression::Real(1.0),
            ]),
        ])
    );
}

#[test]
fn shaped_start_preserves_row_major_nesting() {
    let variable = VarName::new("x");
    let start = shape(&variable, &[2, 2])
        .integer(StartValues::shaped(vec![1.0, 2.0, 3.0, 4.0], Span::DUMMY))
        .unwrap();
    assert_eq!(
        start,
        gast::Expression::Array(vec![
            gast::Expression::Array(vec![
                gast::Expression::Integer(1),
                gast::Expression::Integer(2),
            ]),
            gast::Expression::Array(vec![
                gast::Expression::Integer(3),
                gast::Expression::Integer(4),
            ]),
        ])
    );
}

#[test]
fn non_scalar_mismatch_fails_with_variable_evidence() {
    let variable = VarName::new("x");
    let error = shape(&variable, &[2, 2])
        .real(StartValues::shaped(vec![1.0, 2.0], Span::DUMMY))
        .unwrap_err();
    assert!(matches!(
        error,
        GalecTargetError::AttributeNotEvaluable {
            variable,
            attribute: "start",
            ..
        } if variable == "x"
    ));
}

#[test]
fn scalar_evidence_requires_one_evaluated_value() {
    let variable = VarName::new("x");
    let error = StartValues::evaluated(vec![1.0, 2.0], true, &variable, Span::DUMMY).unwrap_err();
    assert!(matches!(
        error,
        GalecTargetError::AttributeNotEvaluable {
            variable,
            attribute: "start",
            ..
        } if variable == "x"
    ));
}
