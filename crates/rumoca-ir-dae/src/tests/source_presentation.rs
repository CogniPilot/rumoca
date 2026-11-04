use super::*;
use crate::source_view::SourceEquationSides;

fn model() -> Dae {
    let source = TestSource::new("source presentation");
    let owner = source.source("source presentation", 0);
    Dae::construct(source.map, |dae| {
        let (left, right, sum) = dae.expressions(|expressions| {
            let left = expressions.at(owner).literal(DaeLiteral::Real(-0.0))?;
            let right = expressions.at(owner).literal(DaeLiteral::Real(2.0))?;
            let sum = expressions
                .at(owner)
                .binary(BinaryOperator::Add, left, right)?;
            Ok((left, right, sum))
        })?;
        dae.continuous(|continuous| {
            continuous.equation(owner, |equation| {
                equation.equal(left, right)?;
                Ok(())
            })?;
            continuous.equation(owner, |equation| equation.residual(sum))?;
            Ok(())
        })?;
        dae.initialization(|initialization| {
            initialization.equation(owner, |equation| equation.residual(sum))?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn source_presentation_preserves_subtraction_ids_and_provenance() {
    model().inspect(|view| {
        let equation = view.continuous_equation(0).unwrap();
        let presentation = view.source_equation_presentation(equation).unwrap();
        let SourceEquationSides::Equality { left, right } = presentation.sides() else {
            panic!("equality")
        };
        assert_eq!(left, view.expression_id(0).unwrap());
        assert_eq!(right, view.expression_id(1).unwrap());
        assert_eq!(presentation.provenance(), equation.provenance());
    });
}

#[test]
fn source_presentation_keeps_other_operations_as_complete_residuals() {
    model().inspect(|view| {
        for equation in [
            view.continuous_equation(1).unwrap(),
            view.initialization_equation(0).unwrap(),
        ] {
            let presentation = view.source_equation_presentation(equation).unwrap();
            let SourceEquationSides::ZeroResidual(residual) = presentation.sides() else {
                panic!("complete residual")
            };
            assert_eq!(residual, equation.residual());
            assert_eq!(presentation.provenance(), equation.provenance());
        }
    });
}

#[test]
fn source_presentation_does_not_mutate_the_checked_wire_or_expression_count() {
    let dae = model();
    let before = serde_json::to_value(&dae).unwrap();
    dae.inspect(|view| {
        let count = view.expression_count();
        for index in 0..2 {
            view.source_equation_presentation(view.continuous_equation(index).unwrap())
                .unwrap();
        }
        assert_eq!(view.expression_count(), count);
    });
    assert_eq!(serde_json::to_value(&dae).unwrap(), before);
}
