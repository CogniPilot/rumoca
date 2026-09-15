use super::*;

/// A value definition can depend on several independent states without being
/// an independent constraint between those states.
#[test]
fn a_materialized_definition_is_not_a_holonomic_constraint() {
    for dimensions in [vec![], vec![3]] {
        for second_state in [false, true] {
            let model = defining_equation_model(dimensions.clone(), false, second_state);
            model.inspect(|view| {
                assert!(
                    constraints::holonomic_constraints(view).is_empty(),
                    "substituting the observation makes its defining residual identically zero"
                );
            });
        }
    }
}

fn defining_equation_model(
    dimensions: Vec<u32>,
    independent_constraint: bool,
    second_state: bool,
) -> dae::Dae {
    let shape = if dimensions.is_empty() { "" } else { "[3]" };
    let constraint = if independent_constraint {
        " observed*observed = 1;"
    } else {
        ""
    };
    let declaration = if second_state {
        format!("Real y{shape};")
    } else {
        String::new()
    };
    let other = if second_state { "y" } else { "x" };
    let text = format!(
        "Real x{shape}; {declaration} Real observed{shape}; equation observed = x + {other};{constraint}"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("defining_equation.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let value_type = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, dimensions), at)
        })?;
        let x = model.variables(|variables| {
            variables.state(VarName::new("x"), value_type, at, Default::default())
        })?;
        let y = if second_state {
            model.variables(|variables| {
                variables.state(VarName::new("y"), value_type, at, Default::default())
            })?
        } else {
            x
        };
        let observed = model.variables(|variables| {
            variables.algebraic(VarName::new("observed"), value_type, at, Default::default())
        })?;
        let (residual, observed) = model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(x))?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(y))?;
            let observed = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(observed))?;
            let sum = expressions.at(at).binary(dae::BinaryOperator::Add, x, y)?;
            let residual =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, observed, sum)?;
            Ok((residual, observed))
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))?;
        if independent_constraint {
            let constraint = model.expressions(|expressions| {
                let norm =
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Multiply, observed, observed)?;
                let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, norm, one)
            })?;
            model.continuous(|continuous| continuous.value_equation(at, constraint))?;
        }
        Ok(())
    })
    .unwrap()
}

#[test]
fn a_constraint_through_a_definition_retains_its_source_state_anchors() {
    for dimensions in [vec![], vec![3]] {
        let model = defining_equation_model(dimensions, true, true);
        model.inspect(|view| {
            let constraints = constraints::holonomic_constraints(view);
            assert_eq!(constraints.len(), 1, "observed*observed=1 constrains x+y");
            assert_eq!(&*constraints[0].proof.anchored_states, &[0, 1]);
            assert_eq!(
                constraints[0].owner_ordinal, 1,
                "the defining equation must remain owned"
            );
        });
    }
}

#[test]
fn an_independent_constraint_can_have_one_source_state_declaration() {
    for dimensions in [vec![], vec![3]] {
        let model = defining_equation_model(dimensions, true, false);
        model.inspect(|view| {
            let constraints = constraints::holonomic_constraints(view);
            assert_eq!(
                constraints.len(),
                1,
                "observed*observed=1 independently constrains 2*x"
            );
            assert_eq!(&*constraints[0].proof.anchored_states, &[0]);
            assert_eq!(
                constraints[0].owner_ordinal, 1,
                "the observation definition must remain owned"
            );
        });
    }
}
