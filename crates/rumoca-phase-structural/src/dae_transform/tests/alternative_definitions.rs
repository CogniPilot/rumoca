use super::*;

#[test]
fn alternative_definitions_retain_the_independent_constraint() {
    for dimensions in [vec![], vec![3]] {
        alternative_definitions(dimensions, false).inspect(|view| {
            let value = view.variable_id(2).unwrap();
            assert!(
                crate::CausalDefinitions::derive(view)
                    .definition_for_variable(value)
                    .is_none(),
                "the executable definition policy remains unchanged"
            );
            let facts = constraints::DifferentiationFacts::collect(view);
            assert!(
                facts.algebraic_definitions[2].is_some(),
                "an exact state-anchored equality can supply a structural value"
            );
            let constraints = constraints::holonomic_constraints(view);
            assert_eq!(
                constraints.len(),
                1,
                "the alternative equation still constrains the independent states"
            );
            assert_eq!(&*constraints[0].proof.anchored_states, &[0, 1]);
            assert_eq!(constraints[0].owner_ordinal, 1);
            assert_eq!(view.continuous_owners().count(), 2);
        });
    }
}

#[test]
fn alternative_definitions_cannot_close_a_cycle_without_value_anchors() {
    alternative_definitions(vec![], true).inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        assert!(facts.algebraic_definitions[2].is_none());
        let dependent =
            facts.algebraic_definitions[3].expect("the existing partial definition is retained");
        assert!(!facts.can_materialize_value(view, dependent));
        assert!(constraints::holonomic_constraints(view).is_empty());
    });
}

fn alternative_definitions(dimensions: Vec<u32>, cycle: bool) -> dae::Dae {
    let shape = if dimensions.is_empty() { "" } else { "[3]" };
    let equations = if cycle {
        "value=x+dependent; value=y+dependent; dependent=value+x;"
    } else {
        "value=x+x; value=y+y;"
    };
    let text = format!(
        "Real x{shape}; Real y{shape}; Real value{shape}; Real dependent{shape}; equation {equations}"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("alternative_definitions.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let ty = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, dimensions), at)
        })?;
        let x = model.variables(|v| v.state(VarName::new("x"), ty, at, Default::default()))?;
        let y = model.variables(|v| v.state(VarName::new("y"), ty, at, Default::default()))?;
        let value =
            model.variables(|v| v.algebraic(VarName::new("value"), ty, at, Default::default()))?;
        let dependent = model
            .variables(|v| v.algebraic(VarName::new("dependent"), ty, at, Default::default()))?;
        let rows = model.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let y = e.at(at).coordinate(dae::CoordinateInput::State(y))?;
            let value = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(value))?;
            let dependent = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(dependent))?;
            let first = e.at(at).binary(
                dae::BinaryOperator::Add,
                x,
                if cycle { dependent } else { x },
            )?;
            let first = e
                .at(at)
                .binary(dae::BinaryOperator::Subtract, value, first)?;
            let second = e.at(at).binary(
                dae::BinaryOperator::Add,
                y,
                if cycle { dependent } else { y },
            )?;
            let second = e
                .at(at)
                .binary(dae::BinaryOperator::Subtract, value, second)?;
            let third = e.at(at).binary(dae::BinaryOperator::Add, value, x)?;
            let third = e
                .at(at)
                .binary(dae::BinaryOperator::Subtract, dependent, third)?;
            Ok([first, second, third])
        })?;
        model.continuous(|c| {
            c.value_equation(at, rows[0])?;
            c.value_equation(at, rows[1])?;
            if cycle {
                c.value_equation(at, rows[2])?;
            }
            Ok(())
        })
    })
    .unwrap()
}
