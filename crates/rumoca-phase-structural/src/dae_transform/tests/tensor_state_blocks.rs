use super::*;

#[test]
fn independent_literal_entries_complete_a_tensor_state_block() {
    state_block_model(false, false).inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        let block = facts.auxiliary_blocks[0]
            .as_ref()
            .expect("independent velocity block");
        assert_eq!(block.extent, 3);
        assert_eq!(&*block.state_anchors, &[3]);
        let candidates = constraints::direct_state_constraints(
            view,
            &constraints::DifferentiationFacts::collect(view),
        );
        assert!(
            candidates
                .admissible
                .iter()
                .any(|candidate| candidate.state == 0
                    && matches!(candidate.rhs, StateDefinition::Auxiliary(0)))
        );
    });
}

#[test]
fn a_causal_projection_does_not_supply_a_missing_independent_row() {
    state_block_model(true, false).inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        assert!(facts.auxiliary_blocks[0].is_none());
    });
}

#[test]
fn state_dependent_coefficients_cannot_define_an_affine_state_block() {
    state_block_model(false, true).inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        assert!(facts.auxiliary_blocks[0].is_none());
    });
}

fn state_block_model(missing: bool, nonlinear: bool) -> dae::Dae {
    let last = if missing { "der(theta)" } else { "theta" };
    let coefficient = if nonlinear { "v" } else { "{1,0,0}" };
    let text = format!(
        "Real v[3]; Real x; Real y; Real theta; Real a; equation v={{der(x),der(y),{last}}}; a=v[1]; v*{coefficient}=0; v*{{0,1,0}}=0;"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("state_block.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let (scalar, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
            ))
        })?;
        let v = model.variables(|variables| {
            variables.state(VarName::new("v"), vector, at, Default::default())
        })?;
        let x = model.variables(|variables| {
            variables.state(VarName::new("x"), scalar, at, Default::default())
        })?;
        let y = model.variables(|variables| {
            variables.state(VarName::new("y"), scalar, at, Default::default())
        })?;
        let theta = model.variables(|variables| {
            variables.state(VarName::new("theta"), scalar, at, Default::default())
        })?;
        let alias = model.variables(|variables| {
            variables.algebraic(VarName::new("a"), scalar, at, Default::default())
        })?;
        let rows = model.expressions(|e| {
            let v = e.at(at).coordinate(dae::CoordinateInput::State(v))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let dy = e.at(at).coordinate(dae::CoordinateInput::Derivative(y))?;
            let last = e.at(at).coordinate(if missing {
                dae::CoordinateInput::Derivative(theta)
            } else {
                dae::CoordinateInput::State(theta)
            })?;
            let values = e.at(at).array([dx, dy, last])?;
            let tensor = e.at(at).binary(dae::BinaryOperator::Subtract, v, values)?;
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let a = e.at(at).array([one, zero, zero])?;
            let b = e.at(at).array([zero, one, zero])?;
            let first = e.at(at).binary(
                dae::BinaryOperator::Multiply,
                v,
                if nonlinear { v } else { a },
            )?;
            let second = e.at(at).binary(dae::BinaryOperator::Multiply, v, b)?;
            let index = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let projection = e.at(at).index(
                v,
                [dae::Subscript::Index {
                    expression: index,
                    provenance: at,
                }],
            )?;
            let alias = e
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(alias))?;
            let definition = e
                .at(at)
                .binary(dae::BinaryOperator::Subtract, alias, projection)?;
            Ok([tensor, first, second, definition])
        })?;
        for row in rows {
            model.continuous(|continuous| continuous.value_equation(at, row))?;
        }
        Ok(())
    })
    .unwrap()
}
