use super::*;

#[test]
fn shared_derivative_definitions_cannot_manufacture_a_perfect_matching() {
    for dimensions in [vec![], vec![1], vec![3], vec![4096]] {
        let model = derivative_alias_model(dimensions, true);
        let (prepared, _) = inspect_prepare_for_solve(&model);
        assert!(
            prepared.is_err(),
            "x=y, der(x)=a, der(y)=a has no independent equation for the motion"
        );
    }
}

#[test]
fn independent_derivative_equations_survive_state_demotion() {
    for dimensions in [vec![], vec![1], vec![3], vec![4096]] {
        let model = derivative_alias_model(dimensions, false);
        let prepared =
            prepare_for_solve(&model).expect("independent motion equation closes the system");
        prepared.as_dae().inspect(|view| {
            assert!(sort(view).is_ok());
            assert_eq!(view.continuous_owners().count(), 4);
            assert_eq!(
                view.variables()
                    .filter(|(_, v)| v.role() == dae::VariableRole::State)
                    .count(),
                1
            );
        });
    }
}

fn derivative_alias_model(dimensions: Vec<u32>, shared: bool) -> dae::Dae {
    let shape = if dimensions.is_empty() {
        String::new()
    } else {
        format!("{dimensions:?}")
    };
    let acceleration = if shared { "a" } else { "b" };
    let closing = if shared {
        String::new()
    } else {
        format!("Real b{shape};")
    };
    let motion = if shared { "" } else { "a=-y;" };
    let text = format!(
        "model DerivativeAlias Real x{shape},y{shape},a{shape}; {closing} equation x=y; der(x)=a; der(y)={acceleration}; {motion} end DerivativeAlias;"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("derivative_alias.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let ty = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, dimensions), at)
        })?;
        let (x, y, a) = model.variables(|v| {
            Ok((
                v.state(VarName::new("x"), ty, at, Default::default())?,
                v.state(VarName::new("y"), ty, at, Default::default())?,
                v.algebraic(VarName::new("a"), ty, at, Default::default())?,
            ))
        })?;
        let b = if shared {
            a
        } else {
            model.variables(|v| v.algebraic(VarName::new("b"), ty, at, Default::default()))?
        };
        let rows = model.expressions(|e| {
            let x_value = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let y_value = e.at(at).coordinate(dae::CoordinateInput::State(y))?;
            let a = e.at(at).coordinate(dae::CoordinateInput::Algebraic(a))?;
            let b = e.at(at).coordinate(dae::CoordinateInput::Algebraic(b))?;
            let dx = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let dy = e.at(at).coordinate(dae::CoordinateInput::Derivative(y))?;
            let mut rows = vec![
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, x_value, y_value)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, dx, a)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, dy, b)?,
            ];
            if !shared {
                rows.push(e.at(at).binary(dae::BinaryOperator::Add, a, y_value)?);
            }
            Ok(rows)
        })?;
        model.continuous(|c| {
            for row in rows {
                c.value_equation(at, row)?;
            }
            Ok(())
        })
    })
    .unwrap()
}
