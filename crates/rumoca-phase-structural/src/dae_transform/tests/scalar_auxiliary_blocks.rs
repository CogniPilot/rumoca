use super::*;

#[test]
fn coupled_scalar_coordinates_share_one_source_block() {
    let model = scalar_system(false, false);
    model.inspect(|view| {
        let facts = constraints::DifferentiationFacts::collect(view);
        let s = facts.auxiliary_blocks[2]
            .as_ref()
            .expect("s is reconstructed");
        let w = facts.auxiliary_blocks[3]
            .as_ref()
            .expect("w is reconstructed");
        assert!(std::sync::Arc::ptr_eq(s, w));
        assert_eq!(&*s.state_anchors, &[0, 1]);
        assert_eq!(s.extent, 2);
        for owner in view.continuous_owners() {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                panic!("fixture has source-authored scalar equations");
            };
            assert!(s.contains_residual(equation.residual().index()));
        }
    });
}

#[test]
fn scalar_auxiliary_proof_refuses_nonlinear_and_underdetermined_blocks() {
    for (nonlinear, missing) in [(true, false), (false, true)] {
        scalar_system(nonlinear, missing).inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            assert!(facts.auxiliary_blocks.iter().all(Option::is_none));
        });
    }
}

#[test]
fn scalar_auxiliary_coefficients_preserve_source_dag_sharing() {
    let counts = [4, 8].map(|depth| {
        scalar_system_with_depth(false, false, depth).inspect(|view| {
            let facts = constraints::DifferentiationFacts::collect(view);
            facts.auxiliary_blocks[2]
                .as_ref()
                .unwrap()
                .coefficient_node_count()
        })
    });
    assert!(
        counts[1] <= 2 * counts[0] + 20,
        "shared DAG doubled in depth: {counts:?}"
    );
}

fn scalar_system(nonlinear: bool, missing: bool) -> dae::Dae {
    scalar_system_with_depth(nonlinear, missing, 0)
}

fn scalar_system_with_depth(nonlinear: bool, missing: bool, depth: usize) -> dae::Dae {
    let mut first = if nonlinear { "s*w" } else { "s+w" }.to_string();
    for _ in 0..depth {
        first = format!("({first})+({first})");
    }
    let first = format!("{first}=x;");
    let second = if missing { "" } else { "s-w=y;" };
    let text = format!("Real x; Real y; Real s; Real w; equation {first} {second}");
    let mut sources = SourceMap::new();
    let source = sources.add("scalar_system.mo", &text);
    let at = source_provenance(source, &text, &text);
    dae::Dae::construct(sources, |model| {
        let scalar = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let x = model.variables(|v| v.state(VarName::new("x"), scalar, at, Default::default()))?;
        let y = model.variables(|v| v.state(VarName::new("y"), scalar, at, Default::default()))?;
        let s =
            model.variables(|v| v.algebraic(VarName::new("s"), scalar, at, Default::default()))?;
        let w =
            model.variables(|v| v.algebraic(VarName::new("w"), scalar, at, Default::default()))?;
        let rows = model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(x))?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::State(y))?;
            let s = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(s))?;
            let w = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(w))?;
            let operator = if nonlinear {
                dae::BinaryOperator::Multiply
            } else {
                dae::BinaryOperator::Add
            };
            let mut lhs = expressions.at(at).binary(operator, s, w)?;
            for _ in 0..depth {
                lhs = expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, lhs, lhs)?;
            }
            let first = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, lhs, x)?;
            let lhs = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, s, w)?;
            let second = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, lhs, y)?;
            Ok([first, second])
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, rows[0])?;
            if !missing {
                continuous.value_equation(at, rows[1])?;
            }
            Ok(())
        })
    })
    .unwrap()
}
