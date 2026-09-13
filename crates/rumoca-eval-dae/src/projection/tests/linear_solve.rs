use super::*;

#[test]
fn linear_solve_projects_both_complete_tensor_operands() {
    let mut sources = SourceMap::new();
    let source = sources.add("linear_solve_projection.mo", "A*x=b");
    let at = provenance(source, 0, 5);
    let model = dae::Dae::construct(sources, |model| {
        let (matrix, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
            ))
        })?;
        let (matrix, rhs) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("A"),
                    matrix,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("b"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        model.expressions(|expressions| {
            let matrix = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(matrix))?;
            let rhs = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(rhs))?;
            expressions
                .at(at)
                .builtin(dae::PureBuiltin::LinearSolve, [matrix, rhs])?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let solution = view.expression_id(2).unwrap();
        for component in 0..2 {
            let mut dependencies = Vec::new();
            for_each_scalar_coordinate(view, solution, component, None, |coordinate, scalar| {
                dependencies.push(algebraic_scalar(coordinate, scalar));
            })
            .unwrap();
            dependencies.sort_unstable();
            assert_eq!(
                dependencies,
                [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1)]
            );
        }
    });
}

fn algebraic_scalar(coordinate: dae::CoordinateView<'_>, scalar: usize) -> (u32, usize) {
    let dae::CoordinateView::Algebraic(variable) = coordinate else {
        panic!("algebraic input")
    };
    (variable.index(), scalar)
}
