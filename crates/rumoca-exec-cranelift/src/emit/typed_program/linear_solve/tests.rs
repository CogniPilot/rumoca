use super::*;

fn table() -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let matrix = solve::SolveValueType::tensor(element, vec![2, 2]).unwrap();
    let vector = solve::SolveValueType::tensor(element, vec![2]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_linear_solve.mo"),
        1,
        2,
    );
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(901).unwrap()),
            vec![matrix, vector.clone()],
            vec![solve::SolvePureCallOutput::result(vector)],
            at,
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], at)?;
                let rhs = builder.load(inputs[1], at)?;
                let solution = builder.linear_solve(matrix, rhs, at)?;
                builder.store(outputs[0], solution, at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn native_tensor_linear_solve_preserves_uniform_scaling() {
    let table = table();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    for scale in [1.0_f64, 1e-20, 1e20] {
        let inputs = [0.0, 2.0, 3.0, 4.0, -4.0, -5.0].map(|value| (value * scale).to_bits());
        let mut outputs = [0; 2];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &inputs,
                &mut outputs,
            )
            .unwrap_or_else(|error| panic!("scale {scale}: {error}"));
        for (actual, expected) in outputs.map(f64::from_bits).iter().zip([1.0, -2.0]) {
            assert!((actual - expected).abs() < 1e-12, "scale {scale}: {actual}");
        }
    }
}

#[test]
fn native_tensor_linear_solve_rejects_singular_and_nonfinite_inputs() {
    let table = table();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    for inputs in [
        [1.0, 2.0, 2.0, 4.0, 1.0, 1.0],
        [f64::INFINITY, 0.0, 0.0, 1.0, 1.0, 1.0],
    ] {
        let mut outputs = [0; 2];
        let error = compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &inputs.map(f64::to_bits),
                &mut outputs,
            )
            .unwrap_err();
        assert!(error.to_string().contains("linear solve"), "{error}");
    }
}

#[test]
fn native_tensor_linear_solve_directional_relation_is_preserved() {
    let table = table();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    let directional = site.directional().unwrap();
    let inputs = [
        0.0_f64, 2.0, 3.0, 4.0, 1.0, 0.0, 0.0, -1.0, -4.0, -5.0, 2.0, 3.0,
    ];
    let mut outputs = [0; 4];
    compiled
        .call_cells(
            rumoca_eval_solve::PureCallInvocation::Directional(directional),
            &inputs.map(f64::to_bits),
            &mut outputs,
        )
        .unwrap();
    let [x, y, dx, dy] = outputs.map(f64::from_bits);
    assert!((x - 1.0).abs() < 1e-12 && (y + 2.0).abs() < 1e-12);
    assert!((2.0 * dy + x - 2.0).abs() < 1e-12);
    assert!((3.0 * dx + 4.0 * dy - y - 3.0).abs() < 1e-12);
}
