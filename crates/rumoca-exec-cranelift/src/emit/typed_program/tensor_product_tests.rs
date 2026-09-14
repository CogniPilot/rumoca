use super::*;

fn product_shape(lhs: &[u32], rhs: &[u32]) -> (usize, usize, usize) {
    (
        if lhs.len() == 2 { lhs[0] as usize } else { 1 },
        *lhs.last().unwrap() as usize,
        if rhs.len() == 2 { rhs[1] as usize } else { 1 },
    )
}

fn product_table(lhs: &[u32], rhs: &[u32]) -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let (rows, _, columns) = product_shape(lhs, rhs);
    let output = match (lhs.len(), rhs.len()) {
        (1, 1) => solve::SolveValueType::scalar(element),
        (1, 2) => solve::SolveValueType::tensor(element, vec![columns as u32]).unwrap(),
        (2, 1) => solve::SolveValueType::tensor(element, vec![rows as u32]).unwrap(),
        _ => solve::SolveValueType::tensor(element, vec![rows as u32, columns as u32]).unwrap(),
    };
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_tensor_product.mo"),
        1,
        2,
    );
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(902).unwrap()),
            vec![
                solve::SolveValueType::tensor(element, lhs.to_vec()).unwrap(),
                solve::SolveValueType::tensor(element, rhs.to_vec()).unwrap(),
            ],
            vec![solve::SolvePureCallOutput::result(output)],
            at,
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], at)?;
                let rhs = builder.load(inputs[1], at)?;
                let product = builder.matrix_multiply(lhs, rhs, at)?;
                builder.store(outputs[0], product, at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn reference_product(lhs: &[f64], rhs: &[f64], shape: (usize, usize, usize)) -> Vec<f64> {
    let (rows, inner, columns) = shape;
    (0..rows * columns)
        .map(|output| {
            let mut sum = 0.0;
            for term in 0..inner {
                let product =
                    lhs[(output / columns) * inner + term] * rhs[term * columns + output % columns];
                sum += product;
            }
            sum
        })
        .collect()
}

#[test]
fn typed_native_products_preserve_order_across_small_and_large_shapes() {
    let shapes: &[(&[u32], &[u32])] = &[
        (&[3], &[3]),
        (&[3, 3], &[3]),
        (&[3], &[3, 2]),
        (&[3, 3], &[3, 3]),
        (&[4, 4], &[4, 4]),
        (&[5, 5], &[5, 5]),
    ];
    for &(lhs_shape, rhs_shape) in shapes {
        check_product(lhs_shape, rhs_shape);
    }
}

fn check_product(lhs_shape: &[u32], rhs_shape: &[u32]) {
    let table = product_table(lhs_shape, rhs_shape);
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    let shape = product_shape(lhs_shape, rhs_shape);
    for perturbation in [0.0, 0.25, -2.0] {
        let lhs = (0..shape.0 * shape.1)
            .map(|i| [1e16, 1.0, -1e16, -0.0, -2.0][i % 5] + perturbation)
            .collect::<Vec<_>>();
        let rhs = (0..shape.1 * shape.2)
            .map(|i| [1.0, -0.5, 0.25][i % 3])
            .collect::<Vec<_>>();
        let input = lhs
            .iter()
            .chain(&rhs)
            .copied()
            .map(f64::to_bits)
            .collect::<Vec<_>>();
        let mut output = vec![0; shape.0 * shape.2];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &input,
                &mut output,
            )
            .unwrap();
        assert_eq!(
            output,
            reference_product(&lhs, &rhs, shape)
                .into_iter()
                .map(f64::to_bits)
                .collect::<Vec<_>>(),
            "{lhs_shape:?} * {rhs_shape:?}, perturbation {perturbation}"
        );
    }
}

#[test]
fn typed_native_product_directions_reuse_no_stale_values_or_seeds() {
    for n in [3_u32, 5] {
        let table = product_table(&[n, n], &[n, n]);
        let compiled = CompiledPureCallTable::compile(&table).unwrap();
        let site = table.owners()[0].call_site();
        let width = (n * n) as usize;
        let shape = (n as usize, n as usize, n as usize);
        for seed in [1.0, -0.5, 0.0] {
            let lhs = (0..width).map(|i| i as f64 * 0.25).collect::<Vec<_>>();
            let rhs = (0..width).map(|i| 2.0 - i as f64 * 0.5).collect::<Vec<_>>();
            let lhs_tangent = vec![seed; width];
            let rhs_tangent = vec![2.0 * seed; width];
            let input = [&lhs, &lhs_tangent, &rhs, &rhs_tangent]
                .into_iter()
                .flatten()
                .copied()
                .map(f64::to_bits)
                .collect::<Vec<_>>();
            let mut output = vec![0; width * 2];
            compiled
                .call_cells(
                    rumoca_eval_solve::PureCallInvocation::Directional(site.directional().unwrap()),
                    &input,
                    &mut output,
                )
                .unwrap();
            let first = reference_product(&lhs_tangent, &rhs, shape);
            let second = reference_product(&lhs, &rhs_tangent, shape);
            let expected = reference_product(&lhs, &rhs, shape)
                .into_iter()
                .chain(first.into_iter().zip(second).map(|(a, b)| a + b))
                .map(f64::to_bits)
                .collect::<Vec<_>>();
            assert_eq!(output, expected, "dimension {n}, seed {seed}");
        }
    }
}
