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
    for offset in [0.0, 0.25, -2.0] {
        let lhs = (0..shape.0 * shape.1)
            .map(|i| [1e16, 1.0, -1e16, -0.0, -2.0][i % 5] + offset)
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
            "{lhs_shape:?} * {rhs_shape:?}, offset {offset}"
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

#[test]
fn recomputed_multi_read_chain_keeps_leaf_storage_live() {
    use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let scalar = solve::SolveValueType::scalar(element);
    let vector = solve::SolveValueType::tensor(element, vec![8]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fusion_leaf_lifetime.mo"),
        0,
        1,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(957).unwrap()),
            vec![scalar.clone(), scalar, vector.clone()],
            vec![
                solve::SolvePureCallOutput::result(solve::SolveValueType::scalar(element)),
                solve::SolvePureCallOutput::result(vector),
            ],
            at,
            |b, inputs, outputs| {
                let first = b.load(inputs[0], at)?;
                let second = b.load(inputs[1], at)?;
                let bias = b.load(inputs[2], at)?;
                let leaf = b.fill(first, vec![8], at)?;
                let negated = b.unary(solve::SolveUnaryOperator::Negate, leaf, at)?;
                let shifted = b.binary(solve::SolveBinaryOperator::Add, negated, bias, at)?;
                let intervening = b.fill(second, vec![8], at)?;
                let sum = b.reduce(solve::SolveReductionOperator::Sum, intervening, at)?;
                b.store(outputs[0], sum, at)?;
                let squared =
                    b.binary(solve::SolveBinaryOperator::Multiply, shifted, shifted, at)?;
                b.store(outputs[1], squared, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let cells = [vec![1.0_f64], vec![2.0_f64], vec![3.0_f64; 8]];
    let arguments = owner
        .inputs()
        .iter()
        .zip(&cells)
        .map(|(ty, values)| {
            TypedValue::construct(
                ty.clone(),
                values
                    .iter()
                    .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    let reference = eval_pure_call(&table, owner.id(), &arguments).unwrap();
    let expected = reference
        .iter()
        .flat_map(|value| value.elements())
        .map(|cell| match cell {
            solve::SolveValueKind::Real64(bits) => *bits,
            _ => panic!("expected binary64"),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        expected,
        [vec![16.0_f64], vec![4.0_f64; 8]]
            .concat()
            .into_iter()
            .map(f64::to_bits)
            .collect::<Vec<_>>()
    );
    let input = cells
        .into_iter()
        .flatten()
        .map(f64::to_bits)
        .collect::<Vec<_>>();
    let mut output = vec![0; 9];
    CompiledPureCallTable::compile(&table)
        .unwrap()
        .call_cells(
            PureCallInvocation::Primal(&owner.call_site()),
            &input,
            &mut output,
        )
        .unwrap();
    assert_eq!(
        output, expected,
        "deferred leaf must survive the intervening fill"
    );
}

#[test]
fn chained_recomputed_producers_keep_leaf_storage_live() {
    use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let scalar = solve::SolveValueType::scalar(element);
    let vector = solve::SolveValueType::tensor(element, vec![8]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fusion_leaf_lifetime.mo"),
        0,
        1,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(958).unwrap()),
            vec![scalar.clone(), scalar, vector.clone()],
            vec![
                solve::SolvePureCallOutput::result(solve::SolveValueType::scalar(element)),
                solve::SolvePureCallOutput::result(vector),
            ],
            at,
            |b, inputs, outputs| {
                let first = b.load(inputs[0], at)?;
                let second = b.load(inputs[1], at)?;
                let bias = b.load(inputs[2], at)?;
                let leaf = b.fill(first, vec![8], at)?;
                let shifted = b.binary(solve::SolveBinaryOperator::Add, leaf, bias, at)?;
                let squared =
                    b.binary(solve::SolveBinaryOperator::Multiply, shifted, shifted, at)?;
                let intervening = b.fill(second, vec![8], at)?;
                let sum = b.reduce(solve::SolveReductionOperator::Sum, intervening, at)?;
                b.store(outputs[0], sum, at)?;
                let fourth =
                    b.binary(solve::SolveBinaryOperator::Multiply, squared, squared, at)?;
                b.store(outputs[1], fourth, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let cells = [vec![1.0_f64], vec![2.0_f64], vec![3.0_f64; 8]];
    let arguments = owner
        .inputs()
        .iter()
        .zip(&cells)
        .map(|(ty, values)| {
            TypedValue::construct(
                ty.clone(),
                values
                    .iter()
                    .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    let reference = eval_pure_call(&table, owner.id(), &arguments).unwrap();
    let expected = reference
        .iter()
        .flat_map(|value| value.elements())
        .map(|cell| match cell {
            solve::SolveValueKind::Real64(bits) => *bits,
            _ => panic!("expected binary64"),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        expected,
        [vec![16.0_f64], vec![256.0_f64; 8]]
            .concat()
            .into_iter()
            .map(f64::to_bits)
            .collect::<Vec<_>>()
    );
    let input = cells
        .into_iter()
        .flatten()
        .map(f64::to_bits)
        .collect::<Vec<_>>();
    let mut output = vec![0; 9];
    CompiledPureCallTable::compile(&table)
        .unwrap()
        .call_cells(
            PureCallInvocation::Primal(&owner.call_site()),
            &input,
            &mut output,
        )
        .unwrap();
    assert_eq!(
        output, expected,
        "a producer recomputed into a later recomputed reader keeps its leaf live"
    );
}

#[test]
fn recomputed_producer_below_two_fused_producers_keeps_leaf_storage_live() {
    use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let scalar = solve::SolveValueType::scalar(element);
    let vector = solve::SolveValueType::tensor(element, vec![8]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fusion_leaf_lifetime.mo"),
        0,
        1,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(959).unwrap()),
            vec![scalar.clone(), scalar, vector.clone()],
            vec![
                solve::SolvePureCallOutput::result(solve::SolveValueType::scalar(element)),
                solve::SolvePureCallOutput::result(vector),
            ],
            at,
            |b, inputs, outputs| {
                let first = b.load(inputs[0], at)?;
                let second = b.load(inputs[1], at)?;
                let bias = b.load(inputs[2], at)?;
                let leaf = b.fill(first, vec![8], at)?;
                let negated = b.unary(solve::SolveUnaryOperator::Negate, leaf, at)?;
                let magnitude = b.unary(solve::SolveUnaryOperator::Abs, negated, at)?;
                let shifted = b.binary(solve::SolveBinaryOperator::Add, magnitude, bias, at)?;
                let intervening = b.fill(second, vec![8], at)?;
                let sum = b.reduce(solve::SolveReductionOperator::Sum, intervening, at)?;
                b.store(outputs[0], sum, at)?;
                let squared =
                    b.binary(solve::SolveBinaryOperator::Multiply, shifted, shifted, at)?;
                b.store(outputs[1], squared, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let cells = [vec![1.0_f64], vec![2.0_f64], vec![3.0_f64; 8]];
    let arguments = owner
        .inputs()
        .iter()
        .zip(&cells)
        .map(|(ty, values)| {
            TypedValue::construct(
                ty.clone(),
                values
                    .iter()
                    .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    let reference = eval_pure_call(&table, owner.id(), &arguments).unwrap();
    let expected = reference
        .iter()
        .flat_map(|value| value.elements())
        .map(|cell| match cell {
            solve::SolveValueKind::Real64(bits) => *bits,
            _ => panic!("expected binary64"),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        expected,
        [vec![16.0_f64], vec![16.0_f64; 8]]
            .concat()
            .into_iter()
            .map(f64::to_bits)
            .collect::<Vec<_>>()
    );
    let input = cells
        .into_iter()
        .flatten()
        .map(f64::to_bits)
        .collect::<Vec<_>>();
    let mut output = vec![0; 9];
    CompiledPureCallTable::compile(&table)
        .unwrap()
        .call_cells(
            PureCallInvocation::Primal(&owner.call_site()),
            &input,
            &mut output,
        )
        .unwrap();
    assert_eq!(
        output, expected,
        "deferred leaf must survive the intervening fill"
    );
}

#[test]
fn chained_recompute_below_a_fused_producer_keeps_leaf_storage_live() {
    use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let scalar = solve::SolveValueType::scalar(element);
    let vector = solve::SolveValueType::tensor(element, vec![8]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fusion_leaf_lifetime.mo"),
        0,
        1,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(960).unwrap()),
            vec![scalar.clone(), scalar, vector.clone()],
            vec![
                solve::SolvePureCallOutput::result(solve::SolveValueType::scalar(element)),
                solve::SolvePureCallOutput::result(vector),
            ],
            at,
            |b, inputs, outputs| {
                let first = b.load(inputs[0], at)?;
                let second = b.load(inputs[1], at)?;
                let bias = b.load(inputs[2], at)?;
                let leaf = b.fill(first, vec![8], at)?;
                let negated = b.unary(solve::SolveUnaryOperator::Negate, leaf, at)?;
                let shifted = b.binary(solve::SolveBinaryOperator::Add, negated, bias, at)?;
                let squared =
                    b.binary(solve::SolveBinaryOperator::Multiply, shifted, shifted, at)?;
                let intervening = b.fill(second, vec![8], at)?;
                let sum = b.reduce(solve::SolveReductionOperator::Sum, intervening, at)?;
                b.store(outputs[0], sum, at)?;
                let fourth =
                    b.binary(solve::SolveBinaryOperator::Multiply, squared, squared, at)?;
                b.store(outputs[1], fourth, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let cells = [vec![1.0_f64], vec![2.0_f64], vec![3.0_f64; 8]];
    let arguments = owner
        .inputs()
        .iter()
        .zip(&cells)
        .map(|(ty, values)| {
            TypedValue::construct(
                ty.clone(),
                values
                    .iter()
                    .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    let reference = eval_pure_call(&table, owner.id(), &arguments).unwrap();
    let expected = reference
        .iter()
        .flat_map(|value| value.elements())
        .map(|cell| match cell {
            solve::SolveValueKind::Real64(bits) => *bits,
            _ => panic!("expected binary64"),
        })
        .collect::<Vec<_>>();
    assert_eq!(
        expected,
        [vec![16.0_f64], vec![16.0_f64; 8]]
            .concat()
            .into_iter()
            .map(f64::to_bits)
            .collect::<Vec<_>>()
    );
    let input = cells
        .into_iter()
        .flatten()
        .map(f64::to_bits)
        .collect::<Vec<_>>();
    let mut output = vec![0; 9];
    CompiledPureCallTable::compile(&table)
        .unwrap()
        .call_cells(
            PureCallInvocation::Primal(&owner.call_site()),
            &input,
            &mut output,
        )
        .unwrap();
    assert_eq!(
        output, expected,
        "a producer recomputed into a later recomputed reader keeps its leaf live"
    );
}

fn fused_chain_lifetime_table(width: u32, cascade_multi_read: bool) -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let scalar = solve::SolveValueType::scalar(element);
    let vector = solve::SolveValueType::tensor(element, vec![width]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fusion_two_deep_lifetime.mo"),
        0,
        1,
    );
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(958).unwrap()),
            vec![scalar.clone(), scalar.clone(), vector.clone()],
            vec![
                solve::SolvePureCallOutput::result(scalar),
                solve::SolvePureCallOutput::result(vector),
            ],
            at,
            |b, inputs, outputs| {
                let first = b.load(inputs[0], at)?;
                let second = b.load(inputs[1], at)?;
                let bias = b.load(inputs[2], at)?;
                let leaf = b.fill(first, vec![width], at)?;
                let shifted = if cascade_multi_read {
                    let p = b.binary(solve::SolveBinaryOperator::Add, leaf, bias, at)?;
                    b.binary(solve::SolveBinaryOperator::Multiply, p, p, at)?
                } else {
                    let negated = b.unary(solve::SolveUnaryOperator::Negate, leaf, at)?;
                    let absolute = b.unary(solve::SolveUnaryOperator::Abs, negated, at)?;
                    b.binary(solve::SolveBinaryOperator::Add, absolute, bias, at)?
                };
                let intervening = b.fill(second, vec![width], at)?;
                let sum = b.reduce(solve::SolveReductionOperator::Sum, intervening, at)?;
                b.store(outputs[0], sum, at)?;
                let squared =
                    b.binary(solve::SolveBinaryOperator::Multiply, shifted, shifted, at)?;
                b.store(outputs[1], squared, at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn check_fused_chain_lifetime(width: u32, cascade_multi_read: bool) {
    use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};
    let table = fused_chain_lifetime_table(width, cascade_multi_read);
    let owner = &table.owners()[0];
    let values = [vec![1.0_f64], vec![2.0_f64], vec![3.0_f64; width as usize]];
    let arguments = owner
        .inputs()
        .iter()
        .zip(&values)
        .map(|(ty, cells)| {
            TypedValue::construct(
                ty.clone(),
                cells
                    .iter()
                    .map(|x| solve::SolveValueKind::Real64(x.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    let reference = eval_pure_call(&table, owner.id(), &arguments).unwrap();
    let expected = reference
        .iter()
        .flat_map(|value| value.elements())
        .map(|cell| match cell {
            solve::SolveValueKind::Real64(bits) => *bits,
            _ => panic!("expected binary64"),
        })
        .collect::<Vec<_>>();
    let squared = if cascade_multi_read { 256.0 } else { 16.0 };
    let explicit = [vec![2.0 * f64::from(width)], vec![squared; width as usize]];
    assert_eq!(
        expected,
        explicit
            .into_iter()
            .flatten()
            .map(f64::to_bits)
            .collect::<Vec<_>>()
    );
    let inputs = values
        .into_iter()
        .flatten()
        .map(f64::to_bits)
        .collect::<Vec<_>>();
    let mut output = vec![0; 1 + width as usize];
    CompiledPureCallTable::compile(&table)
        .unwrap()
        .call_cells(
            PureCallInvocation::Primal(&owner.call_site()),
            &inputs,
            &mut output,
        )
        .unwrap();
    assert_eq!(
        output, expected,
        "width={width}: both upstream producers must preserve the leaf"
    );
}

#[test]
fn recomputed_two_deep_scalar_chain_keeps_leaf_storage_live() {
    check_fused_chain_lifetime(2, false);
}

#[test]
fn recomputed_two_deep_vector_chain_keeps_leaf_storage_live() {
    check_fused_chain_lifetime(8, false);
}

#[test]
fn recomputed_two_deep_multi_read_producers_keep_leaf_storage_live() {
    check_fused_chain_lifetime(8, true);
}

/// One binary64 owner over `inputs` (tensor extents) whose body is `body`,
/// storing one `output`-extent result; run natively and by the interpreter.
fn native_and_reference(
    inputs: &[Vec<u32>],
    output: Vec<u32>,
    body: impl for<'p> FnOnce(
        &mut solve::TypedProgramBuilder<'p>,
        &[solve::ProgramRegister<'p>],
    ) -> Result<
        solve::ProgramRegister<'p>,
        solve::SolveProgramConstructionError,
    >,
) -> (Vec<u64>, Vec<u64>) {
    use rumoca_eval_solve::{PureCallInvocation, TypedValue, eval_pure_call};
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let tensor =
        |extents: &[u32]| solve::SolveValueType::tensor(element, extents.to_vec()).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_tensor_layout.mo"),
        1,
        2,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(904).unwrap()),
            inputs.iter().map(|extents| tensor(extents)).collect(),
            vec![solve::SolvePureCallOutput::result(tensor(&output))],
            at,
            |b, slots, outputs| {
                let loaded = slots
                    .iter()
                    .map(|slot| b.load(*slot, at))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = body(b, &loaded)?;
                b.store(outputs[0], result, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let cells = inputs
        .iter()
        .enumerate()
        .map(|(input, extents)| {
            let count = extents.iter().product::<u32>() as usize;
            (0..count)
                .map(|i| {
                    let magnitude = [1.0, 1e-3, 37.5, 1e5, 0.125][(i + input) % 5];
                    let sign = if (i + 2 * input) % 3 == 0 { -1.0 } else { 1.0 };
                    sign * magnitude * (1.0 + i as f64 / 7.0)
                })
                .collect::<Vec<f64>>()
        })
        .collect::<Vec<_>>();
    let arguments = owner
        .inputs()
        .iter()
        .zip(&cells)
        .map(|(ty, cells)| {
            let kinds = cells
                .iter()
                .map(|v| solve::SolveValueKind::Real64(v.to_bits()));
            TypedValue::construct(ty.clone(), kinds.collect()).unwrap()
        })
        .collect::<Vec<_>>();
    let reference = eval_pure_call(&table, owner.id(), &arguments).unwrap()[0]
        .elements()
        .iter()
        .map(|cell| match cell {
            solve::SolveValueKind::Real64(bits) => *bits,
            _ => panic!("expected binary64"),
        })
        .collect::<Vec<_>>();
    let input = cells
        .iter()
        .flatten()
        .map(|v| v.to_bits())
        .collect::<Vec<_>>();
    let mut native = vec![0; reference.len()];
    CompiledPureCallTable::compile(&table)
        .unwrap()
        .call_cells(
            PureCallInvocation::Primal(&owner.call_site()),
            &input,
            &mut native,
        )
        .unwrap();
    (native, reference)
}

#[test]
fn published_transposes_above_the_inline_size_move_through_the_host() {
    // 35 and 24 output cells, above the 16 a transpose forms inline; the
    // second moves two-cell trailing elements.
    for (input, output) in [(vec![5, 7], vec![7, 5]), (vec![4, 3, 2], vec![3, 4, 2])] {
        let (native, reference) =
            native_and_reference(std::slice::from_ref(&input), output, |b, operands| {
                b.transpose(
                    operands[0],
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name("native_tensor_layout.mo"),
                        1,
                        2,
                    ),
                )
            });
        assert_eq!(native, reference, "transpose of {input:?}");
    }
}

#[test]
fn products_of_single_use_transposes_read_their_operands_stored_transposed() {
    // Each transpose is read only by a host-sized binary64 product, so the
    // product reads the transposed operand in place: left, right and both.
    let at = || {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("native_tensor_layout.mo"),
            1,
            2,
        )
    };
    let cases = [
        ("left", vec![vec![9, 6], vec![9, 8]], (true, false)),
        ("right", vec![vec![6, 9], vec![8, 9]], (false, true)),
        ("both", vec![vec![9, 6], vec![8, 9]], (true, true)),
        (
            "left, narrow",
            vec![vec![12, 8], vec![12, 2]],
            (true, false),
        ),
    ];
    for (case, inputs, (left, right)) in cases {
        let rows = if left { inputs[0][1] } else { inputs[0][0] };
        let columns = if right { inputs[1][0] } else { inputs[1][1] };
        let (native, reference) =
            native_and_reference(&inputs, vec![rows, columns], |b, operands| {
                let lhs = if left {
                    b.transpose(operands[0], at())?
                } else {
                    operands[0]
                };
                let rhs = if right {
                    b.transpose(operands[1], at())?
                } else {
                    operands[1]
                };
                b.matrix_multiply(lhs, rhs, at())
            });
        assert_eq!(native, reference, "{case}");
    }
}
