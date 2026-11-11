//! Every tensor operation kind over static shapes in both issued forms
//! (straight cells up to `MAX_UNROLLED_PASSES` cells and
//! `MAX_UNROLLED_OPERATIONS` scalar operations, counted loops past either),
//! against the canonical evaluator bit for bit over inputs that include NaN,
//! signed zeros, infinities and subnormals, both in the root and in a child
//! that receives its operands as arguments.
use super::*;
use solve::SolveBinaryOperator as B;

type Build =
    dyn for<'p> Fn(
        &mut solve::TypedProgramBuilder<'p>,
        &[solve::ProgramRegister<'p>],
    ) -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError>;

fn real_type(shape: &[u32]) -> solve::SolveValueType {
    let scalar = solve::SolveScalarType::real(profile());
    match shape {
        [] => solve::SolveValueType::scalar(scalar),
        _ => solve::SolveValueType::tensor(scalar, shape.to_vec()).unwrap(),
    }
}

fn integer_type(shape: &[u32]) -> solve::SolveValueType {
    let scalar = solve::SolveScalarType::integer(profile());
    match shape {
        [] => solve::SolveValueType::scalar(scalar),
        _ => solve::SolveValueType::tensor(scalar, shape.to_vec()).unwrap(),
    }
}

/// The operation in the root (`through_child` false) or in a child the root
/// calls with its loaded inputs.
fn table(
    inputs: &[solve::SolveValueType],
    output: &solve::SolveValueType,
    through_child: bool,
    build: &Build,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = |builder: &mut solve::SolvePureCallTableBuilder, id| {
        builder
            .add_owner(
                identity(id),
                inputs.to_vec(),
                vec![solve::SolvePureCallOutput::result(output.clone())],
                span(26000),
                |b, slots, outputs| {
                    let registers = slots
                        .iter()
                        .map(|slot| b.load(*slot, span(26001)))
                        .collect::<Result<Vec<_>, _>>()?;
                    let result = build(b, &registers)?;
                    b.store(outputs[0], result, span(26002))
                },
            )
            .unwrap()
    };
    let mut root = owner(&mut builder, 260);
    if through_child {
        let child = root;
        root = builder
            .add_owner(
                identity(261),
                inputs.to_vec(),
                vec![solve::SolvePureCallOutput::result(output.clone())],
                span(26010),
                |b, slots, outputs| {
                    let registers = slots
                        .iter()
                        .map(|slot| b.load(*slot, span(26011)))
                        .collect::<Result<Vec<_>, _>>()?;
                    let result = b.call(child, &registers, span(26012))?;
                    b.store(outputs[0], result[0], span(26013))
                },
            )
            .unwrap();
    }
    let site = builder.call_site(root).unwrap();
    (builder.finish(), site)
}

/// Deterministic inputs mixing the IEEE special values with ordinary ones.
struct Values(u64);

impl Values {
    fn next(&mut self) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        self.0 >> 11
    }

    fn real(&mut self) -> f64 {
        const SPECIAL: [f64; 10] = [
            f64::NAN,
            0.0,
            -0.0,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::MIN_POSITIVE,
            1e308,
            -1e-300,
            1.0,
            -3.0,
        ];
        match self.next() % 16 {
            n @ 0..10 => SPECIAL[n as usize],
            10 => f64::from_bits(1 + self.next() % 0x000f_ffff_ffff_ffff),
            11 => -f64::from_bits(1 + self.next() % 0x000f_ffff_ffff_ffff),
            _ => (self.next() % 2001) as f64 / 64.0 - 15.625,
        }
    }

    fn integer(&mut self) -> i64 {
        match self.next() % 8 {
            0 => i64::MAX,
            1 => i64::MIN,
            2 => 0,
            3..6 => 1 + (self.next() % 2) as i64,
            _ => (self.next() % 2001) as i64 - 1000,
        }
    }
}

fn count(value_type: &solve::SolveValueType) -> usize {
    value_type.scalar_count() as usize
}

/// Compile both ways and compare with the evaluator over many inputs.
fn check(inputs: &[solve::SolveValueType], output: solve::SolveValueType, build: &Build) {
    for through_child in [false, true] {
        let (table, site) = table(inputs, &output, through_child, build);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let mut runner = Runner::new(&compiled);
        let mut values = Values(0x9e37_79b9_7f4a_7c15);
        for _ in 0..24 {
            let arguments = inputs
                .iter()
                .map(|value_type| {
                    let is_real = matches!(
                        value_type.element_type(),
                        solve::SolveScalarType::Real { .. }
                    );
                    (0..count(value_type))
                        .map(|_| match is_real {
                            true => real(values.real()),
                            false => solve::SolveValueKind::Integer(values.integer()),
                        })
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            let (status, actual) = runner.run(&cells(arguments.iter().flatten().copied()));
            match oracle(&table, &site, &arguments) {
                Ok(expected) => {
                    assert_eq!(status, 0, "{inputs:?} through child {through_child}");
                    same_cells(&actual, &expected, &output);
                }
                Err(_) => assert_ne!(status, 0, "{inputs:?} through child {through_child}"),
            }
        }
    }
}

/// Equal bits in every cell, except that a Real cell may hold a NaN of either
/// sign where the evaluator holds one: the sign and payload of a NaN an
/// arithmetic operation produces are not specified by WebAssembly, and the
/// host evaluator and the WebAssembly engine differ in them.
fn same_cells(actual: &[u8], expected: &[u8], output: &solve::SolveValueType) {
    assert_eq!(actual.len(), expected.len());
    let real = matches!(output.element_type(), solve::SolveScalarType::Real { .. });
    for (cell, (actual, expected)) in actual
        .chunks_exact(8)
        .zip(expected.chunks_exact(8))
        .enumerate()
    {
        let (a, e) = (
            f64::from_le_bytes(actual.try_into().unwrap()),
            f64::from_le_bytes(expected.try_into().unwrap()),
        );
        if !(real && a.is_nan() && e.is_nan()) {
            assert_eq!(a.to_bits(), e.to_bits(), "cell {cell} of {output:?}");
        }
    }
}

/// The small shapes of rigid body models, and cell counts around the issued
/// pass bound (32).
const FLAT: [&[u32]; 10] = [
    &[1],
    &[3],
    &[9],
    &[3, 3],
    &[31],
    &[32],
    &[33],
    &[4, 8],
    &[6, 6],
    &[2, 3, 3],
];

#[test]
fn elementwise_unary_binary_broadcast_and_scale_match_the_evaluator_bit_for_bit() {
    for shape in FLAT {
        let ty = real_type(shape);
        for operator in [B::Add, B::Subtract, B::Multiply, B::Divide, B::Min, B::Max] {
            check_binary(&ty, operator);
        }
        for operator in [
            solve::SolveUnaryOperator::Negate,
            solve::SolveUnaryOperator::Abs,
        ] {
            check(std::slice::from_ref(&ty), ty.clone(), &move |b, r| {
                b.unary(operator, r[0], span(26102))
            });
        }
        check(&[ty.clone(), real_type(&[])], ty.clone(), &|b, r| {
            b.scale(r[0], r[1], span(26103))
        });
        let integers = integer_type(shape);
        for operator in [B::Add, B::Multiply] {
            check(
                &[integers.clone(), integers.clone()],
                integers.clone(),
                &move |b, r| b.binary(operator, r[0], r[1], span(26104)),
            );
        }
    }
}

/// One binary operator between two aggregates and broadcast either way.
fn check_binary(ty: &solve::SolveValueType, operator: solve::SolveBinaryOperator) {
    check(&[ty.clone(), ty.clone()], ty.clone(), &move |b, r| {
        b.binary(operator, r[0], r[1], span(26100))
    });
    for scalar_on_lhs in [false, true] {
        check(&[ty.clone(), real_type(&[])], ty.clone(), &move |b, r| {
            b.broadcast_binary(operator, r[0], r[1], scalar_on_lhs, span(26101))
        });
    }
}

#[test]
fn reductions_match_the_evaluator_bit_for_bit() {
    use solve::SolveReductionOperator as R;
    for extent in [1, 3, 9, 32, 33, 34, 36] {
        for operator in [R::Sum, R::Product, R::Minimum, R::Maximum] {
            check(&[real_type(&[extent])], real_type(&[]), &move |b, r| {
                b.reduce(operator, r[0], span(26200))
            });
        }
        check(&[integer_type(&[extent])], integer_type(&[]), &|b, r| {
            b.reduce(R::Sum, r[0], span(26201))
        });
    }
}

#[test]
fn matrix_products_match_the_evaluator_bit_for_bit() {
    for (left, right, output) in [
        (&[3][..], &[3][..], &[][..]),
        (&[3, 3][..], &[3, 3][..], &[3, 3][..]),
        (&[3, 3][..], &[3][..], &[3][..]),
        (&[3][..], &[3, 3][..], &[3][..]),
        (&[6, 6][..], &[6, 6][..], &[6, 6][..]),
        (&[3, 2][..], &[2, 5][..], &[3, 5][..]),
        (&[4, 8][..], &[8, 8][..], &[4, 8][..]),
        (&[33, 2][..], &[2][..], &[33][..]),
        (&[2, 256][..], &[256, 2][..], &[2, 2][..]),
        (&[2, 257][..], &[257, 2][..], &[2, 2][..]),
        (&[1][..], &[1][..], &[][..]),
    ] {
        check(
            &[real_type(left), real_type(right)],
            real_type(output),
            &|b, r| b.matrix_multiply(r[0], r[1], span(26300)),
        );
    }
}

#[test]
fn transposes_identities_and_diagonals_match_the_evaluator_bit_for_bit() {
    for (shape, transposed) in [
        (&[3, 3][..], &[3, 3][..]),
        (&[3, 5][..], &[5, 3][..]),
        (&[4, 4][..], &[4, 4][..]),
        (&[1, 33][..], &[33, 1][..]),
        (&[4, 8][..], &[8, 4][..]),
        (&[6, 6][..], &[6, 6][..]),
        (&[2, 3, 3][..], &[3, 2, 3][..]),
    ] {
        check(&[real_type(shape)], real_type(transposed), &|b, r| {
            b.transpose(r[0], span(26400))
        });
    }
    for extent in [1, 3, 4, 5, 6] {
        let shape = [extent, extent];
        check(&[real_type(&[])], real_type(&shape), &move |b, r| {
            let identity =
                b.identity(solve::SolveScalarType::real(profile()), extent, span(26401))?;
            // Keep the scalar input live so every case has one argument.
            b.broadcast_binary(B::Multiply, identity, r[0], false, span(26402))
        });
        check(&[real_type(&[extent])], real_type(&shape), &|b, r| {
            b.diagonal(r[0], span(26403))
        });
    }
}

#[test]
fn concatenations_slices_and_views_match_the_evaluator_bit_for_bit() {
    for (axis, left, right, output) in [
        // Concatenation promotes operands to rank two.
        (0, &[3][..], &[3][..], &[6, 1][..]),
        (1, &[3, 3][..], &[3, 2][..], &[3, 5][..]),
        (1, &[33, 1][..], &[33, 2][..], &[33, 3][..]),
        (0, &[2, 3][..], &[1, 3][..], &[3, 3][..]),
    ] {
        check(
            &[real_type(left), real_type(right)],
            real_type(output),
            &move |b, r| b.concatenate(axis, &[r[0], r[1]], span(26500)),
        );
    }
    for (shape, origin, dimensions) in [
        (&[6, 6][..], &[1, 2][..], &[3, 3][..]),
        (&[9, 9][..], &[2, 1][..], &[4, 4][..]),
        (&[40][..], &[2][..], &[33][..]),
        (&[9, 9][..], &[0, 3][..], &[3, 6][..]),
    ] {
        check(&[real_type(shape)], real_type(dimensions), &|b, r| {
            b.project_slice(r[0], origin.to_vec(), dimensions.to_vec(), span(26501))
        });
        check(
            &[real_type(shape), real_type(dimensions)],
            real_type(shape),
            &|b, r| b.update_slice(r[0], r[1], origin.to_vec(), span(26502)),
        );
    }
    // A one-based row index read at run time, then every column.
    for (rows, columns) in [(3, 3), (6, 6), (2, 33)] {
        let index = integer_type(&[]);
        check(
            &[real_type(&[rows, columns]), index.clone()],
            real_type(&[columns]),
            &move |b, r| b.project_view(r[0], &row_axes(r[1], columns), span(26504)),
        );
        check(
            &[real_type(&[rows, columns]), real_type(&[columns]), index],
            real_type(&[rows, columns]),
            &move |b, r| b.update_view(r[0], r[1], &row_axes(r[2], columns), span(26505)),
        );
    }
}

fn row_axes(
    row: solve::ProgramRegister<'_>,
    columns: u32,
) -> [solve::ProgramTensorViewAxis<'_>; 2] {
    [
        solve::ProgramTensorViewAxis::Index(row),
        solve::ProgramTensorViewAxis::Span {
            origin: 0,
            extent: columns,
        },
    ]
}

#[test]
fn seven_axis_slices_and_five_operand_concatenations_match_the_evaluator_bit_for_bit() {
    // Counted: 128 cells over seven axes of extent two.
    let (shape, origin, dimensions) = ([3; 7], [1, 0, 1, 0, 1, 0, 1], [2; 7]);
    check(
        &[real_type(&shape)],
        real_type(&dimensions),
        &move |b, r| b.project_slice(r[0], origin.to_vec(), dimensions.to_vec(), span(26600)),
    );
    check(
        &[real_type(&shape), real_type(&dimensions)],
        real_type(&shape),
        &move |b, r| b.update_slice(r[0], r[1], origin.to_vec(), span(26601)),
    );
    // Five operands, which a child receives as five arguments.
    for (rows, output) in [(3, [3, 10]), (33, [33, 10])] {
        let operand = real_type(&[rows, 2]);
        let operands: [_; 5] = std::array::from_fn(|_| operand.clone());
        check(&operands, real_type(&output), &|b, r| {
            b.concatenate(1, r, span(26602))
        });
    }
}
