use super::*;

#[test]
fn native_cross_disjoint_outputs_keep_independent_vector_snapshot_bits() {
    for destination in [9, 12] {
        let mut native = cross_for_destination(destination);
        for points in [
            [1., 2., 3., 4., 5., 6.],
            [-0., 0., -0., 0., -0., 0.],
            [0.25, -0.5, 2., -1., 3., 0.125],
        ] {
            // Independent source-vector snapshot, before any output is assigned.
            let [a, b, c, x, y, z] = points;
            let expected = [b * z - c * y, c * x - a * z, a * y - b * x];
            native.execute(&[19.; 3], &points, &expected, &[]);
        }
    }
}

fn cross_ops(destination: solve::Reg) -> Vec<solve::LinearOp> {
    rhs_program(
        3,
        vec![
            load_p(3, 0, 6),
            solve::LinearOp::TensorCross {
                dst_start: destination,
                lhs_start: 3,
                rhs_start: 6,
                lanes: 1,
            },
        ],
        destination,
        (destination + 3).max(9),
    )
}

fn cross_for_destination(destination: solve::Reg) -> Program {
    Program::new(vec![cross_ops(destination)], 3, 6)
}

#[test]
fn native_cross_overlapping_destination_versions_refuse_before_emission() {
    for destination in [3, 4, 5, 6, 7] {
        let block = solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_program_spans(
                vec![cross_ops(destination)],
                vec![solve::source_span_from_offsets(1, 5, 10)],
            )
            .unwrap(),
        );
        let targets = (0..3)
            .map(|index| Some(solve::scalar_slot_y(index)))
            .collect::<Vec<_>>();
        let layout = solve::VarLayout::from_parts(Default::default(), 3, 6);
        let error = solve::NativeRefreshAssignmentSchedule::from_continuous_block(
            &block, &targets, &layout,
        )
        .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("overlapping destination versions")
        );
    }
}

fn disjoint_cross_result(program: &mut Program, inputs: &[f64; 6]) -> [f64; 3] {
    program.write(0, &[19.; 3]);
    program.write(program.p_start, inputs);
    program
        .entry
        .call(&mut program.store, (0, program.p_start as i32, 0., -1, -1))
        .unwrap();
    std::array::from_fn(|index| {
        f64::from_le_bytes(
            program.memory.data(&program.store)[index * 8..index * 8 + 8]
                .try_into()
                .unwrap(),
        )
    })
}

#[test]
fn native_cross_disjoint_publication_preserves_generated_ieee_result_bits() {
    let mut disjoint = cross_for_destination(9);
    let inputs = [
        [f64::from_bits(0x7ff8_0000_0000_0005), 2., 3., 4., 5., 6.],
        [1., f64::from_bits(0xfff8_0000_0000_0123), 3., 4., 5., 6.],
        [1., 2., 3., 4., 5., f64::from_bits(0x7ff0_0000_0000_0001)],
        [f64::INFINITY, 0., -0., 4., 5., 6.],
        [f64::from_bits(1), -f64::from_bits(1), 0., 0., 1., -1.],
    ];
    for values in inputs {
        let expected = disjoint_cross_result(&mut disjoint, &values);
        let [a, b, c, x, y, z] = values;
        let oracle = [b * z - c * y, c * x - a * z, a * y - b * x];
        for (actual, independent) in expected.iter().zip(oracle) {
            assert_eq!(actual.is_nan(), independent.is_nan());
        }
        // Compare disjoint storage layouts bit-exactly. Rust/WASM arithmetic
        // NaN-payload selection equivalence is not assumed.
        for destination in [9, 12] {
            cross_for_destination(destination).execute(&[19.; 3], &values, &expected, &[]);
        }
    }
}
