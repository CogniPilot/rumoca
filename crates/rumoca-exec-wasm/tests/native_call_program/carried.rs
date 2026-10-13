//! Carried state (SOLVE-C101): a run of host Y cells is published into the P
//! cells that hold the previous value of the same state.
use super::stage_glue::{Shape, scale_table, schedule};
use super::*;

const N: usize = 600;

/// Parameters of the staged program: 2N tensor cells, the exponent and a
/// scale factor.
fn parameters() -> Vec<f64> {
    let mut p = (0..2 * N)
        .map(|i| (i as f64 - 311.5) * 0.375)
        .collect::<Vec<_>>();
    p.extend([2.0, 3.0]);
    p
}

fn sentinel(bytes: &[u8]) -> bool {
    bytes.iter().all(|byte| *byte == 0xa5)
}

fn cells(bytes: &[u8]) -> Vec<f64> {
    bytes
        .chunks_exact(8)
        .map(|c| f64::from_le_bytes(c.try_into().unwrap()))
        .collect()
}

const SHAPES: [Shape; 4] = [
    Shape {
        computed_input: false,
        transformed_output: false,
        memoized: false,
    },
    Shape {
        computed_input: true,
        transformed_output: false,
        memoized: false,
    },
    Shape {
        computed_input: false,
        transformed_output: true,
        memoized: false,
    },
    Shape {
        computed_input: false,
        transformed_output: false,
        memoized: true,
    },
];

/// Every pairing the staged program admits: Y cells `0..N` hold the built
/// value and `N..2N` the input `P[0..N]` returned unchanged.
fn pairings() -> Vec<Vec<(std::ops::Range<usize>, std::ops::Range<usize>)>> {
    vec![
        // The returned input paired with its own cells: the value is in place.
        vec![(N..2 * N, 0..N)],
        // The built value published over the cells of the second tensor.
        vec![(0..N, N..2 * N)],
        // The returned input copied into other cells.
        vec![(N..2 * N, N..2 * N)],
        // Both outputs onto the cells they read: a parallel assignment, so the
        // returned input is the previous value of the cells overwritten first.
        vec![(0..N, 0..N), (N..2 * N, N..2 * N)],
        // The two outputs swapped onto each other's input cells.
        vec![(0..N, N..2 * N), (N..2 * N, 0..N)],
        // One run over both outputs: each output commits its own part.
        vec![(0..2 * N, 0..2 * N)],
        // Runs over part of an output, and over the end of one and the start of the next.
        vec![(N + 100..N + 300, 0..200)],
        vec![(N - 50..N + 50, N + 100..N + 200)],
    ]
}

fn host() -> solve::NativeCarriedHost {
    solve::NativeCarriedHost::new(
        (2 * N, 2 * N + 2),
        std::iter::once(0..2 * N),
        std::iter::once(0..2 * N + 2),
    )
}

#[test]
fn carried_runs_publish_the_values_the_uncarried_program_publishes_to_y() {
    for shape in SHAPES {
        let (table, site) = scale_table();
        let (plain, layout) = schedule(shape, &site);
        let compiled =
            compile_native_assignment_schedule_with_calls_wasm(&plain, &layout, &table).unwrap();
        let (status, work_y) = ProgramRunner::new(&compiled, &layout).run(&parameters());
        assert_eq!(status, 0);
        for pairs in pairings() {
            let state = solve::NativeCarriedState::construct_for_host(&host(), &pairs).unwrap();
            let carrying = plain.with_carried_state(state.clone());
            let compiled =
                compile_native_assignment_schedule_with_calls_wasm(&carrying, &layout, &table)
                    .unwrap();
            assert_eq!(compiled.carried_state(), &state);
            let (status, y, p) = ProgramRunner::new(&compiled, &layout).run_carried(&parameters());
            assert_eq!(status, 0, "{pairs:?}");
            let mut expected = parameters();
            state.publish(&work_y, &mut expected);
            for (cell, (got, want)) in p.iter().zip(&expected).enumerate() {
                assert_eq!(got.to_bits(), want.to_bits(), "P[{cell}] for {pairs:?}");
            }
            // The host Y receives the cells that are not carried and nothing else.
            for cell in 0..2 * N {
                let bytes = &y[8 * cell..8 * cell + 8];
                match state.run_of_y(cell) {
                    Some(_) => assert!(sentinel(bytes), "carried Y[{cell}] was written"),
                    None => assert_eq!(cells(bytes)[0].to_bits(), work_y[cell].to_bits()),
                }
            }
        }
    }
}
