use super::*;
use solve::{BinaryOp as B, LinearOp as L};

fn executor(fail_second: bool) -> CausalSeedSchedules {
    let first = vec![
        L::LoadY { dst: 0, index: 0 },
        L::LoadP { dst: 1, index: 0 },
        L::LoadTime { dst: 2 },
        L::Binary {
            dst: 3,
            op: B::Add,
            lhs: 1,
            rhs: 2,
        },
        L::Binary {
            dst: 4,
            op: B::Mul,
            lhs: 0,
            rhs: 3,
        },
        L::StoreOutput { src: 4 },
    ];
    let second = if fail_second {
        vec![
            L::LoadY { dst: 0, index: 1 },
            L::Const { dst: 1, value: 0.0 },
            L::Const { dst: 2, value: 1.0 },
            L::TableLookup {
                dst: 3,
                table_id: 1,
                column: 2,
                input: 0,
            },
            L::StoreOutput { src: 3 },
        ]
    } else {
        vec![
            L::LoadY { dst: 0, index: 1 },
            L::LoadY { dst: 1, index: 0 },
            L::Binary {
                dst: 2,
                op: B::Add,
                lhs: 0,
                rhs: 1,
            },
            L::StoreOutput { src: 2 },
        ]
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("causal_seed.mo"),
        1,
        2,
    );
    let primal = solve::ScalarProgramBlock::with_output_indices(
        vec![first, second],
        vec![span; 2],
        vec![0, 1],
    )
    .unwrap();
    let tangent = solve_eval::ad::lower_scalar_program_block_full_jvp(&primal, 3).unwrap();
    let program = DirectionalProgram::prepare("test_seed", tangent, None, None).unwrap();
    let mut schedules = CausalSeedSchedules::default();
    schedules.schedules.insert(
        Default::default(),
        Rc::new(SeedSchedule {
            program,
            targets: vec![vec![1], vec![2]],
        }),
    );
    schedules
}

fn execute(
    executor: &CausalSeedSchedules,
    y: &[f64],
    p: &[f64],
    time: f64,
    seed: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    executor.apply_selected(
        &[(false, Default::default())],
        AlgebraicLinearization {
            t: time,
            params: p,
            settle: AlgebraicSettle {
                tol: 1e-10,
                max_iters: 4,
            },
        },
        y,
        seed,
        RowEvalContext::default(),
    )
}

#[test]
fn full_parameter_seed_and_prior_segment_commit_remain_live_on_every_call() {
    let executor = executor(false);
    for (x, p, time, sx, sp) in [
        (2.0, 3.0, 0.0, 1.0, 0.0),
        (2.0, 3.0, 0.0, -2.0, 1.0),
        (4.0, 5.0, 0.5, 2.0, -1.0),
    ] {
        let mut seed = [sx, 99.0, 88.0, sp];
        let y = [x, (p + time) * x, (p + time + 1.0) * x];
        execute(&executor, &y, &[p], time, &mut seed).unwrap();
        let first = (p + time) * sx + x * sp;
        assert_eq!(seed, [sx, first, first + sx, sp]);
    }
}

#[test]
fn a_later_segment_failure_restores_every_incoming_seed_bit() {
    let executor = executor(true);
    let mut seed: [f64; 4] = [1.0, 99.0, 88.0, -0.0];
    let before = seed.map(f64::to_bits);
    let mut unprotected = seed;
    let lin = AlgebraicLinearization {
        t: 0.0,
        params: &[3.0],
        settle: AlgebraicSettle {
            tol: 1e-10,
            max_iters: 4,
        },
    };
    assert!(
        executor
            .apply_stages(
                &[(false, Default::default())],
                lin,
                &[2.0, 6.0, 8.0],
                &mut unprotected,
                RowEvalContext::default()
            )
            .is_err()
    );
    assert_eq!(
        unprotected[1], 3.0,
        "the first segment committed before the table fault"
    );
    assert_eq!(unprotected[2], 88.0);
    assert!(execute(&executor, &[2.0, 6.0, 8.0], &[3.0], 0.0, &mut seed).is_err());
    assert_eq!(seed.map(f64::to_bits), before);
}

#[test]
fn an_uncertified_plan_declines_without_touching_seed_storage() {
    let executor = executor(false);
    let mut seed: [f64; 4] = [-0.0, 99.0, 88.0, -0.0];
    let before = seed.map(f64::to_bits);
    let lin = AlgebraicLinearization {
        t: 0.0,
        params: &[3.0],
        settle: AlgebraicSettle {
            tol: 1e-10,
            max_iters: 4,
        },
    };
    assert!(
        !executor
            .apply(
                &solve::RefreshPlan::default(),
                lin,
                &[2.0, 6.0, 8.0],
                &mut seed,
                RowEvalContext::default()
            )
            .unwrap()
    );
    assert_eq!(seed.map(f64::to_bits), before);
}

#[test]
fn mixed_projection_failure_restores_the_entire_incoming_seed() {
    let mut schedules = executor(false);
    schedules.mixed_steps = vec![
        mixed::Step::Assignment(Default::default()),
        mixed::Step::Projection {
            block_index: 0,
            plan: Default::default(),
        },
    ]
    .into();
    let original = [2.0, 9.0, 8.0, 3.0];
    let mut seed = original;
    let result = schedules.apply_mixed(
        AlgebraicLinearization {
            t: 0.0,
            params: &[3.0],
            settle: AlgebraicSettle {
                tol: 1e-10,
                max_iters: 4,
            },
        },
        &[2.0, 6.0, 8.0],
        &mut seed,
        RowEvalContext::default(),
        |_, _, seed| {
            seed[2] = 42.0;
            Err(RuntimeSolveError::solve_ir("directed projection refusal"))
        },
    );
    assert!(result.is_err());
    assert_eq!(seed, original);
}

#[test]
fn missing_mixed_assignment_declines_before_any_projection_or_seed_mutation() {
    let mut schedules = executor(false);
    schedules.mixed_steps = vec![
        mixed::Step::Projection {
            block_index: 0,
            plan: Default::default(),
        },
        mixed::Step::Assignment(Default::default()),
    ]
    .into();
    schedules.schedules.clear();
    let original = [1.0, 2.0, 3.0, 4.0];
    let mut seed = original;
    let accepted = schedules
        .apply_mixed(
            AlgebraicLinearization {
                t: 0.0,
                params: &[3.0],
                settle: AlgebraicSettle {
                    tol: 1e-10,
                    max_iters: 4,
                },
            },
            &[2.0, 6.0, 8.0],
            &mut seed,
            RowEvalContext::default(),
            |_, _, _| panic!("unprepared sweep executed a projection"),
        )
        .unwrap();
    assert!(!accepted);
    assert_eq!(seed, original);
}
