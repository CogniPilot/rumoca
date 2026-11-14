use super::*;

/// Program 0 reads `y[3]`, program 1 reads `y[5]` and `p[2]`; the root
/// conditions read `p[2]`.
fn lanes() -> EventOwnerLanes {
    let owner = |y: &[u32], p: &[u32]| OwnerLanes {
        y: y.into(),
        p: p.into(),
    };
    EventOwnerLanes {
        y: [3, 5].into(),
        p: [2].into(),
        roots: owner(&[], &[0]),
        programs: [owner(&[0], &[]), owner(&[1], &[0])].into(),
    }
}

const T: f64 = 0.5;

/// A journal opened over `y`, `p` with both programs evaluated at the first
/// observation.
fn evaluated(lanes: &EventOwnerLanes, y: &[f64], p: &[f64]) -> EventOwnerJournal {
    let mut journal = EventOwnerJournal::default();
    journal.open(Some(lanes), y, p);
    journal.observe(lanes, y, p);
    for program in 0..2 {
        assert_eq!(journal.clean_program(lanes, program, T, &[]), None);
        journal.record_program(program, T, &[], &[program as f64]);
    }
    journal
}

#[test]
fn a_lane_changed_before_an_evaluation_is_read_by_it() {
    let lanes = lanes();
    let (mut y, p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    // Within the next pass the lane changes, and only then is the program
    // evaluated: it reads the new value, so it is dirty at that evaluation
    // and clean after it.
    y[3] = 1.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), None);
    journal.record_program(0, T, &[], &[10.0]);
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), Some(&[10.0][..]));
    // The other program reads neither changed lane.
    assert_eq!(journal.clean_program(&lanes, 1, T, &[]), Some(&[1.0][..]));
}

#[test]
fn a_lane_changed_after_an_evaluation_reruns_it_next_pass() {
    let lanes = lanes();
    let (y, mut p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    // The write lands after both evaluations of this pass.
    p[2] = 4.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 1, T, &[]), None);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), Some(&[0.0][..]));
    assert_eq!(journal.changed_in_event(), [0, 1]);
}

#[test]
fn a_write_of_the_same_bits_is_no_change() {
    let lanes = lanes();
    let (mut y, p) = (vec![2.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    y[5] = 2.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 1, T, &[]), Some(&[1.0][..]));
    assert_eq!(journal.changed_in_event(), [0, 0]);
}

#[test]
fn a_signed_zero_flip_is_a_change() {
    let lanes = lanes();
    let (mut y, p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    y[3] = -0.0;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), None);
    assert_eq!(journal.changed_in_event(), [1, 0]);
}

#[test]
fn another_time_or_other_overrides_or_a_new_event_rerun_every_program() {
    let lanes = lanes();
    let (y, p) = (vec![0.0; 6], vec![0.0; 3]);
    let mut journal = evaluated(&lanes, &y, &p);
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T + 1.0, &[]), None);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[(0, 1.0)]), None);
    assert!(journal.clean_program(&lanes, 0, T, &[]).is_some());
    assert_eq!(journal.clean_roots(&lanes, T), None);
    journal.record_roots(T, &[7.0]);
    assert_eq!(journal.clean_roots(&lanes, T), Some(&[7.0][..]));
    journal.open(Some(&lanes), &y, &p);
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), None);
    assert_eq!(journal.clean_roots(&lanes, T), None);
    // A journal opened without lanes takes nothing from it.
    journal.open(None, &y, &p);
    journal.record_roots(T, &[7.0]);
    assert_eq!(journal.clean_roots(&lanes, T), None);
}

/// One root condition and one discrete row, each `f(y[3], p[7])`, where `f`
/// returns its first input and checks that its second is finite: `p[7]` is
/// read only by the call's guard.
fn guard_only_problem() -> solve::SolveProblem {
    let profile = solve::SolvePureCallTable::default().arithmetic();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let span = solve::source_span_from_offsets(813, 1, 2);
    let table = solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(813).unwrap()),
            vec![real.clone(), real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span,
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span)?;
                let q = builder.load(inputs[1], span)?;
                builder.check_finite(
                    q,
                    solve::SolveFiniteCheck::Primal {
                        program: 7,
                        operation: 3,
                    },
                    span,
                )?;
                builder.store(outputs[0], x, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let program = vec![
        solve::LinearOp::LoadY { dst: 0, index: 3 },
        solve::LinearOp::LoadP { dst: 1, index: 7 },
        solve::LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: table.owners()[0].call_site(),
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ];
    let block = || {
        solve::ScalarProgramBlock::with_output_indices(
            vec![program.clone()],
            vec![solve::source_span_from_offsets(814, 0, 1)],
            vec![0],
        )
        .unwrap()
    };
    let mut problem = solve::SolveProblem::default();
    problem.events.root_conditions = block();
    problem.discrete.rhs = block();
    problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut problem)
            .expect("refresh owners construct");
    solve::issue_event_owner_reach(&mut problem);
    problem
}

#[test]
fn an_owner_whose_guard_reads_a_written_lane_is_not_clean() {
    let problem = guard_only_problem();
    let lanes =
        EventOwnerLanes::for_problem(&problem).expect("the problem admits the restricted update");
    let (y, mut p) = (vec![0.0; 6], vec![1.0; 8]);
    let mut journal = EventOwnerJournal::default();
    journal.open(Some(&lanes), &y, &p);
    journal.observe(&lanes, &y, &p);
    journal.record_roots(T, &[0.0]);
    journal.record_program(0, T, &[], &[0.0]);
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_roots(&lanes, T), Some(&[0.0][..]));
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), Some(&[0.0][..]));
    // A write of the slot only the guard reads: evaluated again, the call
    // would fail its finite check, so neither owner may keep its last value.
    p[7] = f64::NAN;
    journal.observe(&lanes, &y, &p);
    assert_eq!(journal.clean_roots(&lanes, T), None);
    assert_eq!(journal.clean_program(&lanes, 0, T, &[]), None);
}
