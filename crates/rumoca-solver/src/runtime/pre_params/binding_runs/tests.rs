use super::*;

fn ordinary(dest_p_index: usize, source: solve::PreParamSource) -> solve::PreParamBinding {
    solve::PreParamBinding {
        dest_p_index,
        source,
        clock_schedule: None,
    }
}

fn scheduled(dest_p_index: usize, y_index: usize, period: f64) -> solve::PreParamBinding {
    solve::PreParamBinding {
        dest_p_index,
        source: solve::PreParamSource::Y { index: y_index },
        clock_schedule: Some(
            solve::PeriodicEventSchedule::new(
                rumoca_core::ClockLattice::from_seconds(period, 0.0).unwrap(),
            )
            .unwrap(),
        ),
    }
}

const fn y(index: usize) -> solve::PreParamSource {
    solve::PreParamSource::Y { index }
}

const fn p(index: usize) -> solve::PreParamSource {
    solve::PreParamSource::P { index }
}

fn model(bindings: Vec<solve::PreParamBinding>) -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.pre_param_bindings = bindings;
    model
}

/// Three Y lanes, a break, a scheduled lane, then a P run (sources 0..2, dests
/// 10..12): two ordinary runs, one scheduled, one P run, no aliasing.
fn mixed() -> solve::SolveModel {
    model(vec![
        ordinary(4, y(0)),
        ordinary(5, y(1)),
        ordinary(6, y(2)),
        ordinary(8, y(3)),
        scheduled(7, 4, 0.1),
        ordinary(10, p(0)),
        ordinary(11, p(1)),
        ordinary(12, p(2)),
    ])
}

fn bindings(model: &solve::SolveModel) -> &[solve::PreParamBinding] {
    &model.problem.solve_layout.pre_param_bindings
}

#[test]
fn runs_group_contiguous_lanes_and_keep_scheduled_ones_apart() {
    let model = mixed();
    let plan = PreBindingPlan::new(bindings(&model));
    let shape = plan
        .runs
        .iter()
        .map(|run| (run.first, run.len, run.scheduled))
        .collect::<Vec<_>>();
    assert_eq!(
        shape,
        [(0, 3, false), (3, 1, false), (4, 1, true), (5, 3, false)]
    );
    assert!(!plan.aliased);
}

#[test]
fn commit_matches_the_binding_walk_on_and_off_tick_with_tolerance() {
    let model = mixed();
    let plan = PreBindingPlan::new(bindings(&model));
    let y = [1.0, 2.0, 3.0, 4.0, 5.0];
    for (event_time, tol) in [
        (Some(0.1), 0.0),
        (Some(0.15), 0.0),
        (None, 0.0),
        (Some(0.1), 1.5),
    ] {
        let start = [
            9.0, 1.0, 2.0, 0.0, 1.0, 2.5, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
        ];
        let mut expected = start;
        let expected_changed =
            commit_pre_params_after_event_at(&model, &y, &mut expected, event_time, tol);
        let mut actual = start;
        let actual_changed = plan.commit_after_event_at(&model, &y, &mut actual, event_time, tol);
        assert_eq!(actual, expected, "{event_time:?} tol {tol}");
        assert_eq!(actual_changed, expected_changed, "{event_time:?} tol {tol}");
    }
}

#[test]
fn commit_skips_out_of_range_lanes_like_the_binding_walk() {
    let model = model(vec![
        ordinary(2, y(0)),
        ordinary(3, y(1)),
        ordinary(4, y(2)),
    ]);
    let plan = PreBindingPlan::new(bindings(&model));
    for (y, len) in [(&[7.0, 8.0][..], 5), (&[7.0, 8.0, 9.0][..], 4)] {
        let mut expected = vec![0.0; len];
        commit_pre_params_after_event_at(&model, y, &mut expected, None, 0.0);
        let mut actual = vec![0.0; len];
        plan.commit_after_event_at(&model, y, &mut actual, None, 0.0);
        assert_eq!(actual, expected);
    }
}

#[test]
fn an_aliased_binding_keeps_the_post_event_snapshot() {
    // pre(pre(x)): binding 1 reads binding 0's destination, which binding 0
    // rewrites first. The snapshot read must see the value before that write.
    let model = model(vec![ordinary(1, y(0)), ordinary(2, p(1))]);
    let plan = PreBindingPlan::new(bindings(&model));
    assert!(plan.aliased);
    let mut expected = [0.0, 5.0, 6.0];
    commit_pre_params_after_event_at(&model, &[9.0], &mut expected, None, 0.0);
    let mut actual = [0.0, 5.0, 6.0];
    plan.commit_after_event_at(&model, &[9.0], &mut actual, None, 0.0);
    assert_eq!(actual, expected);
    assert_eq!(actual, [0.0, 9.0, 5.0]);
}

#[test]
fn seed_matches_the_binding_walk_including_its_first_error() {
    let model = mixed();
    let plan = PreBindingPlan::new(bindings(&model));
    let event_pre_y = [1.0, 2.0, 3.0, 4.0, 5.0];
    let event_pre_p = [6.0, 7.0, 8.0];
    let mut expected = [0.0; 13];
    seed_event_entry_pre_params(&model, &event_pre_y, &event_pre_p, &mut expected).unwrap();
    let mut actual = [0.0; 13];
    plan.seed_event_entry(&event_pre_y, &event_pre_p, &mut actual)
        .unwrap();
    assert_eq!(actual, expected);
    for (y_len, p_len, params_len) in [(2, 3, 13), (5, 1, 13), (5, 3, 11), (5, 3, 5), (1, 3, 5)] {
        let mut expected = vec![0.0; params_len];
        let expected = seed_event_entry_pre_params(
            &model,
            &event_pre_y[..y_len],
            &event_pre_p[..p_len],
            &mut expected,
        )
        .unwrap_err()
        .to_string();
        let mut actual = vec![0.0; params_len];
        let actual = plan
            .seed_event_entry(&event_pre_y[..y_len], &event_pre_p[..p_len], &mut actual)
            .unwrap_err()
            .to_string();
        assert_eq!(
            actual, expected,
            "lengths y {y_len} p {p_len} params {params_len}"
        );
    }
}
