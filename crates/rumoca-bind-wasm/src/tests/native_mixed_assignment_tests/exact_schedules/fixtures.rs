//! Full owned vectors; only source-issued scalar slots receive fixture values.
use super::*;

pub(super) struct Case {
    pub(super) name: &'static str,
    pub(super) y: Vec<f64>,
    pub(super) p: Vec<f64>,
    pub(super) time: f64,
}

pub(super) fn cases(model: &solve::SolveModel) -> Vec<Case> {
    let mut cases = vec![Case {
        name: "source-initial",
        y: model.initial_y.clone(),
        p: model.parameters.clone(),
        time: 0.,
    }];
    cases.extend(
        specifications()
            .into_iter()
            .map(|spec| varied_case(model, spec)),
    );
    for case in &cases {
        assert_eq!(case.y.len(), model.problem.layout.y_scalars());
        assert_eq!(case.p.len(), model.problem.layout.p_scalars());
        assert!(case.y.iter().chain(&case.p).all(|v| v.is_finite()));
    }
    cases
}

fn set(case: &mut Case, layout: &solve::VarLayout, name: &str, value: f64) {
    match layout
        .binding(name)
        .unwrap_or_else(|| panic!("original source slot {name} absent"))
    {
        solve::ScalarSlot::Y { index, .. } => case.y[index] = value,
        solve::ScalarSlot::P { index, .. } => case.p[index] = value,
        _ => panic!("source slot {name} is not a mutable checked runtime input/state"),
    }
}

pub(super) fn bytes(values: &[f64]) -> Vec<u8> {
    values
        .iter()
        .flat_map(|value| value.to_le_bytes())
        .collect()
}

type Specification = (
    &'static str,
    f64,
    [f64; 4],
    [f64; 3],
    [f64; 3],
    [f64; 4],
    [f64; 4],
    f64,
);

fn specifications() -> [Specification; 3] {
    [
        (
            "six-dof",
            2.0,
            [0.5, 0.5, 0.5, 0.5],
            [1.25, -0.75, 0.4],
            [0.2, -0.3, 0.15],
            [320., 410., 360., 390.],
            [1., -0.5, 0.25, 0.2],
            1.25,
        ),
        (
            "near-ground",
            0.05,
            [1., 0., 0., 0.],
            [-0.25, 0.5, -0.1],
            [-0.1, 0.05, -0.2],
            [0., 110., 170., 230.],
            [-0.5, 0.25, -0.25, -0.1],
            0.5,
        ),
        (
            "tour-long-time",
            3.0,
            [0., 1., 0., 0.],
            [0.1, -0.2, 0.3],
            [0.4, 0.1, -0.1],
            [430., 450., 470., 490.],
            [0., 0., 0., 0.],
            1000.,
        ),
    ]
}

fn varied_case(model: &solve::SolveModel, spec: Specification) -> Case {
    let (name, altitude, quaternion, velocity, rates, motors, control, time) = spec;

    let mut case = Case {
        name,
        y: model.initial_y.clone(),
        p: model.parameters.clone(),
        time,
    };
    set(&mut case, &model.problem.layout, "vehicle.p[3]", altitude);
    for (base, values) in [
        ("vehicle.q", &quaternion[..]),
        ("vehicle.v_b", &velocity[..]),
        ("vehicle.omega", &rates[..]),
    ] {
        for (index, &value) in values.iter().enumerate() {
            set(
                &mut case,
                &model.problem.layout,
                &format!("{base}[{}]", index + 1),
                value,
            );
        }
    }
    for (index, value) in motors.into_iter().enumerate() {
        set(
            &mut case,
            &model.problem.layout,
            &format!("vehicle.motor[{}].omega", index + 1),
            value,
        );
    }
    for (name, value) in ["forward", "left", "up", "yaw"].into_iter().zip(control) {
        set(&mut case, &model.problem.layout, name, value);
    }
    set(&mut case, &model.problem.layout, "autopilot", 1.);
    set(
        &mut case,
        &model.problem.layout,
        "indoorTour",
        if name == "tour-long-time" { 1. } else { 0. },
    );
    set(&mut case, &model.problem.layout, "commandTime", time);
    case
}
