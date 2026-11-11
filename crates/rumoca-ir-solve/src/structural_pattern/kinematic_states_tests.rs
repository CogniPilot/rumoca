//! The kinematic state partition on small authored systems.

use rumoca_core::{SourceId, Span};

use super::{KinematicStates, NonKinematicReason};
use crate::{
    AlgebraicProjectionBlock, AlgebraicProjectionPlan, BinaryOp, CompareOp, LinearOp,
    ScalarProgramBlock, UnaryOp,
};

fn block(programs: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    let spans = vec![
        Span::from_offsets(SourceId::from_source_name("kinematic_states.mo"), 0, 1);
        programs.len()
    ];
    ScalarProgramBlock::with_program_spans(programs, spans).expect("valid test programs")
}

/// `y[index]`.
fn copy(index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index },
        LinearOp::StoreOutput { src: 0 },
    ]
}

/// `coefficient * y[index]`.
fn scaled(index: usize, coefficient: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index },
        LinearOp::Const {
            dst: 1,
            value: coefficient,
        },
        mul(2, 0, 1),
        LinearOp::StoreOutput { src: 2 },
    ]
}

/// `y[lhs] - coefficient * y[rhs]`, the residual of
/// `y[lhs] = coefficient * y[rhs]`.
fn residual(lhs: usize, rhs: usize, coefficient: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: lhs },
        LinearOp::LoadY { dst: 1, index: rhs },
        LinearOp::Const {
            dst: 2,
            value: coefficient,
        },
        mul(3, 1, 2),
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ]
}

fn mul(dst: u32, lhs: u32, rhs: u32) -> LinearOp {
    LinearOp::Binary {
        dst,
        op: BinaryOp::Mul,
        lhs,
        rhs,
    }
}

fn projection(blocks: Vec<(Vec<usize>, Vec<usize>)>) -> AlgebraicProjectionPlan {
    AlgebraicProjectionPlan {
        blocks: blocks
            .into_iter()
            .map(|(rows, y_indices)| AlgebraicProjectionBlock {
                rows,
                y_indices,
                ..AlgebraicProjectionBlock::default()
            })
            .collect::<Vec<_>>()
            .into(),
    }
}

/// The derivative rows, the implicit rows and the projection solving them.
struct System {
    derivative: ScalarProgramBlock,
    implicit: ScalarProgramBlock,
    plan: AlgebraicProjectionPlan,
}

fn system(
    derivative: Vec<Vec<LinearOp>>,
    implicit: Vec<Vec<LinearOp>>,
    plan: AlgebraicProjectionPlan,
) -> System {
    System {
        derivative: block(derivative),
        implicit: block(implicit),
        plan,
    }
}

fn partition(system: &System, states: usize, solver: usize) -> KinematicStates {
    KinematicStates::derive(
        &system.derivative,
        &system.implicit,
        &system.plan,
        states,
        solver,
    )
}

fn kinematic(system: &System, states: usize, solver: usize) -> Vec<usize> {
    partition(system, states, solver).states().to_vec()
}

/// The `der(q) = v` form is what tells a position from a velocity: the
/// oscillator `x' = v, v' = -x` is otherwise symmetric.
#[test]
fn a_pendulum_angle_is_kinematic_and_its_speed_is_not() {
    let sine = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Sin,
            arg: 0,
        },
        LinearOp::Const {
            dst: 2,
            value: -9.81,
        },
        mul(3, 1, 2),
        LinearOp::StoreOutput { src: 3 },
    ];
    let pendulum = system(vec![copy(1), sine], Vec::new(), projection(Vec::new()));
    assert_eq!(kinematic(&pendulum, 2, 2), [0]);
    assert_eq!(
        partition(&pendulum, 2, 2).reason(1),
        Some(NonKinematicReason::Velocity { of: 0 })
    );
    // A torque solved from both states keeps the same partition.
    let torque = system(
        vec![copy(1), scaled(2, 0.5)],
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 2 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::LoadY { dst: 2, index: 1 },
            LinearOp::Binary {
                dst: 3,
                op: BinaryOp::Add,
                lhs: 1,
                rhs: 2,
            },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ]],
        projection(vec![(vec![0], vec![2])]),
    );
    assert_eq!(kinematic(&torque, 2, 3), [0]);
}

/// `der(x) = v, der(v) = -x, der(y) = b, b = 2 v`: `b` depends on the
/// velocity, so `y` would read a stale `b` after `v` moved.
#[test]
fn a_state_reading_an_algebraic_of_a_velocity_is_not_kinematic() {
    let model = system(
        vec![copy(1), scaled(0, -1.0), copy(3)],
        vec![residual(3, 1, 2.0)],
        projection(vec![(vec![0], vec![3])]),
    );
    assert_eq!(kinematic(&model, 3, 4), [0]);
    assert_eq!(
        partition(&model, 3, 4).reason(2),
        Some(NonKinematicReason::Closure { through: 1 })
    );
    // The same algebraic of the position leaves `y` kinematic.
    let position = system(
        vec![copy(1), scaled(0, -1.0), copy(3)],
        vec![residual(3, 0, 2.0)],
        projection(vec![(vec![0], vec![3])]),
    );
    assert_eq!(kinematic(&position, 3, 4), [0, 2]);
}

/// `der(x) = -x` has no velocity coordinate and nothing dynamic: every state
/// is kinematic, `R` is empty, and a partitioned step is one explicit step.
#[test]
fn a_first_order_system_reading_only_itself_is_kinematic() {
    let model = system(vec![scaled(0, -1.0)], Vec::new(), projection(Vec::new()));
    assert_eq!(kinematic(&model, 1, 1), [0]);
}

/// `der(z) = if v > 0 then 1 else -1` evaluates a relation on the velocity.
#[test]
fn a_relation_on_a_velocity_is_not_kinematic() {
    let relation = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Compare {
            dst: 2,
            op: CompareOp::Gt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::Const {
            dst: 4,
            value: -1.0,
        },
        LinearOp::Select {
            dst: 5,
            cond: 2,
            if_true: 3,
            if_false: 4,
        },
        LinearOp::StoreOutput { src: 5 },
    ];
    let model = system(
        vec![copy(1), scaled(0, -1.0), relation],
        Vec::new(),
        projection(Vec::new()),
    );
    assert_eq!(kinematic(&model, 3, 3), [0]);
    assert_eq!(
        partition(&model, 3, 3).reason(2),
        Some(NonKinematicReason::Relation)
    );
}

/// `der(y) = z1` with `z1 + z2 = x`, `z1 = z2 * z2` solved together: a
/// simultaneous block is a solve, not a value the kinematic row may reuse,
/// even though it reads only the position.
#[test]
fn a_state_reading_an_implicit_block_is_not_kinematic() {
    let sum = vec![
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadY { dst: 1, index: 4 },
        LinearOp::LoadY { dst: 2, index: 0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 3,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let square = vec![
        LinearOp::LoadY { dst: 0, index: 3 },
        LinearOp::LoadY { dst: 1, index: 4 },
        mul(2, 1, 1),
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    let model = system(
        vec![copy(1), scaled(0, -1.0), copy(3)],
        vec![sum, square],
        projection(vec![(vec![0, 1], vec![3, 4])]),
    );
    assert_eq!(kinematic(&model, 3, 5), [0]);
    assert_eq!(
        partition(&model, 3, 5).reason(2),
        Some(NonKinematicReason::SimultaneousBlock)
    );
}

/// A state whose derivative row has no program is not kinematic; the others
/// keep their partition.
#[test]
fn a_derivative_row_without_a_program_is_not_kinematic() {
    let model = system(vec![copy(1)], Vec::new(), projection(Vec::new()));
    assert_eq!(kinematic(&model, 2, 2), [0]);
    assert_eq!(
        partition(&model, 2, 2).reason(1),
        Some(NonKinematicReason::Velocity { of: 0 })
    );
    let unanalysable = system(vec![scaled(1, 2.0)], Vec::new(), projection(Vec::new()));
    assert_eq!(kinematic(&unanalysable, 2, 2), [0]);
    assert_eq!(
        partition(&unanalysable, 2, 2).reason(1),
        Some(NonKinematicReason::Unanalysable)
    );
}
