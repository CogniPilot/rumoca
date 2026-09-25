//! A pure call over parameters is a constant of the whole model.
//!
//! `FixedFramePendulum.Pendulum` writes its rod-length constraint in a frame
//! rotated by the parameter angle `theta`: the frame is the record result of a
//! pure function call and the constraint reads it through a second pure call.
//! Index reduction must treat the frame as time-invariant (MLS 3.7 §12.3: a
//! pure function "always gives the same output for the same input"), so it
//! raises no derivative of the frame and never re-differentiates the frame
//! call. The reduced system then simulates the rigid pendulum.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::construct_formal_derivatives;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = include_str!("../fixtures/index_reduction/FixedFramePendulum.mo");
const MODEL: &str = "FixedFramePendulum.Pendulum";

fn compile() -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, "FixedFramePendulum.mo")
        .unwrap()
        .dae
}

/// Continuous residual owners whose expression calls a function named
/// `suffix`, counted once per owner.
fn owners_calling(view: dae::DaeView<'_>, suffix: &str) -> usize {
    view.continuous_owners()
        .filter(|owner| {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                return false;
            };
            let mut calls = false;
            dae::for_each_expression(view, equation.residual(), |_, node| {
                if let dae::ExpressionOperation::Call { function, .. } = node.operation() {
                    calls |= view
                        .function(function)
                        .is_some_and(|function| function.name().as_str().ends_with(suffix));
                }
            });
            calls
        })
        .count()
}

#[test]
fn a_parameter_frame_from_a_pure_call_is_not_differentiated() {
    let source = compile();
    let formal = construct_formal_derivatives(&source).unwrap();
    formal.inspect(|system| {
        let frame = system
            .source
            .variables()
            .filter(|(_, variable)| variable.name().as_str().starts_with("F."))
            .map(|(id, _)| id)
            .collect::<Vec<_>>();
        assert_eq!(
            frame.len(),
            2,
            "the frame record contributes `F.T` and `F.w`"
        );
        for variable in frame {
            for order in 1..=2 {
                assert!(
                    system.coordinate(variable, order).is_none(),
                    "a parameter-constant frame has no formal derivative of order {order}"
                );
            }
        }
        assert_eq!(
            owners_calling(system.source, "planarRotation"),
            1,
            "the source defines the frame once"
        );
        assert_eq!(
            owners_calling(system.view, "planarRotation"),
            1,
            "index reduction must not add a differentiated copy of the frame call"
        );
    });
}

#[test]
fn the_rotated_frame_pendulum_keeps_its_rod_length_and_energy() {
    let compiled = Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, "FixedFramePendulum.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                t_end: 1.0,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .unwrap();
        let column = |name: &str| {
            let index = result.names.iter().position(|n| n == name).unwrap();
            &result.data[index]
        };
        let (x, y, vx, vy) = (column("x"), column("y"), column("vx"), column("vy"));
        let (theta, g) = (0.4_f64, 9.81);
        let frame = [theta.cos(), -theta.sin(), theta.sin(), theta.cos()];
        for (name, value) in ["F.T[1,1]", "F.T[1,2]", "F.T[2,1]", "F.T[2,2]"]
            .into_iter()
            .zip(frame)
        {
            assert!(
                column(name)
                    .iter()
                    .all(|actual| (actual - value).abs() < 1e-12),
                "{name} must hold the fixed rotation, {solver_mode:?}"
            );
        }
        let initial_energy = g * y[0];
        for row in 0..result.times.len() {
            let length = x[row].hypot(y[row]);
            let energy = 0.5 * (vx[row] * vx[row] + vy[row] * vy[row]) + g * y[row];
            assert!(
                (length - 1.0).abs() < 1e-6,
                "rod length at row {row}, {solver_mode:?}: {length}"
            );
            assert!(
                (energy - initial_energy).abs() < 1e-3,
                "energy at row {row}, {solver_mode:?}: {energy} != {initial_energy}"
            );
        }
    }
}
