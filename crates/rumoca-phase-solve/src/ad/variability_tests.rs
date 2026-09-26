//! MLS variability in the tangent programs (MLS 3.7 §4.4.4, §4.5).
//!
//! A parameter-variability input is constant within one evaluation, so its
//! solver-Y tangent is zero, but it stays a load of the parameter vector and is
//! never folded into a literal: a tunable parameter changed between runs, or a
//! `fixed = false` parameter solved during initialization, reaches the next
//! evaluation. A discrete coordinate or a `pre()`/`previous()` value is a
//! parameter-vector slot, so it has zero continuous tangent; the tangent
//! programs never differentiate through an event boundary.

use super::*;

fn source_span() -> rumoca_core::ProvenanceSpan {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("variability.mo", "x*k + pre(m) + d;");
    rumoca_core::Span::from_offsets(source, 0, 17)
        .require_provenance("variability fixture")
        .unwrap()
}

/// `x*k + pre(m) + d` over solver-Y `x` and parameter-vector slots `k` (a
/// tunable parameter, index 0), `pre(m)` (index 1), and `d` (a discrete Real,
/// index 2).
fn program() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::LoadP { dst: 3, index: 1 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::LoadP { dst: 5, index: 2 },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Add,
            lhs: 4,
            rhs: 5,
        },
        LinearOp::StoreOutput { src: 6 },
    ]
}

fn jvp() -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        lower_scalar_program_block_ad(&[program()]).unwrap(),
        source_span(),
    )
    .unwrap()
}

fn tangent(block: &ScalarProgramBlock, p: &[f64], seed: &[f64]) -> f64 {
    let mut out = [0.0];
    rumoca_eval_solve::eval_scalar_program_block(block, &[3.0], p, 0.0, Some(seed), &mut out)
        .unwrap();
    out[0]
}

#[test]
fn a_parameter_stays_a_load_with_zero_solver_tangent() {
    let block = jvp();
    let program = &block.programs()[0];
    for index in 0..3 {
        assert!(
            program
                .iter()
                .any(|op| matches!(op, LinearOp::LoadP { index: read, .. } if *read == index)),
            "parameter-vector slot {index} is read, never folded into a literal"
        );
    }
    assert!(
        program
            .iter()
            .all(|op| !matches!(op, LinearOp::LoadSeed { index, .. } if *index > 0)),
        "only the solver-Y coordinate carries a seed"
    );
    // One lowered program serves two parameter values: d/dx (x*k) = k.
    assert_eq!(tangent(&block, &[2.0, 7.0, 1.0], &[1.0]), 2.0);
    assert_eq!(tangent(&block, &[5.0, 7.0, 1.0], &[1.0]), 5.0);
}

#[test]
fn discrete_and_pre_values_have_zero_continuous_tangent() {
    let block = jvp();
    // Moving `pre(m)` or `d` between events changes no tangent: with the
    // solver-Y seed at zero the tangent is exactly zero.
    for p in [[2.0, 7.0, 1.0], [2.0, -4.0, 9.0]] {
        assert_eq!(tangent(&block, &p, &[0.0]).to_bits(), 0.0_f64.to_bits());
        assert_eq!(tangent(&block, &p, &[1.0]), 2.0);
    }
}

#[test]
fn lane_programs_read_parameters_at_each_call() {
    let lanes = rumoca_ir_solve::TangentLaneProgram::replicate(&jvp().programs()[0], 2)
        .expect("the tangent widens");
    let prepared = rumoca_eval_solve::PreparedTangentLaneProgram::new(lanes);
    let outputs = prepared.program().lane_outputs();
    let seed = [1.0, 0.5];
    for k in [2.0, 5.0] {
        let mut out = vec![0.0; 2 * outputs];
        prepared
            .eval(
                &[3.0],
                &[k, 7.0, 1.0],
                0.0,
                rumoca_eval_solve::RowEvalContext {
                    seed: Some(&seed),
                    ..Default::default()
                },
                &mut out,
            )
            .unwrap();
        assert_eq!([out[0], out[outputs]], [k, 0.5 * k]);
    }
}
