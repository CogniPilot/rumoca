//! Chained isolation programs answer consecutive causal steps of one row with
//! the values of the per-step isolators, and decline every chain a
//! sequential consumer could not execute that way.

use super::*;

fn prepare(program: Vec<LinearOp>) -> PreparedScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("isolation_chain.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(
            vec![program],
            span.require_provenance("isolation chain").unwrap(),
        )
        .unwrap(),
    )
    .unwrap()
}

const Y: [f64; 2] = [11.0, -7.0];
const P: [f64; 1] = [5.0];

fn eval_program(program: Vec<LinearOp>, outputs: usize) -> Vec<f64> {
    let mut out = vec![f64::NAN; outputs];
    prepare(program)
        .eval_with_context(&Y, &P, 0.0, RowEvalContext::default(), &mut out)
        .unwrap();
    out
}

fn sub(dst: u32, lhs: u32, rhs: u32) -> LinearOp {
    LinearOp::Binary {
        dst,
        op: BinaryOp::Sub,
        lhs,
        rhs,
    }
}

/// Outputs `y0 - p0` and `y1 - y0`: the second output reads the first target.
fn dependent_row() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        sub(2, 0, 1),
        LinearOp::StoreOutput { src: 2 },
        LinearOp::LoadY { dst: 3, index: 1 },
        sub(4, 3, 0),
        LinearOp::StoreOutput { src: 4 },
    ]
}

/// Outputs `y0 - p0` and `y1 - 3`, each expression evaluated before any
/// target is loaded, so no prefix reads the other output's target.
fn independent_row() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::LoadY { dst: 2, index: 0 },
        sub(3, 2, 0),
        LinearOp::StoreOutput { src: 3 },
        LinearOp::LoadY { dst: 4, index: 1 },
        sub(5, 4, 1),
        LinearOp::StoreOutput { src: 5 },
    ]
}

fn single(prepared: &PreparedScalarProgramBlock, output: usize, target: usize) -> f64 {
    let TargetIsolationProgram::Isolator(program) =
        prepared.target_isolation_output_program(0, output, target)
    else {
        panic!("output {output} isolates target {target}");
    };
    eval_program(program, 1)[0]
}

#[test]
fn an_independent_chain_stores_each_single_isolator_value_in_order() {
    let prepared = prepare(independent_row());
    let pairs = [(0, 0), (1, 1)];
    let chain = prepared
        .target_isolation_chain_program(0, &pairs)
        .expect("no prefix reads an earlier target");
    let stores = chain
        .iter()
        .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
        .count();
    assert_eq!(stores, pairs.len(), "one stored value per step");
    let values = eval_program(chain, pairs.len());
    for (slot, &(output, target)) in pairs.iter().enumerate() {
        assert_eq!(
            values[slot].to_bits(),
            single(&prepared, output, target).to_bits(),
            "step {slot} equals its single isolator"
        );
    }
}

#[test]
fn a_prefix_reading_an_earlier_target_declines_the_chain() {
    let prepared = prepare(dependent_row());
    assert!(
        matches!(
            prepared.target_isolation_output_program(0, 1, 1),
            TargetIsolationProgram::Isolator(_)
        ),
        "each step alone is isolable"
    );
    assert_eq!(
        prepared.target_isolation_chain_program(0, &[(0, 0), (1, 1)]),
        None,
        "the second prefix reads y0, which the first step writes"
    );
}

#[test]
fn a_decreasing_prefix_declines_the_chain() {
    let prepared = prepare(independent_row());
    let first = prepared.target_isolation_prefix_len(0, 1, 1).unwrap();
    let second = prepared.target_isolation_prefix_len(0, 0, 0).unwrap();
    assert!(second < first, "the fixture orders the prefixes");
    assert_eq!(
        prepared.target_isolation_chain_program(0, &[(1, 1), (0, 0)]),
        None,
        "a later step would need operations before the evaluated prefix"
    );
}

#[test]
fn a_pair_without_a_shape_declines_the_chain() {
    let prepared = prepare(independent_row());
    assert_eq!(
        prepared.target_isolation_chain_program(0, &[(0, 0), (1, 0)]),
        None
    );
}
