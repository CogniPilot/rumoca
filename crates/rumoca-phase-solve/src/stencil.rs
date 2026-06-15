//! Source-proven affine stencil preservation for Solve IR.
//!
//! Stencil nodes are emitted only when rows come from preserved DAE
//! `for_equation` metadata and the lowered loads are affine across that source
//! iteration domain. This keeps stencil recognition tied to source structure
//! instead of recovering arbitrary scalar row runs after the fact.

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[derive(Debug, Clone)]
pub(crate) struct SourceScalarProgram {
    pub(crate) ops: Vec<solve::LinearOp>,
    pub(crate) source_equation_index: Option<usize>,
}

pub(crate) fn push_source_structured_rows(
    nodes: &mut Vec<solve::ComputeNode>,
    rows: &mut Vec<SourceScalarProgram>,
    for_equations: &[dae::ForEquation],
) {
    let mut residual_rows = Vec::new();
    let mut row = 0;
    while row < rows.len() {
        if let Some(stencil) = source_stencil_at(rows, row, for_equations) {
            flush_residual_rows(nodes, &mut residual_rows);
            row += stencil.count();
            nodes.push(stencil);
        } else {
            residual_rows.push(rows[row].ops.clone());
            row += 1;
        }
    }
    flush_residual_rows(nodes, &mut residual_rows);
    rows.clear();
}

fn flush_residual_rows(nodes: &mut Vec<solve::ComputeNode>, rows: &mut Vec<Vec<solve::LinearOp>>) {
    if rows.is_empty() {
        return;
    }
    nodes.push(solve::ComputeNode::ScalarPrograms(
        solve::ScalarProgramBlock::new(std::mem::take(rows)),
    ));
}

fn source_stencil_at(
    rows: &[SourceScalarProgram],
    start: usize,
    for_equations: &[dae::ForEquation],
) -> Option<solve::ComputeNode> {
    let source_index = rows.get(start)?.source_equation_index?;
    let for_equation = for_equations
        .iter()
        .find(|for_equation| source_belongs_to_for_equation(source_index, for_equation))?;
    let equation_count = common_iteration_equation_count(for_equation)?;
    let relative_source = source_index - for_equation.first_equation_index;
    let iteration_start = relative_source / equation_count;
    let equation_position = relative_source % equation_count;
    let max_count = for_equation.iterations.len().checked_sub(iteration_start)?;
    if max_count < 2 {
        return None;
    }
    let count = max_source_affine_count(
        rows,
        start,
        for_equation,
        iteration_start,
        equation_count,
        equation_position,
        max_count,
    )?;
    let row_ops = row_ops(rows, start, count);
    let strides = affine_strides(&row_ops)?;
    Some(solve::ComputeNode::AffineStencil {
        count,
        domain: solve::AffineStencilDomain {
            index_names: for_equation.index_names.clone(),
            iterations: for_equation
                .iterations
                .iter()
                .skip(iteration_start)
                .take(count)
                .map(|iteration| solve::AffineStencilIteration {
                    index_values: iteration.index_values.clone(),
                })
                .collect(),
        },
        base_ops: rows[start].ops.clone(),
        load_strides: strides.load_strides,
        const_strides: strides.const_strides,
        metadata: solve::TensorNodeMetadata::default(),
        span: for_equation.span,
    })
}

fn max_source_affine_count(
    rows: &[SourceScalarProgram],
    start: usize,
    for_equation: &dae::ForEquation,
    iteration_start: usize,
    equation_count: usize,
    equation_position: usize,
    max_count: usize,
) -> Option<usize> {
    let max_count = max_count.min(rows.len().saturating_sub(start));
    for count in (2..=max_count).rev() {
        if !source_indices_match(
            rows,
            start,
            for_equation,
            iteration_start,
            equation_count,
            equation_position,
            count,
        ) {
            continue;
        }
        if affine_strides(&row_ops(rows, start, count)).is_some() {
            return Some(count);
        }
    }
    None
}

fn source_indices_match(
    rows: &[SourceScalarProgram],
    start: usize,
    for_equation: &dae::ForEquation,
    iteration_start: usize,
    equation_count: usize,
    equation_position: usize,
    count: usize,
) -> bool {
    (0..count).all(|offset| {
        let expected = for_equation.first_equation_index
            + (iteration_start + offset) * equation_count
            + equation_position;
        rows[start + offset].source_equation_index == Some(expected)
    })
}

fn row_ops(rows: &[SourceScalarProgram], start: usize, count: usize) -> Vec<&[solve::LinearOp]> {
    rows[start..start + count]
        .iter()
        .map(|row| row.ops.as_slice())
        .collect()
}

fn source_belongs_to_for_equation(source_index: usize, for_equation: &dae::ForEquation) -> bool {
    let Some(equation_count) = common_iteration_equation_count(for_equation) else {
        return false;
    };
    let start = for_equation.first_equation_index;
    let end = start + equation_count * for_equation.iterations.len();
    (start..end).contains(&source_index)
}

fn common_iteration_equation_count(for_equation: &dae::ForEquation) -> Option<usize> {
    let first = for_equation.iterations.first()?.equation_count;
    if first == 0 {
        return None;
    }
    for_equation
        .iterations
        .iter()
        .all(|iteration| iteration.equation_count == first)
        .then_some(first)
}

#[derive(Clone, Debug, PartialEq)]
struct AffineDelta {
    load_strides: Vec<usize>,
    const_strides: Vec<f64>,
}

#[derive(Clone, Debug, PartialEq)]
struct AffineStrides {
    load_strides: Vec<solve::AffineStencilLoadStride>,
    const_strides: Vec<solve::AffineStencilConstStride>,
}

fn affine_strides(rows: &[&[solve::LinearOp]]) -> Option<AffineStrides> {
    let base = *rows.first()?;
    let mut expected: Option<AffineDelta> = None;
    for (offset, row) in rows.iter().enumerate().skip(1) {
        let deltas = affine_deltas(base, row, offset)?;
        match &expected {
            None => expected = Some(deltas),
            Some(expected) if *expected == deltas => {}
            Some(_) => return None,
        }
    }
    let strides = expected?;
    let mut load_index = 0;
    let mut const_index = 0;
    let mut load_strides = Vec::new();
    let mut const_strides = Vec::new();
    for (op_position, op) in base.iter().enumerate() {
        match op {
            solve::LinearOp::LoadY { .. } | solve::LinearOp::LoadP { .. } => {
                load_strides.push(solve::AffineStencilLoadStride {
                    op_position,
                    stride: strides.load_strides[load_index],
                });
                load_index += 1;
            }
            solve::LinearOp::Const { .. } => {
                let stride = strides.const_strides[const_index];
                if stride != 0.0 {
                    const_strides.push(solve::AffineStencilConstStride {
                        op_position,
                        stride,
                    });
                }
                const_index += 1;
            }
            _ => {}
        }
    }
    Some(AffineStrides {
        load_strides,
        const_strides,
    })
}

fn affine_deltas(
    base: &[solve::LinearOp],
    candidate: &[solve::LinearOp],
    row_offset: usize,
) -> Option<AffineDelta> {
    if base.len() != candidate.len() {
        return None;
    }
    let mut load_strides = Vec::new();
    let mut const_strides = Vec::new();
    for (base_op, candidate_op) in base.iter().zip(candidate) {
        match (base_op, candidate_op) {
            (
                solve::LinearOp::Const {
                    dst: base_dst,
                    value: base_value,
                },
                solve::LinearOp::Const {
                    dst: candidate_dst,
                    value: candidate_value,
                },
            ) => {
                if base_dst != candidate_dst {
                    return None;
                }
                const_strides.push((candidate_value - base_value) / row_offset as f64);
            }
            (
                solve::LinearOp::LoadY {
                    dst: base_dst,
                    index: base_index,
                },
                solve::LinearOp::LoadY {
                    dst: candidate_dst,
                    index: candidate_index,
                },
            )
            | (
                solve::LinearOp::LoadP {
                    dst: base_dst,
                    index: base_index,
                },
                solve::LinearOp::LoadP {
                    dst: candidate_dst,
                    index: candidate_index,
                },
            ) => {
                if base_dst != candidate_dst || candidate_index < base_index {
                    return None;
                }
                let delta = candidate_index - base_index;
                if delta % row_offset != 0 {
                    return None;
                }
                load_strides.push(delta / row_offset);
            }
            (base_op, candidate_op) if base_op == candidate_op => {
                if matches!(base_op, solve::LinearOp::Const { .. }) {
                    const_strides.push(0.0);
                }
            }
            _ => return None,
        }
    }
    Some(AffineDelta {
        load_strides,
        const_strides,
    })
}

trait StencilCount {
    fn count(&self) -> usize;
}

impl StencilCount for solve::ComputeNode {
    fn count(&self) -> usize {
        match self {
            solve::ComputeNode::AffineStencil { count, .. } => *count,
            _ => unreachable!("source_stencil_at only returns AffineStencil nodes"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn decay_row(y: usize, p: usize, source_equation_index: usize) -> SourceScalarProgram {
        SourceScalarProgram {
            source_equation_index: Some(source_equation_index),
            ops: vec![
                solve::LinearOp::LoadP { dst: 0, index: p },
                solve::LinearOp::LoadY { dst: 1, index: y },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::Unary {
                    dst: 3,
                    op: solve::UnaryOp::Neg,
                    arg: 2,
                },
                solve::LinearOp::StoreOutput { src: 3 },
            ],
        }
    }

    fn for_equation(start: usize, count: usize) -> dae::ForEquation {
        dae::ForEquation {
            index_names: vec!["i".to_string()],
            first_equation_index: start,
            iterations: (0..count)
                .map(|offset| dae::ForEquationIteration {
                    index_values: vec![offset as i64 + 1],
                    equation_count: 1,
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
            origin: "test".to_string(),
        }
    }

    fn multi_equation_for_equation(
        start: usize,
        count: usize,
        equation_count: usize,
    ) -> dae::ForEquation {
        dae::ForEquation {
            index_names: vec!["i".to_string()],
            first_equation_index: start,
            iterations: (0..count)
                .map(|offset| dae::ForEquationIteration {
                    index_values: vec![offset as i64 + 1],
                    equation_count,
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
            origin: "test".to_string(),
        }
    }

    #[test]
    fn preserves_source_for_equation_as_stencil_node() {
        let mut rows = (0..4)
            .map(|offset| decay_row(offset, 0, 10 + offset))
            .collect();
        let mut nodes = Vec::new();
        push_source_structured_rows(&mut nodes, &mut rows, &[for_equation(10, 4)]);
        assert_eq!(nodes.len(), 1);
        let solve::ComputeNode::AffineStencil {
            count,
            domain,
            load_strides,
            ..
        } = &nodes[0]
        else {
            panic!("expected AffineStencil");
        };
        assert_eq!(*count, 4);
        assert_eq!(domain.index_names, vec!["i"]);
        assert_eq!(domain.iterations.len(), 4);
        assert_eq!(
            load_strides,
            &vec![
                solve::AffineStencilLoadStride {
                    op_position: 0,
                    stride: 0,
                },
                solve::AffineStencilLoadStride {
                    op_position: 1,
                    stride: 1,
                },
            ]
        );
    }

    #[test]
    fn leaves_matching_rows_scalar_without_source_for_equation() {
        let mut rows = (0..4)
            .map(|offset| decay_row(offset, 0, 10 + offset))
            .collect();
        let mut nodes = Vec::new();
        push_source_structured_rows(&mut nodes, &mut rows, &[]);
        assert!(matches!(
            &nodes[..],
            [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 4
        ));
    }

    #[test]
    fn leaves_non_affine_source_for_equation_scalar() {
        let mut second = decay_row(1, 0, 11);
        second
            .ops
            .insert(0, solve::LinearOp::Const { dst: 9, value: 1.0 });
        let mut fourth = decay_row(9, 0, 13);
        fourth
            .ops
            .insert(0, solve::LinearOp::Const { dst: 9, value: 2.0 });
        let mut rows = vec![decay_row(0, 0, 10), second, decay_row(4, 0, 12), fourth];
        let mut nodes = Vec::new();
        push_source_structured_rows(&mut nodes, &mut rows, &[for_equation(10, 4)]);
        assert!(matches!(
            &nodes[..],
            [solve::ComputeNode::ScalarPrograms(block)] if block.programs.len() == 4
        ));
    }

    #[test]
    fn preserves_fixed_equation_position_in_multi_equation_loop() {
        let mut rows = (0..4)
            .map(|offset| decay_row(offset, 0, 10 + offset * 3 + 1))
            .collect();
        let mut nodes = Vec::new();
        push_source_structured_rows(
            &mut nodes,
            &mut rows,
            &[multi_equation_for_equation(10, 4, 3)],
        );
        assert!(matches!(
            &nodes[..],
            [solve::ComputeNode::AffineStencil { count: 4, .. }]
        ));
    }

    #[test]
    fn preserves_source_subdomain_after_non_affine_boundary_iteration() {
        let mut boundary = decay_row(0, 0, 10);
        boundary
            .ops
            .insert(0, solve::LinearOp::Const { dst: 9, value: 1.0 });
        let mut rows = vec![boundary];
        rows.extend((1..4).map(|offset| decay_row(offset, 0, 10 + offset)));
        let mut nodes = Vec::new();
        push_source_structured_rows(&mut nodes, &mut rows, &[for_equation(10, 4)]);
        assert!(matches!(
            &nodes[..],
            [
                solve::ComputeNode::ScalarPrograms(block),
                solve::ComputeNode::AffineStencil { count: 3, domain, .. }
            ] if block.programs.len() == 1
                && domain.iterations.first().map(|it| it.index_values.as_slice()) == Some(&[2])
        ));
    }
}
