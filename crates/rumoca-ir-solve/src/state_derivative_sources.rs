//! One source-output and register-version walk for Cartesian and quaternion relations.
use crate::{ComputeBlock, ComputeNode, LinearOp, TensorInputKind};
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum StateSource {
    Direct(usize),
    Quaternion { binding: usize, lane: usize },
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct QuaternionBinding {
    pub(crate) q: [usize; 4],
    pub(crate) w: [usize; 3],
}
pub(crate) struct StateDerivativeSources {
    pub(crate) rows: Vec<Option<StateSource>>,
    pub(crate) quaternions: Vec<QuaternionBinding>,
}
impl StateSource {
    fn in_bounds(self, width: usize, bindings: &[QuaternionBinding]) -> bool {
        match self {
            Self::Direct(i) => i < width,
            Self::Quaternion { binding, .. } => bindings
                .get(binding)
                .is_some_and(|b| b.q.iter().chain(&b.w).all(|i| *i < width)),
        }
    }
    pub(crate) fn direct(self) -> Option<usize> {
        match self {
            Self::Direct(i) => Some(i),
            _ => None,
        }
    }
}
pub(crate) fn derive(
    block: &ComputeBlock,
    state_count: usize,
    calls: Option<&crate::SolvePureCallTable>,
) -> Option<StateDerivativeSources> {
    block.checked_source_view("state derivative sources").ok()?;
    let mut reads = vec![None; state_count];
    let mut quaternions = Vec::new();
    let mut covered = vec![false; state_count];
    let mut cursor = 0usize;
    for (node_index, node) in block.nodes.iter().enumerate() {
        let (indices, sources) = node_reads(
            node,
            node_index,
            cursor,
            state_count,
            &mut quaternions,
            calls,
        )?;
        for (index, source) in indices.into_iter().zip(sources) {
            let claimed = covered.get_mut(index)?;
            if std::mem::replace(claimed, true) {
                return None;
            }
            reads[index] = source.filter(|source| source.in_bounds(state_count, &quaternions));
        }
        cursor = crate::tensor::advance_compute_output_cursor(
            "Cartesian kinematics",
            node_index,
            node,
            cursor,
        )
        .ok()?;
    }
    covered
        .iter()
        .all(|&value| value)
        .then_some(StateDerivativeSources {
            rows: reads,
            quaternions,
        })
}
fn node_reads(
    node: &ComputeNode,
    node_index: usize,
    cursor: usize,
    state_count: usize,
    quaternions: &mut Vec<QuaternionBinding>,
    calls: Option<&crate::SolvePureCallTable>,
) -> Option<(Vec<usize>, Vec<Option<StateSource>>)> {
    match node {
        ComputeNode::Map {
            domain, output_map, ..
        } => {
            if let Some((indices, sources)) =
                crate::cartesian_pairs::identity_map_reads(node, state_count)
            {
                return Some((
                    indices,
                    sources
                        .into_iter()
                        .map(|i| Some(StateSource::Direct(i)))
                        .collect(),
                ));
            }
            // Opaque explicit derivative rows retain their original execution;
            // no kinematic identity is inferred from constants or arithmetic.
            let indices = output_map.output_indices(domain).ok()?;
            let sources = vec![None; indices.len()];
            Some((indices, sources))
        }
        ComputeNode::LinSolve { n, .. } => {
            let end = cursor.checked_add(*n)?;
            if end > state_count {
                return None;
            }
            Some(((cursor..end).collect(), vec![None; *n]))
        }
        ComputeNode::ScalarPrograms(rows) => {
            let indices = rows
                .compute_block_output_indices("Cartesian kinematics", node_index, cursor)
                .ok()?;
            let outputs = rows.programs().iter().enumerate().try_fold(
                Vec::new(),
                |mut outputs, (index, program)| {
                    outputs.extend(program_sources(
                        program,
                        rows.program_register_count(index)?,
                        quaternions,
                        calls,
                    )?);
                    Some(outputs)
                },
            )?;
            (indices.len() == outputs.len()).then_some((indices, outputs))
        }
        _ => None,
    }
}

fn program_sources(
    program: &[LinearOp],
    register_count: usize,
    quaternions: &mut Vec<QuaternionBinding>,
    calls: Option<&crate::SolvePureCallTable>,
) -> Option<Vec<Option<StateSource>>> {
    let mut registers = vec![None; register_count];
    let mut outputs = Vec::new();
    for op in program {
        match *op {
            LinearOp::LoadY { dst, index } => {
                registers[dst as usize] = Some(StateSource::Direct(index))
            }
            LinearOp::Move { dst, src } => registers[dst as usize] = registers[src as usize],
            LinearOp::TensorLoad {
                dst_start,
                input: TensorInputKind::Y,
                input_start,
                count,
                seed_start: None,
                lanes: 1,
            } => {
                for offset in 0..count {
                    registers[dst_start as usize + offset] =
                        Some(StateSource::Direct(input_start.checked_add(offset)?));
                }
            }
            LinearOp::StoreOutput { src } => outputs.push(registers[src as usize]),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for index in 0..count {
                    outputs.push(registers[start as usize + index * stride]);
                }
            }
            LinearOp::PureCall {
                dst_start,
                ref input_starts,
                ref site,
            } if site.body_quaternion_kinematics().is_some()
                && calls.is_some_and(|calls| calls.matches_site(site)) =>
            {
                let binding = quaternion_inputs(&registers, input_starts).map(|(q, w)| {
                    let index = quaternions.len();
                    quaternions.push(QuaternionBinding { q, w });
                    index
                });
                for lane in 0..4 {
                    registers[dst_start as usize + lane] =
                        binding.map(|binding| StateSource::Quaternion { binding, lane });
                }
            }
            _ => {
                if let Some(start) = op.dst_register() {
                    let start = start as usize;
                    registers[start..start.checked_add(op.dst_register_count())?].fill(None);
                } else {
                    outputs.extend(std::iter::repeat_n(
                        None,
                        crate::ScalarProgramBlock::checked_program_output_count(
                            std::slice::from_ref(op),
                        )?,
                    ));
                }
            }
        }
    }
    Some(outputs)
}

fn quaternion_inputs(
    registers: &[Option<StateSource>],
    inputs: &[crate::Reg],
) -> Option<([usize; 4], [usize; 3])> {
    let [q_start, w_start] = inputs else {
        return None;
    };
    let mut q = [0; 4];
    let mut w = [0; 3];
    for (offset, value) in q.iter_mut().enumerate() {
        *value = registers
            .get(*q_start as usize + offset)?
            .as_ref()?
            .direct()?;
    }
    for (offset, value) in w.iter_mut().enumerate() {
        *value = registers
            .get(*w_start as usize + offset)?
            .as_ref()?
            .direct()?;
    }
    Some((q, w))
}
