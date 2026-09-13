//! Compact input interactions derived from one checked typed call body.

use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use super::*;
use crate::affinity::Degree;
use crate::{SolveBinaryOperator, SolveUnaryOperator};

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
struct InputInteractions {
    inputs: BTreeSet<usize>,
    nonlinear: BTreeSet<(usize, usize)>,
}

impl InputInteractions {
    fn join(mut self, other: &Self) -> Self {
        self.inputs.extend(&other.inputs);
        self.nonlinear.extend(&other.nonlinear);
        self
    }

    fn product(self, other: &Self) -> Self {
        let pairs = self
            .inputs
            .iter()
            .flat_map(|&lhs| {
                other
                    .inputs
                    .iter()
                    .map(move |&rhs| (lhs.min(rhs), lhs.max(rhs)))
            })
            .collect::<Vec<_>>();
        let mut result = self.join(other);
        result.nonlinear.extend(pairs);
        result
    }

    fn nonlinear(self) -> Self {
        self.clone().product(&self)
    }

    fn substitute(&self, arguments: &[Self]) -> Option<Self> {
        let mut result = Self::default();
        for &input in &self.inputs {
            result = result.join(arguments.get(input)?);
        }
        for &(lhs, rhs) in &self.nonlinear {
            result = result.join(&arguments.get(lhs)?.clone().product(arguments.get(rhs)?));
        }
        Some(result)
    }

    fn degree(&self, inputs: &[Degree]) -> Option<Degree> {
        let mut degree = Degree::Independent;
        for &input in &self.inputs {
            degree = degree.max(*inputs.get(input)?);
        }
        for &(lhs, rhs) in &self.nonlinear {
            if *inputs.get(lhs)? != Degree::Independent && *inputs.get(rhs)? != Degree::Independent
            {
                return Some(Degree::Nonlinear);
            }
        }
        Some(degree)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(in crate::typed_program) struct Affinity(Box<[InputInteractions]>);

impl Affinity {
    pub(in crate::typed_program) fn output_degrees(
        &self,
        inputs: &[Degree],
    ) -> Option<Vec<Degree>> {
        self.0.iter().map(|output| output.degree(inputs)).collect()
    }
}

pub(in crate::typed_program) fn derive(
    body: &TypedProgram,
    input_count: usize,
    output_count: usize,
    available: SolvePureCallTableView<'_>,
) -> Option<Affinity> {
    let mut slots = vec![InputInteractions::default(); body.slots().len()];
    for (index, slot) in slots.iter_mut().take(input_count).enumerate() {
        slot.inputs.insert(index);
    }
    let mut registers = vec![InputInteractions::default(); body.register_types().len()];
    for spanned in body.operations() {
        match spanned.operation() {
            SolveOperation::Load { destination, slot } => {
                registers[destination.index()] = slots[slot.index()].clone()
            }
            SolveOperation::Store { slot, source } => {
                slots[slot.index()] = registers[source.index()].clone()
            }
            SolveOperation::Call {
                owner,
                arguments,
                destinations,
            } => {
                let callee = available.get(owner.index() as usize)?.affinity?;
                let arguments = arguments
                    .iter()
                    .map(|arg| registers[arg.index()].clone())
                    .collect::<Vec<_>>();
                for (destination, output) in destinations.iter().zip(callee.0.iter()) {
                    registers[destination.index()] = output.substitute(&arguments)?;
                }
            }
            operation => {
                let result = operation_interactions(operation, &registers)?;
                operation
                    .visit_output_registers(|output| registers[output.index()] = result.clone());
            }
        }
    }
    Some(Affinity(
        slots
            .into_iter()
            .skip(input_count)
            .take(output_count)
            .collect(),
    ))
}

fn operation_interactions(
    operation: &SolveOperation,
    registers: &[InputInteractions],
) -> Option<InputInteractions> {
    let read = |register: &SolveRegisterId| registers[register.index()].clone();
    Some(match operation {
        SolveOperation::Constant { value, .. } if finite_constant(value.kind()) => {
            InputInteractions::default()
        }
        SolveOperation::Unary {
            operator, operand, ..
        } => match operator {
            SolveUnaryOperator::Negate => read(operand),
            _ => read(operand).nonlinear(),
        },
        SolveOperation::Binary {
            operator, lhs, rhs, ..
        } => binary(*operator, read(lhs), read(rhs)),
        SolveOperation::Scale {
            aggregate, scalar, ..
        }
        | SolveOperation::BroadcastBinary {
            operator: SolveBinaryOperator::Multiply,
            aggregate,
            scalar,
            ..
        } => read(aggregate).product(&read(scalar)),
        SolveOperation::MatrixMultiply { lhs, rhs, .. }
        | SolveOperation::Cross { lhs, rhs, .. } => read(lhs).product(&read(rhs)),
        SolveOperation::LinearSolve { matrix, rhs, .. } => {
            read(matrix).nonlinear().product(&read(rhs))
        }
        SolveOperation::Transpose { operand, .. }
        | SolveOperation::ProjectElement {
            aggregate: operand, ..
        }
        | SolveOperation::ProjectSlice {
            aggregate: operand, ..
        } => read(operand),
        SolveOperation::Fill { value, .. } => read(value),
        _ => return None,
    })
}

fn binary(
    operator: SolveBinaryOperator,
    lhs: InputInteractions,
    rhs: InputInteractions,
) -> InputInteractions {
    match operator {
        SolveBinaryOperator::Add | SolveBinaryOperator::Subtract => lhs.join(&rhs),
        SolveBinaryOperator::Multiply => lhs.product(&rhs),
        SolveBinaryOperator::Divide => lhs.join(&rhs.nonlinear()),
        _ => lhs.join(&rhs).nonlinear(),
    }
}
