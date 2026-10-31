//! Exact coordinate forwarding from total, projection-only checked bodies.

use super::*;
use crate::{SolvePureCallOutput, SolvePureCallOutputKind};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
struct ValueProjection(SolveCallDependency);

/// Possession proves every operation in the body belongs to the total subset.
/// A missing output entry is a constant value, not an unknown body operation.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(in crate::typed_program) struct ValueProjections(Box<[Option<ValueProjection>]>);

impl ValueProjections {
    pub(in crate::typed_program) fn input_coordinate(
        &self,
        mut output_element: usize,
        inputs: &[SolveValueType],
        outputs: &[SolvePureCallOutput],
    ) -> Option<(usize, usize)> {
        for (output, projection) in outputs.iter().zip(&self.0) {
            let count = output.value_type().scalar_count() as usize;
            if output_element >= count {
                output_element -= count;
                continue;
            }
            let projection = &projection.as_ref()?.0;
            let elements = projection.input_elements(
                output.value_type(),
                output_element,
                inputs.get(projection.first_input())?,
            )?;
            let [element] = elements.as_slice() else {
                return None;
            };
            return Some((projection.first_input(), *element));
        }
        None
    }
}

pub(in crate::typed_program) fn derive(
    body: &TypedProgram,
    input_count: usize,
    outputs: &[SolvePureCallOutput],
    available: SolvePureCallTableView<'_>,
) -> Option<ValueProjections> {
    if outputs
        .iter()
        .any(|output| output.kind() != SolvePureCallOutputKind::Result)
    {
        return None;
    }
    let mut slots = vec![None; body.slots().len()];
    for (index, slot) in slots.iter_mut().take(input_count).enumerate() {
        *slot = Some(ValueProjection(SolveCallDependency::exact(
            index,
            Coordinates::identity(body.slots()[index].value_type().dimensions().len()),
        )));
    }
    let mut registers = vec![None; body.register_types().len()];
    for spanned in body.operations() {
        match spanned.operation() {
            SolveOperation::CheckAssertion { .. } => return None,
            SolveOperation::Load { destination, slot } => {
                registers[destination.index()] = slots[slot.index()].clone();
            }
            SolveOperation::Store { slot, source } => {
                slots[slot.index()] = registers[source.index()].clone();
            }
            SolveOperation::Constant { value, .. } if finite_constant(value.kind()) => {}
            SolveOperation::Call {
                owner,
                arguments,
                destinations,
                ..
            } => substitute_call(
                available.get(owner.index() as usize)?.projections?,
                arguments,
                destinations,
                &mut registers,
                spanned.provenance(),
            )?,
            operation @ (SolveOperation::Transpose { .. }
            | SolveOperation::ProjectElement { .. }
            | SolveOperation::ProjectSlice { .. }
            | SolveOperation::Fill { .. }) => {
                project_operation(body, operation, &mut registers, spanned.provenance())?;
            }
            _ => return None,
        }
    }
    Some(ValueProjections(
        slots
            .into_iter()
            .skip(input_count)
            .take(outputs.len())
            .collect(),
    ))
}

fn substitute_call(
    summary: &ValueProjections,
    arguments: &[SolveRegisterId],
    destinations: &[SolveRegisterId],
    registers: &mut [Option<ValueProjection>],
    provenance: Span,
) -> Option<()> {
    if summary.0.len() != destinations.len() {
        return None;
    }
    for (destination, projection) in destinations.iter().zip(&summary.0) {
        let result = match projection {
            Some(ValueProjection(input)) => {
                let argument = arguments.get(input.first_input())?;
                remap(
                    &registers[argument.index()],
                    input.coordinates.as_ref()?,
                    provenance,
                )?
            }
            None => None,
        };
        registers[destination.index()] = result;
    }
    Some(())
}

fn project_operation(
    body: &TypedProgram,
    operation: &SolveOperation,
    registers: &mut [Option<ValueProjection>],
    provenance: Span,
) -> Option<()> {
    let mut inputs = Vec::new();
    let mut outputs = Vec::new();
    operation.visit_input_registers(|input| inputs.push(input));
    operation.visit_output_registers(|output| outputs.push(output));
    let ([input], [output]) = (inputs.as_slice(), outputs.as_slice()) else {
        return None;
    };
    let access = operations::access(body, operation, *output, *input)?;
    registers[output.index()] = remap(&registers[input.index()], &access, provenance)?;
    Some(())
}

fn remap(
    projection: &Option<ValueProjection>,
    access: &Coordinates,
    provenance: Span,
) -> Option<Option<ValueProjection>> {
    // An exact projection is never widened. One too large to compose within
    // the budgets returns the outer `None`, which every caller propagates
    // with `?`, so the whole ValueProjections certificate is declined; an
    // output entry is never set to `None`, which would claim a constant.
    if projection
        .as_ref()
        .is_some_and(|projection| projection.0.composition_exceeds_budget(access))
    {
        return None;
    }
    projection
        .as_ref()
        .map(|projection| projection.0.remap(access, provenance).map(ValueProjection))
        .transpose()
        .ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::SourceId;

    fn span() -> Span {
        Span::from_offsets(SourceId::from_source_name("projection.alg"), 0, 1)
    }

    fn projection(guards: usize) -> Option<ValueProjection> {
        Some(ValueProjection(SolveCallDependency::exact(
            0,
            Coordinates::with_terms(0, 1, 1, 0, guards),
        )))
    }

    #[test]
    fn an_exact_projection_past_the_budget_declines_the_whole_certificate() {
        let access = Coordinates::with_terms(0, 1, 1, 0, 0);
        assert!(
            matches!(remap(&projection(32), &access, span()), Some(Some(_))),
            "within the budget the projection composes"
        );
        assert_eq!(
            remap(&projection(33), &access, span()),
            None,
            "past it the OUTER None declines the certificate"
        );
        // A constant entry stays a constant: never widened, never declined.
        assert_eq!(remap(&None, &access, span()), Some(None));
    }
}
