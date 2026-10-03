//! Checked signature for a compiler-defined foreign body (SPEC_0040 DAE-C30).

use super::*;
use crate::SolveRealFormat;
use rumoca_core::native_body::{NativeArgument, NativeElement};

impl<'program> TypedProgramBuilder<'program> {
    /// Issue one native body over operands that are exactly its catalog
    /// inputs, returning one destination per catalog output in interface
    /// order.
    pub fn native(
        &mut self,
        body: NativeBody,
        operands: &[ProgramRegister<'program>],
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let inputs = body
            .inputs()
            .map(|input| self.native_type(input, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        if inputs.len() != operands.len()
            || inputs
                .iter()
                .zip(operands)
                .any(|(expected, operand)| self.register_type(*operand, provenance) != Ok(expected))
        {
            return Err(SolveProgramConstructionError::InvalidCallInterface { provenance });
        }
        let destinations = body
            .outputs()
            .map(|output| {
                let value_type = self.native_type(output, provenance)?;
                self.issue_register(value_type, provenance)
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.push(
            SolveOperation::Native {
                body,
                operands: operands.iter().map(|operand| operand.id).collect(),
                destinations: destinations
                    .iter()
                    .map(|destination| destination.id)
                    .collect(),
            },
            provenance,
        );
        Ok(destinations)
    }

    /// The value type one catalog argument has under this program's profile.
    fn native_type(
        &self,
        argument: NativeArgument,
        provenance: Span,
    ) -> Result<SolveValueType, SolveProgramConstructionError> {
        let element = match argument.element {
            // A C `int` result must be representable in the Integer domain.
            NativeElement::Integer
                if self.arithmetic.integer_domain().contains(i32::MIN.into())
                    && self.arithmetic.integer_domain().contains(i32::MAX.into()) =>
            {
                SolveScalarType::integer(self.arithmetic)
            }
            // A C `double` is binary64; no other Real format represents it.
            NativeElement::Real if self.arithmetic.real_format() == SolveRealFormat::Binary64 => {
                SolveScalarType::real(self.arithmetic)
            }
            NativeElement::Integer | NativeElement::Real => {
                return Err(SolveProgramConstructionError::InvalidCallInterface { provenance });
            }
        };
        match argument.extent {
            None => Ok(SolveValueType::scalar(element)),
            Some(extent) => SolveValueType::tensor(element, vec![extent])
                .map_err(|_| SolveProgramConstructionError::InvalidCallInterface { provenance }),
        }
    }
}
