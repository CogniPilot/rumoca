//! Adapter from typed registers to the one definitional evaluator of a
//! compiler-defined foreign body (SPEC_0040 DAE-C30).

use super::*;
use rumoca_core::native_body::{NativeBody, NativeBodyError, NativeScalar};

impl EvalFrame<'_, '_> {
    pub(super) fn eval_native(
        &mut self,
        body: NativeBody,
        operands: &[SolveRegisterId],
        destinations: &[SolveRegisterId],
        provenance: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let inputs = operands
            .iter()
            .map(|operand| {
                self.read(*operand, provenance)?
                    .elements
                    .iter()
                    .map(|element| match *element {
                        SolveValueKind::Integer(value) => Ok(NativeScalar::Integer(value)),
                        SolveValueKind::Real64(bits) => {
                            Ok(NativeScalar::Real(f64::from_bits(bits)))
                        }
                        _ => Err(invalid_error("native body operand", provenance)),
                    })
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let inputs = inputs.iter().map(Vec::as_slice).collect::<Vec<_>>();
        let outputs = body.evaluate(&inputs).map_err(|error| match error {
            NativeBodyError::Failure { .. } => TypedProgramEvalError::NativeBody {
                reason: error.to_string(),
                provenance,
            },
            NativeBodyError::OperandMismatch { .. } => {
                invalid_error("native body operands", provenance)
            }
        })?;
        if outputs.len() != destinations.len() {
            return Err(invalid_error("native body results", provenance));
        }
        for (destination, output) in destinations.iter().zip(outputs) {
            let elements = output
                .into_iter()
                .map(|element| match element {
                    NativeScalar::Integer(value) => SolveValueKind::Integer(value),
                    NativeScalar::Real(value) => SolveValueKind::Real64(value.to_bits()),
                })
                .collect();
            let value_type = self.destination_type(*destination, provenance)?.clone();
            let value = TypedValue::checked(value_type, elements, provenance)?;
            self.write(*destination, value, provenance)?;
        }
        Ok(())
    }
}
