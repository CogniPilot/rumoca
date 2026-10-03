//! Final native adapter for a compiler-defined foreign body (SPEC_0040
//! DAE-C30): the operands are packed into one cell tape and the host calls the
//! body's one definitional evaluator, so this backend computes exactly the
//! value every other evaluator computes.

use super::*;
use rumoca_core::native_body::{
    NativeArgument, NativeBody, NativeBodyError, NativeElement, NativeScalar,
};

const NATIVE_HOST_SYMBOL: &str = "rumoca_host_native";

impl ProgramLowerer<'_, '_> {
    pub(super) fn lower_native(
        &mut self,
        body: NativeBody,
        operands: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let input = self.packed_tape(operands, "native body input")?;
        let output_cells = self.register_cell_count(destinations, "native body output")?;
        let output = create_tape(self.builder, self.pointer_type, output_cells)?;
        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(types::I64));
        signature.params.push(AbiParam::new(self.pointer_type));
        signature.params.push(AbiParam::new(self.pointer_type));
        signature.returns.push(AbiParam::new(types::I8));
        let function = self
            .module
            .declare_function(NATIVE_HOST_SYMBOL, Linkage::Import, &signature)
            .map_err(to_backend_err)?;
        let local = declare_far_call_in_func(self.module, function, self.builder.func);
        let code = self
            .builder
            .ins()
            .iconst(types::I64, native_body_code(body));
        let call = self.builder.ins().call(local, &[code, input, output]);
        let status = self.builder.inst_results(call)[0];
        status::propagate(self.builder, status);
        self.unpack_registers(output, destinations)
    }
}

fn native_body_code(body: NativeBody) -> i64 {
    NativeBody::ALL
        .iter()
        .position(|row| *row == body)
        .expect("every native body is a catalog row") as i64
}

/// Host entry for [`ProgramLowerer::lower_native`]. The tapes hold one 64-bit
/// cell per element of each catalog input and output, in interface order:
/// Integer cells are two's-complement `i64`, Real cells binary64 bits.
///
/// # Safety
///
/// `input` and `output` address tapes laid out by the checked operation's
/// catalog signature, which `lower_native` sizes from the typed registers.
pub(in crate::emit) unsafe extern "C" fn rumoca_host_native(
    code: i64,
    input: *const u64,
    output: *mut u64,
) -> u8 {
    let Some(body) = usize::try_from(code)
        .ok()
        .and_then(|code| NativeBody::ALL.get(code).copied())
    else {
        return status::NATIVE_BODY_FAILURE;
    };
    let mut cell = 0usize;
    let inputs = body
        .inputs()
        .map(|argument| {
            (0..argument_cells(argument))
                .map(|_| {
                    // SAFETY: the caller's input tape holds every input cell.
                    let bits = unsafe { input.add(cell).read() };
                    cell += 1;
                    match argument.element {
                        NativeElement::Integer => NativeScalar::Integer(bits as i64),
                        NativeElement::Real => NativeScalar::Real(f64::from_bits(bits)),
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let inputs = inputs.iter().map(Vec::as_slice).collect::<Vec<_>>();
    let outputs = match body.evaluate(&inputs) {
        Ok(outputs) => outputs,
        Err(NativeBodyError::Failure { .. }) => return status::NATIVE_BODY_FOREIGN_ERROR,
        Err(NativeBodyError::OperandMismatch { .. }) => return status::NATIVE_BODY_FAILURE,
    };
    for (cell, element) in outputs.into_iter().flatten().enumerate() {
        let bits = match element {
            NativeScalar::Integer(value) => value as u64,
            NativeScalar::Real(value) => value.to_bits(),
        };
        // SAFETY: the caller's output tape holds every output cell.
        unsafe { output.add(cell).write(bits) };
    }
    0
}

fn argument_cells(argument: NativeArgument) -> usize {
    argument.extent.map_or(1, |extent| extent as usize)
}

#[cfg(test)]
mod tests;
