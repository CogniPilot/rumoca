//! Decoding is private to execution of the adapter's own immutable artifact.
use super::*;
use crate::typed_call::{TypedCallFaultKind, observations::ObservationSource};

#[cfg(any(
    test,
    all(target_arch = "wasm32", target_os = "unknown"),
    all(feature = "host-engine", not(target_arch = "wasm32"))
))]
impl<'model> CompiledCheckedAssertionWasm<'model> {
    pub(super) fn decode_execution<'proof>(
        &'proof self,
        input: &[u64],
        output: &[u64],
        scratch: &[u64],
        status: u32,
    ) -> Result<CheckedAssertionWasmOutcome<'model, 'proof>, WasmCompileError> {
        Decoder {
            invocation: &self.invocation,
            faults: self.artifact.faults(),
            carrier: self.carrier.as_ref(),
        }
        .decode_execution(input, output, scratch, status)
    }
}

pub(crate) struct Decoder<'model, 'proof> {
    pub(crate) invocation: &'proof solve::CheckedAssertionInvocation<'model>,
    pub(crate) faults: &'proof [TypedCallFault],
    pub(crate) carrier: Option<&'proof ObservationPlan>,
}

impl<'model, 'proof> Decoder<'model, 'proof> {
    pub(crate) fn decode_execution(
        &self,
        input: &[u64],
        output: &[u64],
        scratch: &[u64],
        status: u32,
    ) -> Result<CheckedAssertionWasmOutcome<'model, 'proof>, WasmCompileError> {
        let fault = if status == 0 {
            None
        } else {
            Some(
                self.faults
                    .iter()
                    .find(|fault| fault.status == status)
                    .ok_or_else(invalid_carrier)?
                    .clone(),
            )
        };
        if fault
            .as_ref()
            .is_some_and(|fault| fault.kind == TypedCallFaultKind::InvalidBuffer)
        {
            // An unsuccessful guard grants no authority to inspect scratch.
            return Ok(CheckedAssertionWasmOutcome::Fault {
                fault: fault.unwrap(),
                observations: Vec::new(),
            });
        }
        let observations = self.decode_records(input, scratch)?;
        match fault {
            Some(fault) if fault.kind == TypedCallFaultKind::AssertionFailed => {
                self.check_stop(&fault, &observations)?;
                Ok(CheckedAssertionWasmOutcome::Stopped { observations })
            }
            Some(fault) => Ok(CheckedAssertionWasmOutcome::Fault {
                fault,
                observations,
            }),
            None => {
                let mut rest = output;
                let values = self
                    .invocation
                    .owner()
                    .outputs()
                    .iter()
                    .map(|output| take_cells(&mut rest, output.value_type()))
                    .collect::<Result<Vec<_>, _>>()?;
                if !rest.is_empty() {
                    return Err(invalid_carrier());
                }
                Ok(CheckedAssertionWasmOutcome::Complete {
                    values,
                    observations,
                })
            }
        }
    }

    fn decode_records(
        &self,
        input: &[u64],
        scratch: &[u64],
    ) -> Result<Vec<NativeAssertionObservation<'model, 'proof>>, WasmCompileError> {
        let mut observations = Vec::new();
        let Some(carrier) = &self.carrier else {
            return Ok(observations);
        };
        let base = (carrier.range.offset / 8) as usize;
        let initialized = usize::try_from(*scratch.get(base).ok_or_else(invalid_carrier)?)
            .map_err(|_| invalid_carrier())?;
        if initialized > carrier.capacity as usize {
            return Err(invalid_carrier());
        }
        for index in 0..initialized {
            let offset = base + 1 + index * (carrier.stride / 8) as usize;
            let tag = usize::try_from(*scratch.get(offset).ok_or_else(invalid_carrier)?)
                .map_err(|_| invalid_carrier())?;
            let source = carrier.sources.get(tag).ok_or_else(invalid_carrier)?;
            let payload = scratch
                .get(offset + 1..offset + 1 + (source.payload_bytes / 8) as usize)
                .ok_or_else(invalid_carrier)?;
            for action in self
                .invocation
                .projections()
                .iter()
                .filter(|action| action.predicate_output() == source.predicate)
            {
                observations.push(self.observation(source, action, input, payload)?);
            }
        }
        Ok(observations)
    }

    fn check_stop(
        &self,
        fault: &TypedCallFault,
        observations: &[NativeAssertionObservation<'model, '_>],
    ) -> Result<(), WasmCompileError> {
        let last = observations.last().ok_or_else(invalid_carrier)?;
        let origin = last.frames.last().ok_or_else(invalid_carrier)?;
        if origin.owner.id() != fault.owner
            || origin.operation != fault.operation.ok_or_else(invalid_carrier)?
            || last.provenance != fault.provenance
            || !std::ptr::eq(
                origin.program,
                region_program(origin.owner.body(), &fault.region_path)?,
            )
            || origin
                .owner
                .outputs()
                .get(last.origin_predicate)
                .and_then(solve::SolvePureCallOutput::assertion_level)
                != Some(solve::SolveAssertionLevel::Error)
        {
            return Err(invalid_carrier());
        }
        Ok(())
    }

    fn observation(
        &self,
        source: &ObservationSource,
        action: &'proof solve::CheckedAssertionAction<'model>,
        input: &[u64],
        mut payload: &[u64],
    ) -> Result<NativeAssertionObservation<'model, 'proof>, WasmCompileError> {
        let mut frames = Vec::new();
        for (index, frame) in source.frames.iter().enumerate() {
            let owner = self
                .invocation
                .table()
                .owner(frame.owner)
                .ok_or_else(invalid_carrier)?;
            let program = region_program(owner.body(), &frame.regions)?;
            let mut root_input = input;
            let arguments = if index == 0 {
                owner
                    .inputs()
                    .iter()
                    .map(|ty| take_cells(&mut root_input, ty))
                    .collect::<Result<Vec<_>, _>>()?
            } else {
                frame
                    .arguments
                    .iter()
                    .map(|ty| take_cells(&mut payload, ty))
                    .collect::<Result<Vec<_>, _>>()?
            };
            let mut domain_points = Vec::new();
            for domain in &frame.loops {
                let (first, rest) = payload.split_first().ok_or_else(invalid_carrier)?;
                payload = rest;
                let point = usize::try_from(*first).map_err(|_| invalid_carrier())?;
                domain_points.push(
                    domain
                        .index_tuple_at(point)
                        .map_err(|_| invalid_carrier())?
                        .ok_or_else(invalid_carrier)?,
                );
            }
            frames.push(NativeAssertionFrame {
                owner,
                program,
                operation: frame.operation,
                region_path: frame.regions.clone(),
                domain_points,
                arguments,
            });
        }
        let captures = source
            .captures
            .iter()
            .zip(&source.projected_captures)
            .map(|((_, ty), output)| Ok((*output, take_cells(&mut payload, ty)?)))
            .collect::<Result<Vec<_>, WasmCompileError>>()?;
        if !payload.is_empty() {
            return Err(invalid_carrier());
        }
        Ok(NativeAssertionObservation {
            action,
            provenance: source.provenance,
            origin_predicate: source.origin_predicate,
            frames,
            captures,
        })
    }
}

fn take_cells(
    payload: &mut &[u64],
    ty: &solve::SolveValueType,
) -> Result<NativeAssertionCells, WasmCompileError> {
    let count = (crate::typed_call::layout::bytes(ty).map_err(compile_error)? / 8) as usize;
    let cells = payload.get(..count).ok_or_else(invalid_carrier)?;
    let value = NativeAssertionCells {
        value_type: ty.clone(),
        bits: cells.to_vec(),
    };
    *payload = &payload[count..];
    Ok(value)
}

fn region_program<'model>(
    mut program: &'model solve::TypedProgram,
    regions: &[(usize, usize)],
) -> Result<&'model solve::TypedProgram, WasmCompileError> {
    for (operation, arm) in regions {
        program = match program
            .operations()
            .get(*operation)
            .ok_or_else(invalid_carrier)?
            .operation()
        {
            solve::SolveOperation::AssignedConditional { program } => match arm {
                0 => program.if_true().body(),
                1 => program.if_false().body(),
                _ => return Err(invalid_carrier()),
            },
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => match arm {
                0 => if_true.body(),
                1 => if_false.body(),
                _ => return Err(invalid_carrier()),
            },
            solve::SolveOperation::Fold {
                transition,
                continuation,
                ..
            } => match arm {
                0 => transition.body(),
                1 => continuation.as_deref().ok_or_else(invalid_carrier)?.body(),
                _ => return Err(invalid_carrier()),
            },
            _ => return Err(invalid_carrier()),
        };
    }
    Ok(program)
}

fn invalid_carrier() -> WasmCompileError {
    WasmCompileError::Backend("issued native observation carrier is inconsistent".into())
}
