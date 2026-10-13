//! Resource-local transport storage, without execution or FMI publication authority.

use std::{fmt, rc::Rc};

use rumoca_ir_solve::NativeOutputLane;

mod presence;
mod source_initial;
mod storage;
use presence::InputPresence;
use storage::NativeStorage;

use super::{CompiledRetainedNativeProgramWasm, PreparedNativeWasmCode, RetainedNativeWasmHost};
use crate::WasmCompileError;
use rumoca_ir_solve::RetainedAssertionRoot;

/// Exact host input bits; no floating-point conversion initializes these cells.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeTypedInput {
    Integer(i64),
    Boolean(bool),
}

/// One homogeneous caller buffer; its extent is runtime payload, not metadata.
#[derive(Clone, Copy)]
pub enum NativeTypedInputBatch<'values> {
    Integer(&'values [i64]),
    Boolean(&'values [bool]),
    IntegerRepeat { value: i64, count: usize },
    BooleanRepeat { value: bool, count: usize },
}

impl NativeTypedInputBatch<'_> {
    fn len(self) -> usize {
        match self {
            Self::Integer(values) => values.len(),
            Self::Boolean(values) => values.len(),
            Self::IntegerRepeat { count, .. } | Self::BooleanRepeat { count, .. } => count,
        }
    }

    fn lane(self) -> NativeOutputLane {
        match self {
            Self::Integer(_) | Self::IntegerRepeat { .. } => NativeOutputLane::Integer,
            Self::Boolean(_) | Self::BooleanRepeat { .. } => NativeOutputLane::Boolean,
        }
    }

    fn value(self, index: usize) -> NativeTypedInput {
        match self {
            Self::Integer(values) => NativeTypedInput::Integer(values[index]),
            Self::Boolean(values) => NativeTypedInput::Boolean(values[index]),
            Self::IntegerRepeat { value, .. } => NativeTypedInput::Integer(value),
            Self::BooleanRepeat { value, .. } => NativeTypedInput::Boolean(value),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeTransportError {
    BufferShape,
    AllocationRefused,
    SourcePristineUnavailable,
    SnapshotProductUnsupported,
    UnknownInput {
        p_index: usize,
    },
    InputType {
        p_index: usize,
    },
    InvalidInput {
        p_index: usize,
    },
    /// A parameter or constant cell has no exact typed value: a non-integral
    /// or out-of-range Integer, a Boolean other than 0 or 1, or an Integer
    /// magnitude above 2^53 that the program's Real view would round.
    InexactParameter {
        p_index: usize,
    },
    MissingInput {
        p_index: usize,
    },
    Busy,
    ForeignSnapshot,
    MissingOutput {
        p_index: usize,
    },
}

impl fmt::Display for NativeTransportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SnapshotProductUnsupported => f.write_str(
                "native continuation snapshot belongs to an unsupported resolved product",
            ),
            Self::SourcePristineUnavailable => {
                f.write_str("native storage has no original unresolved source defaults")
            }
            Self::AllocationRefused => f.write_str("native transport allocation refused"),
            Self::BufferShape => {
                f.write_str("native transport buffer shape differs from its layout")
            }
            Self::UnknownInput { p_index } => {
                write!(f, "P[{p_index}] has no admitted input storage")
            }
            Self::InputType { p_index } => write!(f, "P[{p_index}] input kind differs"),
            Self::InvalidInput { p_index } => write!(f, "P[{p_index}] Float64 input is non-finite"),
            Self::InexactParameter { p_index } => {
                write!(f, "P[{p_index}] parameter has no exact typed value")
            }
            Self::MissingInput { p_index } => write!(f, "P[{p_index}] input is unassigned"),
            Self::Busy => f.write_str("native product already has an active invocation"),
            Self::ForeignSnapshot => f.write_str("native snapshot belongs to another resource"),
            Self::MissingOutput { p_index } => {
                write!(f, "P[{p_index}] has no accepted typed output")
            }
        }
    }
}

impl std::error::Error for NativeTransportError {}

/// One resolved resource's buffers, without independent publication authority.
pub struct NativeTransport {
    pub(super) product: Rc<CompiledRetainedNativeProgramWasm>,
    pub(super) storage: NativeStorage,
}

/// Pre-initialization buffers bound to their original prepared code.
/// This type has no invocation lease, outcome or published-output API.
pub struct PreparedNativeStorage {
    storage: NativeStorage,
}

impl RetainedNativeWasmHost {
    /// Input presence remains absent even when caller coordinates contain values.
    pub fn transport(&self, y: &[f64], p: &[f64]) -> Result<NativeTransport, NativeTransportError> {
        Ok(NativeTransport {
            product: Rc::clone(&self.product),
            storage: NativeStorage::new(Rc::clone(&self.product.code), y, p)?,
        })
    }
}

impl PreparedNativeWasmCode {
    /// Prepare resource-local buffers without inventing an execution start time.
    pub fn storage(
        &self,
        y: &[f64],
        p: &[f64],
    ) -> Result<PreparedNativeStorage, NativeTransportError> {
        Ok(PreparedNativeStorage {
            storage: NativeStorage::new(Rc::clone(&self.code), y, p)?,
        })
    }
}

impl PreparedNativeStorage {
    /// Check the same private code owner without activating or replacing storage.
    pub fn require_execution_entry(&self) -> Result<(), WasmCompileError> {
        self.storage.code.require_execution_entry()
    }

    /// Restore compiler-issued source defaults without activating this resource.
    pub fn reset_to_source(&mut self) -> Result<(), NativeTransportError> {
        self.storage.restore_original_source()
    }

    pub fn snapshot(&self) -> Result<NativeTransportSnapshot, NativeTransportError> {
        self.storage.snapshot()
    }

    /// Preparation cannot restore an old resolved execution continuation.
    pub fn restore(
        &mut self,
        snapshot: &NativeTransportSnapshot,
    ) -> Result<(), NativeTransportError> {
        self.storage.restore(snapshot, None)
    }

    pub fn inputs(&self) -> &rumoca_ir_solve::RetainedNativePreparation {
        &self.storage.code.inputs
    }

    /// Move the same resource into resolved execution, or return it unchanged.
    pub fn activate(
        self,
        root: RetainedAssertionRoot,
    ) -> Result<NativeTransport, (Box<Self>, WasmCompileError)> {
        let code = PreparedNativeWasmCode {
            code: Rc::clone(&self.storage.code),
        };
        match code.bind_resolved(root) {
            Ok(product) => Ok(NativeTransport {
                product: Rc::new(product),
                storage: self.storage,
            }),
            Err(error) => Err((Box::new(self), error)),
        }
    }
}

// Both public states forward to the same private owner and validation policy.
macro_rules! resource_storage_accessors {
    ($resource:ty) => {
        impl $resource {
            /// Raw coordinates grant no derived-output or execution authority.
            pub fn coordinates(&self) -> (&[f64], &[f64]) {
                (&self.storage.y, &self.storage.p)
            }
            pub fn input_values(
                &self,
            ) -> impl Iterator<
                Item = (
                    rumoca_ir_solve::NativeInputLane,
                    Result<rumoca_ir_solve::SolveValueKind, NativeTransportError>,
                ),
            > {
                self.storage.input_values()
            }
            pub fn set_coordinates(
                &mut self,
                y: &[f64],
                p: &[f64],
            ) -> Result<(), NativeTransportError> {
                self.storage.set_coordinates(y, p)
            }
            pub fn set_typed_input(
                &mut self,
                p_index: usize,
                value: NativeTypedInput,
            ) -> Result<(), NativeTransportError> {
                self.storage.set_typed_input(p_index, value)
            }
            pub fn check_typed_inputs(
                &self,
                p_index: usize,
                values: NativeTypedInputBatch<'_>,
            ) -> Result<(), NativeTransportError> {
                self.storage.check_typed_inputs(p_index, values)
            }
            pub fn set_typed_inputs(
                &mut self,
                p_index: usize,
                values: NativeTypedInputBatch<'_>,
            ) -> Result<(), NativeTransportError> {
                self.storage.set_typed_inputs(p_index, values)
            }
            pub fn set_typed_input_batches(
                &mut self,
                batches: &[(usize, NativeTypedInputBatch<'_>)],
            ) -> Result<(), NativeTransportError> {
                self.storage.set_typed_input_batches(batches)
            }
            pub fn check_float64_input_batches(
                &self,
                batches: &[(usize, &[f64])],
            ) -> Result<(), NativeTransportError> {
                self.storage.check_float64_input_batches(batches)
            }
            /// Complete request preflight precedes all numeric writes and presence changes.
            pub fn set_float64_input_batches(
                &mut self,
                batches: &[(usize, &[f64])],
            ) -> Result<(), NativeTransportError> {
                self.storage.set_float64_input_batches(batches)
            }
            /// Check original Real input membership and actual assigned coverage.
            pub fn check_float64_inputs(
                &self,
                p_index: usize,
                count: usize,
            ) -> Result<(), NativeTransportError> {
                self.storage.check_float64_inputs(p_index, count)
            }
        }
    };
}
resource_storage_accessors!(PreparedNativeStorage);
resource_storage_accessors!(NativeTransport);

impl NativeTransport {
    /// Check this resolved resource's actual entry, without a target-derived flag.
    pub fn require_execution_entry(&self) -> Result<(), WasmCompileError> {
        self.product.code.require_execution_entry()
    }

    pub fn snapshot(&self) -> Result<NativeTransportSnapshot, NativeTransportError> {
        let mut snapshot = self.storage.snapshot()?;
        snapshot.resolved_product = Some(Rc::clone(&self.product));
        Ok(snapshot)
    }

    /// Continuation restore retains this exact active resolved product.
    pub fn restore(
        &mut self,
        snapshot: &NativeTransportSnapshot,
    ) -> Result<(), NativeTransportError> {
        self.storage.restore(snapshot, Some(&self.product))
    }

    /// Consume resolved execution and restore this resource's original source defaults.
    /// Refusal returns the unchanged actual owner; success grants preparation only.
    pub fn reset_to_preparation(
        mut self,
    ) -> Result<PreparedNativeStorage, (Box<Self>, NativeTransportError)> {
        if let Err(error) = self.storage.restore_original_source() {
            return Err((Box::new(self), error));
        }
        Ok(PreparedNativeStorage {
            storage: self.storage,
        })
    }

    pub fn product(&self) -> &CompiledRetainedNativeProgramWasm {
        &self.product
    }

    /// Share this actual resolved product, without rebinding another root.
    pub fn host(&self) -> RetainedNativeWasmHost {
        RetainedNativeWasmHost {
            product: Rc::clone(&self.product),
        }
    }

    /// Published typed cells exist only after an actual checked outcome was consumed.
    pub fn output_values(
        &self,
    ) -> impl Iterator<
        Item = (
            &rumoca_ir_solve::NativeDerivedOutput,
            Result<rumoca_ir_solve::SolveValueKind, NativeTransportError>,
        ),
    > {
        self.product
            .inputs()
            .schedule()
            .derived_outputs()
            .iter()
            .map(|output| {
                let value = if self.product.code.entered.get() {
                    Err(NativeTransportError::Busy)
                } else if !self.storage.outputs_present {
                    Err(NativeTransportError::MissingOutput {
                        p_index: output.p_index(),
                    })
                } else {
                    let offset =
                        self.product.inputs().schedule().input_lane_bytes() + output.lane_offset();
                    Ok(storage::read_cell(
                        &self.storage.lanes,
                        offset,
                        output.lane(),
                    ))
                };
                (output, value)
            })
    }
    /// Acquire the shared-arena gate before exposing any invocation buffers.
    pub fn enter(&mut self) -> Result<NativeInvocationLease<'_>, NativeTransportError> {
        if self.product.code.entered.get() {
            return Err(NativeTransportError::Busy);
        }
        use rumoca_ir_solve::{ScalarSlot, SolveVariableStorageRole, SolveVariableValueKind};
        for run in &self
            .product
            .inputs()
            .root()
            .model()
            .problem
            .solve_layout
            .variable_storage_runs
        {
            if run.role != SolveVariableStorageRole::ExternalInput
                || !matches!(
                    run.value_kind,
                    SolveVariableValueKind::Real
                        | SolveVariableValueKind::Integer
                        | SolveVariableValueKind::Boolean
                )
            {
                continue;
            }
            let ScalarSlot::P { index, .. } = run.base else {
                return Err(NativeTransportError::BufferShape);
            };
            let end = index
                .checked_add(run.scalar_count)
                .ok_or(NativeTransportError::BufferShape)?;
            if let Some(p_index) = self.storage.assigned.first_missing(index..end) {
                return Err(NativeTransportError::MissingInput { p_index });
            }
        }
        // Parameter and constant lanes are written from their source at construction.
        for run in self
            .product
            .inputs()
            .schedule()
            .input_runs()
            .iter()
            .filter(|run| run.role() != SolveVariableStorageRole::ExternalInput)
        {
            if let Some(p_index) = self.storage.assigned.first_missing(run.p_range()) {
                return Err(NativeTransportError::MissingInput { p_index });
            }
        }
        self.product.code.entered.set(true);
        Ok(NativeInvocationLease { transport: self })
    }
}

/// An opaque same-resource checkpoint, containing actual values, not source proof.
#[derive(Clone)]
pub struct NativeTransportSnapshot {
    resolved_product: Option<Rc<CompiledRetainedNativeProgramWasm>>,
    instance_brand: Rc<()>,
    y: Rc<Vec<f64>>,
    p: Rc<Vec<f64>>,
    lanes: Rc<Vec<u64>>,
    assigned: Rc<InputPresence>,
    outputs_present: bool,
}

/// Keeps one product's private arena exclusive. This is transport permission,
/// not a successful execution outcome. Its immutable buffers retain exact bits.
pub struct NativeInvocationLease<'call> {
    pub(super) transport: &'call mut NativeTransport,
}

impl NativeInvocationLease<'_> {
    pub fn product(&self) -> &CompiledRetainedNativeProgramWasm {
        &self.transport.product
    }

    pub fn y(&self) -> &[f64] {
        &self.transport.storage.y
    }

    pub fn p(&self) -> &[f64] {
        &self.transport.storage.p
    }

    pub fn scratch(&self) -> &[u64] {
        &self.transport.storage.scratch
    }

    pub fn typed_lanes(&self) -> &[u64] {
        &self.transport.storage.lanes
    }
}

impl Drop for NativeInvocationLease<'_> {
    fn drop(&mut self) {
        self.transport.product.code.entered.set(false);
    }
}
