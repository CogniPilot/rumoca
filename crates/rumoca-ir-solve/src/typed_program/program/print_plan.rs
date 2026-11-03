//! Print projection of one checked typed program body.
//!
//! A printer spells this projection and nothing else: the frame its storage
//! plan issues, one body item per operation in order, and the output copies.
//! The projection is assembled here from the issued body; a printer cannot
//! mint or amend one, and a program replayed from its wire form projects
//! again. An operation whose printing has not moved here yet is a `Legacy`
//! item naming the audit ledger row that owns its remaining printer decision.

mod scalar;

use serde::Serialize;

use super::{
    SolveOperation, SolveRegisterPlacement, SolveSlot, SolveSlotPlacement, SolveSpannedOperation,
    SolveStorageClass, SolveStoragePlan, SolveValueType,
};
use crate::{SolveRealFormat, SolveScalarType};
pub use scalar::{
    SolveBinaryToken, SolveCheckedToken, SolveConversionToken, SolveDomainCheck,
    SolveElementOperand, SolveIntegerLiteral, SolveKernel, SolveLiteral, SolveUnaryToken,
};

/// The element token of an issued value: each printer has one fixed
/// spelling per token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveElementToken {
    Real64,
    Real32,
    Int64,
    Boolean,
}

impl SolveElementToken {
    fn of(value_type: &SolveValueType) -> Self {
        match value_type.element_type() {
            SolveScalarType::Real {
                format: SolveRealFormat::Binary64,
            } => Self::Real64,
            SolveScalarType::Real {
                format: SolveRealFormat::Binary32,
            } => Self::Real32,
            SolveScalarType::Integer(_) => Self::Int64,
            SolveScalarType::Boolean => Self::Boolean,
        }
    }
}

/// The storage name space a printed name belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveNameSpace {
    Slot,
    Register,
}

/// One slot or register name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveName {
    space: SolveNameSpace,
    id: usize,
}

/// One declaration of the body's frame, in print order.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SolveFrameItem {
    /// Slot `slot` reads the caller's array for input `argument`.
    ArgumentView {
        slot: usize,
        argument: usize,
        element: SolveElementToken,
    },
    /// An owned frame array.
    Frame {
        name: SolveName,
        element: SolveElementToken,
        extent: u32,
    },
    /// An owned frame array for slot `slot`, copied in from input `argument`.
    FrameFromArgument {
        slot: usize,
        argument: usize,
        element: SolveElementToken,
        extent: u32,
    },
}

/// One issued operation of the body, in order.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SolveBodyItem {
    /// Register `register` is a read-only view of slot `slot`.
    View {
        index: usize,
        register: usize,
        slot: usize,
        element: SolveElementToken,
    },
    /// The whole value of `source` is copied into `destination`.
    Copy {
        index: usize,
        destination: SolveName,
        source: SolveName,
    },
    /// Register `destination` holds the issued literal `value`.
    Constant {
        index: usize,
        destination: usize,
        value: SolveLiteral,
    },
    /// The whole value of `if_true` or `if_false`, chosen by the first
    /// element of `condition`.
    Select {
        index: usize,
        destination: usize,
        condition: usize,
        if_true: usize,
        if_false: usize,
    },
    /// `extent` elements of `destination`, each computed by `kernel`, then
    /// checked against the destination's integer `domain`.
    Elementwise {
        index: usize,
        destination: usize,
        extent: u32,
        kernel: SolveKernel,
        domain: Option<SolveDomainCheck>,
    },
    /// `extent` elements of `operand` converted by `token`, then checked
    /// against the destination's integer `domain`.
    Convert {
        index: usize,
        destination: usize,
        operand: usize,
        extent: u32,
        token: SolveConversionToken,
        domain: Option<SolveDomainCheck>,
    },
    /// Not yet projected: printed from the operation by the printer's
    /// unmigrated macro, under the decision owned by audit row `ledger`, or
    /// by no audited row yet (`None`), which the conformance gate rejects.
    Legacy {
        index: usize,
        ledger: Option<&'static str>,
    },
}

/// The copy of output slot `slot` into the caller's output `output`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveOutputCopy {
    slot: usize,
    output: usize,
}

impl SolveName {
    #[must_use]
    pub const fn space(&self) -> SolveNameSpace {
        self.space
    }

    #[must_use]
    pub const fn id(&self) -> usize {
        self.id
    }
}

impl SolveOutputCopy {
    #[must_use]
    pub const fn slot(&self) -> usize {
        self.slot
    }

    #[must_use]
    pub const fn output(&self) -> usize {
        self.output
    }
}

/// The print projection of one program body.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize)]
pub struct SolvePrintPlan {
    slots: Box<[SolveFrameItem]>,
    registers: Box<[SolveFrameItem]>,
    body: Box<[SolveBodyItem]>,
    outputs: Box<[SolveOutputCopy]>,
}

impl SolvePrintPlan {
    pub(super) fn of(
        slots: &[SolveSlot],
        register_types: &[SolveValueType],
        operations: &[SolveSpannedOperation],
        storage: &SolveStoragePlan,
    ) -> Self {
        let slot_items = slots
            .iter()
            .zip(storage.slots())
            .enumerate()
            .map(|(slot, (declared, placement))| {
                let element = SolveElementToken::of(declared.value_type());
                match *placement {
                    SolveSlotPlacement::Argument { argument } => SolveFrameItem::ArgumentView {
                        slot,
                        argument,
                        element,
                    },
                    SolveSlotPlacement::Frame {
                        extent,
                        argument: Some(argument),
                    } => SolveFrameItem::FrameFromArgument {
                        slot,
                        argument,
                        element,
                        extent,
                    },
                    SolveSlotPlacement::Frame {
                        extent,
                        argument: None,
                    } => SolveFrameItem::Frame {
                        name: SolveName {
                            space: SolveNameSpace::Slot,
                            id: slot,
                        },
                        element,
                        extent,
                    },
                }
            })
            .collect();
        let register_items = register_types
            .iter()
            .zip(storage.registers())
            .enumerate()
            .filter_map(|(register, (value_type, placement))| match *placement {
                SolveRegisterPlacement::View { .. } => None,
                SolveRegisterPlacement::Frame { extent } => Some(SolveFrameItem::Frame {
                    name: SolveName {
                        space: SolveNameSpace::Register,
                        id: register,
                    },
                    element: SolveElementToken::of(value_type),
                    extent,
                }),
            })
            .collect();
        let body = operations
            .iter()
            .enumerate()
            .map(|(index, issued)| body_item(index, &issued.operation, register_types, storage))
            .collect();
        let mut output = 0;
        let outputs = slots
            .iter()
            .enumerate()
            .filter(|(_, slot)| slot.storage() == SolveStorageClass::Output)
            .map(|(slot, _)| {
                let copy = SolveOutputCopy { slot, output };
                output += 1;
                copy
            })
            .collect();
        Self {
            slots: slot_items,
            registers: register_items,
            body,
            outputs,
        }
    }

    /// Slot declarations, in slot order.
    #[must_use]
    pub fn slots(&self) -> &[SolveFrameItem] {
        &self.slots
    }

    /// Owned register declarations, in register order.
    #[must_use]
    pub fn registers(&self) -> &[SolveFrameItem] {
        &self.registers
    }

    /// One item per issued operation, in order.
    #[must_use]
    pub fn body(&self) -> &[SolveBodyItem] {
        &self.body
    }

    /// Output copies, in output order.
    #[must_use]
    pub fn outputs(&self) -> &[SolveOutputCopy] {
        &self.outputs
    }
}

fn body_item(
    index: usize,
    operation: &SolveOperation,
    register_types: &[SolveValueType],
    storage: &SolveStoragePlan,
) -> SolveBodyItem {
    let register = |id: usize| SolveName {
        space: SolveNameSpace::Register,
        id,
    };
    let slot = |id: usize| SolveName {
        space: SolveNameSpace::Slot,
        id,
    };
    match operation {
        SolveOperation::Load {
            destination,
            slot: loaded,
        } => match storage.registers().get(destination.index()) {
            Some(SolveRegisterPlacement::View { slot: viewed }) => SolveBodyItem::View {
                index,
                register: destination.index(),
                slot: *viewed,
                element: SolveElementToken::of(&register_types[destination.index()]),
            },
            _ => SolveBodyItem::Copy {
                index,
                destination: register(destination.index()),
                source: slot(loaded.index()),
            },
        },
        SolveOperation::Store {
            slot: stored,
            source,
        } => SolveBodyItem::Copy {
            index,
            destination: slot(stored.index()),
            source: register(source.index()),
        },
        other => {
            scalar::scalar_item(index, other, register_types).unwrap_or(SolveBodyItem::Legacy {
                index,
                ledger: legacy_ledger(other),
            })
        }
    }
}

/// The audit row (rumoca backend-decision audit 677a378b) that owns the
/// remaining printer decision for an operation not yet projected.
fn legacy_ledger(operation: &SolveOperation) -> Option<&'static str> {
    match operation {
        SolveOperation::Map { .. } | SolveOperation::Fold { .. } => Some("D056"),
        SolveOperation::Concatenate { .. } => Some("D061"),
        SolveOperation::Transpose { .. } => Some("D062"),
        SolveOperation::MatrixMultiply { .. } => Some("D063"),
        SolveOperation::Reduce { .. } => Some("D064"),
        SolveOperation::ProjectElement { .. }
        | SolveOperation::ProjectElementDynamic { .. }
        | SolveOperation::ProjectSlice { .. }
        | SolveOperation::ProjectView { .. }
        | SolveOperation::SelectElement { .. }
        | SolveOperation::UpdateElement { .. }
        | SolveOperation::UpdateSlice { .. }
        | SolveOperation::UpdateView { .. } => Some("D065"),
        SolveOperation::LinearSolve { .. } => Some("D066"),
        SolveOperation::Constant { .. }
        | SolveOperation::Unary { .. }
        | SolveOperation::Binary { .. }
        | SolveOperation::BroadcastBinary { .. }
        | SolveOperation::Compare { .. }
        | SolveOperation::Convert { .. } => Some("D068"),
        _ => None,
    }
}
