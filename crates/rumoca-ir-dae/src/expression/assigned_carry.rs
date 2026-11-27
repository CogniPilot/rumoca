//! A value that may be absent, and the two places allowed to hold one.
//!
//! MLS 3.7 §12.4.4: "It is an error to use or return an un-initialized
//! variable in a function." For a field of a record result the executed path
//! never wrote, the check is made where the field is read. Until then the
//! field holds a value or its absence, and the constructor admits that in
//! exactly two positions: a field of the record assigned to a function
//! output, and a call argument at a parameter the callee declares may arrive
//! absent. Every other parent is refused, so no operation can compute with a
//! value that may not exist.
//!
//! The checked reads are explicit. A plain `field(call, k)` asserts the field
//! is present. A parameter that may arrive absent has no plain coordinate at
//! all: it is read through `AssignedParameter { checked: true }`, so a
//! consumer cannot read it without deciding how the check is made.

use super::*;

impl<'dae> ExpressionAt<'_, 'dae> {
    /// Forward the value or absence of a call result's field to a call
    /// argument, without the presence check a plain read of it performs.
    pub fn pass_assigned(
        self,
        operand: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let nodes = &self.storage.expressions.nodes;
        let admitted = matches!(
            nodes.get(operand.index() as usize),
            Some(ExprNode::Field { base, .. })
                if matches!(nodes.get(*base as usize), Some(ExprNode::Call { .. }))
        );
        if !admitted {
            return Err(DaeConstructionError::InvalidAssignedStatePass {
                span: self.provenance.span(),
            });
        }
        let value_type =
            ValueTypeId::from_raw(self.storage.expressions.value_types[operand.index() as usize]);
        let variability = self.storage.expr_variability(operand, self.provenance)?;
        let binder_domain = self.storage.expr_binder_domain(operand, self.provenance)?;
        self.insert(
            ExprNode::PassAssigned {
                operand: operand.index(),
            },
            value_type,
            variability,
            binder_domain,
        )
    }

    /// A parameter its function declares may arrive absent: the checked read
    /// of its value, or (`checked == false`) its value or absence forwarded
    /// to a call argument.
    pub fn assigned_parameter(
        self,
        parameter: FunctionParameterId<'dae>,
        checked: bool,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let (ty, variability) = self
            .storage
            .function_parameter_facts(parameter, self.provenance)?;
        if !accepts_absence(
            self.storage,
            parameter.function().index(),
            parameter.ordinal(),
        ) {
            return Err(DaeConstructionError::InvalidAssignedStatePass {
                span: self.provenance.span(),
            });
        }
        self.insert(
            ExprNode::AssignedParameter {
                function: parameter.function().index(),
                ordinal: parameter.ordinal(),
                checked,
            },
            ty,
            variability,
            None,
        )
    }
}

pub(super) fn accepts_absence(storage: &Storage, function: u32, ordinal: u32) -> bool {
    storage
        .functions
        .get(function as usize)
        .is_some_and(|entry| entry.absent_parameters.binary_search(&ordinal).is_ok())
}

/// A value or absence on its way to a call argument.
fn is_forward(storage: &Storage, expression: u32) -> bool {
    matches!(
        storage.expressions.nodes.get(expression as usize),
        Some(ExprNode::PassAssigned { .. } | ExprNode::AssignedParameter { checked: false, .. })
    )
}

/// Whether `expression` holds a value or its absence: a forwarded state, a
/// definition a carried action issued, a record with such a field, or a read
/// of the output that was assigned such a record.
pub(crate) fn carries_assigned_state(storage: &Storage, expression: u32) -> bool {
    match storage.expressions.nodes.get(expression as usize) {
        Some(
            ExprNode::PassAssigned { .. } | ExprNode::AssignedParameter { checked: false, .. },
        ) => true,
        Some(ExprNode::FunctionValue {
            function,
            definition_ordinal,
            ..
        }) => storage
            .functions
            .get(*function as usize)
            .is_some_and(|entry| {
                entry
                    .carried_definitions
                    .binary_search(definition_ordinal)
                    .is_ok()
                    || entry
                        .definitions
                        .get(*definition_ordinal as usize)
                        .is_some_and(|definition| carries_assigned_state(storage, definition.rhs))
            }),
        Some(ExprNode::Record { operands }) => storage.expressions.operands[operands.indices()]
            .iter()
            .any(|&operand| carries_assigned_state(storage, operand)),
        _ => false,
    }
}

/// Refuse a node that would compute with a value that may be absent, and the
/// plain coordinate of a parameter that may arrive absent.
pub(super) fn check_carried_operands(
    storage: &Storage,
    node: &ExprNode,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let refused = || DaeConstructionError::InvalidAssignedStatePass { span: at.span() };
    match node {
        ExprNode::Coordinate(Coordinate::FunctionParameter { function, ordinal }) => {
            if accepts_absence(storage, *function, *ordinal) {
                return Err(refused());
            }
            Ok(())
        }
        // The operand of a forward is a checked source, never a carrier.
        ExprNode::Record { .. } | ExprNode::PassAssigned { .. } => Ok(()),
        ExprNode::Call {
            function, operands, ..
        } => {
            for (ordinal, &operand) in storage.expressions.operands[operands.indices()]
                .iter()
                .enumerate()
            {
                let accepted = is_forward(storage, operand)
                    && accepts_absence(storage, *function, ordinal as u32);
                if !accepted && carries_assigned_state(storage, operand) {
                    return Err(refused());
                }
            }
            Ok(())
        }
        node => {
            let mut carried = false;
            node.for_each_child(&storage.expressions, |child| {
                carried |= carries_assigned_state(storage, child);
            });
            if carried { Err(refused()) } else { Ok(()) }
        }
    }
}
