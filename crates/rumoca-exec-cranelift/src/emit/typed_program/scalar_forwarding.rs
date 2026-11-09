//! Write-through forwarding of private scalar tape cells in a flat f64 entry.
//!
//! This is physical storage forwarding, not source common-subexpression
//! elimination: every authored operation and guard remains in order. Every
//! store still happens. Only a later load of the same private scalar cell can
//! use the dominating SSA value instead. Aggregate operations and all other
//! forms end the forwarding interval before they execute.

use super::*;

#[derive(Default)]
pub(super) struct ScalarForwarding {
    values: BTreeMap<u32, Value>,
    enabled: bool,
}

impl ScalarForwarding {
    pub(super) fn prepare(
        &mut self,
        operation: &solve::SolveOperation,
        source: &solve::TypedProgram,
    ) {
        use solve::SolveOperation as O;
        self.enabled = matches!(
            operation,
            O::Load { .. }
                | O::Constant { .. }
                | O::Unary { .. }
                | O::Binary { .. }
                | O::Compare { .. }
                | O::Convert { .. }
                | O::Select { .. }
                | O::CheckFinite { .. }
        );
        let mut scalar = |register: solve::SolveRegisterId| {
            self.enabled &= source.register_types()[register.index()].scalar_count() == 1;
        };
        operation.visit_input_registers(&mut scalar);
        operation.visit_output_registers(scalar);
        if !self.enabled {
            self.values.clear();
        }
    }

    pub(super) fn get(&self, cell: u32) -> Option<Value> {
        self.enabled
            .then(|| self.values.get(&cell).copied())
            .flatten()
    }

    pub(super) fn put(&mut self, cell: u32, value: Value) {
        if self.enabled {
            self.values.insert(cell, value);
        }
    }
}
