//! The assertion-observation form of a native owner (SOLVE-C25): which
//! output cells hold its predicates, how its helper is declared, and how the
//! form is told apart from the value row.

use super::super::owned_jit_module::OwnedJitModule;
use super::super::to_backend_err;
use super::{
    CompileError, CompiledPureCallTable, NativeOwnerSupport, PureCallSymbol, checked_cells,
};
use cranelift_codegen::ir::Value;
use cranelift_jit::JITModule;
use cranelift_module::{FuncId, Linkage, Module};
use rumoca_ir_solve as solve;
use std::rc::Rc;

/// Whether an owner publishes assertion predicates, so an observation of it
/// has a form of its own.
pub(super) fn publishes_predicates(owner: &solve::SolvePureCallOwner) -> bool {
    owner
        .outputs()
        .iter()
        .any(|output| output.assertion_level().is_some())
}

pub(super) fn observation_owner_symbol(table: usize, owner: solve::SolvePureCallOwnerId) -> String {
    format!(
        "rumoca_typed_pure_call_observation_{table}_{}",
        owner.index()
    )
}

/// The owner-output cell of each predicate output, by output index.
fn predicate_cells(
    outputs: &[solve::SolvePureCallOutput],
) -> Result<Rc<[Option<u32>]>, CompileError> {
    let mut cell = 0u32;
    let mut cells = Vec::with_capacity(outputs.len());
    for output in outputs {
        cells.push(output.assertion_level().map(|_| cell));
        cell = checked_cells(
            cell,
            output.value_type().scalar_count(),
            "typed output layout",
        )?;
    }
    Ok(cells.into())
}

/// What an assertion-observation form needs beyond a value row: where the
/// owner's predicate cells are, so each executed failed assertion can clear
/// its cell and an Error-level one can stop.
#[derive(Clone)]
pub(super) struct ObservationLowering {
    /// The owner's output tape, which regions lowered inline reach through it.
    pub(super) owner_output: Value,
    pub(super) input_slots: usize,
    pub(super) cells: Rc<[Option<u32>]>,
    /// Only the owner's own program stores its predicate slots; a nested
    /// region has slots of its own.
    pub(super) root: bool,
}

impl ObservationLowering {
    pub(super) fn nested(&self) -> Self {
        Self {
            root: false,
            ..self.clone()
        }
    }

    /// Whether `slot` of the owner's own program is a predicate output, which
    /// this form publishes eagerly instead of by a final store.
    pub(super) fn publishes_slot(&self, slot: solve::SolveSlotId) -> bool {
        self.root
            && slot
                .index()
                .checked_sub(self.input_slots)
                .and_then(|output| self.cells.get(output))
                .is_some_and(Option::is_some)
    }
}

impl CompiledPureCallTable {
    pub(super) fn declare_observation_import(
        &self,
        module: &mut JITModule,
        signature: &cranelift_codegen::ir::Signature,
        primal: &PureCallSymbol,
    ) -> Result<Option<FuncId>, CompileError> {
        let admitted = self
            .owner_support(primal.owner)
            .is_some_and(NativeOwnerSupport::observation_is_native);
        if !admitted {
            return Ok(None);
        }
        let symbol = self
            .observation_symbols
            .get(primal.owner.index() as usize)
            .and_then(Option::as_ref)
            .unwrap_or(primal);
        module
            .declare_function(&symbol.name, Linkage::Import, signature)
            .map(Some)
            .map_err(to_backend_err)
    }
}

/// The observation form's cells and the output slots they occupy; empty for
/// every other form.
pub(super) fn observation_cells(
    observation: bool,
    input_count: usize,
    outputs: &[solve::SolvePureCallOutput],
) -> Result<ObservationCells, CompileError> {
    if !observation {
        return Ok(ObservationCells {
            cells: Rc::from([]),
            slots: Vec::new(),
        });
    }
    let cells = predicate_cells(outputs)?;
    let slots = cells
        .iter()
        .enumerate()
        .filter_map(|(output, cell)| cell.map(|_| input_count + output))
        .collect();
    Ok(ObservationCells { cells, slots })
}

pub(super) fn declare_observation(
    module: &mut OwnedJitModule,
    (table_id, owner): (usize, &solve::SolvePureCallOwner),
    admitted: &NativeOwnerSupport,
    signature: &cranelift_codegen::ir::Signature,
) -> Result<Option<FuncId>, CompileError> {
    if !publishes_predicates(owner) || !admitted.observation_is_native() {
        return Ok(None);
    }
    module
        .declare_function(
            &observation_owner_symbol(table_id, owner.id()),
            Linkage::Export,
            signature,
        )
        .map(Some)
        .map_err(to_backend_err)
}

/// The predicate cells of an observation form and the output slots holding them.
pub(super) struct ObservationCells {
    pub(super) cells: Rc<[Option<u32>]>,
    pub(super) slots: Vec<usize>,
}
