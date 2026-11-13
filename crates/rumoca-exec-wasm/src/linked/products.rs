//! The set assembler: fed the checked products in its owner's list order, it
//! issues each entry's dense position, recomputes its key from the original
//! checked source before printing anything, and prints every module with the
//! same assembler, from the same source, as its browser module; only link
//! names differ. It sees every fault table, so it reserves the refusal status.
use super::plan::refuse_reserved_status;
use super::{LinkedDemand, LinkedEntryId, LinkedEntryKind, LinkedEntryPlan, linked_key_words};
use crate::emit::Linkage;
use crate::emit::derivative_operation::HELPER_ROLES;
use crate::{CompiledNativeCallProgramWasm, WasmCompileError};
use rumoca_ir_solve::{
    self as solve, ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule,
    IssuedAssignmentRows, PreparedKernelSource, SolvePureCallTable, VarLayout,
    fmi::{FmiDerivativeCausalOperation, FmiDerivativeStatus},
};

/// One linked library: an original emission with its issued link names. It
/// imports only `env.memory`, the math ABI and other modules' issued symbols.
#[derive(Debug, Clone)]
pub struct LinkedKernelModule {
    bytes: Vec<u8>,
    exports: Vec<String>,
}

impl LinkedKernelModule {
    pub fn module_bytes(&self) -> &[u8] {
        &self.bytes
    }

    /// The issued symbols this module exports, in export order.
    pub fn exports(&self) -> &[String] {
        &self.exports
    }

    /// A kernel module after every status it can return is checked.
    fn kernel(
        program: CompiledNativeCallProgramWasm,
        entry_statuses: impl IntoIterator<Item = u32>,
        exports: Vec<String>,
    ) -> Result<Self, WasmCompileError> {
        refuse_reserved_status(
            program
                .faults
                .iter()
                .map(|fault| fault.status)
                .chain(program.gather_faults.iter().map(|fault| fault.status))
                .chain(program.linear_solve_faults.iter().map(|fault| fault.status))
                .chain(entry_statuses),
        )?;
        Ok(Self {
            bytes: program.bytes,
            exports,
        })
    }
}

/// The symbols of one complete derivative and its full helper graph.
pub(crate) struct DerivativeSymbols {
    helpers: [Vec<String>; 4],
    control: Vec<String>,
    operations: String,
}

impl DerivativeSymbols {
    fn new(complete: String, kinematic: Option<String>) -> Self {
        let helpers = HELPER_ROLES.map(|role| vec![helper_symbol(&complete, role)]);
        Self {
            helpers,
            control: std::iter::once(complete.clone()).chain(kinematic).collect(),
            operations: complete,
        }
    }

    /// The helper in `HELPER_ROLES` position `role`.
    pub(crate) fn helper(&self, role: usize) -> Linkage<'_> {
        Linkage::Linked {
            entries: &self.helpers[role],
            operations: "",
        }
    }

    pub(crate) fn control(&self) -> Linkage<'_> {
        Linkage::Linked {
            entries: &self.control,
            operations: &self.operations,
        }
    }
}

/// `<complete entry symbol>_<role>`: one name per helper of one derivative,
/// shared by the helper's export and its own control module's import.
pub(crate) fn helper_symbol(operations: &str, role: &str) -> String {
    format!("{operations}_{role}")
}

/// The key `rumoca_ir_solve::PreparedKernelSource` issues for a checked source.
fn issued_key(
    layout: &VarLayout,
    table: &SolvePureCallTable,
    key: impl FnOnce(&PreparedKernelSource) -> Result<String, String>,
) -> Result<[u64; 4], WasmCompileError> {
    let key = PreparedKernelSource::new(layout, table)
        .and_then(|source| key(&source))
        .map_err(WasmCompileError::Input)?;
    linked_key_words(&key)
}

/// A complete linked product: its issued identities and every module.
pub struct LinkedProduct {
    plan: LinkedEntryPlan,
    modules: Vec<LinkedKernelModule>,
}

impl LinkedProduct {
    pub fn plan(&self) -> &LinkedEntryPlan {
        &self.plan
    }

    /// Every module, each after the modules whose symbols it imports.
    pub fn modules(&self) -> &[LinkedKernelModule] {
        &self.modules
    }
}

/// Issues a product's identities from its checked products, in its owner's
/// list order per kind. A refused product leaves the assembler unchanged.
#[derive(Default)]
pub struct LinkedProductAssembler {
    plan: LinkedEntryPlan,
    modules: Vec<LinkedKernelModule>,
}

impl LinkedProductAssembler {
    pub fn new() -> Self {
        Self::default()
    }

    fn next(&self, kind: LinkedEntryKind) -> LinkedEntryId {
        LinkedEntryId {
            kind,
            index: self.plan.next_index(kind),
        }
    }

    fn commit(
        &mut self,
        issued: Vec<(LinkedEntryId, [u64; 4])>,
        modules: Vec<LinkedKernelModule>,
    ) -> Vec<LinkedEntryId> {
        self.modules.extend(modules);
        issued
            .into_iter()
            .map(|(id, key)| {
                self.plan.issue(id, key);
                id
            })
            .collect()
    }

    /// The next complete derivative, its kinematic entry when the operation
    /// issues one, and its own static, dynamic, value and kinematic value
    /// helpers as the operation issues them; helpers precede their control.
    pub fn derivative(
        &mut self,
        operation: &FmiDerivativeCausalOperation<'_>,
    ) -> Result<Vec<LinkedEntryId>, WasmCompileError> {
        let source = operation.source();
        let key = issued_key(&source.problem().layout, source.calls(), |key| {
            key.fmi_derivative(operation)
        })?;
        let complete = self.next(LinkedEntryKind::Derivative);
        let kinematic = operation.kinematic().map(|_| LinkedEntryId {
            kind: LinkedEntryKind::Kinematic,
            index: complete.index,
        });
        if FmiDerivativeStatus::from_code(super::LINKED_DISPATCH_REFUSED).is_some() {
            return Err(WasmCompileError::Backend(
                "a derivative status equals the linked dispatch refusal".into(),
            ));
        }
        let symbols = DerivativeSymbols::new(complete.symbol(), kinematic.map(|id| id.symbol()));
        let (control, helpers) =
            crate::derivative_operation::compile_with(operation, Some(&symbols))?.into_modules();
        let mut modules = Vec::new();
        for (role, program) in helpers.into_iter().enumerate() {
            if let Some(program) = program {
                modules.push(LinkedKernelModule::kernel(
                    program,
                    [],
                    symbols.helpers[role].clone(),
                )?);
            }
        }
        modules.push(LinkedKernelModule {
            bytes: control,
            exports: symbols.control.clone(),
        });
        let issued = std::iter::once(complete)
            .chain(kinematic)
            .map(|id| (id, key))
            .collect();
        Ok(self.commit(issued, modules))
    }

    pub fn exact(
        &mut self,
        source: &ComputeBlock,
        owners: &ContinuousRefreshOwners,
        schedule: &ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
        table: &SolvePureCallTable,
    ) -> Result<LinkedEntryId, WasmCompileError> {
        let key = issued_key(layout, table, |key| {
            key.exact_assignment_schedule(source, owners, schedule)
        })?;
        let id = self.next(LinkedEntryKind::Exact);
        let symbol = vec![id.symbol()];
        let linkage = Linkage::Linked {
            entries: &symbol,
            operations: "",
        };
        let program = crate::emit::emit_exact_assignment_module(
            source, owners, schedule, layout, table, linkage,
        )
        .map_err(WasmCompileError::Backend)?;
        let module = LinkedKernelModule::kernel(program, [], symbol)?;
        Ok(self.commit(vec![(id, key)], vec![module])[0])
    }

    pub fn issued(
        &mut self,
        rows: &IssuedAssignmentRows,
        layout: &VarLayout,
        table: &SolvePureCallTable,
    ) -> Result<LinkedEntryId, WasmCompileError> {
        let key = issued_key(layout, table, |key| key.issued_assignment_rows(rows))?;
        let id = self.next(LinkedEntryKind::Issued);
        let symbol = vec![id.symbol()];
        let linkage = Linkage::Linked {
            entries: &symbol,
            operations: "",
        };
        let program = crate::emit::emit_issued_assignment_module(rows, layout, table, linkage)
            .map_err(WasmCompileError::Backend)?;
        let module = LinkedKernelModule::kernel(program, [], symbol)?;
        Ok(self.commit(vec![(id, key)], vec![module])[0])
    }

    /// One private batch, the programs the admission admitted in its compile
    /// order, in the same modules and entry order its browser compile prints.
    pub fn private(
        &mut self,
        admission: &crate::PrivateProgramAdmission,
        programs: &[&crate::AdmittedPrivateProgram],
    ) -> Result<Vec<LinkedEntryId>, WasmCompileError> {
        let (layout, table) = admission.source_context();
        let first = self.next(LinkedEntryKind::Private).index;
        let issued = (first..)
            .zip(programs)
            .map(|(index, program)| {
                let (block, seed) = program.source();
                let key = issued_key(layout, table, |key| key.private_program(block, seed))?;
                let id = LinkedEntryId {
                    kind: LinkedEntryKind::Private,
                    index,
                };
                Ok((id, key))
            })
            .collect::<Result<Vec<_>, WasmCompileError>>()?;
        let symbols = issued.iter().map(|(id, _)| id.symbol()).collect::<Vec<_>>();
        let modules = admission
            .linked_modules(programs, &symbols)?
            .into_iter()
            .map(|(program, entries)| {
                let statuses = entries
                    .iter()
                    .flat_map(|entry| {
                        entry
                            .gather_faults
                            .iter()
                            .map(|fault| fault.status)
                            .chain(entry.linear_solve_faults.iter().map(|fault| fault.status))
                    })
                    .collect::<Vec<_>>();
                let exports = entries.into_iter().map(|entry| entry.export).collect();
                LinkedKernelModule::kernel(program, statuses, exports)
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(self.commit(issued, modules))
    }

    pub fn event(
        &mut self,
        program: &solve::RetainedEventProgram,
    ) -> Result<LinkedEntryId, WasmCompileError> {
        let layout = &program.source().model().problem.layout;
        let key = issued_key(layout, program.table(), |key| key.retained_event(program))?;
        let id = self.next(LinkedEntryKind::Event);
        let symbol = vec![id.symbol()];
        let linkage = Linkage::Linked {
            entries: &symbol,
            operations: "",
        };
        let (code, _) = crate::emit::emit_event_program_module(program, layout, linkage)
            .map_err(WasmCompileError::Backend)?;
        let module = LinkedKernelModule::kernel(code, [], symbol)?;
        Ok(self.commit(vec![(id, key)], vec![module])[0])
    }

    /// The product, unless it demands an entry the linked profile cannot carry.
    pub fn finish(self, demands: &[LinkedDemand<'_>]) -> Result<LinkedProduct, WasmCompileError> {
        if let Some(demand) = demands.first() {
            return Err(demand.refusal());
        }
        Ok(LinkedProduct {
            plan: self.plan,
            modules: self.modules,
        })
    }
}
