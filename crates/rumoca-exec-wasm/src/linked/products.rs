//! The set assembler: fed the checked products in its owner's list order, it
//! issues each entry's dense position, recomputes its key from the original
//! checked source before printing anything, and prints every module with the
//! same assembler, from the same source, as its browser module; only link
//! names differ. It sees every fault table, so it reserves the refusal status.
use super::plan::refuse_reserved_status;
use super::{
    LinkedBinding, LinkedDemand, LinkedEntry, LinkedEntryId, LinkedEntryKind, LinkedEntryPlan,
    LinkedFaultTables, LinkedWorkspace, linked_key_words,
};
use crate::emit::Linkage;
use crate::emit::derivative_operation::HELPER_ROLES;
use crate::{
    CompiledNativeCallProgramWasm, NativeGatherFault, NativeLinearSolveFault, TypedCallFault,
    WasmCompileError,
};
use rumoca_ir_solve::{
    self as solve, ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule,
    IssuedAssignmentRows, PreparedKernelSource, SolvePureCallTable, VarLayout,
    fmi::{FmiDerivativeCausalOperation, FmiDerivativeStatus},
};

/// One linked library: an original emission with its issued link names. It
/// imports only `env.memory`, the math ABI and other modules' issued symbols.
#[derive(Debug, Clone)]
#[cfg_attr(
    feature = "precompiled-programs",
    derive(serde::Serialize, serde::Deserialize),
    serde(deny_unknown_fields)
)]
pub struct LinkedKernelModule {
    #[cfg_attr(feature = "precompiled-programs", serde(with = "serde_bytes"))]
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

    /// A module of one kernel entry and that entry's binding.
    fn single(
        program: CompiledNativeCallProgramWasm,
        symbol: String,
    ) -> Result<(Self, LinkedBinding), WasmCompileError> {
        let binding = LinkedBinding::Kernel {
            scratch_bytes: program.scratch_bytes,
            faults: tables(
                &program.faults,
                &program.gather_faults,
                &program.linear_solve_faults,
            )?,
        };
        let module = Self {
            bytes: program.bytes,
            exports: vec![symbol],
        };
        Ok((module, binding))
    }
}

/// An entry's fault tables, refused when a status equals the dispatch's
/// refusal: the assembler sees every table, so it owns that reservation.
fn tables(
    typed: &[TypedCallFault],
    gather: &[NativeGatherFault],
    linear: &[NativeLinearSolveFault],
) -> Result<LinkedFaultTables, WasmCompileError> {
    let tables = LinkedFaultTables {
        typed: typed.to_vec(),
        gather: gather.to_vec(),
        linear: linear.to_vec(),
    };
    refuse_reserved_status(tables.statuses())?;
    Ok(tables)
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

/// The digest the linked artifact binds bytes with (BLAKE3, lowercase hex).
#[cfg(feature = "precompiled-programs")]
pub fn linked_digest(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

/// The issued product as it travels from the build to the guest beside the
/// saved product it was captured with: bound to those saved bytes by their
/// digest, carrying every module in link order and the dispatch module. It
/// grants nothing by itself: nothing reads its entries but `certify`, which
/// compares it whole with a product the receiver issued from its own checked
/// sources in the owner's order. The modules and the dispatch module are
/// readable for linking; whatever is linked is certified by the receiver.
#[cfg(feature = "precompiled-programs")]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LinkedArtifact {
    version: u32,
    saved: String,
    entries: Vec<LinkedEntry>,
    modules: Vec<LinkedKernelModule>,
    #[serde(with = "serde_bytes")]
    dispatch: Vec<u8>,
}

#[cfg(feature = "precompiled-programs")]
impl LinkedProduct {
    /// The artifact of this product, bound to the saved product bytes it
    /// was captured with.
    pub fn artifact(&self, saved: &[u8]) -> LinkedArtifact {
        LinkedArtifact {
            version: 1,
            saved: linked_digest(saved),
            entries: self.plan.linked_entries().cloned().collect(),
            modules: self.modules.clone(),
            dispatch: self.plan.dispatch(),
        }
    }
}

#[cfg(feature = "precompiled-programs")]
impl LinkedArtifact {
    /// The modules in link order, for the component link.
    pub fn modules(&self) -> &[LinkedKernelModule] {
        &self.modules
    }

    /// The dispatch module, for the component link.
    pub fn dispatch_module(&self) -> &[u8] {
        &self.dispatch
    }

    /// Admit this artifact only if it is exactly what `reissued`, issued by
    /// the receiver from its own checked sources in the owner's order, and
    /// the saved product `saved` produce: every position's kind, index, key
    /// and binding, every module's bytes and exports in link order, and the
    /// dispatch module. Equal keys at two positions are compared by position.
    pub fn certify(&self, saved: &[u8], reissued: &LinkedProduct) -> Result<(), WasmCompileError> {
        let entries = reissued.plan.linked_entries();
        let same_entries = self.entries.len() == reissued.plan.linked_entries().count()
            && self
                .entries
                .iter()
                .zip(entries)
                .all(|(carried, issued)| carried == issued);
        let same_modules = self.modules.len() == reissued.modules.len()
            && self
                .modules
                .iter()
                .zip(&reissued.modules)
                .all(|(carried, issued)| {
                    carried.bytes == issued.bytes && carried.exports == issued.exports
                });
        if self.version != 1
            || self.saved != linked_digest(saved)
            || !same_entries
            || !same_modules
            || self.dispatch != reissued.plan.dispatch()
        {
            return Err(WasmCompileError::Input(
                "linked artifact differs from the product its checked sources issue".into(),
            ));
        }
        Ok(())
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
        entries: Vec<LinkedEntry>,
        modules: Vec<LinkedKernelModule>,
    ) -> Vec<LinkedEntryId> {
        self.modules.extend(modules);
        entries
            .into_iter()
            .map(|entry| {
                let id = entry.id();
                self.plan.issue(entry);
                id
            })
            .collect()
    }

    /// One kernel entry in its own module.
    fn commit_single(
        &mut self,
        id: LinkedEntryId,
        key: [u64; 4],
        program: CompiledNativeCallProgramWasm,
    ) -> Result<LinkedEntryId, WasmCompileError> {
        let (module, binding) = LinkedKernelModule::single(program, id.symbol())?;
        self.commit(vec![LinkedEntry::new(id, key, binding)], vec![module]);
        Ok(id)
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
        let (control, workspace, helpers) =
            crate::derivative_operation::compile_with(operation, Some(&symbols))?.into_modules();
        let mut modules = Vec::new();
        let mut helper_tables = [None, None, None, None];
        for ((role, program), slot) in helpers.into_iter().enumerate().zip(&mut helper_tables) {
            if let Some(program) = program {
                *slot = Some(tables(
                    &program.faults,
                    &program.gather_faults,
                    &program.linear_solve_faults,
                )?);
                modules.push(LinkedKernelModule {
                    bytes: program.bytes,
                    exports: symbols.helpers[role].clone(),
                });
            }
        }
        modules.push(LinkedKernelModule {
            bytes: control,
            exports: symbols.control.clone(),
        });
        let binding = LinkedBinding::Derivative {
            workspace: LinkedWorkspace::of(workspace),
            helpers: Box::new(helper_tables),
        };
        let entries = std::iter::once(complete)
            .chain(kinematic)
            .map(|id| LinkedEntry::new(id, key, binding.clone()))
            .collect();
        Ok(self.commit(entries, modules))
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
        self.commit_single(id, key, program)
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
        self.commit_single(id, key, program)
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
        let mut modules = Vec::new();
        let mut bindings = Vec::with_capacity(issued.len());
        for (program, entries) in admission.linked_modules(programs, &symbols)? {
            for entry in &entries {
                bindings.push(LinkedBinding::Kernel {
                    scratch_bytes: program.scratch_bytes,
                    faults: tables(
                        &program.faults,
                        &entry.gather_faults,
                        &entry.linear_solve_faults,
                    )?,
                });
            }
            modules.push(LinkedKernelModule {
                bytes: program.bytes,
                exports: entries.into_iter().map(|entry| entry.export).collect(),
            });
        }
        if bindings.len() != issued.len() {
            return Err(WasmCompileError::Backend(
                "linked private modules returned other than one entry per program".into(),
            ));
        }
        let entries = issued
            .into_iter()
            .zip(bindings)
            .map(|((id, key), binding)| LinkedEntry::new(id, key, binding))
            .collect();
        Ok(self.commit(entries, modules))
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
        self.commit_single(id, key, code)
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
