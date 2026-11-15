//! The set assembler: fed the checked products in its owner's list order, it
//! issues each entry's dense position, recomputes its key from the original
//! checked source before printing anything, and prints every module with the
//! same assembler, from the same source, as its browser module; only link
//! names differ. It sees every fault table, so it reserves the refusal status.
use super::plan::{entry_statuses, refuse_reserved_status, typed_position};
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
}

/// A derivative helper's fault tables, refused when a status equals the
/// dispatch's refusal: the assembler sees every table, so it owns that
/// reservation.
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
    let source = PreparedKernelSource::new(layout, table).map_err(WasmCompileError::Input)?;
    source_key(&source, key)
}

/// The key an already-built source context issues. The context digests the whole
/// layout and call table, so a batch over one context builds it once.
fn source_key(
    source: &PreparedKernelSource,
    key: impl FnOnce(&PreparedKernelSource) -> Result<String, String>,
) -> Result<[u64; 4], WasmCompileError> {
    linked_key_words(&key(source).map_err(WasmCompileError::Input)?)
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
/// digest, carrying every module in link order, each kernel module's typed
/// fault table once, in issue order, and the dispatch module. It
/// grants nothing by itself: nothing reads its entries but its admission
/// (`admit`), which compares them, request by request and then whole, with
/// what the receiver issues from its own checked sources in the owner's
/// order. The modules and the dispatch module are
/// readable for linking; whatever is linked is certified by the receiver.
#[cfg(feature = "precompiled-programs")]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct LinkedArtifact {
    version: u32,
    saved: String,
    entries: Vec<LinkedEntry>,
    typed: Vec<Vec<TypedCallFault>>,
    modules: Vec<LinkedKernelModule>,
    #[serde(with = "serde_bytes")]
    dispatch: Vec<u8>,
}

/// The artifact wire version: 2 since kernel bindings name a typed table the
/// artifact carries once instead of each holding a copy.
#[cfg(feature = "precompiled-programs")]
const LINKED_ARTIFACT_VERSION: u32 = 2;

#[cfg(feature = "precompiled-programs")]
impl LinkedProduct {
    /// The artifact of this product, bound to the saved product bytes it
    /// was captured with.
    pub fn artifact(&self, saved: &[u8]) -> LinkedArtifact {
        LinkedArtifact {
            version: LINKED_ARTIFACT_VERSION,
            saved: linked_digest(saved),
            entries: self.plan.linked_entries().cloned().collect(),
            typed: self.plan.typed_tables().to_vec(),
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
}

#[cfg(feature = "precompiled-programs")]
fn refusal(message: &str) -> WasmCompileError {
    WasmCompileError::Input(message.into())
}

/// Every module export is an entry's symbol or a helper symbol of a complete
/// derivative that issued that helper, each exactly once.
#[cfg(feature = "precompiled-programs")]
fn exports_cover(plan: &LinkedEntryPlan, modules: &[LinkedKernelModule]) -> bool {
    let mut expected = Vec::new();
    for entry in plan.linked_entries() {
        let symbol = entry.id().symbol();
        if let (LinkedEntryKind::Derivative, LinkedBinding::Derivative { helpers, .. }) =
            (entry.id().kind, entry.binding())
        {
            for (role, tables) in HELPER_ROLES.iter().zip(helpers.iter()) {
                if tables.is_some() {
                    expected.push(helper_symbol(&symbol, role));
                }
            }
        }
        expected.push(symbol);
    }
    let mut exported = modules
        .iter()
        .flat_map(|module| module.exports.iter().cloned())
        .collect::<Vec<_>>();
    expected.sort_unstable();
    exported.sort_unstable();
    expected == exported
}

/// Staged admission, so that no entry is called before it is certified:
/// `LinkedArtifact::admit` once at load consumes the artifact into a
/// `LinkedAdmission` that owns it and pins the saved product's digest; then
/// `certify_prefix` after each issue and before the
/// receiver binds what it issued; then `finish` with the whole product. No
/// later step takes saved bytes from its caller.
///
/// What this does not prove: that the modules actually linked into the
/// component are the artifact's module bytes. A guest cannot read its own
/// code; that is an inventory property of packaging.
#[cfg(feature = "precompiled-programs")]
pub struct LinkedAdmission {
    artifact: LinkedArtifact,
    saved: String,
    carried: LinkedEntryPlan,
}

#[cfg(feature = "precompiled-programs")]
impl LinkedArtifact {
    /// Whether this artifact was captured with the saved product `saved`.
    pub fn is_bound_to(&self, saved: &[u8]) -> bool {
        self.version == LINKED_ARTIFACT_VERSION && self.saved == linked_digest(saved)
    }

    /// Once, at load, before any entry is bound, with no model source: the
    /// version and saved digest; the entries in the shape their assembler
    /// issues (strict `(kind, index)` order, dense per kind, a kinematic
    /// entry at its derivative's index with its key, bindings of their
    /// entry's kind, kernel bindings naming a carried typed table, no refusal
    /// status); the dispatch module regenerated from
    /// those entries by the crate-private plan builder and equal byte for
    /// byte to the carried one; and the module exports covering exactly the
    /// entries' symbols and their derivatives' helpers. After this the
    /// dispatch routes `(kind, index, key)` only to the symbol this artifact
    /// issued for that position.
    pub fn admit(self, saved: &[u8]) -> Result<LinkedAdmission, WasmCompileError> {
        let saved = linked_digest(saved);
        if self.version != LINKED_ARTIFACT_VERSION || self.saved != saved {
            return Err(refusal("linked artifact belongs to another saved product"));
        }
        // The certifying admission keeps its own records for certify_prefix
        // and finish, so its plan is checked over copies.
        let carried = LinkedEntryPlan::checked(self.entries.clone(), self.typed.clone())
            .map_err(|message| refusal(&message))?;
        if carried.dispatch() != self.dispatch {
            return Err(refusal(
                "linked artifact dispatch differs from the one its entries issue",
            ));
        }
        if !exports_cover(&carried, &self.modules) {
            return Err(refusal(
                "linked artifact module exports differ from its entries' symbols",
            ));
        }
        Ok(LinkedAdmission {
            artifact: self,
            saved,
            carried,
        })
    }

    /// The admission of a build-certified artifact: the version and the
    /// entries in the shape their assembler issues, checked as
    /// [`Self::admit`] checks them, so the carried plan routes only what it
    /// carries. The saved digest, the regenerated dispatch and the module
    /// export coverage were certified with the product before it was
    /// published and are not derived again; the admission can bind by
    /// position but not certify.
    ///
    /// The entries and typed tables move into the carried plan instead of
    /// being copied: a trusted admission is bound by position and never
    /// certifies, so its artifact keeps only the modules and dispatch.
    /// [`LinkedAdmission::certify_prefix`] and [`LinkedAdmission::finish`]
    /// compare against the artifact's own entry and table records, so they
    /// belong to a certifying admission ([`Self::admit`]) and say nothing
    /// about a trusted one.
    pub fn admit_trusted(mut self) -> Result<LinkedAdmission, WasmCompileError> {
        if self.version != LINKED_ARTIFACT_VERSION {
            return Err(refusal("linked artifact belongs to another saved product"));
        }
        let entries = std::mem::take(&mut self.entries);
        let typed = std::mem::take(&mut self.typed);
        let carried =
            LinkedEntryPlan::checked(entries, typed).map_err(|message| refusal(&message))?;
        Ok(LinkedAdmission {
            saved: self.saved.clone(),
            artifact: self,
            carried,
        })
    }
}

#[cfg(feature = "precompiled-programs")]
impl LinkedAdmission {
    /// The admitted entries and typed tables as the plan a build-certified
    /// load binds by position: the same structure the assembler issues,
    /// checked at admission, with no entry issued again.
    pub fn into_carried_plan(self) -> LinkedEntryPlan {
        self.carried
    }

    /// The admitted artifact's modules in link order.
    pub fn modules(&self) -> &[LinkedKernelModule] {
        self.artifact.modules()
    }

    /// The admitted artifact's dispatch module.
    pub fn dispatch_module(&self) -> &[u8] {
        self.artifact.dispatch_module()
    }

    /// Everything `issued` has issued so far equals this artifact's records,
    /// so the receiver may bind and call what it just issued.
    ///
    /// The prefix, exactly: the assembler issues in the receiver's request
    /// order, which interleaves kinds; the artifact keeps entries in
    /// canonical `(kind, index)` order and modules in link order. So
    /// - every issued entry must be the artifact's entry with the same
    ///   `(kind, index)`, equal in key and binding; an issued index the
    ///   artifact lacks is refused, so per kind no more are issued than the
    ///   artifact holds;
    /// - the issued typed tables must be the first typed tables of the
    ///   artifact, position by position: a kernel module's table is issued
    ///   with its module, so a kernel binding's position names the same
    ///   table in both;
    /// - the issued modules must be the first modules of the artifact,
    ///   position by position, bytes and exports equal. Modules are appended
    ///   in issue order by the same assembler that captured the artifact (a
    ///   derivative's own helpers before its control module), so an issued
    ///   entry's modules are the ones its issue appended, at the same
    ///   positions in both lists; no other mapping is used.
    ///
    /// So the whole closure an entry runs is compared at the call that
    /// issues it, before it is usable: a derivative's control module and
    /// every helper module it imports (static and dynamic refresh, values,
    /// kinematic values, as issued), and a private entry's whole shared
    /// module with all of its exports in order.
    pub fn certify_prefix(&self, issued: &LinkedProductAssembler) -> Result<(), WasmCompileError> {
        for entry in issued.plan.linked_entries() {
            let id = entry.id();
            let carried = self
                .artifact
                .entries
                .binary_search_by_key(&id, LinkedEntry::id)
                .ok()
                .map(|position| &self.artifact.entries[position]);
            if carried != Some(entry) {
                return Err(WasmCompileError::Input(format!(
                    "linked entry {} differs from its artifact record",
                    id.symbol()
                )));
            }
        }
        let tables = issued.plan.typed_tables();
        if self.artifact.typed.get(..tables.len()) != Some(tables) {
            return Err(refusal(
                "linked typed fault tables differ from their artifact records",
            ));
        }
        let carried = self
            .artifact
            .modules
            .get(..issued.modules.len())
            .ok_or_else(|| refusal("linked issue exceeds the artifact's modules"))?;
        for (position, (issued, carried)) in issued.modules.iter().zip(carried).enumerate() {
            if issued.bytes != carried.bytes || issued.exports != carried.exports {
                return Err(WasmCompileError::Input(format!(
                    "linked module {position} differs from its artifact record"
                )));
            }
        }
        Ok(())
    }

    /// The whole product the receiver issued, compared with the artifact
    /// under the pinned saved digest: every position's kind, index, key and
    /// binding, every typed table, every module's bytes and exports in link
    /// order, and the
    /// dispatch module, with equal counts (nothing unclaimed, nothing extra).
    /// Equal keys at two positions are compared by position.
    pub fn finish(self, reissued: &LinkedProduct) -> Result<(), WasmCompileError> {
        let artifact = &self.artifact;
        let same_entries = artifact.entries.len() == reissued.plan.linked_entries().count()
            && artifact
                .entries
                .iter()
                .zip(reissued.plan.linked_entries())
                .all(|(carried, issued)| carried == issued);
        let same_modules = artifact.modules.len() == reissued.modules.len()
            && artifact
                .modules
                .iter()
                .zip(&reissued.modules)
                .all(|(carried, issued)| {
                    carried.bytes == issued.bytes && carried.exports == issued.exports
                });
        if artifact.saved != self.saved
            || !same_entries
            || artifact.typed.as_slice() != reissued.plan.typed_tables()
            || !same_modules
            || artifact.dispatch != reissued.plan.dispatch()
        {
            return Err(refusal(
                "linked artifact differs from the product its checked sources issue",
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

    /// What has been issued so far, read only.
    pub fn plan(&self) -> &LinkedEntryPlan {
        &self.plan
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

    /// One kernel entry in its own module, with that module's typed table.
    fn commit_single(
        &mut self,
        id: LinkedEntryId,
        key: [u64; 4],
        program: CompiledNativeCallProgramWasm,
    ) -> Result<LinkedEntryId, WasmCompileError> {
        let typed = self.plan.next_typed()?;
        refuse_reserved_status(program.faults.iter().map(|fault| fault.status))?;
        refuse_reserved_status(entry_statuses(
            &program.gather_faults,
            &program.linear_solve_faults,
        ))?;
        let binding = LinkedBinding::Kernel {
            scratch_bytes: program.scratch_bytes,
            typed,
            gather: program.gather_faults,
            linear: program.linear_solve_faults,
        };
        let module = LinkedKernelModule {
            bytes: program.bytes,
            exports: vec![id.symbol()],
        };
        self.plan.issue_typed(program.faults);
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
    /// A module's typed fault table is issued once; each of its entries names
    /// it and keeps only its own gather and linear tables.
    pub fn private(
        &mut self,
        admission: &crate::PrivateProgramAdmission,
        programs: &[&crate::AdmittedPrivateProgram],
    ) -> Result<Vec<LinkedEntryId>, WasmCompileError> {
        let (layout, table) = admission.source_context();
        let source = PreparedKernelSource::new(layout, table).map_err(WasmCompileError::Input)?;
        let first = self.next(LinkedEntryKind::Private).index;
        let issued = (first..)
            .zip(programs)
            .map(|(index, program)| {
                let (block, seed) = program.source();
                let key = source_key(&source, |key| key.private_program(block, seed))?;
                let id = LinkedEntryId {
                    kind: LinkedEntryKind::Private,
                    index,
                };
                Ok((id, key))
            })
            .collect::<Result<Vec<_>, WasmCompileError>>()?;
        let symbols = issued.iter().map(|(id, _)| id.symbol()).collect::<Vec<_>>();
        let first_table = self.plan.next_typed()?;
        let mut modules = Vec::new();
        let mut typed = Vec::new();
        let mut bindings = Vec::with_capacity(issued.len());
        for (program, entries) in admission.linked_modules(programs, &symbols)? {
            let table = typed_position(first_table, typed.len())?;
            refuse_reserved_status(program.faults.iter().map(|fault| fault.status))?;
            let mut exports = Vec::with_capacity(entries.len());
            for entry in entries {
                refuse_reserved_status(entry_statuses(
                    &entry.gather_faults,
                    &entry.linear_solve_faults,
                ))?;
                bindings.push(LinkedBinding::Kernel {
                    scratch_bytes: program.scratch_bytes,
                    typed: table,
                    gather: entry.gather_faults,
                    linear: entry.linear_solve_faults,
                });
                exports.push(entry.export);
            }
            typed.push(program.faults);
            modules.push(LinkedKernelModule {
                bytes: program.bytes,
                exports,
            });
        }
        if bindings.len() != issued.len() {
            return Err(WasmCompileError::Backend(
                "linked private modules returned other than one entry per program".into(),
            ));
        }
        for table in typed {
            self.plan.issue_typed(table);
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
        let (code, _, _) = crate::emit::emit_event_program_module(program, layout, linkage)
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
