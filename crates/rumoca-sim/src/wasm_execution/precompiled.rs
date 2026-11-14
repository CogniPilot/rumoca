//! Build/load scope for complete current-build core-kernel products.
//! Loading never falls through to emitting a missing or foreign kernel.
use rumoca_exec_wasm::{
    AdmittedPrivateProgram, CompiledExactAssignmentWasm, CompiledPrivateProgramWasm,
    CompiledRetainedEventWasm, LinkedEntryKind, LinkedProduct, LinkedProductAssembler,
    PrecompiledExactAssignments, PrecompiledPrivatePrograms, PrecompiledRetainedEvent,
    PrivateProgramAdmission, WasmCompileError,
};
use rumoca_ir_solve::{
    ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule, PreparedKernelSource,
    ScalarProgramBlock, SolvePureCallTable, VarLayout,
};
use serde::{Deserialize, Serialize};
use std::{
    cell::RefCell,
    collections::{BTreeMap, VecDeque},
    rc::Rc,
};

#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CorePrograms {
    private: Vec<PrecompiledPrivatePrograms>,
    exact: Vec<PrecompiledExactAssignments>,
    issued: Vec<PrecompiledExactAssignments>,
    observations: Vec<PrecompiledRetainedEvent>,
    derivatives: Vec<rumoca_exec_wasm::PrecompiledFmiDerivativeOperation>,
    uncompiled: Vec<String>,
}

pub(crate) struct LoadedPrograms {
    private: RefCell<VecDeque<PrecompiledPrivatePrograms>>,
    exact: RefCell<VecDeque<PrecompiledExactAssignments>>,
    issued: RefCell<VecDeque<PrecompiledExactAssignments>>,
    observations: RefCell<VecDeque<PrecompiledRetainedEvent>>,
    derivatives: RefCell<VecDeque<rumoca_exec_wasm::PrecompiledFmiDerivativeOperation>>,
    errors: RefCell<Vec<String>>,
    /// A linked guest's own product, issued at every load site from the same
    /// checked source before that site's saved entry is bound to it.
    #[cfg(all(
        feature = "linked-guest",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    ))]
    linked: RefCell<LinkedProductAssembler>,
}

/// The guest-issued plan and the index of the entry a load site just issued.
#[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
struct Issued<'a> {
    assembler: std::cell::Ref<'a, LinkedProductAssembler>,
    index: u32,
}

#[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
impl Issued<'_> {
    fn at(&self) -> rumoca_exec_wasm::LinkedPosition<'_> {
        rumoca_exec_wasm::LinkedPosition {
            plan: self.assembler.plan(),
            index: self.index,
        }
    }
}

#[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
impl LoadedPrograms {
    /// Issue this site's entries from its checked source; the first issued
    /// position is where its saved entry binds.
    fn issue(
        &self,
        issue: impl FnOnce(
            &mut LinkedProductAssembler,
        ) -> Result<Vec<rumoca_exec_wasm::LinkedEntryId>, WasmCompileError>,
    ) -> Result<Issued<'_>, String> {
        let ids = issue(&mut self.linked.borrow_mut())
            .map_err(|error| format!("linked entry refused: {error}"))?;
        let first = ids.first().ok_or("a load site issued no linked entry")?;
        Ok(Issued {
            assembler: self.linked.borrow(),
            index: first.index,
        })
    }
}

/// A linked capture: the assembler fed the same checked source at every
/// record site, and how many entries of each kind the saved product holds.
#[derive(Default)]
pub(crate) struct LinkedCapture {
    assembler: LinkedProductAssembler,
    saved: BTreeMap<LinkedEntryKind, usize>,
}

#[derive(Clone)]
pub(crate) enum ProgramMode {
    /// The saved product, and, for a linked export, its linked product.
    Capture(
        Rc<RefCell<CorePrograms>>,
        Option<Rc<RefCell<LinkedCapture>>>,
    ),
    Load(Rc<LoadedPrograms>),
}

thread_local! {
    static ACTIVE: RefCell<Option<ProgramMode>> = const { RefCell::new(None) };
}

pub(crate) fn active() -> Option<ProgramMode> {
    ACTIVE.with(|slot| slot.borrow().clone())
}

pub(crate) struct ProgramScope {
    mode: ProgramMode,
    active: bool,
}

impl ProgramScope {
    fn begin(mode: ProgramMode) -> Result<Self, String> {
        ACTIVE.with(|slot| {
            let mut current = slot.borrow_mut();
            if current.is_some() {
                return Err("precompiled kernel scope is reentrant".into());
            }
            *current = Some(mode.clone());
            Ok(Self { mode, active: true })
        })
    }
    pub(crate) fn capture() -> Result<Self, String> {
        Self::begin(ProgramMode::Capture(
            Rc::new(RefCell::new(CorePrograms::default())),
            None,
        ))
    }
    /// A capture that also issues the linked product from the same checked
    /// sources, at the same record sites, in the same order.
    pub(crate) fn capture_linked() -> Result<Self, String> {
        Self::begin(ProgramMode::Capture(
            Rc::new(RefCell::new(CorePrograms::default())),
            Some(Rc::default()),
        ))
    }
    pub(crate) fn load(programs: CorePrograms) -> Result<Self, String> {
        if !programs.uncompiled.is_empty() {
            return Err("precompiled product has uncompiled kernel kinds".into());
        }
        Self::begin(ProgramMode::Load(Rc::new(LoadedPrograms {
            private: RefCell::new(programs.private.into()),
            exact: RefCell::new(programs.exact.into()),
            issued: RefCell::new(programs.issued.into()),
            observations: RefCell::new(programs.observations.into()),
            derivatives: RefCell::new(programs.derivatives.into()),
            errors: RefCell::new(Vec::new()),
            #[cfg(all(
                feature = "linked-guest",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            ))]
            linked: RefCell::default(),
        })))
    }
    pub(crate) fn finish_capture(mut self) -> Result<CorePrograms, String> {
        let ProgramMode::Capture(programs, _) = &self.mode else {
            return Err("not an export scope".into());
        };
        let result = captured(programs)?;
        self.close();
        Ok(result)
    }
    /// The saved product and the linked product issued beside it. Every
    /// uncompiled kind, including a record site the assembler refused, is
    /// named in the refusal; nothing is issued for a partial set.
    pub(crate) fn finish_capture_linked(mut self) -> Result<(CorePrograms, LinkedProduct), String> {
        let ProgramMode::Capture(programs, Some(linked)) = &self.mode else {
            return Err("not a linked export scope".into());
        };
        let result = captured(programs)?;
        let LinkedCapture { assembler, saved } = std::mem::take(&mut *linked.borrow_mut());
        let product = assembler.finish(&[]).map_err(|error| error.to_string())?;
        for (kind, count) in saved {
            let issued = product
                .plan()
                .entries()
                .filter(|(id, _)| id.kind == kind)
                .count();
            if issued != count {
                return Err(format!(
                    "linked product issued {issued} {kind:?} entries for {count} saved"
                ));
            }
        }
        self.close();
        Ok((result, product))
    }
    /// A linked guest admits its saved programs only with the artifact they
    /// were linked from: every entry it issued while loading must equal the
    /// artifact's, which is bound to the `saved` product bytes.
    #[cfg(all(
        feature = "linked-guest",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    ))]
    pub(crate) fn finish_load_linked(
        self,
        saved: &[u8],
        artifact: &rumoca_exec_wasm::LinkedArtifact,
    ) -> Result<(), String> {
        let ProgramMode::Load(programs) = &self.mode else {
            return Err("not a load scope".into());
        };
        let assembler = std::mem::take(&mut *programs.linked.borrow_mut());
        self.finish_load()?;
        let product = assembler.finish(&[]).map_err(|error| error.to_string())?;
        artifact
            .certify(saved, &product)
            .map_err(|error| error.to_string())
    }
    pub(crate) fn finish_load(mut self) -> Result<(), String> {
        let ProgramMode::Load(programs) = &self.mode else {
            return Err("not a load scope".into());
        };
        if !programs.errors.borrow().is_empty() {
            return Err(format!(
                "precompiled load refused kernels: {:?}",
                programs.errors.borrow()
            ));
        }
        if !programs.private.borrow().is_empty()
            || !programs.exact.borrow().is_empty()
            || !programs.issued.borrow().is_empty()
            || !programs.observations.borrow().is_empty()
            || !programs.derivatives.borrow().is_empty()
        {
            return Err("precompiled kernel inventory has unconsumed entries".into());
        }
        self.close();
        Ok(())
    }
    fn close(&mut self) {
        ACTIVE.with(|slot| *slot.borrow_mut() = None);
        self.active = false;
    }
}
/// The captured saved product, refused with every uncompiled kind named.
fn captured(programs: &RefCell<CorePrograms>) -> Result<CorePrograms, String> {
    if !programs.borrow().uncompiled.is_empty() {
        return Err(format!(
            "precompiled export has unsupported kernel kinds: {:?}",
            programs.borrow().uncompiled
        ));
    }
    Ok(programs.borrow().clone())
}

impl Drop for ProgramScope {
    fn drop(&mut self) {
        if self.active {
            self.close();
        }
    }
}

impl ProgramMode {
    pub(crate) fn unsupported(&self, kind: &str) -> bool {
        match self {
            Self::Capture(output, _) => {
                output.borrow_mut().uncompiled.push(kind.into());
                false
            }
            Self::Load(input) => {
                input.errors.borrow_mut().push(kind.into());
                true
            }
        }
    }

    /// In a linked capture, issue a saved entry of `kind` from the same
    /// checked source. A refusal is an uncompiled kind, named, so the export
    /// refuses instead of issuing a partial linked product.
    fn link<T>(
        &self,
        kind: LinkedEntryKind,
        saved: usize,
        issue: impl FnOnce(&mut LinkedProductAssembler) -> Result<T, WasmCompileError>,
    ) {
        let Self::Capture(output, Some(linked)) = self else {
            return;
        };
        let mut linked = linked.borrow_mut();
        *linked.saved.entry(kind).or_default() += saved;
        if let Err(error) = issue(&mut linked.assembler) {
            output
                .borrow_mut()
                .uncompiled
                .push(format!("linked {kind:?} entry refused: {error}"));
        }
    }

    pub(crate) fn observation(
        &self,
        source: rumoca_ir_solve::RetainedEventProgram,
    ) -> Result<CompiledRetainedEventWasm, String> {
        match self {
            Self::Capture(output, _) => {
                let compiled = CompiledRetainedEventWasm::compile(source)
                    .map_err(|error| error.to_string())
                    .inspect_err(|_| {
                        self.unsupported("event observation compilation refused");
                    })?;
                output
                    .borrow_mut()
                    .observations
                    .push(PrecompiledRetainedEvent::from_compiled(&compiled));
                self.link(LinkedEntryKind::Event, 1, |assembler| {
                    assembler.event(compiled.source())
                });
                Ok(compiled)
            }
            Self::Load(input) => {
                let result = input
                    .observations
                    .borrow_mut()
                    .pop_front()
                    .ok_or_else(|| {
                        "precompiled observation inventory exhausted; compilation disabled"
                            .to_owned()
                    })
                    .and_then(|wire| {
                        #[cfg(all(
                            feature = "linked-guest",
                            target_arch = "wasm32",
                            target_os = "wasi",
                            target_env = "p2"
                        ))]
                        let issued =
                            input.issue(|assembler| assembler.event(&source).map(|id| vec![id]))?;
                        wire.bind(
                            source,
                            #[cfg(all(
                                feature = "linked-guest",
                                target_arch = "wasm32",
                                target_os = "wasi",
                                target_env = "p2"
                            ))]
                            issued.at(),
                        )
                        .map_err(|error| error.to_string())
                    });
                if let Err(error) = &result {
                    input.errors.borrow_mut().push(error.clone());
                }
                result
            }
        }
    }

    /// Bind the next saved private batch. A linked guest first issues the
    /// batch's entries from `admission` and `admitted`, the same admitted
    /// programs in the same order as `requests`.
    pub(crate) fn private(
        &self,
        key: &PreparedKernelSource,
        requests: &[(&ScalarProgramBlock, Option<usize>)],
        layout: &VarLayout,
        #[cfg(all(
            feature = "linked-guest",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        (admission, admitted): (&PrivateProgramAdmission, &[&AdmittedPrivateProgram]),
    ) -> Option<Result<Vec<CompiledPrivateProgramWasm>, String>> {
        let Self::Load(programs) = self else {
            return None;
        };
        let wire = programs.private.borrow_mut().pop_front();
        let result = wire
            .ok_or_else(|| {
                "precompiled private kernel inventory is exhausted; compilation is disabled"
                    .to_owned()
            })
            .and_then(|wire| {
                #[cfg(all(
                    feature = "linked-guest",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                ))]
                let issued = programs.issue(|assembler| assembler.private(admission, admitted))?;
                wire.bind(
                    key,
                    requests,
                    layout,
                    #[cfg(all(
                        feature = "linked-guest",
                        target_arch = "wasm32",
                        target_os = "wasi",
                        target_env = "p2"
                    ))]
                    issued.at(),
                )
                .map_err(|error| error.to_string())
            });
        if let Err(error) = &result {
            programs.errors.borrow_mut().push(error.clone());
        }
        Some(result)
    }
    /// Record one compiled private batch; `admitted` is what `admission`
    /// compiled it from, in the same order.
    pub(crate) fn record_private(
        &self,
        admission: &PrivateProgramAdmission,
        admitted: &[&AdmittedPrivateProgram],
        programs: &[CompiledPrivateProgramWasm],
    ) {
        if let Self::Capture(output, _) = self {
            output
                .borrow_mut()
                .private
                .push(PrecompiledPrivatePrograms::from_compiled(programs));
            self.link(LinkedEntryKind::Private, programs.len(), |assembler| {
                assembler.private(admission, admitted)
            });
        }
    }
    pub(crate) fn exact(
        &self,
        source: &ComputeBlock,
        owners: &ContinuousRefreshOwners,
        schedule: &ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
        calls: &SolvePureCallTable,
    ) -> Option<Result<CompiledExactAssignmentWasm, String>> {
        let Self::Load(programs) = self else {
            return None;
        };
        let wire = programs.exact.borrow_mut().pop_front();
        let result = wire
            .ok_or_else(|| {
                "precompiled assignment inventory is exhausted; compilation is disabled".to_owned()
            })
            .and_then(|program| {
                #[cfg(all(
                    feature = "linked-guest",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                ))]
                let issued = programs.issue(|assembler| {
                    assembler
                        .exact(source, owners, schedule, layout, calls)
                        .map(|id| vec![id])
                })?;
                program
                    .bind(
                        source,
                        owners,
                        schedule,
                        layout,
                        calls,
                        #[cfg(all(
                            feature = "linked-guest",
                            target_arch = "wasm32",
                            target_os = "wasi",
                            target_env = "p2"
                        ))]
                        issued.at(),
                    )
                    .map_err(|error| error.to_string())
            });
        if let Err(error) = &result {
            programs.errors.borrow_mut().push(error.clone());
        }
        Some(result)
    }
    /// Record one compiled exact schedule with the checked source it was
    /// compiled from.
    pub(crate) fn record_exact(
        &self,
        source: &ComputeBlock,
        owners: &ContinuousRefreshOwners,
        schedule: &ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
        calls: &SolvePureCallTable,
        program: &CompiledExactAssignmentWasm,
    ) {
        if let Self::Capture(output, _) = self {
            output
                .borrow_mut()
                .exact
                .push(PrecompiledExactAssignments::from_compiled(program));
            self.link(LinkedEntryKind::Exact, 1, |assembler| {
                assembler.exact(source, owners, schedule, layout, calls)
            });
        }
    }
    pub(crate) fn issued(
        &self,
        rows: &rumoca_ir_solve::IssuedAssignmentRows,
        layout: &VarLayout,
        calls: &SolvePureCallTable,
    ) -> Option<Result<CompiledExactAssignmentWasm, String>> {
        let Self::Load(programs) = self else {
            return None;
        };
        let wire = programs.issued.borrow_mut().pop_front();
        let result = wire
            .ok_or_else(|| {
                "precompiled issued assignment inventory is exhausted; compilation is disabled"
                    .to_owned()
            })
            .and_then(|program| {
                #[cfg(all(
                    feature = "linked-guest",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                ))]
                let issued = programs
                    .issue(|assembler| assembler.issued(rows, layout, calls).map(|id| vec![id]))?;
                program
                    .bind_issued(
                        rows,
                        layout,
                        calls,
                        #[cfg(all(
                            feature = "linked-guest",
                            target_arch = "wasm32",
                            target_os = "wasi",
                            target_env = "p2"
                        ))]
                        issued.at(),
                    )
                    .map_err(|error| error.to_string())
            });
        if let Err(error) = &result {
            programs.errors.borrow_mut().push(error.clone());
        }
        Some(result)
    }
    /// Record compiled issued rows with the rows and slot layout they were
    /// compiled from.
    pub(crate) fn record_issued(
        &self,
        rows: &rumoca_ir_solve::IssuedAssignmentRows,
        layout: &VarLayout,
        calls: &SolvePureCallTable,
        program: &CompiledExactAssignmentWasm,
    ) {
        if let Self::Capture(output, _) = self {
            output
                .borrow_mut()
                .issued
                .push(PrecompiledExactAssignments::from_compiled(program));
            self.link(LinkedEntryKind::Issued, 1, |assembler| {
                assembler.issued(rows, layout, calls)
            });
        }
    }
}

#[cfg(all(
    feature = "fmi-derivative-operation",
    any(
        target_os = "unknown",
        all(
            feature = "linked-guest",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        )
    )
))]
impl ProgramMode {
    pub(crate) fn derivative_operation(
        &self,
        operation: &rumoca_ir_solve::fmi::FmiDerivativeCausalOperation<'_>,
    ) -> Result<rumoca_exec_wasm::BoundFmiDerivativeOperation, String> {
        use rumoca_exec_wasm::{
            PrecompiledFmiDerivativeOperation, compile_fmi_derivative_operation_wasm,
        };
        match self {
            Self::Capture(output, _) => {
                let compiled = compile_fmi_derivative_operation_wasm(operation)
                    .map_err(|error| error.to_string())?;
                let wire = PrecompiledFmiDerivativeOperation::from_compiled(&compiled);
                let bound = compiled.bind().map_err(|error| error.to_string())?;
                output.borrow_mut().derivatives.push(wire);
                self.link(LinkedEntryKind::Derivative, 1, |assembler| {
                    assembler.derivative(operation)
                });
                Ok(bound)
            }
            Self::Load(input) => {
                let result = input.derivatives.borrow_mut().pop_front()
                    .ok_or_else(|| "precompiled complete derivative inventory is exhausted; compilation is disabled".to_owned())
                    .and_then(|wire| {
                        #[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
                        let issued = input.issue(|assembler| assembler.derivative(operation))?;
                        wire.bind(
                            operation,
                            #[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
                            issued.at(),
                        )
                        .map_err(|error| error.to_string())
                    });
                if let Err(error) = &result {
                    input.errors.borrow_mut().push(error.clone());
                }
                result
            }
        }
    }
}
