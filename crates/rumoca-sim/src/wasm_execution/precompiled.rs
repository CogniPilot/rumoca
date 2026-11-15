//! Build/load scope for complete current-build core-kernel products.
//! Loading never falls through to emitting a missing or foreign kernel.
use rumoca_exec_wasm::{
    AdmittedPrivateProgram, CompiledExactAssignmentWasm, CompiledPrivateProgramWasm,
    CompiledRetainedEventWasm, LinkedEntryKind, LinkedProduct, LinkedProductAssembler,
    PrecompiledExactAssignments, PrecompiledPrivatePrograms, PrecompiledRetainedEvent,
    PrivateProgramAdmission, SavedBinding, WasmCompileError,
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
    /// How every saved program binds: `Certify` only in the builder's
    /// certifying load, `Trusted` for a product certified when it was built
    /// or cached.
    binding: SavedBinding,
    /// A linked guest's own product and the admission of the artifact it was
    /// linked from; a plain load has none and binds nothing linked.
    #[cfg(all(
        feature = "linked-guest",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    ))]
    linked: Option<LinkedLoad>,
}

/// How a linked guest binds its load sites to the admitted artifact.
#[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
enum LinkedLoad {
    /// Build-time certification: every site issues its entries again from the
    /// same checked sources as the capture, and each issue is certified
    /// against the artifact before its entry is bound. Only the build runs
    /// this; a published product is certified before it is published.
    Certify {
        assembler: RefCell<LinkedProductAssembler>,
        admission: RefCell<Option<rumoca_exec_wasm::LinkedAdmission>>,
    },
    /// The load of a build-certified product: every site reserves its
    /// entries, in the order the build issued them, from the carried plan.
    /// Nothing is issued, emitted or hashed again; each binder still checks
    /// the reserved entry against the saved program it binds.
    Trusted {
        plan: rumoca_exec_wasm::LinkedEntryPlan,
        counts: BTreeMap<LinkedEntryKind, u32>,
        next: RefCell<BTreeMap<LinkedEntryKind, u32>>,
    },
}

/// The plan a load site binds in and the index of its first entry.
#[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
struct Issued<'a> {
    plan: IssuedPlan<'a>,
    index: u32,
}

#[cfg(all(
    feature = "linked-guest",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
enum IssuedPlan<'a> {
    Issued(std::cell::Ref<'a, LinkedProductAssembler>),
    Carried(&'a rumoca_exec_wasm::LinkedEntryPlan),
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
            plan: match &self.plan {
                IssuedPlan::Issued(assembler) => assembler.plan(),
                IssuedPlan::Carried(plan) => plan,
            },
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
    /// The position this site's saved entry binds at. `reserve` names, in
    /// issue order, how many entries of each kind the site issues (a
    /// derivative's kinematic entry shares its index and is not reserved);
    /// the first kind's first index is returned.
    ///
    /// Certify: issue the entries from the checked source, check they are
    /// exactly the reservation, and certify everything issued so far,
    /// including each new entry's whole module closure, against the admitted
    /// artifact before the first position is returned.
    ///
    /// Trusted: reserve the next indices of each kind in the carried plan,
    /// refusing a reservation past the plan's count.
    fn issue(
        &self,
        reserve: &[(LinkedEntryKind, u32)],
        issue: impl FnOnce(
            &mut LinkedProductAssembler,
        ) -> Result<Vec<rumoca_exec_wasm::LinkedEntryId>, WasmCompileError>,
    ) -> Result<Issued<'_>, String> {
        let linked = self
            .linked
            .as_ref()
            .ok_or("a linked guest binds only through its admitted artifact")?;
        let (&(first_kind, _), _) = reserve
            .split_first()
            .ok_or("a load site reserves no linked entry")?;
        match linked {
            LinkedLoad::Certify {
                assembler,
                admission,
            } => {
                let ids = issue(&mut assembler.borrow_mut())
                    .map_err(|error| format!("linked entry refused: {error}"))?;
                let first = ids.first().ok_or("a load site issued no linked entry")?;
                // A kinematic entry sits at its derivative's index and binds
                // through it; it is never reserved on its own.
                let ids = ids
                    .iter()
                    .filter(|id| id.kind != LinkedEntryKind::Kinematic)
                    .collect::<Vec<_>>();
                for &(kind, count) in reserve {
                    let issued = ids.iter().filter(|id| id.kind == kind).count();
                    if u32::try_from(issued).ok() != Some(count) {
                        return Err(format!(
                            "a load site issued {issued} {kind:?} entries, reserved {count}"
                        ));
                    }
                }
                let reserved: usize = reserve.iter().map(|&(_, count)| count as usize).sum();
                if first.kind != first_kind || ids.len() != reserved {
                    return Err("a load site issued outside its reservation".into());
                }
                let assembler = assembler.borrow();
                admission
                    .borrow()
                    .as_ref()
                    .ok_or("the linked load is already finished")?
                    .certify_prefix(&assembler)
                    .map_err(|error| format!("linked entry refused: {error}"))?;
                Ok(Issued {
                    plan: IssuedPlan::Issued(assembler),
                    index: first.index,
                })
            }
            LinkedLoad::Trusted { plan, counts, next } => {
                let mut next = next.borrow_mut();
                let mut first = None;
                for &(kind, count) in reserve {
                    let at = next.entry(kind).or_insert(0);
                    let end = at
                        .checked_add(count)
                        .filter(|end| *end <= counts.get(&kind).copied().unwrap_or(0))
                        .ok_or_else(|| {
                            format!("a load site reserves past the carried {kind:?} entries")
                        })?;
                    if kind == first_kind && first.is_none() {
                        first = Some(*at);
                    }
                    *at = end;
                }
                Ok(Issued {
                    plan: IssuedPlan::Carried(plan),
                    index: first.ok_or("a load site reserves no linked entry")?,
                })
            }
        }
    }
}

/// A linked capture: the assembler fed the same checked source at every
/// record site, and how many entries of each kind the saved product holds.
///
/// Capture is the export half of a precompiled product: only an export, which
/// lowers a model (`lowering`), opens a capture scope. A build without
/// lowering (the compiled guest) only loads, so the capture constructors and
/// their state are unused there; every record site still matches both modes.
#[cfg_attr(not(feature = "lowering"), allow(dead_code))]
#[derive(Default)]
pub(crate) struct LinkedCapture {
    assembler: LinkedProductAssembler,
    saved: BTreeMap<LinkedEntryKind, usize>,
}

#[derive(Clone)]
pub(crate) enum ProgramMode {
    /// The saved product, and, for a linked export, its linked product.
    #[cfg_attr(not(feature = "lowering"), allow(dead_code))]
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
    #[cfg_attr(not(feature = "lowering"), allow(dead_code))]
    pub(crate) fn capture() -> Result<Self, String> {
        Self::begin(ProgramMode::Capture(
            Rc::new(RefCell::new(CorePrograms::default())),
            None,
        ))
    }
    /// A capture that also issues the linked product from the same checked
    /// sources, at the same record sites, in the same order.
    #[cfg_attr(not(feature = "lowering"), allow(dead_code))]
    pub(crate) fn capture_linked() -> Result<Self, String> {
        Self::begin(ProgramMode::Capture(
            Rc::new(RefCell::new(CorePrograms::default())),
            Some(Rc::default()),
        ))
    }
    /// The load of a product certified when it was produced (a cached or
    /// packaged browser build): every saved program binds after its
    /// structural checks, with nothing keyed or emitted again.
    pub(crate) fn load(programs: CorePrograms) -> Result<Self, String> {
        Self::load_with(
            programs,
            SavedBinding::Trusted,
            #[cfg(all(
                feature = "linked-guest",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            ))]
            None,
        )
    }
    /// A linked guest's load: the saved programs, bound only to entries the
    /// guest issues and certifies against the already admitted artifact.
    #[cfg(all(
        feature = "linked-guest",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    ))]
    pub(crate) fn load_linked(
        programs: CorePrograms,
        admission: rumoca_exec_wasm::LinkedAdmission,
        certify: bool,
    ) -> Result<Self, String> {
        let linked = if certify {
            LinkedLoad::Certify {
                assembler: RefCell::default(),
                admission: RefCell::new(Some(admission)),
            }
        } else {
            let plan = admission.into_carried_plan();
            let mut counts = BTreeMap::new();
            for (id, _) in plan.entries() {
                // Kinematic entries bind through their derivative's position.
                if id.kind != LinkedEntryKind::Kinematic {
                    *counts.entry(id.kind).or_insert(0u32) += 1;
                }
            }
            LinkedLoad::Trusted {
                plan,
                counts,
                next: RefCell::default(),
            }
        };
        let binding = if certify {
            SavedBinding::Certify
        } else {
            SavedBinding::Trusted
        };
        Self::load_with(programs, binding, Some(linked))
    }
    fn load_with(
        programs: CorePrograms,
        binding: SavedBinding,
        #[cfg(all(
            feature = "linked-guest",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        linked: Option<LinkedLoad>,
    ) -> Result<Self, String> {
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
            binding,
            #[cfg(all(
                feature = "linked-guest",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            ))]
            linked,
        })))
    }
    #[cfg_attr(not(feature = "lowering"), allow(dead_code))]
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
    #[cfg_attr(not(feature = "lowering"), allow(dead_code))]
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
    /// Close a linked load: every saved program was bound, and the whole
    /// product the guest issued equals the admitted artifact under the saved
    /// digest pinned at admission, with nothing unclaimed.
    #[cfg(all(
        feature = "linked-guest",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    ))]
    pub(crate) fn finish_load_linked(self) -> Result<(), String> {
        let ProgramMode::Load(programs) = &self.mode else {
            return Err("not a load scope".into());
        };
        let linked = programs.linked.as_ref().ok_or("not a linked load scope")?;
        match linked {
            LinkedLoad::Certify {
                assembler,
                admission,
            } => {
                let assembler = std::mem::take(&mut *assembler.borrow_mut());
                let admission = admission
                    .borrow_mut()
                    .take()
                    .ok_or("the linked load is already finished")?;
                self.finish_load()?;
                let product = assembler.finish(&[]).map_err(|error| error.to_string())?;
                admission
                    .finish(&product)
                    .map_err(|error| error.to_string())
            }
            LinkedLoad::Trusted { counts, next, .. } => {
                if *next.borrow() != *counts {
                    return Err("the linked load bound other than every carried entry".into());
                }
                self.finish_load()
            }
        }
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
#[cfg_attr(not(feature = "lowering"), allow(dead_code))]
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
    /// How a load scope binds its saved programs; a capture binds none.
    pub(crate) fn binding(&self) -> Option<SavedBinding> {
        match self {
            Self::Capture(..) => None,
            Self::Load(programs) => Some(programs.binding),
        }
    }

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
                        let issued = input.issue(&[(LinkedEntryKind::Event, 1)], |assembler| {
                            assembler.event(&source).map(|id| vec![id])
                        })?;
                        wire.bind(
                            input.binding,
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

    /// Bind the next saved private batch. `key` is the batch's key context,
    /// which only a certifying load needs. A certifying linked guest first
    /// issues the batch's entries from `admission` and `admitted`, the same
    /// admitted programs in the same order as `requests`; a build-certified
    /// load admits nothing and reserves the batch's positions.
    pub(crate) fn private(
        &self,
        key: Option<&PreparedKernelSource>,
        requests: &[(&ScalarProgramBlock, Option<usize>)],
        layout: &VarLayout,
        #[cfg(all(
            feature = "linked-guest",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        (admission, admitted): (
            &PrivateProgramAdmission,
            Option<&[&AdmittedPrivateProgram]>,
        ),
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
                let count = u32::try_from(requests.len())
                    .map_err(|_| "private batch exceeds the entry index range")?;
                #[cfg(all(
                    feature = "linked-guest",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                ))]
                let issued = programs.issue(&[(LinkedEntryKind::Private, count)], |assembler| {
                    let admitted = admitted.ok_or_else(|| {
                        WasmCompileError::Input(
                            "a certifying linked load lost a private admission".into(),
                        )
                    })?;
                    assembler.private(admission, admitted)
                })?;
                let certify = match programs.binding {
                    SavedBinding::Certify => {
                        Some(key.ok_or("a certifying load lost its private key context")?)
                    }
                    SavedBinding::Trusted => None,
                };
                wire.bind(
                    certify,
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
                let issued = programs.issue(&[(LinkedEntryKind::Exact, 1)], |assembler| {
                    assembler
                        .exact(source, owners, schedule, layout, calls)
                        .map(|id| vec![id])
                })?;
                program
                    .bind(
                        programs.binding,
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
                let issued = programs.issue(&[(LinkedEntryKind::Issued, 1)], |assembler| {
                    assembler.issued(rows, layout, calls).map(|id| vec![id])
                })?;
                program
                    .bind_issued(
                        programs.binding,
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
                        let issued = input.issue(
                            &[(LinkedEntryKind::Derivative, 1)],
                            |assembler| assembler.derivative(operation),
                        )?;
                        wire.bind(
                            input.binding,
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
