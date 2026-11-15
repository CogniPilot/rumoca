//! Private programs requested while a runtime is prepared share their modules.
//! A precompiled load binds that original complete inventory without emission.
use rumoca_exec_wasm::{AdmittedPrivateProgram, CompiledPrivateProgramWasm};
use rumoca_ir_solve::{ScalarProgramBlock, SolvePureCallTable, VarLayout};
use std::{
    cell::{OnceCell, RefCell},
    rc::{Rc, Weak},
};

pub(super) struct ModuleBatch {
    admission: rumoca_exec_wasm::PrivateProgramAdmission,
    pending: RefCell<Vec<Rc<Entry>>>,
    this: Weak<ModuleBatch>,
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    mode: Option<super::precompiled::ProgramMode>,
    // The whole-model key context, built only when something keys a private
    // entry (a capture, or the builder's certifying load); a build-certified
    // load never builds it.
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    source: OnceCell<Result<rumoca_ir_solve::PreparedKernelSource, String>>,
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    calls: Rc<SolvePureCallTable>,
    // The one layout every product of this preparation checks its inputs
    // against; products share it and never keep a copy of their own.
    layout: Rc<VarLayout>,
    // The one scratch every entry runs on. An entry's scratch spans its whole
    // module, so a buffer per entry grew with the entry count. Entries run one
    // at a time, read nothing a previous call left in the buffer, and their
    // callers copy results out before the next call, so the buffer, grown on
    // demand to the widest entry called, serves all of them.
    scratch: Rc<RefCell<Vec<f64>>>,
}

pub(super) struct Entry {
    admitted: Option<AdmittedPrivateProgram>,
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    source: ScalarProgramBlock,
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    seed: Option<usize>,
    outputs: usize,
    batch: Weak<ModuleBatch>,
    compiled: OnceCell<Result<CompiledPrivateProgramWasm, String>>,
}

impl ModuleBatch {
    pub(super) fn new(layout: VarLayout, calls: Rc<SolvePureCallTable>) -> Rc<Self> {
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        let mode = super::precompiled::active();
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        let source = OnceCell::new();
        // A capture or certifying load refuses an unkeyable source up front.
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        if let Some(mode) = &mode
            && mode.binding() != Some(rumoca_exec_wasm::SavedBinding::Trusted)
            && source
                .get_or_init(|| rumoca_ir_solve::PreparedKernelSource::new(&layout, &calls))
                .is_err()
        {
            mode.unsupported("checked kernel source identity refused");
        }
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        let batch_calls = Rc::clone(&calls);
        let admission = rumoca_exec_wasm::PrivateProgramAdmission::new(layout.clone(), calls);
        Rc::new_cyclic(|this| Self {
            admission,
            pending: RefCell::default(),
            this: this.clone(),
            #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
            mode,
            #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
            source,
            #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
            calls: batch_calls,
            layout: Rc::new(layout),
            scratch: Rc::default(),
        })
    }

    /// The whole-model key context, built on first use.
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    fn key_source(&self) -> Result<&rumoca_ir_solve::PreparedKernelSource, String> {
        self.source
            .get_or_init(|| rumoca_ir_solve::PreparedKernelSource::new(&self.layout, &self.calls))
            .as_ref()
            .map_err(Clone::clone)
    }

    pub(super) fn layout(&self) -> &Rc<VarLayout> {
        &self.layout
    }

    pub(super) fn scratch(&self) -> Rc<RefCell<Vec<f64>>> {
        Rc::clone(&self.scratch)
    }

    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    pub(super) fn mode(&self) -> Option<&super::precompiled::ProgramMode> {
        self.mode.as_ref()
    }

    pub(super) fn request(
        &self,
        block: ScalarProgramBlock,
        seed: Option<usize>,
    ) -> Result<Rc<Entry>, String> {
        let outputs = block.stored_output_count();
        // A build-certified load admits nothing: it binds the saved
        // programs. Only the builder's certifying load admits while loading,
        // to issue its own entries from the admitted programs.
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        let loading = self.mode.as_ref().and_then(super::precompiled::ProgramMode::binding)
            == Some(rumoca_exec_wasm::SavedBinding::Trusted);
        #[cfg(not(all(feature = "precompiled-runtime", target_arch = "wasm32")))]
        let loading = false;
        let admitted = if loading {
            None
        } else {
            let admitted = self.admission.admit(block.clone(), seed);
            #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
            if admitted.is_err()
                && let Some(mode) = &self.mode
            {
                mode.unsupported("private admission refused");
            }
            Some(admitted.map_err(|error| error.to_string())?)
        };
        let entry = Rc::new(Entry {
            admitted,
            outputs,
            batch: self.this.clone(),
            compiled: OnceCell::new(),
            #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
            source: block,
            #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
            seed,
        });
        self.pending.borrow_mut().push(Rc::clone(&entry));
        Ok(entry)
    }

    fn compile_pending(
        &self,
        pending: &[Rc<Entry>],
    ) -> Result<Vec<CompiledPrivateProgramWasm>, String> {
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        if let Some(mode) = &self.mode {
            let source = match mode.binding() {
                Some(rumoca_exec_wasm::SavedBinding::Certify) => Some(self.key_source()?),
                _ => None,
            };
            let requests = pending
                .iter()
                .map(|entry| (&entry.source, entry.seed))
                .collect::<Vec<_>>();
            #[cfg(all(
                feature = "linked-guest",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            ))]
            let admitted = pending
                .iter()
                .map(|entry| entry.admitted.as_ref())
                .collect::<Option<Vec<_>>>();
            if let Some(result) = mode.private(
                source,
                &requests,
                &self.layout,
                #[cfg(all(
                    feature = "linked-guest",
                    target_arch = "wasm32",
                    target_os = "wasi",
                    target_env = "p2"
                ))]
                (&self.admission, admitted.as_deref()),
            ) {
                return result;
            }
        }
        let sources = pending
            .iter()
            .map(|entry| entry.admitted.as_ref())
            .collect::<Option<Vec<_>>>()
            .ok_or("precompiled kernel lost its load owner; compilation is disabled")?;
        let compiled = self
            .admission
            .compile(&sources)
            .map_err(|error| error.to_string())?;
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        if let Some(mode) = &self.mode {
            mode.record_private(&self.admission, &sources, &compiled);
        }
        Ok(compiled)
    }

    pub(super) fn finish(&self) {
        let pending = std::mem::take(&mut *self.pending.borrow_mut());
        if pending.is_empty() {
            return;
        }
        match self.compile_pending(&pending) {
            Ok(compiled) if compiled.len() == pending.len() => {
                for (entry, code) in pending.iter().zip(compiled) {
                    let _ = entry.compiled.set(Ok(code));
                }
            }
            Ok(_) => {
                for entry in &pending {
                    let _ = entry
                        .compiled
                        .set(Err("precompiled private inventory length differs".into()));
                }
            }
            Err(error) => {
                #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
                if let Some(mode) = &self.mode {
                    mode.unsupported("private compilation refused");
                }
                for entry in &pending {
                    let _ = entry.compiled.set(Err(error.clone()));
                }
            }
        }
    }
}

impl Entry {
    pub(super) fn output_count(&self) -> usize {
        self.outputs
    }
    pub(super) fn code(&self) -> Result<&CompiledPrivateProgramWasm, String> {
        if self.compiled.get().is_none() {
            match self.batch.upgrade() {
                Some(batch) => batch.finish(),
                None => {
                    let _ = self
                        .compiled
                        .set(Err("WASM private entry outlived its module batch".into()));
                }
            }
        }
        match self.compiled.get() {
            Some(Ok(code)) => Ok(code),
            Some(Err(error)) => Err(error.clone()),
            None => Err("WASM private entry was not emitted by its module".into()),
        }
    }
}
