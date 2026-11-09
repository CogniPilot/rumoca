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
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    source: Result<rumoca_ir_solve::PreparedKernelSource, String>,
    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    layout: VarLayout,
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
        let source = rumoca_ir_solve::PreparedKernelSource::new(&layout, &calls);
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        let mode = super::precompiled::active();
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        if source.is_err()
            && let Some(mode) = &mode
        {
            mode.unsupported("checked kernel source identity refused");
        }
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
            layout,
        })
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
        #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
        let loading = matches!(self.mode, Some(super::precompiled::ProgramMode::Load(_)));
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
            let source = self.source.as_ref().map_err(Clone::clone)?;
            let requests = pending
                .iter()
                .map(|entry| (&entry.source, entry.seed))
                .collect::<Vec<_>>();
            if let Some(result) = mode.private(source, &requests, &self.layout) {
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
            mode.record_private(&compiled);
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
