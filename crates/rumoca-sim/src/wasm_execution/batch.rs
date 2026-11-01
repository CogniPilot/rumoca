//! Private programs requested while a runtime is prepared share their modules.
//!
//! Each request meets every refusal of its own when it is made. Emission and
//! instantiation wait until preparation finishes (or the first call of a
//! pending entry), and then build the modules of every pending request.
use rumoca_exec_wasm::CompiledPrivateProgramWasm;
use rumoca_ir_solve::{ScalarProgramBlock, SolvePureCallTable, VarLayout};
use std::{
    cell::{OnceCell, RefCell},
    rc::{Rc, Weak},
};

struct Context {
    layout: VarLayout,
    calls: Rc<SolvePureCallTable>,
}

pub(super) struct ModuleBatch {
    context: Rc<Context>,
    admission: rumoca_exec_wasm::PrivateProgramAdmission,
    pending: RefCell<Vec<Rc<Entry>>>,
    // Self-reference handed to requests, so a pending entry can finish it.
    this: Weak<ModuleBatch>,
}

pub(super) struct Entry {
    block: ScalarProgramBlock,
    seed: Option<usize>,
    context: Rc<Context>,
    batch: Weak<ModuleBatch>,
    compiled: OnceCell<Result<CompiledPrivateProgramWasm, String>>,
}

impl ModuleBatch {
    pub(super) fn new(layout: VarLayout, calls: Rc<SolvePureCallTable>) -> Rc<Self> {
        let admission =
            rumoca_exec_wasm::PrivateProgramAdmission::new(layout.clone(), Rc::clone(&calls));
        Rc::new_cyclic(|this| Self {
            context: Rc::new(Context { layout, calls }),
            admission,
            pending: RefCell::default(),
            this: this.clone(),
        })
    }

    /// Admit one complete program; `Some(seed_len)` requests a directional entry.
    pub(super) fn request(
        &self,
        block: ScalarProgramBlock,
        seed: Option<usize>,
    ) -> Result<Rc<Entry>, String> {
        self.admission
            .admit(&block, seed)
            .map_err(|error| error.to_string())?;
        let entry = Rc::new(Entry {
            block,
            seed,
            context: Rc::clone(&self.context),
            batch: self.this.clone(),
            compiled: OnceCell::new(),
        });
        self.pending.borrow_mut().push(Rc::clone(&entry));
        Ok(entry)
    }

    /// Emit and instantiate every pending request in shared modules.
    pub(super) fn finish(&self) {
        let pending = std::mem::take(&mut *self.pending.borrow_mut());
        if pending.is_empty() {
            return;
        }
        let sources = pending
            .iter()
            .map(|entry| (&entry.block, entry.seed))
            .collect::<Vec<_>>();
        match rumoca_exec_wasm::compile_private_programs_wasm(
            &sources,
            &self.context.layout,
            &self.context.calls,
        ) {
            Ok(compiled) => {
                for (entry, code) in pending.iter().zip(compiled) {
                    let _ = entry.compiled.set(Ok(code));
                }
            }
            // Each request met its own refusals when it was admitted, so a
            // failed build is the engine's (a host limit); every entry reports
            // it. Rebuilding them one module each would multiply what failed.
            Err(error) => {
                let error = error.to_string();
                for entry in &pending {
                    let _ = entry.compiled.set(Err(error.clone()));
                }
            }
        }
    }
}

impl Entry {
    pub(super) fn output_count(&self) -> usize {
        self.block.stored_output_count()
    }

    /// The compiled entry, finishing its pending module on first use.
    pub(super) fn code(&self) -> Result<&CompiledPrivateProgramWasm, String> {
        if self.compiled.get().is_none() {
            match self.batch.upgrade() {
                Some(batch) => batch.finish(),
                None => self.compile_alone(),
            }
        }
        match self.compiled.get() {
            Some(Ok(code)) => Ok(code),
            Some(Err(error)) => Err(error.clone()),
            None => Err("WASM private entry was not emitted by its module".into()),
        }
    }

    fn compile_alone(&self) {
        let compiled = rumoca_exec_wasm::compile_private_programs_wasm(
            &[(&self.block, self.seed)],
            &self.context.layout,
            &self.context.calls,
        )
        .map_err(|error| error.to_string())
        .and_then(|mut codes| {
            codes
                .pop()
                .ok_or_else(|| "WASM private entry is absent".into())
        });
        let _ = self.compiled.set(compiled);
    }
}
