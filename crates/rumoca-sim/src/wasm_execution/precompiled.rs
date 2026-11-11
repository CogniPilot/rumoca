//! Build/load scope for complete current-build core-kernel products.
//! Loading never falls through to emitting a missing or foreign kernel.
use rumoca_exec_wasm::{
    CompiledExactAssignmentWasm, CompiledPrivateProgramWasm, CompiledRetainedEventWasm,
    PrecompiledExactAssignments, PrecompiledPrivatePrograms, PrecompiledRetainedEvent,
};
use rumoca_ir_solve::{ComputeBlock, PreparedKernelSource, ScalarProgramBlock, VarLayout};
use serde::{Deserialize, Serialize};
use std::ops::Range;
use std::{cell::RefCell, collections::VecDeque, rc::Rc};

#[derive(Clone, Default, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CorePrograms {
    private: Vec<PrecompiledPrivatePrograms>,
    exact: Vec<PrecompiledExactAssignments>,
    torn: Vec<PrecompiledExactAssignments>,
    observations: Vec<PrecompiledRetainedEvent>,
    uncompiled: Vec<String>,
}

pub(crate) struct LoadedPrograms {
    private: RefCell<VecDeque<PrecompiledPrivatePrograms>>,
    exact: RefCell<VecDeque<PrecompiledExactAssignments>>,
    torn: RefCell<VecDeque<PrecompiledExactAssignments>>,
    observations: RefCell<VecDeque<PrecompiledRetainedEvent>>,
    errors: RefCell<Vec<String>>,
}

#[derive(Clone)]
pub(crate) enum ProgramMode {
    Capture(Rc<RefCell<CorePrograms>>),
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
        Self::begin(ProgramMode::Capture(Rc::new(RefCell::new(
            CorePrograms::default(),
        ))))
    }
    pub(crate) fn load(programs: CorePrograms) -> Result<Self, String> {
        if !programs.uncompiled.is_empty() {
            return Err("precompiled product has uncompiled kernel kinds".into());
        }
        Self::begin(ProgramMode::Load(Rc::new(LoadedPrograms {
            private: RefCell::new(programs.private.into()),
            exact: RefCell::new(programs.exact.into()),
            torn: RefCell::new(programs.torn.into()),
            observations: RefCell::new(programs.observations.into()),
            errors: RefCell::new(Vec::new()),
        })))
    }
    pub(crate) fn finish_capture(mut self) -> Result<CorePrograms, String> {
        let ProgramMode::Capture(programs) = &self.mode else {
            return Err("not an export scope".into());
        };
        if !programs.borrow().uncompiled.is_empty() {
            return Err(format!(
                "precompiled export has unsupported kernel kinds: {:?}",
                programs.borrow().uncompiled
            ));
        }
        let result = programs.borrow().clone();
        self.close();
        Ok(result)
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
            || !programs.torn.borrow().is_empty()
            || !programs.observations.borrow().is_empty()
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
            Self::Capture(output) => {
                output.borrow_mut().uncompiled.push(kind.into());
                false
            }
            Self::Load(input) => {
                input.errors.borrow_mut().push(kind.into());
                true
            }
        }
    }

    pub(crate) fn observation(
        &self,
        source: rumoca_ir_solve::RetainedEventProgram,
    ) -> Result<CompiledRetainedEventWasm, String> {
        match self {
            Self::Capture(output) => {
                let compiled = CompiledRetainedEventWasm::compile(source)
                    .map_err(|error| error.to_string())
                    .inspect_err(|_| {
                        self.unsupported("event observation compilation refused");
                    })?;
                output
                    .borrow_mut()
                    .observations
                    .push(PrecompiledRetainedEvent::from_compiled(&compiled));
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
                    .and_then(|wire| wire.bind(source).map_err(|error| error.to_string()));
                if let Err(error) = &result {
                    input.errors.borrow_mut().push(error.clone());
                }
                result
            }
        }
    }

    pub(crate) fn private(
        &self,
        key: &PreparedKernelSource,
        requests: &[(&ScalarProgramBlock, Option<usize>)],
        layout: &VarLayout,
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
                wire.bind(key, requests, layout)
                    .map_err(|error| error.to_string())
            });
        if let Err(error) = &result {
            programs.errors.borrow_mut().push(error.clone());
        }
        Some(result)
    }
    pub(crate) fn record_private(&self, programs: &[CompiledPrivateProgramWasm]) {
        if let Self::Capture(output) = self {
            output
                .borrow_mut()
                .private
                .push(PrecompiledPrivatePrograms::from_compiled(programs));
        }
    }
    pub(crate) fn exact(
        &self,
        key: &PreparedKernelSource,
        source: &ComputeBlock,
        layout: &VarLayout,
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
                program
                    .bind(key, source, layout)
                    .map_err(|error| error.to_string())
            });
        if let Err(error) = &result {
            programs.errors.borrow_mut().push(error.clone());
        }
        Some(result)
    }
    pub(crate) fn record_exact(&self, program: &CompiledExactAssignmentWasm) {
        if let Self::Capture(output) = self {
            output
                .borrow_mut()
                .exact
                .push(PrecompiledExactAssignments::from_compiled(program));
        }
    }
    pub(crate) fn torn(
        &self,
        key: &PreparedKernelSource,
        sweep: (&ScalarProgramBlock, &[usize], &[Range<usize>]),
        layout: &VarLayout,
    ) -> Option<Result<CompiledExactAssignmentWasm, String>> {
        let Self::Load(programs) = self else {
            return None;
        };
        let wire = programs.torn.borrow_mut().pop_front();
        let result = wire
            .ok_or_else(|| {
                "precompiled torn assignment inventory is exhausted; compilation is disabled"
                    .to_owned()
            })
            .and_then(|program| {
                program
                    .bind_torn(key, sweep, layout)
                    .map_err(|error| error.to_string())
            });
        if let Err(error) = &result {
            programs.errors.borrow_mut().push(error.clone());
        }
        Some(result)
    }
    pub(crate) fn record_torn(&self, program: &CompiledExactAssignmentWasm) {
        if let Self::Capture(output) = self {
            output
                .borrow_mut()
                .torn
                .push(PrecompiledExactAssignments::from_compiled(program));
        }
    }
}
