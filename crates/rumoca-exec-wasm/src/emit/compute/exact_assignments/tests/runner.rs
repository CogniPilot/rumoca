use super::*;
use wasmi::{Caller, Engine, Linker, Memory, MemoryType, Module, Store, TypedFunc};

type Entry = TypedFunc<(i32, i32, f64, i32, i32), i32>;

#[derive(Default)]
struct State {
    child: Option<Entry>,
    inside: bool,
    calls: usize,
}

pub(super) struct Runner {
    engine: Engine,
    store: Store<State>,
    public: Memory,
}

impl Runner {
    pub(super) fn new() -> Self {
        let engine = Engine::default();
        let mut store = Store::new(&engine, State::default());
        let public = Memory::new(&mut store, MemoryType::new(1, None)).unwrap();
        Self {
            engine,
            store,
            public,
        }
    }

    pub(super) fn instance(&mut self, artifact: &CompiledNativeCallProgramWasm) -> Entry {
        let module = Module::new(&self.engine, artifact.module_bytes()).unwrap();
        let mut linker = Linker::new(&self.engine);
        linker.define("env", "memory", self.public).unwrap();
        define_math(&mut linker);
        linker
            .instantiate(&mut self.store, &module)
            .unwrap()
            .start(&mut self.store)
            .unwrap()
            .get_typed_func(&self.store, "eval_private")
            .unwrap()
    }

    pub(super) fn run(&mut self, entry: Entry, inputs: [f64; 2]) -> (i32, u64) {
        for (offset, value) in [(0, inputs[0]), (32, inputs[1]), (4096, 77.)] {
            self.public
                .write(&mut self.store, offset, &value.to_le_bytes())
                .unwrap();
        }
        let status = entry.call(&mut self.store, (0, 32, 0., 4096, 0)).unwrap();
        (status, self.bits(4096))
    }

    pub(super) fn inputs(&self) -> [u64; 2] {
        [self.bits(0), self.bits(32)]
    }
    pub(super) fn child_output(&self) -> u64 {
        self.bits(8192)
    }
    pub(super) fn nested_calls(&self) -> usize {
        self.store.data().calls
    }

    pub(super) fn nested(&mut self, child: Entry) {
        self.store.data_mut().child = Some(child);
        for (offset, value) in [(128, 5f64), (160, 7f64)] {
            self.public
                .write(&mut self.store, offset, &value.to_le_bytes())
                .unwrap();
        }
    }

    fn bits(&self, offset: usize) -> u64 {
        u64::from_le_bytes(
            self.public.data(&self.store)[offset..offset + 8]
                .try_into()
                .unwrap(),
        )
    }
}

fn nested_sin(mut caller: Caller<'_, State>, value: f64) -> f64 {
    if !caller.data().inside
        && let Some(child) = caller.data().child
    {
        caller.data_mut().inside = true;
        caller.data_mut().calls += 1;
        assert_eq!(child.call(&mut caller, (128, 160, 0., 8192, 0)).unwrap(), 0);
        caller.data_mut().inside = false;
    }
    value.sin()
}

/// The complete math ABI, as every host defines it; `sin` runs a child entry.
fn define_math(linker: &mut Linker<State>) {
    use crate::emit::MathImport;
    for import in MathImport::ALL {
        let name = import.symbol();
        let binary = move |x: f64, y: f64| crate::math_abi::evaluate(import, x, y);
        let unary = move |x: f64| crate::math_abi::evaluate(import, x, 0.0);
        match import {
            MathImport::Sin => linker.func_wrap("env", name, nested_sin),
            MathImport::Pow | MathImport::Atan2 => linker.func_wrap("env", name, binary),
            _ => linker.func_wrap("env", name, unary),
        }
        .unwrap();
    }
}
