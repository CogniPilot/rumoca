//! Two wasmi stores over one shared memory each: the browser modules called
//! directly, and the linked modules reached only through the dispatch.
use super::product::{BrowserModule, Product, Shape};
use crate::linked::{LINKED_DISPATCH_EXPORT, LinkedEntryId, LinkedEntryKind};
use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use wasmi::{Engine, Func, Linker, Memory, MemoryType, Module, Store, Val, core::F64};

const PAGES: u32 = 64;
pub(super) const TIME: f64 = 0.375;

pub(super) struct Host {
    pub(super) store: Store<()>,
    pub(super) memory: Memory,
    engine: Engine,
}

impl Host {
    fn new() -> Self {
        let engine = Engine::default();
        let mut store = Store::new(&engine, ());
        let memory = Memory::new(&mut store, MemoryType::new(PAGES, None)).unwrap();
        Self {
            store,
            memory,
            engine,
        }
    }

    fn linker(&self) -> Linker<()> {
        let mut linker = Linker::new(&self.engine);
        linker.define("env", "memory", self.memory).unwrap();
        crate::host_runtime::define_math(&mut linker).unwrap();
        linker
    }

    fn instantiate(&mut self, linker: &Linker<()>, bytes: &[u8]) -> wasmi::Instance {
        let module = Module::new(&self.engine, bytes).unwrap();
        linker
            .instantiate(&mut self.store, &module)
            .unwrap()
            .start(&mut self.store)
            .unwrap()
    }

    pub(super) fn image(&self) -> Vec<u8> {
        self.memory.data(&self.store).to_vec()
    }

    fn load(&mut self, image: &[u8]) {
        let data = self.memory.data_mut(&mut self.store);
        data.fill(0);
        data[..image.len()].copy_from_slice(image);
    }
}

/// The browser product: every entry called directly by its browser export.
pub(super) struct Browser {
    pub(super) host: Host,
    entries: BTreeMap<LinkedEntryId, Func>,
}

impl Browser {
    pub(super) fn new(product: &Product) -> Self {
        let mut browser = Self {
            host: Host::new(),
            entries: BTreeMap::new(),
        };
        for module in &product.browser {
            match module {
                BrowserModule::Derivative {
                    helpers,
                    control,
                    entries,
                } => browser.link_derivative(helpers, control, entries),
                BrowserModule::Kernel { bytes, entries } => {
                    let linker = browser.host.linker();
                    let instance = browser.host.instantiate(&linker, bytes);
                    browser.bind(&instance, entries);
                }
            }
        }
        browser
    }

    /// Each helper linked into its own control module by role, as the
    /// browser runtime does.
    fn link_derivative(
        &mut self,
        helpers: &[(&'static str, Vec<u8>)],
        control: &[u8],
        exported: &[(LinkedEntryId, &'static str)],
    ) {
        let mut linker = self.host.linker();
        for (role, bytes) in helpers {
            let base = self.host.linker();
            let instance = self.host.instantiate(&base, bytes);
            let export = if role.ends_with("refresh") {
                "eval_assignments"
            } else {
                "eval_values"
            };
            let function = instance.get_func(&self.host.store, export).unwrap();
            linker.define("operation", role, function).unwrap();
        }
        let instance = self.host.instantiate(&linker, control);
        self.bind(&instance, exported);
    }

    fn bind<S: AsRef<str>>(&mut self, instance: &wasmi::Instance, exported: &[(LinkedEntryId, S)]) {
        for (id, export) in exported {
            let function = instance
                .get_func(&self.host.store, export.as_ref())
                .unwrap();
            self.entries.insert(*id, function);
        }
    }

    pub(super) fn call(&mut self, id: LinkedEntryId, frame: &Frame) -> i32 {
        self.host.load(&frame.image);
        let count = if id.kind == LinkedEntryKind::Derivative {
            9
        } else {
            5
        };
        let inputs = frame.arguments()[..count].to_vec();
        let mut output = [Val::I32(0)];
        self.entries[&id]
            .call(&mut self.host.store, &inputs, &mut output)
            .unwrap();
        let Val::I32(status) = output[0] else {
            panic!("entries return an i32 status")
        };
        status
    }
}

/// The linked product: every module linked by its issued symbols over one
/// memory, and only the dispatch export reached. Every symbol is reached
/// through a host trampoline that records its name, in call order.
pub(super) struct Linked {
    pub(super) host: Host,
    dispatch: Func,
    pub(super) trace: Arc<Mutex<Vec<String>>>,
}

impl Linked {
    pub(super) fn new(product: &Product) -> Self {
        let mut host = Host::new();
        let mut linker = host.linker();
        let trace = Arc::new(Mutex::new(Vec::new()));
        for module in product.linked.modules() {
            let instance = host.instantiate(&linker, module.module_bytes());
            for symbol in module.exports() {
                let function = instance.get_func(&host.store, symbol).unwrap();
                let traced = traced(&mut host.store, function, symbol, &trace);
                // A second module exporting the same symbol is refused here.
                linker.define("env", symbol, traced).unwrap();
            }
        }
        let instance = host.instantiate(&linker, &product.linked.plan().dispatch());
        let dispatch = instance
            .get_func(&host.store, LINKED_DISPATCH_EXPORT)
            .unwrap();
        Self {
            host,
            dispatch,
            trace,
        }
    }

    /// The symbols reached since the last call to this.
    pub(super) fn reached(&self) -> Vec<String> {
        std::mem::take(&mut *self.trace.lock().unwrap())
    }

    /// `kind` and `index` as raw codes, so absent identities can be asked.
    pub(super) fn call_raw(&mut self, kind: u32, index: u32, key: [u64; 4], frame: &Frame) -> i32 {
        self.host.load(&frame.image);
        let mut inputs = vec![Val::I32(kind as i32), Val::I32(index as i32)];
        inputs.extend(key.map(|word| Val::I64(word as i64)));
        inputs.extend(frame.arguments());
        let mut output = [Val::I32(0)];
        self.dispatch
            .call(&mut self.host.store, &inputs, &mut output)
            .unwrap();
        let Val::I32(status) = output[0] else {
            panic!("the dispatch returns an i32 status")
        };
        status
    }

    pub(super) fn call(&mut self, product: &Product, id: LinkedEntryId, frame: &Frame) -> i32 {
        let key = product.linked.plan().key_words(id).unwrap();
        self.call_raw(id.kind.code(), id.index, key, frame)
    }
}

/// `function` behind a host trampoline that records `symbol` when reached.
fn traced(
    store: &mut Store<()>,
    function: Func,
    symbol: &str,
    trace: &Arc<Mutex<Vec<String>>>,
) -> Func {
    let ty = function.ty(&*store);
    let (name, log) = (symbol.to_owned(), Arc::clone(trace));
    Func::new(store, ty, move |mut caller, inputs, outputs| {
        log.lock().unwrap().push(name.clone());
        function.call(&mut caller, inputs, outputs)
    })
}

/// One call's memory image and the superset arguments `a0 a1 t a3 .. a8`.
pub(super) struct Frame {
    pub(super) image: Vec<u8>,
    pub(super) pointers: [u32; 8],
    /// The derivative's `cache_valid` argument, a value 0 or 1.
    pub(super) cache_valid: i32,
}

impl Frame {
    /// Deterministic inputs, or `y` when given, laid out from offset 64.
    pub(super) fn new(shape: Shape, y: Option<&[f64]>) -> Self {
        let (regions, y_len, p_len): (Vec<(usize, u32)>, usize, usize) = match shape {
            Shape::Derivative(workspace) => {
                let regions = workspace.regions().to_vec();
                let cells = |arg: u32| {
                    regions
                        .iter()
                        .find(|(region, _)| *region == arg)
                        .map_or(0, |(_, bytes)| *bytes as usize / 8)
                };
                let (y, p) = (cells(0), cells(1));
                (
                    regions
                        .into_iter()
                        .map(|(arg, bytes)| (arg as usize, bytes))
                        .collect(),
                    y,
                    p,
                )
            }
            Shape::Kernel { y, p, scratch } => (
                vec![(0, (y * 8) as u32), (1, (p * 8) as u32), (3, scratch)],
                y,
                p,
            ),
        };
        let mut pointers = [0u32; 8];
        let mut end = 64usize;
        for (arg, bytes) in &regions {
            pointers[*arg] = end as u32;
            end = (end + *bytes as usize + 15) & !7;
        }
        assert!(
            end <= (PAGES as usize) << 16,
            "the frame fits the test memory"
        );
        let mut image = vec![0u8; end];
        let mut put = |at: u32, index: usize, value: f64| {
            let at = at as usize + index * 8;
            image[at..at + 8].copy_from_slice(&value.to_bits().to_le_bytes());
        };
        for index in 0..y_len {
            let value = y.map_or(0.25 * (index as f64 + 1.0), |y| y[index]);
            put(pointers[0], index, value);
        }
        for index in 0..p_len {
            put(pointers[1], index, 2.0 + index as f64);
        }
        Self {
            image,
            pointers,
            cache_valid: 0,
        }
    }

    fn arguments(&self) -> Vec<Val> {
        let p = |arg: usize| Val::I32(self.pointers[arg] as i32);
        vec![
            p(0),
            p(1),
            Val::F64(F64::from(TIME)),
            p(3),
            p(4),
            p(5),
            p(6),
            p(7),
            Val::I32(self.cache_valid),
        ]
    }
}
