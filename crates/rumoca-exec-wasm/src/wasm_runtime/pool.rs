//! Weakly retained pool; distinct live kernels never share private addresses.
use crate::WasmCompileError;
use crate::private_arena::{ArenaPoolPlan, ArenaRegion};
use js_sys::{Object, Reflect, WebAssembly};
use std::cell::RefCell;
use std::rc::{Rc, Weak};
use wasm_bindgen::{JsCast, JsValue};

thread_local! {
    static CURRENT: RefCell<Weak<Pool>> = const { RefCell::new(Weak::new()) };
}

struct Pool {
    memory: WebAssembly::Memory,
    allocation: RefCell<Allocation>,
}

struct Allocation {
    plan: ArenaPoolPlan,
    pages: u32,
}

pub(super) struct ArenaLease {
    pool: Rc<Pool>,
    region: ArenaRegion,
}

impl ArenaLease {
    pub(super) fn allocate(bytes: u32) -> Result<Self, WasmCompileError> {
        let pool = CURRENT.with(|current| {
            let mut current = current.borrow_mut();
            if let Some(pool) = current.upgrade() {
                return Ok(pool);
            }
            let pool = Rc::new(Pool::new()?);
            *current = Rc::downgrade(&pool);
            Ok::<_, WasmCompileError>(pool)
        })?;
        let region = pool.reserve(bytes)?;
        Ok(Self { pool, region })
    }

    pub(super) fn install(&self, env: &Object) -> Result<(), WasmCompileError> {
        set(env, "rumoca_private_arena", self.pool.memory.as_ref())?;
        let descriptor = Object::new();
        set(&descriptor, "value", &JsValue::from_str("i32"))?;
        set(&descriptor, "mutable", &JsValue::FALSE)?;
        let base =
            WebAssembly::Global::new(&descriptor, &JsValue::from_f64(f64::from(self.region.base)))
                .map_err(|error| backend("private arena base creation", error))?;
        set(env, "rumoca_private_arena_base", base.as_ref())
    }
}

impl Pool {
    fn new() -> Result<Self, WasmCompileError> {
        let descriptor = Object::new();
        set(&descriptor, "initial", &JsValue::from_f64(1.0))?;
        set(&descriptor, "maximum", &JsValue::from_f64(65536.0))?;
        set(&descriptor, "shared", &JsValue::FALSE)?;
        let memory = WebAssembly::Memory::new(&descriptor)
            .map_err(|error| backend("private arena allocation", error))?;
        Ok(Self {
            memory,
            allocation: RefCell::new(Allocation {
                plan: ArenaPoolPlan::default(),
                pages: 1,
            }),
        })
    }

    fn reserve(&self, bytes: u32) -> Result<ArenaRegion, WasmCompileError> {
        let mut allocation = self.allocation.try_borrow_mut().map_err(|_| {
            WasmCompileError::Backend("private arena allocation is reentrant".into())
        })?;
        let region = allocation
            .plan
            .prepare(bytes)
            .map_err(WasmCompileError::Backend)?;
        let pages = u32::try_from(region.end / 65536)
            .map_err(|_| WasmCompileError::Backend("private arena extent exceeds wasm32".into()))?;
        if pages > allocation.pages {
            self.grow(pages - allocation.pages)?;
            allocation.pages = pages;
        }
        allocation
            .plan
            .commit(region)
            .map_err(WasmCompileError::Backend)?;
        Ok(region)
    }

    fn grow(&self, delta: u32) -> Result<(), WasmCompileError> {
        // js-sys Memory::grow has no catch binding; keep allocation failure a
        // checked pre-admission Result instead of an escaping JS exception.
        let function = Reflect::get(self.memory.as_ref(), &JsValue::from_str("grow"))
            .map_err(|error| backend("private arena grow lookup", error))?
            .dyn_into::<js_sys::Function>()
            .map_err(|_| WasmCompileError::Backend("private memory grow is not callable".into()))?;
        function
            .call1(self.memory.as_ref(), &JsValue::from_f64(f64::from(delta)))
            .map(|_| ())
            .map_err(|error| backend("private arena growth", error))
    }
}

fn set(object: &Object, name: &str, value: &JsValue) -> Result<(), WasmCompileError> {
    Reflect::set(object, &JsValue::from_str(name), value)
        .map(|_| ())
        .map_err(|error| backend("private arena import", error))
}

fn backend(operation: &str, error: JsValue) -> WasmCompileError {
    WasmCompileError::Backend(format!("{operation} failed: {error:?}"))
}
