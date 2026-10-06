//! Actual emitted bytes, full buffers and immutable parameter/guard checks.
use super::*;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store, TypedFunc};

type Entry = TypedFunc<(i32, i32, f64, i32, i32), i32>;
pub(super) struct Run {
    pub(super) status: Option<i32>,
    pub(super) output: Vec<u8>,
    pub(super) trap: Option<String>,
    pub(super) parameter_immutable: bool,
    pub(super) guards_unchanged: bool,
}

type Unary = (&'static str, fn(f64) -> f64);

pub(super) struct Execution {
    store: Store<()>,
    memory: Memory,
    call: Entry,
    p: usize,
    scratch: usize,
    end: usize,
    y_bytes: usize,
    p_bytes: usize,
}

impl Execution {
    pub(super) fn new(module: &[u8], scratch_bytes: u32, layout: &solve::VarLayout) -> Self {
        let engine = Engine::default();
        let module = Module::new(&engine, module).unwrap();
        let mut store = Store::new(&engine, ());
        let y_bytes = layout.y_scalars() * 8;
        let p_bytes = layout.p_scalars() * 8;
        let p = y_bytes + 16;
        let scratch = p + p_bytes + 16;
        let end = scratch + scratch_bytes as usize;
        let memory = Memory::new(
            &mut store,
            MemoryType::new((end + 16).div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        linker.define("env", "memory", memory).unwrap();
        bind(&mut linker);
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let call = instance.get_typed_func(&store, "eval_assignments").unwrap();
        Self {
            store,
            memory,
            call,
            p,
            scratch,
            end,
            y_bytes,
            p_bytes,
        }
    }

    pub(super) fn run(&mut self, case: &fixtures::Case) -> Run {
        assert_eq!(case.y.len() * 8, self.y_bytes);
        assert_eq!(case.p.len() * 8, self.p_bytes);
        let y = fixtures::bytes(&case.y);
        let p = fixtures::bytes(&case.p);
        self.memory
            .write(&mut self.store, 0, &vec![0xa5; self.end + 16])
            .unwrap();
        self.memory.write(&mut self.store, 0, &y).unwrap();
        self.memory.write(&mut self.store, self.p, &p).unwrap();
        let status = self.call.call(
            &mut self.store,
            (0, self.p as i32, case.time, self.scratch as i32, 0),
        );
        let bytes = self.memory.data(&self.store);
        let parameter_immutable = bytes[self.p..self.p + self.p_bytes] == p;
        let mut guards_unchanged = true;
        for range in [
            self.y_bytes..self.p,
            self.p + self.p_bytes..self.scratch,
            self.end..self.end + 16,
        ] {
            guards_unchanged &= bytes[range].iter().all(|&cell| cell == 0xa5);
        }
        Run {
            status: status.as_ref().ok().copied(),
            trap: status.err().map(|e| e.to_string()),
            output: bytes[..self.y_bytes].to_vec(),
            parameter_immutable,
            guards_unchanged,
        }
    }
}

fn bind(linker: &mut Linker<()>) {
    let functions: [Unary; 13] = [
        ("abs", f64::abs),
        ("sin", f64::sin),
        ("cos", f64::cos),
        ("tan", f64::tan),
        ("asin", f64::asin),
        ("acos", f64::acos),
        ("atan", f64::atan),
        ("sinh", f64::sinh),
        ("cosh", f64::cosh),
        ("tanh", f64::tanh),
        ("exp", f64::exp),
        ("log", f64::ln),
        ("log10", f64::log10),
    ];
    for (name, function) in functions {
        linker
            .func_wrap("env", name, move |value: f64| function(value))
            .unwrap();
    }
    linker
        .func_wrap("env", "pow", |x: f64, y: f64| x.powf(y))
        .unwrap();
    linker
        .func_wrap("env", "atan2", |x: f64, y: f64| x.atan2(y))
        .unwrap();
}

pub(super) fn canonical(
    model: &solve::SolveModel,
    schedule: &solve::ExactRefreshAssignmentSchedule,
    case: &fixtures::Case,
) -> Result<Vec<u8>, String> {
    let mut y = case.y.clone();
    let owners = &model.problem.continuous.refresh_owners;
    for &id in schedule.program_ids() {
        let owner = owners
            .exact_assignment_program(id)
            .ok_or("issued program absent")?;
        let block = owner
            .final_scalar_program(&model.problem.continuous.implicit_rhs)
            .map_err(|e| e.to_string())?;
        let mut tuple = vec![0.; owner.target_indices().len()];
        rumoca_eval_solve::eval_scalar_program_block_with_context(
            &block,
            &y,
            &case.p,
            case.time,
            rumoca_eval_solve::RowEvalContext {
                pure_calls: Some(&model.pure_calls),
                external_tables: Some(model.external_tables.as_slice()),
                ..Default::default()
            },
            &mut tuple,
        )
        .map_err(|e| e.to_string())?;
        for (&target, value) in owner.target_indices().iter().zip(tuple) {
            y[target] = value;
        }
    }
    Ok(fixtures::bytes(&y))
}

pub(super) fn math_imports(bytes: &[u8]) -> Vec<String> {
    let module = Module::new(&Engine::default(), bytes).unwrap();
    module
        .imports()
        .filter(|import| matches!(import.ty(), wasmi::ExternType::Func(_)))
        .map(|import| {
            assert_eq!(import.module(), "env");
            import.name().to_owned()
        })
        .collect()
}
