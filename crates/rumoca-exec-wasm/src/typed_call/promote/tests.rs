//! Differential tests: a promoted function and the function it came from
//! return the same status and leave the same memory outside the promoted cells.
use super::*;
use crate::typed_call::emit::CELL;
use wasm_encoder::{
    BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection,
    ImportSection, MemoryType, Module, TypeSection,
};
use wasmi::{Engine, Linker, Memory, MemoryType as RuntimeMemoryType, Store};

const INPUT: i32 = 0x100;
const OUTPUT: i32 = 0x400;
const SCRATCH: i32 = 0x800;
const CELLS: u32 = 32;
const RESERVED: u32 = 32;
const HEADER: [(u32, ValType); 4] = [
    (1, ValType::I32),
    (4, ValType::I64),
    (2, ValType::F64),
    (3, ValType::I32),
];
const LOCALS: u32 = 13;

fn frame() -> Frame {
    Frame {
        scratch_bytes: CELLS * 8,
        reserved_bytes: RESERVED,
        locals: LOCALS,
        math_arity: BTreeMap::new(),
    }
}

fn module_of(locals: &[(u32, ValType)], code: &[I<'static>]) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([ValType::I32; 3], [ValType::I32]);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import(
        "env",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        }),
    );
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(0);
    functions.function(0);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("run", ExportKind::Func, 0);
    module.section(&exports);
    let mut function = Function::new(locals.iter().copied());
    for instruction in code {
        function.instruction(instruction);
    }
    let mut section = CodeSection::new();
    section.function(&function);
    section.function(&callee());
    module.section(&section);
    module.finish()
}

/// A callee that reads the first two cells of its input tuple, writes their sum
/// to its output and a constant to the first cell of its scratch.
fn callee() -> Function {
    let mut function = Function::new([]);
    for instruction in [
        I::LocalGet(1),
        I::LocalGet(0),
        I::I64Load(CELL),
        I::LocalGet(0),
        I::I64Load(wasm_encoder::MemArg { offset: 8, ..CELL }),
        I::I64Add,
        I::I64Store(CELL),
        I::LocalGet(2),
        I::I64Const(4242),
        I::I64Store(CELL),
        I::I32Const(0),
        I::End,
    ] {
        function.instruction(&instruction);
    }
    function
}

fn run(bytes: &[u8], memory_image: &[u8]) -> (Option<i32>, Vec<u8>) {
    let engine = Engine::default();
    let module = wasmi::Module::new(&engine, bytes).expect("module validates");
    let mut store = Store::new(&engine, ());
    let memory = Memory::new(&mut store, RuntimeMemoryType::new(1, None)).unwrap();
    memory.write(&mut store, 0, memory_image).unwrap();
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory).unwrap();
    let instance = linker
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    let call = instance
        .get_typed_func::<(i32, i32, i32), i32>(&store, "run")
        .unwrap();
    let status = call.call(&mut store, (INPUT, OUTPUT, SCRATCH)).ok();
    (status, memory.data(&store).to_vec())
}

struct Random(u64);

impl Random {
    fn next(&mut self, bound: u32) -> u32 {
        self.0 = self
            .0
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        ((self.0 >> 33) % u64::from(bound)) as u32
    }
}

/// The allocation of the address pushed at instruction `at`.
fn extent(at: usize, offset: u32, bytes: u32) -> Extent {
    let range = CellRange {
        base: 2,
        offset,
        bytes,
    };
    Extent::of(at, range)
}

fn address(base: u32, offset: u32) -> Vec<I<'static>> {
    vec![I::LocalGet(base), I::I32Const(offset as i32), I::I32Add]
}

struct Generator {
    random: Random,
    code: Vec<I<'static>>,
    spans: Vec<Extent>,
    /// Whether an operation reaches scratch cells by an address the pass cannot
    /// name per cell (indexed, narrow, through a pointer, a call frame).
    reaches: bool,
}

impl Generator {
    fn cell(&mut self) -> u32 {
        self.random.next(CELLS) * 8
    }

    /// Record the allocation of the address pushed next.
    fn span(&mut self, offset: u32, bytes: u32) {
        let range = CellRange {
            base: 2,
            offset,
            bytes,
        };
        self.spans.push(Extent::of(self.code.len(), range));
    }

    fn emit(&mut self, instructions: Vec<I<'static>>) {
        self.code.extend(instructions);
    }

    fn operation(&mut self, depth: u32) {
        match self.random.next(15) {
            0 => {
                let (cell, value) = (self.cell(), self.random.next(1000) as i64 - 500);
                self.emit(address(2, cell));
                self.emit(vec![I::I64Const(value), I::I64Store(CELL)]);
            }
            1 => {
                let cell = self.cell();
                let value =
                    [0.5, -0.0, f64::NAN, f64::INFINITY, 3.25][self.random.next(5) as usize];
                self.emit(address(2, cell));
                self.emit(vec![I::F64Const(value.into()), I::F64Store(CELL)]);
            }
            2 => {
                let (to, from) = (self.cell(), self.cell());
                let real = self.random.next(2) == 0;
                self.emit(address(2, to));
                self.emit(address(2, from));
                self.emit(if real {
                    vec![
                        I::F64Load(CELL),
                        I::F64Const(1.0.into()),
                        I::F64Add,
                        I::F64Store(CELL),
                    ]
                } else {
                    vec![
                        I::I64Load(CELL),
                        I::I64Const(3),
                        I::I64Add,
                        I::I64Store(CELL),
                    ]
                });
            }
            3 => self.copy(),
            4 => {
                let (to, from) = (self.cell(), self.random.next(32) * 8);
                self.emit(address(2, to));
                self.emit(address(0, from));
                self.emit(vec![I::I64Load(CELL), I::I64Store(CELL)]);
            }
            5 => {
                let (to, from) = (self.random.next(32) * 8, self.cell());
                self.emit(address(1, to));
                self.emit(address(2, from));
                self.emit(vec![I::F64Load(CELL), I::F64Store(CELL)]);
            }
            6 => {
                let (cell, count) = (self.cell(), 1 + self.random.next(6));
                let cell = cell.min((CELLS - count) * 8);
                let byte = [0, 0xff, 0x3c][self.random.next(3) as usize];
                self.emit(address(2, cell));
                self.emit(vec![
                    I::I32Const(byte),
                    I::I32Const((count * 8) as i32),
                    I::MemoryFill(0),
                ]);
            }
            7 => self.dynamic(),
            8 => {
                self.reaches = true;
                let cell = self.cell();
                self.emit(address(2, cell));
                let value = self.random.next(100) as i32;
                self.emit(vec![I::I32Const(value), I::I32Store(MemArgNarrow::ARG)]);
            }
            9 if depth < 3 => self.branch(depth),
            10 if depth < 3 => self.repeat(depth),
            11 => {
                self.emit(address(0, 0));
                self.emit(vec![
                    I::I64Load(CELL),
                    I::I64Eqz,
                    I::If(BlockType::Empty),
                    I::I32Const(7),
                    I::Return,
                    I::End,
                ]);
            }
            12 => self.pointer(),
            13 => self.call(),
            _ => self.copy(),
        }
    }

    fn copy(&mut self) {
        let count = 1 + self.random.next(8);
        let limit = (CELLS - count) * 8;
        let (to, from) = (self.cell().min(limit), self.cell().min(limit));
        let (dst, src) = match self.random.next(4) {
            0 => (address(2, to), address(0, from % 200)),
            1 => (address(1, from % 200), address(2, to)),
            _ => (address(2, to), address(2, from)),
        };
        self.emit(dst);
        self.emit(src);
        self.emit(vec![
            I::I32Const((count * 8) as i32),
            I::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            },
        ]);
    }

    /// An indexed access over a run of cells: the run stays in memory.
    fn dynamic(&mut self) {
        self.reaches = true;
        let count = 2 + self.random.next(6);
        let first = self.random.next(CELLS - count) * 8;
        self.span(first, count * 8);
        self.emit(address(2, first));
        let index = self.random.next(count) as i32;
        self.emit(vec![
            I::I32Const(index),
            I::I32Const(8),
            I::I32Mul,
            I::I32Add,
        ]);
        if self.random.next(2) == 0 {
            self.emit(vec![I::I64Const(77), I::I64Store(CELL)]);
        } else {
            let other = self.cell();
            self.emit(address(2, other));
            self.emit(vec![
                I::I64Const(5),
                I::I64Store(CELL),
                I::I64Load(CELL),
                I::Drop,
            ]);
        }
    }

    /// A call whose frames are runs of cells.
    fn call(&mut self) {
        self.reaches = true;
        let (input, output, scratch) = (self.random.next(CELLS - 2) * 8, self.cell(), self.cell());
        for (offset, bytes) in [(input, 16), (output, 8), (scratch, 8)] {
            self.span(offset, bytes);
            self.emit(address(2, offset));
        }
        self.emit(vec![I::Call(1), I::Drop]);
    }

    /// A cell holding the address of a run, read back through the pointer.
    fn pointer(&mut self) {
        self.reaches = true;
        let count = 1 + self.random.next(4);
        let target = self.random.next(CELLS - count) * 8;
        let holder = self.cell();
        self.emit(address(2, holder));
        self.span(target, count * 8);
        self.emit(address(2, target));
        self.emit(vec![I::I64ExtendI32U, I::I64Store(CELL)]);
        self.emit(address(2, holder));
        self.emit(vec![
            I::I32Load(MemArgNarrow::ARG),
            I::I64Load(CELL),
            I::Drop,
        ]);
    }

    fn branch(&mut self, depth: u32) {
        let at = self.random.next(32) * 8;
        self.emit(address(0, at));
        self.emit(vec![I::I64Load(CELL), I::I64Eqz, I::If(BlockType::Empty)]);
        for _ in 0..1 + self.random.next(3) {
            self.operation(depth + 1);
        }
        self.emit(vec![I::Else]);
        for _ in 0..1 + self.random.next(3) {
            self.operation(depth + 1);
        }
        self.emit(vec![I::End]);
    }

    fn repeat(&mut self, depth: u32) {
        self.emit(vec![
            I::I64Const(0),
            I::LocalSet(4),
            I::Block(BlockType::Empty),
            I::Loop(BlockType::Empty),
        ]);
        for _ in 0..1 + self.random.next(4) {
            self.operation(depth + 1);
        }
        self.emit(vec![
            I::LocalGet(4),
            I::I64Const(1),
            I::I64Add,
            I::LocalTee(4),
            I::I64Const(3),
            I::I64LtU,
            I::BrIf(0),
            I::End,
            I::End,
        ]);
    }
}

struct MemArgNarrow;

impl MemArgNarrow {
    const ARG: wasm_encoder::MemArg = wasm_encoder::MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
}

/// A random function: operations, then every scratch cell published to the
/// output buffer so the final cell values are observable.
fn program(seed: u64) -> (Body, bool) {
    let mut generator = Generator {
        random: Random(seed),
        code: Vec::new(),
        spans: Vec::new(),
        reaches: false,
    };
    let count = 4 + generator.random.next(24) as usize;
    for _ in 0..count {
        generator.operation(0);
    }
    for cell in 0..CELLS {
        generator.emit(address(1, 224 + (cell % 4) * 8 + (cell / 4) * 32));
        generator.emit(address(2, cell * 8));
        generator.emit(vec![I::I64Load(CELL), I::I64Store(CELL)]);
    }
    generator.emit(vec![I::I32Const(0), I::End]);
    let body = Body {
        code: generator.code,
        spans: generator.spans,
    };
    (body, generator.reaches)
}

fn memory_image(seed: u64) -> Vec<u8> {
    let mut random = Random(seed ^ 0x9e37_79b9);
    let mut image = vec![0u8; 65536];
    for byte in &mut image[..SCRATCH as usize] {
        *byte = random.next(256) as u8;
    }
    if random.next(3) == 0 {
        image[INPUT as usize..INPUT as usize + 8].fill(0);
    }
    image
}

fn first_difference(left: &[u8], right: &[u8]) -> Option<usize> {
    left.iter().zip(right).position(|(a, b)| a != b)
}

fn compare(seed: u64, body: &Body) -> Option<Promoted> {
    let frame = frame();
    let promoted = promote(body, &frame)?;
    let original = module_of(&HEADER, &body.code);
    let locals = HEADER
        .iter()
        .chain(&promoted.locals)
        .copied()
        .collect::<Vec<_>>();
    let rewritten = module_of(&locals, &promoted.code);
    for variant in 0..3 {
        let image = memory_image(seed * 7 + variant);
        let (status, mut memory) = run(&original, &image);
        let (promoted_status, mut promoted_memory) = run(&rewritten, &image);
        let private = (SCRATCH + RESERVED as i32) as usize..(SCRATCH as u32 + CELLS * 8) as usize;
        memory[private.clone()].fill(0);
        promoted_memory[private].fill(0);
        assert_eq!(status, promoted_status, "status, seed {seed}");
        assert_eq!(
            first_difference(&memory, &promoted_memory),
            None,
            "seed {seed}"
        );
    }
    Some(promoted)
}

/// The scratch offsets of the constant addresses left in `code`.
fn scratch_addresses(code: &[I<'static>]) -> Vec<u32> {
    code.windows(3)
        .filter_map(|window| match window {
            [I::LocalGet(2), I::I32Const(offset), I::I32Add] => Some(*offset as u32),
            _ => None,
        })
        .collect()
}

/// The pass rewrites every program to the same result, and promotes every
/// cell of a program whose addresses are all constants: the only constant
/// scratch addresses left are the leading bytes the caller reads back.
#[test]
fn promoted_random_programs_match_the_originals_and_leave_no_promotable_cell_in_memory() {
    let mut constant_only = 0;
    for seed in 0..1500 {
        let (body, reaches) = program(seed);
        let promoted = compare(seed, &body);
        if !reaches {
            constant_only += 1;
            let promoted = promoted.unwrap_or_else(|| panic!("seed {seed} promotes nothing"));
            assert!(
                scratch_addresses(&promoted.code)
                    .iter()
                    .all(|offset| *offset < RESERVED),
                "seed {seed} leaves a promotable cell in memory"
            );
        }
    }
    assert!(
        constant_only > 0,
        "the generator emits no constant-only program"
    );
}

/// Cell 20 = 5; a call reads it; cell 20 = 9; output = cell 20 + 1.
fn call_then_reuse() -> Body {
    let mut code = Vec::new();
    let mut spans = Vec::new();
    code.extend(address(2, 160));
    code.extend([I::I64Const(5), I::I64Store(CELL)]);
    spans.push(extent(code.len(), 160, 16));
    code.extend(address(2, 160));
    spans.push(extent(code.len(), 176, 8));
    code.extend(address(2, 176));
    spans.push(extent(code.len(), 184, 8));
    code.extend(address(2, 184));
    code.extend([I::Call(1), I::Drop]);
    code.extend(address(2, 160));
    code.extend([I::I64Const(9), I::I64Store(CELL)]);
    code.extend(address(1, 0));
    code.extend(address(2, 160));
    code.extend([
        I::I64Load(CELL),
        I::I64Const(1),
        I::I64Add,
        I::I64Store(CELL),
        I::I32Const(0),
        I::End,
    ]);
    Body { code, spans }
}

/// A cell that is a call argument in one place and a private variable in
/// another keeps its memory web only around the call.
#[test]
fn a_cell_reused_after_a_call_is_a_local_where_it_is_private() {
    let body = call_then_reuse();
    let promoted = promote(&body, &frame()).expect("the private web is promoted");
    let stores = |code: &[I<'static>]| code.iter().filter(|i| matches!(i, I::I64Store(_))).count();
    // The first store feeds the call and stays; the second store and the load
    // after it become local operations.
    assert_eq!(stores(&promoted.code), stores(&body.code) - 1);
    assert!(
        promoted
            .code
            .iter()
            .any(|i| matches!(i, I::LocalSet(l) if *l >= LOCALS))
    );
    let original = module_of(&HEADER, &body.code);
    let locals = HEADER
        .iter()
        .chain(&promoted.locals)
        .copied()
        .collect::<Vec<_>>();
    let rewritten = module_of(&locals, &promoted.code);
    let image = memory_image(1);
    let (a, mut ma) = run(&original, &image);
    let (b, mut mb) = run(&rewritten, &image);
    let private = (SCRATCH as usize)..(SCRATCH as usize + 256);
    ma[private.clone()].fill(0);
    mb[private].fill(0);
    assert_eq!((a, first_difference(&ma, &mb)), (b, None));
}

/// Walks every path of a function, up to the loop passes that can carry a
/// value across an iteration, and reports the first value that crosses from a
/// definition to a use the promotion treats differently.
struct Paths<'a> {
    code: &'a [I<'static>],
    accesses: &'a [webs::Access],
    web_of: &'a [Option<usize>],
    by_at: BTreeMap<usize, Vec<usize>>,
    end_of: Vec<usize>,
    else_of: Vec<Option<usize>>,
    steps: usize,
    exhausted: bool,
}

#[derive(Clone, Copy)]
struct Open {
    start: usize,
    is_loop: bool,
    passes: u32,
}

type Defs = BTreeMap<u32, Vec<usize>>;

const PATH_STEPS: usize = 400_000;
const LOOP_PASSES: u32 = 2;

impl<'a> Paths<'a> {
    fn new(
        code: &'a [I<'static>],
        accesses: &'a [webs::Access],
        web_of: &'a [Option<usize>],
    ) -> Self {
        let mut by_at: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
        for (index, access) in accesses.iter().enumerate() {
            by_at.entry(access.at).or_default().push(index);
        }
        let (mut end_of, mut else_of) = (vec![0; code.len()], vec![None; code.len()]);
        let mut open = Vec::new();
        for (at, instruction) in code.iter().enumerate() {
            match instruction {
                I::Block(_) | I::Loop(_) | I::If(_) => open.push(at),
                I::Else => else_of[*open.last().unwrap()] = Some(at),
                I::End => close_label(&mut open, &mut end_of, at),
                _ => {}
            }
        }
        Self {
            code,
            accesses,
            web_of,
            by_at,
            end_of,
            else_of,
            steps: 0,
            exhausted: false,
        }
    }

    /// Where a branch to the label `depth` outward continues, or `None` when
    /// it leaves the function.
    fn branch(&self, depth: u32, open: &mut Vec<Open>) -> Option<usize> {
        let target = open.len().checked_sub(1 + depth as usize)?;
        let frame = open[target];
        if frame.is_loop {
            if frame.passes >= LOOP_PASSES {
                return None;
            }
            open.truncate(target + 1);
            open[target].passes += 1;
            Some(frame.start + 1)
        } else {
            open.truncate(target);
            Some(self.end_of[frame.start] + 1)
        }
    }

    fn run(&mut self, mut at: usize, mut open: Vec<Open>, mut defs: Defs) -> Result<(), String> {
        loop {
            self.steps += 1;
            if self.steps > PATH_STEPS {
                self.exhausted = true;
                return Ok(());
            }
            let Some(instruction) = self.code.get(at) else {
                return Ok(());
            };
            match instruction {
                I::Block(_) | I::Loop(_) => {
                    let is_loop = matches!(instruction, I::Loop(_));
                    open.push(Open {
                        start: at,
                        is_loop,
                        passes: 0,
                    });
                    at += 1;
                }
                I::If(_) => {
                    let mut then_open = open.clone();
                    let frame = Open {
                        start: at,
                        is_loop: false,
                        passes: 0,
                    };
                    then_open.push(frame);
                    self.run(at + 1, then_open, defs.clone())?;
                    open.push(frame);
                    at = self.else_of[at].map_or(self.end_of[at], |otherwise| otherwise + 1);
                }
                I::Else => at = self.end_of[open.last().expect("else inside if").start],
                I::End if open.pop().is_none() => return Ok(()),
                I::End => at += 1,
                I::Br(depth) => match self.branch(*depth, &mut open) {
                    Some(next) => at = next,
                    None => return Ok(()),
                },
                I::BrIf(depth) => {
                    let mut taken = open.clone();
                    let next = self.branch(*depth, &mut taken);
                    next.map(|next| self.run(next, taken, defs.clone()))
                        .transpose()?;
                    at += 1;
                }
                I::Return | I::Unreachable => return Ok(()),
                _ => {
                    self.visit(at, &mut defs)?;
                    at += 1;
                }
            }
        }
    }

    /// The definition and the use are in one web, or both stay in memory.
    fn same_web(&self, definition: usize, index: usize) -> Result<(), String> {
        if self.web_of[definition] == self.web_of[index] {
            return Ok(());
        }
        let (def, used) = (&self.accesses[definition], &self.accesses[index]);
        Err(format!(
            "the definition at {} of cell {} (web {:?}, barrier {}) reaches the use at {} (web {:?}, barrier {})",
            def.at,
            def.cell,
            self.web_of[definition],
            def.barrier,
            used.at,
            self.web_of[index],
            used.barrier,
        ))
    }

    /// The uses at `at` read the definitions that reach their cell, which must
    /// be in the same web as the use (or both in memory); then the definitions
    /// at `at` take effect.
    fn visit(&self, at: usize, defs: &mut Defs) -> Result<(), String> {
        let Some(here) = self.by_at.get(&at) else {
            return Ok(());
        };
        for &index in here.iter().filter(|index| self.accesses[**index].uses) {
            let cell = self.accesses[index].cell;
            for &definition in defs.get(&cell).into_iter().flatten() {
                self.same_web(definition, index)?;
            }
        }
        for &index in here {
            let access = &self.accesses[index];
            match access.def {
                webs::Def::No => {}
                webs::Def::Strong => {
                    defs.insert(access.cell, vec![index]);
                }
                webs::Def::May => defs.entry(access.cell).or_default().push(index),
            }
        }
        Ok(())
    }
}

/// Whether the walk of every path of `body` found a value that crosses
/// between a promoted web and memory (an address-taken access, or a web the
/// promotion kept in memory). `Ok(false)` when the walk ran out of steps.
fn webs_are_separated_from_address_taken_uses(body: &Body) -> Result<bool, String> {
    let frame = frame();
    let Some(scan) = scan::scan(body, &frame) else {
        return Ok(true);
    };
    let Some(resolution) = webs::resolve(&scan, body, &frame) else {
        return Ok(true);
    };
    let accesses = webs::collect(&scan, &resolution.expanded, &frame);
    let web_of = accesses
        .iter()
        .map(|access| access.key.and_then(|key| resolution.web(&key)))
        .collect::<Vec<_>>();
    // A promoted web holds no access made through an address.
    for web in &resolution.webs {
        for key in &web.keys {
            let member = accesses.iter().find(|access| access.key == Some(*key));
            assert!(member.is_some_and(|access| !access.barrier));
        }
    }
    let mut paths = Paths::new(&body.code, &accesses, &web_of);
    paths.run(0, Vec::new(), Defs::new())?;
    Ok(!paths.exhausted)
}

/// No value flows between a promoted web and an address-taken access on any
/// path: the witness the promotion carries (a web has no barrier member, and
/// the reaching definitions of a use are all in its web) holds on every path
/// of every generated function, not only on the paths the differential run
/// happens to execute.
#[test]
fn no_path_carries_a_value_between_a_promoted_web_and_an_address_taken_access() {
    let mut walked = 0;
    for seed in 0..1500 {
        let (body, _) = program(seed);
        match webs_are_separated_from_address_taken_uses(&body) {
            Ok(complete) => walked += usize::from(complete),
            Err(message) => panic!("seed {seed}: {message}"),
        }
    }
    assert!(
        walked > 1000,
        "only {walked} programs were walked completely"
    );
}

/// The path walk is a real check: promoting the store that feeds a call, which
/// reads the cell through its frame, is reported as a value crossing between a
/// web and memory.
#[test]
fn the_path_walk_rejects_a_web_that_a_call_frame_reads() {
    let body = call_then_reuse();
    let frame = frame();
    let scan = scan::scan(&body, &frame).expect("the function is modelled");
    let expanded = vec![true; scan.events.len()];
    let accesses = webs::collect(&scan, &expanded, &frame);
    let first_store = accesses
        .iter()
        .position(|access| access.key.is_some() && access.def == webs::Def::Strong)
        .expect("the first store is a candidate");
    let mut web_of = vec![None; accesses.len()];
    web_of[first_store] = Some(0);
    let mut paths = Paths::new(&body.code, &accesses, &web_of);
    assert!(paths.run(0, Vec::new(), Defs::new()).is_err());
    web_of[first_store] = None;
    let mut paths = Paths::new(&body.code, &accesses, &web_of);
    assert!(paths.run(0, Vec::new(), Defs::new()).is_ok());
}

/// Close the label opened at the top of `open` at instruction `at`; the end of
/// the function closes none.
fn close_label(open: &mut Vec<usize>, end_of: &mut [usize], at: usize) {
    if let Some(start) = open.pop() {
        end_of[start] = at;
    }
}
