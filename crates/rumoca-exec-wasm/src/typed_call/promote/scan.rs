//! A symbolic walk of the operand stack that classifies every use of an
//! address into the scratch frame.
use super::{Body, EXPANDED_BYTES, Extent, Frame};
use wasm_encoder::{BlockType, Instruction as I, MemArg};

/// One end of a copy or fill.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Side {
    /// A constant offset from the scratch pointer; `addr` is the index of the
    /// first of the three instructions that push the address.
    Scratch { offset: u32, addr: usize },
    /// A constant offset from the input or output pointer.
    Static { base: u32, offset: u32, addr: usize },
    /// Any other address, left on the stack by code the pass keeps.
    Opaque,
}

/// A static access, or a constant-length copy or fill between static
/// addresses that becomes cell moves.
#[derive(Clone, Copy, Debug)]
pub(super) enum Event {
    Load {
        at: usize,
        addr: usize,
        offset: u32,
        real: bool,
    },
    Store {
        at: usize,
        addr: usize,
        offset: u32,
        real: bool,
    },
    Copy {
        at: usize,
        dst: Side,
        src: Side,
        len_at: usize,
        bytes: u32,
    },
    Fill {
        at: usize,
        dst: Side,
        value_at: usize,
        len_at: usize,
        value: u8,
        bytes: u32,
    },
}

/// Scratch bytes `from..to`; `to` is `u32::MAX` when the extent is unknown.
#[derive(Clone, Copy, Debug)]
pub(super) struct Span {
    pub from: u32,
    pub to: u32,
}

/// An instruction that reads or writes scratch cells the pass cannot name
/// individually: an indexed access, a call frame, a narrow access, a copy it
/// does not expand.
#[derive(Clone, Debug)]
pub(super) struct Barrier {
    pub at: usize,
    pub reads: Vec<Span>,
    pub writes: Vec<Span>,
}

pub(super) struct Scan {
    pub events: Vec<Event>,
    pub barriers: Vec<Barrier>,
    /// Cells some address escapes to, or that the caller reads back: they
    /// stay in memory whatever the code does with them.
    pub blocked: Vec<bool>,
}

#[derive(Clone, Copy)]
enum Val {
    /// `local.get` of a parameter (0 input, 1 output, 2 scratch).
    Param(u32, usize),
    Const(i64, usize),
    Scratch(u32, usize),
    /// A scratch address plus an offset the pass cannot evaluate.
    Derived(u32, usize),
    Static(u32, u32, usize),
    Other,
}

struct Frames {
    stacks: Vec<Vec<Val>>,
    results: Vec<usize>,
    dead: Vec<bool>,
}

struct Scanner<'a> {
    body: &'a Body,
    frame: &'a Frame,
    stack: Frames,
    events: Vec<Event>,
    barriers: Vec<Barrier>,
    blocked: Vec<bool>,
    everything: bool,
}

pub(super) fn scan(body: &Body, frame: &Frame) -> Option<Scan> {
    let cells = (frame.scratch_bytes as usize).div_ceil(8);
    let mut scanner = Scanner {
        body,
        frame,
        stack: Frames {
            stacks: vec![Vec::new()],
            results: vec![1],
            dead: vec![false],
        },
        events: Vec::new(),
        barriers: Vec::new(),
        blocked: vec![false; cells],
        everything: false,
    };
    for (at, instruction) in body.code.iter().enumerate() {
        scanner.step(at, instruction)?;
    }
    if !scanner.stack.stacks.is_empty() || scanner.everything {
        return None;
    }
    Some(Scan {
        events: scanner.events,
        barriers: scanner.barriers,
        blocked: scanner.blocked,
    })
}

impl Scanner<'_> {
    fn push(&mut self, value: Val) {
        if let Some(top) = self.stack.stacks.last_mut() {
            top.push(value);
        }
    }

    fn pop(&mut self) -> Option<Val> {
        let dead = *self.stack.dead.last()?;
        match self.stack.stacks.last_mut()?.pop() {
            Some(value) => Some(value),
            None if dead => Some(Val::Other),
            None => None,
        }
    }

    /// The bytes reached from the scratch address that starts at `start`: the
    /// whole allocation the emitter recorded for exactly that address, else
    /// everything above it.
    fn span(&self, offset: u32, start: usize) -> Span {
        let to = self
            .body
            .spans
            .binary_search_by_key(&start, Extent::at)
            .ok()
            .and_then(|found| self.body.spans[found].scratch_end(start, offset))
            .map_or(u32::MAX, |to| to);
        Span { from: offset, to }
    }

    /// The extent of a scratch address value, when it is one.
    fn extent(&self, value: Val) -> Option<Span> {
        match value {
            Val::Scratch(offset, start) | Val::Derived(offset, start) => {
                Some(self.span(offset, start))
            }
            _ => None,
        }
    }

    /// Mark the cells of `span` as reached by something the pass does not
    /// understand.
    fn block(&mut self, span: Span) {
        let first = (span.from / 8) as usize;
        let last = match span.to {
            u32::MAX => self.blocked.len(),
            to => (to as usize).div_ceil(8).min(self.blocked.len()),
        };
        for slot in self.blocked.get_mut(first..last).unwrap_or_default() {
            *slot = true;
        }
    }

    /// A value consumed by an instruction the pass does not model: an address
    /// into the scratch frame escapes, and may be used at any later time.
    fn escape(&mut self, value: Val) {
        if matches!(value, Val::Param(2, _)) {
            self.everything = true;
        } else if let Some(span) = self.extent(value) {
            self.block(span);
        }
    }

    fn consume(&mut self, count: usize) -> Option<()> {
        for _ in 0..count {
            let value = self.pop()?;
            self.escape(value);
        }
        Some(())
    }

    fn side(&self, value: Val) -> Side {
        match value {
            Val::Scratch(offset, addr) => Side::Scratch { offset, addr },
            Val::Static(base, offset, addr) => Side::Static { base, offset, addr },
            _ => Side::Opaque,
        }
    }

    /// Record an access that reaches `span`, now.
    fn reach(&mut self, at: usize, reads: Option<Span>, writes: Option<Span>) {
        self.barriers.push(Barrier {
            at,
            reads: reads.into_iter().collect(),
            writes: writes.into_iter().collect(),
        });
    }

    fn step(&mut self, at: usize, instruction: &I<'static>) -> Option<()> {
        match instruction {
            I::LocalGet(n) if *n <= 2 => self.push(Val::Param(*n, at)),
            I::LocalSet(2) | I::LocalTee(2) => return None,
            I::I32Const(value) => self.push(Val::Const(i64::from(*value), at)),
            I::I32Add => self.add(at)?,
            I::I64Load(m) => self.load(at, m, false)?,
            I::F64Load(m) => self.load(at, m, true)?,
            I::I64Store(m) => self.store(at, m, false)?,
            I::F64Store(m) => self.store(at, m, true)?,
            I::I32Load(m) => self.narrow(at, m, false)?,
            I::I32Store(m) => self.narrow(at, m, true)?,
            I::MemoryCopy { .. } => self.copy(at)?,
            I::MemoryFill(_) => self.fill(at)?,
            I::Block(bt) | I::Loop(bt) => self.open(bt, 0)?,
            I::If(bt) => self.open(bt, 1)?,
            I::Else => {
                self.stack.stacks.last_mut()?.clear();
                *self.stack.dead.last_mut()? = false;
            }
            I::End => self.close()?,
            I::Br(_) | I::Unreachable => *self.stack.dead.last_mut()? = true,
            I::Return => {
                self.consume(1)?;
                *self.stack.dead.last_mut()? = true;
            }
            I::Call(function) => self.call(at, *function)?,
            other => {
                let (pops, pushes) = effect(other)?;
                self.consume(pops)?;
                for _ in 0..pushes {
                    self.push(Val::Other);
                }
            }
        }
        Some(())
    }

    fn call(&mut self, at: usize, function: u32) -> Option<()> {
        let params = self.frame.math_arity.get(&function).copied().unwrap_or(3);
        let mut spans = Vec::new();
        for _ in 0..params {
            let value = self.pop()?;
            if matches!(value, Val::Param(2, _)) {
                self.everything = true;
            }
            spans.extend(self.extent(value));
        }
        if !spans.is_empty() {
            self.barriers.push(Barrier {
                at,
                reads: spans.clone(),
                writes: spans,
            });
        }
        self.push(Val::Other);
        Some(())
    }

    fn add(&mut self, at: usize) -> Option<()> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        match (lhs, rhs) {
            (Val::Param(base, start), Val::Const(offset, constant))
                if constant == start + 1 && at == constant + 1 && offset >= 0 =>
            {
                let offset = u32::try_from(offset).ok()?;
                self.push(if base == 2 {
                    Val::Scratch(offset, start)
                } else {
                    Val::Static(base, offset, start)
                });
            }
            (Val::Scratch(offset, start) | Val::Derived(offset, start), other)
            | (other, Val::Scratch(offset, start) | Val::Derived(offset, start))
                if !matches!(
                    other,
                    Val::Scratch(..) | Val::Derived(..) | Val::Param(2, _)
                ) =>
            {
                self.push(Val::Derived(offset, start));
            }
            _ => {
                self.escape(lhs);
                self.escape(rhs);
                self.push(Val::Other);
            }
        }
        Some(())
    }

    fn load(&mut self, at: usize, memarg: &MemArg, real: bool) -> Option<()> {
        let address = self.pop()?;
        match address {
            Val::Scratch(base, addr) => {
                let offset = base.checked_add(u32::try_from(memarg.offset).ok()?)?;
                self.access(at, offset, |offset| Event::Load {
                    at,
                    addr,
                    offset,
                    real,
                });
            }
            Val::Derived(..) => {
                let span = self.extent(address);
                self.reach(at, span, None);
            }
            other => self.escape(other),
        }
        self.push(Val::Other);
        Some(())
    }

    fn store(&mut self, at: usize, memarg: &MemArg, real: bool) -> Option<()> {
        let value = self.pop()?;
        self.escape(value);
        let address = self.pop()?;
        match address {
            Val::Scratch(base, addr) => {
                let offset = base.checked_add(u32::try_from(memarg.offset).ok()?)?;
                self.access(at, offset, |offset| Event::Store {
                    at,
                    addr,
                    offset,
                    real,
                });
            }
            Val::Derived(..) => {
                let span = self.extent(address);
                self.reach(at, None, span);
            }
            other => self.escape(other),
        }
        Some(())
    }

    /// An eight-byte access at `offset` is an event when it is aligned to a
    /// cell inside the frame; otherwise it reaches the cells it touches.
    fn access(&mut self, at: usize, offset: u32, event: impl FnOnce(u32) -> Event) {
        if offset.is_multiple_of(8) && offset.saturating_add(8) <= self.frame.scratch_bytes {
            self.events.push(event(offset));
        } else {
            let span = Span {
                from: offset,
                to: offset.saturating_add(8),
            };
            self.reach(at, Some(span), Some(span));
        }
    }

    /// A narrower access reaches the cell it touches.
    fn narrow(&mut self, at: usize, memarg: &MemArg, writes: bool) -> Option<()> {
        if writes {
            let value = self.pop()?;
            self.escape(value);
        }
        let address = self.pop()?;
        let reached = match address {
            Val::Scratch(base, _) => {
                let from = base.saturating_add(u32::try_from(memarg.offset).ok()?);
                Some(Span {
                    from,
                    to: from.saturating_add(4),
                })
            }
            Val::Derived(..) => self.extent(address),
            other => {
                self.escape(other);
                None
            }
        };
        self.reach(at, reached.filter(|_| !writes), reached.filter(|_| writes));
        if !writes {
            self.push(Val::Other);
        }
        Some(())
    }

    fn open(&mut self, block: &BlockType, operands: usize) -> Option<()> {
        let results = match block {
            BlockType::Empty => 0,
            BlockType::Result(_) => 1,
            BlockType::FunctionType(_) => return None,
        };
        self.consume(operands)?;
        self.stack.stacks.push(Vec::new());
        self.stack.results.push(results);
        self.stack.dead.push(false);
        Some(())
    }

    fn close(&mut self) -> Option<()> {
        self.stack.stacks.pop()?;
        let results = self.stack.results.pop()?;
        self.stack.dead.pop()?;
        for _ in 0..results {
            self.push(Val::Other);
        }
        Some(())
    }

    fn copy(&mut self, at: usize) -> Option<()> {
        let (len, src, dst) = (self.pop()?, self.pop()?, self.pop()?);
        let (dst_side, src_side) = (self.side(dst), self.side(src));
        let bytes = match len {
            Val::Const(bytes, _) if bytes >= 0 => u32::try_from(bytes).ok(),
            _ => None,
        };
        match (len, bytes) {
            (Val::Const(_, len_at), Some(bytes))
                if bytes <= EXPANDED_BYTES
                    && bytes.is_multiple_of(8)
                    && aligned(dst_side)
                    && aligned(src_side) =>
            {
                self.events.push(Event::Copy {
                    at,
                    dst: dst_side,
                    src: src_side,
                    len_at,
                    bytes,
                });
                // A dynamic end reaches the cells of its span, wherever they are.
                self.reach_dynamic(at, src, dst);
            }
            _ => {
                let (read, write) = (self.copied(src, bytes), self.copied(dst, bytes));
                self.reach(at, read, write);
            }
        }
        Some(())
    }

    /// The bytes one end of a copy of `bytes` bytes (when known) reaches.
    fn copied(&self, end: Val, bytes: Option<u32>) -> Option<Span> {
        match (end, bytes) {
            (Val::Scratch(from, _), Some(bytes)) => Some(Span {
                from,
                to: from.saturating_add(bytes),
            }),
            _ => self.extent(end),
        }
    }

    /// A copy with an indexed end: that end reaches its whole span.
    fn reach_dynamic(&mut self, at: usize, src: Val, dst: Val) {
        let read = self.extent(src).filter(|_| matches!(src, Val::Derived(..)));
        let write = self.extent(dst).filter(|_| matches!(dst, Val::Derived(..)));
        if read.is_some() || write.is_some() {
            self.reach(at, read, write);
        }
    }

    fn fill(&mut self, at: usize) -> Option<()> {
        let (len, value, dst) = (self.pop()?, self.pop()?, self.pop()?);
        let dst_side = self.side(dst);
        match (len, value) {
            (Val::Const(bytes, len_at), Val::Const(byte, value_at))
                if (0..=i64::from(EXPANDED_BYTES)).contains(&bytes)
                    && bytes % 8 == 0
                    && (0..=255).contains(&byte)
                    && !matches!(dst_side, Side::Opaque)
                    && aligned(dst_side) =>
            {
                self.events.push(Event::Fill {
                    at,
                    dst: dst_side,
                    value_at,
                    len_at,
                    value: byte as u8,
                    bytes: bytes as u32,
                });
            }
            _ => {
                let span = self.extent(dst);
                self.reach(at, None, span);
            }
        }
        Some(())
    }
}

/// The operands popped and results pushed by an instruction that touches no
/// address, or `None` for one the pass does not model.
fn effect(instruction: &I<'static>) -> Option<(usize, usize)> {
    Some(match instruction {
        I::Nop => (0, 0),
        I::LocalGet(_) | I::I64Const(_) | I::F64Const(_) | I::MemorySize(_) => (0, 1),
        I::LocalSet(_) | I::Drop => (1, 0),
        I::LocalTee(_)
        | I::I64Eqz
        | I::I32Eqz
        | I::I64ExtendI32U
        | I::I32WrapI64
        | I::F64Neg
        | I::F64Abs
        | I::F64Sqrt
        | I::F64Floor
        | I::F64Ceil
        | I::F64Trunc
        | I::F64ConvertI64S
        | I::I64TruncF64S
        | I::I64ReinterpretF64
        | I::F64ReinterpretI64 => (1, 1),
        I::Select => (3, 1),
        I::BrIf(_) => (1, 0),
        I::I32Mul
        | I::I32And
        | I::I32Or
        | I::I32DivU
        | I::I32RemU
        | I::I32Eq
        | I::I32GeU
        | I::I64Add
        | I::I64Sub
        | I::I64Mul
        | I::I64And
        | I::I64Or
        | I::I64Xor
        | I::I64Eq
        | I::I64Ne
        | I::I64LtS
        | I::I64LtU
        | I::I64GtS
        | I::I64GtU
        | I::I64LeS
        | I::I64GeS
        | I::I64GeU
        | I::I64DivS
        | I::I64RemS
        | I::F64Add
        | I::F64Sub
        | I::F64Mul
        | I::F64Div
        | I::F64Lt
        | I::F64Gt
        | I::F64Le
        | I::F64Ge
        | I::F64Eq
        | I::F64Ne => (2, 1),
        _ => return None,
    })
}

/// Whether an end of a copy starts at a cell.
fn aligned(side: Side) -> bool {
    match side {
        Side::Scratch { offset, .. } | Side::Static { offset, .. } => offset.is_multiple_of(8),
        Side::Opaque => true,
    }
}
