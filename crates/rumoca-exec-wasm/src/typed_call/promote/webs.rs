//! Which uses of a scratch cell can share one local.
//!
//! A cell is reused for different values over a function, and calls and
//! indexed accesses reach it through memory. The reaching definitions of each
//! cell over the structured control flow link every use to the definitions
//! that can supply its value; the linked accesses of a cell are a web. A web
//! that no barrier (a call frame, an indexed access, an access the pass does
//! not rewrite) reads or writes is a variable, and each such web takes its own
//! local, so a cell that is a call argument in one place is still a local
//! where it is private.
use super::scan::{Barrier, Event, Scan, Side, Span};
use super::{Body, Frame};
use crate::typed_call::cost::{ANALYSIS_ACCESSES, ENGINE_LOCAL_LIMIT};
use std::collections::{BTreeMap, BTreeSet};
use wasm_encoder::Instruction as I;

/// Names one cell access of a static event: a load, a store, or a cell of a
/// copy or fill (`destination` for the cells written).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct Key {
    pub event: usize,
    pub offset: u32,
    pub destination: bool,
}

/// A local, numbered within its kind.
#[derive(Clone, Copy, Debug)]
pub(super) struct Slot {
    pub ordinal: u32,
    pub real: bool,
}

/// A web that takes a local, with the proof that no address-taken use reaches
/// it: the only constructor, [`Web::seal`], refuses a web with a member that
/// reads or writes cells through an address (a barrier), so a `Web` exists
/// only for a web that is a variable and rewriting any of its accesses to the
/// local cannot change what memory an address-taken use reads or leaves.
#[derive(Clone, Debug)]
pub(super) struct Web {
    pub slot: Slot,
    pub keys: Vec<Key>,
}

impl Web {
    /// The web of `members`, or `None` when one of them is a barrier.
    fn seal(slot: Slot, members: &[&Access]) -> Option<Self> {
        let keys = members
            .iter()
            .map(|access| access.key.filter(|_| !access.barrier))
            .collect::<Option<Vec<_>>>()?;
        Some(Self { slot, keys })
    }
}

/// The webs that became locals, and how every event expands.
pub(super) struct Resolution {
    pub webs: Vec<Web>,
    by_key: BTreeMap<Key, usize>,
    pub integers: u32,
    pub reals: u32,
    /// Whether each event becomes cell moves (always true for loads, stores
    /// and fills).
    pub expanded: Vec<bool>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Def {
    No,
    Strong,
    May,
}

pub(super) struct Access {
    pub at: usize,
    pub cell: u32,
    pub uses: bool,
    pub def: Def,
    pub barrier: bool,
    pub real: Option<bool>,
    pub key: Option<Key>,
}

pub(super) fn resolve(scan: &Scan, body: &Body, frame: &Frame) -> Option<Resolution> {
    let structure = Structure::of(&body.code)?;
    let mut expanded = scan
        .events
        .iter()
        .map(|event| {
            !matches!(
                event,
                Event::Copy {
                    dst: Side::Opaque,
                    src: Side::Opaque,
                    ..
                }
            )
        })
        .collect::<Vec<_>>();
    loop {
        let accesses = collect(scan, &expanded, frame);
        if accesses.len() > ANALYSIS_ACCESSES {
            return None;
        }
        let webs = solve(&accesses, &body.code, &structure, frame.locals);
        let mut per_event = vec![Vec::new(); scan.events.len()];
        for (index, access) in accesses.iter().enumerate() {
            if let Some(key) = access.key {
                per_event[key.event].push(index);
            }
        }
        let mut changed = false;
        for (index, event) in scan.events.iter().enumerate() {
            if let Event::Copy {
                dst, src, bytes, ..
            } = event
                && expanded[index]
                && !webs.settled(&per_event[index], (*dst, *src), *bytes)
            {
                expanded[index] = false;
                changed = true;
            }
        }
        if !changed {
            return Some(webs.finish(expanded));
        }
    }
}

/// The cell accesses of every event and barrier, for the cells that are
/// candidates for a local, and the barriers of the copies that stay.
pub(super) fn collect(scan: &Scan, expanded: &[bool], frame: &Frame) -> Vec<Access> {
    let mut accesses = Vec::new();
    let mut kept = Vec::new();
    for (index, event) in scan.events.iter().enumerate() {
        event_accesses(index, event, expanded[index], (&mut accesses, &mut kept));
    }
    let reserved = frame.reserved_bytes.div_ceil(8);
    accesses.retain(|access| {
        access.cell >= reserved
            && !scan
                .blocked
                .get(access.cell as usize)
                .copied()
                .unwrap_or(true)
    });
    let cells = accesses.iter().map(|a| a.cell).collect::<BTreeSet<_>>();
    for barrier in scan.barriers.iter().chain(&kept) {
        reach_cells(&mut accesses, barrier, &cells);
    }
    accesses
}

/// The accesses one event makes; a copy that stays `memory.copy` is a barrier.
fn event_accesses(
    index: usize,
    event: &Event,
    expanded: bool,
    (accesses, kept): (&mut Vec<Access>, &mut Vec<Barrier>),
) {
    match *event {
        Event::Load {
            at, offset, real, ..
        } => accesses.push(access(
            at,
            offset,
            (true, Def::No),
            Some(real),
            index,
            false,
        )),
        Event::Store {
            at, offset, real, ..
        } => accesses.push(access(
            at,
            offset,
            (false, Def::Strong),
            Some(real),
            index,
            true,
        )),
        Event::Copy { dst, src, .. } if expanded && same_cells(dst, src) => {}
        Event::Copy {
            at,
            dst,
            src,
            bytes,
            ..
        } if expanded => {
            accesses.extend(moved(index, at, src, (false, bytes)));
            accesses.extend(moved(index, at, dst, (true, bytes)));
        }
        Event::Copy {
            at,
            dst,
            src,
            bytes,
            ..
        } => kept.push(Barrier {
            at,
            reads: scratch_span(src, bytes).into_iter().collect(),
            writes: scratch_span(dst, bytes).into_iter().collect(),
        }),
        Event::Fill { at, dst, bytes, .. } => accesses.extend(moved(index, at, dst, (true, bytes))),
    }
}

/// The cell reads or writes of one scratch end of a copy or fill.
fn moved(
    event: usize,
    at: usize,
    side: Side,
    (destination, bytes): (bool, u32),
) -> impl Iterator<Item = Access> {
    let first = match side {
        Side::Scratch { offset, .. } => Some(offset),
        _ => None,
    };
    first.into_iter().flat_map(move |first| {
        (0..bytes / 8).map(move |cell| {
            let def = if destination { Def::Strong } else { Def::No };
            access(
                at,
                first + cell * 8,
                (!destination, def),
                None,
                event,
                destination,
            )
        })
    })
}

fn access(
    at: usize,
    offset: u32,
    (uses, def): (bool, Def),
    real: Option<bool>,
    event: usize,
    destination: bool,
) -> Access {
    Access {
        at,
        cell: offset / 8,
        uses,
        def,
        barrier: false,
        real,
        key: Some(Key {
            event,
            offset,
            destination,
        }),
    }
}

fn scratch_span(side: Side, bytes: u32) -> Option<Span> {
    match side {
        Side::Scratch { offset, .. } => Some(Span {
            from: offset,
            to: offset.saturating_add(bytes),
        }),
        _ => None,
    }
}

/// The candidate cells that `spans` reach.
fn reached(spans: &[Span], cells: &BTreeSet<u32>) -> BTreeSet<u32> {
    let mut reached = BTreeSet::new();
    for span in spans {
        let last = match span.to {
            u32::MAX => u32::MAX,
            to => to.div_ceil(8),
        };
        reached.extend(cells.range(span.from / 8..last).copied());
    }
    reached
}

/// One access per candidate cell that `barrier` can read or write.
fn reach_cells(accesses: &mut Vec<Access>, barrier: &Barrier, cells: &BTreeSet<u32>) {
    let reads = reached(&barrier.reads, cells);
    let writes = reached(&barrier.writes, cells);
    accesses.extend(reads.union(&writes).map(|cell| Access {
        at: barrier.at,
        cell: *cell,
        uses: reads.contains(cell),
        def: if writes.contains(cell) {
            Def::May
        } else {
            Def::No
        },
        barrier: true,
        real: None,
        key: None,
    }));
}

/// Matching `end` and `else` instructions of the structured control flow.
struct Structure {
    end_of: Vec<usize>,
    else_of: Vec<Option<usize>>,
}

impl Structure {
    fn of(code: &[I<'static>]) -> Option<Self> {
        let mut end_of = vec![0; code.len()];
        let mut else_of = vec![None; code.len()];
        let mut open: Vec<usize> = Vec::new();
        for (at, instruction) in code.iter().enumerate() {
            match instruction {
                I::Block(_) | I::Loop(_) | I::If(_) => open.push(at),
                I::Else => else_of[*open.last()?] = Some(at),
                I::End => close(&mut open, &mut end_of, at),
                _ => {}
            }
        }
        open.is_empty().then_some(Self { end_of, else_of })
    }
}

#[derive(Clone, PartialEq, Eq)]
struct State {
    bits: Vec<u64>,
    live: bool,
}

impl State {
    fn dead(words: usize) -> Self {
        Self {
            bits: vec![0; words],
            live: false,
        }
    }

    fn merge(&mut self, other: &Self) {
        if !other.live {
            return;
        }
        if !self.live {
            *self = other.clone();
            return;
        }
        for (mine, theirs) in self.bits.iter_mut().zip(&other.bits) {
            *mine |= theirs;
        }
    }

    fn set(&mut self, bit: usize) {
        self.bits[bit / 64] |= 1 << (bit % 64);
    }

    fn clear_range(&mut self, (low, high): (usize, usize)) {
        for bit in low..high {
            self.bits[bit / 64] &= !(1 << (bit % 64));
        }
    }

    fn contains(&self, bit: usize) -> bool {
        self.bits[bit / 64] >> (bit % 64) & 1 == 1
    }
}

struct Dsu(Vec<usize>);

impl Dsu {
    fn find(&mut self, mut node: usize) -> usize {
        while self.0[node] != node {
            self.0[node] = self.0[self.0[node]];
            node = self.0[node];
        }
        node
    }

    fn union(&mut self, a: usize, b: usize) {
        let (a, b) = (self.find(a), self.find(b));
        if a != b {
            self.0[a.max(b)] = a.min(b);
        }
    }
}

struct Label {
    pending: State,
}

struct Walk<'a> {
    code: &'a [I<'static>],
    structure: &'a Structure,
    accesses: &'a [Access],
    /// Access indices sorted by instruction.
    order: Vec<usize>,
    /// The definition number range of the cell of each access.
    ranges: Vec<(usize, usize)>,
    /// The definition number of each access, if it defines.
    def_no: Vec<usize>,
    def_access: Vec<usize>,
    dsu: Dsu,
    labels: Vec<Label>,
    words: usize,
}

pub(super) struct Webs {
    root: Vec<usize>,
    /// The webs that are locals, by their root access.
    sealed: BTreeMap<usize, Web>,
    integers: u32,
    reals: u32,
}

fn solve(accesses: &[Access], code: &[I<'static>], structure: &Structure, locals: u32) -> Webs {
    let mut walk = Walk::new(code, structure, accesses);
    let mut state = State {
        bits: vec![0; walk.words],
        live: true,
    };
    walk.run(0, code.len().saturating_sub(1), &mut state);
    walk.finish(locals)
}

impl<'a> Walk<'a> {
    fn new(code: &'a [I<'static>], structure: &'a Structure, accesses: &'a [Access]) -> Self {
        let mut order = (0..accesses.len()).collect::<Vec<_>>();
        order.sort_by_key(|index| accesses[*index].at);
        let mut by_cell = (0..accesses.len()).collect::<Vec<_>>();
        by_cell.sort_by_key(|index| (accesses[*index].cell, accesses[*index].at));
        let mut def_no = vec![usize::MAX; accesses.len()];
        let mut def_access = Vec::new();
        let mut ranges = vec![(0, 0); accesses.len()];
        for group in by_cell.chunk_by(|a, b| accesses[*a].cell == accesses[*b].cell) {
            let low = def_access.len();
            for index in group.iter().filter(|i| accesses[**i].def != Def::No) {
                def_no[*index] = def_access.len();
                def_access.push(*index);
            }
            for index in group {
                ranges[*index] = (low, def_access.len());
            }
        }
        let words = def_access.len().div_ceil(64).max(1);
        Self {
            code,
            structure,
            accesses,
            order,
            ranges,
            def_no,
            def_access,
            dsu: Dsu((0..accesses.len()).collect()),
            labels: Vec::new(),
            words,
        }
    }

    /// Link the use `index` to the definitions of its cell that reach it.
    fn link(&mut self, index: usize, state: &State) {
        let (low, high) = self.ranges[index];
        for bit in (low..high).filter(|bit| state.contains(*bit)) {
            self.dsu.union(index, self.def_access[bit]);
        }
    }

    /// Process the accesses of instruction `at`: uses link to the definitions
    /// that reach them, then definitions take effect.
    fn visit(&mut self, at: usize, state: &mut State) {
        if !state.live {
            return;
        }
        let start = self
            .order
            .partition_point(|index| self.accesses[*index].at < at);
        let end = self
            .order
            .partition_point(|index| self.accesses[*index].at <= at);
        for position in start..end {
            let index = self.order[position];
            if self.accesses[index].uses {
                self.link(index, state);
            }
        }
        for position in start..end {
            let index = self.order[position];
            match self.accesses[index].def {
                Def::No => {}
                Def::Strong => {
                    state.clear_range(self.ranges[index]);
                    state.set(self.def_no[index]);
                }
                Def::May => state.set(self.def_no[index]),
            }
        }
    }

    fn branch(&mut self, depth: usize, state: &State) {
        let Some(label) = self
            .labels
            .len()
            .checked_sub(1 + depth)
            .and_then(|at| self.labels.get_mut(at))
        else {
            return;
        };
        label.pending.merge(state);
    }

    fn run(&mut self, mut at: usize, end: usize, state: &mut State) {
        while at < end {
            match &self.code[at] {
                I::Block(_) => {
                    let finish = self.structure.end_of[at];
                    self.labels.push(Label {
                        pending: State::dead(self.words),
                    });
                    self.run(at + 1, finish, state);
                    let label = self.labels.pop().expect("label pushed above");
                    state.merge(&label.pending);
                    at = finish + 1;
                }
                I::Loop(_) => {
                    at = self.run_loop(at, state);
                }
                I::If(_) => {
                    at = self.run_if(at, state);
                }
                I::Br(depth) => {
                    self.branch(*depth as usize, state);
                    state.live = false;
                    at += 1;
                }
                I::BrIf(depth) => {
                    self.branch(*depth as usize, state);
                    at += 1;
                }
                I::Return | I::Unreachable => {
                    state.live = false;
                    at += 1;
                }
                _ => {
                    self.visit(at, state);
                    at += 1;
                }
            }
        }
    }

    /// A loop runs until the state at its header stops growing.
    fn run_loop(&mut self, at: usize, state: &mut State) -> usize {
        let finish = self.structure.end_of[at];
        if !state.live {
            return finish + 1;
        }
        let mut header = state.clone();
        loop {
            self.labels.push(Label {
                pending: State::dead(self.words),
            });
            let mut inside = header.clone();
            self.run(at + 1, finish, &mut inside);
            let label = self.labels.pop().expect("label pushed above");
            let mut next = header.clone();
            next.merge(&label.pending);
            *state = inside;
            if next == header {
                break;
            }
            header = next;
        }
        finish + 1
    }

    fn run_if(&mut self, at: usize, state: &mut State) -> usize {
        let finish = self.structure.end_of[at];
        let otherwise = self.structure.else_of[at];
        self.labels.push(Label {
            pending: State::dead(self.words),
        });
        let mut then_state = state.clone();
        self.run(at + 1, otherwise.unwrap_or(finish), &mut then_state);
        if let Some(otherwise) = otherwise {
            self.run(otherwise + 1, finish, state);
        }
        state.merge(&then_state);
        let label = self.labels.pop().expect("label pushed above");
        state.merge(&label.pending);
        finish + 1
    }

    fn finish(mut self, locals: u32) -> Webs {
        let count = self.accesses.len();
        let root = (0..count)
            .map(|index| self.dsu.find(index))
            .collect::<Vec<_>>();
        let mut members: BTreeMap<usize, Vec<&Access>> = BTreeMap::new();
        let mut real: BTreeMap<usize, bool> = BTreeMap::new();
        for (index, access) in self.accesses.iter().enumerate() {
            members.entry(root[index]).or_default().push(access);
            if let Some(kind) = access.real {
                real.entry(root[index]).or_insert(kind);
            }
        }
        let mut sealed = BTreeMap::new();
        let (mut integers, mut reals) = (0u32, 0u32);
        for (&web, members) in &members {
            if locals.saturating_add(integers + reals + 1) >= ENGINE_LOCAL_LIMIT {
                break;
            }
            let kind = real.get(&web).copied().unwrap_or(false);
            let ordinal = if kind { &mut reals } else { &mut integers };
            let slot = Slot {
                ordinal: *ordinal,
                real: kind,
            };
            if let Some(sealed_web) = Web::seal(slot, members) {
                *ordinal += 1;
                sealed.insert(web, sealed_web);
            }
        }
        Webs {
            root,
            sealed,
            integers,
            reals,
        }
    }
}

impl Webs {
    /// Whether a copy keeps its cell moves: with an end of unknown origin, the
    /// other end must be scratch cells that are all promoted, so that the open
    /// end cannot reach them.
    fn settled(&self, accesses: &[usize], (dst, src): (Side, Side), bytes: u32) -> bool {
        let fixed = match (dst, src) {
            (Side::Opaque, Side::Opaque) => return false,
            (Side::Opaque, fixed) | (fixed, Side::Opaque) => fixed,
            _ => return true,
        };
        matches!(fixed, Side::Scratch { .. })
            && accesses.len() == (bytes / 8) as usize
            && accesses
                .iter()
                .all(|index| self.sealed.contains_key(&self.root[*index]))
    }

    fn finish(self, expanded: Vec<bool>) -> Resolution {
        let webs = self.sealed.into_values().collect::<Vec<_>>();
        let by_key = webs
            .iter()
            .enumerate()
            .flat_map(|(index, web)| web.keys.iter().map(move |key| (*key, index)))
            .collect();
        Resolution {
            webs,
            by_key,
            integers: self.integers,
            reals: self.reals,
            expanded,
        }
    }
}

/// Whether two ends name the same bytes: a copy between them moves nothing.
pub(super) fn same_cells(dst: Side, src: Side) -> bool {
    match (dst, src) {
        (Side::Scratch { offset: a, .. }, Side::Scratch { offset: b, .. }) => a == b,
        (
            Side::Static {
                base: x, offset: a, ..
            },
            Side::Static {
                base: y, offset: b, ..
            },
        ) => x == y && a == b,
        _ => false,
    }
}

/// Match the `end` at `at` with the construct it closes; the `end` of the
/// function itself closes none.
fn close(open: &mut Vec<usize>, end_of: &mut [usize], at: usize) {
    if let Some(start) = open.pop() {
        end_of[start] = at;
    }
}

impl Resolution {
    /// The local of the web the access `key` belongs to, when it is promoted.
    pub(super) fn slot(&self, key: &Key) -> Option<Slot> {
        self.web(key).map(|web| self.webs[web].slot)
    }

    /// The index in `webs` of the web the access `key` belongs to.
    pub(super) fn web(&self, key: &Key) -> Option<usize> {
        self.by_key.get(key).copied()
    }
}
