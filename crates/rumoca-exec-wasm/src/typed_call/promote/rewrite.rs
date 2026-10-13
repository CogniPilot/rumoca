//! The rewrite of the accesses of promoted webs into local operations and of
//! static copies and fills into cell moves.
use super::scan::{Event, Scan, Side};
use super::webs::{Key, Resolution, Slot, same_cells};
use super::{Body, Frame, Promoted};
use crate::typed_call::emit::CELL;
use wasm_encoder::{Instruction as I, ValType};

enum Edit {
    Keep,
    Drop,
    Replace(Vec<I<'static>>),
}

/// Where one cell of a copy end lives.
enum Place {
    Local(u32, bool),
    Memory(u32, u32),
    Open(u32),
}

pub(super) struct Rewrite<'a> {
    resolution: &'a Resolution,
    /// The first local the rewrite declares: integers, then reals, then one
    /// temporary for an open copy end.
    base: u32,
}

pub(super) fn apply(
    body: &Body,
    scan: &Scan,
    resolution: &Resolution,
    frame: &Frame,
) -> Option<Promoted> {
    if resolution.webs.is_empty() && !resolution.expanded.iter().any(|expanded| *expanded) {
        return None;
    }
    let rewrite = Rewrite {
        resolution,
        base: frame.locals,
    };
    let mut edits = body.code.iter().map(|_| Edit::Keep).collect::<Vec<_>>();
    for (index, event) in scan.events.iter().enumerate() {
        rewrite.edit(&mut edits, index, *event);
    }
    let mut code = Vec::with_capacity(body.code.len());
    for (instruction, edit) in body.code.iter().zip(edits) {
        match edit {
            Edit::Keep => code.push(instruction.clone()),
            Edit::Drop => {}
            Edit::Replace(replacement) => code.extend(replacement),
        }
    }
    let mut locals = Vec::new();
    for (count, kind) in [
        (resolution.integers, ValType::I64),
        (resolution.reals, ValType::F64),
        (1, ValType::I32),
    ] {
        if count > 0 {
            locals.push((count, kind));
        }
    }
    Some(Promoted { code, locals })
}

impl Rewrite<'_> {
    fn local(&self, slot: Slot) -> u32 {
        if slot.real {
            self.base + self.resolution.integers + slot.ordinal
        } else {
            self.base + slot.ordinal
        }
    }

    fn temp(&self) -> u32 {
        self.base + self.resolution.integers + self.resolution.reals
    }

    fn slot(&self, event: usize, offset: u32, destination: bool) -> Option<Slot> {
        self.resolution.slot(&Key {
            event,
            offset,
            destination,
        })
    }

    fn edit(&self, edits: &mut [Edit], index: usize, event: Event) {
        match event {
            Event::Load {
                at,
                addr,
                offset,
                real,
            } => {
                if let Some(slot) = self.slot(index, offset, false) {
                    drop_range(edits, addr, 3);
                    let mut read = vec![I::LocalGet(self.local(slot))];
                    read.extend(convert(slot.real, real));
                    edits[at] = Edit::Replace(read);
                }
            }
            Event::Store {
                at,
                addr,
                offset,
                real,
            } => {
                if let Some(slot) = self.slot(index, offset, true) {
                    drop_range(edits, addr, 3);
                    let mut write = convert(real, slot.real).into_iter().collect::<Vec<_>>();
                    write.push(I::LocalSet(self.local(slot)));
                    edits[at] = Edit::Replace(write);
                }
            }
            Event::Copy {
                at,
                dst,
                src,
                len_at,
                bytes,
            } if self.resolution.expanded[index] => {
                drop_side(edits, dst);
                drop_side(edits, src);
                edits[len_at] = Edit::Drop;
                edits[at] = Edit::Replace(self.copy_moves(index, (dst, src), bytes));
            }
            Event::Copy { .. } => {}
            Event::Fill {
                at,
                dst,
                value_at,
                len_at,
                value,
                bytes,
            } => {
                drop_side(edits, dst);
                edits[value_at] = Edit::Drop;
                edits[len_at] = Edit::Drop;
                edits[at] = Edit::Replace(self.fill_stores(index, dst, value, bytes));
            }
        }
    }

    fn place(&self, event: usize, side: Side, cell: u32, destination: bool) -> Place {
        match side {
            Side::Scratch { offset, .. } => {
                let offset = offset + cell * 8;
                match self.slot(event, offset, destination) {
                    Some(slot) => Place::Local(self.local(slot), slot.real),
                    None => Place::Memory(2, offset),
                }
            }
            Side::Static { base, offset, .. } => Place::Memory(base, offset + cell * 8),
            Side::Opaque => Place::Open(cell * 8),
        }
    }

    fn address(&self, place: &Place) -> Vec<I<'static>> {
        match *place {
            Place::Memory(base, offset) => {
                vec![I::LocalGet(base), I::I32Const(offset as i32), I::I32Add]
            }
            Place::Open(0) => vec![I::LocalGet(self.temp())],
            Place::Open(offset) => vec![
                I::LocalGet(self.temp()),
                I::I32Const(offset as i32),
                I::I32Add,
            ],
            Place::Local(..) => Vec::new(),
        }
    }

    fn copy_moves(&self, event: usize, (dst, src): (Side, Side), bytes: u32) -> Vec<I<'static>> {
        let cells = bytes / 8;
        let mut code = Vec::new();
        if matches!(dst, Side::Opaque) || matches!(src, Side::Opaque) {
            code.push(I::LocalSet(self.temp()));
        }
        if same_cells(dst, src) {
            return code;
        }
        let order: Vec<u32> = if after(dst, src) {
            (0..cells).rev().collect()
        } else {
            (0..cells).collect()
        };
        for cell in order {
            let (to, from) = (
                self.place(event, dst, cell, true),
                self.place(event, src, cell, false),
            );
            let real = match (&from, &to) {
                (Place::Local(_, real), _) | (_, Place::Local(_, real)) => *real,
                _ => false,
            };
            code.extend(self.address(&to));
            code.extend(self.read(&from, real));
            code.extend(self.write(&to, real));
        }
        code
    }

    fn fill_stores(&self, event: usize, dst: Side, value: u8, bytes: u32) -> Vec<I<'static>> {
        let pattern = u64::from_ne_bytes([value; 8]);
        let mut code = Vec::new();
        for cell in 0..bytes / 8 {
            let to = self.place(event, dst, cell, true);
            code.extend(self.address(&to));
            let real = matches!(to, Place::Local(_, true));
            code.push(if real {
                I::F64Const(f64::from_bits(pattern).into())
            } else {
                I::I64Const(pattern as i64)
            });
            code.extend(self.write(&to, real));
        }
        code
    }

    fn read(&self, place: &Place, real: bool) -> Vec<I<'static>> {
        match place {
            Place::Local(local, kind) => {
                let mut read = vec![I::LocalGet(*local)];
                read.extend(convert(*kind, real));
                read
            }
            _ => {
                let mut read = self.address(place);
                read.push(if real {
                    I::F64Load(CELL)
                } else {
                    I::I64Load(CELL)
                });
                read
            }
        }
    }

    fn write(&self, place: &Place, real: bool) -> Vec<I<'static>> {
        match place {
            Place::Local(local, kind) => {
                let mut write = convert(real, *kind).into_iter().collect::<Vec<_>>();
                write.push(I::LocalSet(*local));
                write
            }
            _ => vec![if real {
                I::F64Store(CELL)
            } else {
                I::I64Store(CELL)
            }],
        }
    }
}

/// The reinterpretation from a value of one kind to the other, bit for bit.
fn convert(from_real: bool, to_real: bool) -> Option<I<'static>> {
    match (from_real, to_real) {
        (true, false) => Some(I::I64ReinterpretF64),
        (false, true) => Some(I::F64ReinterpretI64),
        _ => None,
    }
}

fn drop_range(edits: &mut [Edit], start: usize, count: usize) {
    for edit in &mut edits[start..start + count] {
        *edit = Edit::Drop;
    }
}

fn drop_side(edits: &mut [Edit], side: Side) {
    match side {
        Side::Scratch { addr, .. } | Side::Static { addr, .. } => drop_range(edits, addr, 3),
        Side::Opaque => {}
    }
}

/// Whether the cells of `dst` follow those of `src` in one address space, so
/// a copy between overlapping ranges must run from the last cell.
fn after(dst: Side, src: Side) -> bool {
    match (dst, src) {
        (Side::Scratch { offset: a, .. }, Side::Scratch { offset: b, .. }) => a > b,
        (
            Side::Static {
                base: x, offset: a, ..
            },
            Side::Static {
                base: y, offset: b, ..
            },
        ) => x == y && a > b,
        _ => false,
    }
}
