use super::{Degree, LinearOp, Registers, visit_program_degrees};
use std::collections::{BTreeMap, BTreeSet, btree_set};
use std::iter::Peekable;

pub(super) struct SourceOutput<'source> {
    pub(super) node: usize,
    pub(super) program: usize,
    pub(super) operations: &'source [LinearOp],
    pub(super) offset: usize,
}

struct SelectedProgram<'source> {
    operations: &'source [LinearOp],
    offsets: BTreeSet<usize>,
}

pub(super) fn block_is_affine(
    outputs: &BTreeMap<usize, SourceOutput<'_>>,
    rows: &[usize],
    targets: &BTreeSet<usize>,
) -> bool {
    let mut programs = BTreeMap::<(usize, usize), SelectedProgram<'_>>::new();
    for row in rows {
        let Some(output) = outputs.get(row) else {
            return false;
        };
        programs
            .entry((output.node, output.program))
            .or_insert_with(|| SelectedProgram {
                operations: output.operations,
                offsets: BTreeSet::new(),
            })
            .offsets
            .insert(output.offset);
    }
    programs.values().all(|program| {
        visit_program_degrees(program.operations, targets, &program.offsets, |degree| {
            degree != Degree::Nonlinear
        })
        .is_some()
    })
}

pub(super) struct SelectedStores<'outputs, Visit> {
    offsets: Peekable<btree_set::Iter<'outputs, usize>>,
    output_start: usize,
    visit: Visit,
}

impl<'outputs, Visit: FnMut(Degree) -> bool> SelectedStores<'outputs, Visit> {
    pub(super) fn new(offsets: &'outputs BTreeSet<usize>, visit: Visit) -> Self {
        Self {
            offsets: offsets.iter().peekable(),
            output_start: 0,
            visit,
        }
    }

    pub(super) fn is_complete(&mut self) -> bool {
        self.offsets.peek().is_none()
    }

    pub(super) fn read(
        &mut self,
        registers: &Registers,
        start: u32,
        count: usize,
        stride: usize,
    ) -> Option<()> {
        while let Some(&&output) = self.offsets.peek() {
            let offset = output.checked_sub(self.output_start)?;
            if offset >= count {
                break;
            }
            let register = start.checked_add(u32::try_from(offset.checked_mul(stride)?).ok()?)?;
            if !(self.visit)(registers.read(register, 1)?) {
                return None;
            }
            self.offsets.next();
        }
        if !self.is_complete() {
            self.output_start = self.output_start.checked_add(count)?;
        }
        Some(())
    }
}
