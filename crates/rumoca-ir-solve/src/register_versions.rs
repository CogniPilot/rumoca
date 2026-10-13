//! Canonical immutable source-point register range versions.
mod program;
#[cfg(test)]
mod tests;

pub use program::{RegisterOperandPiece, RegisterOperationVersions, RegisterVersionProgram};

use crate::Reg;
use std::{collections::BTreeMap, ops::Deref, sync::Arc};

pub(crate) struct RegisterVersionInventory<T> {
    pieces: BTreeMap<Reg, RegisterVersionPiece<T>>,
}
impl<T> Default for RegisterVersionInventory<T> {
    fn default() -> Self {
        Self {
            pieces: BTreeMap::new(),
        }
    }
}
#[derive(Debug)]
pub(crate) struct RegisterVersionPiece<T> {
    pub(crate) end: u64,
    pub(crate) offset: usize,
    pub(crate) family: Arc<T>,
}
impl<T> Clone for RegisterVersionPiece<T> {
    fn clone(&self) -> Self {
        Self {
            end: self.end,
            offset: self.offset,
            family: self.family.clone(),
        }
    }
}
/// An immutable view retaining exactly the source owners present at capture.
pub(crate) struct RegisterVersionView<T> {
    pub(crate) pieces: Vec<(usize, usize, RegisterVersionPiece<T>)>,
}
impl<T> Deref for RegisterVersionInventory<T> {
    type Target = BTreeMap<Reg, RegisterVersionPiece<T>>;
    fn deref(&self) -> &Self::Target {
        &self.pieces
    }
}
impl<T> RegisterVersionInventory<T> {
    pub(crate) fn install(&mut self, start: Reg, count: usize, family: Arc<T>) {
        if count == 0 {
            return;
        }
        let end = u64::from(start) + count as u64;
        // Splitting a destination retains the exact source family and offset.
        let first = self
            .pieces
            .range(..=start)
            .next_back()
            .map_or(start, |(&key, _)| key);
        let affected: Vec<_> = self
            .pieces
            .range(first..)
            .take_while(|(key, _)| u64::from(**key) < end)
            .filter(|(_, piece)| piece.end > u64::from(start))
            .map(|(&key, piece)| (key, piece.clone()))
            .collect();
        for (key, piece) in affected {
            self.pieces.remove(&key);
            if key < start {
                self.pieces.insert(
                    key,
                    RegisterVersionPiece {
                        end: u64::from(start),
                        ..piece.clone()
                    },
                );
            }
            if piece.end > end {
                let offset = piece.offset + (end - u64::from(key)) as usize;
                self.pieces
                    .insert(end as Reg, RegisterVersionPiece { offset, ..piece });
            }
        }
        self.pieces.insert(
            start,
            RegisterVersionPiece {
                end,
                offset: 0,
                family,
            },
        );
    }
    pub(crate) fn view(&self, start: Reg, count: usize) -> Option<RegisterVersionView<T>> {
        let end = u64::from(start).checked_add(u64::try_from(count).ok()?)?;
        if end > u64::from(Reg::MAX) + 1 {
            return None;
        }
        let mut cursor = u64::from(start);
        let mut pieces = Vec::new();
        while cursor < end {
            let (&key, piece) = self.pieces.range(..=cursor as Reg).next_back()?;
            if piece.end <= cursor {
                return None;
            }
            let limit = piece.end.min(end);
            let offset = piece.offset + (cursor - u64::from(key)) as usize;
            pieces.push((
                (cursor - u64::from(start)) as usize,
                (limit - u64::from(start)) as usize,
                RegisterVersionPiece {
                    offset,
                    ..piece.clone()
                },
            ));
            cursor = limit;
        }
        Some(RegisterVersionView { pieces })
    }
    pub(crate) fn first_missing(&self, start: Reg, count: usize) -> Reg {
        let end = u64::from(start) + count as u64;
        let mut cursor = u64::from(start);
        while cursor < end {
            let Some((_, piece)) = self.pieces.range(..=cursor as Reg).next_back() else {
                break;
            };
            if piece.end <= cursor {
                break;
            }
            cursor = piece.end.min(end);
        }
        cursor as Reg
    }
    pub(crate) fn strided_view(
        &self,
        start: Reg,
        count: usize,
        stride: usize,
        lanes: usize,
    ) -> Option<RegisterVersionView<T>> {
        if count == 0 {
            return Some(RegisterVersionView { pieces: Vec::new() });
        }
        let width = (count - 1)
            .checked_mul(stride)?
            .checked_add(1)?
            .checked_mul(lanes)?;
        let end = u64::from(start).checked_add(width as u64)?;
        if end > u64::from(Reg::MAX) + 1 {
            return None;
        }
        let first = self
            .pieces
            .range(..=start)
            .next_back()
            .map_or(start, |(&key, _)| key);
        let mut pieces = Vec::new();
        let mut cursor = 0;
        for (&key, piece) in self
            .pieces
            .range(first..)
            .take_while(|(key, _)| u64::from(**key) < end)
        {
            if piece.end <= u64::from(start) {
                continue;
            }
            let low = u64::from(key).max(u64::from(start)) - u64::from(start);
            let high = piece.end.min(end) - u64::from(start);
            if selected_gap(cursor, low as usize, stride, lanes) {
                return None;
            }
            pieces.push((
                low as usize,
                high as usize,
                RegisterVersionPiece {
                    offset: piece.offset + (u64::from(start) + low - u64::from(key)) as usize,
                    ..piece.clone()
                },
            ));
            cursor = high as usize;
        }
        if selected_gap(cursor, width, stride, lanes) {
            return None;
        }
        Some(RegisterVersionView { pieces })
    }
}
fn selected_gap(start: usize, end: usize, stride: usize, lanes: usize) -> bool {
    if start >= end {
        return false;
    }
    if stride == 0 {
        return start < lanes;
    }
    let step = stride * lanes;
    let block = start / step * step;
    let first = if start < block + lanes {
        start
    } else {
        block + step
    };
    first < end
}
