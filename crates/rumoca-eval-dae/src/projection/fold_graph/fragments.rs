//! Ordered completed capture chunks; allocation identity only indexes live data.
use crate::projection::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use super::{FunctionParameterDependency, OrderedDependencies};

type Dependencies = Arc<[FunctionParameterDependency]>;

#[derive(Debug, Clone)]
enum Piece {
    Scalar(FunctionParameterDependency),
    Completed(Dependencies),
}

impl PartialEq for Piece {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Scalar(a), Self::Scalar(b)) => a == b,
            (Self::Completed(a), Self::Completed(b)) => Arc::ptr_eq(a, b),
            _ => false,
        }
    }
}
impl Eq for Piece {}

impl Hash for Piece {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Self::Scalar(value) => value.hash(state),
            Self::Completed(values) => Arc::as_ptr(values)
                .cast::<FunctionParameterDependency>()
                .hash(state),
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct Recording {
    pieces: Vec<Piece>,
    scalars: HashSet<FunctionParameterDependency>,
    chunks: HashSet<*const FunctionParameterDependency>,
}

impl Recording {
    pub(super) fn scalar(&mut self, dependency: &FunctionParameterDependency) {
        if self.scalars.insert(dependency.clone()) {
            self.pieces.push(Piece::Scalar(dependency.clone()));
        }
    }

    pub(super) fn completed(&mut self, values: &Dependencies) {
        if !values.is_empty()
            && self
                .chunks
                .insert(Arc::as_ptr(values).cast::<FunctionParameterDependency>())
        {
            self.pieces.push(Piece::Completed(Arc::clone(values)));
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct Inventories {
    sequences: HashMap<Arc<[Piece]>, Dependencies>,
    completed: HashSet<Dependencies>,
    #[cfg(test)]
    pub(super) flattened_occurrences: u64,
}

impl Inventories {
    pub(super) fn finish(&mut self, recording: Recording) -> Dependencies {
        if let Some(values) = self.sequences.get(recording.pieces.as_slice()) {
            return Arc::clone(values);
        }
        let mut ordered = OrderedDependencies::default();
        for piece in &recording.pieces {
            let _occurrences = append(piece, &mut ordered);
            #[cfg(test)]
            {
                self.flattened_occurrences += _occurrences;
            }
        }
        let values = ordered.into_values();
        let inventory = if let Some(inventory) = self.completed.get(values.as_slice()) {
            Arc::clone(inventory)
        } else {
            let inventory: Dependencies = values.into();
            self.completed.insert(Arc::clone(&inventory));
            inventory
        };
        self.sequences
            .insert(recording.pieces.into(), Arc::clone(&inventory));
        inventory
    }
}

fn append(piece: &Piece, ordered: &mut OrderedDependencies) -> u64 {
    match piece {
        Piece::Scalar(value) => {
            ordered.insert(value);
            1
        }
        Piece::Completed(values) => {
            for value in values.iter() {
                ordered.insert(value);
            }
            values.len() as u64
        }
    }
}

#[cfg(test)]
mod tests;
