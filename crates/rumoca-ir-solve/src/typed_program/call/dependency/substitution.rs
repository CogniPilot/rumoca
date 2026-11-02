//! A call's output dependencies: each callee summary relation composed with
//! the relations of the argument it reads.
//!
//! Wide calls repeat the same work many times over. Outputs read the same
//! argument through the same access, arguments share relations, and the
//! unions they feed mostly receive relations they already hold. Relations are
//! therefore interned once per call and handled by index: each distinct
//! composition is built once, and each output's union is formed over indices
//! before its relations are inserted, first occurrences only, in the original
//! order. Inserting a relation again adds nothing, so the result is the one
//! composing and inserting every pair in turn would give.

use std::collections::{HashMap, HashSet};
use std::num::NonZeroUsize;

use indexmap::IndexSet;

use super::{Coordinates, Dependencies, SolveCallDependency};
use crate::SolveValueType;

/// A relation by index: input run start, run length and interned coordinates
/// (`None` for a whole-input dependency).
type Relation = (usize, NonZeroUsize, Option<u32>);

#[derive(Default)]
pub(super) struct Interned {
    ids: HashMap<Coordinates, u32>,
    coordinates: Vec<Coordinates>,
}

impl Interned {
    fn id(&mut self, coordinates: &Coordinates) -> Option<u32> {
        if let Some(&id) = self.ids.get(coordinates) {
            return Some(id);
        }
        let id = u32::try_from(self.coordinates.len()).ok()?;
        self.ids.insert(coordinates.clone(), id);
        self.coordinates.push(coordinates.clone());
        Some(id)
    }

    fn relation(&mut self, dependency: &SolveCallDependency) -> Option<Relation> {
        let coordinates = match &dependency.coordinates {
            Some(coordinates) => Some(self.id(coordinates)?),
            None => None,
        };
        Some((dependency.input, dependency.count, coordinates))
    }

    fn dependency(&self, (input, count, coordinates): Relation) -> SolveCallDependency {
        SolveCallDependency {
            input,
            count,
            coordinates: coordinates.map(|id| self.coordinates[id as usize].clone()),
        }
    }
}

/// One call's substitution state. Entries keyed by a register are dropped
/// when the call assigns that register.
#[derive(Default)]
pub(super) struct Substitution {
    interned: Interned,
    /// Each argument register's relations.
    arguments: HashMap<usize, Vec<Relation>>,
    /// Composed coordinates per (access, source), `None` when whole.
    composed: HashMap<(u32, u32), Option<u32>>,
    /// Composed relations per (argument register, access).
    reads: HashMap<(usize, Option<u32>), ReadRelations>,
    /// Finished outputs per (summary, output type), while no argument has
    /// been reassigned.
    finished: HashMap<(Vec<SolveCallDependency>, SolveValueType), Vec<SolveCallDependency>>,
}

/// One read's composed relations: its whole-input runs merged, and its
/// distinct exact relations in order.
struct ReadRelations {
    whole: Vec<(usize, usize)>,
    exact: Vec<Relation>,
}

impl ReadRelations {
    fn new(relations: IndexSet<Relation>) -> Self {
        let mut whole: Vec<(usize, usize)> = Vec::new();
        let mut exact = Vec::new();
        for relation in relations {
            let (input, count, coordinates) = relation;
            if coordinates.is_some() {
                exact.push(relation);
            } else {
                whole.push((input, input + count.get()));
            }
        }
        whole.sort_unstable();
        let mut merged: Vec<(usize, usize)> = Vec::with_capacity(whole.len());
        for (start, end) in whole {
            match merged.last_mut() {
                Some(last) if last.1 >= start => last.1 = last.1.max(end),
                _ => merged.push((start, end)),
            }
        }
        Self {
            whole: merged,
            exact,
        }
    }
}

/// What one output reads: the argument register and the callee's access to
/// it, `None` when the callee reads the whole input.
pub(super) struct Read<'a> {
    pub register: usize,
    pub access: Option<&'a Coordinates>,
}

impl Substitution {
    pub(super) fn finished(
        &self,
        summary: &[SolveCallDependency],
        output: &SolveValueType,
    ) -> Option<&Vec<SolveCallDependency>> {
        self.finished.get(&(summary.to_vec(), output.clone()))
    }

    pub(super) fn finish(
        &mut self,
        summary: &[SolveCallDependency],
        output: &SolveValueType,
        result: &[SolveCallDependency],
    ) {
        self.finished
            .insert((summary.to_vec(), output.clone()), result.to_vec());
    }

    /// The register now holds a new value: forget what was read from it.
    pub(super) fn assigned(&mut self, register: usize) {
        if self.arguments.remove(&register).is_some() {
            self.reads.retain(|(read, _), _| *read != register);
            self.finished.clear();
        }
    }

    /// The union of `reads`, each composed with the argument relations, in
    /// read order, as inserting every composed relation in turn would give.
    /// `compose` builds one composition of a source with an access, `None`
    /// for a whole-input dependency.
    pub(super) fn output<'a, E>(
        &mut self,
        reads: impl IntoIterator<Item = Read<'a>>,
        registers: &[Vec<SolveCallDependency>],
        mut compose: impl FnMut(&Coordinates, &Coordinates) -> Result<Option<Coordinates>, E>,
        overflow: impl Fn() -> E,
    ) -> Result<Vec<SolveCallDependency>, E> {
        let mut seen = HashSet::new();
        let mut keys = Vec::new();
        for read in reads {
            let access = match read.access {
                Some(access) => Some(self.interned.id(access).ok_or_else(&overflow)?),
                None => None,
            };
            let key = (read.register, access);
            if !seen.insert(key) {
                continue;
            }
            if !self.reads.contains_key(&key) {
                let composed =
                    self.compose_read(read.register, access, registers, &mut compose, &overflow)?;
                self.reads.insert(key, ReadRelations::new(composed));
            }
            keys.push(key);
        }
        // Whole-input runs first: which leaves end whole, and the order of the
        // exact relations kept on the others, do not depend on when a run is
        // inserted, and a relation on a covered leaf need not be built at all.
        let mut dependencies = Dependencies::default();
        for key in &keys {
            for &(start, end) in &self.reads[key].whole {
                dependencies.insert(SolveCallDependency::whole_run(start, end));
            }
        }
        let mut exact = HashSet::new();
        let relations = keys
            .iter()
            .flat_map(|key| self.reads[key].exact.iter().copied())
            .filter(|&relation| exact.insert(relation));
        for relation in relations {
            let (input, count, _) = relation;
            if !dependencies.covers(input..input + count.get()) {
                dependencies.insert(self.interned.dependency(relation));
            }
        }
        Ok(dependencies.finish())
    }

    /// Build, intern and keep one composition not built before.
    fn compose_new<E>(
        &mut self,
        access: u32,
        source: u32,
        compose: &mut impl FnMut(&Coordinates, &Coordinates) -> Result<Option<Coordinates>, E>,
        overflow: &impl Fn() -> E,
    ) -> Result<Option<u32>, E> {
        let relation = compose(
            &self.interned.coordinates[source as usize],
            &self.interned.coordinates[access as usize],
        )?;
        let coordinates = match relation {
            Some(relation) => Some(self.interned.id(&relation).ok_or_else(overflow)?),
            None => None,
        };
        self.composed.insert((access, source), coordinates);
        Ok(coordinates)
    }

    fn compose_read<E>(
        &mut self,
        register: usize,
        access: Option<u32>,
        registers: &[Vec<SolveCallDependency>],
        compose: &mut impl FnMut(&Coordinates, &Coordinates) -> Result<Option<Coordinates>, E>,
        overflow: &impl Fn() -> E,
    ) -> Result<IndexSet<Relation>, E> {
        if !self.arguments.contains_key(&register) {
            let relations = registers[register]
                .iter()
                .map(|dependency| self.interned.relation(dependency))
                .collect::<Option<Vec<_>>>()
                .ok_or_else(overflow)?;
            self.arguments.insert(register, relations);
        }
        let mut composed = IndexSet::new();
        for index in 0..self.arguments[&register].len() {
            let (input, count, source) = self.arguments[&register][index];
            let coordinates = match (access, source) {
                // The callee reads the whole input, or the argument already
                // depends on all of it.
                (None, _) | (_, None) => None,
                (Some(access), Some(source)) => match self.composed.get(&(access, source)) {
                    Some(&coordinates) => coordinates,
                    None => self.compose_new(access, source, compose, overflow)?,
                },
            };
            composed.insert((input, count, coordinates));
        }
        Ok(composed)
    }
}

#[cfg(test)]
mod tests;
