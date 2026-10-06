//! Exact projection membership, with bounded lazy scalar bitsets.
use crate::projection::{HashMap, HashSet};
use std::collections::hash_map::Entry;

use super::{ScalarExpressionDependency, domain_context::DomainContextId};

const DENSE_LIMIT: usize = 65_536;
const WORD_LIMIT: usize = 2_097_152;
const SCOPE_LIMIT: usize = 65_536;
const MAX_WORD_GAP: usize = 8;

#[derive(Debug, Default)]
struct Scalars {
    words: Vec<u64>,
    sparse_from: Option<usize>,
    generation: u64,
}

#[derive(Debug, Default)]
struct Expression {
    plain: [Scalars; 2],
    scoped: HashMap<(super::Activation, usize, usize), ContextTile>,
}

#[derive(Debug, Default)]
struct ContextTile {
    rows: Vec<u64>,
    generation: u64,
}

struct Allocation<'a> {
    words: &'a mut usize,
    scoped_words: &'a mut usize,
    scopes: &'a mut usize,
    word_limit: usize,
    scope_limit: usize,
}

/// Membership changes representation only: expression, selected field, scalar
/// and exact projection-local lexical identity retain their original equality.
/// Sparse high addresses/fields and exhausted dense budgets keep the HashSet.
#[derive(Debug)]
pub(super) struct Visited {
    expressions: Vec<Option<Box<Expression>>>,
    sparse: HashSet<ScalarExpressionDependency>,
    words: usize,
    scoped_words: usize,
    scopes: usize,
    word_limit: usize,
    scope_limit: usize,
    generation: u64,
}

impl Default for Visited {
    fn default() -> Self {
        Self {
            expressions: Vec::new(),
            sparse: HashSet::default(),
            words: 0,
            scoped_words: 0,
            scopes: 0,
            word_limit: WORD_LIMIT,
            scope_limit: SCOPE_LIMIT,
            generation: 0,
        }
    }
}

impl Visited {
    pub(super) fn insert(&mut self, key: ScalarExpressionDependency) -> bool {
        let expression = key.expression as usize;
        if key.field.is_some() || expression >= DENSE_LIMIT || key.scalar >= DENSE_LIMIT {
            super::profile::membership(
                if key.field.is_some() {
                    super::profile::Membership::Field
                } else if expression >= DENSE_LIMIT {
                    super::profile::Membership::Expression
                } else {
                    super::profile::Membership::Scalar
                },
                Some(&key),
            );
            return self.sparse.insert(key);
        }
        if self.expressions.len() <= expression {
            self.expressions.resize_with(expression + 1, || None);
        }
        let entry = self.expressions[expression].get_or_insert_with(Default::default);
        let inserted = if key.domain_context == DomainContextId::default() {
            insert_bits(
                &mut entry.plain[key.activation.index()],
                key.scalar,
                &mut self.words,
                self.word_limit,
                self.generation,
            )
        } else {
            insert_scoped(
                entry,
                &key,
                Allocation {
                    words: &mut self.words,
                    scoped_words: &mut self.scoped_words,
                    scopes: &mut self.scopes,
                    word_limit: self.word_limit,
                    scope_limit: self.scope_limit,
                },
                self.generation,
            )
        };
        inserted.unwrap_or_else(|| {
            super::profile::fallback_key(&key);
            self.sparse.insert(key)
        })
    }

    pub(super) fn clear(&mut self) {
        self.sparse.clear();
        let reclaim =
            self.scopes > 0 && (self.scopes >= self.scope_limit || self.words >= self.word_limit);
        super::profile::clear(self.generation, self.scopes, self.words, reclaim);
        if reclaim {
            // No previous-generation membership survives this boundary. Drop
            // full scoped storage rather than permanently charge obsolete
            // lexical IDs against future folds. Releasing capacities preserves
            // the aggregate allocation bound across disjoint expressions.
            for expression in self.expressions.iter_mut().flatten() {
                expression.scoped = HashMap::default();
            }
            self.words -= self.scoped_words;
            self.scoped_words = 0;
            self.scopes = 0;
        }
        if let Some(next) = self.generation.checked_add(1) {
            self.generation = next;
        } else {
            self.expressions.clear();
            self.words = 0;
            self.scoped_words = 0;
            self.scopes = 0;
            self.generation = 0;
        }
    }
}

fn insert_scoped(
    entry: &mut Expression,
    key: &ScalarExpressionDependency,
    allocation: Allocation<'_>,
    generation: u64,
) -> Option<bool> {
    let (page, row) = key.domain_context.page_address();
    let bits = match entry.scoped.entry((key.activation, key.scalar / 64, page)) {
        Entry::Occupied(bits) => bits.into_mut(),
        Entry::Vacant(slot) => {
            // IDs are exact local lexical identities. Packing their low six
            // bits changes storage only; arbitrary high pages remain map keys.
            if *allocation.scopes >= allocation.scope_limit
                || row + 1 > allocation.word_limit.saturating_sub(*allocation.words)
            {
                super::profile::membership(
                    if *allocation.scopes >= allocation.scope_limit {
                        super::profile::Membership::ScopeBudget
                    } else {
                        super::profile::Membership::WordBudget
                    },
                    Some(key),
                );
                return None;
            }
            *allocation.scopes += 1;
            slot.insert(ContextTile::default())
        }
    };
    if bits.generation != generation {
        bits.rows.fill(0);
        bits.generation = generation;
    }
    if row >= bits.rows.len() {
        let additional = row + 1 - bits.rows.len();
        if additional > allocation.word_limit.saturating_sub(*allocation.words) {
            super::profile::membership(super::profile::Membership::WordBudget, Some(key));
            return None;
        }
        bits.rows.resize(row + 1, 0);
        *allocation.words += additional;
        *allocation.scoped_words += additional;
    }
    let mask = 1_u64 << (key.scalar % 64);
    let inserted = bits.rows[row] & mask == 0;
    bits.rows[row] |= mask;
    super::profile::membership(super::profile::Membership::Dense, None);
    Some(inserted)
}

fn insert_bits(
    bits: &mut Scalars,
    scalar: usize,
    words: &mut usize,
    limit: usize,
    generation: u64,
) -> Option<bool> {
    if bits.generation != generation {
        bits.words.fill(0);
        bits.sparse_from = None;
        bits.generation = generation;
    }
    let index = scalar / 64;
    if bits.sparse_from.is_some_and(|start| index >= start) {
        super::profile::membership(super::profile::Membership::Cutover, None);
        return None;
    }
    if index >= bits.words.len() {
        let additional = index + 1 - bits.words.len();
        if additional > MAX_WORD_GAP + 1 || additional > limit.saturating_sub(*words) {
            super::profile::membership(
                if additional > MAX_WORD_GAP + 1 {
                    super::profile::Membership::Gap
                } else {
                    super::profile::Membership::WordBudget
                },
                None,
            );
            // A key already stored sparsely must never migrate into the bitmap
            // after subsequent low scalars grow it: that would report a second
            // first insertion. The word cutover is immutable and exact.
            bits.sparse_from = Some(index);
            return None;
        }
        bits.words.resize(index + 1, 0);
        *words += additional;
    }
    let mask = 1_u64 << (scalar % 64);
    let inserted = bits.words[index] & mask == 0;
    bits.words[index] |= mask;
    super::profile::membership(super::profile::Membership::Dense, None);
    Some(inserted)
}

#[cfg(test)]
mod tests;
