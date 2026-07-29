#[cfg(test)]
mod tests;

use super::{DaeConstructionError, DaeProvenance, checked_u32};

const EMPTY: u32 = u32::MAX;
const LEAF: u32 = u32::MAX;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FunctionReadSet(u32);

impl FunctionReadSet {
    pub(crate) const EMPTY: Self = Self(EMPTY);

    const fn raw(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FunctionReadFact {
    pub(crate) value: u32,
    pub(crate) definition: u32,
    pub(crate) witness: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ConflictingFunctionRead {
    pub(crate) expected: FunctionReadFact,
    pub(crate) found: FunctionReadFact,
}

#[derive(Debug)]
pub(crate) enum FunctionReadMergeError {
    Conflict(ConflictingFunctionRead),
    Construction(DaeConstructionError),
}

/// One node in a persistent Patricia trie.
///
/// `branch` is `LEAF` for a leaf and otherwise the most-significant differing
/// bit (0..31). `key` is the leaf key or the minimum descendant key. The
/// remaining words are definition/witness or zero/one child indexes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FunctionReadNode {
    branch: u32,
    key: u32,
    zero_or_definition: u32,
    one_or_witness: u32,
}

impl FunctionReadNode {
    const fn leaf(fact: FunctionReadFact) -> Self {
        Self {
            branch: LEAF,
            key: fact.value,
            zero_or_definition: fact.definition,
            one_or_witness: fact.witness,
        }
    }

    const fn branch(key: u32, bit: u32, zero: u32, one: u32) -> Self {
        Self {
            branch: bit,
            key,
            zero_or_definition: zero,
            one_or_witness: one,
        }
    }

    const fn is_leaf(self) -> bool {
        self.branch == LEAF
    }

    const fn depth(self) -> u32 {
        if self.is_leaf() {
            u32::BITS
        } else {
            self.branch
        }
    }

    const fn fact(self) -> FunctionReadFact {
        FunctionReadFact {
            value: self.key,
            definition: self.zero_or_definition,
            witness: self.one_or_witness,
        }
    }
}

/// Construction-only immutable read maps with structural sharing.
///
/// A singleton occupies one 16-byte node. Structural union links disjoint
/// prefixes with one branch and rebuilds at most one branch per overlapping
/// Patricia level; it never duplicates leaves.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(crate) struct FunctionReadSets {
    nodes: Vec<FunctionReadNode>,
}

impl FunctionReadSets {
    pub(crate) fn singleton(
        &mut self,
        fact: FunctionReadFact,
        provenance: DaeProvenance,
    ) -> Result<FunctionReadSet, DaeConstructionError> {
        self.push(FunctionReadNode::leaf(fact), provenance)
            .map(FunctionReadSet)
    }

    pub(crate) fn merge(
        &mut self,
        lhs: FunctionReadSet,
        rhs: FunctionReadSet,
        provenance: DaeProvenance,
    ) -> Result<FunctionReadSet, FunctionReadMergeError> {
        if lhs == rhs || rhs.raw() == EMPTY {
            return Ok(lhs);
        }
        if lhs.raw() == EMPTY {
            return Ok(rhs);
        }
        self.union(lhs.raw(), rhs.raw(), provenance)
            .map(FunctionReadSet)
    }

    pub(crate) fn try_for_each<E>(
        &self,
        root: FunctionReadSet,
        visit: &mut impl FnMut(FunctionReadFact) -> Result<(), E>,
    ) -> Result<(), E> {
        self.visit_at(root.raw(), visit)
    }

    fn union(
        &mut self,
        lhs: u32,
        rhs: u32,
        provenance: DaeProvenance,
    ) -> Result<u32, FunctionReadMergeError> {
        if lhs == rhs {
            return Ok(lhs);
        }
        let lhs_node = self.nodes[lhs as usize];
        let rhs_node = self.nodes[rhs as usize];
        let lhs_depth = lhs_node.depth();
        let rhs_depth = rhs_node.depth();
        let common = (lhs_node.key ^ rhs_node.key).leading_zeros();
        if common < lhs_depth.min(rhs_depth) {
            let (zero, one) = if bit(lhs_node.key, common) {
                (rhs, lhs)
            } else {
                (lhs, rhs)
            };
            return self.push_branch(common, zero, one, provenance);
        }
        if lhs_depth == u32::BITS && rhs_depth == u32::BITS {
            let expected = lhs_node.fact();
            let found = rhs_node.fact();
            return if expected.definition == found.definition {
                Ok(lhs)
            } else {
                Err(FunctionReadMergeError::Conflict(ConflictingFunctionRead {
                    expected,
                    found,
                }))
            };
        }
        if lhs_depth == rhs_depth {
            let zero = self.union(
                lhs_node.zero_or_definition,
                rhs_node.zero_or_definition,
                provenance,
            )?;
            let one = self.union(lhs_node.one_or_witness, rhs_node.one_or_witness, provenance)?;
            if zero == lhs_node.zero_or_definition && one == lhs_node.one_or_witness {
                return Ok(lhs);
            }
            return self.push_branch(lhs_depth, zero, one, provenance);
        }
        if lhs_depth < rhs_depth {
            let (zero, one) = if bit(rhs_node.key, lhs_depth) {
                (
                    lhs_node.zero_or_definition,
                    self.union(lhs_node.one_or_witness, rhs, provenance)?,
                )
            } else {
                (
                    self.union(lhs_node.zero_or_definition, rhs, provenance)?,
                    lhs_node.one_or_witness,
                )
            };
            if zero == lhs_node.zero_or_definition && one == lhs_node.one_or_witness {
                return Ok(lhs);
            }
            return self.push_branch(lhs_depth, zero, one, provenance);
        }
        let (zero, one) = if bit(lhs_node.key, rhs_depth) {
            (
                rhs_node.zero_or_definition,
                self.union(lhs, rhs_node.one_or_witness, provenance)?,
            )
        } else {
            (
                self.union(lhs, rhs_node.zero_or_definition, provenance)?,
                rhs_node.one_or_witness,
            )
        };
        if zero == rhs_node.zero_or_definition && one == rhs_node.one_or_witness {
            return Ok(rhs);
        }
        self.push_branch(rhs_depth, zero, one, provenance)
    }

    fn push_branch(
        &mut self,
        depth: u32,
        zero: u32,
        one: u32,
        provenance: DaeProvenance,
    ) -> Result<u32, FunctionReadMergeError> {
        let key = self.nodes[zero as usize].key;
        self.push(FunctionReadNode::branch(key, depth, zero, one), provenance)
            .map_err(FunctionReadMergeError::Construction)
    }

    fn visit_at<E>(
        &self,
        raw: u32,
        visit: &mut impl FnMut(FunctionReadFact) -> Result<(), E>,
    ) -> Result<(), E> {
        if raw == EMPTY {
            return Ok(());
        }
        let node = self.nodes[raw as usize];
        if node.is_leaf() {
            return visit(node.fact());
        }
        self.visit_at(node.zero_or_definition, visit)?;
        self.visit_at(node.one_or_witness, visit)
    }

    fn push(
        &mut self,
        node: FunctionReadNode,
        provenance: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        let raw = checked_u32(self.nodes.len(), "function read proof arena", provenance)?;
        if raw == EMPTY {
            return Err(DaeConstructionError::CapacityExceeded {
                arena: "function read proof arena",
                attempted_index: self.nodes.len(),
                span: provenance.span(),
            });
        }
        self.nodes.push(node);
        Ok(raw)
    }
}

const fn bit(value: u32, depth: u32) -> bool {
    value & (1 << (u32::BITS - depth - 1)) != 0
}

const _: [(); 16] = [(); std::mem::size_of::<FunctionReadNode>()];
const _: [(); 4] = [(); std::mem::size_of::<FunctionReadSet>()];
