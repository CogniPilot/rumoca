//! The call level of every function owner of a block.
//!
//! An owner's level is one more than the deepest chain of calls that reaches
//! it from the block's methods; an owner no method reaches is at level 1.
//! Owners at one level never hold frames at the same time, which is what a
//! printer that keeps one scratch overlay per level relies on. A recursive
//! cycle is cut at the owner count, the deepest acyclic chain.

use super::super::call::visit_calls;
use super::SolveAlgorithmBlock;

/// The issued call levels of one block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolveBlockCallLevels {
    levels: Box<[usize]>,
    deepest: usize,
}

impl SolveBlockCallLevels {
    /// The level of owner `owner` (dense, in table order).
    #[must_use]
    pub fn level(&self, owner: usize) -> Option<usize> {
        self.levels.get(owner).copied()
    }

    /// Every owner's level, in table order.
    #[must_use]
    pub fn levels(&self) -> &[usize] {
        &self.levels
    }

    /// The deepest level, or 0 for a block without functions.
    #[must_use]
    pub const fn deepest(&self) -> usize {
        self.deepest
    }
}

impl SolveAlgorithmBlock {
    /// The call level of every function owner, from the calls of the
    /// block's methods and of each owner's body.
    #[must_use]
    pub fn call_levels(&self) -> SolveBlockCallLevels {
        let owners = self.functions().owners();
        let count = owners.len();
        let mut roots = Vec::new();
        for method in [self.startup(), self.recalibrate(), self.do_step()] {
            visit_calls(method.program(), &mut |owner| {
                roots.push(owner.index() as usize)
            });
        }
        let edges: Vec<Vec<usize>> = owners
            .iter()
            .map(|owner| {
                let mut called = Vec::new();
                visit_calls(owner.body(), &mut |callee| {
                    called.push(callee.index() as usize)
                });
                called
            })
            .collect();
        let mut levels = vec![1usize; count];
        let mut frontier: Vec<(usize, usize)> = roots.into_iter().map(|owner| (owner, 1)).collect();
        while let Some((owner, depth)) = frontier.pop() {
            if owner >= count || depth > count || levels[owner] > depth {
                continue;
            }
            levels[owner] = depth;
            frontier.extend(edges[owner].iter().map(|callee| (*callee, depth + 1)));
        }
        let deepest = levels.iter().copied().max().unwrap_or(0);
        SolveBlockCallLevels {
            levels: levels.into(),
            deepest,
        }
    }
}
