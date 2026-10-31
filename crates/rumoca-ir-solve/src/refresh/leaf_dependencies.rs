//! Construction-issued transitive dependence of every exact assignment target.
//!
//! An exact assignment target's value depends on the solver-Y slots its
//! assignment reads ([`ExactRefreshAssignmentProgram::assignment_y_dependencies`]);
//! a read slot that another exact program assigns depends in turn on that
//! assignment's reads. Following every such chain ends at the leaves: the
//! slots no exact program assigns, the states among them. The leaves are
//! issued once, when the programs are built, so later stages read the
//! structure instead of rediscovering it, and `validate` re-derives them.
use super::*;
use std::collections::BTreeSet;

/// The leaves one exact assignment target depends on.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LeafDependencies {
    /// Exactly these solver-Y slots, none of them assigned by an exact program.
    Bounded(IndexIntervals),
    /// The chain is cyclic or reaches an unresolved read; any slot may matter.
    Unbounded,
}

impl LeafDependencies {
    /// Whether any leaf lies in `slots`. Unbounded dependence always may.
    #[must_use]
    pub fn meets(&self, slots: &IndexIntervals) -> bool {
        match self {
            Self::Bounded(leaves) => !leaves.intersection(slots).is_empty(),
            Self::Unbounded => true,
        }
    }
}

type Node = (usize, usize);

/// What the traversal needs of one program: its targets and, per target, the
/// solver-Y slots its assignment reads (`None` when unissued).
pub(super) struct ProgramReads<'a> {
    pub(super) targets: &'a [usize],
    pub(super) reads: Vec<Option<&'a IndexIntervals>>,
}

/// One target's pending traversal: its leaf reads, the assignments it reads
/// through, and how many of those have been visited.
struct Frame {
    node: Node,
    leaves: IndexIntervals,
    children: Vec<Node>,
    next: usize,
    unbounded: bool,
}

struct Assigners {
    /// Every exact assignment target, sorted, with the assignments issuing it.
    by_target: BTreeMap<usize, Vec<Node>>,
    targets: Vec<usize>,
}

impl Assigners {
    fn of(programs: &[ProgramReads<'_>]) -> Self {
        let mut by_target: BTreeMap<usize, Vec<Node>> = BTreeMap::new();
        for (program, owner) in programs.iter().enumerate() {
            for (position, &target) in owner.targets.iter().enumerate() {
                by_target
                    .entry(target)
                    .or_default()
                    .push((program, position));
            }
        }
        let targets = by_target.keys().copied().collect();
        Self { by_target, targets }
    }

    fn frame(&self, programs: &[ProgramReads<'_>], node: Node) -> Frame {
        let mut frame = Frame {
            node,
            leaves: IndexIntervals::default(),
            children: Vec::new(),
            next: 0,
            unbounded: false,
        };
        let Some(reads) = programs
            .get(node.0)
            .and_then(|program| program.reads.get(node.1).copied().flatten())
        else {
            frame.unbounded = true;
            return frame;
        };
        for interval in reads.intervals() {
            let (start, end) = (*interval.start(), *interval.end());
            let first = self.targets.partition_point(|&target| target < start);
            let last = self.targets.partition_point(|&target| target <= end);
            let mut cursor = start;
            for &target in self.targets.get(first..last).unwrap_or(&[]) {
                frame.leaves = frame.leaves.union(&IndexIntervals::range(cursor, target));
                frame
                    .children
                    .extend(self.by_target.get(&target).into_iter().flatten().copied());
                cursor = target + 1;
            }
            frame.leaves = frame
                .leaves
                .union(&IndexIntervals::range(cursor, end.saturating_add(1)));
        }
        frame
    }
}

/// Issue the leaves of every target of every program, in program and target
/// order.
pub(super) fn issue(programs: &[ExactRefreshAssignmentProgram]) -> Vec<Box<[LeafDependencies]>> {
    let views = programs
        .iter()
        .map(|program| ProgramReads {
            targets: program.target_indices(),
            reads: (0..program.target_indices().len())
                .map(|position| program.assignment_y_dependencies(position))
                .collect(),
        })
        .collect::<Vec<_>>();
    issue_reads(&views)
}

/// [`issue`] over the programs' targets and reads.
pub(super) fn issue_reads(programs: &[ProgramReads<'_>]) -> Vec<Box<[LeafDependencies]>> {
    let assigners = Assigners::of(programs);
    let mut done: Vec<Vec<Option<LeafDependencies>>> = programs
        .iter()
        .map(|program| vec![None; program.targets.len()])
        .collect();
    let mut visiting = BTreeSet::new();
    for (program, owner) in programs.iter().enumerate() {
        for position in 0..owner.targets.len() {
            resolve(
                programs,
                &assigners,
                (program, position),
                &mut done,
                &mut visiting,
            );
        }
    }
    done.into_iter()
        .map(|targets| {
            targets
                .into_iter()
                .map(|leaves| leaves.unwrap_or(LeafDependencies::Unbounded))
                .collect()
        })
        .collect()
}

fn issued(done: &[Vec<Option<LeafDependencies>>], node: Node) -> Option<&LeafDependencies> {
    done.get(node.0)?.get(node.1)?.as_ref()
}

/// Post-order traversal from `root` with an explicit stack: deep assignment
/// chains must not exhaust the call stack.
fn resolve(
    programs: &[ProgramReads<'_>],
    assigners: &Assigners,
    root: Node,
    done: &mut [Vec<Option<LeafDependencies>>],
    visiting: &mut BTreeSet<Node>,
) {
    if issued(done, root).is_some() {
        return;
    }
    visiting.insert(root);
    let mut stack = vec![assigners.frame(programs, root)];
    while let Some(frame) = stack.last_mut() {
        if let Some(&child) = frame.children.get(frame.next) {
            frame.next += 1;
            if issued(done, child).is_some() {
                continue;
            }
            if !visiting.insert(child) {
                frame.unbounded = true;
                continue;
            }
            stack.push(assigners.frame(programs, child));
            continue;
        }
        let Some(frame) = stack.pop() else {
            break;
        };
        let leaves = combine(&frame, done);
        visiting.remove(&frame.node);
        if let Some(slot) = done
            .get_mut(frame.node.0)
            .and_then(|targets| targets.get_mut(frame.node.1))
        {
            *slot = Some(leaves);
        }
    }
}

fn combine(frame: &Frame, done: &[Vec<Option<LeafDependencies>>]) -> LeafDependencies {
    if frame.unbounded {
        return LeafDependencies::Unbounded;
    }
    let mut leaves = frame.leaves.clone();
    for &child in &frame.children {
        match issued(done, child) {
            Some(LeafDependencies::Bounded(child)) => leaves = leaves.union(child),
            Some(LeafDependencies::Unbounded) | None => return LeafDependencies::Unbounded,
        }
    }
    LeafDependencies::Bounded(leaves)
}

#[cfg(test)]
mod tests;
