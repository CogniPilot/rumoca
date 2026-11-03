//! Issued occurrences retain register scope, source order and callee binding.
use super::*;
use crate::{SolveAssertionForwarding, SolveOperation, SolveRegisterId, SolveSpannedOperation};

#[derive(Clone, Copy)]
enum Namespace<'source> {
    Entry(&'source SharedHelperProgramEntry),
    Helper(BoundSolvePureCallBody<'source>),
}

impl<'source> Namespace<'source> {
    fn resolve(
        self,
        owner: SolvePureCallOwnerId,
        provenance: Span,
    ) -> Result<BoundSolvePureCallBody<'source>, SolveProgramConstructionError> {
        match self {
            Self::Entry(entry) => entry.callee(owner, provenance),
            Self::Helper(helper) => helper.resolve_child(owner, provenance),
        }
    }
}

/// Nominal source program and local operation. Clone retains this key;
/// independent construction or wire replay requires fresh binding.
#[derive(Clone, Copy)]
pub struct EntryCallOccurrenceId<'source> {
    program: &'source TypedProgram,
    operation_index: usize,
}

impl PartialEq for EntryCallOccurrenceId<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.program, other.program) && self.operation_index == other.operation_index
    }
}
impl Eq for EntryCallOccurrenceId<'_> {}
impl std::hash::Hash for EntryCallOccurrenceId<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(
            &(self.program as *const TypedProgram, self.operation_index),
            state,
        );
    }
}

/// One actual source occurrence, not caller-supplied interface-compatible parts.
#[derive(Clone, Copy)]
pub struct EntryCallOccurrence<'source> {
    program: &'source TypedProgram,
    operation: &'source SolveSpannedOperation,
    operation_index: usize,
    callee: BoundSolvePureCallBody<'source>,
    arguments: &'source [SolveRegisterId],
    destinations: &'source [SolveRegisterId],
    forwarding: &'source [SolveAssertionForwarding],
}

impl<'source> EntryCallOccurrence<'source> {
    pub fn id(self) -> EntryCallOccurrenceId<'source> {
        EntryCallOccurrenceId {
            program: self.program,
            operation_index: self.operation_index,
        }
    }
    pub fn operation_index(self) -> usize {
        self.operation_index
    }

    pub fn program(self) -> &'source TypedProgram {
        self.program
    }
    pub fn operation(self) -> &'source SolveSpannedOperation {
        self.operation
    }
    pub fn callee(self) -> BoundSolvePureCallBody<'source> {
        self.callee
    }
    pub fn arguments(self) -> &'source [SolveRegisterId] {
        self.arguments
    }
    pub fn destinations(self) -> &'source [SolveRegisterId] {
        self.destinations
    }
    pub fn assertion_forwarding(self) -> &'source [SolveAssertionForwarding] {
        self.forwarding
    }
}

struct Frame<'source> {
    program: &'source TypedProgram,
    operations: std::iter::Enumerate<std::slice::Iter<'source, SolveSpannedOperation>>,
}

/// Preorder source traversal includes structured regions, without executing
/// them or following helper calls recursively. Each helper has its own iterator.
pub struct EntryCallOccurrences<'source> {
    namespace: Namespace<'source>,
    stack: Vec<Frame<'source>>,
}

impl SharedHelperProgramEntry {
    pub fn call_occurrences(&self) -> EntryCallOccurrences<'_> {
        EntryCallOccurrences::new(Namespace::Entry(self), self.body())
    }
}

impl<'source> BoundSolvePureCallBody<'source> {
    pub fn call_occurrences(self) -> EntryCallOccurrences<'source> {
        EntryCallOccurrences::new(Namespace::Helper(self), self.body())
    }
}

impl<'source> EntryCallOccurrences<'source> {
    fn new(namespace: Namespace<'source>, program: &'source TypedProgram) -> Self {
        Self {
            namespace,
            stack: vec![Frame {
                program,
                operations: program.operations().iter().enumerate(),
            }],
        }
    }
}

impl<'source> Iterator for EntryCallOccurrences<'source> {
    type Item = Result<EntryCallOccurrence<'source>, SolveProgramConstructionError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let frame = self.stack.last_mut()?;
            let Some((operation_index, operation)) = frame.operations.next() else {
                self.stack.pop();
                continue;
            };
            let program = frame.program;
            for child in crate::visitor::nested_typed_programs(operation.operation())
                .into_iter()
                .flatten()
                .rev()
            {
                self.stack.push(Frame {
                    program: child,
                    operations: child.operations().iter().enumerate(),
                });
            }
            if let SolveOperation::Call {
                owner,
                arguments,
                destinations,
                assertion_forwarding,
            } = operation.operation()
            {
                let callee = self.namespace.resolve(*owner, operation.provenance());
                return Some(callee.map(|callee| EntryCallOccurrence {
                    program,
                    operation,
                    operation_index,
                    callee,
                    arguments,
                    destinations,
                    forwarding: assertion_forwarding,
                }));
            }
        }
    }
}
