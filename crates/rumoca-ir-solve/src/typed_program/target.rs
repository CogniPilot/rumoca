//! Shared finite-domain capability admission before target planning or emission.

#[cfg(test)]
mod tests;

use super::{SolveAssertionMessage, SolveOperation, TypedProgram};
use rumoca_core::Span;

/// Typed-function targets whose finite-domain operation support is checked here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SolveFunctionTarget {
    CFmi,
    Wasm,
}

/// A finite-domain admission bound to this exact immutable typed program.
/// Other existing target obligations remain with their checked target owners.
#[derive(Debug)]
pub struct SolveAdmittedFunctionDomain<'a> {
    target: SolveFunctionTarget,
    program: &'a TypedProgram,
}

impl<'a> SolveAdmittedFunctionDomain<'a> {
    /// The exact program admitted for this target's finite-domain vocabulary.
    #[must_use]
    pub const fn program(&self) -> &'a TypedProgram {
        self.program
    }

    /// The target whose finite-domain operation capability was checked.
    #[must_use]
    pub const fn target(&self) -> SolveFunctionTarget {
        self.target
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct UnsupportedFiniteCheck {
    operation: usize,
    region_path: Vec<(usize, usize)>,
    provenance: Span,
}

/// A source-bound unsupported operation, issued before a target can emit it.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("{target:?} target does not support CheckFinite at {location:?}")]
pub struct SolveFunctionTargetRefusal {
    target: SolveFunctionTarget,
    location: UnsupportedFiniteCheck,
}

impl SolveFunctionTargetRefusal {
    #[must_use]
    pub const fn operation(&self) -> usize {
        self.location.operation
    }

    #[must_use]
    pub fn region_path(&self) -> &[(usize, usize)] {
        &self.location.region_path
    }

    #[must_use]
    pub const fn provenance(&self) -> Span {
        self.location.provenance
    }

    #[must_use]
    pub const fn opcode(&self) -> &'static str {
        "check_finite"
    }

    #[must_use]
    pub const fn message(&self) -> &'static str {
        match self.target {
            SolveFunctionTarget::CFmi => {
                "CheckFinite is not supported by the C FMI execution profile"
            }
            SolveFunctionTarget::Wasm => {
                "CheckFinite is not supported by the Wasm execution profile"
            }
        }
    }
}

impl SolveFunctionTarget {
    /// Admit the finite-domain vocabulary over every original region, without
    /// sampling branch conditions, pruning outputs or changing IEEE arithmetic.
    pub fn admit_finite_domain(
        self,
        program: &TypedProgram,
    ) -> Result<SolveAdmittedFunctionDomain<'_>, SolveFunctionTargetRefusal> {
        if let Some(location) = first_finite_check(program, &mut Vec::new()) {
            return Err(SolveFunctionTargetRefusal {
                target: self,
                location,
            });
        }
        Ok(SolveAdmittedFunctionDomain {
            target: self,
            program,
        })
    }
}

fn first_finite_check(
    program: &TypedProgram,
    path: &mut Vec<(usize, usize)>,
) -> Option<UnsupportedFiniteCheck> {
    for (index, spanned) in program.operations().iter().enumerate() {
        if matches!(spanned.operation(), SolveOperation::CheckFinite { .. }) {
            return Some(UnsupportedFiniteCheck {
                operation: index,
                region_path: path.clone(),
                provenance: spanned.provenance(),
            });
        }
        for (arm, child) in regions(spanned.operation())
            .into_iter()
            .enumerate()
            .filter_map(|(arm, child)| child.map(|program| (arm, program)))
        {
            path.push((index, arm));
            let found = first_finite_check(child, path);
            path.pop();
            if found.is_some() {
                return found;
            }
        }
    }
    None
}

fn regions(operation: &SolveOperation) -> [Option<&TypedProgram>; 2] {
    match operation {
        SolveOperation::AssignedConditional { program } => [
            Some(program.if_true().body()),
            Some(program.if_false().body()),
        ],
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => [Some(if_true.body()), Some(if_false.body())],
        SolveOperation::Map { body, .. }
        | SolveOperation::CheckAssertion {
            message: SolveAssertionMessage::Captures { program: body },
            ..
        } => [Some(body.body()), None],
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => [
            Some(transition.body()),
            continuation.as_ref().map(|region| region.body()),
        ],
        SolveOperation::CheckFinite { .. }
        | SolveOperation::AssignedState { .. }
        | SolveOperation::Constant { .. }
        | SolveOperation::Load { .. }
        | SolveOperation::Store { .. }
        | SolveOperation::Unary { .. }
        | SolveOperation::Binary { .. }
        | SolveOperation::Compare { .. }
        | SolveOperation::Convert { .. }
        | SolveOperation::Select { .. }
        | SolveOperation::Scale { .. }
        | SolveOperation::BroadcastBinary { .. }
        | SolveOperation::Transpose { .. }
        | SolveOperation::MatrixMultiply { .. }
        | SolveOperation::Cross { .. }
        | SolveOperation::Reduce { .. }
        | SolveOperation::Identity { .. }
        | SolveOperation::Diagonal { .. }
        | SolveOperation::Concatenate { .. }
        | SolveOperation::Fill { .. }
        | SolveOperation::ConstructAggregate { .. }
        | SolveOperation::ProjectElement { .. }
        | SolveOperation::ProjectElementDynamic { .. }
        | SolveOperation::ProjectSlice { .. }
        | SolveOperation::ProjectView { .. }
        | SolveOperation::SelectElement { .. }
        | SolveOperation::UpdateElement { .. }
        | SolveOperation::UpdateSlice { .. }
        | SolveOperation::UpdateView { .. }
        | SolveOperation::Call { .. }
        | SolveOperation::LinearSolve { .. }
        | SolveOperation::Native { .. }
        | SolveOperation::CheckAssertion {
            message: SolveAssertionMessage::NoCaptures,
            ..
        } => [None, None],
    }
}
