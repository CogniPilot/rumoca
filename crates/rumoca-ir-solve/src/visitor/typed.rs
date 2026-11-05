//! Explicit structural traversal of checked typed program owners.
use super::SolveVisitor;
use crate::{SolveAssertionMessage, SolveOperation, SolvePureCallTable, TypedProgram};

/// Visit every canonical primal and available directional owner exactly once.
/// Calls remain leaves: their owners already occur in the same table.
pub fn walk_pure_call_table<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    table: &SolvePureCallTable,
) -> Result<(), V::Error> {
    for owner in table.owners() {
        visitor.visit_typed_program(owner.body())?;
        if let Some(directional) = owner.directional() {
            visitor.visit_typed_program(directional.body())?;
        }
    }
    Ok(())
}

/// Preorder structural walk, retaining each region's checked register owner.
/// A bounded fold's continuation precedes its transition.
pub fn walk_typed_program<V: SolveVisitor + ?Sized>(
    visitor: &mut V,
    program: &TypedProgram,
) -> Result<(), V::Error> {
    for (index, operation) in program.operations().iter().enumerate() {
        visitor.visit_typed_operation(program, index, operation)?;
        for child in nested_typed_programs(operation.operation())
            .into_iter()
            .flatten()
        {
            visitor.visit_typed_program(child)?;
        }
    }
    Ok(())
}

/// Structural children in the canonical source traversal order; no execution
/// or branch selection is performed.
pub(crate) fn nested_typed_programs(operation: &SolveOperation) -> [Option<&TypedProgram>; 2] {
    match operation {
        SolveOperation::AssignedConditional { program } => [
            Some(program.if_true().body()),
            Some(program.if_false().body()),
        ],
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => [Some(if_true.body()), Some(if_false.body())],
        SolveOperation::Map { body, .. } => [Some(body.body()), None],
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => [
            continuation.as_ref().map(|p| p.body()),
            Some(transition.body()),
        ],
        SolveOperation::CheckAssertion { message, .. } => [
            match message {
                SolveAssertionMessage::Captures { program } => Some(program.body()),
                SolveAssertionMessage::NoCaptures => None,
            },
            None,
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
        | SolveOperation::Native { .. } => [None, None],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn typed_walk_returns_the_consumers_first_operation_error() {
        struct Stop;
        impl SolveVisitor for Stop {
            type Error = usize;
            fn visit_typed_operation(
                &mut self,
                _program: &TypedProgram,
                index: usize,
                _operation: &crate::SolveSpannedOperation,
            ) -> Result<(), Self::Error> {
                Err(index)
            }
        }
        let profile = crate::SolveArithmeticProfile::construct(
            crate::SolveRealFormat::Binary64,
            crate::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
        );
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("typed_walk.mo"),
            0,
            1,
        );
        let program = TypedProgram::construct(profile, |builder| {
            builder.constant(crate::SolveValue::boolean(true), span)?;
            builder.constant(crate::SolveValue::boolean(false), span)?;
            Ok(())
        })
        .unwrap();
        assert_eq!(Stop.visit_typed_program(&program), Err(0));
    }
}
