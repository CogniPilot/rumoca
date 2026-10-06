//! Typed operators select the existing target catalog, never a second host ABI.
use crate::emit::MathImport;
use rumoca_ir_solve as solve;
use std::collections::BTreeSet;

pub(super) fn unary(operator: solve::SolveUnaryOperator) -> Option<MathImport> {
    use solve::SolveUnaryOperator as U;
    Some(match operator {
        U::Sin => MathImport::Sin,
        U::Cos => MathImport::Cos,
        U::Tan => MathImport::Tan,
        U::Asin => MathImport::Asin,
        U::Acos => MathImport::Acos,
        U::Atan => MathImport::Atan,
        U::Sinh => MathImport::Sinh,
        U::Cosh => MathImport::Cosh,
        U::Tanh => MathImport::Tanh,
        U::Exp => MathImport::Exp,
        U::Log => MathImport::Log,
        U::Log10 => MathImport::Log10,
        _ => return None,
    })
}

pub(super) fn binary(operator: solve::SolveBinaryOperator) -> Option<MathImport> {
    match operator {
        solve::SolveBinaryOperator::Power => Some(MathImport::Pow),
        solve::SolveBinaryOperator::Atan2 => Some(MathImport::Atan2),
        _ => None,
    }
}

pub(super) fn collect(program: &solve::TypedProgram, imports: &mut BTreeSet<MathImport>) {
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::Unary { operator, .. } => imports.extend(unary(*operator)),
            solve::SolveOperation::Binary { operator, .. }
            | solve::SolveOperation::BroadcastBinary { operator, .. } => {
                imports.extend(binary(*operator))
            }
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                collect(if_true.body(), imports);
                collect(if_false.body(), imports);
            }
            solve::SolveOperation::Map { body, .. } => collect(body.body(), imports),
            solve::SolveOperation::Fold {
                transition,
                continuation,
                ..
            } => {
                collect(transition.body(), imports);
                if let Some(predicate) = continuation {
                    collect(predicate.body(), imports);
                }
            }
            _ => {}
        }
    }
}
