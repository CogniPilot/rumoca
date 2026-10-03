use rumoca_ir_dae as dae;

/// These checked builtins are linear in every value operand and retain their
/// index map when applied to a derivative of the same shape; `sum` reduces
/// its one operand, so its derivative is the sum of the operand's derivative.
pub(super) fn is_linear_tensor_map(builtin: dae::PureBuiltin) -> bool {
    matches!(
        builtin,
        dae::PureBuiltin::Vector
            | dae::PureBuiltin::Sum
            | dae::PureBuiltin::Transpose
            | dae::PureBuiltin::Diagonal
            | dae::PureBuiltin::Skew
            | dae::PureBuiltin::PromotedCat1
            | dae::PureBuiltin::PromotedCat2
    )
}

/// Both value preflight and reconstruction consume this profile. The DAE
/// constructor owns arity and shape; the caller proves each operand can be
/// reconstructed from exact state or invariant values.
pub(super) fn is_materializable_builtin(builtin: dae::PureBuiltin) -> bool {
    is_linear_tensor_map(builtin)
        || matches!(
            builtin,
            dae::PureBuiltin::Zeros
                | dae::PureBuiltin::Ones
                | dae::PureBuiltin::Fill
                | dae::PureBuiltin::Identity
                | dae::PureBuiltin::Cross
                | dae::PureBuiltin::OuterProduct
                | dae::PureBuiltin::Sin
                | dae::PureBuiltin::Cos
                | dae::PureBuiltin::Sqrt
                | dae::PureBuiltin::Exp
                | dae::PureBuiltin::Log
                | dae::PureBuiltin::Atan2
                | dae::PureBuiltin::LinearSolve
        )
}

/// Arithmetic whose source tensor operation has an exact rule through order two.
pub(super) fn is_differentiable_binary(operator: dae::BinaryOperator) -> bool {
    matches!(
        operator,
        dae::BinaryOperator::Add
            | dae::BinaryOperator::Subtract
            | dae::BinaryOperator::Multiply
            | dae::BinaryOperator::Divide
            | dae::BinaryOperator::ElementwiseAdd
            | dae::BinaryOperator::ElementwiseSubtract
            | dae::BinaryOperator::ElementwiseMultiply
            | dae::BinaryOperator::ElementwiseDivide
    )
}

/// Scalar `u ^ v` whose exponent `v` is time-invariant has the closed
/// derivative `v * u^(v-1) * du` (and, at order two,
/// `v*(v-1)*u^(v-2)*du^2 + v*u^(v-1)*d2u`). Only the base is differentiated.
pub(super) fn is_differentiable_power<'dae>(
    view: dae::DaeView<'dae>,
    operator: dae::BinaryOperator,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> bool {
    operator == dae::BinaryOperator::Power
        && super::equalities::is_time_invariant(view, rhs)
        && [lhs, rhs].into_iter().all(|operand| {
            view.expression(operand)
                .is_some_and(|node| node.value_type().dimensions().is_empty())
        })
}

/// The structural preflight and reconstruction share this closed derivative profile.
pub(super) fn is_differentiable_builtin(builtin: dae::PureBuiltin, order: u8) -> bool {
    (1..=2).contains(&order) && is_materializable_builtin(builtin)
}
