//! Structural zero folding for the symbolic differentiator.
//!
//! Differentiating MultiBody-style array equations produces huge sub-expressions
//! whose value is a literal zero array: `d/dt` of a parameter or constant is
//! `zeros(n)`, and the product / outer-product / skew / transpose rules then wrap
//! those zeros in a growing tower of dead terms — `outerProduct(zeros(3), -e)`,
//! `transpose(zeros(3,3)) * b0.r`, `skew(-zeros(3))`. Folding them away as the
//! derivative is built keeps the prepared DAE proportional to the *live* part of
//! the derivative instead of to the whole differentiated tree.
//!
//! # Why this is not algebra
//!
//! [`is_zero_array`] only accepts a *literal* `zeros(...)` array — optionally
//! negated, transposed, skewed, or an outer product with a literal-zero operand.
//! An arbitrary expression that merely happens to evaluate to zero is never
//! folded. Do NOT generalise this to `0 * x -> 0` for an arbitrary scalar `x`:
//! IEEE-754 makes `0 * NaN` and `0 * inf` equal `NaN`, so dropping `x` would
//! change the computed value.
//!
//! # Why the multiplicative array fold is nevertheless admissible
//!
//! `zeros(m,n) * X` is *not* IEEE-equal to `zeros(m,·)` for arbitrary `X`; the
//! same `0 * NaN` reasoning applies elementwise. The fold is admissible here for
//! a reason that has nothing to do with the zero and everything to do with where
//! `X` comes from, and it does not transfer to the scalar rule above:
//!
//! * Every multiplicative zero this module folds is `d/dt(u)` for a `u` the
//!   differentiator proved time-invariant, produced by the product rule
//!   `d(u*X) = d(u)*X + u*d(X)`.
//! * `X` is therefore not an arbitrary expression: it is, verbatim, a factor of
//!   the product `u*X` in the *undifferentiated* constraint row, which the
//!   reduced system retains (MLS 3.7 manifold preservation — see
//!   `reduce_constrained_dummy_derivatives`).
//! * Non-finite values propagate through `+`, `-` and `*`, so a non-finite `X`
//!   makes the retained constraint row's residual non-finite too. The reduced
//!   system is only defined where that row is satisfied, so the differentiated
//!   row's value at such a point is unobservable: it can never be the difference
//!   between a converged solution and a wrong one.
//!
//! The scalar rule has no such companion row — `0 * x` is a value the model
//! wrote, and `x` need not appear anywhere else — which is exactly why it stays
//! refused.
//!
//! Every fold also preserves the shape: a term is only replaced by
//! `zeros(dims)` when `dims` is statically known *for both operands*, and an
//! additive zero is only dropped when both sides have the same statically known
//! dimensions. When either shape cannot be determined the original expression is
//! kept — `matrix_product_dims` answers a shape *query* with the one side it
//! knows, which is the wrong answer for a shape-preserving *rewrite*.

use super::*;

/// Which operand of an additive expression survives the fold.
enum Keep {
    Lhs,
    Rhs,
}

/// True when `expr` is a literal zero array.
pub(super) fn is_zero_array(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Zeros,
            ..
        } => true,
        Expression::BuiltinCall {
            function: BuiltinFunction::Skew | BuiltinFunction::Transpose,
            args,
            ..
        } => args.len() == 1 && is_zero_array(&args[0]),
        Expression::BuiltinCall {
            function: BuiltinFunction::OuterProduct | BuiltinFunction::Cross,
            args,
            ..
        } => args.len() == 2 && args.iter().any(is_zero_array),
        Expression::Unary {
            op: OpUnary::Minus | OpUnary::DotMinus,
            rhs,
            ..
        } => is_zero_array(rhs),
        _ => false,
    }
}

/// `lhs op rhs`, with structural zeros folded out.
pub(super) fn binary(
    dae: &Dae,
    op: OpBinary,
    lhs: Expression,
    rhs: Expression,
    span: Span,
) -> Expression {
    match op {
        OpBinary::Mul | OpBinary::MulElem => {
            if (is_zero_array(&lhs) || is_zero_array(&rhs))
                && let Some(zero) = zero_array(product_dims(dae, &op, &lhs, &rhs), span)
            {
                return zero;
            }
        }
        OpBinary::Add | OpBinary::AddElem | OpBinary::Sub | OpBinary::SubElem => {
            match additive_identity(dae, &op, &lhs, &rhs) {
                Some(Keep::Lhs) => return lhs,
                Some(Keep::Rhs) => return rhs,
                None => {}
            }
        }
        _ => {}
    }
    make_binary(op, lhs, rhs, span)
}

/// `derivative op other` (or `other op derivative`), cloning `other` only when
/// the product survives the fold.
///
/// This is the product rule's hot path: `d(a*b) = da*b + a*db` otherwise clones
/// both operands even when one factor is a literal zero and the whole term is
/// about to be discarded.
pub(super) fn scaled(
    dae: &Dae,
    op: OpBinary,
    derivative: Expression,
    other: &Expression,
    derivative_first: bool,
    span: Span,
) -> Expression {
    if is_zero_array(&derivative) {
        let dims = if derivative_first {
            product_dims(dae, &op, &derivative, other)
        } else {
            product_dims(dae, &op, other, &derivative)
        };
        if let Some(zero) = zero_array(dims, span) {
            return zero;
        }
    }
    if derivative_first {
        make_binary(op, derivative, other.clone(), span)
    } else {
        make_binary(op, other.clone(), derivative, span)
    }
}

/// One half of `d(outerProduct(a, b)) = outerProduct(da, b) + outerProduct(a, db)`,
/// cloning the undifferentiated operand only when the term survives the fold.
pub(super) fn outer_product_term(
    dae: &Dae,
    derivative: Expression,
    other: &Expression,
    derivative_first: bool,
    span: Span,
) -> Expression {
    if is_zero_array(&derivative) {
        let (lhs, rhs) = if derivative_first {
            (&derivative, other)
        } else {
            (other, &derivative)
        };
        if let Some(zero) = zero_array(outer_product_dims(dae, lhs, rhs), span) {
            return zero;
        }
    }
    let args = if derivative_first {
        vec![derivative, other.clone()]
    } else {
        vec![other.clone(), derivative]
    };
    array_builtin(dae, BuiltinFunction::OuterProduct, args, span)
}

/// `-rhs`, folded when `rhs` is a literal zero array.
pub(super) fn unary(dae: &Dae, op: OpUnary, rhs: Expression, span: Span) -> Expression {
    if matches!(op, OpUnary::Minus | OpUnary::DotMinus)
        && is_zero_array(&rhs)
        && let Some(zero) = zero_array(fold_dims(&rhs, dae), span)
    {
        return zero;
    }
    make_unary(op, rhs, span)
}

/// An array builtin call, folded when a literal-zero operand makes it zero.
pub(super) fn array_builtin(
    dae: &Dae,
    function: BuiltinFunction,
    args: Vec<Expression>,
    span: Span,
) -> Expression {
    let has_zero_operand = matches!(
        function,
        BuiltinFunction::Skew
            | BuiltinFunction::Transpose
            | BuiltinFunction::OuterProduct
            | BuiltinFunction::Cross
    ) && args.iter().any(is_zero_array);
    let node = Expression::BuiltinCall {
        function,
        args,
        span,
    };
    if has_zero_operand && let Some(zero) = zero_array(fold_dims(&node, dae), span) {
        return zero;
    }
    node
}

fn additive_identity(dae: &Dae, op: &OpBinary, lhs: &Expression, rhs: &Expression) -> Option<Keep> {
    let lhs_dims = fold_dims(lhs, dae)?;
    // Both sides must carry the same statically known shape, so dropping one of
    // them cannot change the result shape (`zeros(3) .+ scalar` is a 3-vector).
    if fold_dims(rhs, dae)? != lhs_dims {
        return None;
    }
    if is_zero_array(rhs) {
        return Some(Keep::Lhs);
    }
    if is_zero_array(lhs) && matches!(op, OpBinary::Add | OpBinary::AddElem) {
        return Some(Keep::Rhs);
    }
    None
}

fn zero_array(dims: Option<Vec<i64>>, span: Span) -> Option<Expression> {
    let dims = dims?;
    (!dims.is_empty()).then(|| zeros_for_dims(&dims, span))
}

/// Result shape of a product the fold is about to replace by a zero array.
///
/// [`matrix_product_dims`] answers with whichever operand it does know when the
/// other shape is unmodelled. That is the right answer for a shape *query* and
/// the wrong one for a shape-preserving *rewrite*: `zeros(3,3) * x` is a
/// 3-vector when `x` is a 3-vector and a 3x3 matrix when `x` is a matrix, so
/// folding on the known side alone can silently change the width of a row. This
/// wrapper therefore refuses unless the other operand's shape is known too, or
/// it is a proven scalar — in which case MLS 10.6 keeps the array operand's own
/// shape and there is nothing to invent.
fn product_dims(dae: &Dae, op: &OpBinary, lhs: &Expression, rhs: &Expression) -> Option<Vec<i64>> {
    let lhs_dims = modelled_operand_dims(dae, lhs)?;
    let rhs_dims = modelled_operand_dims(dae, rhs)?;
    match op {
        OpBinary::Mul => matrix_product_dims(lhs_dims, rhs_dims),
        _ => lhs_dims.or(rhs_dims),
    }
}

/// `Some(Some(dims))` for an array of statically known shape, `Some(None)` for a
/// proven scalar, and `None` when the operand's shape is not modelled at all.
fn modelled_operand_dims(dae: &Dae, expr: &Expression) -> Option<Option<Vec<i64>>> {
    match fold_dims(expr, dae) {
        Some(dims) => Some(Some(dims)),
        None => expression_is_scalar(expr, dae).then_some(None),
    }
}

/// [`expression_dims`] extended with the array builtins it does not model.
///
/// Kept separate from `expression_dims` so that widening the shape model stays
/// scoped to zero folding and cannot change any other differentiation decision.
pub(super) fn fold_dims(expr: &Expression, dae: &Dae) -> Option<Vec<i64>> {
    match expr {
        Expression::Unary { rhs, .. } => fold_dims(rhs, dae),
        Expression::Binary { op, lhs, rhs, .. } => binary_dims(dae, op, lhs, rhs),
        Expression::BuiltinCall { function, args, .. } => builtin_dims(dae, *function, args),
        _ => expression_dims(expr, dae),
    }
}

fn binary_dims(dae: &Dae, op: &OpBinary, lhs: &Expression, rhs: &Expression) -> Option<Vec<i64>> {
    let lhs_dims = fold_dims(lhs, dae);
    let rhs_dims = fold_dims(rhs, dae);
    match op {
        OpBinary::Mul => matrix_product_dims(lhs_dims, rhs_dims),
        OpBinary::Div => lhs_dims,
        OpBinary::Add
        | OpBinary::AddElem
        | OpBinary::Sub
        | OpBinary::SubElem
        | OpBinary::MulElem
        | OpBinary::DivElem => lhs_dims.or(rhs_dims),
        _ => None,
    }
}

fn builtin_dims(dae: &Dae, function: BuiltinFunction, args: &[Expression]) -> Option<Vec<i64>> {
    match (function, args) {
        (BuiltinFunction::Der, [arg]) => fold_dims(arg, dae),
        (BuiltinFunction::Zeros | BuiltinFunction::Ones, [_, ..]) => literal_dims(args),
        (BuiltinFunction::Identity, [n]) => literal_dim(n).map(|n| vec![n, n]),
        (BuiltinFunction::Skew, [_]) => Some(vec![3, 3]),
        (BuiltinFunction::Cross, [_, _]) => Some(vec![3]),
        (BuiltinFunction::Transpose, [arg]) => match fold_dims(arg, dae)?.as_slice() {
            [rows, cols] => Some(vec![*cols, *rows]),
            _ => None,
        },
        (BuiltinFunction::OuterProduct, [lhs, rhs]) => outer_product_dims(dae, lhs, rhs),
        _ => None,
    }
}

fn outer_product_dims(dae: &Dae, lhs: &Expression, rhs: &Expression) -> Option<Vec<i64>> {
    match (
        fold_dims(lhs, dae)?.as_slice(),
        fold_dims(rhs, dae)?.as_slice(),
    ) {
        ([rows], [cols]) => Some(vec![*rows, *cols]),
        _ => None,
    }
}

fn literal_dims(args: &[Expression]) -> Option<Vec<i64>> {
    args.iter().map(literal_dim).collect()
}

fn literal_dim(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("dae_prepare_zero_fold_tests.mo"),
            1,
            2,
        )
    }

    fn model() -> Dae {
        let mut model = Dae::default();
        for (name, dims) in [("m", vec![3, 3]), ("v", vec![3]), ("w", vec![3])] {
            let mut variable = Variable::new(VarName::new(name), span());
            variable.dims = dims;
            variable.source_span = span();
            model
                .variables
                .algebraics
                .insert(VarName::new(name), variable);
        }
        let mut scalar = Variable::new(VarName::new("s"), span());
        scalar.source_span = span();
        model.variables.algebraics.insert(VarName::new("s"), scalar);
        model
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::new(name),
            subscripts: vec![],
            span: span(),
        }
    }

    fn zeros(dims: &[i64]) -> Expression {
        zeros_for_dims(dims, span())
    }

    #[test]
    fn literal_zero_matrix_times_matrix_folds_to_a_zero_matrix() {
        let folded = binary(&model(), OpBinary::Mul, zeros(&[3, 3]), var("m"), span());
        assert_eq!(folded, zeros(&[3, 3]));
    }

    #[test]
    fn literal_zero_matrix_times_vector_keeps_the_vector_shape() {
        let folded = binary(&model(), OpBinary::Mul, zeros(&[3, 3]), var("v"), span());
        assert_eq!(folded, zeros(&[3]));
    }

    #[test]
    fn additive_zero_of_matching_shape_is_dropped() {
        let model = model();
        assert_eq!(
            binary(&model, OpBinary::Add, var("v"), zeros(&[3]), span()),
            var("v")
        );
        assert_eq!(
            binary(&model, OpBinary::Sub, var("v"), zeros(&[3]), span()),
            var("v")
        );
        assert_eq!(
            binary(&model, OpBinary::Add, zeros(&[3]), var("v"), span()),
            var("v")
        );
    }

    #[test]
    fn subtracting_from_a_zero_array_keeps_the_negation() {
        // `0 - x` is `-x`, not `x`: only the right-hand identity may be dropped.
        let folded = binary(&model(), OpBinary::Sub, zeros(&[3]), var("v"), span());
        assert!(matches!(folded, Expression::Binary { .. }));
    }

    #[test]
    fn additive_zero_of_unknown_or_different_shape_is_kept() {
        // `zeros(3) .+ s` broadcasts to a 3-vector; dropping either side would
        // change the result shape, so nothing is folded.
        let folded = binary(&model(), OpBinary::AddElem, zeros(&[3]), var("s"), span());
        assert!(matches!(folded, Expression::Binary { .. }));
    }

    #[test]
    fn skew_and_transpose_of_a_literal_zero_fold_to_the_right_shape() {
        let model = model();
        assert_eq!(
            array_builtin(&model, BuiltinFunction::Skew, vec![zeros(&[3])], span()),
            zeros(&[3, 3])
        );
        assert_eq!(
            array_builtin(
                &model,
                BuiltinFunction::Transpose,
                vec![zeros(&[3, 2])],
                span()
            ),
            zeros(&[2, 3])
        );
    }

    #[test]
    fn outer_product_with_a_literal_zero_operand_folds_to_a_zero_matrix() {
        let folded = array_builtin(
            &model(),
            BuiltinFunction::OuterProduct,
            vec![zeros(&[3]), var("w")],
            span(),
        );
        assert_eq!(folded, zeros(&[3, 3]));
    }

    #[test]
    fn negating_a_literal_zero_array_folds() {
        let folded = unary(&model(), OpUnary::Minus, zeros(&[3]), span());
        assert_eq!(folded, zeros(&[3]));
    }

    /// A `zeros(3,3) * x` whose right operand has no modelled shape must be
    /// kept: the product is a 3-vector when `x` is a vector and a 3x3 matrix
    /// when `x` is a matrix, so folding on the known side alone would decide the
    /// row width from one operand.
    #[test]
    fn zero_matrix_times_an_unmodelled_operand_is_kept() {
        let model = model();
        let unmodelled = Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var("unknown_shape")],
            span: span(),
        };
        assert_eq!(fold_dims(&unmodelled, &model), None);
        assert!(!expression_is_scalar(&unmodelled, &model));
        let folded = binary(
            &model,
            OpBinary::Mul,
            zeros(&[3, 3]),
            unmodelled.clone(),
            span(),
        );
        assert!(
            matches!(folded, Expression::Binary { .. }),
            "unknown right shape must block the fold"
        );
        let scaled_term = scaled(
            &model,
            OpBinary::Mul,
            zeros(&[3, 3]),
            &unmodelled,
            true,
            span(),
        );
        assert!(
            matches!(scaled_term, Expression::Binary { .. }),
            "the product-rule hot path must block on the same unknown shape"
        );
    }

    /// A proven scalar operand leaves the zero array's own shape intact, so the
    /// fold is admissible with only one modelled shape.
    #[test]
    fn zero_array_times_a_proven_scalar_keeps_the_array_shape() {
        let model = model();
        let folded = binary(&model, OpBinary::MulElem, zeros(&[3]), var("s"), span());
        assert_eq!(folded, zeros(&[3]));
    }

    /// The array counterpart of the scalar `0 * x` refusal, pinned deliberately.
    ///
    /// This *does* fold, and the module header says why: the surviving operand
    /// is a factor of the retained undifferentiated constraint row, so a
    /// non-finite value there already makes that row's residual non-finite and
    /// the differentiated row unobservable. The scalar case has no such
    /// companion row and stays refused — see
    /// `expressions_that_are_not_literal_zeros_are_never_folded`.
    #[test]
    fn zero_array_times_a_live_array_folds_by_the_companion_row_argument() {
        let model = model();
        assert_eq!(
            binary(&model, OpBinary::Mul, zeros(&[3, 3]), var("m"), span()),
            zeros(&[3, 3])
        );
        assert_eq!(
            array_builtin(
                &model,
                BuiltinFunction::OuterProduct,
                vec![zeros(&[3]), var("w")],
                span()
            ),
            zeros(&[3, 3])
        );
    }

    #[test]
    fn expressions_that_are_not_literal_zeros_are_never_folded() {
        let model = model();
        // A product of two live matrices must survive untouched...
        let live = binary(&model, OpBinary::Mul, var("m"), var("m"), span());
        assert!(matches!(live, Expression::Binary { .. }));
        // ...and a scalar literal `0.0` is not a structural zero array, so the
        // IEEE-sensitive `0 * x` rewrite never happens.
        let scalar_zero = Expression::Literal {
            value: Literal::Real(0.0),
            span: span(),
        };
        assert!(!is_zero_array(&scalar_zero));
        let kept = binary(&model, OpBinary::Mul, scalar_zero, var("m"), span());
        assert!(matches!(kept, Expression::Binary { .. }));
    }

    #[test]
    fn scaled_folds_a_zero_derivative_without_materialising_the_other_operand() {
        let model = model();
        let folded = scaled(
            &model,
            OpBinary::Mul,
            zeros(&[3, 3]),
            &var("m"),
            true,
            span(),
        );
        assert_eq!(folded, zeros(&[3, 3]));
        let kept = scaled(&model, OpBinary::Mul, var("m"), &var("m"), true, span());
        assert!(matches!(kept, Expression::Binary { .. }));
    }
}
