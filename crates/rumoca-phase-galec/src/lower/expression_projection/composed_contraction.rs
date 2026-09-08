//! Composed tensor contractions: `A*X*A'` read as one node, not as a nest.
//!
//! A quadratic form is a contraction whose left operand is itself a
//! contraction. Its value `(A*X)[row][contracted]` is reused by every result
//! column, so the composed node materializes it once as an explicit rank-1
//! tensor over the contracted axis and contracts that tensor with the right
//! operand. Both facts are read off the checked DAE at the node: the operand's
//! own operation says it is a contraction, its operand shapes say what the
//! intermediate's extent is, and this node's own outer index says the
//! intermediate is replicated across something.
//!
//! Nothing here inspects a lowered statement. The intermediate is an IR value
//! with its own declaration and its own liveness, decided before either
//! operand is projected, which is what retires the loop fission that used to
//! recover the same shape by splitting an already-emitted body.

use super::*;

/// One composed contraction: the operands of the left contraction, and the
/// contraction shape projecting it at this node's contracted index takes.
pub(super) struct ComposedContraction<'dae> {
    pub(super) lhs: dae::ExprId<'dae>,
    pub(super) rhs: dae::ExprId<'dae>,
    pub(super) contraction: TensorContraction,
    pub(super) scalar_type: gast::ScalarType,
    pub(super) span: Span,
}

/// Where a materialized contraction accumulates.
pub(super) enum ContractionAccumulator {
    /// A scalar temporary the contraction declares for itself.
    Fresh,
    /// One element of the intermediate tensor a composing contraction
    /// declared, which the composing node reads back at the same index.
    Element(Box<IntermediateElement>),
}

/// The element of an intermediate tensor one contracted value owns, with the
/// shape of the tensor it belongs to.
pub(super) struct IntermediateElement {
    /// The intermediate tensor.
    pub(super) name: gast::Name,
    /// Elements in it: one per value of the composing node's contracted index.
    pub(super) count: u32,
    /// That index, which this contraction never reads.
    pub(super) row: gast::Name,
    /// The element this contraction accumulates into.
    pub(super) reference: gast::Reference,
}

/// Attest that this contraction's loop is a whole row of a matrix product, so
/// the target may accumulate the intermediate run at once instead of walking a
/// column per element.
///
/// Three facts, all owned by this node: the accumulator is the intermediate
/// tensor it was handed, which is a temporary the composing node declared and
/// therefore aliases neither operand; the coefficient is the left operand
/// projected without the row index, so no value of that index can change it,
/// and it is refused unless it is pure arithmetic, because the row form
/// evaluates it once per contracted value rather than once per element; and
/// the right operand is a reference whose last subscript is the row index, so
/// dropping it names one run per contracted value.
pub(super) fn issue_real_matrix_multiply_occurrence(
    format: rumoca_ir_galec::package::AlgorithmCodeRealFormat,
    element: &IntermediateElement,
    iterator: &gast::Name,
    extent: u32,
    scale: &gast::Expression,
    source: &gast::Expression,
    seed: gast::RealMatrixMultiplySeed,
) -> Option<gast::RealMatrixMultiplyOccurrenceContract> {
    if !is_hoistable_coefficient(scale) || mentions(scale, &element.row) {
        return None;
    }
    let gast::Expression::Ref(gast::Reference::Local(source_part)) = source else {
        return None;
    };
    let (last, rest) = source_part.subscripts.split_last()?;
    if !is_reference_to(last, &element.row)
        || rest.iter().any(|index| mentions(index, &element.row))
    {
        return None;
    }
    let gast::Reference::Local(mut target_part) = element.reference.clone() else {
        return None;
    };
    let target_row = target_part.subscripts.pop()?;
    if !is_reference_to(&target_row, &element.row) || target_part.name != element.name {
        return None;
    }
    let mut source_part = source_part.clone();
    source_part.subscripts.pop();
    Some(gast::RealMatrixMultiplyOccurrenceContract::new(
        format,
        (gast::Reference::Local(target_part), element.count),
        (iterator.clone(), extent),
        (scale.clone(), gast::Reference::Local(source_part)),
        seed,
    ))
}

/// Whether an expression may be evaluated once per contracted value instead of
/// once per element: literals, reads and arithmetic only. A call or a
/// comparison would have its evaluation count changed, and a conditional holds
/// a legalized twin that a rewrite of one arm would leave stale.
fn is_hoistable_coefficient(value: &gast::Expression) -> bool {
    match value {
        gast::Expression::Bool(_) | gast::Expression::Integer(_) | gast::Expression::Real(_) => {
            true
        }
        gast::Expression::Ref(_) | gast::Expression::Neg(_) => true,
        gast::Expression::Paren(inner) => is_hoistable_coefficient(inner),
        gast::Expression::Binary { op, lhs, rhs } => {
            matches!(
                op,
                gast::BinaryOp::Add
                    | gast::BinaryOp::Sub
                    | gast::BinaryOp::Mul
                    | gast::BinaryOp::Div
                    | gast::BinaryOp::Pow
            ) && is_hoistable_coefficient(lhs)
                && is_hoistable_coefficient(rhs)
        }
        gast::Expression::Size { .. }
        | gast::Expression::Call(_)
        | gast::Expression::Not(_)
        | gast::Expression::If(_)
        | gast::Expression::Array(_) => false,
    }
}

fn is_reference_to(value: &gast::Expression, name: &gast::Name) -> bool {
    matches!(
        value,
        gast::Expression::Ref(gast::Reference::Local(part))
            if part.name == *name && part.subscripts.is_empty()
    )
}

fn mentions(value: &gast::Expression, name: &gast::Name) -> bool {
    any_expression(value, &mut |node| match node {
        gast::Expression::Ref(reference) | gast::Expression::Neg(reference) => {
            Some(mentions_in_reference(reference, name))
        }
        gast::Expression::Size { array, dimension } => {
            Some(mentions_in_reference(array, name) || mentions(dimension, name))
        }
        _ => None,
    })
}

fn mentions_in_reference(reference: &gast::Reference, name: &gast::Name) -> bool {
    reference_parts(reference)
        .iter()
        .any(|part| part.name == *name || part.subscripts.iter().any(|index| mentions(index, name)))
}

/// Read `lhs` as the left contraction of a composed contraction, or `None`
/// when this node is not one.
///
/// Four conditions, all answered by the checked DAE and this node's own shape:
///
/// * the node replicates over an outer index on the right, so an intermediate
///   value is reused rather than consumed once;
/// * the left operand is a matrix, so projecting it names a row of a matrix
///   rather than an element of a vector;
/// * the left operand's own operation is `*`; and
/// * its operand shapes make that `*` a contraction at this node's own
///   contracted index.
pub(super) fn composed_left_contraction<'dae>(
    view: dae::DaeView<'dae>,
    lhs: dae::ExprId<'dae>,
    contraction: &TensorContraction,
    contracted: &gast::Expression,
) -> Option<ComposedContraction<'dae>> {
    if contraction.rhs_outer.is_empty() || !contraction.lhs_matrix {
        return None;
    }
    let node = view.expression(lhs)?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Multiply,
        lhs: inner_lhs,
        rhs: inner_rhs,
    } = node.operation()
    else {
        return None;
    };
    let indices = [contraction.lhs_outer.first()?.clone(), contracted.clone()];
    let inner = tensor_contraction(
        view.expression(inner_lhs)?.value_type().dimensions(),
        view.expression(inner_rhs)?.value_type().dimensions(),
        &indices,
    )?;
    let span = node.provenance().span();
    let scalar_type = scalar_type(node.value_type().scalar_type(), "<contraction>", span).ok()?;
    Some(ComposedContraction {
        lhs: inner_lhs,
        rhs: inner_rhs,
        contraction: inner,
        scalar_type,
        span,
    })
}

/// The element of an intermediate tensor one contracted value owns.
pub(super) fn intermediate_element(
    name: &gast::Name,
    contracted: &gast::Expression,
) -> gast::Reference {
    let mut part = gast::RefPart::plain(name.clone());
    part.subscripts.push(contracted.clone());
    gast::Reference::Local(part)
}

#[cfg(test)]
mod mentions_tests {
    use super::*;

    fn name(text: &str) -> gast::Name {
        gast::Name::ident(text)
    }

    fn local(text: &str) -> gast::Expression {
        gast::Expression::Ref(gast::Reference::local(name(text)))
    }

    #[test]
    fn matrix_occurrence_issuer_retains_the_complete_arithmetic_relation() {
        let iterator = name("contracted");
        let row = name("column");
        let target_name = name("intermediate");
        let target = intermediate_element(&target_name, &local("column"));
        let element = IntermediateElement {
            name: target_name.clone(),
            count: 4,
            row: row.clone(),
            reference: target,
        };
        let mut source = gast::RefPart::plain(name("source"));
        source.subscripts = vec![local("contracted"), local("column")];
        let scale = local("coefficient");
        let occurrence = issue_real_matrix_multiply_occurrence(
            rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary32,
            &element,
            &iterator,
            3,
            &scale,
            &gast::Expression::Ref(gast::Reference::Local(source)),
            gast::RealMatrixMultiplySeed::PositiveZero,
        )
        .expect("checked contraction issues one occurrence");

        assert_eq!(
            occurrence.format(),
            rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary32
        );
        assert_eq!(
            occurrence.target_run(),
            &gast::Reference::local(target_name)
        );
        assert_eq!(occurrence.count(), 4);
        assert_eq!(occurrence.iterator(), &iterator);
        assert_eq!(occurrence.extent(), 3);
        assert_eq!(occurrence.scale(), &scale);
        let mut expected_source = gast::RefPart::plain(name("source"));
        expected_source.subscripts.push(local("contracted"));
        assert_eq!(
            occurrence.source_run(),
            &gast::Reference::Local(expected_source)
        );
        assert_eq!(
            occurrence.seed(),
            &gast::RealMatrixMultiplySeed::PositiveZero
        );
        assert_eq!(
            occurrence.primitive_rounding(),
            gast::RealMatrixMultiplyRounding::RoundToNearestTiesToEven
        );
        assert_eq!(
            occurrence.contraction(),
            gast::RealMatrixMultiplyContraction::SeparateMultiplyAdd
        );
        assert_eq!(
            occurrence.intermediate_precision(),
            gast::RealMatrixMultiplyIntermediatePrecision::AccumulatorFormatOnly
        );
        assert_eq!(
            occurrence.final_rounding(),
            gast::RealMatrixMultiplyFinalRounding::None
        );
        assert_eq!(
            occurrence.signed_zero(),
            gast::RealMatrixMultiplySignedZero::IeeePrimitiveResult
        );
        assert_eq!(
            occurrence.nan(),
            gast::RealMatrixMultiplyNan::QuietPayloadAndSignQuotient
        );
        assert_eq!(
            occurrence.infinity(),
            gast::RealMatrixMultiplyInfinity::IeeePrimitiveResult
        );
        assert_eq!(
            occurrence.subnormal(),
            gast::RealMatrixMultiplySubnormal::GradualUnderflow
        );
        assert_eq!(
            occurrence.status(),
            gast::RealMatrixMultiplyStatus::NoObservableFloatingStatus
        );
    }

    /// A call's arguments are ordinary expressions and are searched in full:
    /// the intermediate must not be renamed out from under a call that reads
    /// it.
    #[test]
    fn call_arguments_are_searched() {
        let call = gast::Expression::Call(gast::FunctionCall {
            function: name("f"),
            arguments: vec![gast::Expression::Integer(1), local("k")],
        });
        assert!(mentions(&call, &name("k")));
        assert!(!mentions(&call, &name("j")));
    }

    /// Both halves of every branch pair are searched, and so is the mandatory
    /// else value.
    #[test]
    fn conditional_branches_and_else_are_searched() {
        let in_condition = gast::Expression::If(gast::IfExpression::new(
            vec![(local("k"), gast::Expression::Integer(1))],
            gast::Expression::Integer(0),
        ));
        assert!(mentions(&in_condition, &name("k")));

        let in_value = gast::Expression::If(gast::IfExpression::new(
            vec![(gast::Expression::Bool(true), local("k"))],
            gast::Expression::Integer(0),
        ));
        assert!(mentions(&in_value, &name("k")));

        let in_else = gast::Expression::If(gast::IfExpression::new(
            vec![(gast::Expression::Bool(true), gast::Expression::Integer(1))],
            local("k"),
        ));
        assert!(mentions(&in_else, &name("k")));

        let absent = gast::Expression::If(gast::IfExpression::new(
            vec![(gast::Expression::Bool(true), gast::Expression::Integer(1))],
            gast::Expression::Integer(0),
        ));
        assert!(!mentions(&absent, &name("k")));
    }

    #[test]
    fn array_elements_are_searched() {
        let array = gast::Expression::Array(vec![gast::Expression::Integer(1), local("k")]);
        assert!(mentions(&array, &name("k")));
        assert!(!mentions(&array, &name("j")));
    }

    /// A subscript is an expression, so a name reached only through one is
    /// still a mention; a bare literal never is.
    #[test]
    fn subscripts_are_searched_and_literals_are_not() {
        let mut part = gast::RefPart::plain(name("a"));
        part.subscripts = vec![local("k")];
        let subscripted = gast::Expression::Ref(gast::Reference::Local(part));
        assert!(mentions(&subscripted, &name("k")));

        assert!(!mentions(&gast::Expression::Real(1.5), &name("k")));
        assert!(!mentions(&gast::Expression::Bool(false), &name("k")));
        assert!(!mentions(&gast::Expression::Integer(3), &name("k")));
    }

    /// `size(a, k)` reaches the name through its dimension argument.
    #[test]
    fn size_dimension_is_searched() {
        let query = gast::Expression::Size {
            array: gast::Reference::local(name("a")),
            dimension: Box::new(local("k")),
        };
        assert!(mentions(&query, &name("k")));
    }

    #[test]
    fn a_bare_local_reference_is_recognized_exactly() {
        assert!(is_reference_to(&local("k"), &name("k")));
        assert!(!is_reference_to(&local("j"), &name("k")));

        let mut part = gast::RefPart::plain(name("k"));
        part.subscripts = vec![gast::Expression::Integer(1)];
        let subscripted = gast::Expression::Ref(gast::Reference::Local(part));
        assert!(!is_reference_to(&subscripted, &name("k")));
    }
}
