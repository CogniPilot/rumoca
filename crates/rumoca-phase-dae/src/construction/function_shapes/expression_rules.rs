//! The exact shape rule for each expression form a function scope may contain.
//!
//! Every rule reads one [`ShapeEnvironment`] and either proves an exact shape
//! or reports the operand it could not prove. Nothing here approximates: MLS
//! §12.2 makes a constructed function's extents part of its identity, so a
//! widened or defaulted shape would be a wrong DAE function rather than a
//! deferred question.

use super::*;

/// The shape this scope proves for `expression` without minting anything.
///
/// `None` means "not proven here", which is a valid answer rather than an
/// error: the caller is checking a statement MLS §11.5 does not execute, so it
/// owns no specialization for a callee and no proof of an extent only the
/// unexecuted path settles. Every function call is therefore refused a result
/// shape ([`reject_shape_call`]) instead of being specialized.
pub(in crate::construction) fn call_free_expression_shape(
    expression: &Expression,
    values: &ShapeEnvironment,
) -> Option<ValueShape> {
    expression_shape(expression, values, &mut reject_shape_call).ok()
}

/// The shape this scope proves for one assignment target, after its subscripts.
///
/// MLS §11.2.1 writes the left-hand side as a component reference with
/// subscripts, so the target's shape is the declared shape with those
/// subscripts applied. `None` is again "not proven here".
pub(in crate::construction) fn call_free_target_shape(
    component: &rumoca_core::ComponentReference,
    values: &ShapeEnvironment,
) -> Option<ValueShape> {
    let [target] = component.parts() else {
        return None;
    };
    let declared = values.get(&VarName::new(&target.ident))?.clone();
    apply_subscripts(declared, &target.subs, values).ok()
}

pub(super) type FunctionResultShape<'scope> = dyn FnMut(&rumoca_core::Reference, &[Expression], bool, Span) -> Result<ValueShape, ToDaeError>
    + 'scope;

pub(super) fn expression_shape(
    expression: &Expression,
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
) -> Result<ValueShape, ToDaeError> {
    let span = expression_span(expression)?;
    match expression {
        Expression::Literal { .. } => Ok(Vec::new()),
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let shape = values
                .get(name.var_name())
                .cloned()
                .ok_or_else(|| ToDaeError::unresolved_reference(name.as_str(), span))?;
            apply_subscripts(shape, subscripts, values)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let shape = expression_shape(base, values, function_result)?;
            apply_subscripts(shape, subscripts, values)
        }
        Expression::Unary { rhs, .. } => expression_shape(rhs, values, function_result),
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = expression_shape(lhs, values, function_result)?;
            let rhs = expression_shape(rhs, values, function_result)?;
            binary_shape(op.clone(), lhs, rhs, span)
        }
        Expression::BuiltinCall { function, args, .. } => {
            builtin_shape(*function, args, values, function_result, span)
        }
        Expression::StringConversion { value, format, .. } => {
            string_conversion_shape(value, format, values, function_result, span)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } if *is_constructor && name.as_str().starts_with("__rumoca_named_arg__.") => {
            let [value] = args.as_slice() else {
                return Err(ToDaeError::unsupported_flat(
                    "function shape proof",
                    "named argument wrapper must contain one value",
                    span,
                ));
            };
            expression_shape(value, values, function_result)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => function_result(name, args, *is_constructor, span),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let expected = expression_shape(else_branch, values, function_result)?;
            for (_, value) in branches {
                let found = expression_shape(value, values, function_result)?;
                require_same_shape(&expected, &found, span)?;
            }
            Ok(expected)
        }
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => {
            if *is_matrix {
                matrix_expression_shape(elements, values, function_result, span)
            } else {
                array_expression_shape(elements, values, function_result, span)
            }
        }
        Expression::Range {
            start, step, end, ..
        } => range_expression_shape(start, step.as_deref(), end, values, span),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => comprehension_expression_shape(
            expr,
            indices,
            filter.as_deref(),
            values,
            function_result,
            span,
        ),
        Expression::FieldAccess { .. } => {
            field_access_shape(expression, values, function_result, span)
        }
        Expression::Tuple { .. } | Expression::Empty { .. } => {
            Err(unshaped_expression_form(expression, span))
        }
    }
}

/// Exact shape of a structural record field access.
///
/// A symbolically indexed record-array projection consumes the occurrence plan
/// selected by scope, declaration chain, `DefId`, and `InstanceId`. Every other
/// structural access combines the recursively proven base shape with the exact
/// trailing dimensions retained for its field `DefId`. Neither route recovers
/// a field from spelling or from scalarized variable rows.
fn field_access_shape(
    expression: &Expression,
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let plans = values
        .record_array_fields()
        .ok_or_else(|| unshaped_expression_form(expression, span))?;
    if let Some(plan) = plans.get(expression) {
        return match plan {
            RecordArrayFieldPlan::MaterializedCoordinate { shape, .. } => {
                concrete_dimensions(shape, span, "record field coordinate")
            }
            RecordArrayFieldPlan::Projection {
                coordinates,
                shape,
                subscripts,
                ..
            } => {
                let extent = u32::try_from(coordinates.len()).map_err(|_| {
                    ToDaeError::unsupported_flat(
                        "function shape proof",
                        "record-array projection extent exceeds the DAE shape domain",
                        span,
                    )
                })?;
                let mut projected = Vec::with_capacity(shape.len() + 1);
                projected.push(extent);
                projected.extend(concrete_dimensions(
                    shape,
                    span,
                    "record field projection element",
                )?);
                apply_subscripts(projected, subscripts, values)
            }
        };
    }

    let Expression::FieldAccess {
        base, field_def_id, ..
    } = expression
    else {
        unreachable!("field access shape is called only for field access")
    };
    let mut shape = expression_shape(base, values, function_result)?;
    let field_shape =
        plans
            .field_shape(*field_def_id)
            .ok_or_else(|| ToDaeError::MissingSemanticIdentity {
                identity: format!(
                    "MLS §12.2 record-field projection requires retained shape metadata for record field declaration {}",
                    field_def_id.index()
                ),
            })?;
    shape.extend(concrete_dimensions(
        field_shape,
        span,
        "record field declaration",
    )?);
    Ok(shape)
}

/// Name the construct behind an expression form that owns no checked shape.
///
/// A rejection that does not say *which* construct it refuses cannot be read as
/// a contract (SPEC_0008 acceptance-contract-before-rejection), so each form
/// reports itself rather than sharing one message.
///
/// `Tuple` and `Empty` are defensive: flatten lowers an
/// MLS §11.2.1.1 receiving list into `Statement::FunctionCall::outputs` and an
/// absent expression into no expression at all, so neither reaches a shape
/// query in the MSL cohort. They are still named rather than shared, because a
/// future producer that does emit one must read what was refused.
fn unshaped_expression_form(expression: &Expression, span: Span) -> ToDaeError {
    let detail = match expression {
        Expression::Tuple { .. } => {
            "an MLS §11.2.1.1 result tuple has no shape of its own; only its individual results do"
        }
        Expression::FieldAccess { .. } => {
            "MLS §12.2 record-field projection has no typed occurrence plan"
        }
        Expression::Empty { .. } => "an absent expression has no shape",
        _ => unreachable!("only the unshaped expression forms reach this rule"),
    };
    ToDaeError::unsupported_flat("function shape proof", detail, span)
}

fn string_conversion_shape(
    value: &Expression,
    format: &rumoca_core::StringConversionFormat,
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    if !expression_shape(value, values, function_result)?.is_empty() {
        return shape_mismatch(span);
    }
    for operand in format.operands() {
        if !expression_shape(operand, values, function_result)?.is_empty() {
            return shape_mismatch(span);
        }
    }
    Ok(Vec::new())
}

fn array_expression_shape(
    elements: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let Some(first) = elements.first() else {
        return Ok(vec![0]);
    };
    let child = expression_shape(first, values, function_result)?;
    for element in &elements[1..] {
        let found = expression_shape(element, values, function_result)?;
        require_same_shape(&child, &found, span)?;
    }
    let count = u32::try_from(elements.len()).map_err(|_| {
        ToDaeError::unsupported_flat(
            "function shape proof",
            "array element count exceeds the DAE shape domain",
            span,
        )
    })?;
    Ok(std::iter::once(count).chain(child).collect())
}

/// The shape MLS §10.4.2.1 gives the `[ ]` concatenation operator.
///
/// MLS §10.4.2.1 defines both spellings over `promote`:
///
/// > Concatenation along first dimension: `[A; B; C; …] = cat(1, promote(A, n),
/// > promote(B, n), promote(C, n), …)` where `n = max(2, ndims(A), ndims(B),
/// > ndims(C), …)`.
/// >
/// > Concatenation along second dimension: `[A, B, C, …] = cat(2, promote(A,
/// > n), promote(B, n), promote(C, n), …)` where `n = max(2, …)`. If necessary,
/// > 1-sized dimensions are added to the right of A, B, C before the operation
/// > is carried out, especially that each operand has at least two dimensions.
///
/// A `[ ]` result therefore always has rank 2 here, never the rank the element
/// nesting alone suggests: `[0, 1, 1, 0, 0]` is a 1x5 matrix, not a 5-vector.
/// Proving it as a vector is what made `Modelica.Math.Matrices.isEqual(...,
/// [0, 1, 1, 0, 0], ...)` look like a rank-1 argument for a `Real[:,:]` formal.
///
/// Parse builds a row-per-element node for the `;` spelling and a
/// operand-per-element node for the `,` spelling
/// (`rumoca-phase-parse/src/expressions.rs::convert_range_primary`), so a
/// constructor whose every element is itself a `[ ]` node is the `;` form.
///
/// KNOWN FRONTEND LIMITATION (not introduced here): those two spellings alias
/// when a `,` operand is itself written with brackets. `[[1,2],[3,4]]` (OMC:
/// 1x4) and `[1,2;3,4]` (OMC: 2x2) reach this rule as the *same* node, and both
/// are read as the `;` form. The aliasing is in the parse IR — `is_matrix` is a
/// bool with no room for the separator — and predates this rule, which only
/// changes the previously wrong rank of the unambiguous single-row form.
fn matrix_expression_shape(
    elements: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    if elements.is_empty() {
        // MLS §10.4.2.1: "There must be at least one argument (i.e., [] is not
        // defined)."
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "MLS §10.4.2.1 leaves the empty matrix construction `[]` undefined",
            span,
        ));
    }
    let is_row_element = |element: &Expression| {
        matches!(
            element,
            Expression::Array {
                is_matrix: true,
                ..
            }
        )
    };
    if elements.iter().all(is_row_element) {
        // `[A; B; …]`: each source row first performs its own promoted
        // dimension-2 concatenation, then the rows concatenate along
        // dimension 1. This is the unambiguous parse shape for the `;`
        // spelling, so vectors and matrices retain the exact promotion MLS
        // gives them instead of being guessed from element nesting.
        let mut rows = Vec::with_capacity(elements.len());
        for row in elements {
            let Expression::Array {
                elements: operands, ..
            } = row
            else {
                unreachable!("every element was proven to be a matrix constructor")
            };
            rows.push(promoted_concatenation_shape(
                1,
                operands,
                values,
                function_result,
                span,
            )?);
        }
        return concatenate_proven_shapes(0, &rows, span);
    }
    // `[A, B, …]`: one row, concatenated along dimension 2. When the source
    // operands are not themselves array nodes, Parse gives this form a unique
    // representation, so the checked promoted-concatenation owner can derive
    // the exact shape for scalars, references, and function results alike.
    // Array-node operands retain the narrow refusal below because the current
    // Flat producer also uses that shape for expanded comprehensions.
    if elements
        .iter()
        .all(|element| !matches!(element, Expression::Array { .. }))
    {
        return promoted_concatenation_shape(1, elements, values, function_result, span);
    }
    let columns = matrix_row_columns(elements, values, function_result, span)?;
    Ok(vec![1, columns])
}

/// The column count one `[ ]` row proves for its operands.
///
/// Each operand is `promote`d to rank 2 first — MLS Operator 10.1 "Fills
/// dimensions of size 1 from the right" — so a scalar becomes 1x1 and the row
/// is 1 x (operand count).
///
/// ACCEPTANCE CONTRACT (SPEC_0008): this fallback is only for a horizontal row
/// containing an array *node*, the shape that still aliases the comprehension
/// frontend form described above. Syntactically non-array operands use the
/// checked promoted-concatenation owner directly in [`matrix_expression_shape`].
fn matrix_row_columns(
    operands: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<u32, ToDaeError> {
    if operands.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "MLS §10.4.2.1 leaves an empty matrix construction row undefined",
            span,
        ));
    }
    for operand in operands {
        let shape = expression_shape(operand, values, function_result)?;
        if !shape.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "function shape proof",
                format!(
                    "MLS §10.4.2.1 ambiguous horizontal `[ ]` row has a rank-{} operand; only \
                     the structurally unambiguous `;` form has a checked promoted-concatenation \
                     owner",
                    shape.len()
                ),
                span,
            ));
        }
    }
    u32::try_from(operands.len()).map_err(|_| {
        ToDaeError::unsupported_flat(
            "function shape proof",
            "matrix column count exceeds the DAE shape domain",
            span,
        )
    })
}

/// Exact promoted-concatenation shape for one unambiguous `;` matrix row.
fn promoted_concatenation_shape(
    axis: usize,
    operands: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let shapes = operands
        .iter()
        .map(|operand| expression_shape(operand, values, function_result))
        .collect::<Result<Vec<_>, _>>()?;
    let rank = shapes.iter().map(Vec::len).max().unwrap_or(0).max(2);
    let promoted = shapes
        .into_iter()
        .map(|mut shape| {
            shape.resize(rank, 1);
            shape
        })
        .collect::<Vec<_>>();
    concatenate_proven_shapes(axis, &promoted, span)
}

fn concatenate_proven_shapes(
    axis: usize,
    shapes: &[ValueShape],
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let Some((first, rest)) = shapes.split_first() else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "MLS §10.4.2.1 leaves an empty concatenation undefined",
            span,
        ));
    };
    let mut result = first.clone();
    for shape in rest {
        if shape.len() != result.len()
            || shape
                .iter()
                .zip(&result)
                .enumerate()
                .any(|(dimension, (found, expected))| dimension != axis && found != expected)
        {
            return shape_mismatch(span);
        }
        result[axis] = result[axis].checked_add(shape[axis]).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "concatenation extent exceeds the DAE shape domain",
                span,
            )
        })?;
    }
    Ok(result)
}

/// The shape MLS §10.4.1 gives an array constructor with iterators.
///
/// The result has one dimension per iterator — the number of values that
/// iterator's range denotes, which [`specialized_comprehension_plan`] is the
/// single owner of — followed by the dimensions of the element expression. The
/// element is shaped in the scope MLS §10.4.1 opens, where each index is a
/// fresh scalar that shadows any enclosing coordinate of the same flat name,
/// and its shape is the same on every iteration because that scope binds the
/// indices as scalars rather than as any particular value.
fn comprehension_expression_shape(
    body: &Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&Expression>,
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let plan = specialized_comprehension_plan(indices, filter, values, span)?;
    let extents = plan.domain.extents().map_err(|error| {
        ToDaeError::unsupported_flat(
            "function shape proof",
            format!("array comprehension has an invalid compact domain: {error}"),
            span,
        )
    })?;
    let mut shape = Vec::with_capacity(extents.len());
    for extent in extents {
        shape.push(u32::try_from(extent).map_err(|_| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "array comprehension extent exceeds the DAE shape domain",
                span,
            )
        })?);
    }
    let mut scoped = values.clone();
    for index in indices {
        scoped.insert(VarName::new(&index.name), Vec::new());
    }
    shape.extend(expression_shape(body, &scoped, function_result)?);
    Ok(shape)
}

fn range_expression_shape(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    values: &ShapeEnvironment,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let start = evaluate_shape_integer(start, values)?;
    let step = step
        .map(|step| evaluate_shape_integer(step, values))
        .transpose()?
        .unwrap_or(1);
    let end = evaluate_shape_integer(end, values)?;
    Ok(vec![range_cardinality(start, step, end, span)?])
}

pub(super) fn reject_shape_call(
    name: &rumoca_core::Reference,
    _arguments: &[Expression],
    _is_constructor: bool,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    Err(ToDaeError::unsupported_flat(
        "function shape proof",
        format!(
            "dependent extents cannot call runtime function `{}`",
            name.as_str()
        ),
        span,
    ))
}

fn builtin_shape(
    function: BuiltinFunction,
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    match function {
        BuiltinFunction::Size if arguments.len() == 1 => {
            let rank = expression_shape(&arguments[0], values, function_result)?.len();
            Ok(vec![u32::try_from(rank).map_err(|_| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    "rank exceeds the DAE shape domain",
                    span,
                )
            })?])
        }
        BuiltinFunction::Size | BuiltinFunction::Sum | BuiltinFunction::Product => Ok(Vec::new()),
        BuiltinFunction::Zeros => arguments
            .iter()
            .map(|argument| {
                let extent = evaluate_shape_integer(argument, values)?;
                u32::try_from(extent).ok().ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function shape proof",
                        format!("zeros extent `{extent}` is invalid"),
                        span,
                    )
                })
            })
            .collect(),
        BuiltinFunction::Vector => vector_shape(arguments, values, function_result, span),
        BuiltinFunction::Transpose => transpose_shape(arguments, values, function_result, span),
        BuiltinFunction::Diagonal => diagonal_shape(arguments, values, function_result, span),
        BuiltinFunction::OuterProduct => {
            outer_product_shape(arguments, values, function_result, span)
        }
        BuiltinFunction::Identity => identity_shape(arguments, values, function_result, span),
        BuiltinFunction::Cross => cross_shape(arguments, values, function_result, span),
        BuiltinFunction::Skew => skew_shape(arguments, values, function_result, span),
        BuiltinFunction::Smooth => arguments
            .get(1)
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    "smooth requires two arguments",
                    span,
                )
            })
            .and_then(|value| expression_shape(value, values, function_result)),
        BuiltinFunction::Integer => scalar_integer_shape(arguments, values, function_result, span),
        BuiltinFunction::Min | BuiltinFunction::Max if arguments.len() == 1 => Ok(Vec::new()),
        BuiltinFunction::Der
        | BuiltinFunction::Pre
        | BuiltinFunction::Sample
        | BuiltinFunction::Clock
        | BuiltinFunction::Hold
        | BuiltinFunction::Previous
        | BuiltinFunction::SubSample
        | BuiltinFunction::SuperSample
        | BuiltinFunction::ShiftSample
        | BuiltinFunction::BackSample
        | BuiltinFunction::NoClock
        | BuiltinFunction::Abs
        | BuiltinFunction::Sign
        | BuiltinFunction::Sqrt
        | BuiltinFunction::Floor
        | BuiltinFunction::Ceil
        | BuiltinFunction::Sin
        | BuiltinFunction::Cos
        | BuiltinFunction::Tan
        | BuiltinFunction::Asin
        | BuiltinFunction::Acos
        | BuiltinFunction::Atan
        | BuiltinFunction::Sinh
        | BuiltinFunction::Cosh
        | BuiltinFunction::Tanh
        | BuiltinFunction::Exp
        | BuiltinFunction::Log
        | BuiltinFunction::Log10
        | BuiltinFunction::NoEvent => arguments
            .first()
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    format!("{} requires an argument", function.name()),
                    span,
                )
            })
            .and_then(|value| expression_shape(value, values, function_result)),
        BuiltinFunction::Interval => scalar_interval_shape(arguments, span),
        // MLS §3.7.4.5 `semiLinear` returns `if x >= 0 then positiveSlope*x else
        // negativeSlope*x`, so its shape is the common shape of its operands.
        BuiltinFunction::Atan2
        | BuiltinFunction::Mod
        | BuiltinFunction::Min
        | BuiltinFunction::Max
        | BuiltinFunction::SemiLinear => {
            shared_operand_shape(function, arguments, values, function_result, span)
        }
        _ => Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!("{} has no exact checked shape rule", function.name()),
            span,
        )),
    }
}

/// MLS §10.3.2: one scalar or array with at most one non-unit dimension
/// reshapes to a vector whose extent is the checked product of all dimensions.
fn vector_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [argument] = arguments else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!("vector requires one argument, found {}", arguments.len()),
            span,
        ));
    };
    let dimensions = expression_shape(argument, values, function_result)?;
    if dimensions.iter().filter(|&&extent| extent > 1).count() > 1 {
        return shape_mismatch(span);
    }
    let extent = dimensions.iter().try_fold(1_u32, |product, extent| {
        product.checked_mul(*extent).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "vector extent exceeds the DAE shape domain",
                span,
            )
        })
    })?;
    Ok(vec![extent])
}

/// MLS §10.3.5 / ARR-038: transpose requires rank two or greater and exchanges
/// only the first two extents of the compact operand shape.
fn transpose_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [argument] = arguments else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!("transpose requires one argument, found {}", arguments.len()),
            span,
        ));
    };
    let mut dimensions = expression_shape(argument, values, function_result)?;
    if dimensions.len() < 2 {
        return shape_mismatch(span);
    }
    dimensions.swap(0, 1);
    Ok(dimensions)
}

/// MLS §10.3.5 / ARR-041: a vector's one compact extent owns both axes of
/// the square diagonal matrix. The constructor retains the domain; it never
/// materializes the off-diagonal zeros as scalar expressions.
fn diagonal_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [argument] = arguments else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!(
                "diagonal requires one vector, found {} arguments",
                arguments.len()
            ),
            span,
        ));
    };
    let dimensions = expression_shape(argument, values, function_result)?;
    let [extent] = dimensions.as_slice() else {
        return shape_mismatch(span);
    };
    Ok(vec![*extent, *extent])
}

/// MLS §10.3.5 / ARR-042: the two vector domains become the two matrix axes
/// in source order. Unequal vector lengths are valid and remain compact.
fn outer_product_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [lhs, rhs] = arguments else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!(
                "outerProduct requires two vectors, found {} arguments",
                arguments.len()
            ),
            span,
        ));
    };
    let lhs_dimensions = expression_shape(lhs, values, function_result)?;
    let [lhs_extent] = lhs_dimensions.as_slice() else {
        return shape_mismatch(span);
    };
    let rhs_dimensions = expression_shape(rhs, values, function_result)?;
    let [rhs_extent] = rhs_dimensions.as_slice() else {
        return shape_mismatch(span);
    };
    Ok(vec![*lhs_extent, *rhs_extent])
}

/// MLS §10.3.3: one proven scalar Integer `n` owns an exact `n x n` result.
fn identity_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [extent] = arguments else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "identity requires one scalar Integer extent",
            span,
        ));
    };
    if !expression_shape(extent, values, function_result)?.is_empty() {
        return shape_mismatch(span);
    }
    let extent = evaluate_shape_integer(extent, values)?;
    let extent = u32::try_from(extent).map_err(|_| {
        ToDaeError::unsupported_flat(
            "function shape proof",
            format!("identity extent `{extent}` is invalid"),
            span,
        )
    })?;
    Ok(vec![extent, extent])
}

/// MLS §10.3.5: `cross` is closed over two common numeric 3-vectors.
///
/// Scalar type compatibility is owned by typecheck and rechecked by the DAE
/// constructor. This scope proves only the exact rank/extents used to mint a
/// function specialization.
fn cross_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [lhs, rhs] = arguments else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "cross requires two 3-vector arguments",
            span,
        ));
    };
    let lhs = expression_shape(lhs, values, function_result)?;
    let rhs = expression_shape(rhs, values, function_result)?;
    if lhs == [3] && rhs == lhs {
        Ok(lhs)
    } else {
        shape_mismatch(span)
    }
}

/// MLS §10.3.5: `skew` maps exactly one Real 3-vector to a 3x3 matrix.
///
/// Scalar type is owned by typecheck and rechecked by the DAE constructor.
/// This scope proves the exact rank/extents used for function specialization.
fn skew_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [argument] = arguments else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "skew requires one 3-vector argument",
            span,
        ));
    };
    if expression_shape(argument, values, function_result)? == [3] {
        Ok(vec![3, 3])
    } else {
        shape_mismatch(span)
    }
}

/// The one shape every operand of an elementwise builtin must agree on.
fn shared_operand_shape(
    function: BuiltinFunction,
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let Some((first, rest)) = arguments.split_first() else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!("{} requires arguments", function.name()),
            span,
        ));
    };
    let expected = expression_shape(first, values, function_result)?;
    for argument in rest {
        let found = expression_shape(argument, values, function_result)?;
        require_same_shape(&expected, &found, span)?;
    }
    Ok(expected)
}

fn scalar_interval_shape(arguments: &[Expression], span: Span) -> Result<ValueShape, ToDaeError> {
    if arguments.len() > 1 {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "interval accepts at most one inference operand",
            span,
        ));
    }
    Ok(Vec::new())
}

fn scalar_integer_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [value] = arguments else {
        return Err(invalid_integer_shape(span));
    };
    let shape = expression_shape(value, values, function_result)?;
    if shape.is_empty() {
        Ok(shape)
    } else {
        Err(invalid_integer_shape(span))
    }
}

fn invalid_integer_shape(span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function shape proof",
        "integer requires one scalar argument",
        span,
    )
}

fn binary_shape(
    operator: OpBinary,
    lhs: ValueShape,
    rhs: ValueShape,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    match operator {
        OpBinary::Mul => match (lhs.as_slice(), rhs.as_slice()) {
            ([], _) => Ok(rhs),
            (_, []) => Ok(lhs),
            ([lhs_n], [rhs_n]) if lhs_n == rhs_n => Ok(Vec::new()),
            ([rows, inner], [rhs_inner]) if inner == rhs_inner => Ok(vec![*rows]),
            ([lhs_inner], [rhs_inner, columns]) if lhs_inner == rhs_inner => Ok(vec![*columns]),
            ([rows, inner], [rhs_inner, columns]) if inner == rhs_inner => {
                Ok(vec![*rows, *columns])
            }
            _ => shape_mismatch(span),
        },
        OpBinary::Div => {
            if rhs.is_empty() {
                Ok(lhs)
            } else {
                shape_mismatch(span)
            }
        }
        OpBinary::MulElem | OpBinary::DivElem | OpBinary::ExpElem => {
            if lhs.is_empty() {
                Ok(rhs)
            } else if rhs.is_empty() || lhs == rhs {
                Ok(lhs)
            } else {
                shape_mismatch(span)
            }
        }
        OpBinary::Exp => {
            if rhs.is_empty() {
                Ok(lhs)
            } else {
                shape_mismatch(span)
            }
        }
        _ => {
            require_same_shape(&lhs, &rhs, span)?;
            Ok(lhs)
        }
    }
}

fn apply_subscripts(
    shape: ValueShape,
    subscripts: &[Subscript],
    values: &ShapeEnvironment,
) -> Result<ValueShape, ToDaeError> {
    let mut remaining = shape.into_iter();
    let mut result = Vec::new();
    for subscript in subscripts {
        let source_extent = remaining.next().ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "subscript count exceeds expression rank",
                subscript.span(),
            )
        })?;
        match subscript {
            Subscript::Index { value, span } => {
                if *value < 1
                    || u32::try_from(*value)
                        .ok()
                        .is_none_or(|value| value > source_extent)
                {
                    return Err(ToDaeError::unsupported_flat(
                        "function shape proof",
                        format!("literal index `{value}` is outside extent {source_extent}"),
                        *span,
                    ));
                }
            }
            Subscript::Colon { .. } => result.push(source_extent),
            Subscript::Expr { expr, .. } => {
                let index_shape = expression_shape(expr, values, &mut reject_shape_call)?;
                result.extend(index_shape);
            }
        }
    }
    result.extend(remaining);
    Ok(result)
}

fn require_same_shape(expected: &[u32], found: &[u32], span: Span) -> Result<(), ToDaeError> {
    if expected == found {
        Ok(())
    } else {
        shape_mismatch(span)
    }
}

fn shape_mismatch<T>(span: Span) -> Result<T, ToDaeError> {
    Err(ToDaeError::unsupported_flat(
        "function shape proof",
        "expression shapes are inconsistent",
        span,
    ))
}

fn range_cardinality(start: i64, step: i64, end: i64, span: Span) -> Result<u32, ToDaeError> {
    if step == 0 {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "range step cannot be zero",
            span,
        ));
    }
    let distance = if step > 0 {
        end.checked_sub(start)
    } else {
        start.checked_sub(end)
    };
    let Some(distance) = distance else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "range cardinality overflowed",
            span,
        ));
    };
    if distance < 0 {
        return Ok(0);
    }
    let count = distance
        .checked_div(step.abs())
        .and_then(|count| count.checked_add(1))
        .and_then(|count| u32::try_from(count).ok())
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "range cardinality exceeds the DAE shape domain",
                span,
            )
        })?;
    Ok(count)
}
