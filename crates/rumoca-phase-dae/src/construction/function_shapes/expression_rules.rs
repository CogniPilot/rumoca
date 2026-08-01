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

pub(super) type FunctionResultShape<'scope> = dyn FnMut(&rumoca_core::Reference, &[Expression], Span) -> Result<ValueShape, ToDaeError>
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
        Expression::FunctionCall { name, args, .. } => function_result(name, args, span),
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
        Expression::Tuple { .. } | Expression::FieldAccess { .. } | Expression::Empty { .. } => {
            Err(unshaped_expression_form(expression, span))
        }
    }
}

/// Name the construct behind an expression form that owns no checked shape.
///
/// A rejection that does not say *which* construct it refuses cannot be read as
/// a contract (SPEC_0008 acceptance-contract-before-rejection), so each form
/// reports itself rather than sharing one message.
///
/// Reachability differs per arm. `FieldAccess` is reached by real models —
/// `Modelica.Electrical.Batteries.Examples.ShowImpedance` and
/// `Modelica.Magnetic.QuasiStatic.FluxTubes.Examples.BasicExamples.
/// SinglePhaseInductance` both land here through a symbolically indexed record
/// array element. `Tuple` and `Empty` are defensive: flatten lowers an
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
            "MLS §12.2 record-field projection has an exact shape only through a flattened field \
             name; a structural field access of a symbolically indexed record array element keeps \
             no such name"
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
        // `[A; B; …]`: concatenate the 1 x n rows along dimension 1.
        let mut columns = None;
        for row in elements {
            let Expression::Array {
                elements: operands, ..
            } = row
            else {
                unreachable!("every element was proven to be a matrix constructor")
            };
            let column_count = matrix_row_columns(operands, values, function_result, span)?;
            match columns {
                Some(expected) if expected != column_count => return shape_mismatch(span),
                None => columns = Some(column_count),
                _ => {}
            }
        }
        let rows = u32::try_from(elements.len()).map_err(|_| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "matrix row count exceeds the DAE shape domain",
                span,
            )
        })?;
        let columns = columns.expect("a matrix construction owns at least one row");
        return Ok(vec![rows, columns]);
    }
    // `[A, B, …]`: one row, concatenated along dimension 2.
    let columns = matrix_row_columns(elements, values, function_result, span)?;
    Ok(vec![1, columns])
}

/// The column count one `[ ]` row proves for its operands.
///
/// Each operand is `promote`d to rank 2 first — MLS Operator 10.1 "Fills
/// dimensions of size 1 from the right" — so a scalar becomes 1x1 and the row
/// is 1 x (operand count).
///
/// ACCEPTANCE CONTRACT (SPEC_0008): this rule proves a `[ ]` row of *scalar*
/// operands and nothing else. A non-scalar operand needs the real `cat`
/// promotion — a vector operand becomes an n x 1 column, so MLS §10.4.2.1's own
/// example states "`[v1; v2]` is a `(n1+n2) x 1` matrix", and a row of vectors
/// transposes into the result — and the canonical DAE has no concatenation
/// owner to build that with. Proving a shape the lowering cannot construct
/// would only move the rejection to `ED020`, so the shape rule and
/// [`lower_matrix_expression`] refuse exactly the same operands.
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
                    "MLS §10.4.2.1 `[ ]` concatenation of a rank-{} operand needs a `cat` \
                     promotion owner the canonical DAE does not have; only scalar operands \
                     have an exact checked shape rule",
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
