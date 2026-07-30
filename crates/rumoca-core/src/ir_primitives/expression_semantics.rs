use super::*;

/// Structural expression equality for the shared Flat/DAE expression grammar.
///
/// This is a syntactic IR query, not expression evaluation: it never folds,
/// resolves, or executes expressions. It exists in `rumoca-core` because
/// multiple phases need one span-insensitive definition of shared-expression
/// identity.
pub fn expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    match (lhs, rhs) {
        (Expression::Binary { .. }, Expression::Binary { .. }) => {
            binary_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::Unary { .. }, Expression::Unary { .. }) => {
            unary_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::VarRef { .. }, Expression::VarRef { .. }) => {
            var_refs_semantically_equal(lhs, rhs)
        }
        (Expression::BuiltinCall { .. }, Expression::BuiltinCall { .. }) => {
            builtin_calls_semantically_equal(lhs, rhs)
        }
        (Expression::FunctionCall { .. }, Expression::FunctionCall { .. }) => {
            function_calls_semantically_equal(lhs, rhs)
        }
        (Expression::StringConversion { .. }, Expression::StringConversion { .. }) => {
            string_conversions_semantically_equal(lhs, rhs)
        }
        (Expression::Literal { value: lhs, .. }, Expression::Literal { value: rhs, .. }) => {
            lhs == rhs
        }
        (Expression::If { .. }, Expression::If { .. }) => {
            if_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::Array { .. }, Expression::Array { .. }) => arrays_semantically_equal(lhs, rhs),
        (Expression::Tuple { .. }, Expression::Tuple { .. }) => tuples_semantically_equal(lhs, rhs),
        (Expression::Range { .. }, Expression::Range { .. }) => ranges_semantically_equal(lhs, rhs),
        (Expression::ArrayComprehension { .. }, Expression::ArrayComprehension { .. }) => {
            array_comprehensions_semantically_equal(lhs, rhs)
        }
        (Expression::Index { .. }, Expression::Index { .. }) => {
            index_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::FieldAccess { .. }, Expression::FieldAccess { .. }) => {
            field_accesses_semantically_equal(lhs, rhs)
        }
        (Expression::Empty { .. }, Expression::Empty { .. }) => true,
        _ => false,
    }
}

/// Span-insensitive fingerprint paired with [`expressions_semantically_equal`].
///
/// The value is an in-process lookup accelerator, not a persistent content ID;
/// callers must confirm equality within a fingerprint bucket.
///
/// No string bytes are hashed here. Where the expression already carries an
/// interned name — the `VarRef` and `FunctionCall` arms — the fingerprint uses
/// that [`VarNameId`], never the rendered spelling (SPEC_0032 §3): the interner
/// is a bijection between text and id, so bucketing is unchanged while the
/// spelling no longer has to be walked byte by byte. Where the expression holds
/// bare `&str` text instead, see [`hash_unowned_text_len`] for why the
/// fingerprint summarises it rather than interning it.
///
/// The contract callers depend on is one-directional: whenever
/// [`expressions_semantically_equal`] holds, this value must agree. Each arm
/// below therefore hashes a *subset* of what its equality counterpart compares.
pub fn expression_semantic_fingerprint(expr: &Expression) -> u64 {
    let mut hasher = DefaultHasher::new();
    hash_expression_semantics(expr, &mut hasher);
    hasher.finish()
}

fn hash_discriminant<T>(value: &T, hasher: &mut impl Hasher) {
    std::mem::discriminant(value).hash(hasher);
}

/// Hash text the caller holds only as a `&str`.
///
/// Only the length is hashed. That is a strict subset of what the matching
/// equality arm compares, so equal expressions still fingerprint equal; unequal
/// text of equal length shares a bucket and is separated by
/// [`expressions_semantically_equal`], which every caller must run anyway.
///
/// Interning here instead would be a pessimisation and a leak. `VarName::intern`
/// hashes the whole string to find its id, so a caller holding only a `&str`
/// pays the string hash *plus* a global `RwLock`, a map probe and an allocation
/// on first sight — strictly more than hashing the `&str` would have cost.
/// Interning pays off only where the caller already holds a `VarName`, as the
/// `VarRef` and `FunctionCall` arms do. Worse, the interner has no eviction and
/// its ids are assumed stable, so feeding it comprehension indices, field names
/// and string literals out of a long-lived process (the LSP server sees an
/// arbitrary edit stream for hours) would grow it without bound.
fn hash_unowned_text_len(text: &str, hasher: &mut impl Hasher) {
    text.len().hash(hasher);
}

fn hash_expression_semantics(expr: &Expression, hasher: &mut impl Hasher) {
    hash_discriminant(expr, hasher);
    match expr {
        Expression::Binary { op, lhs, rhs, .. } => {
            hash_discriminant(op, hasher);
            hash_expression_semantics(lhs, hasher);
            hash_expression_semantics(rhs, hasher);
        }
        Expression::Unary { op, rhs, .. } => {
            hash_discriminant(op, hasher);
            hash_expression_semantics(rhs, hasher);
        }
        Expression::VarRef {
            name, subscripts, ..
        } => {
            // Exactly the identity `var_refs_semantically_equal` compares:
            // `VarName`, whose `Hash` is its `VarNameId`. Deliberately *not*
            // `component_ref.def_id` — a `DefId` names the declaration, and one
            // declaration backs many flat variables (`r1.v` and `r2.v` share
            // the `Resistor.v` `DefId`; `DefIdVarRefIndex` in phase-flatten
            // stores a `Vec` per `DefId` and disambiguates by scope for exactly
            // that reason), so a def-id fingerprint would equate distinct
            // variables and disagree with equality.
            name.var_name().hash(hasher);
            hash_subscripts_semantics(subscripts, hasher);
        }
        Expression::BuiltinCall { function, args, .. } => {
            hash_discriminant(function, hasher);
            hash_expression_slice_semantics(args, hasher);
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } => {
            // `function_calls_semantically_equal` compares whole `Reference`s,
            // so the resolved callee is part of equality and may be hashed.
            // Adding it splits a bucket that the old rendered-name hash merged:
            // two `FunctionInstanceId`s can render identically (an inherited or
            // redeclared instance of one declaration) yet are not equal, and
            // used to collide.
            name.var_name().hash(hasher);
            name.resolved_function().hash(hasher);
            is_constructor.hash(hasher);
            hash_expression_slice_semantics(args, hasher);
        }
        Expression::StringConversion {
            declaration,
            value,
            format,
            ..
        } => hash_string_conversion_semantics(*declaration, value, format, hasher),
        Expression::Literal { value, .. } => hash_literal_semantics(value, hasher),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.len().hash(hasher);
            for (condition, value) in branches {
                hash_expression_semantics(condition, hasher);
                hash_expression_semantics(value, hasher);
            }
            hash_expression_semantics(else_branch, hasher);
        }
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => {
            is_matrix.hash(hasher);
            hash_expression_slice_semantics(elements, hasher);
        }
        Expression::Tuple { elements, .. } => hash_expression_slice_semantics(elements, hasher),
        Expression::Range {
            start, step, end, ..
        } => {
            hash_expression_semantics(start, hasher);
            step.is_some().hash(hasher);
            if let Some(step) = step {
                hash_expression_semantics(step, hasher);
            }
            hash_expression_semantics(end, hasher);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            hash_expression_semantics(expr, hasher);
            indices.len().hash(hasher);
            for index in indices {
                hash_unowned_text_len(&index.name, hasher);
                hash_expression_semantics(&index.range, hasher);
            }
            filter.is_some().hash(hasher);
            if let Some(filter) = filter {
                hash_expression_semantics(filter, hasher);
            }
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            hash_expression_semantics(base, hasher);
            hash_subscripts_semantics(subscripts, hasher);
        }
        Expression::FieldAccess { base, field, .. } => {
            hash_expression_semantics(base, hasher);
            hash_unowned_text_len(field, hasher);
        }
        Expression::Empty { .. } => {}
    }
}

fn hash_expression_slice_semantics(expressions: &[Expression], hasher: &mut impl Hasher) {
    expressions.len().hash(hasher);
    for expression in expressions {
        hash_expression_semantics(expression, hasher);
    }
}

fn hash_subscripts_semantics(subscripts: &[Subscript], hasher: &mut impl Hasher) {
    subscripts.len().hash(hasher);
    for subscript in subscripts {
        hash_discriminant(subscript, hasher);
        match subscript {
            Subscript::Index { value, .. } => value.hash(hasher),
            Subscript::Colon { .. } => {}
            Subscript::Expr { expr, .. } => hash_expression_semantics(expr, hasher),
        }
    }
}

fn hash_literal_semantics(value: &Literal, hasher: &mut impl Hasher) {
    hash_discriminant(value, hasher);
    match value {
        Literal::Real(value) => value.to_bits().hash(hasher),
        Literal::Integer(value) => value.hash(hasher),
        Literal::Boolean(value) => value.hash(hasher),
        // User *data*, not a name: an arbitrary literal from arbitrary source
        // text. It must never reach the interner — that map has no eviction and
        // hands out ids assumed stable for the process, so interning literals
        // would let an editing session in the LSP server grow it without bound.
        Literal::String(value) => hash_unowned_text_len(value, hasher),
    }
}

fn binary_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Binary {
            op: lhs_op,
            lhs: lhs_lhs,
            rhs: lhs_rhs,
            ..
        },
        Expression::Binary {
            op: rhs_op,
            lhs: rhs_lhs,
            rhs: rhs_rhs,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_op == rhs_op
        && expressions_semantically_equal(lhs_lhs, rhs_lhs)
        && expressions_semantically_equal(lhs_rhs, rhs_rhs)
}

fn unary_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Unary {
            op: lhs_op,
            rhs: lhs_rhs,
            ..
        },
        Expression::Unary {
            op: rhs_op,
            rhs: rhs_rhs,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_op == rhs_op && expressions_semantically_equal(lhs_rhs, rhs_rhs)
}

fn var_refs_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::VarRef {
            name: lhs_name,
            subscripts: lhs_subscripts,
            ..
        },
        Expression::VarRef {
            name: rhs_name,
            subscripts: rhs_subscripts,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    // Flat names are globally unique — `flat::Model::variables` is keyed by
    // VarName and flatten's name simplification fails loudly on rename
    // collisions — so two references denote the same variable iff their
    // rendered names match; attached resolution metadata (spans, def-ids,
    // component structure) does not change the meaning.
    lhs_name.var_name() == rhs_name.var_name()
        && subscripts_semantically_equal(lhs_subscripts, rhs_subscripts)
}

fn builtin_calls_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::BuiltinCall {
            function: lhs_function,
            args: lhs_args,
            ..
        },
        Expression::BuiltinCall {
            function: rhs_function,
            args: rhs_args,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_function == rhs_function && expression_slices_semantically_equal(lhs_args, rhs_args)
}

fn function_calls_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::FunctionCall {
            name: lhs_name,
            args: lhs_args,
            is_constructor: lhs_constructor,
            ..
        },
        Expression::FunctionCall {
            name: rhs_name,
            args: rhs_args,
            is_constructor: rhs_constructor,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_name == rhs_name
        && lhs_constructor == rhs_constructor
        && expression_slices_semantically_equal(lhs_args, rhs_args)
}

fn string_conversions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::StringConversion {
            declaration: lhs_declaration,
            value: lhs_value,
            format: lhs_format,
            ..
        },
        Expression::StringConversion {
            declaration: rhs_declaration,
            value: rhs_value,
            format: rhs_format,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_declaration == rhs_declaration
        && expressions_semantically_equal(lhs_value, rhs_value)
        && string_conversion_formats_semantically_equal(lhs_format, rhs_format)
}

fn string_conversion_formats_semantically_equal(
    lhs: &StringConversionFormat,
    rhs: &StringConversionFormat,
) -> bool {
    match (lhs, rhs) {
        (
            StringConversionFormat::Options {
                minimum_length: lhs_minimum,
                left_justified: lhs_left,
                significant_digits: lhs_digits,
            },
            StringConversionFormat::Options {
                minimum_length: rhs_minimum,
                left_justified: rhs_left,
                significant_digits: rhs_digits,
            },
        ) => {
            optional_expressions_semantically_equal(lhs_minimum.as_deref(), rhs_minimum.as_deref())
                && optional_expressions_semantically_equal(lhs_left.as_deref(), rhs_left.as_deref())
                && optional_expressions_semantically_equal(
                    lhs_digits.as_deref(),
                    rhs_digits.as_deref(),
                )
        }
        (
            StringConversionFormat::Format { value: lhs },
            StringConversionFormat::Format { value: rhs },
        ) => expressions_semantically_equal(lhs, rhs),
        _ => false,
    }
}

fn hash_string_conversion_format(format: &StringConversionFormat, hasher: &mut impl Hasher) {
    hash_discriminant(format, hasher);
    match format {
        StringConversionFormat::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => {
            for operand in [minimum_length, left_justified, significant_digits] {
                operand.is_some().hash(hasher);
                if let Some(operand) = operand {
                    hash_expression_semantics(operand, hasher);
                }
            }
        }
        StringConversionFormat::Format { value } => hash_expression_semantics(value, hasher),
    }
}

fn hash_string_conversion_semantics(
    declaration: DefId,
    value: &Expression,
    format: &StringConversionFormat,
    hasher: &mut impl Hasher,
) {
    declaration.hash(hasher);
    hash_expression_semantics(value, hasher);
    hash_string_conversion_format(format, hasher);
}

fn if_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::If {
            branches: lhs_branches,
            else_branch: lhs_else,
            ..
        },
        Expression::If {
            branches: rhs_branches,
            else_branch: rhs_else,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_branches.len() == rhs_branches.len()
        && lhs_branches
            .iter()
            .zip(rhs_branches)
            .all(expression_branch_pairs_semantically_equal)
        && expressions_semantically_equal(lhs_else, rhs_else)
}

fn expression_branch_pairs_semantically_equal(
    ((lhs_cond, lhs_value), (rhs_cond, rhs_value)): (
        &(Expression, Expression),
        &(Expression, Expression),
    ),
) -> bool {
    expressions_semantically_equal(lhs_cond, rhs_cond)
        && expressions_semantically_equal(lhs_value, rhs_value)
}

fn arrays_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Array {
            elements: lhs_elements,
            is_matrix: lhs_matrix,
            ..
        },
        Expression::Array {
            elements: rhs_elements,
            is_matrix: rhs_matrix,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_matrix == rhs_matrix && expression_slices_semantically_equal(lhs_elements, rhs_elements)
}

fn tuples_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Tuple {
            elements: lhs_elements,
            ..
        },
        Expression::Tuple {
            elements: rhs_elements,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expression_slices_semantically_equal(lhs_elements, rhs_elements)
}

fn ranges_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Range {
            start: lhs_start,
            step: lhs_step,
            end: lhs_end,
            ..
        },
        Expression::Range {
            start: rhs_start,
            step: rhs_step,
            end: rhs_end,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expressions_semantically_equal(lhs_start, rhs_start)
        && optional_expressions_semantically_equal(lhs_step.as_deref(), rhs_step.as_deref())
        && expressions_semantically_equal(lhs_end, rhs_end)
}

fn array_comprehensions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::ArrayComprehension {
            expr: lhs_expr,
            indices: lhs_indices,
            filter: lhs_filter,
            ..
        },
        Expression::ArrayComprehension {
            expr: rhs_expr,
            indices: rhs_indices,
            filter: rhs_filter,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expressions_semantically_equal(lhs_expr, rhs_expr)
        && lhs_indices.len() == rhs_indices.len()
        && lhs_indices.iter().zip(rhs_indices).all(|(lhs, rhs)| {
            lhs.name == rhs.name && expressions_semantically_equal(&lhs.range, &rhs.range)
        })
        && optional_expressions_semantically_equal(lhs_filter.as_deref(), rhs_filter.as_deref())
}

fn index_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Index {
            base: lhs_base,
            subscripts: lhs_subscripts,
            ..
        },
        Expression::Index {
            base: rhs_base,
            subscripts: rhs_subscripts,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expressions_semantically_equal(lhs_base, rhs_base)
        && subscripts_semantically_equal(lhs_subscripts, rhs_subscripts)
}

fn field_accesses_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::FieldAccess {
            base: lhs_base,
            field: lhs_field,
            ..
        },
        Expression::FieldAccess {
            base: rhs_base,
            field: rhs_field,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_field == rhs_field && expressions_semantically_equal(lhs_base, rhs_base)
}

fn expression_slices_semantically_equal(lhs: &[Expression], rhs: &[Expression]) -> bool {
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .zip(rhs)
            .all(|(lhs, rhs)| expressions_semantically_equal(lhs, rhs))
}

fn optional_expressions_semantically_equal(
    lhs: Option<&Expression>,
    rhs: Option<&Expression>,
) -> bool {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => expressions_semantically_equal(lhs, rhs),
        (None, None) => true,
        _ => false,
    }
}

fn subscripts_semantically_equal(lhs: &[Subscript], rhs: &[Subscript]) -> bool {
    lhs.len() == rhs.len()
        && lhs.iter().zip(rhs).all(|(lhs, rhs)| match (lhs, rhs) {
            (Subscript::Index { value: lhs, .. }, Subscript::Index { value: rhs, .. }) => {
                lhs == rhs
            }
            (Subscript::Colon { .. }, Subscript::Colon { .. }) => true,
            (Subscript::Expr { expr: lhs, .. }, Subscript::Expr { expr: rhs, .. }) => {
                expressions_semantically_equal(lhs, rhs)
            }
            _ => false,
        })
}
