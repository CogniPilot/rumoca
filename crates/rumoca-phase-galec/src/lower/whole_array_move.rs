//! The whole-array-move decision: when an elementwise tensor projection may
//! be flattened into a single whole-array assignment.
//!
//! Split out of `user_functions.rs` (SPEC_0021 size budget): everything here
//! serves one question, asked at exactly one call site
//! (`lower_tensor_function_assignment`) — is this projection provably the
//! identity move of an identically-shaped object? Two conjuncts answer it,
//! and both are required: subscript identity
//! ([`whole_array_projection_source`]) and declared-shape equality
//! ([`declared_shape`]). [`provable_whole_array_move`] composes them and is
//! the only entry point.

use super::*;
use user_functions::{
    parameter_name, record_parameter_field_name, record_value_field_name, value_name,
};

/// Recover the whole-array source of an elementwise projection that turned out
/// to be the identity.
///
/// A tensor assignment lowers its right-hand side once, at the coordinate
/// `indices` names, and only then decides how to iterate. When that lowering
/// hands back exactly `source[i][j]` for the very index expressions it was
/// given, the assignment does not compute anything per element: it moves a
/// whole array. This is asked before any loop is constructed, so the loop is
/// never built rather than built and recognised again -- the projection's own
/// result is the evidence, not the shape of emitted statements.
///
/// The subscripts must be the projection indices themselves, in order. An
/// offset walk (`source[7 + i - 1]`), a strided walk or a transposed one all
/// fail that equality and keep their loop, which is where they belong.
///
/// Subscript identity is necessary but NOT sufficient: the caller must also
/// prove, via [`declared_shape`], that the source's declared extents equal
/// the target's and the element types match. Per-element identity at the
/// index names says nothing about the objects' shapes — a projection can hand
/// back `source[i][j]` for a `source` declared with different extents than
/// the target, and a whole-array assignment between differently-shaped
/// objects is a statement the checked Algorithm Code must never contain. The
/// unit regression
/// `whole_array_move_needs_shape_equality_not_just_subscript_identity`
/// constructs exactly that pairing and pins the rejection.
///
/// Earlier parts of a state path must be unsubscripted: dropping the last
/// part's subscripts has to leave a reference that still denotes the whole
/// array, and a subscripted prefix could itself depend on a projection index.
fn whole_array_projection_source(
    value: &gast::Expression,
    indices: &[gast::Expression],
) -> Option<gast::Reference> {
    if indices.is_empty() {
        return None;
    }
    let gast::Expression::Ref(reference) = value else {
        return None;
    };
    match reference {
        gast::Reference::Local(part) if part.subscripts == indices => {
            Some(gast::Reference::Local(gast::RefPart {
                name: part.name.clone(),
                subscripts: Vec::new(),
                span: part.span,
            }))
        }
        gast::Reference::State(parts) => {
            let (last, prefix) = parts.split_last()?;
            if last.subscripts != indices || prefix.iter().any(|part| !part.subscripts.is_empty()) {
                return None;
            }
            let mut parts = prefix.to_vec();
            parts.push(gast::RefPart {
                name: last.name.clone(),
                subscripts: Vec::new(),
                span: last.span,
            });
            Some(gast::Reference::State(parts))
        }
        gast::Reference::Local(_) => None,
    }
}

/// The whole decision, both conjuncts, one place: an elementwise projection
/// may be flattened into a whole-array move exactly when the projection is
/// the subscript identity ([`whole_array_projection_source`]) AND the
/// source's declared shape equals the target's, element type included
/// ([`declared_shape`]). Returns the subscript-stripped source on success.
///
/// The shape conjunct deserves its own defense, because on today's corpus it
/// is a barrier, not a repair: every front-end path that currently produces
/// an identity projection reads an object declared from the target's own
/// type, and the shapes that WOULD go wrong (`y := s[1:2]` from a longer
/// `s`, a comprehension over a leading sub-range) reach this point with
/// their range arithmetic unfolded (`s[1 + (i - 1)]`), so the subscript
/// check already rejects them. One projection-folding improvement — folding
/// `1 + (i - 1)` to `i`, which is a natural cleanup — turns each of those
/// into a subscript-identity projection whose flattening writes a
/// whole-array assignment between differently-shaped objects. The unit test
/// `whole_array_move_needs_shape_equality_not_just_subscript_identity`
/// constructs exactly that situation and fails if this function ever
/// collapses it.
pub(super) fn provable_whole_array_move<'a, 'dae>(
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    value: &gast::Expression,
    indices: &[gast::Expression],
    target_extents: &[u32],
    target_scalar: gast::ScalarType,
) -> Option<gast::Reference> {
    let source = whole_array_projection_source(value, indices)?;
    let (extents, scalar) = declared_shape(lowerer, &source)?;
    (extents.as_slice() == target_extents && scalar == target_scalar).then_some(source)
}

/// Declared extents and element type of an identity-projection source, when a
/// declaration for it is visible to this lowerer.
///
/// This is the shape half of the whole-array-move evidence (the subscript
/// half is [`whole_array_projection_source`]): only when the source's
/// declared extents equal the target's, element type included, does
/// per-element identity at the index names imply whole-array identity.
///
/// Resolution is by declaration site:
/// - `Local` names: the lowerer's materialized temporaries (newest first, so
///   a name resolves to the declaration actually in scope), then the function
///   scope's parameters, outputs and locals — the same flattened declarations
///   `function_parameters` / `function_locals` emit, under the same names.
/// - `State` paths: the classified block variables, single-part paths only.
///   A multi-part path's leaf shape is not resolved here.
///
/// `None` means "no visible declaration proves the shape", and the caller
/// keeps the loop — the fallback is the semantics-preserving direction, so an
/// unresolvable name can only cost a collapse, never correctness.
fn declared_shape<'a, 'dae>(
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    reference: &gast::Reference,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    match reference {
        gast::Reference::Local(part) => {
            if let Some(declaration) = lowerer
                .temporary_locals
                .iter()
                .rev()
                .find(|declaration| declaration.name.lexeme() == part.name.lexeme())
            {
                return declared_variable_shape(declaration);
            }
            let function = lowerer.view.function(lowerer.function_scope?)?;
            function_scope_shape(lowerer.view, function, part.name.lexeme())
        }
        gast::Reference::State(parts) => {
            let [part] = parts.as_slice() else {
                return None;
            };
            lowerer.state_shape(part.name.lexeme())
        }
    }
}

/// The shape a [`gast::VariableDeclaration`] declares, when it is a primitive
/// with fully-literal extents. Every declaration this lowering emits is —
/// a non-literal or derived dimension yields `None`, and the caller keeps
/// its loop.
fn declared_variable_shape(
    declaration: &gast::VariableDeclaration,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    let gast::TypeRef::Primitive(scalar) = declaration.ty else {
        return None;
    };
    let mut extents = Vec::with_capacity(declaration.dimensions.len());
    for dimension in &declaration.dimensions {
        let gast::Dimension::Expr(expression) = dimension else {
            return None;
        };
        extents.push(u32::try_from(constant_integer(expression)?).ok()?);
    }
    Some((extents, scalar))
}

/// The declared shape of `name` among a function scope's parameters, outputs
/// and locals, resolved under the same flattened names
/// `function_parameters` / `function_locals` declare them with (record
/// fields included).
fn function_scope_shape<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    name: &str,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    for parameter in function.parameters() {
        let ty = view.value_type(parameter.value_type())?;
        if !ty.is_record() {
            if parameter_name(parameter).is_ok_and(|candidate| candidate.lexeme() == name) {
                return value_type_shape(ty, name, parameter.declaration().span());
            }
            continue;
        }
        for ordinal in 0..ty.record_field_count() {
            let (field_name, field_type) = view.record_field(parameter.value_type(), ordinal)?;
            if record_parameter_field_name(parameter, field_name)
                .is_ok_and(|candidate| candidate.lexeme() == name)
            {
                return value_type_shape(
                    view.value_type(field_type)?,
                    name,
                    parameter.declaration().span(),
                );
            }
        }
    }
    for value in function.values() {
        let ty = view.value_type(value.value_type())?;
        if !ty.is_record() {
            if value_name(value).is_ok_and(|candidate| candidate.lexeme() == name) {
                return value_type_shape(ty, name, value.declaration().span());
            }
            continue;
        }
        for ordinal in 0..ty.record_field_count() {
            let (field_name, field_type) = view.record_field(value.value_type(), ordinal)?;
            if record_value_field_name(value, field_name)
                .is_ok_and(|candidate| candidate.lexeme() == name)
            {
                return value_type_shape(
                    view.value_type(field_type)?,
                    name,
                    value.declaration().span(),
                );
            }
        }
    }
    None
}

/// A checked `ValueType`'s shape as (extents, element type), through the same
/// scalar mapping every declaration uses (so Enumeration compares as the
/// Integer it is declared as).
fn value_type_shape(
    ty: &dae::ValueType,
    name: &str,
    span: Span,
) -> Option<(Vec<u32>, gast::ScalarType)> {
    let scalar = scalar_type(ty.scalar_type(), name, span).ok()?;
    Some((ty.dimensions().to_vec(), scalar))
}

#[cfg(test)]
mod tests {
    use super::value_type_shape;
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;
    use rumoca_ir_galec::ast as gast;

    /// MLS §4.9.5: an enumeration value IS its ordinal, so an enumeration
    /// array compares shapes as the Integer array it is declared as. A shape
    /// derived any other way would refuse a whole-array move the scalar
    /// mapping in `lower.rs` accepts.
    #[test]
    fn an_enumeration_array_shapes_as_the_integer_array_it_is_declared_as() {
        let ty = dae::ValueType::array(dae::ScalarType::Enumeration, vec![3, 2]);
        assert_eq!(
            value_type_shape(&ty, "modes", Span::DUMMY),
            Some((vec![3, 2], gast::ScalarType::Integer))
        );
        let scalar = dae::ValueType::scalar(dae::ScalarType::Real);
        assert_eq!(
            value_type_shape(&scalar, "x", Span::DUMMY),
            Some((Vec::new(), gast::ScalarType::Real))
        );
    }

    /// A String has no GALEC scalar type, so its declared shape must not
    /// exist at all: answering anything would let a shape comparison prove a
    /// move of a value the target cannot even declare.
    #[test]
    fn a_string_value_has_no_declared_shape() {
        let ty = dae::ValueType::array(dae::ScalarType::String, vec![4]);
        assert_eq!(value_type_shape(&ty, "labels", Span::DUMMY), None);
    }
}
