//! Parameter-constant closure over continuous algebraic definitions.
//!
//! A continuous algebraic whose defining equation resolves, transitively, to
//! parameters and literal constants holds the same value at every instant, so
//! its time derivative is identically zero. The differential structure need not
//! raise such a coordinate's offset, and the prolongation folds its derivative
//! to zero rather than differentiating its defining expression, which may use a
//! non-smooth builtin such as `max` or `abs`.
//!
//! The closure is conservative: it proves invariance only for a coordinate whose
//! value is fixed by parameters and constants. A coordinate that reads a state,
//! an input, `time`, a state derivative, a function result, a conditional, or an
//! unresolved coordinate is never classified invariant, and neither is one whose
//! definition is cyclic, absent, or ambiguous.

use std::collections::HashMap;

use rumoca_ir_dae as dae;

use crate::CausalDefinitions;
use crate::residual_normalization::equation_sides;

/// Whether `expression` holds the same value at every instant, given the
/// algebraic variables already proved time-invariant.
///
/// The accepted forms mirror the whole-model constancy that differentiation
/// resolves to zero: literals, parameter coordinates, invariant algebraic
/// coordinates, fixed domain binders, and total arithmetic, aggregate, builtin,
/// and fixed index operations over such operands. Every other operation and
/// coordinate refuses, so an accepted expression is a genuine constant of the
/// whole model. A domain binder is a compile-time loop index whose value is
/// fixed while differentiating at a domain point, so it too is invariant.
pub(crate) fn expression_is_time_invariant<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    invariant_algebraics: &[bool],
) -> bool {
    let Some(node) = view.expression(expression) else {
        return false;
    };
    let all = |operands: dae::ExpressionOperands<'dae>| {
        operands
            .iter()
            .all(|operand| expression_is_time_invariant(view, operand, invariant_algebraics))
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Real(_)
            | dae::DaeLiteral::Integer(_)
            | dae::DaeLiteral::Boolean(_)
            | dae::DaeLiteral::Enumeration(_),
        ) => true,
        dae::ExpressionOperation::Coordinate(coordinate) => {
            coordinate_is_time_invariant(coordinate, invariant_algebraics)
        }
        // Every unary and binary operator of this IR is a pure function of its
        // operands, so a fixed operand yields a fixed result.
        dae::ExpressionOperation::Unary { operand, .. } => {
            expression_is_time_invariant(view, operand, invariant_algebraics)
        }
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
            expression_is_time_invariant(view, lhs, invariant_algebraics)
                && expression_is_time_invariant(view, rhs, invariant_algebraics)
        }
        // A parameter-guarded selection among fixed values is itself fixed. The
        // operand list carries both the guards and the branch values.
        dae::ExpressionOperation::Conditional(operands)
        | dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        } => all(operands),
        dae::ExpressionOperation::Index { base, subscripts } => {
            expression_is_time_invariant(view, base, invariant_algebraics)
                && subscripts.iter().all(|subscript| match subscript {
                    dae::SubscriptView::Whole { .. } => true,
                    dae::SubscriptView::Index { expression, .. }
                    | dae::SubscriptView::Slice { expression, .. } => {
                        expression_is_time_invariant(view, expression, invariant_algebraics)
                    }
                })
        }
        _ => false,
    }
}

fn coordinate_is_time_invariant(
    coordinate: dae::CoordinateView<'_>,
    invariant_algebraics: &[bool],
) -> bool {
    match coordinate {
        dae::CoordinateView::Parameter(_) | dae::CoordinateView::Binder(_) => true,
        dae::CoordinateView::Algebraic(algebraic) => invariant_algebraics
            .get(algebraic.index() as usize)
            .copied()
            .unwrap_or(false),
        _ => false,
    }
}

/// The continuous algebraic variables whose value is fixed by parameters and
/// constants, indexed by variable identity.
///
/// A variable enters the set when it has a single non-cyclic definition whose
/// value is time-invariant against the coordinates already in the set. The
/// monotone fixpoint starts empty, so a definitional cycle never resolves to
/// invariant. A `StateSelect.always` request is honored as a genuine dynamic
/// coordinate and is excluded.
pub(crate) fn invariant_algebraic_variables(view: dae::DaeView<'_>) -> Vec<bool> {
    invariant_algebraic_variables_with_causal(view, &CausalDefinitions::derive(view))
}

/// [`invariant_algebraic_variables`] reusing a causal-definition analysis the
/// caller already built, avoiding a second derivation on large models.
pub(crate) fn invariant_algebraic_variables_with_causal<'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalDefinitions<'dae>,
) -> Vec<bool> {
    let family_values = family_element_definitions(view, causal);
    let mut invariant = vec![false; view.variable_count()];
    loop {
        let mut changed = false;
        for (id, variable) in view.variables() {
            let index = id.index() as usize;
            if invariant[index] || variable.role() != dae::VariableRole::Algebraic {
                continue;
            }
            if variable.state_select() == rumoca_core::StateSelect::Always {
                continue;
            }
            if variable_is_time_invariant(view, causal, &family_values, id, variable, &invariant) {
                invariant[index] = true;
                changed = true;
            }
        }
        if !changed {
            return invariant;
        }
    }
}

/// Whether one algebraic declaration has a definition whose value is
/// time-invariant against the coordinates already proved invariant.
///
/// A whole-variable causal definition is checked directly. Otherwise every
/// scalar must carry a non-cyclic causal definition, or the whole array must be
/// defined element-wise by a single structured family, and each such value must
/// be invariant.
fn variable_is_time_invariant<'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalDefinitions<'dae>,
    family_values: &HashMap<u32, dae::ExprId<'dae>>,
    id: dae::VariableId<'dae>,
    variable: dae::VariableView<'dae>,
    invariant: &[bool],
) -> bool {
    if let Some(definition) = causal.definition_for_variable(id) {
        return expression_is_time_invariant(view, definition, invariant);
    }
    if let Some(count) = variable.value_type().scalar_count()
        && count > 0
        && causal.fully_defines_variable(id)
    {
        return (0..count).all(|scalar| {
            u32::try_from(scalar)
                .ok()
                .and_then(|scalar| causal.scalar_definition_for_variable(id, scalar))
                .is_some_and(|definition| expression_is_time_invariant(view, definition, invariant))
        });
    }
    family_values
        .get(&id.index())
        .is_some_and(|value| expression_is_time_invariant(view, *value, invariant))
}

/// The element-wise value each algebraic array receives from a single structured
/// family `target[binders] = value`, keyed by target variable identity.
///
/// A target that a causal definition already owns, or that more than one family
/// writes, is omitted so the closure never rests on an ambiguous definition.
fn family_element_definitions<'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalDefinitions<'dae>,
) -> HashMap<u32, dae::ExprId<'dae>> {
    let mut values: HashMap<u32, dae::ExprId<'dae>> = HashMap::new();
    let mut ambiguous: std::collections::HashSet<u32> = std::collections::HashSet::new();
    for owner in view.continuous_owners() {
        let dae::ContinuousOwnerView::Structured { family, .. } = owner else {
            continue;
        };
        let Some((target, value)) = family_element_definition(view, family) else {
            continue;
        };
        let index = target.index();
        if causal
            .definition_for_variable(dae::VariableId::from(target))
            .is_some()
            || causal.fully_defines_variable(dae::VariableId::from(target))
            || values.insert(index, value).is_some()
        {
            ambiguous.insert(index);
        }
    }
    for index in ambiguous {
        values.remove(&index);
    }
    values
}

/// Read a structured family body of the exact shape `target[binders] = value`,
/// where `target` is a whole algebraic array indexed by every binder of the
/// family's own domain, so the family defines each element of the array.
fn family_element_definition<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, dae::ExprId<'dae>)> {
    let mut bodies = family.bodies().iter();
    let body = bodies.next()?;
    if bodies.next().is_some() {
        return None;
    }
    let (lhs, rhs) = equation_sides(view, body)?;
    family_indexed_target(view, family, lhs)
        .map(|target| (target, rhs))
        .or_else(|| family_indexed_target(view, family, rhs).map(|target| (target, lhs)))
}

/// The algebraic array a family body indexes with exactly its domain binders.
fn family_indexed_target<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::AlgebraicId<'dae>> {
    let dae::ExpressionOperation::Index { base, subscripts } =
        view.expression(expression)?.operation()
    else {
        return None;
    };
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(target)) =
        view.expression(base)?.operation()
    else {
        return None;
    };
    let dimensions = view
        .variable(dae::VariableId::from(target))?
        .value_type()
        .dimensions()
        .len();
    if subscripts.len() != dimensions {
        return None;
    }
    subscripts
        .iter()
        .all(|subscript| family_domain_binder(view, family, subscript))
        .then_some(target)
}

/// Whether a subscript is a binder of the family's own domain.
fn family_domain_binder<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
    subscript: dae::SubscriptView<'dae>,
) -> bool {
    let dae::SubscriptView::Index { expression, .. } = subscript else {
        return false;
    };
    matches!(
        view.expression(expression).map(|node| node.operation()),
        Some(dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)))
            if binder.domain() == family.domain()
    )
}

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::*;

    /// A defining value for the algebraic `c`, built over a parameter `p` and a
    /// state `x`. The closure must accept a parameter-only value and refuse any
    /// value that reads the state.
    enum Value {
        BuiltinOverParameter,
        AlgebraicChain,
        MaxOverState,
        BuiltinOverState,
    }

    fn classify(value: Value) -> (dae::Dae, u32) {
        let mut sources = SourceMap::new();
        let text = "parameter Real p; Real x; Real a; Real c; equation definitions;";
        let source = sources.add("invariance.mo", text);
        let span = Span::from_offsets(source, 0, text.len());
        let provenance = dae::DaeProvenance::source(span).unwrap();
        let mut target = 0;
        let dae = dae::Dae::construct(sources, |dae| {
            let real = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    provenance,
                )
            })?;
            let (p, x, a, c) = dae.variables(|variables| {
                Ok((
                    variables.parameter(
                        VarName::new("p"),
                        real,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.state(
                        VarName::new("x"),
                        real,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.algebraic(
                        VarName::new("a"),
                        real,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.algebraic(
                        VarName::new("c"),
                        real,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            target = c.index();
            let coordinates = [
                dae::CoordinateInput::Parameter(p),
                dae::CoordinateInput::State(x),
                dae::CoordinateInput::Algebraic(a),
                dae::CoordinateInput::Algebraic(c),
            ];
            let residuals = dae.expressions(|expressions| {
                residuals(expressions, value, coordinates, provenance)
            })?;
            dae.continuous(|continuous| add_residuals(continuous, residuals, provenance))
        })
        .unwrap();
        (dae, target)
    }

    type Coordinates<'dae> = [dae::CoordinateInput<'dae>; 4];

    fn residuals<'dae>(
        expressions: &mut dae::Expressions<'_, 'dae>,
        value: Value,
        coordinates: Coordinates<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let [p, x, a, c] = coordinates;
        let p_value = expressions.at(provenance).coordinate(p)?;
        let x_value = expressions.at(provenance).coordinate(x)?;
        let a_value = expressions.at(provenance).coordinate(a)?;
        let c_value = expressions.at(provenance).coordinate(c)?;
        let a_definition = expressions
            .at(provenance)
            .builtin(dae::PureBuiltin::Abs, [p_value])?;
        let c_definition = match value {
            Value::BuiltinOverParameter => expressions
                .at(provenance)
                .builtin(dae::PureBuiltin::Abs, [p_value])?,
            Value::AlgebraicChain => {
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Add, a_value, a_value)?
            }
            Value::MaxOverState => expressions
                .at(provenance)
                .builtin(dae::PureBuiltin::Max, [x_value, p_value])?,
            Value::BuiltinOverState => expressions
                .at(provenance)
                .builtin(dae::PureBuiltin::Abs, [x_value])?,
        };
        let a_residual = expressions.at(provenance).binary(
            dae::BinaryOperator::Subtract,
            a_value,
            a_definition,
        )?;
        let c_residual = expressions.at(provenance).binary(
            dae::BinaryOperator::Subtract,
            c_value,
            c_definition,
        )?;
        Ok(vec![a_residual, c_residual])
    }

    fn add_residuals<'dae>(
        continuous: &mut dae::ContinuousEquations<'_, 'dae>,
        residuals: Vec<dae::ExprId<'dae>>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        for residual in residuals {
            continuous.equation(provenance, |equation| equation.residual(residual))?;
        }
        Ok(())
    }

    fn target_is_invariant(value: Value) -> bool {
        let (dae, target) = classify(value);
        dae.inspect(|view| invariant_algebraic_variables(view)[target as usize])
    }

    #[test]
    fn a_parameter_only_algebraic_is_time_invariant() {
        assert!(target_is_invariant(Value::BuiltinOverParameter));
    }

    #[test]
    fn an_algebraic_reached_only_through_invariant_algebraics_is_time_invariant() {
        assert!(target_is_invariant(Value::AlgebraicChain));
    }

    #[test]
    fn a_max_over_a_state_is_not_time_invariant() {
        assert!(!target_is_invariant(Value::MaxOverState));
    }

    #[test]
    fn a_builtin_over_a_state_is_not_time_invariant() {
        assert!(!target_is_invariant(Value::BuiltinOverState));
    }
}
