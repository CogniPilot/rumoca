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
//! an input, `time`, a state derivative, a conditional over a varying guard, an
//! impure function result, or an unresolved coordinate is never classified
//! invariant, and neither is one whose definition is cyclic, absent, or
//! ambiguous.
//!
//! A call is invariant when its callee is pure and every argument is invariant.
//! MLS 3.7 §12.3 guarantees that a pure function "always gives the same output
//! for the same input", and a checked DAE function body reads only its own
//! inputs, locals, and constants, never a model coordinate. A function is
//! treated as impure when it is an external function without an explicit
//! `pure`, or when its body reaches such a function (the recursive case of
//! §12.3).

use std::collections::HashMap;

use rumoca_ir_dae as dae;

use crate::CausalDefinitions;
use crate::causal_definitions::families::family_element_definition;

/// Whole-model time-invariance facts: the algebraic variables proved
/// parameter-constant and the functions whose calls are deterministic in their
/// arguments, both indexed by DAE identity.
#[derive(Clone, Debug, Default)]
pub(crate) struct TimeInvariance {
    algebraics: Vec<bool>,
    pure_functions: Vec<bool>,
}

impl TimeInvariance {
    /// Derive the invariance facts of `view`.
    pub(crate) fn derive(view: dae::DaeView<'_>) -> Self {
        Self::derive_with_causal(view, &CausalDefinitions::derive(view))
    }

    /// [`TimeInvariance::derive`] reusing a causal-definition analysis the
    /// caller already built, avoiding a second derivation on large models.
    pub(crate) fn derive_with_causal<'dae>(
        view: dae::DaeView<'dae>,
        causal: &CausalDefinitions<'dae>,
    ) -> Self {
        let pure_functions = pure_functions(view);
        let algebraics = invariant_algebraic_variables(view, causal, &pure_functions);
        Self {
            algebraics,
            pure_functions,
        }
    }

    /// Whether the algebraic variable with this identity index is
    /// parameter-constant.
    pub(crate) fn algebraic(&self, index: u32) -> bool {
        self.algebraics
            .get(index as usize)
            .copied()
            .unwrap_or(false)
    }

    /// The parameter-constant flag of every variable, indexed by identity.
    pub(crate) fn algebraics(&self) -> &[bool] {
        &self.algebraics
    }

    /// Whether any algebraic variable is parameter-constant.
    pub(crate) fn has_invariant_algebraic(&self) -> bool {
        self.algebraics.iter().any(|&flag| flag)
    }

    /// Whether `expression` holds the same value at every instant.
    pub(crate) fn expression<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> bool {
        expression_is_time_invariant(view, expression, self.scope())
    }

    fn scope(&self) -> InvarianceScope<'_> {
        InvarianceScope {
            algebraics: &self.algebraics,
            pure_functions: &self.pure_functions,
        }
    }
}

/// Borrowed facts one invariance query reads.
#[derive(Clone, Copy)]
struct InvarianceScope<'facts> {
    algebraics: &'facts [bool],
    pure_functions: &'facts [bool],
}

impl InvarianceScope<'_> {
    fn pure_function(self, function: dae::FunctionId<'_>) -> bool {
        self.pure_functions
            .get(function.index() as usize)
            .copied()
            .unwrap_or(false)
    }
}

/// Whether `expression` holds the same value at every instant, given the
/// algebraic variables already proved time-invariant.
///
/// The accepted forms mirror the whole-model constancy that differentiation
/// resolves to zero: literals, checked integer ranges, parameter coordinates,
/// invariant algebraic coordinates, and domain binders fixed at the current
/// point. Arithmetic, conditionals, aggregates, field selections, builtins,
/// pure calls, comprehensions and indexing preserve invariance when their
/// operands are invariant.
fn expression_is_time_invariant<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    scope: InvarianceScope<'_>,
) -> bool {
    let Some(node) = view.expression(expression) else {
        return false;
    };
    let invariant = |operand| expression_is_time_invariant(view, operand, scope);
    let all = |operands: dae::ExpressionOperands<'dae>| operands.iter().all(invariant);
    match node.operation() {
        dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Real(_)
            | dae::DaeLiteral::Integer(_)
            | dae::DaeLiteral::Boolean(_)
            | dae::DaeLiteral::Enumeration(_),
        )
        | dae::ExpressionOperation::Range(_) => true,
        dae::ExpressionOperation::Coordinate(coordinate) => {
            coordinate_is_time_invariant(coordinate, scope.algebraics)
        }
        // Every unary and binary operator of this IR is a pure function of its
        // operands, so a fixed operand yields a fixed result.
        dae::ExpressionOperation::Unary { operand, .. }
        | dae::ExpressionOperation::Field { base: operand, .. } => invariant(operand),
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => invariant(lhs) && invariant(rhs),
        // A parameter-guarded selection among fixed values is itself fixed. The
        // operand list carries both the guards and the branch values.
        dae::ExpressionOperation::Conditional(operands)
        | dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Record(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        } => all(operands),
        dae::ExpressionOperation::Call {
            function,
            arguments,
            ..
        } => scope.pure_function(function) && all(arguments),
        // A comprehension domain is fixed at translation (MLS 3.7 §10.4.1), so
        // its value is fixed when its body is: the binder coordinate is invariant.
        dae::ExpressionOperation::Comprehension { body, .. } => invariant(body),
        dae::ExpressionOperation::Index { base, subscripts } => {
            invariant(base)
                && subscripts.iter().all(|subscript| match subscript {
                    dae::SubscriptView::Whole { .. } => true,
                    dae::SubscriptView::Index { expression, .. }
                    | dae::SubscriptView::Slice { expression, .. } => invariant(expression),
                })
        }
        _ => false,
    }
}

/// The functions whose every call is deterministic in its arguments, indexed
/// by function identity.
///
/// MLS 3.7 §12.3 treats a function as impure when "It is declared impure. It
/// is an external function without explicit purity. It calls another function
/// treated as impure". A checked DAE admits no Modelica body declared impure,
/// and records an external body's purity with the bare form already impure, so
/// the greatest fixpoint below removes every function whose body reaches an
/// impure callee. Recursion among pure functions stays pure.
fn pure_functions(view: dae::DaeView<'_>) -> Vec<bool> {
    let functions = (0..view.function_count())
        .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)))
        .collect::<Vec<_>>();
    let mut pure = functions
        .iter()
        .map(|function| {
            function
                .external()
                .is_none_or(|external| external.purity().is_pure())
        })
        .collect::<Vec<_>>();
    let mut traversal = dae::ExpressionTraversal::new();
    let callees = functions
        .iter()
        .map(|function| function_callees(view, *function, &mut traversal))
        .collect::<Vec<_>>();
    loop {
        let mut changed = false;
        for (index, callees) in callees.iter().enumerate() {
            if pure[index]
                && callees
                    .iter()
                    .any(|callee| !pure.get(*callee as usize).copied().unwrap_or(false))
            {
                pure[index] = false;
                changed = true;
            }
        }
        if !changed {
            return pure;
        }
    }
}

/// Every function a function body calls, by identity index.
fn function_callees<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    traversal: &mut dae::ExpressionTraversal<'dae>,
) -> Vec<u32> {
    let mut roots = (0..function.definition_count())
        .filter_map(|index| function.definition_id(index))
        .filter_map(|id| view.function_definition(id))
        .map(|definition| definition.rhs())
        .collect::<Vec<_>>();
    statement_roots(function.statements(), &mut roots);
    if let Some(external) = function.external() {
        roots.extend(external.arguments().filter_map(|argument| match argument {
            dae::ExternalArgumentView::Input(expression) => Some(expression),
            dae::ExternalArgumentView::Output(_) => None,
        }));
    }
    let mut callees = Vec::new();
    traversal.visit_pruned(view, roots, |_, node| {
        if let dae::ExpressionOperation::Call { function, .. } = node.operation() {
            callees.push(function.index());
        }
        true
    });
    callees
}

/// The expressions a statement list evaluates besides its definitions: the
/// assertion predicates and messages and the shared branch guards and values.
fn statement_roots<'dae>(
    statements: dae::FunctionStatements<'dae>,
    roots: &mut Vec<dae::ExprId<'dae>>,
) {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Conditional { conditional } => {
                roots.extend(conditional.conditions());
                for branch in conditional
                    .branches()
                    .chain(std::iter::once(conditional.fallback()))
                {
                    statement_roots(branch.statements(), roots);
                }
            }
            dae::FunctionStatementView::RequireAssigned { definition, .. } => {
                roots.push(definition.rhs())
            }
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => {}
            dae::FunctionStatementView::Assertion {
                condition, message, ..
            } => roots.extend([condition, message]),
            dae::FunctionStatementView::For { statements, .. } => {
                statement_roots(statements, roots);
            }
        }
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
fn invariant_algebraic_variables<'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalDefinitions<'dae>,
    pure_functions: &[bool],
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
            let scope = InvarianceScope {
                algebraics: &invariant,
                pure_functions,
            };
            if variable_is_time_invariant(view, causal, &family_values, id, variable, scope) {
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
    invariant: InvarianceScope<'_>,
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

#[cfg(test)]
mod tests;
