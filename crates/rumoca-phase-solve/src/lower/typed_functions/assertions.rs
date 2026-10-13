//! Function-assertion discovery over checked DAE statement owners.

use std::{
    collections::{HashMap, HashSet},
    ops::Range,
    sync::Arc,
};

use super::assigned_sources::{FunctionAssertionCondition, FunctionAssertionMessage};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[derive(Clone)]
pub(super) struct FunctionAssertion<'dae> {
    pub(super) condition: FunctionAssertionCondition<'dae>,
    pub(super) message: FunctionAssertionMessage<'dae>,
    pub(crate) level: dae::AssertionLevel,
    pub(super) provenance: dae::DaeProvenance,
}

/// Source-owned iteration statements and their assertion interval. Discovery
/// issues this association while it visits the DAE statement, before lowering.
#[derive(Clone)]
pub(super) struct LoopStatements<'dae> {
    pub(super) statements: dae::FunctionStatements<'dae>,
    pub(super) assertions: Range<usize>,
}

#[derive(Clone)]
pub(super) struct BranchStatements<'dae> {
    pub(super) branch: dae::FunctionBranchView<'dae>,
    pub(super) assertions: Range<usize>,
}

#[derive(Clone)]
pub(super) struct ConditionalStatements<'dae> {
    pub(super) states: Vec<dae::FunctionValueId<'dae>>,
    pub(super) branches: Vec<BranchStatements<'dae>>,
    pub(super) fallback: BranchStatements<'dae>,
    pub(super) assertions: Range<usize>,
}

pub(super) struct FunctionAssertionInventory<'dae> {
    pub(super) assertions: Vec<FunctionAssertion<'dae>>,
    pub(super) loops: Arc<HashMap<dae::FunctionFoldId<'dae>, LoopStatements<'dae>>>,
    pub(super) conditionals:
        Arc<HashMap<dae::FunctionConditionalId<'dae>, ConditionalStatements<'dae>>>,
}

impl<'dae> FunctionAssertionInventory<'dae> {
    pub(super) fn discover(
        view: dae::DaeView<'dae>,
        function: dae::FunctionView<'dae>,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let mut assertions = Vec::new();
        let mut loops = HashMap::new();
        let mut conditionals = HashMap::new();
        collect_assertion_conditions(
            view,
            function.statements(),
            &mut assertions,
            &mut loops,
            &mut conditionals,
        )?;
        super::assigned_sources::retain_required_states(
            view,
            function.statements(),
            &mut conditionals,
        )?;
        Ok(Self {
            assertions,
            loops: Arc::new(loops),
            conditionals: Arc::new(conditionals),
        })
    }
}

pub(super) fn assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Result<Vec<FunctionAssertion<'dae>>, solve::SolveProgramConstructionError> {
    Ok(FunctionAssertionInventory::discover(view, function)?.assertions)
}

fn collect_assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
    assertions: &mut Vec<FunctionAssertion<'dae>>,
    loops: &mut HashMap<dae::FunctionFoldId<'dae>, LoopStatements<'dae>>,
    conditionals: &mut HashMap<dae::FunctionConditionalId<'dae>, ConditionalStatements<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::RequireAssigned {
                conditional,
                value,
                provenance,
                ..
            } => assertions.push(FunctionAssertion {
                condition: FunctionAssertionCondition::Assigned { conditional, value },
                message: FunctionAssertionMessage::UnassignedValue(value),
                level: dae::AssertionLevel::Error,
                provenance,
            }),
            dae::FunctionStatementView::Conditional { conditional } => {
                collect_conditional_assertions(view, conditional, assertions, loops, conditionals)?;
            }
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => {}
            dae::FunctionStatementView::Assertion {
                condition,
                message,
                level,
                provenance,
            } => assertions.push(FunctionAssertion {
                condition: FunctionAssertionCondition::Expression(condition),
                message: FunctionAssertionMessage::Expression(message),
                level,
                provenance,
            }),
            dae::FunctionStatementView::For {
                fold, statements, ..
            } => {
                view.function_fold(fold)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                let start = assertions.len();
                collect_assertion_conditions(
                    view,
                    statements.clone(),
                    assertions,
                    loops,
                    conditionals,
                )?;
                if loops
                    .insert(
                        fold,
                        LoopStatements {
                            statements,
                            assertions: start..assertions.len(),
                        },
                    )
                    .is_some()
                {
                    return Err(solve::SolveProgramConstructionError::WireMismatch);
                }
            }
        }
    }
    Ok(())
}

fn collect_branch_assertions<'dae>(
    view: dae::DaeView<'dae>,
    branch: dae::FunctionBranchView<'dae>,
    assertions: &mut Vec<FunctionAssertion<'dae>>,
    loops: &mut HashMap<dae::FunctionFoldId<'dae>, LoopStatements<'dae>>,
    conditionals: &mut HashMap<dae::FunctionConditionalId<'dae>, ConditionalStatements<'dae>>,
) -> Result<BranchStatements<'dae>, solve::SolveProgramConstructionError> {
    let start = assertions.len();
    collect_assertion_conditions(view, branch.statements(), assertions, loops, conditionals)?;
    Ok(BranchStatements {
        branch,
        assertions: start..assertions.len(),
    })
}

fn collect_conditional_assertions<'dae>(
    view: dae::DaeView<'dae>,
    conditional: dae::FunctionConditionalBodyView<'dae>,
    assertions: &mut Vec<FunctionAssertion<'dae>>,
    loops: &mut HashMap<dae::FunctionFoldId<'dae>, LoopStatements<'dae>>,
    conditionals: &mut HashMap<dae::FunctionConditionalId<'dae>, ConditionalStatements<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    let start = assertions.len();
    let branches = conditional
        .branches()
        .map(|branch| collect_branch_assertions(view, branch, assertions, loops, conditionals))
        .collect::<Result<Vec<_>, _>>()?;
    let fallback = collect_branch_assertions(
        view,
        conditional.fallback(),
        assertions,
        loops,
        conditionals,
    )?;
    if conditionals
        .insert(
            conditional.id(),
            ConditionalStatements {
                states: Vec::new(),
                branches,
                fallback,
                assertions: start..assertions.len(),
            },
        )
        .is_some()
    {
        return Err(solve::SolveProgramConstructionError::WireMismatch);
    }
    Ok(())
}

/// Every value an assertion message converts to text that the declaring
/// function's frame evaluates as one owner output, in message order.
///
/// Failed-only regions evaluate these values in the declaring frame, including
/// loop-local binders and message-only calls.
pub(super) fn message_values<'dae>(
    view: dae::DaeView<'dae>,
    assertion: &FunctionAssertion<'dae>,
) -> Vec<dae::ExprId<'dae>> {
    let mut values = Vec::new();
    if let Some(message) = assertion.message.expression() {
        collect_message_values(view, message, &mut values);
    }
    values
}

fn collect_message_values<'dae>(
    view: dae::DaeView<'dae>,
    message: dae::ExprId<'dae>,
    values: &mut Vec<dae::ExprId<'dae>>,
) {
    let Some(node) = view.expression(message) else {
        return;
    };
    match node.operation() {
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } => {
            collect_message_values(view, lhs, values);
            collect_message_values(view, rhs, values);
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            let options = match format {
                dae::StringConversionFormatView::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } => vec![minimum_length, left_justified, significant_digits],
                dae::StringConversionFormatView::Format { .. } => Vec::new(),
            };
            for value in std::iter::once(value).chain(options.into_iter().flatten()) {
                values.push(value);
            }
        }
        _ => {}
    }
}

/// Every call owner the typed body lowering reads.
///
/// The body lowers each statement in order, so a definition no result or
/// assertion reads is still lowered; its calls need registered owners exactly
/// like the calls a result reaches.
pub(super) fn nested_calls<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    assertions: &[FunctionAssertion<'dae>],
) -> Vec<dae::ExprId<'dae>> {
    nested_calls_outside(view, function, assertions, &HashSet::new())
}

/// The call owners the body reads, leaving out the refused message values.
pub(super) fn nested_calls_outside<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    assertions: &[FunctionAssertion<'dae>],
    refused: &HashSet<dae::ExprId<'dae>>,
) -> Vec<dae::ExprId<'dae>> {
    let mut roots = function.result_values().rhs_iter().collect::<Vec<_>>();
    roots.extend(
        assertions
            .iter()
            .flat_map(|assertion| assertion.condition.inputs()),
    );
    roots.extend(
        assertions
            .iter()
            .flat_map(|assertion| message_values(view, assertion))
            .filter(|value| !refused.contains(value)),
    );
    collect_statement_roots(function.statements(), &mut roots, &|_, _| false);
    let mut calls = Vec::new();
    let mut seen = HashSet::new();
    for root in roots {
        dae::for_each_expression(view, root, |_, node| {
            if let dae::ExpressionOperation::Call { owner, .. } = node.operation()
                && seen.insert(owner)
            {
                calls.push(owner);
            }
        });
    }
    calls
}

pub(super) fn statement_roots<'dae>(
    statements: dae::FunctionStatements<'dae>,
    has_state: &impl Fn(dae::FunctionConditionalId<'dae>, dae::FunctionValueId<'dae>) -> bool,
) -> Vec<dae::ExprId<'dae>> {
    let mut roots = Vec::new();
    collect_statement_roots(statements, &mut roots, has_state);
    roots
}

fn collect_statement_roots<'dae>(
    statements: dae::FunctionStatements<'dae>,
    roots: &mut Vec<dae::ExprId<'dae>>,
    has_state: &impl Fn(dae::FunctionConditionalId<'dae>, dae::FunctionValueId<'dae>) -> bool,
) {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::RequireAssigned {
                conditional, value, ..
            } => {
                // Every available exact predecessor state crosses the checked
                // state interface, including one below a newer local owner.
                // Its old guards and arm payloads are not reads in this scope.
                conditional.visit_value_inputs_pruned(value, has_state, |input| roots.push(input));
            }
            dae::FunctionStatementView::Conditional { conditional } => {
                roots.extend(conditional.conditions());
                for branch in conditional
                    .branches()
                    .chain(std::iter::once(conditional.fallback()))
                {
                    collect_statement_roots(branch.statements(), roots, has_state);
                }
            }
            dae::FunctionStatementView::Assignment { definition } => roots.push(definition.rhs()),
            dae::FunctionStatementView::AssignmentGroup { definitions, .. } => {
                roots.extend(definitions.rhs_iter())
            }
            dae::FunctionStatementView::Assertion { condition, .. } => roots.push(condition),
            dae::FunctionStatementView::For { statements, .. } => {
                collect_statement_roots(statements, roots, has_state);
            }
        }
    }
}

/// The message values that call a function holding call-scoped assertions.
///
/// A message renders in a failed-only region, where a nested assertion would be
/// an effect of the message itself. SOLVE-C25 refuses such a message at its
/// span, so no capture is issued for the value and no slot for the callee.
pub(super) fn refused_message_values<'dae>(
    view: dae::DaeView<'dae>,
    assertions: &[FunctionAssertion<'dae>],
    callees: &HashMap<dae::ExprId<'dae>, super::CalleeInterface<'dae>>,
) -> HashSet<dae::ExprId<'dae>> {
    let mut refused = HashSet::new();
    for value in assertions
        .iter()
        .flat_map(|assertion| message_values(view, assertion))
    {
        let mut asserting = false;
        dae::for_each_expression(view, value, |_, node| {
            if let dae::ExpressionOperation::Call { owner, .. } = node.operation() {
                asserting |= callees
                    .get(&owner)
                    .is_some_and(|callee| !callee.assertions.is_empty());
            }
        });
        if asserting {
            refused.insert(value);
        }
    }
    refused
}
