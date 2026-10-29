//! Closed finite-domain counter proofs (MLS 11.2.2; GAL-028/030).
//!
//! The ordinary loop invariant cannot bound `k := k + 1`: its hull grows
//! forever although a checked finite domain executes only finitely often.
//! Recognize one constant-step write of a scalar Integer carried by this fold.
//! Count its closed binder-only guards in the exact domain, then issue the
//! range at that definition. This is bounded proof work, never statement or
//! array expansion: emission retains the original compact loop and guards.
//! Unknown controls, recurrences, nested folds and exhausted proof fuel issue
//! no fact; the existing index proof still decides admission.

mod closed_guard;

use super::*;
use closed_guard::ClosedGuard;

const COORDINATE_BUDGET: u32 = 65_536;
const EXPRESSION_BUDGET: usize = 1_000_000;

#[derive(Clone, Default)]
pub(in crate::lower) struct FiniteCounterBounds<'dae> {
    definitions: HashMap<dae::FunctionDefinitionId<'dae>, (i64, i64)>,
}

impl<'dae> FiniteCounterBounds<'dae> {
    pub(in crate::lower) fn range(
        &self,
        definition: dae::FunctionDefinitionId<'dae>,
    ) -> Option<(i64, i64)> {
        self.definitions.get(&definition).copied()
    }

    fn for_loop(
        lowerer: &ExpressionLowerer<'_, 'dae>,
        fold: dae::FunctionFoldId<'dae>,
        body: dae::FunctionStatements<'dae>,
    ) -> Self {
        let mut result = Self::default();
        let mut sites = Vec::new();
        if collect_sites(body, &[], &mut sites, 0).is_none() {
            return result;
        }
        let mut writes = HashMap::new();
        for site in &sites {
            *writes.entry(site.definition.target()).or_insert(0usize) += 1;
        }
        let mut fuel = EXPRESSION_BUDGET;
        for site in &sites {
            // Every write is inventoried, including non-increment writes and
            // assignments in other conditional arms. A second site invalidates
            // this recurrence rather than being silently ignored.
            if writes[&site.definition.target()] != 1 {
                continue;
            }
            if let Some(range) = site_range(lowerer, fold, site, &mut fuel) {
                result.definitions.insert(site.definition.id(), range);
            }
        }
        result
    }
}

struct Site<'dae> {
    definition: dae::FunctionDefinitionView<'dae>,
    guards: Vec<(dae::ExprId<'dae>, bool)>,
}

fn collect_sites<'dae>(
    body: dae::FunctionStatements<'dae>,
    guards: &[(dae::ExprId<'dae>, bool)],
    sites: &mut Vec<Site<'dae>>,
    depth: usize,
) -> Option<()> {
    if depth > 64 {
        return None;
    }
    for statement in body {
        match statement {
            dae::FunctionStatementView::Assignment { definition } => sites.push(Site {
                definition,
                guards: guards.to_vec(),
            }),
            dae::FunctionStatementView::Conditional { conditional } => {
                let mut path = guards.to_vec();
                for (condition, branch) in conditional.conditions().zip(conditional.branches()) {
                    path.push((condition, true));
                    collect_sites(branch.statements(), &path, sites, depth + 1)?;
                    path.last_mut()?.1 = false;
                }
                collect_sites(conditional.fallback().statements(), &path, sites, depth + 1)?;
            }
            dae::FunctionStatementView::Assertion { .. } => {}
            // No proof is issued for simultaneous writes or another lifetime.
            dae::FunctionStatementView::AssignmentGroup { .. }
            | dae::FunctionStatementView::For { .. }
            | dae::FunctionStatementView::RequireAssigned { .. } => return None,
        }
    }
    Some(())
}

fn site_range<'dae>(
    lowerer: &ExpressionLowerer<'_, 'dae>,
    fold: dae::FunctionFoldId<'dae>,
    site: &Site<'dae>,
    fuel: &mut usize,
) -> Option<(i64, i64)> {
    let view = lowerer.view;
    let function = view.function(fold.function())?;
    let target = function
        .values()
        .find(|v| v.id() == site.definition.target())?;
    let ty = view.value_type(target.value_type())?;
    if ty.scalar_type() != dae::ScalarType::Integer || !ty.dimensions().is_empty() {
        return None;
    }
    let initial = *lowerer
        .local_integer_bounds
        .get(value_name(target).ok()?.lexeme())?;
    let step = increment(view, fold, site.definition)?;
    let count = count_updates(view, fold, &site.guards, fuel)?;
    if count == 0 {
        return None;
    }
    let first = (initial.0.checked_add(step)?, initial.1.checked_add(step)?);
    let total = step.checked_mul(i64::from(count))?;
    let last = (initial.0.checked_add(total)?, initial.1.checked_add(total)?);
    Some((first.0.min(last.0), first.1.max(last.1)))
}

fn increment<'dae>(
    view: dae::DaeView<'dae>,
    fold: dae::FunctionFoldId<'dae>,
    definition: dae::FunctionDefinitionView<'dae>,
) -> Option<i64> {
    let dae::ExpressionOperation::Binary { operator, lhs, rhs } =
        view.expression(definition.rhs())?.operation()
    else {
        return None;
    };
    let mut expression = lhs;
    // Follow only definition-owned aliases of this exact local, ending at
    // this fold's carried input. Display names never establish identity.
    for _ in 0..64 {
        match view.expression(expression)?.operation() {
            dae::ExpressionOperation::FunctionValue {
                value,
                definition: source,
            } if value == definition.target() => expression = source.rhs(),
            dae::ExpressionOperation::FunctionFoldParameter {
                fold: source,
                carried,
                ..
            } if source == fold => {
                if view.function_fold(fold)?.targets().nth(carried as usize)? != definition.target()
                {
                    return None;
                }
                let step = literal_integer(view, rhs)?;
                return match operator {
                    dae::BinaryOperator::Add => Some(step),
                    dae::BinaryOperator::Subtract => step.checked_neg(),
                    _ => None,
                };
            }
            _ => return None,
        }
    }
    None
}

fn count_updates<'dae>(
    view: dae::DaeView<'dae>,
    fold: dae::FunctionFoldId<'dae>,
    guards: &[(dae::ExprId<'dae>, bool)],
    fuel: &mut usize,
) -> Option<u32> {
    let fold = view.function_fold(fold)?;
    if fold.continuation().is_some() {
        return None;
    }
    let domain = view.domain(fold.domain())?;
    if domain.scalar_count() > COORDINATE_BUDGET {
        return None;
    }
    let mut count = 0u32;
    for ordinal in 0..domain.scalar_count() {
        *fuel = fuel.checked_sub(1)?;
        let mut coordinates = Vec::new();
        let mut remainder = u64::from(ordinal);
        for binder in domain.structured().binders.iter().rev() {
            let distance = (i128::from(binder.upper) - i128::from(binder.lower)).abs();
            let extent = u64::try_from(distance / i128::from(binder.step).abs() + 1).ok()?;
            let offset = remainder % extent;
            remainder /= extent;
            coordinates.push(
                i64::try_from(
                    i128::from(binder.lower) + i128::from(binder.step) * i128::from(offset),
                )
                .ok()?,
            );
        }
        coordinates.reverse();
        let mut active = true;
        for &(guard, expected) in guards {
            let actual =
                ClosedGuard::evaluate(view, fold.domain(), &coordinates, guard, fuel)?.boolean()?;
            if actual != expected {
                active = false;
                break;
            }
        }
        count = count.checked_add(u32::from(active))?;
    }
    Some(count)
}

/// Install definition-bound facts only for this exact loop lifetime. Cloned
/// speculative lowerers carry the same facts, while the saved outer context
/// is restored even when ordinary lowering refuses the loop.
pub(super) fn lower_loop<'a, 'dae>(
    view: dae::DaeView<'dae>,
    fold: dae::FunctionFoldId<'dae>,
    body: dae::FunctionStatements<'dae>,
    span: Span,
    lowerer: &mut ExpressionLowerer<'a, 'dae>,
    statements: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), GalecTargetError> {
    let saved = lowerer.finite_counter_bounds.clone();
    let proven = FiniteCounterBounds::for_loop(lowerer, fold, body.clone());
    lowerer
        .finite_counter_bounds
        .definitions
        .extend(proven.definitions);
    let assigned = assigned_local_names(view, body.clone());
    let speculative_body = body.clone();
    let carried = LoopIntegerBounds::enter(
        lowerer,
        assigned,
        function_loop_runs_body(view, fold),
        |speculative| {
            lower_function_for(
                view,
                fold,
                speculative_body.clone(),
                span,
                speculative,
                &mut Vec::new(),
            )
        },
    );
    let result = lower_function_for(view, fold, body, span, lowerer, statements)
        .and_then(|()| carried.commit(lowerer, span));
    lowerer.finite_counter_bounds = saved;
    result
}
