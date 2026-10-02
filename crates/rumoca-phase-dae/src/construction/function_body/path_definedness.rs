//! MLS §12.4.4 definedness predicates of path-partial function values.
//!
//! A top-level conditional may define a value on some paths only (see
//! `FunctionDefinednessPlan`). Construction keeps one Boolean predicate per
//! such value that holds exactly on the executed paths that defined it, and
//! asserts it where the plan places a use or the return of the value: "It is
//! an error to use or return an uninitialized variable in a function."

use super::*;

/// One target a top-level conditional leaves path-partial, with the predicate
/// it already carried when an earlier conditional left it path-partial too,
/// and the typed literal the join takes on paths that never write it.
pub(in crate::construction) struct PartialTarget<'dae> {
    pub(in crate::construction) name: VarName,
    pub(in crate::construction) prior: Option<dae::ExprId<'dae>>,
    pub(in crate::construction) dead: dae::ExprId<'dae>,
}

/// Definedness predicates of the path-partial values of one top-level
/// sequence, keyed by value.
#[derive(Default)]
pub(in crate::construction) struct DefinednessPredicates<'dae> {
    defined: HashMap<VarName, dae::ExprId<'dae>>,
}

impl<'dae> DefinednessPredicates<'dae> {
    /// The partial targets of one conditional, each with the predicate it
    /// carries in when it was already path-partial and its lowered seed.
    pub(in crate::construction) fn partial_targets(
        &self,
        construction: &mut dae::DaeConstruction<'dae>,
        partial: &[PartialJoinPlan],
        span: Span,
    ) -> Result<Vec<PartialTarget<'dae>>, dae::DaeConstructionError> {
        partial
            .iter()
            .map(|join| {
                let prior = if join.was_partial {
                    Some(self.predicate(&join.target, span)?)
                } else {
                    None
                };
                Ok(PartialTarget {
                    name: join.target.clone(),
                    prior,
                    dead: lower_function_value_seed(construction, &join.seed, span)?,
                })
            })
            .collect()
    }

    pub(in crate::construction) fn record(&mut self, defined: Vec<(VarName, dae::ExprId<'dae>)>) {
        self.defined.extend(defined);
    }

    fn predicate(
        &self,
        name: &VarName,
        span: Span,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.defined
            .get(name)
            .copied()
            .ok_or(dae::DaeConstructionError::IncompleteDefinition {
                kind: "function definedness predicate",
                index: 0,
                span,
            })
    }

    /// Assert that the executed path defined each value before its use or
    /// return; every path past the assertion has the value's definition.
    pub(in crate::construction) fn assert_defined(
        &mut self,
        construction: &mut dae::DaeConstruction<'dae>,
        body: &mut dae::FunctionBody<'dae>,
        names: &[VarName],
        span: Span,
    ) -> Result<(), dae::DaeConstructionError> {
        let provenance =
            dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, span)?;
        for name in names {
            let condition = self.predicate(name, span)?;
            let message = construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::String(format!(
                        "`{name}` is used without a value: the executed path never assigned it"
                    )))
            })?;
            construction
                .functions(|functions| functions.assertion(body, condition, message, provenance))?;
            self.defined.remove(name);
        }
        Ok(())
    }
}

/// The predicate each path-partial target of one conditional carries out: a
/// writing or never-completing path defines it, any other path keeps the
/// predicate it carried in (`false` for a value with no earlier definition).
pub(super) fn lower_path_definedness<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    input: &FunctionConditional<'_, '_, 'dae>,
    conditions: &[dae::ExprId<'dae>],
    branch_values: &[BranchState<'dae>],
    fallback_values: Option<&BranchState<'dae>>,
    provenance: dae::DaeProvenance,
) -> Result<Vec<(VarName, dae::ExprId<'dae>)>, dae::DaeConstructionError> {
    if input.partial.is_empty() {
        return Ok(Vec::new());
    }
    let literal = |construction: &mut dae::DaeConstruction<'dae>, value: bool| {
        construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Boolean(value))
        })
    };
    let defines = literal(construction, true)?;
    let undefined = literal(construction, false)?;
    let diverges = input
        .blocks
        .iter()
        .map(|block| branch_never_completes(&block.stmts))
        .collect::<Vec<_>>();
    let fallback_diverges = input.fallback.is_some_and(branch_never_completes);
    let mut defined = Vec::with_capacity(input.partial.len());
    for target in input.partial {
        let carried = target.prior.unwrap_or(undefined);
        let path = |state: Option<&BranchState<'dae>>, diverges: bool| {
            if diverges || state.is_some_and(|state| state.values.contains_key(&target.name)) {
                defines
            } else {
                carried
            }
        };
        let arms = conditions
            .iter()
            .zip(branch_values.iter().zip(&diverges))
            .map(|(condition, (state, diverges))| (*condition, path(Some(state), *diverges)))
            .collect::<Vec<_>>();
        let fallback = path(fallback_values, fallback_diverges);
        let predicate = construction
            .expressions(|expressions| expressions.at(provenance).conditional(arms, fallback))?;
        defined.push((target.name.clone(), predicate));
    }
    Ok(defined)
}
