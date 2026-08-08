//! Lower compiler-generated function seeds whose values are proven dead.

use super::*;

pub(super) fn lower_function_sequence_seeds<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    plans: &[FunctionStatementPlan],
    span: Span,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let mut seeds = Vec::new();
    collect_function_sequence_seeds(plans, &mut seeds);
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, span)?;
    for (target, seed) in seeds {
        let value = lower_function_value_seed(construction, seed, span)?;
        let target = function_value_coordinate(symbols.coordinates, target);
        construction
            .functions(|functions| functions.assign(&mut body, target, value, provenance))?;
    }
    Ok(body)
}

pub(super) fn lower_named_function_seeds<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    seeds: &[(VarName, FunctionValueSeed)],
    span: Span,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, span)?;
    for (target, seed) in seeds {
        let value = lower_function_value_seed(construction, seed, span)?;
        let target = function_value_coordinate(symbols.coordinates, target);
        construction
            .functions(|functions| functions.assign(&mut body, target, value, provenance))?;
    }
    Ok(body)
}

pub(super) fn collect_function_sequence_seeds<'plan>(
    plans: &'plan [FunctionStatementPlan],
    seeds: &mut Vec<(&'plan VarName, &'plan FunctionValueSeed)>,
) {
    for plan in plans {
        match plan {
            FunctionStatementPlan::Assignment(assignment) => {
                if let Some(seed) = assignment.seed() {
                    seeds.push((assignment.target(), seed));
                }
            }
            FunctionStatementPlan::MultiOutputCall { outputs } => {
                seeds.extend(
                    outputs
                        .iter()
                        .flatten()
                        .filter_map(|output| output.seed().map(|seed| (output.target(), seed))),
                );
            }
            FunctionStatementPlan::RecordAssembly(assembly) => {
                if let Some(seed) = &assembly.seed {
                    seeds.push((&assembly.target, seed));
                }
            }
            FunctionStatementPlan::ArrayAssembly(assembly) => {
                if let Some(seed) = &assembly.seed {
                    seeds.push((&assembly.target, seed));
                }
            }
            FunctionStatementPlan::If {
                branches, fallback, ..
            } => {
                for branch in branches {
                    collect_function_sequence_seeds(branch, seeds);
                }
                if let Some(fallback) = fallback {
                    collect_function_sequence_seeds(fallback, seeds);
                }
            }
            FunctionStatementPlan::ProvenBranch { statements, .. }
            | FunctionStatementPlan::For { statements, .. } => {
                collect_function_sequence_seeds(statements, seeds);
            }
            _ => {}
        }
    }
}
