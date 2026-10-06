//! MLS §11.2.1.1: one call from the old state, then ordered receiver writes.
use super::*;

pub(super) fn lower_function_loop_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    call: FunctionMultiOutputCall<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(loop_body.body()),
            values: None,
            owner_clock: None,
        },
        binders,
        call.callee,
        call.args,
        provenance,
    )?;
    let selected = call
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, output)| output.as_ref().map(|output| (ordinal, output)))
        .collect::<Vec<_>>();
    // Materialize every projection before changing any receiving definition.
    let results = operands.results(
        construction,
        selected.iter().map(|(ordinal, _)| *ordinal),
        provenance,
    )?;
    let mut pending = HashMap::new();
    let mut positions = HashMap::new();
    let mut assignments = Vec::with_capacity(selected.len());
    for ((_, output), mut value) in selected.into_iter().zip(results) {
        let target = function_value_coordinate(symbols.coordinates, output.target());
        if !output.subscripts().is_empty() {
            value = lower_function_array_update(
                construction,
                FunctionArrayUpdate {
                    symbols: LoweringSymbols {
                        coordinates: symbols.coordinates,
                        functions: symbols.functions,
                        shapes: symbols.shapes,
                        function_body: Some(loop_body.body()),
                        // Receiver indices observe earlier receiver writes;
                        // every resulting expression still reads the old body.
                        values: Some(&pending),
                        owner_clock: None,
                    },
                    binders,
                    base: pending.get(output.target()).copied(),
                    target,
                    subscripts: output.subscripts(),
                    value,
                    provenance,
                },
            )?;
        }
        pending.insert(output.target().clone(), value);
        // Compose repeated scalar/aggregate receivers left to right and
        // publish one final value per branded target, in first-source order.
        let position = *positions.entry(target).or_insert_with(|| {
            let position = assignments.len();
            assignments.push((target, value));
            position
        });
        assignments[position].1 = value;
    }
    construction
        .functions(|functions| functions.assign_all_loop(loop_body, &assignments, provenance))?;
    Ok(())
}
