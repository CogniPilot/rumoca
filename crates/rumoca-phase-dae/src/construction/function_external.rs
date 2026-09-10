use super::*;

/// Lower one proven MLS §12.9 external interface into its checked DAE body.
///
/// The reservation is consumed by `define_external`, so an external function
/// receives exactly one body and never opens a Modelica build state. Argument
/// expressions are lowered without a body capability: an external body has no
/// statement that could have defined a function value, so only the declared
/// formals and closed constants are reachable from an argument.
pub(super) fn define_external_function<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    shapes: &ShapeEnvironment,
    reservation: dae::FunctionReservation<'_, 'dae>,
    function: &rumoca_core::Function,
    plan: &ExternalFunctionPlan,
) -> Result<(), dae::DaeConstructionError> {
    let external = function
        .external
        .as_ref()
        .expect("external lowering runs only for declared external functions");
    let provenance = dae::DaeProvenance::source(function.span)?;
    let mut arguments = Vec::with_capacity(plan.arguments.len());
    for argument in &plan.arguments {
        arguments.push(match argument {
            ExternalArgumentPlan::Input(ordinal) => {
                let source = &external.args[*ordinal];
                let lowered = lower_expression_scoped(
                    construction,
                    LoweringSymbols {
                        coordinates,
                        functions,
                        shapes,
                        function_body: None,
                        values: None,
                        owner_clock: None,
                    },
                    &HashMap::new(),
                    source,
                    None,
                )?;
                dae::ExternalArgument::Input(lowered)
            }
            ExternalArgumentPlan::Output(name) => {
                dae::ExternalArgument::Output(function_value_coordinate(coordinates, name))
            }
        });
    }
    let result = plan
        .result
        .as_ref()
        .map(|name| function_value_coordinate(coordinates, name));
    let body = dae::ExternalFunctionBody::new(
        plan.purity,
        plan.language,
        plan.symbol.clone(),
        arguments,
        result,
        plan.linkage.clone(),
    );
    construction.functions(|owner| owner.define_external(reservation, body, provenance))
}
