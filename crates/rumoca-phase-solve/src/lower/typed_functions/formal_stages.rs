//! Tensor residual kernels for source-bound differential-stage analysis.

use std::collections::HashMap;

use rumoca_core::{ComprehensionScalarView, Span, StructuredIndexDomain};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{FormalDerivativeStage, FormalDerivativeView, FormalStageEquation};

use super::model_coordinates::collect_model_coordinate_types;
use super::{
    ExpressionLowerer, LoweredValue, PureCallRegistry, arithmetic_profile, lower_primitive_type,
};

/// Analysis kernels retain their issuing formal root. They are not a prepared
/// model: numerical rank, initialization and executable state admission remain open.
pub struct FormalDerivativePrograms<'map, 'source, 'formal> {
    formal: FormalDerivativeView<'map, 'source, 'formal>,
    table: solve::SolvePureCallTable,
    stages: Vec<FormalStageProgram<'map, 'source, 'formal>>,
    guesses: Vec<FormalCausalGuess<'formal>>,
}

pub struct FormalStageProgram<'map, 'source, 'formal> {
    stage: FormalDerivativeStage<'map, 'source, 'formal>,
    equations: Vec<FormalResidualProgram<'source, 'formal>>,
}

/// Result outputs follow the owner's body order; each output is a complete
/// tensor. For a multi-body family, canonical scalar rows interleave the body
/// outputs at each domain point. Assertion predicates follow all result outputs.
pub struct FormalResidualProgram<'source, 'formal> {
    equation: FormalStageEquation<'source, 'formal>,
    program: FormalExpressionProgram<'formal>,
}

pub(crate) struct FormalCausalGuess<'formal> {
    pub(crate) target: dae::AlgebraicId<'formal>,
    pub(crate) program: FormalExpressionProgram<'formal>,
}

pub(crate) struct FormalExpressionProgram<'formal> {
    pub(crate) site: solve::SolvePureCallSite,
    pub(crate) inputs: Vec<dae::CoordinateView<'formal>>,
    pub(crate) value_outputs: usize,
    pub(crate) assertions: Vec<FormalResidualAssertion<'formal>>,
}

pub struct FormalResidualAssertion<'formal> {
    message: dae::ExprId<'formal>,
    provenance: dae::DaeProvenance,
}

impl<'map, 'source, 'formal> FormalDerivativePrograms<'map, 'source, 'formal> {
    pub fn formal(&self) -> FormalDerivativeView<'map, 'source, 'formal> {
        self.formal
    }
    pub fn table(&self) -> &solve::SolvePureCallTable {
        &self.table
    }
    pub fn stages(&self) -> &[FormalStageProgram<'map, 'source, 'formal>] {
        &self.stages
    }
    pub(crate) fn guesses(&self) -> &[FormalCausalGuess<'formal>] {
        &self.guesses
    }
}

impl<'map, 'source, 'formal> FormalStageProgram<'map, 'source, 'formal> {
    pub fn stage(&self) -> FormalDerivativeStage<'map, 'source, 'formal> {
        self.stage
    }
    pub fn equations(&self) -> &[FormalResidualProgram<'source, 'formal>] {
        &self.equations
    }
}

impl<'source, 'formal> FormalResidualProgram<'source, 'formal> {
    pub fn equation(&self) -> FormalStageEquation<'source, 'formal> {
        self.equation
    }
    pub fn site(&self) -> &solve::SolvePureCallSite {
        &self.program.site
    }
    pub fn inputs(&self) -> &[dae::CoordinateView<'formal>] {
        &self.program.inputs
    }
    pub fn residual_outputs(&self) -> usize {
        self.program.value_outputs
    }
    pub fn assertions(&self) -> &[FormalResidualAssertion<'formal>] {
        &self.program.assertions
    }
}

impl<'formal> FormalResidualAssertion<'formal> {
    pub fn message(&self) -> dae::ExprId<'formal> {
        self.message
    }
    pub fn provenance(&self) -> dae::DaeProvenance {
        self.provenance
    }
}

/// Compile complete formal equation owners using shared tensor lowering and AD.
/// This evaluates residuals at supplied points; it does not select or certify a basis.
pub fn lower_formal_derivative_stages<'map, 'source, 'formal>(
    formal: FormalDerivativeView<'map, 'source, 'formal>,
) -> Result<FormalDerivativePrograms<'map, 'source, 'formal>, solve::SolveProgramConstructionError>
{
    lower_stages(formal, true)
}

pub(crate) fn lower_state_selection_stages<'map, 'source, 'formal>(
    formal: FormalDerivativeView<'map, 'source, 'formal>,
) -> Result<FormalDerivativePrograms<'map, 'source, 'formal>, solve::SolveProgramConstructionError>
{
    lower_stages(formal, false)
}

fn lower_stages<'map, 'source, 'formal>(
    formal: FormalDerivativeView<'map, 'source, 'formal>,
    include_highest: bool,
) -> Result<FormalDerivativePrograms<'map, 'source, 'formal>, solve::SolveProgramConstructionError>
{
    let mut registry = PureCallRegistry::new();
    let stages = formal
        .stages()
        .filter(|stage| include_highest || stage.level() < 0)
        .map(|stage| {
            let equations = stage
                .equations()
                .map(|equation| registry.formal_residual(formal.view, equation))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(FormalStageProgram { stage, equations })
        })
        .collect::<Result<Vec<_>, solve::SolveProgramConstructionError>>()?;
    let guesses = if include_highest {
        Vec::new()
    } else {
        lower_guesses(formal.view, &stages, &mut registry)?
    };
    Ok(FormalDerivativePrograms {
        formal,
        table: registry.finish(),
        stages,
        guesses,
    })
}

struct ResidualBodyDomain<'formal> {
    id: dae::DomainId<'formal>,
    domain: StructuredIndexDomain,
}

#[derive(Clone, Copy)]
struct FormalExpressionBody<'formal> {
    value: dae::ExprId<'formal>,
    value_type: dae::ValueTypeId<'formal>,
}

fn lower_guesses<'formal>(
    view: dae::DaeView<'formal>,
    stages: &[FormalStageProgram<'_, '_, 'formal>],
    registry: &mut PureCallRegistry<'formal>,
) -> Result<Vec<FormalCausalGuess<'formal>>, solve::SolveProgramConstructionError> {
    let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    let mut needed = std::collections::BTreeSet::new();
    for equation in stages.iter().flat_map(|stage| stage.equations()) {
        for &coordinate in equation.inputs() {
            if let dae::CoordinateView::Algebraic(variable) = coordinate {
                needed.insert(variable.index());
            }
        }
    }
    for &target in definitions.order().iter().rev() {
        if !needed.contains(&target.index()) {
            continue;
        }
        let expression = definitions
            .definition(target)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        let at = view
            .expression(expression)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
            .provenance()
            .span();
        for (coordinate, _) in collect_model_coordinate_types(view, [expression], [], at)? {
            if let dae::CoordinateView::Algebraic(variable) = coordinate.coordinate() {
                needed.insert(variable.index());
            }
        }
    }
    definitions
        .order()
        .iter()
        .filter(|target| needed.contains(&target.index()))
        .map(|&target| {
            let expression = definitions
                .definition(target)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
            let at = view
                .expression(expression)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .provenance()
                .span();
            let body = FormalExpressionBody {
                value: expression,
                value_type: view
                    .variable(target.into())
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                    .value_type_id(),
            };
            let program = registry.formal_expressions(view, vec![body], None, at)?;
            Ok(FormalCausalGuess { target, program })
        })
        .collect()
}

fn owner_bodies<'formal>(
    view: dae::DaeView<'formal>,
    owner: dae::ContinuousOwnerView<'formal>,
) -> Result<
    (
        Vec<dae::ExprId<'formal>>,
        Option<ResidualBodyDomain<'formal>>,
        Span,
    ),
    solve::SolveProgramConstructionError,
> {
    let family = match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => {
            return Ok((
                vec![equation.residual()],
                None,
                equation.provenance().span(),
            ));
        }
        dae::ContinuousOwnerView::Structured { family, .. } => family,
    };
    let domain = match family.scalar_view() {
        ComprehensionScalarView::RowMajorProjection => None,
        projection => {
            let mut domain = view
                .domain(family.domain())
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .structured()
                .clone();
            if let ComprehensionScalarView::BinderPrefixProjection { binder_count } = projection {
                domain.binders.truncate(binder_count as usize);
            }
            Some(ResidualBodyDomain {
                id: family.domain(),
                domain,
            })
        }
    };
    Ok((
        family.bodies().iter().collect(),
        domain,
        family.provenance().span(),
    ))
}

fn residual_output_type<'formal>(
    view: dae::DaeView<'formal>,
    body: FormalExpressionBody<'formal>,
    domain: Option<&ResidualBodyDomain<'formal>>,
) -> Result<solve::SolveValueType, solve::SolveProgramConstructionError> {
    let node = view
        .expression(body.value)
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    let value_type = lower_primitive_type(view, body.value_type, arithmetic_profile())?;
    let Some(domain) = domain else {
        return Ok(value_type);
    };
    let mut shape = domain
        .domain
        .extents()
        .map_err(|_| solve::SolveProgramConstructionError::InvalidMap {
            provenance: node.provenance().span(),
        })?
        .into_iter()
        .map(|extent| {
            u32::try_from(extent).map_err(|_| {
                solve::SolveProgramConstructionError::IdentityOverflow {
                    provenance: node.provenance().span(),
                }
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    shape.extend(value_type.dimensions());
    if shape.is_empty() {
        return Ok(value_type);
    }
    solve::SolveValueType::tensor(value_type.element_type(), shape).map_err(|_| {
        solve::SolveProgramConstructionError::InvalidMap {
            provenance: node.provenance().span(),
        }
    })
}

impl<'formal> PureCallRegistry<'formal> {
    fn formal_residual<'source>(
        &mut self,
        view: dae::DaeView<'formal>,
        equation: FormalStageEquation<'source, 'formal>,
    ) -> Result<FormalResidualProgram<'source, 'formal>, solve::SolveProgramConstructionError> {
        let (bodies, domain, at) = owner_bodies(view, equation.value())?;
        let bodies = bodies
            .into_iter()
            .map(|value| {
                Ok(FormalExpressionBody {
                    value,
                    value_type: view
                        .expression(value)
                        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                        .value_type_id(),
                })
            })
            .collect::<Result<_, solve::SolveProgramConstructionError>>()?;
        let program = self.formal_expressions(view, bodies, domain, at)?;
        Ok(FormalResidualProgram { equation, program })
    }

    fn formal_expressions(
        &mut self,
        view: dae::DaeView<'formal>,
        bodies: Vec<FormalExpressionBody<'formal>>,
        domain: Option<ResidualBodyDomain<'formal>>,
        at: Span,
    ) -> Result<FormalExpressionProgram<'formal>, solve::SolveProgramConstructionError> {
        let coordinates =
            collect_model_coordinate_types(view, bodies.iter().map(|body| body.value), [], at)?;
        let (callees, predicate_ranges, assertions) =
            self.register_expression_calls(view, bodies.iter().map(|body| (body.value, ())))?;
        let predicate_count = assertions.len();
        let inputs = coordinates
            .iter()
            .map(|(_, value_type)| lower_primitive_type(view, *value_type, arithmetic_profile()))
            .collect::<Result<Vec<_>, _>>()?;
        let mut outputs = bodies
            .iter()
            .map(|&body| {
                residual_output_type(view, body, domain.as_ref())
                    .map(solve::SolvePureCallOutput::result)
            })
            .collect::<Result<Vec<_>, _>>()?;
        outputs.extend(
            std::iter::repeat_with(solve::SolvePureCallOutput::assertion_predicate)
                .take(predicate_count),
        );
        let residual_outputs = bodies.len();
        let identity = self.identities.issue(at)?;
        let owner =
            self.table
                .add_owner(identity, inputs, outputs, at, |builder, inputs, outputs| {
                    let model_coordinates = coordinates
                        .iter()
                        .zip(inputs)
                        .map(|((key, value_type), input)| {
                            builder
                                .load(*input, at)
                                .map(|value| (*key, LoweredValue::scalar(*value_type, value)))
                        })
                        .collect::<Result<HashMap<_, _>, _>>()?;
                    let mut lowerer = ExpressionLowerer {
                        view,
                        builder,
                        model_coordinates,
                        parameters: HashMap::new(),
                        function_values: HashMap::new(),
                        conditional_groups: HashMap::new(),
                        fold_parameters: HashMap::new(),
                        fold_values: HashMap::new(),
                        binders: HashMap::new(),
                        callees,
                        predicate_ranges,
                        cache: HashMap::new(),
                        call_values: HashMap::new(),
                        predicate_values: vec![None; predicate_count],
                        next_direct_assertion: 0,
                        direct_assertion_count: 0,
                    };
                    for (&body, &output) in bodies.iter().zip(outputs) {
                        let value = lowerer.residual_body(body, domain.as_ref(), at)?;
                        lowerer.builder.store(output, value, at)?;
                    }
                    for (predicate, &output) in lowerer
                        .predicate_values
                        .into_iter()
                        .zip(&outputs[residual_outputs..])
                    {
                        let value = predicate.ok_or(
                            solve::SolveProgramConstructionError::InvalidCallOutput {
                                provenance: at,
                            },
                        )?;
                        lowerer.builder.store(output, value, at)?;
                    }
                    Ok(())
                })?;
        Ok(FormalExpressionProgram {
            site: self
                .table
                .call_site(owner)
                .ok_or(solve::SolveProgramConstructionError::UnknownCallOwner { provenance: at })?,
            inputs: coordinates
                .into_iter()
                .map(|(coordinate, _)| coordinate.coordinate())
                .collect(),
            value_outputs: residual_outputs,
            assertions: assertions
                .into_iter()
                .map(|entry| FormalResidualAssertion {
                    message: entry.assertion.message,
                    provenance: entry.assertion.provenance,
                })
                .collect(),
        })
    }
}

impl<'program, 'formal> ExpressionLowerer<'_, 'program, 'formal> {
    fn residual_body(
        &mut self,
        body: FormalExpressionBody<'formal>,
        domain: Option<&ResidualBodyDomain<'formal>>,
        at: Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        if let Some(domain) = domain {
            self.mapped_expression(body.value, domain.id, domain.domain.clone(), at)
        } else {
            let value = self.expression(body.value)?;
            self.coerce_value(value, body.value_type, at)?
                .only_register(at)
        }
    }
}
