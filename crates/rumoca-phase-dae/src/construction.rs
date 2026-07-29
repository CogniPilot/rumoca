use std::collections::{HashMap, HashSet};

use rumoca_core::{
    BuiltinFunction, Causality, ClockLattice, ClockRational, Expression, Literal, OpBinary,
    OpUnary, SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, Subscript, VarName,
    Variability,
};
use rumoca_eval_flat::constant::{EvalContext, eval_expr};
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::balance::BalanceDetail;
use crate::{ToDaeError, ToDaeOptions};

mod algorithm;
mod analysis;
mod clocks;
mod equation_systems;
mod expression;
mod function_array_assembly;
mod function_body;
mod function_construction;
mod function_record_assembly;
mod function_shapes;
mod model_algorithm;
mod record_equation;
mod structured_body;
mod variable_construction;
use algorithm::{
    AlgorithmStatementContext, lower_algorithm_assignment, lower_algorithm_function_call,
    own_clocked_algorithm_targets,
};
use analysis::{
    Analysis, ComprehensionKey, ComprehensionPlan, DelayPlan, DerivedParameterPlan,
    EquationPartition, FunctionArrayAssemblyPlan, FunctionIntegerReduction, FunctionLoopLowering,
    FunctionPlan, FunctionRecordAssemblyPlan, FunctionStatementPlan, ModelAlgorithmPlan,
    PlannedRole, RecordArrayFieldPlan, RecordEquationPlan, analyze, defined_discrete_targets,
    effective_variable_scalar_type, equation_partition, primitive_scalar_type,
    structured_assignment_names,
};
use clocks::{LoweredClocks, lower_clocks, lower_sampled_value_clocks};
use equation_systems::lower_equation_systems;
use expression::{
    FunctionArrayUpdate, LoweringSymbols, all_model_expressions, derivative_reference,
    expression_children, expression_span, lower_clocked_expression, lower_coordinate_reference,
    lower_expression, lower_expression_scoped, lower_function_array_update,
    lower_function_expression, lower_function_expression_scoped, lower_model_algorithm_expression,
    planned_input_variability, require_span, variable_attribute_expressions,
};
use function_array_assembly::lower_function_array_assembly;
use function_body::{
    FunctionConditional, FunctionFold, TotalArrayDefinition, flattened_function_loop,
    function_value_coordinate, lower_function_conditional, lower_function_fold,
    lower_guarded_function_return, lower_integer_reduction, lower_total_function_array_definition,
};
use function_construction::{
    FunctionRegistry, define_functions, function_value_type, reserve_functions,
};
use function_record_assembly::lower_function_record_assembly;
use function_shapes::{
    FunctionShapeAnalysis, FunctionSpecializationKey, ShapeEnvironment, ValueShape,
    evaluate_shape_integer,
};
use model_algorithm::{
    lower_declarative_model_algorithm, lower_separated_array_sum_model_algorithm,
    lower_total_array_model_algorithm,
};
use record_equation::lower_record_equation;
use structured_body::lower_structured_body;
use variable_construction::{define_variables, reserve_variables};

#[derive(Clone, Copy)]
enum Coordinate<'dae> {
    Parameter(dae::ParameterId<'dae>),
    Input(dae::InputId<'dae>),
    State(dae::StateId<'dae>),
    Algebraic(dae::AlgebraicId<'dae>),
    DiscreteReal(dae::DiscreteRealId<'dae>),
    DiscreteValue(dae::DiscreteValueId<'dae>),
    FunctionParameter(dae::FunctionParameterId<'dae>),
    FunctionValue(dae::FunctionValueId<'dae>),
}

impl<'dae> Coordinate<'dae> {
    fn current(self) -> dae::CoordinateInput<'dae> {
        match self {
            Self::Parameter(id) => dae::CoordinateInput::Parameter(id),
            Self::Input(id) => dae::CoordinateInput::Input(id),
            Self::State(id) => dae::CoordinateInput::State(id),
            Self::Algebraic(id) => dae::CoordinateInput::Algebraic(id),
            Self::DiscreteReal(id) => dae::CoordinateInput::DiscreteReal(id),
            Self::DiscreteValue(id) => dae::CoordinateInput::DiscreteValue(id),
            Self::FunctionParameter(id) => dae::CoordinateInput::FunctionParameter(id),
            Self::FunctionValue(_) => {
                unreachable!("function values require their semantic body owner")
            }
        }
    }

    fn derivative(self, span: Span) -> Result<dae::CoordinateInput<'dae>, ToDaeError> {
        match self {
            Self::State(id) => Ok(dae::CoordinateInput::Derivative(id)),
            Self::Parameter(_)
            | Self::Input(_)
            | Self::Algebraic(_)
            | Self::DiscreteReal(_)
            | Self::DiscreteValue(_)
            | Self::FunctionParameter(_)
            | Self::FunctionValue(_) => Err(ToDaeError::unsupported_flat(
                "derivative target",
                "der(...) must name a coordinate classified as a continuous state",
                span,
            )),
        }
    }

    fn previous(self, span: Span) -> Result<dae::CoordinateInput<'dae>, ToDaeError> {
        match self {
            Self::DiscreteReal(id) => Ok(dae::CoordinateInput::PreDiscreteReal(id)),
            Self::DiscreteValue(id) => Ok(dae::CoordinateInput::PreDiscreteValue(id)),
            Self::Parameter(_)
            | Self::Input(_)
            | Self::State(_)
            | Self::Algebraic(_)
            | Self::FunctionParameter(_)
            | Self::FunctionValue(_) => Err(ToDaeError::unsupported_flat(
                "pre expression",
                "pre(...) must name a discrete coordinate in canonical DAE",
                span,
            )),
        }
    }
}

struct ReservedVariable<'flat, 'dae> {
    flat: &'flat flat::Variable,
    role: PlannedRole,
    scalar_type: dae::ScalarType,
    coordinate: Coordinate<'dae>,
    definition: dae::VariableReservation<'dae>,
}

pub(crate) fn construct(
    flat: &flat::Model,
    source_map: SourceMap,
    options: ToDaeOptions,
) -> Result<dae::Dae, ToDaeError> {
    let analysis = analyze(flat)?;
    if options.error_on_unbalanced && !flat.is_partial && !analysis.balance.is_balanced() {
        return Err(ToDaeError::unbalanced_from_detail(analysis.balance));
    }

    dae::Dae::construct(source_map, |construction| {
        build_checked(flat, &analysis, construction)
    })
    .map_err(ToDaeError::from)
}

pub(crate) fn balance_detail(flat: &flat::Model) -> Result<BalanceDetail, ToDaeError> {
    analyze(flat).map(|analysis| analysis.balance)
}

fn build_checked<'dae>(
    flat: &flat::Model,
    analysis: &Analysis,
    construction: &mut dae::DaeConstruction<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let value_types = reserve_value_types(flat, analysis, construction)?;
    let (function_ids, reserved_functions) =
        reserve_functions(flat, &analysis.function_shapes, construction)?;
    let functions = FunctionRegistry {
        flat,
        shapes: &analysis.function_shapes,
        ids: function_ids,
        comprehension_plans: &analysis.comprehension_plans,
        record_array_fields: &analysis.record_array_fields,
        constants: &analysis.constants,
        delay_plans: &analysis.delay_plans,
        reinit_state_pre: &analysis.reinit_state_pre,
    };
    let (coordinates, reserved) = reserve_variables(flat, analysis, construction, &value_types)?;
    define_functions(
        construction,
        flat,
        &coordinates,
        &functions,
        reserved_functions,
        &analysis.function_plans,
    )?;
    let assigned_discrete_targets = defined_discrete_targets(flat, &analysis.roles)
        .expect("analysis already validates discrete equation ownership");
    define_variables(
        construction,
        &coordinates,
        &functions,
        &assigned_discrete_targets,
        &analysis.derived_parameters,
        reserved,
    )?;
    let clocks = lower_clocks(construction, flat, &analysis.clock_plans)?;
    lower_sampled_value_clocks(
        construction,
        flat,
        &coordinates,
        &analysis.sampled_values,
        &clocks,
    )?;
    lower_bindings(
        construction,
        &coordinates,
        &functions,
        &analysis.roles,
        flat,
    )?;
    lower_equation_systems(construction, flat, analysis, &coordinates, &functions)?;
    lower_assertions(
        construction,
        &coordinates,
        &functions,
        &analysis.sample_lattices,
        flat.assert_equations
            .iter()
            .chain(&flat.initial_assert_equations),
    )?;
    lower_algorithms(
        construction,
        &coordinates,
        &functions,
        &analysis.sample_lattices,
        &flat.algorithms,
        &analysis.model_algorithm_plans,
    )?;
    lower_when_chains(
        construction,
        &coordinates,
        &functions,
        &analysis.sample_lattices,
        &clocks,
        &flat.when_chains,
    )
}

fn reserve_value_types<'dae>(
    flat: &flat::Model,
    analysis: &Analysis,
    construction: &mut dae::DaeConstruction<'dae>,
) -> Result<HashMap<VarName, dae::ValueTypeId<'dae>>, dae::DaeConstructionError> {
    let mut value_types = HashMap::new();
    for (name, variable) in &flat.variables {
        if matches!(analysis.roles[name], PlannedRole::Clock) {
            continue;
        }
        let provenance = dae::DaeProvenance::source(variable.source_span)?;
        let scalar = effective_variable_scalar_type(&flat.variable_type_names[name], variable)
            .expect("analysis accepts only primitive value types");
        let dimensions = variable
            .dims
            .iter()
            .map(|extent| {
                u32::try_from(*extent).map_err(|_| dae::DaeConstructionError::CapacityExceeded {
                    arena: "variable dimension",
                    attempted_index: usize::MAX,
                    span: variable.source_span,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let value_type = construction.types(|types| {
            types.intern(
                variable.type_id,
                dae::ValueType::array(scalar, dimensions),
                provenance,
            )
        })?;
        value_types.insert(name.clone(), value_type);
    }
    Ok(value_types)
}

#[derive(Clone, Copy)]
struct FunctionSymbols<'symbols, 'dae> {
    coordinates: &'symbols HashMap<VarName, Coordinate<'dae>>,
    functions: &'symbols FunctionRegistry<'symbols, 'dae>,
    shapes: &'symbols ShapeEnvironment,
}

fn lower_function_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    debug_assert_eq!(statements.len(), plans.len());
    let mut index = 0usize;
    while index < statements.len() {
        let statement = &statements[index];
        let plan = &plans[index];
        if let FunctionStatementPlan::ArrayAssembly(assembly) = plan {
            let statement_count = assembly.direct_count + 1;
            lower_function_array_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + statement_count],
                assembly,
            )?;
            index += statement_count;
            continue;
        }
        if let FunctionStatementPlan::RecordAssembly(assembly) = plan {
            lower_function_record_assembly(
                construction,
                symbols,
                &mut body,
                &statements[index..index + assembly.statement_count],
                assembly,
            )?;
            index += assembly.statement_count;
            continue;
        }
        body = lower_function_statement(construction, symbols, body, statement, plan)?;
        index += 1;
    }
    Ok(body)
}

fn lower_function_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    statement: &rumoca_core::Statement,
    plan: &FunctionStatementPlan,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    match (statement, plan) {
        (
            rumoca_core::Statement::Assignment {
                comp, value, span, ..
            },
            FunctionStatementPlan::Assignment {
                target,
                subscript_count,
            },
        ) => {
            lower_function_assignment(
                construction,
                symbols,
                &mut body,
                FunctionAssignment {
                    component: comp,
                    value,
                    span: *span,
                    target,
                    subscript_count: *subscript_count,
                },
            )?;
            Ok(body)
        }
        (
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            },
            FunctionStatementPlan::For {
                domain,
                binder_spans,
                lowering,
                statements,
                source_depth,
            },
        ) => lower_function_loop(
            construction,
            symbols,
            body,
            FunctionLoop {
                indices,
                source_statements: equations,
                span: *span,
                domain,
                binder_spans,
                lowering,
                plans: statements,
                source_depth: *source_depth,
            },
        ),
        (
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            },
            FunctionStatementPlan::If {
                branches,
                fallback,
                targets,
            },
        ) => {
            lower_function_conditional(
                construction,
                &mut body,
                FunctionConditional {
                    symbols,
                    blocks: cond_blocks,
                    fallback: else_block.as_deref(),
                    branch_plans: branches,
                    fallback_plans: fallback.as_deref(),
                    targets,
                    span: *span,
                },
            )?;
            Ok(body)
        }
        (_, FunctionStatementPlan::ArrayAssemblyMember) => {
            unreachable!("array assembly members are consumed by their leading owner")
        }
        (_, FunctionStatementPlan::RecordAssemblyMember) => {
            unreachable!("record assembly members are consumed by their leading owner")
        }
        _ => unreachable!("function analysis and construction plans remain aligned"),
    }
}

struct FunctionAssignment<'statement> {
    component: &'statement rumoca_core::ComponentReference,
    value: &'statement Expression,
    span: Span,
    target: &'statement VarName,
    subscript_count: usize,
}

fn lower_function_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    assignment: FunctionAssignment<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let target = function_value_coordinate(symbols.coordinates, assignment.target);
    let mut value = lower_function_expression(
        construction,
        symbols.coordinates,
        symbols.functions,
        symbols.shapes,
        body,
        assignment.value,
    )?;
    let provenance = dae::DaeProvenance::source(assignment.span)?;
    let subscripts = &assignment
        .component
        .parts
        .last()
        .expect("function assignment target was validated")
        .subs;
    debug_assert_eq!(assignment.subscript_count, subscripts.len());
    if !subscripts.is_empty() {
        let binders = HashMap::new();
        value = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(body),
                    values: None,
                    owner_clock: None,
                },
                binders: &binders,
                target,
                subscripts,
                value,
                provenance,
            },
        )?;
    }
    construction.functions(|owner| owner.assign(body, target, value, provenance))
}

struct FunctionLoop<'statement> {
    indices: &'statement [rumoca_core::ForIndex],
    source_statements: &'statement [rumoca_core::Statement],
    span: Span,
    domain: &'statement StructuredIndexDomain,
    binder_spans: &'statement [Span],
    lowering: &'statement FunctionLoopLowering,
    plans: &'statement [FunctionStatementPlan],
    source_depth: usize,
}

fn lower_function_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: FunctionLoop<'_>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let owner = dae::DaeProvenance::source(input.span)?;
    let domain_provenance = match input.binder_spans {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let domain = construction
        .domains(|domains| domains.structured(input.domain.clone(), domain_provenance))?;
    let (indices, statements) =
        flattened_function_loop(input.indices, input.source_statements, input.source_depth);
    let binders = lower_function_binders(construction, domain, &indices, input.binder_spans)?;
    let mut loop_shapes = symbols.shapes.clone();
    loop_shapes.extend(binders.keys().cloned().map(|binder| (binder, Vec::new())));
    let loop_symbols = FunctionSymbols {
        coordinates: symbols.coordinates,
        functions: symbols.functions,
        shapes: &loop_shapes,
    };
    match input.lowering {
        FunctionLoopLowering::TotalArrayDefinition { target } => {
            lower_total_function_array_definition(
                construction,
                &mut body,
                TotalArrayDefinition {
                    symbols: loop_symbols,
                    domain,
                    binders: &binders,
                    statements,
                    plans: input.plans,
                    target,
                    owner,
                },
            )?;
            Ok(body)
        }
        FunctionLoopLowering::Fold { targets } => lower_function_fold(
            construction,
            loop_symbols,
            body,
            FunctionFold {
                domain,
                binders: &binders,
                statements,
                plans: input.plans,
                targets,
                owner,
            },
        ),
    }
}

fn lower_function_binders<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    domain: dae::DomainId<'dae>,
    indices: &[&rumoca_core::ForIndex],
    spans: &[Span],
) -> Result<HashMap<VarName, dae::DomainBinderId<'dae>>, dae::DaeConstructionError> {
    let mut binders = HashMap::with_capacity(indices.len());
    for (ordinal, (index, span)) in indices.iter().zip(spans).enumerate() {
        let provenance = dae::DaeProvenance::source(*span)?;
        let binder = construction.domains(|domains| domains.binder(domain, ordinal, provenance))?;
        binders.insert(VarName::new(&index.ident), binder);
    }
    Ok(binders)
}

fn lower_optional_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    expression: Option<&Expression>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    expression
        .map(|expression| lower_expression(construction, coordinates, functions, expression, None))
        .transpose()
}

fn lower_optional_attribute_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    expression: Option<&Expression>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    expression
        .map(|expression| {
            lower_attribute_expression(construction, coordinates, functions, expression)
        })
        .transpose()
}

fn lower_attribute_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    expression: &Expression,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates,
            functions,
            shapes: functions.shapes.model_values(),
            function_body: None,
            values: None,
            owner_clock: None,
        },
        &HashMap::new(),
        expression,
        None,
    )
}

fn lower_bindings<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    roles: &HashMap<VarName, PlannedRole>,
    flat: &flat::Model,
) -> Result<(), dae::DaeConstructionError> {
    for (name, variable) in &flat.variables {
        let Some(binding) = &variable.binding else {
            continue;
        };
        if matches!(roles[name], PlannedRole::Clock) {
            continue;
        }
        let coordinate = coordinates[name];
        if matches!(coordinate, Coordinate::Parameter(_) | Coordinate::Input(_)) {
            continue;
        }
        let owner_span = binding.span().unwrap_or(variable.source_span);
        let owner = dae::DaeProvenance::generated(dae::DaeGeneration::BindingEquation, owner_span)?;
        let rhs = lower_expression(construction, coordinates, functions, binding, None)?;
        match coordinate {
            Coordinate::DiscreteValue(target) => {
                construction.discrete(|discrete| discrete.assignment(owner, target, rhs))?;
            }
            Coordinate::Parameter(_)
            | Coordinate::Input(_)
            | Coordinate::FunctionParameter(_)
            | Coordinate::FunctionValue(_) => {
                unreachable!("non-equation binding coordinates were filtered before lowering")
            }
            coordinate => {
                lower_residual_binding(construction, coordinate, owner, rhs)?;
            }
        }
    }
    Ok(())
}

fn lower_residual_binding<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinate: Coordinate<'dae>,
    owner: dae::DaeProvenance,
    rhs: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let lhs = construction
        .expressions(|expressions| expressions.at(owner).coordinate(coordinate.current()))?;
    let residual = generated_residual(construction, owner, lhs, rhs)?;
    match coordinate {
        Coordinate::DiscreteReal(_) => {
            construction.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(residual))
            })?;
            Ok(())
        }
        Coordinate::State(_) | Coordinate::Algebraic(_) => {
            construction.continuous(|continuous| continuous.value_equation(owner, residual))
        }
        Coordinate::Parameter(_)
        | Coordinate::Input(_)
        | Coordinate::DiscreteValue(_)
        | Coordinate::FunctionParameter(_)
        | Coordinate::FunctionValue(_) => {
            unreachable!("caller passes only residual-defined coordinates")
        }
    }
}

fn generated_residual<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner: dae::DaeProvenance,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::SyntheticResidual, owner.span())?;
    construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, lhs, rhs)
    })
}

fn lower_assertions<'dae, 'flat>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    assertions: impl IntoIterator<Item = &'flat flat::AssertEquation>,
) -> Result<(), dae::DaeConstructionError> {
    for assertion in assertions {
        let (condition, _) = lower_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            &assertion.condition,
        )?;
        let action_guard = negate_condition(construction, condition, assertion.span)?;
        let message = lower_expression(
            construction,
            coordinates,
            functions,
            &assertion.message,
            None,
        )?;
        let level = lower_optional_expression(
            construction,
            coordinates,
            functions,
            assertion.level.as_ref(),
        )?;
        let provenance = dae::DaeProvenance::source(assertion.span)?;
        construction.events(|events| {
            events.assert_with_level(action_guard, action_guard, message, level, provenance)
        })?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
struct EventGuard<'dae> {
    trigger: dae::ConditionId<'dae>,
    condition: dae::ConditionId<'dae>,
    owner_clock: Option<dae::ClockId<'dae>>,
}

fn lower_algorithms<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    algorithms: &[flat::Algorithm],
    plans: &[ModelAlgorithmPlan],
) -> Result<(), dae::DaeConstructionError> {
    debug_assert_eq!(algorithms.len(), plans.len());
    for (algorithm, plan) in algorithms.iter().zip(plans) {
        match plan {
            ModelAlgorithmPlan::Declarative { target } => {
                lower_declarative_model_algorithm(
                    construction,
                    coordinates,
                    functions,
                    algorithm,
                    target,
                )?;
            }
            ModelAlgorithmPlan::TotalArrayDefinition {
                target,
                domain,
                binder_spans,
            } => {
                lower_total_array_model_algorithm(
                    construction,
                    coordinates,
                    functions,
                    algorithm,
                    target,
                    domain,
                    binder_spans,
                )?;
            }
            ModelAlgorithmPlan::SeparatedArraySum {
                array_target,
                scalar_target,
                domain,
                binder_spans,
            } => {
                lower_separated_array_sum_model_algorithm(
                    construction,
                    coordinates,
                    functions,
                    algorithm,
                    array_target,
                    scalar_target,
                    domain,
                    binder_spans,
                )?;
            }
            ModelAlgorithmPlan::Event => {
                lower_algorithm_statements(
                    construction,
                    coordinates,
                    functions,
                    sample_lattices,
                    None,
                    &algorithm.statements,
                    algorithm.span,
                )?;
            }
        }
    }
    Ok(())
}

// SPEC_0021 exception: the structured-assignment arm performs one bounded
// leaf expansion while retaining the surrounding statement grammar.
#[allow(clippy::excessive_nesting)]
fn lower_algorithm_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    parent: Option<EventGuard<'dae>>,
    statements: &[rumoca_core::Statement],
    owner_span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let context = AlgorithmStatementContext {
        coordinates,
        functions,
        parent,
        owner_span,
    };
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                lower_algorithm_assignment(construction, context, comp, value, *span)?;
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => {
                lower_algorithm_if(
                    construction,
                    coordinates,
                    functions,
                    sample_lattices,
                    parent,
                    cond_blocks,
                    else_block.as_deref().unwrap_or_default(),
                    *span,
                )?;
            }
            rumoca_core::Statement::When { blocks, span } => {
                lower_algorithm_when(
                    construction,
                    coordinates,
                    functions,
                    sample_lattices,
                    parent,
                    blocks,
                    *span,
                )?;
            }
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => {
                lower_algorithm_function_call(construction, context, comp, args, outputs, *span)?;
            }
            _ => unreachable!("algorithm analysis restricts the checked statement grammar"),
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn lower_algorithm_if<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    parent: Option<EventGuard<'dae>>,
    blocks: &[rumoca_core::StatementBlock],
    else_block: &[rumoca_core::Statement],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let mut previous = None;
    for block in blocks {
        let (condition, owner_clock) = lower_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            &block.cond,
        )?;
        let available = match previous {
            Some(previous) => {
                let not_previous = negate_condition(construction, previous, span)?;
                combine_conditions(construction, condition, not_previous, false, span)?
            }
            None => condition,
        };
        let guard = match parent {
            Some(parent) => EventGuard {
                trigger: parent.trigger,
                condition: combine_conditions(
                    construction,
                    parent.condition,
                    available,
                    false,
                    span,
                )?,
                owner_clock: parent.owner_clock.or(owner_clock),
            },
            None => EventGuard {
                trigger: available,
                condition: available,
                owner_clock,
            },
        };
        if let Some(clock) = guard.owner_clock {
            own_clocked_algorithm_targets(construction, coordinates, clock, &block.stmts)?;
        }
        lower_algorithm_statements(
            construction,
            coordinates,
            functions,
            sample_lattices,
            Some(guard),
            &block.stmts,
            span,
        )?;
        previous = Some(match previous {
            Some(previous) => combine_conditions(construction, previous, condition, true, span)?,
            None => condition,
        });
    }
    if !else_block.is_empty() {
        let available = match previous {
            Some(previous) => negate_condition(construction, previous, span)?,
            None => always_condition(construction, span)?,
        };
        let guard = match parent {
            Some(parent) => EventGuard {
                trigger: parent.trigger,
                condition: combine_conditions(
                    construction,
                    parent.condition,
                    available,
                    false,
                    span,
                )?,
                owner_clock: parent.owner_clock,
            },
            None => EventGuard {
                trigger: available,
                condition: available,
                owner_clock: None,
            },
        };
        lower_algorithm_statements(
            construction,
            coordinates,
            functions,
            sample_lattices,
            Some(guard),
            else_block,
            span,
        )?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn lower_algorithm_when<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    parent: Option<EventGuard<'dae>>,
    blocks: &[rumoca_core::StatementBlock],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let mut previous = None;
    let mut guarded_blocks = Vec::with_capacity(blocks.len());
    for block in blocks {
        let (condition, owner_clock) = lower_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            &block.cond,
        )?;
        let available = match previous {
            Some(previous) => {
                let not_previous = negate_condition(construction, previous, span)?;
                combine_conditions(construction, condition, not_previous, false, span)?
            }
            None => condition,
        };
        let guard = match parent {
            Some(parent) => EventGuard {
                trigger: available,
                condition: combine_conditions(
                    construction,
                    parent.condition,
                    available,
                    false,
                    span,
                )?,
                owner_clock: parent.owner_clock.or(owner_clock),
            },
            None => EventGuard {
                trigger: available,
                condition: available,
                owner_clock,
            },
        };
        guarded_blocks.push((block, guard));
        previous = Some(match previous {
            Some(previous) => combine_conditions(construction, previous, condition, true, span)?,
            None => condition,
        });
    }
    for (block, guard) in &guarded_blocks {
        if let Some(clock) = guard.owner_clock {
            own_clocked_algorithm_targets(construction, coordinates, clock, &block.stmts)?;
        }
    }
    for (block, guard) in guarded_blocks {
        lower_algorithm_statements(
            construction,
            coordinates,
            functions,
            sample_lattices,
            Some(guard),
            &block.stmts,
            span,
        )?;
    }
    Ok(())
}

fn always_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner_span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, owner_span)?;
    let expression = construction.expressions(|expressions| {
        expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Boolean(true))
    })?;
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(
            condition,
            dae::ConditionInput::Discrete(expression),
            provenance,
        )
    })?;
    Ok(condition)
}

fn lower_when_chains<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    clocks: &LoweredClocks<'dae>,
    chains: &[flat::WhenChain],
) -> Result<(), dae::DaeConstructionError> {
    for chain in chains {
        lower_when_chain(
            construction,
            coordinates,
            functions,
            sample_lattices,
            clocks,
            chain,
        )?;
    }
    Ok(())
}

fn lower_when_chain<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    clocks: &LoweredClocks<'dae>,
    chain: &flat::WhenChain,
) -> Result<(), dae::DaeConstructionError> {
    let mut guards = Vec::with_capacity(chain.branches.len());
    for branch in &chain.branches {
        let (condition, owner_clock) = lower_when_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            clocks,
            &branch.condition,
        )?;
        guards.push(EventGuard {
            trigger: condition,
            condition,
            owner_clock,
        });
    }
    for (branch, guard) in chain.branches.iter().zip(&guards) {
        if let Some(clock) = guard.owner_clock {
            own_clocked_targets(construction, coordinates, clock, &branch.equations)?;
        }
    }
    for (branch, guard) in chain.branches.iter().zip(guards) {
        lower_when_equations(
            construction,
            coordinates,
            functions,
            sample_lattices,
            guard,
            &branch.equations,
        )?;
    }
    Ok(())
}

fn own_clocked_targets<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    clock: dae::ClockId<'dae>,
    equations: &[flat::WhenEquation],
) -> Result<(), dae::DaeConstructionError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, .. } => {
                let provenance = dae::DaeProvenance::source(equation.span())?;
                construction.clocks(|clocks| match coordinates[target] {
                    Coordinate::DiscreteReal(variable) => {
                        clocks.own_discrete_real(clock, variable, provenance)?;
                        Ok(())
                    }
                    Coordinate::DiscreteValue(variable) => {
                        clocks.own_discrete_value(clock, variable, provenance)?;
                        Ok(())
                    }
                    _ => unreachable!("clock analysis accepts only discrete clocked targets"),
                })?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, branch) in branches {
                    own_clocked_targets(construction, coordinates, clock, branch)?;
                }
                own_clocked_targets(construction, coordinates, clock, else_branch)?;
            }
            flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. } => {}
            flat::WhenEquation::FunctionCallOutputs { .. } => {
                unreachable!("analysis rejects unchecked event function calls")
            }
        }
    }
    Ok(())
}

fn lower_when_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    clocks: &LoweredClocks<'dae>,
    expression: &Expression,
) -> Result<(dae::ConditionId<'dae>, Option<dae::ClockId<'dae>>), dae::DaeConstructionError> {
    let Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    else {
        return lower_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            expression,
        );
    };
    let Some(clock) = clocks.by_variable.get(name.var_name()).copied() else {
        return lower_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            expression,
        );
    };
    debug_assert!(
        subscripts.is_empty(),
        "clock analysis accepts scalar aliases"
    );
    let provenance = dae::DaeProvenance::source(*span)?;
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(condition, dae::ConditionInput::Clock(clock), provenance)
    })?;
    Ok((condition, Some(clock)))
}

fn lower_when_equations<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    guard: EventGuard<'dae>,
    equations: &[flat::WhenEquation],
) -> Result<(), dae::DaeConstructionError> {
    for equation in equations {
        let provenance = dae::DaeProvenance::source(equation.span())?;
        match equation {
            flat::WhenEquation::Assign { target, value, .. } => {
                let value = match guard.owner_clock {
                    Some(clock) => lower_clocked_expression(
                        construction,
                        coordinates,
                        functions,
                        clock,
                        value,
                    )?,
                    None => lower_expression(construction, coordinates, functions, value, None)?,
                };
                lower_when_assignment(construction, coordinates[target], guard, value, provenance)?;
            }
            flat::WhenEquation::Reinit { state, value, .. } => {
                let Coordinate::State(state) = coordinates[state] else {
                    unreachable!("analysis accepts only state reinitialization")
                };
                let value = lower_expression(construction, coordinates, functions, value, None)?;
                construction.events(|events| {
                    events.reinitialize(guard.trigger, guard.condition, state, value, provenance)
                })?;
            }
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                let (condition, _) = lower_condition(
                    construction,
                    coordinates,
                    functions,
                    sample_lattices,
                    condition,
                )?;
                let failed = negate_condition(construction, condition, equation.span())?;
                let action_guard = combine_conditions(
                    construction,
                    guard.condition,
                    failed,
                    false,
                    equation.span(),
                )?;
                let message =
                    lower_expression(construction, coordinates, functions, message, None)?;
                construction.events(|events| {
                    events.assert(guard.trigger, action_guard, message, provenance)
                })?;
            }
            flat::WhenEquation::Terminate { message, .. } => {
                let message =
                    lower_expression(construction, coordinates, functions, message, None)?;
                construction.events(|events| {
                    events.terminate(guard.trigger, guard.condition, message, provenance)
                })?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                lower_conditional_when(
                    construction,
                    LoweringSymbols {
                        coordinates,
                        functions,
                        shapes: functions.shapes.model_values(),
                        function_body: None,
                        values: None,
                        owner_clock: guard.owner_clock,
                    },
                    sample_lattices,
                    guard,
                    branches,
                    else_branch,
                    equation.span(),
                )?;
            }
            flat::WhenEquation::FunctionCallOutputs { .. } => {
                unreachable!("analysis rejects unchecked event function calls")
            }
        }
    }
    Ok(())
}

fn lower_when_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    target: Coordinate<'dae>,
    guard: EventGuard<'dae>,
    value: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    match target {
        Coordinate::DiscreteReal(target) => construction
            .events(|events| {
                events.assign_discrete_real(
                    guard.trigger,
                    guard.condition,
                    target,
                    value,
                    provenance,
                )
            })
            .map(|_| ()),
        Coordinate::DiscreteValue(target) => construction
            .events(|events| {
                events.assign_discrete_value(
                    guard.trigger,
                    guard.condition,
                    target,
                    value,
                    provenance,
                )
            })
            .map(|_| ()),
        _ => unreachable!("analysis accepts only discrete when targets"),
    }
}

fn lower_conditional_when<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    parent: EventGuard<'dae>,
    branches: &[(Expression, Vec<flat::WhenEquation>)],
    else_branch: &[flat::WhenEquation],
    span: Span,
) -> Result<(), dae::DaeConstructionError> {
    let mut previous = None;
    for (condition, equations) in branches {
        let (condition, _) = lower_condition(
            construction,
            symbols.coordinates,
            symbols.functions,
            sample_lattices,
            condition,
        )?;
        let available = match previous {
            Some(previous) => {
                let not_previous = negate_condition(construction, previous, span)?;
                combine_conditions(construction, condition, not_previous, false, span)?
            }
            None => condition,
        };
        let guard_condition =
            combine_conditions(construction, parent.condition, available, false, span)?;
        let guard = EventGuard {
            trigger: parent.trigger,
            condition: guard_condition,
            owner_clock: parent.owner_clock,
        };
        lower_when_equations(
            construction,
            symbols.coordinates,
            symbols.functions,
            sample_lattices,
            guard,
            equations,
        )?;
        previous = Some(match previous {
            Some(previous) => combine_conditions(construction, previous, condition, true, span)?,
            None => condition,
        });
    }
    if !else_branch.is_empty() {
        let guard = match previous {
            Some(previous) => {
                let available = negate_condition(construction, previous, span)?;
                combine_conditions(construction, parent.condition, available, false, span)?
            }
            None => parent.condition,
        };
        let guard = EventGuard {
            trigger: parent.trigger,
            condition: guard,
            owner_clock: parent.owner_clock,
        };
        lower_when_equations(
            construction,
            symbols.coordinates,
            symbols.functions,
            sample_lattices,
            guard,
            else_branch,
        )?;
    }
    Ok(())
}

fn lower_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    expression: &Expression,
) -> Result<(dae::ConditionId<'dae>, Option<dae::ClockId<'dae>>), dae::DaeConstructionError> {
    let (condition, relations, owner_clock) = lower_condition_tree(
        construction,
        coordinates,
        functions,
        sample_lattices,
        expression,
    )?;
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::ConditionLowering,
        expression
            .span()
            .expect("analysis proves condition provenance"),
    )?;
    for relation in relations {
        construction.conditions(|conditions| conditions.root(relation, condition, provenance))?;
    }
    Ok((condition, owner_clock))
}

type LoweredCondition<'dae> = (
    dae::ConditionId<'dae>,
    Vec<dae::RelationId<'dae>>,
    Option<dae::ClockId<'dae>>,
);
type LoweredConditionNode<'dae> = (
    dae::ConditionInput<'dae>,
    Vec<dae::RelationId<'dae>>,
    Option<dae::ClockId<'dae>>,
);

fn lower_condition_tree<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    expression: &Expression,
) -> Result<LoweredCondition<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(
        expression
            .span()
            .expect("analysis proves condition provenance"),
    )?;
    let (input, relations, owner_clock) = match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            ..
        } => (dae::ConditionInput::Initial, Vec::new(), None),
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => {
            let (condition, relations, _) =
                lower_condition_tree(construction, coordinates, functions, sample_lattices, rhs)?;
            (dae::ConditionInput::Not(condition), relations, None)
        }
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        }
        | Expression::Binary {
            op: OpBinary::Or,
            lhs,
            rhs,
            ..
        } => lower_binary_condition(
            construction,
            coordinates,
            functions,
            sample_lattices,
            (lhs, rhs),
            matches!(
                expression,
                Expression::Binary {
                    op: OpBinary::Or,
                    ..
                }
            ),
            provenance,
        )?,
        Expression::Array { elements, .. } => {
            return lower_vector_condition(
                construction,
                coordinates,
                functions,
                sample_lattices,
                elements,
                provenance.span(),
            );
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            ..
        } => lower_sample_condition(construction, sample_lattices, provenance)?,
        Expression::BuiltinCall {
            function: BuiltinFunction::Change,
            args,
            ..
        } => {
            let expression = lower_change_expression(
                construction,
                coordinates,
                functions,
                args,
                provenance.span(),
            )?;
            (dae::ConditionInput::Discrete(expression), Vec::new(), None)
        }
        Expression::Binary {
            op:
                OpBinary::Eq | OpBinary::Neq | OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge,
            ..
        } => {
            let expression =
                lower_expression(construction, coordinates, functions, expression, None)?;
            let relation = construction
                .conditions(|conditions| conditions.relation(expression, provenance))?;
            (
                dae::ConditionInput::Relation(relation),
                vec![relation],
                None,
            )
        }
        _ => {
            let expression =
                lower_expression(construction, coordinates, functions, expression, None)?;
            (dae::ConditionInput::Discrete(expression), Vec::new(), None)
        }
    };
    let condition = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| conditions.define(condition, input, provenance))?;
    Ok((condition, relations, owner_clock))
}

fn lower_sample_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let lattice = *sample_lattices
        .iter()
        .find_map(|(span, lattice)| (*span == provenance.span()).then_some(lattice))
        .expect("analysis proves every sample condition has an exact clock lattice");
    let clock = construction.clocks(|clocks| clocks.periodic(lattice, provenance))?;
    Ok((dae::ConditionInput::Clock(clock), Vec::new(), Some(clock)))
}

fn lower_binary_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    operands: (&Expression, &Expression),
    disjunction: bool,
    provenance: dae::DaeProvenance,
) -> Result<LoweredConditionNode<'dae>, dae::DaeConstructionError> {
    let (lhs, rhs) = operands;
    let (lhs, mut relations, lhs_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, lhs)?;
    let (rhs, rhs_relations, rhs_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, rhs)?;
    relations.extend(rhs_relations);
    let input = if disjunction {
        dae::ConditionInput::Or(lhs, rhs)
    } else {
        dae::ConditionInput::And(lhs, rhs)
    };
    let owner_clock = merge_condition_clock(lhs_clock, rhs_clock, disjunction, provenance)?;
    Ok((input, relations, owner_clock))
}

fn lower_vector_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    sample_lattices: &[(Span, ClockLattice)],
    elements: &[Expression],
    span: Span,
) -> Result<
    (
        dae::ConditionId<'dae>,
        Vec<dae::RelationId<'dae>>,
        Option<dae::ClockId<'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let Some(first) = elements.first() else {
        let expression = construction.expressions(|expressions| {
            expressions
                .at(generated)
                .literal(dae::DaeLiteral::Boolean(false))
        })?;
        let condition = construction.conditions(|conditions| conditions.reserve(generated))?;
        construction.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Discrete(expression),
                generated,
            )
        })?;
        return Ok((condition, Vec::new(), None));
    };
    let (mut condition, mut relations, mut owner_clock) =
        lower_condition_tree(construction, coordinates, functions, sample_lattices, first)?;
    for element in &elements[1..] {
        let (rhs, rhs_relations, rhs_clock) = lower_condition_tree(
            construction,
            coordinates,
            functions,
            sample_lattices,
            element,
        )?;
        condition = combine_conditions(construction, condition, rhs, true, span)?;
        relations.extend(rhs_relations);
        owner_clock = merge_condition_clock(owner_clock, rhs_clock, true, generated)?;
    }
    Ok((condition, relations, owner_clock))
}

fn lower_change_expression<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    arguments: &[Expression],
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (name, subscripts) =
        derivative_reference(&arguments[0]).expect("algorithm analysis proves change target");
    let current_provenance = dae::DaeProvenance::source(
        arguments[0]
            .span()
            .expect("algorithm analysis proves change operand provenance"),
    )?;
    let generated = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    let current = lower_coordinate_reference(
        construction,
        symbols,
        &HashMap::new(),
        coordinates[name.var_name()].current(),
        subscripts,
        current_provenance,
    )?;
    let previous_coordinate = coordinates[name.var_name()]
        .previous(span)
        .expect("algorithm analysis proves a discrete change target");
    let previous = lower_coordinate_reference(
        construction,
        symbols,
        &HashMap::new(),
        previous_coordinate,
        subscripts,
        generated,
    )?;
    construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::NotEqual, current, previous)
    })
}

fn merge_condition_clock<'dae>(
    lhs: Option<dae::ClockId<'dae>>,
    rhs: Option<dae::ClockId<'dae>>,
    disjunction: bool,
    provenance: dae::DaeProvenance,
) -> Result<Option<dae::ClockId<'dae>>, dae::DaeConstructionError> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs != rhs => Err(dae::DaeConstructionError::DuplicateKey {
            kind: "condition clock owner",
            key: format!("{} and {}", lhs.index(), rhs.index()),
            span: provenance.span(),
        }),
        (Some(clock), Some(_)) => Ok(Some(clock)),
        (Some(clock), None) | (None, Some(clock)) if !disjunction => Ok(Some(clock)),
        _ => Ok(None),
    }
}

fn negate_condition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    condition: dae::ConditionId<'dae>,
    span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let negated = construction.conditions(|conditions| conditions.reserve(provenance))?;
    construction.conditions(|conditions| {
        conditions.define(negated, dae::ConditionInput::Not(condition), provenance)
    })?;
    Ok(negated)
}

fn combine_conditions<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    lhs: dae::ConditionId<'dae>,
    rhs: dae::ConditionId<'dae>,
    disjunction: bool,
    span: Span,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(dae::DaeGeneration::ConditionLowering, span)?;
    let combined = construction.conditions(|conditions| conditions.reserve(provenance))?;
    let input = if disjunction {
        dae::ConditionInput::Or(lhs, rhs)
    } else {
        dae::ConditionInput::And(lhs, rhs)
    };
    construction.conditions(|conditions| conditions.define(combined, input, provenance))?;
    Ok(combined)
}

fn lower_structured_equations<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equations: &[flat::Equation],
    families: &[flat::StructuredEquationFamily],
    excluded_families: &HashSet<usize>,
    initialization: bool,
) -> Result<(), dae::DaeConstructionError> {
    for (family_index, family) in families.iter().enumerate() {
        if excluded_families.contains(&family_index) {
            continue;
        }
        let owner = equation_owner_provenance(&family.origin, family.span)?;
        let generated_root = equation_generation(&family.origin);
        let domain =
            construction.domains(|domains| domains.structured(family.domain.clone(), owner))?;
        let bodies = if let Some(template) = &family.template {
            let mut binders = HashMap::with_capacity(family.domain.binders.len());
            for (ordinal, binder) in family.domain.binders.iter().enumerate() {
                let id = construction.domains(|domains| domains.binder(domain, ordinal, owner))?;
                binders.insert(VarName::new(&binder.display_name), id);
            }
            template
                .body
                .iter()
                .map(|body| {
                    let symbols = LoweringSymbols {
                        coordinates,
                        functions,
                        shapes: functions.shapes.model_values(),
                        function_body: None,
                        values: None,
                        owner_clock: None,
                    };
                    lower_structured_body(
                        construction,
                        symbols,
                        &binders,
                        body,
                        generated_root,
                        owner.span(),
                    )
                })
                .collect::<Result<Vec<_>, _>>()?
        } else {
            lower_materialized_family_bodies(
                construction,
                coordinates,
                functions,
                equations,
                family,
                owner,
            )?
        };
        let scalar_view = family
            .template
            .as_ref()
            .map(|template| template.scalar_view)
            .unwrap_or(rumoca_core::ComprehensionScalarView::RowMajorProjection);
        insert_structured_family(
            construction,
            initialization,
            owner,
            domain,
            scalar_view,
            bodies,
        )?;
    }
    Ok(())
}

fn insert_structured_family<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    initialization: bool,
    owner: dae::DaeProvenance,
    domain: dae::DomainId<'dae>,
    scalar_view: rumoca_core::ComprehensionScalarView,
    bodies: Vec<dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    if initialization {
        construction.initialization(|system| {
            system.structured_family(owner, domain, scalar_view, |residuals| {
                insert_family_bodies(residuals, bodies)
            })?;
            Ok(())
        })
    } else {
        construction.continuous(|system| {
            system.structured_family(owner, domain, scalar_view, |residuals| {
                insert_family_bodies(residuals, bodies)
            })?;
            Ok(())
        })
    }
}

fn insert_family_bodies<'dae>(
    residuals: &mut dae::StructuredResiduals<'_, 'dae>,
    bodies: Vec<dae::ExprId<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    for body in bodies {
        residuals.body(body)?;
    }
    Ok(())
}

fn lower_materialized_family_bodies<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equations: &[flat::Equation],
    family: &flat::StructuredEquationFamily,
    owner: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let domain_count = family
        .domain
        .scalar_count()
        .expect("analysis validates the structured domain");
    let extents = family
        .domain
        .extents()
        .expect("analysis validates the structured domain");
    let mut bodies = Vec::with_capacity(family.equations_per_point);
    for body_ordinal in 0..family.equations_per_point {
        let mut scalar_bodies = Vec::with_capacity(domain_count);
        for point in 0..domain_count {
            let offset = point
                .checked_mul(family.equations_per_point)
                .and_then(|offset| offset.checked_add(body_ordinal))
                .expect("analysis validates the materialized family row range");
            let equation = &equations[family.first_equation_index + offset];
            let symbols = LoweringSymbols {
                coordinates,
                functions,
                shapes: functions.shapes.model_values(),
                function_body: None,
                values: None,
                owner_clock: None,
            };
            scalar_bodies.push(lower_structured_body(
                construction,
                symbols,
                &HashMap::new(),
                &equation.residual,
                equation_generation(&equation.origin),
                equation.span,
            )?);
        }
        let provenance = dae::DaeProvenance::generated(
            dae::DaeGeneration::ArrayEquationProjection,
            owner.span(),
        )?;
        bodies.push(pack_row_major_body(
            construction,
            &scalar_bodies,
            &extents,
            provenance,
        )?);
    }
    Ok(bodies)
}

pub(super) fn pack_row_major_body<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    scalars: &[dae::ExprId<'dae>],
    extents: &[usize],
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let Some((&outer, inner_extents)) = extents.split_first() else {
        return Ok(scalars[0]);
    };
    let inner_count = inner_extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent))
        .expect("analysis validates the structured domain cardinality");
    let mut elements = Vec::with_capacity(outer);
    for chunk in scalars.chunks_exact(inner_count) {
        elements.push(pack_row_major_body(
            construction,
            chunk,
            inner_extents,
            provenance,
        )?);
    }
    construction.expressions(|expressions| expressions.at(provenance).array(elements))
}

struct EquationRows<'scope> {
    equations: &'scope [flat::Equation],
    excluded: &'scope HashSet<usize>,
    records: &'scope HashMap<usize, RecordEquationPlan>,
    roles: &'scope HashMap<VarName, PlannedRole>,
    initialization: bool,
}

fn lower_equations<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    input: EquationRows<'_>,
) -> Result<(), dae::DaeConstructionError> {
    for (index, equation) in input.equations.iter().enumerate() {
        if input.excluded.contains(&index) {
            continue;
        }
        let owner = equation_owner_provenance(&equation.origin, equation.span)?;
        let generation = equation_generation(&equation.origin);
        if let Some(plan) = input.records.get(&index) {
            lower_record_equation(
                construction,
                coordinates,
                functions,
                equation,
                plan,
                owner,
                input.initialization,
            )?;
            continue;
        }
        if input.initialization {
            let residual = lower_expression(
                construction,
                coordinates,
                functions,
                &equation.residual,
                generation,
            )?;
            construction.initialization(|system| system.value_equation(owner, residual))?;
            continue;
        }
        match equation_partition(equation, input.roles)
            .expect("analysis already validates equation ownership")
        {
            EquationPartition::Continuous => {
                let residual = lower_expression(
                    construction,
                    coordinates,
                    functions,
                    &equation.residual,
                    generation,
                )?;
                construction.continuous(|system| system.value_equation(owner, residual))?;
            }
            EquationPartition::DiscreteReal { .. } => {
                let residual = lower_expression(
                    construction,
                    coordinates,
                    functions,
                    &equation.residual,
                    generation,
                )?;
                construction.discrete(|system| {
                    system.real_equation(owner, |equation| equation.residual(residual))
                })?;
            }
            EquationPartition::DiscreteValue(plan) => {
                let value = lower_expression(
                    construction,
                    coordinates,
                    functions,
                    plan.value.as_ref(),
                    if plan.generated {
                        Some(dae::DaeGeneration::DiscreteUpdate)
                    } else {
                        generation
                    },
                )?;
                let Coordinate::DiscreteValue(target) = coordinates[plan.target] else {
                    unreachable!("analysis classifies the equation target as discrete-valued")
                };
                construction.discrete(|system| system.assignment(owner, target, value))?;
            }
        }
    }
    Ok(())
}

fn equation_generation(origin: &flat::EquationOrigin) -> Option<dae::DaeGeneration> {
    match origin {
        flat::EquationOrigin::ComponentEquation { .. } => None,
        flat::EquationOrigin::Connection { .. } => Some(dae::DaeGeneration::ConnectionEquation),
        flat::EquationOrigin::FlowSum { .. } | flat::EquationOrigin::UnconnectedFlow { .. } => {
            Some(dae::DaeGeneration::FlowBalanceEquation)
        }
        flat::EquationOrigin::Algorithm { .. } => Some(dae::DaeGeneration::AlgorithmEquation),
        flat::EquationOrigin::Reinit { .. } => Some(dae::DaeGeneration::EventActionLowering),
        flat::EquationOrigin::WhenAssignment { .. } => Some(dae::DaeGeneration::DiscreteUpdate),
        flat::EquationOrigin::Binding { .. } => Some(dae::DaeGeneration::BindingEquation),
    }
}

fn equation_owner_provenance(
    origin: &flat::EquationOrigin,
    span: Span,
) -> Result<dae::DaeProvenance, dae::DaeConstructionError> {
    match equation_generation(origin) {
        Some(generation) => dae::DaeProvenance::generated(generation, span),
        None => dae::DaeProvenance::source(span),
    }
}

#[cfg(test)]
mod tests;
