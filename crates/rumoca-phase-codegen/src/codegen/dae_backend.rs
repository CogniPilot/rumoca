//! Immutable template projection of the checked DAE.
//!
//! This is deliberately not the wire-v11 representation. Wire records exist
//! for persistence and reconstruction; templates receive semantic arrays with
//! explicit typed identities and systems.

use rumoca_ir_dae as dae;
use serde_json::{Value, json};

pub(super) const TEMPLATE_SCHEMA_VERSION: u16 = 5;

#[derive(Debug, thiserror::Error)]
pub(super) enum DaeBackendError {
    #[error("{0}")]
    Numeric(#[from] rumoca_eval_dae::NumericEvaluationError),
    #[error(
        "numeric attribute `{attribute}` for `{variable}` contains {actual} scalars; expected one or {expected}"
    )]
    AttributeShape {
        variable: String,
        attribute: &'static str,
        actual: usize,
        expected: usize,
        span: rumoca_core::Span,
    },
}

impl DaeBackendError {
    pub(super) const fn span(&self) -> rumoca_core::Span {
        match self {
            Self::Numeric(error) => error.span(),
            Self::AttributeShape { span, .. } => *span,
        }
    }
}

pub(super) fn project(model: &dae::Dae) -> Result<Value, DaeBackendError> {
    model.inspect(project_view)
}

fn project_view(view: dae::DaeView<'_>) -> Result<Value, DaeBackendError> {
    Ok(json!({
        "schema": {
            "name": "rumoca.checked-dae-template",
            "version": TEMPLATE_SCHEMA_VERSION,
        },
        "value_types": project_value_types(view),
        "variables": project_variables(view)?,
        "functions": project_functions(view),
        "domains": project_domains(view),
        "expressions": project_expressions(view),
        "systems": {
            "continuous": project_continuous(view),
            "initialization": project_initialization(view),
            "discrete_real": project_discrete_real(view),
            "discrete_values": project_discrete_values(view),
            "conditions": project_conditions(view),
            "events": project_events(view),
            "clocks": project_clocks(view),
            "temporal": project_temporal(view),
        },
    }))
}

fn project_value_types(view: dae::DaeView<'_>) -> Vec<Value> {
    (0..view.value_type_count())
        .map(|index| {
            let id = view
                .value_type_id(index)
                .expect("dense checked value-type identity resolves");
            json!({
                "id": id.index(),
                "value_type": view
                    .value_type(id)
                    .expect("checked value type resolves"),
                "effective_flat_type": view.effective_flat_type(id).map(|id| id.0),
                "provenance": view
                    .value_type_provenance(id)
                    .expect("checked value type has provenance"),
            })
        })
        .collect()
}

fn project_variables(view: dae::DaeView<'_>) -> Result<Vec<Value>, DaeBackendError> {
    let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(view);
    let mut projected = Vec::with_capacity(view.variable_count());
    for (id, variable) in view.variables() {
        let scalar_count = variable.value_type().scalar_count();
        let static_binding = matches!(
            variable.role(),
            dae::VariableRole::Parameter | dae::VariableRole::Constant
        )
        .then_some(variable.binding())
        .flatten();
        let binding_values = numeric_values(&mut evaluator, variable, "binding", static_binding)?;
        let start_values = numeric_values(&mut evaluator, variable, "start", variable.start())?;
        let minimum_values =
            numeric_values(&mut evaluator, variable, "minimum", variable.minimum())?;
        let maximum_values =
            numeric_values(&mut evaluator, variable, "maximum", variable.maximum())?;
        let nominal_values =
            numeric_values(&mut evaluator, variable, "nominal", variable.nominal())?;
        projected.push(json!({
            "id": id.index(),
            "name": variable.name().as_str(),
            "role": variable.role(),
            "variability": variable.variability(),
            "value_type": {
                "scalar": variable.value_type().scalar_type(),
                "dimensions": variable.value_type().dimensions(),
            },
            "value_type_id": variable.value_type_id().index(),
            "scalar_count": scalar_count,
            "scalar_names": scalar_count
                .into_iter()
                .flat_map(|count| 0..count)
                .filter_map(|scalar| variable.scalar_name(scalar))
                .collect::<Vec<_>>(),
            "declaration": variable.declaration(),
            "attributes": {
                "component_reference": variable.component_reference(),
                "binding": variable.binding().map(|id| id.index()),
                "binding_values": binding_values,
                "start": variable.start().map(|id| id.index()),
                "start_values": start_values,
                "fixed": variable.fixed(),
                "minimum": variable.minimum().map(|id| id.index()),
                "minimum_values": minimum_values,
                "maximum": variable.maximum().map(|id| id.index()),
                "maximum_values": maximum_values,
                "nominal": variable.nominal().map(|id| id.index()),
                "nominal_values": nominal_values,
                "unit": variable.unit(),
                "state_select": variable.state_select(),
                "description": variable.description(),
                "causality": variable.causality(),
                "tunable": variable.is_tunable(),
                "origin": variable.origin(),
            },
        }));
    }
    Ok(projected)
}

fn numeric_values<'dae>(
    evaluator: &mut rumoca_eval_dae::NumericEvaluator<'dae>,
    variable: dae::VariableView<'dae>,
    attribute: &'static str,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<Vec<f64>>, DaeBackendError> {
    let numeric_expression = expression.filter(|_| {
        matches!(
            variable.value_type().scalar_type(),
            dae::ScalarType::Real | dae::ScalarType::Integer
        )
    });
    numeric_expression
        .map(|expression| {
            let mut values = evaluator.expression(expression)?;
            if values.len() == 1 && variable.scalar_count() > 1 {
                values.resize(variable.scalar_count(), values[0]);
            }
            if values.len() != variable.scalar_count() {
                return Err(DaeBackendError::AttributeShape {
                    variable: variable.name().to_string(),
                    attribute,
                    actual: values.len(),
                    expected: variable.scalar_count(),
                    span: variable.declaration().span(),
                });
            }
            Ok(values)
        })
        .transpose()
}

fn project_functions(view: dae::DaeView<'_>) -> Vec<Value> {
    (0..view.function_count())
        .map(|index| {
            let id = view
                .function_id(index)
                .expect("dense checked function identity resolves");
            let function = view.function(id).expect("checked function resolves");
            json!({
                "id": id.index(),
                "name": function.name().as_str(),
                "parameters": function
                    .parameters()
                    .map(|parameter| json!({
                        "ordinal": parameter.id().ordinal(),
                        "name": parameter.name().as_str(),
                        "value_type": parameter.value_type().index(),
                        "declaration": parameter.declaration(),
                    }))
                    .collect::<Vec<_>>(),
                "parameter_types": function
                    .parameter_types()
                    .iter()
                    .map(|id| id.index())
                    .collect::<Vec<_>>(),
                "result_types": function
                    .result_types()
                    .iter()
                    .map(|id| id.index())
                    .collect::<Vec<_>>(),
                "values": function
                    .values()
                    .map(|value| json!({
                        "ordinal": value.id().ordinal(),
                        "name": value.name().as_str(),
                        "value_type": value.value_type().index(),
                        "role": value.role(),
                        "declaration": value.declaration(),
                    }))
                    .collect::<Vec<_>>(),
                "definitions": project_function_definitions(view, function),
                "statements": function
                    .statements()
                    .map(project_function_statement)
                    .collect::<Vec<_>>(),
                "folds": (0..function.fold_count())
                    .map(|ordinal| {
                        let fold = view
                            .function_fold(function.fold_id(ordinal).unwrap())
                            .expect("checked function fold resolves");
                        json!({
                            "ordinal": fold.id().ordinal(),
                            "domain": fold.domain().index(),
                            "targets": fold.targets().map(|target| target.ordinal()).collect::<Vec<_>>(),
                            "parameter_definitions": definition_ordinals(fold.parameter_values()),
                            "initial_definitions": definition_ordinals(fold.initial_values()),
                            "update_definitions": definition_ordinals(fold.update_values()),
                            "output_definitions": definition_ordinals(fold.output_values()),
                            "provenance": fold.provenance(),
                        })
                    })
                    .collect::<Vec<_>>(),
                "results": function
                    .result_values()
                    .iter()
                    .map(|definition| definition.id().ordinal())
                    .collect::<Vec<_>>(),
                "declaration": function.declaration(),
            })
        })
        .collect()
}

fn project_function_definitions<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Vec<Value> {
    let mut current = vec![None; function.values().count()];
    (0..function.definition_count())
        .map(|ordinal| {
            let id = function
                .definition_id(ordinal)
                .expect("dense checked function definition identity resolves");
            let definition = view
                .function_definition(id)
                .expect("checked function definition resolves");
            let target = definition.target().ordinal() as usize;
            let previous_target_definition = current[target];
            current[target] = Some(definition.id().ordinal());
            json!({
                "ordinal": definition.id().ordinal(),
                "target": target,
                "rhs": definition.rhs().index(),
                "previous_target_definition": previous_target_definition,
                "provenance": definition.provenance(),
            })
        })
        .collect()
}

fn project_function_statement(statement: dae::FunctionStatementView<'_>) -> Value {
    match statement {
        dae::FunctionStatementView::Assignment { definition } => json!({
            "kind": "assignment",
            "definition": definition.id().ordinal(),
        }),
        dae::FunctionStatementView::For {
            fold,
            statements,
            provenance,
        } => json!({
            "kind": "for",
            "fold": fold.ordinal(),
            "statements": statements.map(project_function_statement).collect::<Vec<_>>(),
            "provenance": provenance,
        }),
    }
}

fn project_domains(view: dae::DaeView<'_>) -> Vec<Value> {
    (0..view.domain_count())
        .map(|index| {
            let id = view
                .domain_id(index)
                .expect("dense checked domain identity resolves");
            let domain = view.domain(id).expect("checked domain resolves");
            json!({
                "id": id.index(),
                "domain": domain.structured(),
                "extents": domain.extents(),
                "scalar_count": domain.scalar_count(),
                "provenance": domain.provenance(),
            })
        })
        .collect()
}

fn project_expressions(view: dae::DaeView<'_>) -> Vec<Value> {
    (0..view.expression_count())
        .map(|index| {
            let id = view
                .expression_id(index)
                .expect("dense checked expression identity resolves");
            let expression = view.expression(id).expect("checked expression resolves");
            json!({
                "id": id.index(),
                "operation": project_expression_operation(expression.operation()),
                "value_type_id": expression.value_type_id().index(),
                "value_type": {
                    "scalar": expression.value_type().scalar_type(),
                    "dimensions": expression.value_type().dimensions(),
                },
                "variability": expression.variability(),
                "binder_domain": expression.binder_domain().map(|id| id.index()),
                "provenance": expression.provenance(),
            })
        })
        .collect()
}

fn project_expression_operation(operation: dae::ExpressionOperation<'_>) -> Value {
    match operation {
        dae::ExpressionOperation::Literal(value) => json!({
            "kind": "literal",
            "value": value,
        }),
        dae::ExpressionOperation::Coordinate(coordinate) => json!({
            "kind": "coordinate",
            "coordinate": project_coordinate(coordinate),
        }),
        dae::ExpressionOperation::Unary { operator, operand } => json!({
            "kind": "unary",
            "operator": operator,
            "operand": operand.index(),
        }),
        dae::ExpressionOperation::Binary { operator, lhs, rhs } => json!({
            "kind": "binary",
            "operator": operator,
            "lhs": lhs.index(),
            "rhs": rhs.index(),
        }),
        dae::ExpressionOperation::Conditional(operands) => json!({
            "kind": "conditional",
            "operands": expression_ids(operands),
        }),
        dae::ExpressionOperation::Array(elements) => json!({
            "kind": "array",
            "elements": expression_ids(elements),
        }),
        dae::ExpressionOperation::Record(fields) => json!({
            "kind": "record",
            "fields": expression_ids(fields),
        }),
        dae::ExpressionOperation::Field { base, field } => json!({
            "kind": "field",
            "base": base.index(),
            "field": field,
        }),
        dae::ExpressionOperation::Range(range) => project_range_operation(range),
        dae::ExpressionOperation::Comprehension { domain, body } => json!({
            "kind": "comprehension",
            "domain": domain.index(),
            "body": body.index(),
        }),
        dae::ExpressionOperation::Index { base, subscripts } => {
            project_index_operation(base, subscripts)
        }
        dae::ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } => project_array_update_operation(base, value, subscripts),
        dae::ExpressionOperation::Builtin { builtin, arguments } => {
            project_builtin_operation(builtin, arguments)
        }
        dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
        } => project_call_operation(function, output, arguments),
        dae::ExpressionOperation::StringConversion {
            declaration,
            value,
            format,
        } => project_string_conversion(declaration, value, format),
        dae::ExpressionOperation::FunctionValue { value, definition } => {
            project_function_value_operation(value, definition)
        }
        dae::ExpressionOperation::FunctionFoldParameter {
            fold,
            carried,
            definition,
        } => project_function_fold_operation("function_fold_parameter", fold, carried, definition),
        dae::ExpressionOperation::FunctionFoldOutput {
            fold,
            carried,
            definition,
        } => project_function_fold_operation("function_fold_output", fold, carried, definition),
    }
}

fn project_string_conversion(
    declaration: rumoca_core::DefId,
    value: dae::ExprId<'_>,
    format: dae::StringConversionFormatView<'_>,
) -> Value {
    let format = match format {
        dae::StringConversionFormatView::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => json!({
            "kind": "options",
            "minimum_length": minimum_length.map(dae::ExprId::index),
            "left_justified": left_justified.map(dae::ExprId::index),
            "significant_digits": significant_digits.map(dae::ExprId::index),
        }),
        dae::StringConversionFormatView::Format { value } => json!({
            "kind": "format",
            "value": value.index(),
        }),
    };
    json!({
        "kind": "string_conversion",
        "declaration": declaration,
        "value": value.index(),
        "format": format,
    })
}

fn project_range_operation(range: dae::RangeView<'_>) -> Value {
    json!({
        "kind": "range",
        "start": project_range_bound(range.start()),
        "explicit_step": range.explicit_step().map(project_range_bound),
        "stop": project_range_bound(range.stop()),
    })
}

fn project_range_bound(bound: dae::RangeBoundView<'_>) -> Value {
    json!({
        "expression": bound.expression().index(),
        "value": bound.value(),
        "provenance": bound.provenance(),
    })
}

fn project_index_operation(base: dae::ExprId<'_>, subscripts: dae::SubscriptsView<'_>) -> Value {
    json!({
        "kind": "index",
        "base": base.index(),
        "subscripts": subscripts.iter().map(project_subscript).collect::<Vec<_>>(),
    })
}

fn project_array_update_operation(
    base: dae::ExprId<'_>,
    value: dae::ExprId<'_>,
    subscripts: dae::SubscriptsView<'_>,
) -> Value {
    json!({
        "kind": "array_update",
        "base": base.index(),
        "value": value.index(),
        "subscripts": subscripts.iter().map(project_subscript).collect::<Vec<_>>(),
    })
}

fn project_builtin_operation(
    builtin: dae::PureBuiltin,
    arguments: dae::ExpressionOperands<'_>,
) -> Value {
    json!({
        "kind": "builtin",
        "builtin": builtin,
        "arguments": expression_ids(arguments),
    })
}

fn project_call_operation(
    function: dae::FunctionId<'_>,
    output: u32,
    arguments: dae::ExpressionOperands<'_>,
) -> Value {
    json!({
        "kind": "call",
        "function": function.index(),
        "output": output,
        "arguments": expression_ids(arguments),
    })
}

fn project_function_value_operation(
    value: dae::FunctionValueId<'_>,
    definition: dae::FunctionDefinitionView<'_>,
) -> Value {
    json!({
        "kind": "function_value",
        "function": value.function().index(),
        "value": value.ordinal(),
        "definition": definition.id().ordinal(),
    })
}

fn project_function_fold_operation(
    kind: &'static str,
    fold: dae::FunctionFoldId<'_>,
    carried: u32,
    definition: dae::FunctionDefinitionView<'_>,
) -> Value {
    json!({
        "kind": kind,
        "function": fold.function().index(),
        "fold": fold.ordinal(),
        "carried": carried,
        "definition": definition.id().ordinal(),
    })
}

fn expression_ids(operands: dae::ExpressionOperands<'_>) -> Vec<u32> {
    operands.iter().map(|id| id.index()).collect()
}

fn definition_ordinals(definitions: dae::FunctionDefinitionValues<'_>) -> Vec<u32> {
    definitions
        .iter()
        .map(|definition| definition.id().ordinal())
        .collect()
}

fn project_coordinate(coordinate: dae::CoordinateView<'_>) -> Value {
    match coordinate {
        dae::CoordinateView::Parameter(id) => coordinate_id("parameter", id.index()),
        dae::CoordinateView::Input(id) => coordinate_id("input", id.index()),
        dae::CoordinateView::State(id) => coordinate_id("state", id.index()),
        dae::CoordinateView::Derivative(id) => coordinate_id("derivative", id.index()),
        dae::CoordinateView::Algebraic(id) => coordinate_id("algebraic", id.index()),
        dae::CoordinateView::DiscreteReal(id) => coordinate_id("discrete_real", id.index()),
        dae::CoordinateView::DiscreteValue(id) => coordinate_id("discrete_value", id.index()),
        dae::CoordinateView::PreDiscreteReal(id) => coordinate_id("pre_discrete_real", id.index()),
        dae::CoordinateView::PreDiscreteValue(id) => {
            coordinate_id("pre_discrete_value", id.index())
        }
        dae::CoordinateView::Time => json!({ "kind": "time" }),
        dae::CoordinateView::Condition(id) => coordinate_id("condition", id.index()),
        dae::CoordinateView::Delay(id) => coordinate_id("delay", id.index()),
        dae::CoordinateView::Previous(id) => coordinate_id("previous", id.index()),
        dae::CoordinateView::Terminal(id) => coordinate_id("terminal", id.index()),
        dae::CoordinateView::Binder(id) => json!({
            "kind": "binder",
            "domain": id.domain().index(),
            "ordinal": id.ordinal(),
        }),
        dae::CoordinateView::FunctionParameter(id) => json!({
            "kind": "function_parameter",
            "function": id.function().index(),
            "ordinal": id.ordinal(),
        }),
    }
}

fn coordinate_id(kind: &'static str, id: u32) -> Value {
    json!({ "kind": kind, "id": id })
}

fn project_subscript(subscript: dae::SubscriptView<'_>) -> Value {
    match subscript {
        dae::SubscriptView::Index {
            expression,
            provenance,
        } => json!({
            "kind": "index",
            "expression": expression.index(),
            "provenance": provenance,
        }),
        dae::SubscriptView::Whole { provenance } => json!({
            "kind": "whole",
            "provenance": provenance,
        }),
        dae::SubscriptView::Slice {
            expression,
            provenance,
        } => json!({
            "kind": "slice",
            "expression": expression.index(),
            "provenance": provenance,
        }),
    }
}

fn project_continuous(view: dae::DaeView<'_>) -> Value {
    json!({
        "owners": view
            .continuous_owners()
            .map(project_continuous_owner)
            .collect::<Vec<_>>(),
    })
}

fn project_continuous_owner(owner: dae::ContinuousOwnerView<'_>) -> Value {
    match owner {
        dae::ContinuousOwnerView::Residual { id, equation } => json!({
            "kind": "residual",
            "id": id.index(),
            "residual": equation.residual().index(),
            "provenance": equation.provenance(),
        }),
        dae::ContinuousOwnerView::Structured { id, family } => {
            project_family("structured", id.index(), family)
        }
    }
}

fn project_initialization(view: dae::DaeView<'_>) -> Value {
    json!({
        "owners": (0..view.initialization_owner_count())
            .map(|index| {
                let owner = view
                    .initialization_owner(index)
                    .expect("dense checked initialization owner resolves");
                match owner {
                    dae::InitializationOwnerView::Residual { id, equation } => json!({
                        "kind": "residual",
                        "id": id.index(),
                        "residual": equation.residual().index(),
                        "provenance": equation.provenance(),
                    }),
                    dae::InitializationOwnerView::Structured { id, family } => {
                        project_family("structured", id.index(), family)
                    }
                }
            })
            .collect::<Vec<_>>(),
    })
}

fn project_family(kind: &'static str, id: u32, family: dae::StructuredFamilyView<'_>) -> Value {
    json!({
        "kind": kind,
        "id": id,
        "domain": family.domain().index(),
        "scalar_view": family.scalar_view(),
        "bodies": expression_ids(family.bodies()),
        "scalar_rows": family.scalar_rows(),
        "provenance": family.provenance(),
    })
}

fn project_discrete_real(view: dae::DaeView<'_>) -> Value {
    json!({
        "residuals": (0..view.discrete_real_equation_count())
            .map(|index| {
                let equation = view
                    .discrete_real_equation(index)
                    .expect("dense checked discrete Real equation resolves");
                let activation = match equation.activation() {
                    dae::DiscreteRealActivation::Always => {
                        json!({ "kind": "always" })
                    }
                    dae::DiscreteRealActivation::When { trigger, guard } => json!({
                        "kind": "when",
                        "trigger": trigger.index(),
                        "guard": guard.index(),
                    }),
                };
                json!({
                    "id": index,
                    "residual": equation.residual().index(),
                    "activation": activation,
                    "provenance": equation.provenance(),
                })
            })
            .collect::<Vec<_>>(),
    })
}

fn project_discrete_values(view: dae::DaeView<'_>) -> Value {
    let owners = (0..view.discrete_value_owner_count())
        .map(|index| {
            let id = view
                .discrete_value_owner_id(index)
                .expect("dense checked B.1c owner identity resolves");
            let owner = view
                .discrete_value_owner(id)
                .expect("checked B.1c owner resolves");
            json!({
                "id": id.index(),
                "targets": owner
                    .targets()
                    .iter()
                    .map(|target| target.index())
                    .collect::<Vec<_>>(),
                "branches": owner
                    .branches()
                    .iter()
                    .map(|branch| {
                        let activation = match branch.activation() {
                            dae::DiscreteBranchActivation::Always => {
                                json!({ "kind": "always" })
                            }
                            dae::DiscreteBranchActivation::When { trigger, guard } => json!({
                                "kind": "when",
                                "trigger": trigger.index(),
                                "guard": guard.index(),
                            }),
                        };
                        json!({
                            "activation": activation,
                            "values": branch
                                .values()
                                .iter()
                                .map(|(value, provenance)| json!({
                                    "expression": value.index(),
                                    "provenance": provenance,
                                }))
                                .collect::<Vec<_>>(),
                            "provenance": branch.provenance(),
                        })
                    })
                    .collect::<Vec<_>>(),
                "provenance": owner.provenance(),
            })
        })
        .collect::<Vec<_>>();
    json!({ "owners": owners })
}

fn project_conditions(view: dae::DaeView<'_>) -> Value {
    json!({
        "relations": (0..view.relation_count())
            .map(|index| {
                let id = view
                    .relation_id(index)
                    .expect("dense checked relation identity resolves");
                let relation = view.relation(id).expect("checked relation resolves");
                json!({
                    "id": id.index(),
                    "expression": relation.expression().index(),
                    "provenance": relation.provenance(),
                })
            })
            .collect::<Vec<_>>(),
        "conditions": (0..view.condition_count())
            .map(|index| {
                let id = view
                    .condition_id(index)
                    .expect("dense checked condition identity resolves");
                let condition = view.condition(id).expect("checked condition resolves");
                json!({
                    "id": id.index(),
                    "operation": project_condition_operation(condition.operation()),
                    "provenance": condition.provenance(),
                })
            })
            .collect::<Vec<_>>(),
        "roots": (0..view.root_count())
            .map(|index| {
                let id = view
                    .root_id(index)
                    .expect("dense checked root identity resolves");
                let root = view.root(id).expect("checked root resolves");
                json!({
                    "id": id.index(),
                    "relation": root.relation().index(),
                    "activation": root.activation().index(),
                    "provenance": root.provenance(),
                })
            })
            .collect::<Vec<_>>(),
    })
}

fn project_condition_operation(operation: dae::ConditionOperation<'_>) -> Value {
    match operation {
        dae::ConditionOperation::Initial => json!({ "kind": "initial" }),
        dae::ConditionOperation::Relation(id) => {
            json!({ "kind": "relation", "relation": id.index() })
        }
        dae::ConditionOperation::Discrete(id) => {
            json!({ "kind": "discrete", "expression": id.index() })
        }
        dae::ConditionOperation::Clock(id) => {
            json!({ "kind": "clock", "clock": id.index() })
        }
        dae::ConditionOperation::Not(id) => {
            json!({ "kind": "not", "condition": id.index() })
        }
        dae::ConditionOperation::And(lhs, rhs) => json!({
            "kind": "and",
            "lhs": lhs.index(),
            "rhs": rhs.index(),
        }),
        dae::ConditionOperation::Or(lhs, rhs) => json!({
            "kind": "or",
            "lhs": lhs.index(),
            "rhs": rhs.index(),
        }),
    }
}

fn project_events(view: dae::DaeView<'_>) -> Value {
    json!({
        "time_events": (0..view.time_event_count())
            .map(|index| {
                let id = view
                    .time_event_id(index)
                    .expect("dense checked time-event identity resolves");
                let event = view.time_event(id).expect("checked time event resolves");
                json!({
                    "id": id.index(),
                    "instant": event.instant(),
                    "provenance": event.provenance(),
                })
            })
            .collect::<Vec<_>>(),
        "actions": (0..view.event_action_count())
            .map(|index| {
                let id = view
                    .event_action_id(index)
                    .expect("dense checked event-action identity resolves");
                let action = view.event_action(id).expect("checked event action resolves");
                json!({
                    "id": id.index(),
                    "guard": action.guard().index(),
                    "operation": project_event_action(action.operation()),
                    "provenance": action.provenance(),
                })
            })
            .collect::<Vec<_>>(),
    })
}

fn project_event_action(operation: dae::EventActionOperation<'_>) -> Value {
    match operation {
        dae::EventActionOperation::Assert { message, level } => json!({
            "kind": "assert",
            "message": message.index(),
            "level": level.map(|id| id.index()),
        }),
        dae::EventActionOperation::Terminate { message } => json!({
            "kind": "terminate",
            "message": message.index(),
        }),
        dae::EventActionOperation::Reinitialize { state, value } => json!({
            "kind": "reinitialize",
            "state": state.index(),
            "value": value.index(),
        }),
    }
}

fn project_clocks(view: dae::DaeView<'_>) -> Value {
    json!({
        "clocks": (0..view.clock_count())
            .map(|index| {
                let id = view
                    .clock_id(index)
                    .expect("dense checked clock identity resolves");
                let clock = view.clock(id).expect("checked clock resolves");
                let operation = match clock.operation() {
                    dae::ClockOperation::Periodic(lattice) => json!({
                        "kind": "periodic",
                        "lattice": lattice,
                    }),
                    dae::ClockOperation::Triggered(condition) => json!({
                        "kind": "triggered",
                        "condition": condition.index(),
                    }),
                };
                json!({
                    "id": id.index(),
                    "operation": operation,
                    "provenance": clock.provenance(),
                })
            })
            .collect::<Vec<_>>(),
        "ownerships": (0..view.clock_ownership_count())
            .map(|index| {
                let id = view
                    .clock_ownership_id(index)
                    .expect("dense checked clock ownership identity resolves");
                let ownership = view
                    .clock_ownership(id)
                    .expect("checked clock ownership resolves");
                json!({
                    "id": id.index(),
                    "variable": ownership.variable().index(),
                    "kind": match ownership.kind() {
                        dae::ClockedVariableKind::DiscreteReal => "discrete_real",
                        dae::ClockedVariableKind::DiscreteValue => "discrete_value",
                    },
                    "clock": ownership.clock().index(),
                    "provenance": ownership.provenance(),
                })
            })
            .collect::<Vec<_>>(),
    })
}

fn project_temporal(view: dae::DaeView<'_>) -> Value {
    json!({
        "previous": (0..view.previous_value_count())
            .map(|index| {
                let id = view
                    .previous_id(index)
                    .expect("dense checked previous identity resolves");
                let previous = view.previous(id).expect("checked previous value resolves");
                json!({
                    "id": id.index(),
                    "variable": previous.variable().index(),
                    "clock": previous.clock().index(),
                    "provenance": previous.provenance(),
                })
            })
            .collect::<Vec<_>>(),
        "terminals": (0..view.terminal_count())
            .map(|index| {
                let id = view
                    .terminal_id(index)
                    .expect("dense checked terminal identity resolves");
                let terminal = view.terminal(id).expect("checked terminal resolves");
                json!({
                    "id": id.index(),
                    "provenance": terminal.provenance(),
                })
            })
            .collect::<Vec<_>>(),
        "delays": (0..view.delay_count())
            .map(|index| {
                let id = view
                    .delay_id(index)
                    .expect("dense checked delay identity resolves");
                let delay = view.delay(id).expect("checked delay resolves");
                let operation = match delay.operation() {
                    dae::DelayOperation::ParameterDelay { delay_time } => json!({
                        "parameter_delay": {
                            "delay_time": project_positive_parameter(delay_time),
                        }
                    }),
                    dae::DelayOperation::BoundedDelay {
                        delay_time,
                        delay_max,
                    } => json!({
                        "bounded_delay": {
                            "delay_time": delay_time.index(),
                            "delay_max": project_positive_parameter(delay_max),
                        }
                    }),
                };
                json!({
                    "id": id.index(),
                    "source": delay.source().index(),
                    "operation": operation,
                    "value_type": {
                        "scalar": delay.value_type().scalar_type(),
                        "dimensions": delay.value_type().dimensions(),
                    },
                    "variability": delay.variability(),
                    "provenance": delay.provenance(),
                })
            })
            .collect::<Vec<_>>(),
    })
}

fn project_positive_parameter(parameter: dae::PositiveParameterView<'_>) -> Value {
    json!({
        "expression": parameter.expression().index(),
        "value": parameter.value(),
        "provenance": parameter.provenance(),
    })
}
