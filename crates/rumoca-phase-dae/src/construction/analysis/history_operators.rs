use rumoca_core::FallibleExpressionVisitor;
use rumoca_ir_flat::FallibleStatementVisitor;

use super::*;

/// Analysis proof for one MLS §3.7.5 `edge` or `change` occurrence.
///
/// The source span alone is not an occurrence identity: separate flattened
/// instances may retain the same declaration span. Pairing it with the exact
/// runtime instance makes certificate lookup independent of rendered names.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct HistoryOperatorKey {
    span: Span,
    instance: InstanceId,
}

#[derive(Clone, Debug, PartialEq)]
pub(in crate::construction) struct HistoryOperatorPlan {
    function: BuiltinFunction,
    instance: InstanceId,
    operand_span: Span,
    subscripts: Box<[Subscript]>,
}

impl HistoryOperatorPlan {
    pub(in crate::construction) fn instance(&self) -> InstanceId {
        self.instance
    }

    pub(in crate::construction) fn operand_span(&self) -> Span {
        self.operand_span
    }

    pub(in crate::construction) fn subscripts(&self) -> &[Subscript] {
        &self.subscripts
    }
}

#[derive(Default)]
pub(in crate::construction) struct HistoryOperatorPlans {
    plans: HashMap<HistoryOperatorKey, HistoryOperatorPlan>,
}

impl HistoryOperatorPlans {
    pub(in crate::construction) fn certificate(
        &self,
        function: BuiltinFunction,
        span: Span,
        instance: InstanceId,
    ) -> Option<&HistoryOperatorPlan> {
        let plan = self.plans.get(&HistoryOperatorKey { span, instance })?;
        (plan.function == function).then_some(plan)
    }
}

pub(super) fn analyze_history_operators(
    owners: &StructuredEquationOwners<'_>,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<HistoryOperatorPlans, ToDaeError> {
    let flat = owners.model();
    let mut analyzer = HistoryOperatorAnalyzer {
        flat,
        roles,
        plans: HistoryOperatorPlans::default(),
        in_function: false,
    };
    analyzer.visit_model_owners(owners)?;
    Ok(analyzer.plans)
}

struct HistoryOperatorAnalyzer<'model> {
    flat: &'model flat::Model,
    roles: &'model HashMap<VarName, PlannedRole>,
    plans: HistoryOperatorPlans,
    in_function: bool,
}

impl HistoryOperatorAnalyzer<'_> {
    fn history_plan(
        &self,
        function: BuiltinFunction,
        arguments: &[Expression],
        span: Span,
    ) -> Result<(HistoryOperatorKey, HistoryOperatorPlan), ToDaeError> {
        require_span(span, format!("{} expression", function.name()))?;
        if self.in_function {
            return Err(ToDaeError::unsupported_runtime_operator(
                function.name(),
                format!(
                    "{} is prohibited in a function semantic owner by SPEC_0022 FUNC-010",
                    function.name()
                ),
                span,
            ));
        }
        let [argument] = arguments else {
            return Err(invalid_history_reference(function, span));
        };
        let Some((name, subscripts)) = derivative_reference(argument) else {
            return Err(invalid_history_reference(function, span));
        };
        if !matches!(
            self.roles.get(name.var_name()),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        ) {
            return Err(ToDaeError::unsupported_flat(
                format!("{} expression", function.name()),
                format!("{}(...) must name a discrete coordinate", function.name()),
                span,
            ));
        }
        let variable = self.flat.variables.get(name.var_name()).ok_or_else(|| {
            ToDaeError::unresolved_reference(name.as_str(), argument.span().unwrap_or(span))
        })?;
        let scalar_type = effective_variable_scalar_type(self.flat, variable).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                format!("{} expression", function.name()),
                format!(
                    "{}(...) requires an exact primitive scalar type certificate",
                    function.name()
                ),
                span,
            )
        })?;
        if function == BuiltinFunction::Edge && scalar_type != dae::ScalarType::Boolean {
            return Err(ToDaeError::unsupported_flat(
                "edge expression",
                "edge(...) must name a Boolean coordinate",
                span,
            ));
        }
        let operand_span = argument
            .span()
            .ok_or_else(|| ToDaeError::MissingProvenance {
                owner: format!("{} operand", function.name()),
            })?;
        let key = HistoryOperatorKey {
            span,
            instance: variable.instance_id,
        };
        let plan = HistoryOperatorPlan {
            function,
            instance: variable.instance_id,
            operand_span,
            subscripts: subscripts.to_vec().into_boxed_slice(),
        };
        Ok((key, plan))
    }
}

impl ModelExpressionOwnerVisitor for HistoryOperatorAnalyzer<'_> {
    fn enter_function_owners(&mut self) -> Result<(), Self::Error> {
        self.in_function = true;
        Ok(())
    }
}

impl FallibleExpressionVisitor for HistoryOperatorAnalyzer<'_> {
    type Error = ToDaeError;

    fn visit_expression(&mut self, expression: &Expression) -> Result<(), Self::Error> {
        if let Expression::BuiltinCall {
            function: function @ (BuiltinFunction::Edge | BuiltinFunction::Change),
            args,
            span,
        } = expression
        {
            let (key, plan) = self.history_plan(*function, args, *span)?;
            if let Some(existing) = self.plans.plans.get(&key)
                && existing != &plan
            {
                return Err(ToDaeError::unsupported_flat(
                    "history operator certificate",
                    "one source occurrence resolved to conflicting runtime coordinates or operands",
                    *span,
                ));
            }
            self.plans.plans.insert(key, plan);
        }
        self.walk_expression(expression)
    }
}

impl FallibleStatementVisitor for HistoryOperatorAnalyzer<'_> {}

fn invalid_history_reference(function: BuiltinFunction, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "history operator expression",
        format!(
            "{}(...) must have exactly one resolved variable-reference operand",
            function.name()
        ),
        span,
    )
}
