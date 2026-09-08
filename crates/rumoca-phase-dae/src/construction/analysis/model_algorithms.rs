use super::*;

type EventAlgorithmAnalysis<'flat> = (
    Vec<(Span, PeriodicClockSchedule)>,
    ModelAlgorithmSequence<'flat>,
);

struct ModelAlgorithmAnalysisRequest<'analysis> {
    roles: &'analysis HashMap<VarName, PlannedRole>,
    expression_roles: &'analysis HashMap<VarName, PlannedRole>,
    constants: &'analysis EvalContext,
    function_shapes: &'analysis FunctionShapeAnalysis,
    sample_aliases: &'analysis HashMap<VarName, PeriodicClockSchedule>,
    sample_lattices: &'analysis mut Vec<(Span, PeriodicClockSchedule)>,
}

pub(super) fn analyze_event_algorithms<'flat>(
    flat: &'flat flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
    function_shapes: &FunctionShapeAnalysis,
    sample_aliases: &HashMap<VarName, PeriodicClockSchedule>,
) -> Result<EventAlgorithmAnalysis<'flat>, ToDaeError> {
    let mut sample_lattices = Vec::new();
    validate_when_chains(
        &flat.when_chains,
        roles,
        expression_roles,
        constants,
        function_shapes.model_values(),
        &mut sample_lattices,
    )?;
    let plans = analyze_model_algorithms(
        flat,
        ModelAlgorithmAnalysisRequest {
            roles,
            expression_roles,
            constants,
            function_shapes,
            sample_aliases,
            sample_lattices: &mut sample_lattices,
        },
    )?;
    Ok((sample_lattices, plans))
}

pub(in crate::construction) struct ModelAlgorithmSequence<'flat> {
    flat: &'flat flat::Model,
    entries: Box<[ModelAlgorithmEntry<'flat>]>,
}

impl<'flat> ModelAlgorithmSequence<'flat> {
    pub(in crate::construction) fn flat(&self) -> &'flat flat::Model {
        self.flat
    }

    pub(in crate::construction) fn entries<'sequence>(
        &'sequence self,
    ) -> impl ExactSizeIterator<
        Item = (&'flat flat::Algorithm, &'sequence ModelAlgorithmPlan<'flat>),
    > + 'sequence {
        self.entries.iter().map(|entry| (entry.source, &entry.plan))
    }
}

fn analyze_model_algorithms<'flat>(
    flat: &'flat flat::Model,
    request: ModelAlgorithmAnalysisRequest<'_>,
) -> Result<ModelAlgorithmSequence<'flat>, ToDaeError> {
    let ModelAlgorithmAnalysisRequest {
        roles,
        expression_roles,
        constants,
        function_shapes,
        sample_aliases,
        sample_lattices,
    } = request;
    let entries = flat
        .algorithms
        .iter()
        .map(|source| -> Result<ModelAlgorithmEntry<'flat>, ToDaeError> {
            validate_model_algorithm(
                flat,
                source,
                expression_roles,
                function_shapes.model_values(),
                constants,
                sample_lattices,
            )?;
            Ok(ModelAlgorithmEntry {
                source,
                plan: analyze_model_algorithm(
                    flat,
                    source,
                    roles,
                    function_shapes,
                    constants,
                    sample_aliases,
                )?,
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ModelAlgorithmSequence {
        flat,
        entries: entries.into_boxed_slice(),
    })
}

struct ModelAlgorithmEntry<'flat> {
    source: &'flat flat::Algorithm,
    plan: ModelAlgorithmPlan<'flat>,
}

#[cfg(test)]
mod sequence_api_tests {
    use super::*;

    macro_rules! assert_not_implemented {
        ($ty:ty, $bound:path) => {
            const _: fn() = || {
                trait AmbiguousIfImplemented<Marker> {
                    fn probe() {}
                }
                impl<T> AmbiguousIfImplemented<()> for T {}
                struct Implements;
                impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}
                let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
            };
        };
    }

    assert_not_implemented!(ModelAlgorithmSequence<'static>, ::core::clone::Clone);
    assert_not_implemented!(EventLoweringProduct<'static>, ::core::clone::Clone);

    #[test]
    fn algorithm_sequence_and_event_product_are_affine_api_products() {}
}

pub(in crate::construction) enum ModelAlgorithmPlan<'flat> {
    Declarative {
        target: VarName,
    },
    TotalArrayDefinition {
        target: VarName,
        domain: StructuredIndexDomain,
        binder_spans: Vec<Span>,
    },
    SeparatedArraySum {
        array_target: VarName,
        scalar_target: VarName,
        domain: StructuredIndexDomain,
        binder_spans: Vec<Span>,
    },
    Event(EventLoweringProduct<'flat>),
}

pub(in crate::construction) struct EventLoweringProduct<'flat> {
    statements: Box<[EventStatementPlan<'flat>]>,
    targets: Box<[VarName]>,
}

impl<'flat> EventLoweringProduct<'flat> {
    pub(in crate::construction) fn statements(&self) -> &[EventStatementPlan<'flat>] {
        &self.statements
    }

    pub(in crate::construction) fn targets(&self) -> &[VarName] {
        &self.targets
    }
}

pub(in crate::construction) enum EventStatementPlan<'flat> {
    Assignment {
        component: &'flat rumoca_core::ComponentReference,
        value: &'flat Expression,
        span: Span,
        route: EventAssignmentRoute<'flat>,
    },
    If {
        blocks: Box<[EventBlockPlan<'flat>]>,
        else_product: EventElseProduct<'flat>,
        span: Span,
    },
    When {
        blocks: Box<[EventBlockPlan<'flat>]>,
        span: Span,
    },
    FunctionCall {
        component: &'flat rumoca_core::Reference,
        arguments: &'flat [Expression],
        span: Span,
        plan: ModelEventFunctionCallPlan,
    },
    TensorLoop(ModelEventTensorLoopPlan<'flat>),
    Assert {
        condition: &'flat Expression,
        message: &'flat Expression,
        level: Option<&'flat Expression>,
        span: Span,
    },
}

pub(in crate::construction) struct EventBlockPlan<'flat> {
    pub(in crate::construction) condition: AlgorithmConditionProduct<'flat>,
    pub(in crate::construction) statements: Box<[EventStatementPlan<'flat>]>,
}

pub(in crate::construction) enum EventElseProduct<'flat> {
    Absent,
    Statements(Box<[EventStatementPlan<'flat>]>),
}

pub(in crate::construction) enum EventAssignmentRoute<'flat> {
    Coordinate,
    Structured(StructuredAssignmentPlan),
    FunctionCall {
        component: &'flat rumoca_core::Reference,
        arguments: &'flat [Expression],
        plan: ModelEventFunctionCallPlan,
    },
}

pub(in crate::construction) struct ModelEventTensorLoopPlan<'flat> {
    pub(in crate::construction) domain: StructuredIndexDomain,
    pub(in crate::construction) binder_spans: Vec<Span>,
    pub(in crate::construction) assignments: Box<[ModelEventTensorAssignment<'flat>]>,
    pub(in crate::construction) span: Span,
}

pub(in crate::construction) struct ModelEventTensorAssignment<'flat> {
    pub(in crate::construction) target: VarName,
    pub(in crate::construction) component: &'flat rumoca_core::ComponentReference,
    pub(in crate::construction) value: &'flat Expression,
    pub(in crate::construction) span: Span,
}

pub(super) fn analyze_model_algorithm<'flat>(
    flat: &'flat flat::Model,
    algorithm: &'flat flat::Algorithm,
    roles: &HashMap<VarName, PlannedRole>,
    shapes: &FunctionShapeAnalysis,
    constants: &EvalContext,
    sample_aliases: &HashMap<VarName, PeriodicClockSchedule>,
) -> Result<ModelAlgorithmPlan<'flat>, ToDaeError> {
    let model_values = shapes.model_values();
    if contains_event_control(&algorithm.statements) {
        let targets = model_algorithm_targets(flat, algorithm);
        if targets.iter().any(|target| {
            !matches!(
                roles[target],
                PlannedRole::DiscreteReal | PlannedRole::DiscreteValue
            )
        }) {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "a mixed continuous/event algorithm requires one checked atomic owner",
                algorithm.span,
            ));
        }
        let calls = ModelEventCallAnalysis::new(flat, roles, shapes);
        return issue_event_lowering_product(
            &algorithm.statements,
            targets,
            EventAnalysisContext {
                flat,
                roles,
                model_values,
                calls: &calls,
                constants,
                sample_aliases,
            },
        )
        .map(ModelAlgorithmPlan::Event);
    }
    let targets = model_algorithm_targets(flat, algorithm);
    if let Some(plan) = analyze_separated_array_sum(flat, algorithm, &targets, roles, model_values)?
    {
        return Ok(plan);
    }
    let [target] = targets.as_slice() else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "a multi-output algorithm requires one checked atomic vector-equation owner",
            algorithm.span,
        ));
    };
    let variable = &flat.variables[target];
    if !variable.dims.is_empty() {
        return analyze_total_array_definition(algorithm, target, &variable.dims, model_values);
    }
    if !is_declarative_role(roles[target]) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "algorithm target `{target}` has non-computable role {:?}",
                roles[target]
            ),
            algorithm.span,
        ));
    }
    let assigned = validate_declarative_sequence(&algorithm.statements, target, false)?;
    if !assigned {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!("algorithm does not define `{target}` on every control-flow path"),
            algorithm.span,
        ));
    }
    Ok(ModelAlgorithmPlan::Declarative {
        target: target.clone(),
    })
}

#[derive(Clone, Copy)]
struct EventAnalysisContext<'flat, 'analysis> {
    flat: &'flat flat::Model,
    roles: &'analysis HashMap<VarName, PlannedRole>,
    model_values: &'analysis ShapeEnvironment,
    calls: &'analysis ModelEventCallAnalysis<'flat, 'analysis>,
    constants: &'analysis EvalContext,
    sample_aliases: &'analysis HashMap<VarName, PeriodicClockSchedule>,
}

fn issue_event_lowering_product<'flat>(
    statements: &'flat [rumoca_core::Statement],
    targets: Vec<VarName>,
    context: EventAnalysisContext<'flat, '_>,
) -> Result<EventLoweringProduct<'flat>, ToDaeError> {
    let statements = issue_event_statement_products(statements, context)?;
    Ok(EventLoweringProduct {
        statements: statements.into_boxed_slice(),
        targets: targets.into_boxed_slice(),
    })
}

fn issue_event_statement_products<'flat>(
    statements: &'flat [rumoca_core::Statement],
    context: EventAnalysisContext<'flat, '_>,
) -> Result<Vec<EventStatementPlan<'flat>>, ToDaeError> {
    statements
        .iter()
        .map(|statement| issue_event_statement_product(statement, context))
        .collect()
}

fn issue_event_statement_product<'flat>(
    statement: &'flat rumoca_core::Statement,
    context: EventAnalysisContext<'flat, '_>,
) -> Result<EventStatementPlan<'flat>, ToDaeError> {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, span } => {
            let function_call = context.calls.analyze_assignment_call(comp, value, *span)?;
            let structured = structured_assignment_plan(context.flat, comp, value);
            let route = match (function_call, structured) {
                (Some(plan), None) => {
                    let Expression::FunctionCall { name, args, .. } = value else {
                        return Err(ToDaeError::unsupported_algorithm(
                            "model",
                            "event call certificate lost its exact source payload",
                            *span,
                        ));
                    };
                    EventAssignmentRoute::FunctionCall {
                        component: name,
                        arguments: args,
                        plan,
                    }
                }
                (None, Some(plan)) => EventAssignmentRoute::Structured(plan),
                (None, None) => EventAssignmentRoute::Coordinate,
                (Some(_), Some(_)) => {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "one event assignment cannot own two lowering plans",
                        *span,
                    ));
                }
            };
            Ok(EventStatementPlan::Assignment {
                component: comp,
                value,
                span: *span,
                route,
            })
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            span,
        } => {
            let blocks = issue_event_block_products(cond_blocks, context)?;
            let else_product = match else_block {
                Some(statements) => EventElseProduct::Statements(
                    issue_event_statement_products(statements, context)?.into_boxed_slice(),
                ),
                None => EventElseProduct::Absent,
            };
            Ok(EventStatementPlan::If {
                blocks,
                else_product,
                span: *span,
            })
        }
        rumoca_core::Statement::When { blocks, span } => Ok(EventStatementPlan::When {
            blocks: issue_event_block_products(blocks, context)?,
            span: *span,
        }),
        rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } => {
            let plan = context.calls.analyze_call(comp, args, outputs, *span)?;
            Ok(EventStatementPlan::FunctionCall {
                component: comp,
                arguments: args,
                span: *span,
                plan,
            })
        }
        rumoca_core::Statement::For { .. } => {
            analyze_event_tensor_loop(context.flat, statement, context.model_values)
                .map(EventStatementPlan::TensorLoop)
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            span,
        } => Ok(EventStatementPlan::Assert {
            condition,
            message,
            level: level.as_deref(),
            span: *span,
        }),
        _ => Err(ToDaeError::unsupported_algorithm(
            "model",
            "event plan received a statement outside the checked grammar",
            required_statement_span(statement, "event algorithm statement")?,
        )),
    }
}

fn issue_event_block_products<'flat>(
    blocks: &'flat [rumoca_core::StatementBlock],
    context: EventAnalysisContext<'flat, '_>,
) -> Result<Box<[EventBlockPlan<'flat>]>, ToDaeError> {
    blocks
        .iter()
        .map(|block| -> Result<EventBlockPlan<'flat>, ToDaeError> {
            Ok(EventBlockPlan {
                condition: issue_algorithm_condition(
                    &block.cond,
                    context.flat,
                    context.roles,
                    context.constants,
                    context.sample_aliases,
                )?,
                statements: issue_event_statement_products(&block.stmts, context)?
                    .into_boxed_slice(),
            })
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Vec::into_boxed_slice)
}

fn analyze_event_tensor_loop<'flat>(
    flat: &'flat flat::Model,
    statement: &'flat rumoca_core::Statement,
    model_values: &ShapeEnvironment,
) -> Result<ModelEventTensorLoopPlan<'flat>, ToDaeError> {
    let rumoca_core::Statement::For {
        indices,
        equations,
        span,
    } = statement
    else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "event tensor-loop analysis requires one checked for statement",
            required_statement_span(statement, "event tensor-loop statement")?,
        ));
    };
    if equations.is_empty() {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "an event tensor loop requires total element assignments",
            *span,
        ));
    }
    let mut assignments = Vec::with_capacity(equations.len());
    let mut dimensions = None;
    let mut targets = HashSet::with_capacity(equations.len());
    for equation in equations {
        let rumoca_core::Statement::Assignment {
            comp, value, span, ..
        } = equation
        else {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "an event tensor loop requires only total element assignments",
                required_statement_span(equation, "event tensor-loop statement")?,
            ));
        };
        let target = assignment_target(comp);
        if !targets.insert(target.clone()) {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                format!("event tensor loop assigns `{target}` more than once"),
                *span,
            ));
        }
        let target_dimensions = &flat.variables[&target].dims;
        validate_event_tensor_target(indices, comp, target_dimensions, model_values, *span)?;
        match &dimensions {
            Some(expected) if expected != target_dimensions => {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "event tensor-loop targets must share one exact domain",
                    *span,
                ));
            }
            None => dimensions = Some(target_dimensions.clone()),
            _ => {}
        }
        assignments.push(ModelEventTensorAssignment {
            target,
            component: comp,
            value,
            span: *span,
        });
    }
    let Some(dimensions) = dimensions else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "an event tensor loop requires total element assignments",
            *span,
        ));
    };
    let (domain, binder_spans) = event_tensor_domain(indices, &dimensions, *span)?;
    let target_set = assignments
        .iter()
        .map(|assignment| assignment.target.clone())
        .collect::<HashSet<_>>();
    let mut available = HashSet::new();
    for assignment in &assignments {
        let Some(last_part) = assignment.component.parts().last() else {
            return Err(ToDaeError::unsupported_algorithm(
                "model",
                "an event tensor-loop target names no component",
                *span,
            ));
        };
        let subscripts = last_part.subs.as_slice();
        validate_tensor_target_reads(assignment.value, &target_set, &available, subscripts)?;
        available.insert(assignment.target.clone());
    }
    Ok(ModelEventTensorLoopPlan {
        domain,
        binder_spans,
        assignments: assignments.into_boxed_slice(),
        span: *span,
    })
}

fn validate_event_tensor_target(
    indices: &[rumoca_core::ForIndex],
    component: &rumoca_core::ComponentReference,
    dimensions: &[i64],
    model_values: &ShapeEnvironment,
    span: Span,
) -> Result<(), ToDaeError> {
    let Some(part) = component.parts().last() else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "event tensor-loop assignment has no checked target",
            span,
        ));
    };
    if dimensions.is_empty()
        || indices.len() != dimensions.len()
        || part.subs.len() != dimensions.len()
    {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "event tensor loop must cover every target axis exactly once",
            span,
        ));
    }
    for ((index, subscript), extent) in indices.iter().zip(&part.subs).zip(dimensions) {
        validate_total_axis(index, subscript, *extent, model_values)?;
    }
    Ok(())
}

fn event_tensor_domain(
    indices: &[rumoca_core::ForIndex],
    dimensions: &[i64],
    span: Span,
) -> Result<(StructuredIndexDomain, Vec<Span>), ToDaeError> {
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for ((index, extent), ordinal) in indices.iter().zip(dimensions).zip(0u32..) {
        binders.push(StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(ordinal),
            display_name: index.ident.clone(),
            lower: 1,
            upper: *extent,
            step: 1,
        });
        binder_spans.push(expression_span(&index.range)?);
    }
    let domain = StructuredIndexDomain { binders };
    domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_algorithm(
            "model",
            format!("event tensor-loop domain is not computable: {error}"),
            span,
        )
    })?;
    Ok((domain, binder_spans))
}

fn validate_tensor_target_reads(
    value: &Expression,
    targets: &HashSet<VarName>,
    available: &HashSet<VarName>,
    expected_subscripts: &[Subscript],
) -> Result<(), ToDaeError> {
    struct CurrentTargetRead<'target> {
        targets: &'target HashSet<VarName>,
        available: &'target HashSet<VarName>,
        expected_subscripts: &'target [Subscript],
        invalid: Option<(VarName, bool)>,
    }

    impl rumoca_core::ExpressionVisitor for CurrentTargetRead<'_> {
        fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
            if !self.targets.contains(name.var_name()) {
                self.walk_var_ref(name, subscripts);
                return;
            }
            let available = self.available.contains(name.var_name());
            let same_element = same_tensor_element(subscripts, self.expected_subscripts);
            if !available || !same_element {
                self.invalid = Some((name.var_name().clone(), available));
            }
            self.walk_var_ref(name, subscripts);
        }

        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if matches!(function, BuiltinFunction::Pre | BuiltinFunction::Previous) {
                return;
            }
            self.walk_builtin_call(function, args);
        }
    }

    let mut proof = CurrentTargetRead {
        targets,
        available,
        expected_subscripts,
        invalid: None,
    };
    rumoca_core::ExpressionVisitor::visit_expression(&mut proof, value);
    if let Some((target, available)) = proof.invalid {
        let detail = if available {
            "reads a different tensor element"
        } else {
            "reads itself or a later tensor target"
        };
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!("event tensor loop {detail} through `{target}`"),
            expression_span(value)?,
        ));
    }
    Ok(())
}

fn same_tensor_element(actual: &[Subscript], expected: &[Subscript]) -> bool {
    actual.len() == expected.len()
        && actual
            .iter()
            .zip(expected)
            .all(|(actual, expected)| match (actual, expected) {
                (Subscript::Expr { expr: actual, .. }, Subscript::Expr { expr: expected, .. }) => {
                    rumoca_core::expressions_semantically_equal(actual, expected)
                }
                _ => false,
            })
}

fn analyze_separated_array_sum<'flat>(
    flat: &flat::Model,
    algorithm: &flat::Algorithm,
    targets: &[VarName],
    roles: &HashMap<VarName, PlannedRole>,
    model_values: &ShapeEnvironment,
) -> Result<Option<ModelAlgorithmPlan<'flat>>, ToDaeError> {
    let Some((array_target, scalar_target)) =
        separated_array_sum_targets(flat, algorithm, targets, roles)?
    else {
        return Ok(None);
    };
    let [
        rumoca_core::Statement::Assignment {
            comp: initial_target,
            value: initial,
            ..
        },
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
    ] = algorithm.statements.as_slice()
    else {
        return Ok(None);
    };
    let [
        rumoca_core::Statement::Assignment {
            comp: array_component,
            value: element,
            ..
        },
        rumoca_core::Statement::Assignment {
            comp: update_target,
            value: update,
            ..
        },
    ] = equations.as_slice()
    else {
        return Ok(None);
    };
    if assignment_target(initial_target) != *scalar_target
        || !is_zero(initial)
        || assignment_target(array_component) != *array_target
        || assignment_target(update_target) != *scalar_target
    {
        return Ok(None);
    }
    let Some(subscripts) = array_component
        .parts()
        .last()
        .map(|part| part.subs.as_slice())
    else {
        return Ok(None);
    };
    let dimensions = &flat.variables[array_target].dims;
    if indices.len() != dimensions.len() || subscripts.len() != dimensions.len() {
        return Ok(None);
    }
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (((index, subscript), extent), ordinal) in
        indices.iter().zip(subscripts).zip(dimensions).zip(0u32..)
    {
        validate_total_axis(index, subscript, *extent, model_values)?;
        let range_span = expression_span(&index.range)?;
        binders.push(StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(ordinal),
            display_name: index.ident.clone(),
            lower: 1,
            upper: *extent,
            step: 1,
        });
        binder_spans.push(range_span);
    }
    reject_read_before_definition(element, array_target, false)?;
    reject_read_before_definition(element, scalar_target, false)?;
    if !is_additive_element_update(update, scalar_target, array_target, subscripts) {
        return Ok(None);
    }
    let domain = StructuredIndexDomain { binders };
    domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_algorithm(
            "model",
            format!("separated array-reduction domain is not computable: {error}"),
            *span,
        )
    })?;
    Ok(Some(ModelAlgorithmPlan::SeparatedArraySum {
        array_target: array_target.clone(),
        scalar_target: scalar_target.clone(),
        domain,
        binder_spans,
    }))
}

fn separated_array_sum_targets<'targets>(
    flat: &flat::Model,
    algorithm: &flat::Algorithm,
    targets: &'targets [VarName],
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<Option<(&'targets VarName, &'targets VarName)>, ToDaeError> {
    let [first, second] = targets else {
        return Ok(None);
    };
    let targets = match (
        flat.variables[first].dims.is_empty(),
        flat.variables[second].dims.is_empty(),
    ) {
        (false, true) => (first, second),
        (true, false) => (second, first),
        _ => return Ok(None),
    };
    if !is_declarative_role(roles[targets.0]) || !is_declarative_role(roles[targets.1]) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "separated array reduction has a non-computable target role",
            algorithm.span,
        ));
    }
    Ok(Some(targets))
}

fn is_declarative_role(role: PlannedRole) -> bool {
    matches!(
        role,
        PlannedRole::Algebraic
            | PlannedRole::Output
            | PlannedRole::DiscreteReal
            | PlannedRole::DiscreteValue
    )
}

fn is_zero(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Literal {
            value: Literal::Integer(0) | Literal::Real(0.0),
            ..
        }
    )
}

fn is_additive_element_update(
    expression: &Expression,
    scalar_target: &VarName,
    array_target: &VarName,
    expected_subscripts: &[Subscript],
) -> bool {
    let Expression::Binary {
        op: OpBinary::Add | OpBinary::AddElem,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return false;
    };
    is_unsubscripted_reference(lhs, scalar_target)
        && is_exact_element_reference(rhs, array_target, expected_subscripts)
}

fn is_unsubscripted_reference(expression: &Expression, target: &VarName) -> bool {
    matches!(
        expression,
        Expression::VarRef {
            name, subscripts, ..
        } if name.var_name() == target && subscripts.is_empty()
    )
}

fn is_exact_element_reference(
    expression: &Expression,
    target: &VarName,
    expected_subscripts: &[Subscript],
) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return false;
    };
    name.var_name() == target
        && subscripts.len() == expected_subscripts.len()
        && subscripts
            .iter()
            .zip(expected_subscripts)
            .all(|(actual, expected)| match (actual, expected) {
                (Subscript::Expr { expr: actual, .. }, Subscript::Expr { expr: expected, .. }) => {
                    rumoca_core::expressions_semantically_equal(actual, expected)
                }
                _ => false,
            })
}

fn analyze_total_array_definition<'flat>(
    algorithm: &flat::Algorithm,
    target: &VarName,
    dimensions: &[i64],
    model_values: &ShapeEnvironment,
) -> Result<ModelAlgorithmPlan<'flat>, ToDaeError> {
    let [
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
    ] = algorithm.statements.as_slice()
    else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "an array algorithm requires one compact total-definition loop",
            algorithm.span,
        ));
    };
    let [rumoca_core::Statement::Assignment { comp, value, .. }] = equations.as_slice() else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "a total array-definition loop requires one element assignment",
            *span,
        ));
    };
    let Some(component) = comp.parts().last() else {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "array loop assignment has no checked target",
            *span,
        ));
    };
    if assignment_target(comp) != *target
        || indices.len() != dimensions.len()
        || component.subs.len() != dimensions.len()
    {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            "array loop must bind every target axis exactly once",
            *span,
        ));
    }
    let mut binders = Vec::with_capacity(indices.len());
    let mut binder_spans = Vec::with_capacity(indices.len());
    for (((index, subscript), extent), ordinal) in indices
        .iter()
        .zip(&component.subs)
        .zip(dimensions)
        .zip(0u32..)
    {
        validate_total_axis(index, subscript, *extent, model_values)?;
        let range_span = expression_span(&index.range)?;
        binders.push(StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(ordinal),
            display_name: index.ident.clone(),
            lower: 1,
            upper: *extent,
            step: 1,
        });
        binder_spans.push(range_span);
    }
    reject_read_before_definition(value, target, false)?;
    let domain = StructuredIndexDomain { binders };
    domain.scalar_count().map_err(|error| {
        ToDaeError::unsupported_algorithm(
            "model",
            format!("array loop domain is not computable: {error}"),
            *span,
        )
    })?;
    Ok(ModelAlgorithmPlan::TotalArrayDefinition {
        target: target.clone(),
        domain,
        binder_spans,
    })
}

fn validate_total_axis(
    index: &rumoca_core::ForIndex,
    subscript: &Subscript,
    extent: i64,
    model_values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    let span = expression_span(&index.range)?;
    let Expression::Range {
        start, step, end, ..
    } = &index.range
    else {
        return Err(invalid_total_axis(
            index,
            "axis requires an explicit range",
            span,
        ));
    };
    let exact_range = settled_integer_value(start, model_values) == Some(1)
        && step
            .as_deref()
            .map(|step| settled_integer_value(step, model_values))
            .unwrap_or(Some(1))
            == Some(1)
        && settled_integer_value(end, model_values) == Some(extent);
    let exact_subscript = matches!(
        subscript,
        Subscript::Expr { expr, .. }
            if matches!(
                expr.as_ref(),
                Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == index.ident && subscripts.is_empty()
            )
    );
    if exact_range && exact_subscript && extent >= 0 {
        Ok(())
    } else {
        Err(invalid_total_axis(
            index,
            "range and subscript must cover one declared array axis exactly",
            span,
        ))
    }
}

fn invalid_total_axis(index: &rumoca_core::ForIndex, detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_algorithm(
        "model",
        format!("loop index `{}`: {detail}", index.ident),
        span,
    )
}

fn integer_value(expression: &Expression) -> Option<i64> {
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn settled_integer_value(expression: &Expression, model_values: &ShapeEnvironment) -> Option<i64> {
    integer_value(expression).or_else(|| model_values.proven_extent(expression))
}

fn validate_declarative_sequence(
    statements: &[rumoca_core::Statement],
    target: &VarName,
    mut assigned: bool,
) -> Result<bool, ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, span } => {
                let written = assignment_target(comp);
                if &written != target || comp.parts().iter().any(|part| !part.subs.is_empty()) {
                    return Err(ToDaeError::unsupported_algorithm(
                        "model",
                        "declarative scalar algorithm assignment escaped its checked target",
                        *span,
                    ));
                }
                reject_read_before_definition(value, target, assigned)?;
                assigned = true;
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                let mut exits = Vec::with_capacity(cond_blocks.len() + 1);
                for block in cond_blocks {
                    reject_read_before_definition(&block.cond, target, assigned)?;
                    exits.push(validate_declarative_sequence(
                        &block.stmts,
                        target,
                        assigned,
                    )?);
                }
                exits.push(match else_block {
                    Some(fallback) => validate_declarative_sequence(fallback, target, assigned)?,
                    None => assigned,
                });
                assigned = exits.into_iter().all(std::convert::identity);
            }
            _ => {
                let span = required_statement_span(
                    statement,
                    "unsupported declarative model algorithm statement",
                )?;
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "declarative algorithm requires scalar assignments and conditionals",
                    span,
                ));
            }
        }
    }
    Ok(assigned)
}

fn reject_read_before_definition(
    expression: &Expression,
    target: &VarName,
    assigned: bool,
) -> Result<(), ToDaeError> {
    if assigned {
        return Ok(());
    }
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    if references.iter().any(|reference| reference == target) {
        return Err(ToDaeError::unsupported_algorithm(
            "model",
            format!(
                "`{target}` is read before definition; checked start/pre initialization is required"
            ),
            expression_span(expression)?,
        ));
    }
    Ok(())
}

pub(in crate::construction) fn event_targets(flat: &flat::Model) -> HashSet<VarName> {
    let mut written = when_chain_targets(flat);
    for algorithm in &flat.algorithms {
        collect_event_control_targets(&algorithm.statements, &mut written);
    }
    resolve_written_targets(flat, written)
}

pub(in crate::construction) fn when_chain_targets(flat: &flat::Model) -> HashSet<VarName> {
    let mut written = HashSet::new();
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            collect_when_equation_targets(&branch.equations, &mut written);
        }
    }
    resolve_written_targets(flat, written)
}

pub(in crate::construction) fn algorithm_targets(flat: &flat::Model) -> HashSet<VarName> {
    flat.algorithms
        .iter()
        .flat_map(|algorithm| model_algorithm_targets(flat, algorithm))
        .collect()
}

pub(in crate::construction) fn model_algorithm_targets(
    flat: &flat::Model,
    algorithm: &flat::Algorithm,
) -> Vec<VarName> {
    let mut written = HashSet::new();
    collect_statement_targets(&algorithm.statements, &mut written);
    let mut targets = resolve_written_targets(flat, written)
        .into_iter()
        .collect::<Vec<_>>();
    targets.sort_by(|left, right| left.as_str().cmp(right.as_str()));
    targets
}

fn collect_statement_targets(
    statements: &[rumoca_core::Statement],
    targets: &mut HashSet<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::Assignment { comp, .. } if !comp.parts().is_empty() => {
                targets.insert(assignment_target(comp));
            }
            rumoca_core::Statement::FunctionCall { outputs, .. } => {
                targets.extend(outputs.iter().flatten().map(|output| output.to_var_name()));
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_statement_targets(equations, targets);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_statement_targets(&block.stmts, targets);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_statement_targets(&block.stmts, targets);
                }
                if let Some(fallback) = else_block {
                    collect_statement_targets(fallback, targets);
                }
            }
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_statement_targets(&block.stmts, targets);
                }
            }
            _ => {}
        }
    }
}

fn contains_event_control(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::When { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_event_control(equations),
        rumoca_core::Statement::While { block, .. } => contains_event_control(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                is_event_condition(&block.cond) || contains_event_control(&block.stmts)
            }) || else_block.as_deref().is_some_and(contains_event_control)
        }
        _ => false,
    })
}

pub(in crate::construction) fn is_event_condition(expression: &Expression) -> bool {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Change | BuiltinFunction::Sample,
            ..
        } => true,
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => is_event_condition(rhs),
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            lhs,
            rhs,
            ..
        } => is_event_condition(lhs) || is_event_condition(rhs),
        _ => false,
    }
}

fn collect_event_control_targets(
    statements: &[rumoca_core::Statement],
    targets: &mut HashSet<VarName>,
) {
    for statement in statements {
        match statement {
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_statement_targets(&block.stmts, targets);
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_event_control_targets(equations, targets);
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_event_control_targets(&block.stmts, targets);
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => collect_conditional_event_targets(cond_blocks, else_block.as_deref(), targets),
            _ => {}
        }
    }
}

fn collect_conditional_event_targets(
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    targets: &mut HashSet<VarName>,
) {
    let event_control = blocks.iter().any(|block| is_event_condition(&block.cond));
    for statements in blocks
        .iter()
        .map(|block| block.stmts.as_slice())
        .chain(fallback)
    {
        if event_control {
            collect_statement_targets(statements, targets);
        } else {
            collect_event_control_targets(statements, targets);
        }
    }
}

fn collect_when_equation_targets(equations: &[flat::WhenEquation], targets: &mut HashSet<VarName>) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, .. } => {
                targets.insert(target.clone());
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    collect_when_equation_targets(equations, targets);
                }
                if let Some(else_branch) = else_branch {
                    collect_when_equation_targets(else_branch, targets);
                }
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, .. } => {
                targets.extend(outputs.iter().cloned());
            }
            flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. } => {}
        }
    }
}

fn resolve_written_targets(flat: &flat::Model, written: HashSet<VarName>) -> HashSet<VarName> {
    let mut targets = HashSet::new();
    for target in written {
        if flat.variables.contains_key(&target) {
            targets.insert(target);
            continue;
        }
        let prefix = format!("{target}.");
        targets.extend(
            flat.variables
                .keys()
                .filter(|name| name.as_str().starts_with(&prefix))
                .cloned(),
        );
    }
    targets
}

fn assignment_target(component: &rumoca_core::ComponentReference) -> VarName {
    rumoca_core::component_ref_to_base_reference(component)
        .var_name()
        .clone()
}
