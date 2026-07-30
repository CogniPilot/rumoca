use super::*;

#[cfg(test)]
#[path = "function_shapes/tests/missing_provenance.rs"]
mod provenance_tests;

pub(super) type ValueShape = Vec<u32>;
pub(super) type ShapeEnvironment = HashMap<VarName, ValueShape>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct FunctionSpecializationKey {
    pub(super) function: VarName,
    pub(super) inputs: Vec<ValueShape>,
}

#[derive(Clone, Debug)]
pub(super) struct FunctionShapeCertificate {
    pub(super) key: FunctionSpecializationKey,
    pub(super) parameters: Vec<ValueShape>,
    pub(super) results: Vec<ValueShape>,
    pub(super) values: ShapeEnvironment,
}

pub(super) struct FunctionShapeAnalysis {
    model_values: ShapeEnvironment,
    certificates: Vec<FunctionShapeCertificate>,
    certificate_by_key: HashMap<FunctionSpecializationKey, usize>,
    dependencies: Vec<Vec<usize>>,
    constructor_names: HashSet<VarName>,
    constructor_fields_by_key: HashMap<FunctionSpecializationKey, Vec<ValueShape>>,
}

impl FunctionShapeAnalysis {
    pub(super) fn analyze(flat: &flat::Model) -> Result<Self, ToDaeError> {
        let model_values = concrete_model_shapes(flat)?;
        let constructor_names = flat
            .functions
            .values()
            .filter(|function| function.is_constructor)
            .map(|function| function.name.clone())
            .collect();
        let mut analyzer = ShapeAnalyzer {
            flat,
            analysis: Self {
                model_values,
                certificates: Vec::new(),
                certificate_by_key: HashMap::new(),
                dependencies: Vec::new(),
                constructor_names,
                constructor_fields_by_key: HashMap::new(),
            },
            active_specializations: Vec::new(),
        };
        analyzer.discover_model_calls()?;
        Ok(analyzer.analysis)
    }

    pub(super) fn model_values(&self) -> &ShapeEnvironment {
        &self.model_values
    }

    pub(super) fn certificates(&self) -> &[FunctionShapeCertificate] {
        &self.certificates
    }

    pub(super) fn construction_components(&self) -> Vec<rumoca_core::DependencyScc> {
        rumoca_core::dependency_first_sccs(&self.dependencies)
            .expect("function shape dependencies reference known certificates")
    }

    pub(super) fn constructor_field_shapes(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
    ) -> Option<&[ValueShape]> {
        let inputs = arguments
            .iter()
            .map(|argument| self.expression_shape(argument, values))
            .collect::<Result<Vec<_>, _>>()
            .ok()?;
        self.constructor_fields_by_key
            .get(&FunctionSpecializationKey {
                function: name.var_name().clone(),
                inputs,
            })
            .map(Vec::as_slice)
    }

    pub(super) fn certificate(
        &self,
        key: &FunctionSpecializationKey,
    ) -> Option<&FunctionShapeCertificate> {
        self.certificate_by_key
            .get(key)
            .map(|index| &self.certificates[*index])
    }

    pub(super) fn call_key(
        &self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        values: &ShapeEnvironment,
        span: Span,
    ) -> Result<FunctionSpecializationKey, ToDaeError> {
        let inputs = arguments
            .iter()
            .map(|argument| self.expression_shape(argument, values))
            .collect::<Result<Vec<_>, _>>()?;
        let key = FunctionSpecializationKey {
            function: name.var_name().clone(),
            inputs,
        };
        self.certificate(&key).ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape specialization",
                format!(
                    "`{}` has no constructor-proven specialization for this call signature",
                    name.as_str()
                ),
                span,
            )
        })?;
        Ok(key)
    }

    pub(super) fn expression_shape(
        &self,
        expression: &Expression,
        values: &ShapeEnvironment,
    ) -> Result<ValueShape, ToDaeError> {
        let mut resolve = |name: &rumoca_core::Reference, arguments: &[Expression], span: Span| {
            if self.constructor_names.contains(name.var_name()) {
                return Ok(Vec::new());
            }
            let key = self.call_key(name, arguments, values, span)?;
            self.certificate(&key)
                .and_then(|certificate| certificate.results.first())
                .cloned()
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function result shape",
                        format!("`{}` has no first result", name.as_str()),
                        span,
                    )
                })
        };
        expression_shape(expression, values, &mut resolve)
    }
}

struct ShapeAnalyzer<'flat> {
    flat: &'flat flat::Model,
    analysis: FunctionShapeAnalysis,
    active_specializations: Vec<usize>,
}

impl ShapeAnalyzer<'_> {
    fn discover_model_calls(&mut self) -> Result<(), ToDaeError> {
        let values = self.analysis.model_values.clone();
        for expression in all_model_expressions(self.flat) {
            self.discover_calls(expression, &values)?;
        }
        for algorithm in self
            .flat
            .algorithms
            .iter()
            .chain(&self.flat.initial_algorithms)
        {
            self.discover_statements(&algorithm.statements, &values)?;
        }
        for chain in &self.flat.when_chains {
            for branch in chain.branches() {
                self.discover_calls(&branch.condition, &values)?;
                self.discover_when_equations(&branch.equations, &values)?;
            }
        }
        for assertion in self
            .flat
            .assert_equations
            .iter()
            .chain(&self.flat.initial_assert_equations)
        {
            self.discover_calls(&assertion.condition, &values)?;
            self.discover_calls(&assertion.message, &values)?;
            if let Some(level) = &assertion.level {
                self.discover_calls(level, &values)?;
            }
        }
        Ok(())
    }

    fn discover_calls(
        &mut self,
        expression: &Expression,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        if matches!(expression, Expression::FunctionCall { .. }) {
            self.discover_expression(expression, values)?;
            return Ok(());
        }
        for child in expression_children(expression) {
            self.discover_calls(child, values)?;
        }
        Ok(())
    }

    fn discover_expression(
        &mut self,
        expression: &Expression,
        values: &ShapeEnvironment,
    ) -> Result<ValueShape, ToDaeError> {
        if let Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            span,
        } = expression
            && !name.as_str().starts_with("__rumoca_named_arg__.")
        {
            return self.discover_constructor(name, args, *span, values);
        }
        if let Expression::FunctionCall {
            name, args, span, ..
        } = expression
            && enumeration_conversion(self.flat, name, args, *span)?.is_some()
        {
            // MLS §4.9.5.2: the conversion yields one enumeration value, and its
            // Integer ordinal is already proven constant by the recognizer.
            return Ok(Vec::new());
        }
        let mut resolve = |name: &rumoca_core::Reference, arguments: &[Expression], span: Span| {
            let inputs = arguments
                .iter()
                .map(|argument| self.discover_expression(argument, values))
                .collect::<Result<Vec<_>, _>>()?;
            let key = FunctionSpecializationKey {
                function: name.var_name().clone(),
                inputs,
            };
            let index = self.ensure_specialization(key, span)?;
            self.analysis.certificates[index]
                .results
                .first()
                .cloned()
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function result shape",
                        format!("`{}` has no first result", name.as_str()),
                        span,
                    )
                })
        };
        expression_shape(expression, values, &mut resolve)
    }

    fn discover_constructor(
        &mut self,
        name: &rumoca_core::Reference,
        arguments: &[Expression],
        span: Span,
        values: &ShapeEnvironment,
    ) -> Result<ValueShape, ToDaeError> {
        let constructor = self
            .flat
            .functions
            .get(name.var_name())
            .ok_or_else(|| ToDaeError::unresolved_reference(name.as_str(), span))?;
        if !constructor.is_constructor {
            return Err(ToDaeError::unsupported_flat(
                "record constructor",
                format!("`{}` is not constructor metadata", name.as_str()),
                span,
            ));
        }
        if constructor.inputs.len() != arguments.len() {
            return Err(ToDaeError::unsupported_flat(
                "record constructor",
                format!(
                    "`{}` expects {} fields but receives {}",
                    name.as_str(),
                    constructor.inputs.len(),
                    arguments.len()
                ),
                span,
            ));
        }
        let mut inputs = Vec::with_capacity(arguments.len());
        let mut fields = Vec::with_capacity(arguments.len());
        for (parameter, argument) in constructor.inputs.iter().zip(arguments) {
            let actual = self.discover_expression(argument, values)?;
            inputs.push(actual.clone());
            fields.push(resolve_declared_shape(parameter, Some(&actual), values)?);
        }
        let key = FunctionSpecializationKey {
            function: name.var_name().clone(),
            inputs,
        };
        if let Some(previous) = self
            .analysis
            .constructor_fields_by_key
            .insert(key, fields.clone())
            && previous != fields
        {
            return Err(ToDaeError::unsupported_flat(
                "record constructor",
                "one constructor specialization resolved to inconsistent field shapes",
                span,
            ));
        }
        Ok(Vec::new())
    }

    fn ensure_specialization(
        &mut self,
        key: FunctionSpecializationKey,
        call_span: Span,
    ) -> Result<usize, ToDaeError> {
        let caller = self.active_specializations.last().copied();
        if let Some(index) = self.analysis.certificate_by_key.get(&key).copied() {
            self.record_dependency(caller, index);
            return Ok(index);
        }
        let function =
            self.flat.functions.get(&key.function).ok_or_else(|| {
                ToDaeError::unresolved_reference(key.function.as_str(), call_span)
            })?;
        let certificate = resolve_certificate(
            self.flat,
            function,
            key.clone(),
            call_span,
            &self.analysis.model_values,
        )?;
        let index = self.analysis.certificates.len();
        self.analysis.certificate_by_key.insert(key, index);
        self.analysis.certificates.push(certificate);
        self.analysis.dependencies.push(Vec::new());
        self.record_dependency(caller, index);

        let values = self.analysis.certificates[index].values.clone();
        self.active_specializations.push(index);
        let result = (|| {
            self.discover_parameter_defaults(function, &values)?;
            self.discover_statements(&function.body, &values)
        })();
        let completed = self.active_specializations.pop();
        debug_assert_eq!(completed, Some(index));
        result?;
        Ok(index)
    }

    fn discover_parameter_defaults(
        &mut self,
        function: &rumoca_core::Function,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        let parameters = function
            .inputs
            .iter()
            .chain(&function.outputs)
            .chain(&function.locals);
        for default in parameters.filter_map(|parameter| parameter.default.as_ref()) {
            self.discover_calls(default, values)?;
        }
        Ok(())
    }

    fn record_dependency(&mut self, caller: Option<usize>, dependency: usize) {
        let Some(caller) = caller else {
            return;
        };
        let dependencies = &mut self.analysis.dependencies[caller];
        if !dependencies.contains(&dependency) {
            dependencies.push(dependency);
        }
    }

    fn discover_statements(
        &mut self,
        statements: &[rumoca_core::Statement],
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for statement in statements {
            self.discover_statement(statement, values)?;
        }
        Ok(())
    }

    fn discover_statement(
        &mut self,
        statement: &rumoca_core::Statement,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                self.discover_component_subscripts(comp, values)?;
                self.discover_calls(value, values)
            }
            rumoca_core::Statement::For {
                indices, equations, ..
            } => {
                let mut loop_values = values.clone();
                for index in indices {
                    self.discover_calls(&index.range, &loop_values)?;
                    loop_values.insert(VarName::new(&index.ident), Vec::new());
                }
                self.discover_statements(equations, &loop_values)
            }
            rumoca_core::Statement::While { block, .. } => {
                self.discover_calls(&block.cond, values)?;
                self.discover_statements(&block.stmts, values)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                self.discover_statement_blocks(cond_blocks, values)?;
                self.discover_statements(else_block.as_deref().unwrap_or_default(), values)
            }
            rumoca_core::Statement::When { blocks, .. } => {
                self.discover_statement_blocks(blocks, values)
            }
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            } => {
                let inputs = args
                    .iter()
                    .map(|argument| self.discover_expression(argument, values))
                    .collect::<Result<Vec<_>, _>>()?;
                self.ensure_specialization(
                    FunctionSpecializationKey {
                        function: comp.to_var_name(),
                        inputs,
                    },
                    *span,
                )?;
                Ok(())
            }
            rumoca_core::Statement::Reinit {
                variable, value, ..
            } => {
                self.discover_component_subscripts(variable, values)?;
                self.discover_calls(value, values)
            }
            rumoca_core::Statement::Assert {
                condition,
                message,
                level,
                ..
            } => {
                self.discover_calls(condition, values)?;
                self.discover_calls(message, values)?;
                match level {
                    Some(level) => self.discover_calls(level, values),
                    None => Ok(()),
                }
            }
            rumoca_core::Statement::Empty { .. }
            | rumoca_core::Statement::Return { .. }
            | rumoca_core::Statement::Break { .. } => Ok(()),
        }
    }

    fn discover_statement_blocks(
        &mut self,
        blocks: &[rumoca_core::StatementBlock],
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for block in blocks {
            self.discover_calls(&block.cond, values)?;
            self.discover_statements(&block.stmts, values)?;
        }
        Ok(())
    }

    fn discover_component_subscripts(
        &mut self,
        component: &rumoca_core::ComponentReference,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for subscript in component.parts().iter().flat_map(|part| part.subs.iter()) {
            if let Subscript::Expr { expr, .. } = subscript {
                self.discover_calls(expr.as_ref(), values)?;
            }
        }
        Ok(())
    }

    fn discover_when_equations(
        &mut self,
        equations: &[flat::WhenEquation],
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        for equation in equations {
            self.discover_when_equation(equation, values)?;
        }
        Ok(())
    }

    fn discover_when_equation(
        &mut self,
        equation: &flat::WhenEquation,
        values: &ShapeEnvironment,
    ) -> Result<(), ToDaeError> {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                self.discover_calls(value, values)
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                self.discover_calls(condition, values)?;
                self.discover_calls(message, values)?;
                if let Some(level) = level {
                    self.discover_calls(level, values)?;
                }
                Ok(())
            }
            flat::WhenEquation::Terminate { message, .. } => self.discover_calls(message, values),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, equations) in branches {
                    self.discover_calls(condition, values)?;
                    self.discover_when_equations(equations, values)?;
                }
                if let Some(else_branch) = else_branch {
                    self.discover_when_equations(else_branch, values)?;
                }
                Ok(())
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                self.discover_calls(function, values)
            }
        }
    }
}

fn concrete_model_shapes(flat: &flat::Model) -> Result<ShapeEnvironment, ToDaeError> {
    let mut values = flat
        .variables
        .iter()
        .map(|(name, variable)| {
            concrete_dimensions(&variable.dims, variable.source_span, "model variable")
                .map(|shape| (name.clone(), shape))
        })
        .collect::<Result<ShapeEnvironment, _>>()?;
    values.insert(VarName::new("time"), Vec::new());
    values.extend(
        flat.enum_literal_ordinals
            .keys()
            .map(|name| (VarName::new(name), Vec::new())),
    );
    Ok(values)
}

fn concrete_dimensions(
    dimensions: &[i64],
    span: Span,
    owner: &'static str,
) -> Result<ValueShape, ToDaeError> {
    dimensions
        .iter()
        .map(|extent| {
            u32::try_from(*extent).ok().ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    format!("{owner} has non-concrete extent `{extent}`"),
                    span,
                )
            })
        })
        .collect()
}

fn resolve_certificate(
    flat: &flat::Model,
    function: &rumoca_core::Function,
    key: FunctionSpecializationKey,
    call_span: Span,
    global_values: &ShapeEnvironment,
) -> Result<FunctionShapeCertificate, ToDaeError> {
    require_span(call_span, "function specialization call")?;
    if key.inputs.len() != function.inputs.len() {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!(
                "`{}` expects {} input shapes but receives {}",
                function.name,
                function.inputs.len(),
                key.inputs.len()
            ),
            call_span,
        ));
    }
    let mut values = global_values.clone();
    let mut parameters = Vec::with_capacity(function.inputs.len());
    for (parameter, actual) in function.inputs.iter().zip(&key.inputs) {
        let shape = resolve_declared_shape(parameter, Some(actual), &values)?;
        values.insert(VarName::new(&parameter.name), shape.clone());
        parameters.push(shape);
    }
    let mut results = Vec::with_capacity(function.outputs.len());
    for result in &function.outputs {
        let shape = resolve_declared_shape(result, None, &values)?;
        values.insert(VarName::new(&result.name), shape.clone());
        results.push(shape);
    }
    for local in &function.locals {
        let shape = resolve_declared_shape(local, None, &values)?;
        values.insert(VarName::new(&local.name), shape);
    }
    // MLS §12.2: a record value's declared fields are readable through the
    // joined reference identity Flat renders, so each field carries its own
    // proven shape in the same environment as the value that declares it.
    for value in function
        .inputs
        .iter()
        .chain(&function.outputs)
        .chain(&function.locals)
    {
        for (path, field) in record_field_projections(value, flat) {
            let shape = resolve_declared_shape(field, None, &values)?;
            values.insert(path, shape);
        }
    }
    Ok(FunctionShapeCertificate {
        key,
        parameters,
        results,
        values,
    })
}

fn resolve_declared_shape(
    value: &rumoca_core::FunctionParam,
    actual: Option<&ValueShape>,
    values: &ShapeEnvironment,
) -> Result<ValueShape, ToDaeError> {
    if let Some(actual) = actual
        && actual.len() != value.dimensions().len()
    {
        return Err(shape_error(
            value,
            format!(
                "declared rank {} does not match call-site rank {}",
                value.dimensions().len(),
                actual.len()
            ),
        ));
    }
    let mut shape = Vec::with_capacity(value.dimensions().len());
    for (axis, declared) in value.dimensions().iter().copied().enumerate() {
        let source = value.shape_expr.get(axis);
        let resolved = if declared > 0 {
            u32::try_from(declared).map_err(|_| {
                shape_error(
                    value,
                    format!("extent `{declared}` exceeds the DAE shape domain"),
                )
            })?
        } else {
            match source {
                Some(Subscript::Colon { .. }) => actual
                    .and_then(|shape| shape.get(axis))
                    .copied()
                    .ok_or_else(|| {
                        shape_error(
                            value,
                            format!(
                                "axis {} is variable-size but has no call-site equality",
                                axis + 1
                            ),
                        )
                    })?,
                Some(Subscript::Index { value: extent, .. }) => {
                    concrete_extent(*extent, value, axis)?
                }
                Some(Subscript::Expr { expr, .. }) => {
                    let extent = evaluate_shape_integer(expr, values)?;
                    concrete_extent(extent, value, axis)?
                }
                None => {
                    return Err(shape_error(
                        value,
                        format!(
                            "axis {} uses a dynamic sentinel without a symbolic declaration",
                            axis + 1
                        ),
                    ));
                }
            }
        };
        if let Some(actual) = actual
            && actual[axis] != resolved
        {
            return Err(shape_error(
                value,
                format!(
                    "axis {} requires extent {resolved} but the call site proves {}",
                    axis + 1,
                    actual[axis]
                ),
            ));
        }
        shape.push(resolved);
    }
    Ok(shape)
}

fn concrete_extent(
    extent: i64,
    value: &rumoca_core::FunctionParam,
    axis: usize,
) -> Result<u32, ToDaeError> {
    u32::try_from(extent).ok().ok_or_else(|| {
        shape_error(
            value,
            format!("axis {} resolves to invalid extent `{extent}`", axis + 1),
        )
    })
}

fn shape_error(value: &rumoca_core::FunctionParam, detail: impl Into<String>) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function shape proof",
        format!("`{}`: {}", value.name, detail.into()),
        value.span,
    )
}

pub(super) fn evaluate_shape_integer(
    expression: &Expression,
    values: &ShapeEnvironment,
) -> Result<i64, ToDaeError> {
    let span = expression_span(expression)?;
    match expression {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Ok(*value),
        Expression::BuiltinCall {
            function: BuiltinFunction::Size,
            args,
            ..
        } => {
            let [array, dimension] = args.as_slice() else {
                return Err(ToDaeError::unsupported_flat(
                    "function shape proof",
                    "a dependent extent requires size(value, literal_axis)",
                    span,
                ));
            };
            let axis = evaluate_shape_integer(dimension, values)?;
            let axis = usize::try_from(axis)
                .ok()
                .and_then(|axis| axis.checked_sub(1))
                .ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function shape proof",
                        "size axis must be a positive integer",
                        span,
                    )
                })?;
            let shape = expression_shape(array, values, &mut reject_shape_call)?;
            shape.get(axis).copied().map(i64::from).ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    format!("size axis {} exceeds rank {}", axis + 1, shape.len()),
                    span,
                )
            })
        }
        Expression::Unary {
            op: OpUnary::Plus,
            rhs,
            ..
        } => evaluate_shape_integer(rhs, values),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => evaluate_shape_integer(rhs, values)?
            .checked_neg()
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    "dependent extent arithmetic overflowed",
                    span,
                )
            }),
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = evaluate_shape_integer(lhs, values)?;
            let rhs = evaluate_shape_integer(rhs, values)?;
            checked_shape_arithmetic(op.clone(), lhs, rhs, span)
        }
        // A scalar coordinate proven by this environment carries a shape but no
        // value, and an extent written over it needs the value. Naming that
        // cause keeps the rejection honest: the missing owner is a
        // value-proven specialization, not a malformed extent expression.
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() && values.get(name.var_name()).is_some_and(Vec::is_empty) => {
            Err(ToDaeError::unsupported_flat(
                "function shape proof",
                format!(
                    "extent depends on the value of scalar `{}`, which requires a \
                     value-proven function specialization",
                    name.as_str()
                ),
                span,
            ))
        }
        _ => Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "dependent extent is not an exact Integer expression over proven shape axes",
            span,
        )),
    }
}

fn checked_shape_arithmetic(
    operator: OpBinary,
    lhs: i64,
    rhs: i64,
    span: Span,
) -> Result<i64, ToDaeError> {
    let result = match operator {
        OpBinary::Add | OpBinary::AddElem => lhs.checked_add(rhs),
        OpBinary::Sub | OpBinary::SubElem => lhs.checked_sub(rhs),
        OpBinary::Mul | OpBinary::MulElem => lhs.checked_mul(rhs),
        OpBinary::Div | OpBinary::DivElem if rhs != 0 && lhs % rhs == 0 => lhs.checked_div(rhs),
        _ => None,
    };
    result.ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "function shape proof",
            "dependent extent arithmetic is non-integral, unsupported, or overflowing",
            span,
        )
    })
}

type FunctionResultShape<'scope> = dyn FnMut(&rumoca_core::Reference, &[Expression], Span) -> Result<ValueShape, ToDaeError>
    + 'scope;

fn expression_shape(
    expression: &Expression,
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
) -> Result<ValueShape, ToDaeError> {
    let span = expression_span(expression)?;
    match expression {
        Expression::Literal { .. } => Ok(Vec::new()),
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let shape = values
                .get(name.var_name())
                .cloned()
                .ok_or_else(|| ToDaeError::unresolved_reference(name.as_str(), span))?;
            apply_subscripts(shape, subscripts, values)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let shape = expression_shape(base, values, function_result)?;
            apply_subscripts(shape, subscripts, values)
        }
        Expression::Unary { rhs, .. } => expression_shape(rhs, values, function_result),
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = expression_shape(lhs, values, function_result)?;
            let rhs = expression_shape(rhs, values, function_result)?;
            binary_shape(op.clone(), lhs, rhs, span)
        }
        Expression::BuiltinCall { function, args, .. } => {
            builtin_shape(*function, args, values, function_result, span)
        }
        Expression::StringConversion { value, format, .. } => {
            string_conversion_shape(value, format, values, function_result, span)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } if *is_constructor && name.as_str().starts_with("__rumoca_named_arg__.") => {
            let [value] = args.as_slice() else {
                return Err(ToDaeError::unsupported_flat(
                    "function shape proof",
                    "named argument wrapper must contain one value",
                    span,
                ));
            };
            expression_shape(value, values, function_result)
        }
        Expression::FunctionCall { name, args, .. } => function_result(name, args, span),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let expected = expression_shape(else_branch, values, function_result)?;
            for (_, value) in branches {
                let found = expression_shape(value, values, function_result)?;
                require_same_shape(&expected, &found, span)?;
            }
            Ok(expected)
        }
        Expression::Array { elements, .. } => {
            array_expression_shape(elements, values, function_result, span)
        }
        Expression::Range {
            start, step, end, ..
        } => range_expression_shape(start, step.as_deref(), end, values, span),
        Expression::Tuple { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::FieldAccess { .. }
        | Expression::Empty { .. } => Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "expression form has no exact checked shape rule",
            span,
        )),
    }
}

fn string_conversion_shape(
    value: &Expression,
    format: &rumoca_core::StringConversionFormat,
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    if !expression_shape(value, values, function_result)?.is_empty() {
        return shape_mismatch(span);
    }
    for operand in format.operands() {
        if !expression_shape(operand, values, function_result)?.is_empty() {
            return shape_mismatch(span);
        }
    }
    Ok(Vec::new())
}

fn array_expression_shape(
    elements: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let Some(first) = elements.first() else {
        return Ok(vec![0]);
    };
    let child = expression_shape(first, values, function_result)?;
    for element in &elements[1..] {
        let found = expression_shape(element, values, function_result)?;
        require_same_shape(&child, &found, span)?;
    }
    let count = u32::try_from(elements.len()).map_err(|_| {
        ToDaeError::unsupported_flat(
            "function shape proof",
            "array element count exceeds the DAE shape domain",
            span,
        )
    })?;
    Ok(std::iter::once(count).chain(child).collect())
}

fn range_expression_shape(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    values: &ShapeEnvironment,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let start = evaluate_shape_integer(start, values)?;
    let step = step
        .map(|step| evaluate_shape_integer(step, values))
        .transpose()?
        .unwrap_or(1);
    let end = evaluate_shape_integer(end, values)?;
    Ok(vec![range_cardinality(start, step, end, span)?])
}

fn reject_shape_call(
    name: &rumoca_core::Reference,
    _arguments: &[Expression],
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    Err(ToDaeError::unsupported_flat(
        "function shape proof",
        format!(
            "dependent extents cannot call runtime function `{}`",
            name.as_str()
        ),
        span,
    ))
}

fn builtin_shape(
    function: BuiltinFunction,
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    match function {
        BuiltinFunction::Size if arguments.len() == 1 => {
            let rank = expression_shape(&arguments[0], values, function_result)?.len();
            Ok(vec![u32::try_from(rank).map_err(|_| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    "rank exceeds the DAE shape domain",
                    span,
                )
            })?])
        }
        BuiltinFunction::Size | BuiltinFunction::Sum | BuiltinFunction::Product => Ok(Vec::new()),
        BuiltinFunction::Zeros => arguments
            .iter()
            .map(|argument| {
                let extent = evaluate_shape_integer(argument, values)?;
                u32::try_from(extent).ok().ok_or_else(|| {
                    ToDaeError::unsupported_flat(
                        "function shape proof",
                        format!("zeros extent `{extent}` is invalid"),
                        span,
                    )
                })
            })
            .collect(),
        BuiltinFunction::Smooth => arguments
            .get(1)
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    "smooth requires two arguments",
                    span,
                )
            })
            .and_then(|value| expression_shape(value, values, function_result)),
        BuiltinFunction::Integer => scalar_integer_shape(arguments, values, function_result, span),
        BuiltinFunction::Min | BuiltinFunction::Max if arguments.len() == 1 => Ok(Vec::new()),
        BuiltinFunction::Der
        | BuiltinFunction::Pre
        | BuiltinFunction::Sample
        | BuiltinFunction::Clock
        | BuiltinFunction::Hold
        | BuiltinFunction::Previous
        | BuiltinFunction::SubSample
        | BuiltinFunction::SuperSample
        | BuiltinFunction::ShiftSample
        | BuiltinFunction::BackSample
        | BuiltinFunction::NoClock
        | BuiltinFunction::Abs
        | BuiltinFunction::Sign
        | BuiltinFunction::Sqrt
        | BuiltinFunction::Floor
        | BuiltinFunction::Ceil
        | BuiltinFunction::Sin
        | BuiltinFunction::Cos
        | BuiltinFunction::Tan
        | BuiltinFunction::Asin
        | BuiltinFunction::Acos
        | BuiltinFunction::Atan
        | BuiltinFunction::Sinh
        | BuiltinFunction::Cosh
        | BuiltinFunction::Tanh
        | BuiltinFunction::Exp
        | BuiltinFunction::Log
        | BuiltinFunction::Log10
        | BuiltinFunction::NoEvent => arguments
            .first()
            .ok_or_else(|| {
                ToDaeError::unsupported_flat(
                    "function shape proof",
                    format!("{} requires an argument", function.name()),
                    span,
                )
            })
            .and_then(|value| expression_shape(value, values, function_result)),
        BuiltinFunction::Interval => scalar_interval_shape(arguments, span),
        BuiltinFunction::Atan2
        | BuiltinFunction::Mod
        | BuiltinFunction::Min
        | BuiltinFunction::Max => {
            let Some(first) = arguments.first() else {
                return Err(ToDaeError::unsupported_flat(
                    "function shape proof",
                    format!("{} requires arguments", function.name()),
                    span,
                ));
            };
            let expected = expression_shape(first, values, function_result)?;
            for argument in &arguments[1..] {
                let found = expression_shape(argument, values, function_result)?;
                require_same_shape(&expected, &found, span)?;
            }
            Ok(expected)
        }
        _ => Err(ToDaeError::unsupported_flat(
            "function shape proof",
            format!("{} has no exact checked shape rule", function.name()),
            span,
        )),
    }
}

fn scalar_interval_shape(arguments: &[Expression], span: Span) -> Result<ValueShape, ToDaeError> {
    if arguments.len() > 1 {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "interval accepts at most one inference operand",
            span,
        ));
    }
    Ok(Vec::new())
}

fn scalar_integer_shape(
    arguments: &[Expression],
    values: &ShapeEnvironment,
    function_result: &mut FunctionResultShape<'_>,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    let [value] = arguments else {
        return Err(invalid_integer_shape(span));
    };
    let shape = expression_shape(value, values, function_result)?;
    if shape.is_empty() {
        Ok(shape)
    } else {
        Err(invalid_integer_shape(span))
    }
}

fn invalid_integer_shape(span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function shape proof",
        "integer requires one scalar argument",
        span,
    )
}

fn binary_shape(
    operator: OpBinary,
    lhs: ValueShape,
    rhs: ValueShape,
    span: Span,
) -> Result<ValueShape, ToDaeError> {
    match operator {
        OpBinary::Mul => match (lhs.as_slice(), rhs.as_slice()) {
            ([], _) => Ok(rhs),
            (_, []) => Ok(lhs),
            ([lhs_n], [rhs_n]) if lhs_n == rhs_n => Ok(Vec::new()),
            ([rows, inner], [rhs_inner]) if inner == rhs_inner => Ok(vec![*rows]),
            ([lhs_inner], [rhs_inner, columns]) if lhs_inner == rhs_inner => Ok(vec![*columns]),
            ([rows, inner], [rhs_inner, columns]) if inner == rhs_inner => {
                Ok(vec![*rows, *columns])
            }
            _ => shape_mismatch(span),
        },
        OpBinary::Div => {
            if rhs.is_empty() {
                Ok(lhs)
            } else {
                shape_mismatch(span)
            }
        }
        OpBinary::MulElem | OpBinary::DivElem | OpBinary::ExpElem => {
            if lhs.is_empty() {
                Ok(rhs)
            } else if rhs.is_empty() || lhs == rhs {
                Ok(lhs)
            } else {
                shape_mismatch(span)
            }
        }
        OpBinary::Exp => {
            if rhs.is_empty() {
                Ok(lhs)
            } else {
                shape_mismatch(span)
            }
        }
        _ => {
            require_same_shape(&lhs, &rhs, span)?;
            Ok(lhs)
        }
    }
}

fn apply_subscripts(
    shape: ValueShape,
    subscripts: &[Subscript],
    values: &ShapeEnvironment,
) -> Result<ValueShape, ToDaeError> {
    let mut remaining = shape.into_iter();
    let mut result = Vec::new();
    for subscript in subscripts {
        let source_extent = remaining.next().ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "subscript count exceeds expression rank",
                subscript.span(),
            )
        })?;
        match subscript {
            Subscript::Index { value, span } => {
                if *value < 1
                    || u32::try_from(*value)
                        .ok()
                        .is_none_or(|value| value > source_extent)
                {
                    return Err(ToDaeError::unsupported_flat(
                        "function shape proof",
                        format!("literal index `{value}` is outside extent {source_extent}"),
                        *span,
                    ));
                }
            }
            Subscript::Colon { .. } => result.push(source_extent),
            Subscript::Expr { expr, .. } => {
                let index_shape = expression_shape(expr, values, &mut reject_shape_call)?;
                result.extend(index_shape);
            }
        }
    }
    result.extend(remaining);
    Ok(result)
}

fn require_same_shape(expected: &[u32], found: &[u32], span: Span) -> Result<(), ToDaeError> {
    if expected == found {
        Ok(())
    } else {
        shape_mismatch(span)
    }
}

fn shape_mismatch<T>(span: Span) -> Result<T, ToDaeError> {
    Err(ToDaeError::unsupported_flat(
        "function shape proof",
        "expression shapes are inconsistent",
        span,
    ))
}

fn range_cardinality(start: i64, step: i64, end: i64, span: Span) -> Result<u32, ToDaeError> {
    if step == 0 {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "range step cannot be zero",
            span,
        ));
    }
    let distance = if step > 0 {
        end.checked_sub(start)
    } else {
        start.checked_sub(end)
    };
    let Some(distance) = distance else {
        return Err(ToDaeError::unsupported_flat(
            "function shape proof",
            "range cardinality overflowed",
            span,
        ));
    };
    if distance < 0 {
        return Ok(0);
    }
    let count = distance
        .checked_div(step.abs())
        .and_then(|count| count.checked_add(1))
        .and_then(|count| u32::try_from(count).ok())
        .ok_or_else(|| {
            ToDaeError::unsupported_flat(
                "function shape proof",
                "range cardinality exceeds the DAE shape domain",
                span,
            )
        })?;
    Ok(count)
}

#[cfg(test)]
mod tests {
    use rumoca_core::{EffectiveType, Reference, SourceMap, Subscript, TypeId};

    use super::*;

    fn literal(value: f64, span: Span) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span,
        }
    }

    fn array(extent: usize, span: Span) -> Expression {
        Expression::Array {
            elements: (0..extent)
                .map(|ordinal| literal(ordinal as f64, span))
                .collect(),
            is_matrix: false,
            span,
        }
    }

    fn real_param(name: &str, dimensions: Vec<i64>, span: Span) -> rumoca_core::FunctionParam {
        let value_type = EffectiveType::new(TypeId::new(1), TypeId::new(1), dimensions)
            .expect("fixture function type is resolved");
        rumoca_core::FunctionParam::new(name, "Real", value_type, span)
    }

    fn identity_function(span: Span, result_has_shape_equality: bool) -> rumoca_core::Function {
        let mut function = rumoca_core::Function::new("identity", span);
        function.add_input(
            real_param("u", vec![0], span).with_shape_expr(vec![Subscript::colon(span)]),
        );
        let mut output = real_param("y", vec![0], span);
        if result_has_shape_equality {
            output = output.with_shape_expr(vec![Subscript::expr(
                Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Size,
                    args: vec![
                        Expression::VarRef {
                            name: Reference::new("u"),
                            subscripts: Vec::new(),
                            span,
                        },
                        Expression::Literal {
                            value: Literal::Integer(1),
                            span,
                        },
                    ],
                    span,
                }),
                span,
            )]);
        }
        function.add_output(output);
        function
    }

    fn call(extent: usize, span: Span) -> flat::Equation {
        flat::Equation::new(
            Expression::FunctionCall {
                name: Reference::new("identity"),
                args: vec![array(extent, span)],
                is_constructor: false,
                span,
            },
            span,
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        )
    }

    #[test]
    fn record_constructor_arity_remains_strict() {
        let mut sources = SourceMap::new();
        let source = sources.add("record_arity.mo", "Pair(1.0);");
        let span = Span::from_offsets(source, 0, 9);
        let mut constructor = rumoca_core::Function::new("Pair", span);
        constructor.is_constructor = true;
        constructor.add_input(real_param("left", Vec::new(), span));
        constructor.add_input(real_param("right", Vec::new(), span));

        let mut model = flat::Model::new();
        model.add_function(constructor);
        model.add_equation(flat::Equation::new(
            Expression::FunctionCall {
                name: Reference::new("Pair"),
                args: vec![literal(1.0, span)],
                is_constructor: true,
                span,
            },
            span,
            flat::EquationOrigin::ComponentEquation {
                component: String::new(),
            },
        ));

        let Err(error) = FunctionShapeAnalysis::analyze(&model) else {
            panic!("record constructor with one missing field must be rejected");
        };
        assert!(matches!(
            error,
            ToDaeError::UnsupportedFlatSemantics {
                feature,
                detail,
                span: error_span,
            } if feature == "record constructor"
                && detail == "`Pair` expects 2 fields but receives 1"
                && error_span == span
        ));
    }

    #[test]
    fn reachable_calls_receive_distinct_concrete_shape_certificates() {
        let mut sources = SourceMap::new();
        let source = sources.add("shape.mo", "identity({1,2}); identity({1,2,3});");
        let first = Span::from_offsets(source, 0, 15);
        let second = Span::from_offsets(source, 17, 34);
        let mut model = flat::Model::new();
        model.add_function(identity_function(first, true));
        model.add_equation(call(2, first));
        model.add_equation(call(3, second));

        let analysis = FunctionShapeAnalysis::analyze(&model).unwrap();
        let certificates = analysis.certificates();
        assert_eq!(certificates.len(), 2);
        assert_eq!(certificates[0].parameters, vec![vec![2]]);
        assert_eq!(certificates[0].results, vec![vec![2]]);
        assert_eq!(certificates[1].parameters, vec![vec![3]]);
        assert_eq!(certificates[1].results, vec![vec![3]]);
    }

    #[test]
    fn empty_array_call_has_a_zero_extent_shape_certificate() {
        let mut sources = SourceMap::new();
        let source = sources.add("empty_shape.mo", "identity({});");
        let span = Span::from_offsets(source, 0, 13);
        let mut model = flat::Model::new();
        model.add_function(identity_function(span, true));
        model.add_equation(call(0, span));

        let analysis = FunctionShapeAnalysis::analyze(&model).unwrap();
        let [certificate] = analysis.certificates() else {
            panic!("empty array call should have one shape certificate");
        };
        assert_eq!(certificate.parameters, vec![vec![0]]);
        assert_eq!(certificate.results, vec![vec![0]]);
    }

    #[test]
    fn unresolved_result_axis_is_rejected_at_analysis() {
        let mut sources = SourceMap::new();
        let source = sources.add("shape_error.mo", "identity({1,2});");
        let span = Span::from_offsets(source, 0, 15);
        let mut model = flat::Model::new();
        model.add_function(identity_function(span, false));
        model.add_equation(call(2, span));

        let error = match FunctionShapeAnalysis::analyze(&model) {
            Ok(_) => panic!("an unresolved result axis must not produce a certificate"),
            Err(error) => error,
        };
        assert!(matches!(
            error,
            ToDaeError::UnsupportedFlatSemantics { feature, span: error_span, .. }
                if feature == "function shape proof" && error_span == span
        ));
    }
}
