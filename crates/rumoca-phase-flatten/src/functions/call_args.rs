//! MLS §12.4.1 argument-slot construction for collected function calls.
//!
//! Final Flat calls are executable artifacts: named and default arguments are
//! projected into declaration-order slots before the DAE boundary. Partial
//! function applications remain source-shaped because their unfilled slots
//! are part of the function value rather than an invocation.

use rumoca_core::{ExpressionRewriter, FallibleExpressionRewriter, FallibleStatementRewriter};
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

use crate::errors::FlattenError;

/// Construct declaration-order argument vectors for every executable call in
/// the finalized Flat model.
pub(crate) fn materialize_flat_function_call_args(
    flat: &mut flat::Model,
) -> Result<(), FlattenError> {
    let catalog = FunctionCatalog::from_model(flat)?;
    let variable_dims = flat
        .variables
        .iter()
        .map(|(name, variable)| (name.clone(), variable.dims.clone()))
        .collect();
    let mut rewriter = CallArgumentMaterializer {
        catalog,
        variable_dims,
    };

    rewrite_model_expressions(flat, &mut rewriter)
}

#[derive(Clone)]
struct FunctionSignature {
    name: rumoca_core::VarName,
    inputs: Vec<rumoca_core::FunctionParam>,
}

struct FunctionCatalog {
    by_instance: FxHashMap<rumoca_core::FunctionInstanceId, FunctionSignature>,
    unique_instance_by_def: FxHashMap<rumoca_core::DefId, Option<rumoca_core::FunctionInstanceId>>,
}

impl FunctionCatalog {
    fn from_model(flat: &flat::Model) -> Result<Self, FlattenError> {
        let mut by_instance = FxHashMap::default();
        let mut unique_instance_by_def = FxHashMap::default();
        for function in flat.functions.values() {
            let instance_id = function
                .instance_id
                .ok_or_else(|| FlattenError::internal("missing function instance identity"))?;
            by_instance.insert(
                instance_id,
                FunctionSignature {
                    name: function.name.clone(),
                    inputs: function.inputs.clone(),
                },
            );
            if let Some(def_id) = function.def_id {
                unique_instance_by_def
                    .entry(def_id)
                    .and_modify(|candidate| *candidate = None)
                    .or_insert(Some(instance_id));
            }
        }
        Ok(Self {
            by_instance,
            unique_instance_by_def,
        })
    }

    fn for_reference(
        &self,
        reference: &rumoca_core::Reference,
    ) -> Result<Option<FunctionSignature>, FlattenError> {
        let Some(resolved) = reference.resolved_function() else {
            if reference
                .target_def_id()
                .is_some_and(|def_id| self.unique_instance_by_def.contains_key(&def_id))
            {
                return Err(FlattenError::internal(format!(
                    "function reference `{}` has declaration identity but no Flat function instance",
                    reference.as_str()
                )));
            }
            return Ok(None);
        };
        self.by_instance
            .get(&resolved.instance_id)
            .cloned()
            .map(Some)
            .ok_or_else(|| {
                FlattenError::internal(format!(
                    "resolved function instance {:?} is absent from Flat",
                    resolved.instance_id
                ))
            })
    }

    fn for_component_reference(
        &self,
        reference: &rumoca_core::ComponentReference,
        span: rumoca_core::Span,
    ) -> Result<Option<FunctionSignature>, FlattenError> {
        let def_id = reference.target_def_id();
        let Some(instance_id) = self.unique_instance_by_def.get(&def_id) else {
            return Ok(None);
        };
        let Some(instance_id) = instance_id else {
            return Err(FlattenError::invalid_function_call_args(
                reference.to_var_name().as_str(),
                "call target declaration has multiple Flat function instances",
                span,
            ));
        };
        Ok(self.by_instance.get(instance_id).cloned())
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum CallUse {
    Executable,
    PartialApplication,
}

struct CallArgumentMaterializer {
    catalog: FunctionCatalog,
    variable_dims: FxHashMap<rumoca_core::VarName, Vec<i64>>,
}

impl CallArgumentMaterializer {
    fn rewrite_with_use(
        &mut self,
        expression: &rumoca_core::Expression,
        call_use: CallUse,
    ) -> Result<rumoca_core::Expression, FlattenError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expression
        else {
            return self.walk_expression(expression);
        };
        let Some(signature) = self.catalog.for_reference(name)? else {
            return Ok(rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: self.rewrite_expressions(args)?,
                is_constructor: *is_constructor,
                span: *span,
            });
        };
        let args = self.arguments_for_call(&signature, args, call_use, *span)?;
        Ok(rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args,
            is_constructor: *is_constructor,
            span: *span,
        })
    }

    fn arguments_for_call(
        &mut self,
        signature: &FunctionSignature,
        args: &[rumoca_core::Expression],
        call_use: CallUse,
        span: rumoca_core::Span,
    ) -> Result<Vec<rumoca_core::Expression>, FlattenError> {
        let supplied = bind_supplied_arguments(signature, args, span)?;
        if call_use == CallUse::PartialApplication {
            return self.rewrite_partial_arguments(signature, supplied);
        }

        let mut slots = vec![None; signature.inputs.len()];
        for argument in supplied {
            let use_kind = call_use_for_input(&signature.inputs[argument.slot]);
            slots[argument.slot] = Some(self.rewrite_with_use(argument.value, use_kind)?);
        }
        self.fill_defaults(signature, &mut slots, span)?;
        let args = slots
            .into_iter()
            .map(|argument| {
                argument.ok_or_else(|| {
                    FlattenError::internal("materialized function call contains an empty slot")
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        validate_vectorized_dimensions(signature, &args, &self.variable_dims, span)?;
        Ok(args)
    }

    fn rewrite_partial_arguments(
        &mut self,
        signature: &FunctionSignature,
        supplied: Vec<SuppliedArgument<'_>>,
    ) -> Result<Vec<rumoca_core::Expression>, FlattenError> {
        supplied
            .into_iter()
            .map(|argument| {
                let use_kind = call_use_for_input(&signature.inputs[argument.slot]);
                let value = self.rewrite_with_use(argument.value, use_kind)?;
                Ok(match argument.named_marker {
                    Some(marker) => rebuild_named_marker(marker, value),
                    None => value,
                })
            })
            .collect()
    }

    fn fill_defaults(
        &mut self,
        signature: &FunctionSignature,
        slots: &mut [Option<rumoca_core::Expression>],
        span: rumoca_core::Span,
    ) -> Result<(), FlattenError> {
        let mut prior_values = FxHashMap::default();
        for (index, input) in signature.inputs.iter().enumerate() {
            if slots[index].is_none() {
                let default = input
                    .default
                    .as_ref()
                    .ok_or_else(|| missing_default_error(signature, input, span))?;
                let mut substituter = DefaultInputSubstituter {
                    values: &prior_values,
                };
                let substituted = substituter.rewrite_expression(default);
                slots[index] =
                    Some(self.rewrite_with_use(&substituted, call_use_for_input(input))?);
            }
            if let (Some(def_id), Some(value)) = (input.def_id, slots[index].as_ref()) {
                prior_values.insert(def_id, value.clone());
            }
        }
        Ok(())
    }

    fn rewrite_statement_call(
        &mut self,
        statement: &rumoca_core::Statement,
    ) -> Result<Option<rumoca_core::Statement>, FlattenError> {
        let rumoca_core::Statement::FunctionCall {
            comp,
            args,
            outputs,
            span,
        } = statement
        else {
            return Ok(None);
        };
        let rewritten_comp = self.rewrite_component_reference(comp)?;
        let Some(signature) = self.catalog.for_component_reference(comp, *span)? else {
            return Ok(Some(rumoca_core::Statement::FunctionCall {
                comp: rewritten_comp,
                args: self.rewrite_expressions(args)?,
                outputs: outputs
                    .iter()
                    .map(|output| {
                        output
                            .as_ref()
                            .map(|output| self.rewrite_component_reference(output))
                            .transpose()
                    })
                    .collect::<Result<Vec<_>, _>>()?,
                span: *span,
            }));
        };
        Ok(Some(rumoca_core::Statement::FunctionCall {
            comp: rewritten_comp,
            args: self.arguments_for_call(&signature, args, CallUse::Executable, *span)?,
            outputs: outputs
                .iter()
                .map(|output| {
                    output
                        .as_ref()
                        .map(|output| self.rewrite_component_reference(output))
                        .transpose()
                })
                .collect::<Result<Vec<_>, _>>()?,
            span: *span,
        }))
    }
}

impl FallibleExpressionRewriter for CallArgumentMaterializer {
    type Error = FlattenError;

    fn rewrite_expression(
        &mut self,
        expression: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, Self::Error> {
        self.rewrite_with_use(expression, CallUse::Executable)
    }
}

impl FallibleStatementRewriter for CallArgumentMaterializer {
    fn rewrite_statement(
        &mut self,
        statement: &rumoca_core::Statement,
    ) -> Result<rumoca_core::Statement, Self::Error> {
        match self.rewrite_statement_call(statement)? {
            Some(statement) => Ok(statement),
            None => self.walk_statement(statement),
        }
    }
}

struct SuppliedArgument<'a> {
    slot: usize,
    value: &'a rumoca_core::Expression,
    named_marker: Option<&'a rumoca_core::Expression>,
}

fn bind_supplied_arguments<'a>(
    signature: &FunctionSignature,
    args: &'a [rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<Vec<SuppliedArgument<'a>>, FlattenError> {
    let mut supplied = Vec::with_capacity(args.len());
    let mut filled = vec![false; signature.inputs.len()];
    let mut positional = 0usize;
    let mut saw_named = false;
    for argument in args {
        let (slot, value, named_marker) = if has_named_argument_prefix(argument) {
            let Some((name, value)) = named_argument_value(argument) else {
                return Err(call_error(
                    signature,
                    "compiler-generated named argument marker is malformed",
                    span,
                ));
            };
            saw_named = true;
            let Some(slot) = signature.inputs.iter().position(|input| input.name == name) else {
                return Err(call_error(
                    signature,
                    format!("named argument `{name}` does not match any input"),
                    span,
                ));
            };
            (slot, value, Some(argument))
        } else {
            if saw_named {
                return Err(call_error(
                    signature,
                    "positional arguments may not follow named arguments",
                    span,
                ));
            }
            let slot = positional;
            positional += 1;
            (slot, argument, None)
        };
        if slot >= signature.inputs.len() {
            return Err(call_error(
                signature,
                format!(
                    "{} positional argument(s) for {} input slot(s)",
                    positional,
                    signature.inputs.len()
                ),
                span,
            ));
        }
        if std::mem::replace(&mut filled[slot], true) {
            return Err(call_error(
                signature,
                format!(
                    "input slot `{}` is filled more than once",
                    signature.inputs[slot].name
                ),
                span,
            ));
        }
        supplied.push(SuppliedArgument {
            slot,
            value,
            named_marker,
        });
    }
    Ok(supplied)
}

fn has_named_argument_prefix(argument: &rumoca_core::Expression) -> bool {
    matches!(
        argument,
        rumoca_core::Expression::FunctionCall { name, .. }
            if name.is_generated()
                && name
                    .as_str()
                    .starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    )
}

fn named_argument_value(
    argument: &rumoca_core::Expression,
) -> Option<(&str, &rumoca_core::Expression)> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor: true,
        ..
    } = argument
    else {
        return None;
    };
    if !name.is_generated() {
        return None;
    }
    let input_name = name
        .as_str()
        .strip_prefix(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)?;
    let [value] = args.as_slice() else {
        return None;
    };
    Some((input_name, value))
}

fn rebuild_named_marker(
    marker: &rumoca_core::Expression,
    value: rumoca_core::Expression,
) -> rumoca_core::Expression {
    let rumoca_core::Expression::FunctionCall {
        name,
        is_constructor,
        span,
        ..
    } = marker
    else {
        unreachable!("named argument parser returned a non-call marker");
    };
    rumoca_core::Expression::FunctionCall {
        name: name.clone(),
        args: vec![value],
        is_constructor: *is_constructor,
        span: *span,
    }
}

fn call_use_for_input(input: &rumoca_core::FunctionParam) -> CallUse {
    if input.type_class == Some(rumoca_core::ClassType::Function) {
        CallUse::PartialApplication
    } else {
        CallUse::Executable
    }
}

fn call_error(
    signature: &FunctionSignature,
    reason: impl Into<String>,
    span: rumoca_core::Span,
) -> FlattenError {
    FlattenError::invalid_function_call_args(signature.name.as_str(), reason, span)
}

fn validate_vectorized_dimensions(
    signature: &FunctionSignature,
    args: &[rumoca_core::Expression],
    variable_dims: &FxHashMap<rumoca_core::VarName, Vec<i64>>,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    let mut vectorized: Option<(Vec<i64>, String)> = None;
    for (input, argument) in signature.inputs.iter().zip(args) {
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = argument
        else {
            continue;
        };
        if !subscripts.is_empty() {
            continue;
        }
        let Some(dims) = variable_dims.get(name.var_name()) else {
            continue;
        };
        if dims.len() <= input.dimensions().len() {
            continue;
        }
        let extra = dims[..dims.len() - input.dimensions().len()].to_vec();
        match &vectorized {
            None => vectorized = Some((extra, name.as_str().to_string())),
            Some((expected, first_name)) if *expected != extra => {
                return Err(call_error(
                    signature,
                    format!(
                        "vectorized arguments must share dimensions: `{first_name}` adds {expected:?} but `{}` adds {extra:?} (MLS §12.4.6)",
                        name.as_str()
                    ),
                    span,
                ));
            }
            Some(_) => {}
        }
    }
    Ok(())
}

struct DefaultInputSubstituter<'a> {
    values: &'a FxHashMap<rumoca_core::DefId, rumoca_core::Expression>,
}

impl ExpressionRewriter for DefaultInputSubstituter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let Some(value) = name
            .target_def_id()
            .and_then(|def_id| self.values.get(&def_id))
        else {
            return self.walk_var_ref_expression(name, subscripts, span);
        };
        if subscripts.is_empty() {
            return value.clone();
        }
        rumoca_core::Expression::Index {
            base: Box::new(value.clone()),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

fn rewrite_model_expressions(
    flat: &mut flat::Model,
    rewriter: &mut CallArgumentMaterializer,
) -> Result<(), FlattenError> {
    for variable in flat.variables.values_mut() {
        for expression in [
            &mut variable.binding,
            &mut variable.start,
            &mut variable.min,
            &mut variable.max,
            &mut variable.nominal,
        ]
        .into_iter()
        .flatten()
        {
            *expression = rewriter.rewrite_expression(expression)?;
        }
    }
    for equation in flat
        .equations
        .iter_mut()
        .chain(flat.initial_equations.iter_mut())
    {
        equation.residual = rewriter.rewrite_expression(&equation.residual)?;
    }
    rewrite_structured_templates(flat, rewriter)?;
    rewrite_assertions(&mut flat.assert_equations, rewriter)?;
    rewrite_assertions(&mut flat.initial_assert_equations, rewriter)?;
    for algorithm in flat
        .algorithms
        .iter_mut()
        .chain(flat.initial_algorithms.iter_mut())
    {
        algorithm.statements = rewriter.rewrite_statements(&algorithm.statements)?;
    }
    for chain in &mut flat.when_chains {
        for branch in chain.branches_mut() {
            branch.condition = rewriter.rewrite_expression(&branch.condition)?;
            rewrite_when_equations(&mut branch.equations, rewriter)?;
        }
    }
    rewrite_function_expressions(flat, rewriter)
}

fn rewrite_structured_templates(
    flat: &mut flat::Model,
    rewriter: &mut CallArgumentMaterializer,
) -> Result<(), FlattenError> {
    for family in flat
        .structured_equations
        .iter_mut()
        .chain(flat.initial_structured_equations.iter_mut())
    {
        let Some(template) = family.template.as_mut() else {
            continue;
        };
        template.body = rewriter.rewrite_expressions(&template.body)?;
    }
    Ok(())
}

fn rewrite_assertions(
    assertions: &mut [flat::AssertEquation],
    rewriter: &mut CallArgumentMaterializer,
) -> Result<(), FlattenError> {
    for assertion in assertions {
        assertion.condition = rewriter.rewrite_expression(&assertion.condition)?;
        assertion.message = rewriter.rewrite_expression(&assertion.message)?;
        if let Some(level) = assertion.level.as_mut() {
            *level = rewriter.rewrite_expression(level)?;
        }
    }
    Ok(())
}

fn rewrite_when_equations(
    equations: &mut [flat::WhenEquation],
    rewriter: &mut CallArgumentMaterializer,
) -> Result<(), FlattenError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                *value = rewriter.rewrite_expression(value)?;
            }
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                *condition = rewriter.rewrite_expression(condition)?;
                *message = rewriter.rewrite_expression(message)?;
                if let Some(level) = level.as_deref_mut() {
                    *level = rewriter.rewrite_expression(level)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                *message = rewriter.rewrite_expression(message)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    *condition = rewriter.rewrite_expression(condition)?;
                    rewrite_when_equations(branch_equations, rewriter)?;
                }
                if let Some(else_branch) = else_branch {
                    rewrite_when_equations(else_branch, rewriter)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                *function = rewriter.rewrite_expression(function)?;
            }
        }
    }
    Ok(())
}

fn rewrite_function_expressions(
    flat: &mut flat::Model,
    rewriter: &mut CallArgumentMaterializer,
) -> Result<(), FlattenError> {
    for function in flat.functions.values_mut() {
        for parameter in function
            .inputs
            .iter_mut()
            .chain(function.outputs.iter_mut())
            .chain(function.locals.iter_mut())
        {
            for expression in [
                &mut parameter.default,
                &mut parameter.min,
                &mut parameter.max,
            ]
            .into_iter()
            .flatten()
            {
                *expression = rewriter.rewrite_expression(expression)?;
            }
            for expression in parameter
                .shape_expr
                .iter_mut()
                .filter_map(subscript_expression_mut)
            {
                **expression = rewriter.rewrite_expression(expression)?;
            }
        }
        function.body = rewriter.rewrite_statements(&function.body)?;
    }
    Ok(())
}

fn missing_default_error(
    signature: &FunctionSignature,
    input: &rumoca_core::FunctionParam,
    span: rumoca_core::Span,
) -> FlattenError {
    FlattenError::invalid_function_call_args(
        signature.name.as_str(),
        format!("input `{}` has no argument and no default", input.name),
        span,
    )
}

fn subscript_expression_mut(
    subscript: &mut rumoca_core::Subscript,
) -> Option<&mut Box<rumoca_core::Expression>> {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => Some(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FUNCTION_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(100);
    const FIRST_INPUT_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(101);
    const SECOND_INPUT_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(102);
    const OUTPUT_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(103);
    const PACKAGE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(99);
    const APPLY_OUTPUT_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(201);

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("call_args.mo"),
            1,
            2,
        )
    }

    fn component_reference(
        parts: &[(&str, rumoca_core::DefId)],
    ) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference::construct(
            false,
            span(),
            parts
                .iter()
                .map(|(ident, def_id)| rumoca_core::ComponentRefPart {
                    ident: (*ident).to_string(),
                    span: span(),
                    subs: Vec::new(),
                    def_id: *def_id,
                })
                .collect(),
        )
        .expect("test reference is nonempty and every part is resolved")
    }

    fn resolved_call(
        instance_id: rumoca_core::FunctionInstanceId,
        args: Vec<rumoca_core::Expression>,
    ) -> rumoca_core::Expression {
        let reference = rumoca_core::Reference::from_component_reference(component_reference(&[
            ("Pkg", PACKAGE_DEF_ID),
            ("f", FUNCTION_DEF_ID),
        ]))
        .with_resolved_function(rumoca_core::ResolvedFunctionReference {
            instance_id,
            base_part_count: 2,
        });
        rumoca_core::Expression::FunctionCall {
            name: reference,
            args,
            is_constructor: false,
            span: span(),
        }
    }

    fn literal(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: span(),
        }
    }

    fn named_argument(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::generated(format!(
                "{}{name}",
                rumoca_core::NAMED_FUNCTION_ARG_PREFIX
            )),
            args: vec![value],
            is_constructor: true,
            span: span(),
        }
    }

    fn parameter_ref(def_id: rumoca_core::DefId, name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(component_reference(&[(
                name, def_id,
            )])),
            subscripts: Vec::new(),
            span: span(),
        }
    }

    fn model_with_function() -> (flat::Model, rumoca_core::FunctionInstanceId) {
        let mut model = flat::Model::new();
        let mut function = rumoca_core::Function::new("Pkg.f", span());
        function.def_id = Some(FUNCTION_DEF_ID);
        function.inputs.push(
            crate::test_support::real_param("a", Vec::new(), span())
                .with_def_id(FIRST_INPUT_DEF_ID),
        );
        function.inputs.push(
            crate::test_support::real_param("b", Vec::new(), span())
                .with_def_id(SECOND_INPUT_DEF_ID)
                .with_default(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Add,
                    lhs: Box::new(parameter_ref(FIRST_INPUT_DEF_ID, "a")),
                    rhs: Box::new(literal(1.0)),
                    span: span(),
                }),
        );
        function.outputs.push(
            crate::test_support::real_param("y", Vec::new(), span()).with_def_id(OUTPUT_DEF_ID),
        );
        function.body.push(rumoca_core::Statement::Assignment {
            comp: component_reference(&[("y", OUTPUT_DEF_ID)]),
            value: parameter_ref(FIRST_INPUT_DEF_ID, "a"),
            span: span(),
        });
        model.add_function(function);
        let instance_id = model.functions[&rumoca_core::VarName::new("Pkg.f")]
            .instance_id
            .expect("function instance");
        (model, instance_id)
    }

    #[test]
    fn executable_call_materializes_dependent_default_positionally() {
        let (mut model, instance_id) = model_with_function();
        model.add_equation(flat::Equation::new(
            resolved_call(instance_id, vec![literal(4.0)]),
            span(),
            flat::EquationOrigin::ComponentEquation {
                component: "y".to_string(),
            },
        ));

        materialize_flat_function_call_args(&mut model).expect("materialize call");

        let rumoca_core::Expression::FunctionCall { args, .. } = &model.equations[0].residual
        else {
            panic!("expected call");
        };
        assert_eq!(args.len(), 2);
        assert_eq!(args[0], literal(4.0));
        assert!(matches!(
            &args[1],
            rumoca_core::Expression::Binary { lhs, .. } if **lhs == literal(4.0)
        ));
    }

    #[test]
    fn named_call_is_materialized_in_declaration_order() {
        let (mut model, instance_id) = model_with_function();
        let named = named_argument("b", literal(9.0));
        model.add_equation(flat::Equation::new(
            resolved_call(instance_id, vec![literal(4.0), named]),
            span(),
            flat::EquationOrigin::ComponentEquation {
                component: "y".to_string(),
            },
        ));

        materialize_flat_function_call_args(&mut model).expect("materialize call");

        let rumoca_core::Expression::FunctionCall { args, .. } = &model.equations[0].residual
        else {
            panic!("expected call");
        };
        assert_eq!(args, &vec![literal(4.0), literal(9.0)]);
    }

    #[test]
    fn partial_application_keeps_named_captures_and_unbound_input() {
        let (mut model, inner_instance_id) = model_with_function();
        let inner = model
            .functions
            .get_mut(&rumoca_core::VarName::new("Pkg.f"))
            .expect("inner function");
        inner.inputs = ["u", "A", "w"]
            .into_iter()
            .enumerate()
            .map(|(index, name)| {
                crate::test_support::real_param(name, Vec::new(), span())
                    .with_def_id(rumoca_core::DefId(300 + index as u32))
            })
            .collect();
        let mut apply = rumoca_core::Function::new("Pkg.apply", span());
        apply.def_id = Some(rumoca_core::DefId(200));
        apply.inputs.push(
            crate::test_support::aggregate_param("fn", "Pkg.Partial", Vec::new(), span())
                .with_type_class(rumoca_core::ClassType::Function),
        );
        apply.outputs.push(
            crate::test_support::real_param("y", Vec::new(), span())
                .with_def_id(APPLY_OUTPUT_DEF_ID),
        );
        apply.body.push(rumoca_core::Statement::Assignment {
            comp: component_reference(&[("y", APPLY_OUTPUT_DEF_ID)]),
            value: literal(0.0),
            span: span(),
        });
        model.add_function(apply);
        let apply_instance_id = model.functions[&rumoca_core::VarName::new("Pkg.apply")]
            .instance_id
            .expect("apply instance");
        model.add_equation(flat::Equation::new(
            resolved_call(
                apply_instance_id,
                vec![resolved_call(
                    inner_instance_id,
                    vec![
                        named_argument("A", literal(2.0)),
                        named_argument("w", literal(3.0)),
                    ],
                )],
            ),
            span(),
            flat::EquationOrigin::ComponentEquation {
                component: "y".to_string(),
            },
        ));

        materialize_flat_function_call_args(&mut model).expect("materialize call");

        let rumoca_core::Expression::FunctionCall {
            args: outer_args, ..
        } = &model.equations[0].residual
        else {
            panic!("expected outer call");
        };
        let rumoca_core::Expression::FunctionCall {
            args: inner_args, ..
        } = &outer_args[0]
        else {
            panic!("expected partial application");
        };
        assert_eq!(inner_args.len(), 2);
        assert_eq!(
            inner_args
                .iter()
                .filter_map(named_argument_value)
                .map(|(name, _value)| name)
                .collect::<Vec<_>>(),
            vec!["A", "w"]
        );
    }
}
