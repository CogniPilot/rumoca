use crate::errors::ToDaeError;
use indexmap::{IndexMap, IndexSet};
use rumoca_core::ExpressionRewriter;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

pub(crate) fn flat_to_dae_var_name(name: &rumoca_core::VarName) -> rumoca_core::VarName {
    name.clone()
}

pub(crate) fn dae_to_flat_var_name(name: &rumoca_core::VarName) -> rumoca_core::VarName {
    name.clone()
}

pub(crate) fn flat_to_dae_expression(expr: &rumoca_core::Expression) -> rumoca_core::Expression {
    expr.clone()
}

pub(crate) fn flat_to_dae_expression_with_refs(
    expr: &rumoca_core::Expression,
    flat: &flat::Model,
) -> Result<rumoca_core::Expression, ToDaeError> {
    DaeReferenceRewriter { flat }.rewrite_expression(expr)
}

pub fn attach_dae_reference_metadata(dae: &mut dae::Dae) -> Result<(), ToDaeError> {
    assign_missing_source_component_ref_def_ids(dae);
    let scope = DaeReferenceScope::new(dae);
    let mut rewriter = DaeMetadataReferenceRewriter {
        scope,
        local_names: Vec::new(),
        error: None,
    };
    rewriter.rewrite_equations(&mut dae.continuous.equations);
    rewriter.rewrite_equations(&mut dae.initialization.equations);
    rewriter.rewrite_equations(&mut dae.discrete.real_updates);
    rewriter.rewrite_equations(&mut dae.discrete.valued_updates);
    rewriter.rewrite_equations(&mut dae.conditions.equations);
    rewriter.rewrite_expression_slots(&mut dae.conditions.relations);
    rewriter.rewrite_expression_slots(&mut dae.events.synthetic_root_conditions);
    rewriter.rewrite_event_actions(&mut dae.events.event_actions);
    rewriter.rewrite_expression_slots(&mut dae.clocks.constructor_exprs);
    rewriter.rewrite_expression_slots(&mut dae.clocks.triggered_conditions);
    rewriter.rewrite_variable_attributes(&mut dae.variables);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

fn assign_missing_source_component_ref_def_ids(dae: &mut dae::Dae) {
    let mut next = next_component_ref_def_id(dae);
    assign_missing_partition_def_ids(&mut dae.variables.states, &mut next);
    assign_missing_partition_def_ids(&mut dae.variables.algebraics, &mut next);
    assign_missing_partition_def_ids(&mut dae.variables.inputs, &mut next);
    assign_missing_partition_def_ids(&mut dae.variables.outputs, &mut next);
    assign_missing_partition_def_ids(&mut dae.variables.parameters, &mut next);
    assign_missing_partition_def_ids(&mut dae.variables.constants, &mut next);
    assign_missing_partition_def_ids(&mut dae.variables.discrete_reals, &mut next);
    assign_missing_partition_def_ids(&mut dae.variables.discrete_valued, &mut next);
}

fn next_component_ref_def_id(dae: &dae::Dae) -> u32 {
    dae.variables
        .states
        .values()
        .chain(dae.variables.algebraics.values())
        .chain(dae.variables.inputs.values())
        .chain(dae.variables.outputs.values())
        .chain(dae.variables.parameters.values())
        .chain(dae.variables.constants.values())
        .chain(dae.variables.discrete_reals.values())
        .chain(dae.variables.discrete_valued.values())
        .filter_map(|var| var.component_ref.as_ref()?.def_id)
        .map(|def_id| def_id.index())
        .max()
        .map_or(1, |max| max.saturating_add(1))
}

fn assign_missing_partition_def_ids(
    partition: &mut IndexMap<rumoca_core::VarName, dae::Variable>,
    next: &mut u32,
) {
    for var in partition.values_mut() {
        if var.origin != dae::VariableOrigin::Source {
            continue;
        }
        let Some(component_ref) = var.component_ref.as_mut() else {
            continue;
        };
        if component_ref.def_id.is_some() {
            continue;
        }
        component_ref.def_id = Some(rumoca_core::DefId::new(*next));
        *next = next.saturating_add(1);
    }
}

/// The structured reference for a rendered scalar target name produced by
/// equation/algorithm lowering. Rendered names are internal bookkeeping
/// keys; any reference emitted into equations must carry the structured
/// component reference so DAE resolution never parses names. Targets whose
/// rendered subscripts are not static integers stay unstructured and fail
/// resolution loudly.
pub(crate) fn structured_target_reference(
    target: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, ToDaeError> {
    let owner = span
        .require_provenance("DAE structured target reference")
        .map_err(|err| ToDaeError::runtime_metadata_violation(format!("{err} for `{target}`")))?;
    match rumoca_core::component_reference_from_flat_name(target, owner.span()) {
        Some(component_ref) => Ok(rumoca_core::Reference::from_component_reference(
            component_ref,
        )),
        None => Ok(target.clone().into()),
    }
}

pub(crate) fn structured_target_reference_with_flat_metadata(
    target: &rumoca_core::VarName,
    span: rumoca_core::Span,
    flat: &flat::Model,
) -> Result<rumoca_core::Reference, ToDaeError> {
    let owner = structured_target_owner_from_flat(target, span, flat)?;
    if let Some(component_ref) = flat_component_ref_for_target(target, flat, owner) {
        return Ok(rumoca_core::Reference::with_component_reference(
            target.as_str(),
            component_ref,
        ));
    }
    structured_target_reference(target, owner.span())
}

pub(crate) fn structured_target_reference_with_dae_metadata(
    target: &rumoca_core::VarName,
    span: rumoca_core::Span,
    dae: &dae::Dae,
) -> Result<rumoca_core::Reference, ToDaeError> {
    let owner = structured_target_owner_from_dae(target, span, dae)?;
    if let Some(component_ref) = dae_component_ref_for_target(target, dae, owner) {
        return Ok(rumoca_core::Reference::with_component_reference(
            target.as_str(),
            component_ref,
        ));
    }
    structured_target_reference(target, owner.span())
}

fn structured_target_owner_from_flat(
    target: &rumoca_core::VarName,
    span: rumoca_core::Span,
    flat: &flat::Model,
) -> Result<rumoca_core::ProvenanceSpan, ToDaeError> {
    if let Ok(owner) = span.require_provenance("DAE structured target reference") {
        return Ok(owner);
    }
    if let Some(span) = flat_target_span(target, flat) {
        return span
            .require_provenance("DAE structured target reference")
            .map_err(|err| target_provenance_error(target, err.to_string(), span));
    }
    span.require_provenance("DAE structured target reference")
        .map_err(|err| target_provenance_error(target, err.to_string(), span))
}

fn structured_target_owner_from_dae(
    target: &rumoca_core::VarName,
    span: rumoca_core::Span,
    dae: &dae::Dae,
) -> Result<rumoca_core::ProvenanceSpan, ToDaeError> {
    if let Ok(owner) = span.require_provenance("DAE structured target reference") {
        return Ok(owner);
    }
    if let Some(span) = dae_target_span(target, dae) {
        return span
            .require_provenance("DAE structured target reference")
            .map_err(|err| target_provenance_error(target, err.to_string(), span));
    }
    span.require_provenance("DAE structured target reference")
        .map_err(|err| target_provenance_error(target, err.to_string(), span))
}

fn flat_target_span(
    target: &rumoca_core::VarName,
    flat: &flat::Model,
) -> Option<rumoca_core::Span> {
    flat.variables
        .get(target)
        .and_then(flat_variable_span)
        .or_else(|| {
            flat.record_instances
                .get(target)
                .map(|record| record.source_span)
        })
        .or_else(|| {
            rumoca_core::parse_scalar_name(target.as_str()).and_then(|scalar| {
                flat.variables
                    .get(&rumoca_core::VarName::new(scalar.base))
                    .and_then(flat_variable_span)
            })
        })
}

fn dae_target_span(target: &rumoca_core::VarName, dae: &dae::Dae) -> Option<rumoca_core::Span> {
    find_dae_variable(target, dae)
        .and_then(dae_variable_span)
        .or_else(|| {
            rumoca_core::parse_scalar_name(target.as_str()).and_then(|scalar| {
                find_dae_variable(&rumoca_core::VarName::new(scalar.base), dae)
                    .and_then(dae_variable_span)
            })
        })
}

fn find_dae_variable<'a>(
    target: &rumoca_core::VarName,
    dae: &'a dae::Dae,
) -> Option<&'a dae::Variable> {
    dae.variables
        .states
        .get(target)
        .or_else(|| dae.variables.algebraics.get(target))
        .or_else(|| dae.variables.inputs.get(target))
        .or_else(|| dae.variables.outputs.get(target))
        .or_else(|| dae.variables.parameters.get(target))
        .or_else(|| dae.variables.constants.get(target))
        .or_else(|| dae.variables.discrete_reals.get(target))
        .or_else(|| dae.variables.discrete_valued.get(target))
}

fn flat_component_ref_for_target(
    target: &rumoca_core::VarName,
    flat: &flat::Model,
    owner: rumoca_core::ProvenanceSpan,
) -> Option<rumoca_core::ComponentReference> {
    flat.variables
        .get(target)
        .and_then(|variable| variable.component_ref.clone())
        .or_else(|| {
            flat.record_instances
                .get(target)
                .map(|record| record.component_ref.clone())
        })
        .map(|component_ref| component_ref_with_missing_spans(component_ref, owner.span()))
        .or_else(|| {
            let scalar = rumoca_core::parse_scalar_name(target.as_str())?;
            let mut component_ref = flat
                .variables
                .get(&rumoca_core::VarName::new(scalar.base))?
                .component_ref
                .clone()?;
            append_scalar_indices(&mut component_ref, &scalar.indices, owner);
            Some(component_ref_with_missing_spans(
                component_ref,
                owner.span(),
            ))
        })
}

fn dae_component_ref_for_target(
    target: &rumoca_core::VarName,
    dae: &dae::Dae,
    owner: rumoca_core::ProvenanceSpan,
) -> Option<rumoca_core::ComponentReference> {
    find_dae_variable(target, dae)
        .and_then(|variable| variable.component_ref.clone())
        .map(|component_ref| component_ref_with_missing_spans(component_ref, owner.span()))
        .or_else(|| {
            let scalar = rumoca_core::parse_scalar_name(target.as_str())?;
            let mut component_ref =
                find_dae_variable(&rumoca_core::VarName::new(scalar.base), dae)?
                    .component_ref
                    .clone()?;
            append_scalar_indices(&mut component_ref, &scalar.indices, owner);
            Some(component_ref_with_missing_spans(
                component_ref,
                owner.span(),
            ))
        })
}

fn append_scalar_indices(
    component_ref: &mut rumoca_core::ComponentReference,
    indices: &[i64],
    owner: rumoca_core::ProvenanceSpan,
) {
    if let Some(part) = component_ref.parts.last_mut() {
        part.subs.extend(
            indices.iter().map(|index| {
                rumoca_core::Subscript::generated_index_with_provenance(*index, owner)
            }),
        );
    }
}

fn component_ref_with_missing_spans(
    mut component_ref: rumoca_core::ComponentReference,
    owner_span: rumoca_core::Span,
) -> rumoca_core::ComponentReference {
    if component_ref.span.is_dummy() {
        component_ref.span = owner_span;
    }
    for part in &mut component_ref.parts {
        if part.span.is_dummy() {
            part.span = owner_span;
        }
        for subscript in &mut part.subs {
            fill_subscript_missing_span(subscript, owner_span);
        }
    }
    component_ref
}

fn fill_subscript_missing_span(
    subscript: &mut rumoca_core::Subscript,
    owner_span: rumoca_core::Span,
) {
    match subscript {
        rumoca_core::Subscript::Index { span, .. }
        | rumoca_core::Subscript::Colon { span }
        | rumoca_core::Subscript::Expr { span, .. } => {
            if span.is_dummy() {
                *span = owner_span;
            }
        }
    }
}

fn flat_variable_span(variable: &flat::Variable) -> Option<rumoca_core::Span> {
    real_span(variable.source_span)
        .or_else(|| variable.component_ref.as_ref().and_then(component_ref_span))
}

fn dae_variable_span(variable: &dae::Variable) -> Option<rumoca_core::Span> {
    real_span(variable.source_span)
        .or_else(|| variable.component_ref.as_ref().and_then(component_ref_span))
        .or_else(|| variable.start_span.and_then(real_span))
}

fn component_ref_span(
    component_ref: &rumoca_core::ComponentReference,
) -> Option<rumoca_core::Span> {
    real_span(component_ref.span).or_else(|| {
        component_ref.parts.iter().find_map(|part| {
            real_span(part.span).or_else(|| part.subs.iter().find_map(subscript_span))
        })
    })
}

fn subscript_span(subscript: &rumoca_core::Subscript) -> Option<rumoca_core::Span> {
    real_span(subscript.span())
}

fn real_span(span: rumoca_core::Span) -> Option<rumoca_core::Span> {
    (!span.is_dummy()).then_some(span)
}

fn target_provenance_error(
    target: &rumoca_core::VarName,
    detail: String,
    span: rumoca_core::Span,
) -> ToDaeError {
    if span.is_dummy() {
        ToDaeError::runtime_metadata_violation(format!("{detail} for `{target}`"))
    } else {
        ToDaeError::runtime_metadata_violation_at(format!("{detail} for `{target}`"), span)
    }
}

pub(crate) fn dae_to_flat_expression(expr: &rumoca_core::Expression) -> rumoca_core::Expression {
    expr.clone()
}

struct DaeReferenceRewriter<'a> {
    flat: &'a flat::Model,
}

impl DaeReferenceRewriter<'_> {
    fn rewrite_expression(
        &self,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        match expr {
            rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
                self.rewrite_binary_expression(op, lhs, rhs, *span)
            }
            rumoca_core::Expression::Unary { op, rhs, span } => {
                self.rewrite_unary_expression(op, rhs, *span)
            }
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } => self.rewrite_var_ref_expression(name, subscripts, *span),
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.rewrite_builtin_call_expression(*function, args, *span),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.rewrite_function_call_expression(name, args, *is_constructor, *span),
            rumoca_core::Expression::Literal { value, span } => {
                Ok(rumoca_core::Expression::Literal {
                    value: value.clone(),
                    span: *span,
                })
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
            } => self.rewrite_if_expression(branches, else_branch, *span),
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                span,
            } => self.rewrite_array_expression(elements, *is_matrix, *span),
            rumoca_core::Expression::Tuple { elements, span } => {
                self.rewrite_tuple_expression(elements, *span)
            }
            rumoca_core::Expression::Range {
                start,
                step,
                end,
                span,
            } => self.rewrite_range_expression(start, step.as_deref(), end, *span),
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                span,
            } => {
                self.rewrite_array_comprehension_expression(expr, indices, filter.as_deref(), *span)
            }
            rumoca_core::Expression::Index {
                base,
                subscripts,
                span,
            } => self.rewrite_index_expression(base, subscripts, *span),
            rumoca_core::Expression::FieldAccess { base, field, span } => {
                self.rewrite_field_access_expression(base, field, *span)
            }
            rumoca_core::Expression::Empty { span } => {
                Ok(rumoca_core::Expression::Empty { span: *span })
            }
        }
    }

    fn rewrite_binary_expression(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(self.rewrite_expression(lhs)?),
            rhs: Box::new(self.rewrite_expression(rhs)?),
            span,
        })
    }

    fn rewrite_unary_expression(
        &self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(self.rewrite_expression(rhs)?),
            span,
        })
    }

    fn rewrite_builtin_call_expression(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::BuiltinCall {
            function,
            args: self.rewrite_expressions(args)?,
            span,
        })
    }

    fn rewrite_function_call_expression(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: self.rewrite_expressions(args)?,
            is_constructor,
            span,
        })
    }

    fn rewrite_if_expression(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::If {
            branches: self.rewrite_branches(branches)?,
            else_branch: Box::new(self.rewrite_expression(else_branch)?),
            span,
        })
    }

    fn rewrite_array_expression(
        &self,
        elements: &[rumoca_core::Expression],
        is_matrix: bool,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Array {
            elements: self.rewrite_expressions(elements)?,
            is_matrix,
            span,
        })
    }

    fn rewrite_tuple_expression(
        &self,
        elements: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Tuple {
            elements: self.rewrite_expressions(elements)?,
            span,
        })
    }

    fn rewrite_range_expression(
        &self,
        start: &rumoca_core::Expression,
        step: Option<&rumoca_core::Expression>,
        end: &rumoca_core::Expression,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Range {
            start: Box::new(self.rewrite_expression(start)?),
            step: step
                .map(|step| self.rewrite_expression(step).map(Box::new))
                .transpose()?,
            end: Box::new(self.rewrite_expression(end)?),
            span,
        })
    }

    fn rewrite_array_comprehension_expression(
        &self,
        expr: &rumoca_core::Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&rumoca_core::Expression>,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(self.rewrite_expression(expr)?),
            indices: self.rewrite_comprehension_indices(indices)?,
            filter: filter
                .map(|filter| self.rewrite_expression(filter).map(Box::new))
                .transpose()?,
            span,
        })
    }

    fn rewrite_index_expression(
        &self,
        base: &rumoca_core::Expression,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::Index {
            base: Box::new(self.rewrite_expression(base)?),
            subscripts: self.rewrite_subscripts(subscripts)?,
            span,
        })
    }

    fn rewrite_field_access_expression(
        &self,
        base: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        Ok(rumoca_core::Expression::FieldAccess {
            base: Box::new(self.rewrite_expression(base)?),
            field: field.to_owned(),
            span,
        })
    }

    fn rewrite_expressions(
        &self,
        exprs: &[rumoca_core::Expression],
    ) -> Result<Vec<rumoca_core::Expression>, ToDaeError> {
        exprs
            .iter()
            .map(|expr| self.rewrite_expression(expr))
            .collect()
    }

    fn rewrite_branches(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    ) -> Result<Vec<(rumoca_core::Expression, rumoca_core::Expression)>, ToDaeError> {
        branches
            .iter()
            .map(|(condition, value)| {
                Ok((
                    self.rewrite_expression(condition)?,
                    self.rewrite_expression(value)?,
                ))
            })
            .collect()
    }

    fn rewrite_comprehension_indices(
        &self,
        indices: &[rumoca_core::ComprehensionIndex],
    ) -> Result<Vec<rumoca_core::ComprehensionIndex>, ToDaeError> {
        indices
            .iter()
            .map(|index| {
                Ok(rumoca_core::ComprehensionIndex {
                    name: index.name.clone(),
                    range: self.rewrite_expression(&index.range)?,
                })
            })
            .collect()
    }

    fn rewrite_subscripts(
        &self,
        subscripts: &[rumoca_core::Subscript],
    ) -> Result<Vec<rumoca_core::Subscript>, ToDaeError> {
        subscripts
            .iter()
            .map(|subscript| self.rewrite_subscript(subscript))
            .collect()
    }

    fn rewrite_subscript(
        &self,
        subscript: &rumoca_core::Subscript,
    ) -> Result<rumoca_core::Subscript, ToDaeError> {
        match subscript {
            rumoca_core::Subscript::Index { value, span } => Ok(rumoca_core::Subscript::Index {
                value: *value,
                span: *span,
            }),
            rumoca_core::Subscript::Colon { span } => {
                Ok(rumoca_core::Subscript::Colon { span: *span })
            }
            rumoca_core::Subscript::Expr { expr, span } => Ok(rumoca_core::Subscript::Expr {
                expr: Box::new(self.rewrite_expression(expr)?),
                span: *span,
            }),
        }
    }

    fn rewrite_var_ref_expression(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, ToDaeError> {
        if let Some(rewritten) = self.rewrite_embedded_scalar_ref(name, subscripts, span)? {
            return Ok(rewritten);
        }
        let rewritten_name = self.reference_with_flat_variable_metadata(name);
        Ok(rumoca_core::Expression::VarRef {
            name: rewritten_name,
            subscripts: self.rewrite_subscripts(subscripts)?,
            span,
        })
    }

    fn reference_with_flat_variable_metadata(
        &self,
        name: &rumoca_core::Reference,
    ) -> rumoca_core::Reference {
        self.flat
            .variables
            .get(name.var_name())
            .and_then(|var| var.component_ref.clone())
            .or_else(|| {
                self.flat
                    .record_instances
                    .get(name.var_name())
                    .map(|record| record.component_ref.clone())
            })
            .map(|component_ref| {
                rumoca_core::Reference::with_component_reference(name.as_str(), component_ref)
            })
            .unwrap_or_else(|| name.clone())
    }

    fn rewrite_embedded_scalar_ref(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, ToDaeError> {
        if !subscripts.is_empty() {
            return Ok(None);
        }
        let Some(scalar_name) = rumoca_core::parse_scalar_name(name.as_str()) else {
            return Ok(None);
        };
        let base_var = self
            .flat
            .variables
            .get(&rumoca_core::VarName::new(scalar_name.base));
        let Some(base_var) = base_var else {
            return Ok(None);
        };
        if base_var.dims.is_empty() {
            return Ok(None);
        }
        let Some(component_ref) = base_var.component_ref.clone() else {
            return Ok(None);
        };
        let owner = scalar_name_owner_span(span, base_var.source_span);
        let subscript_context = "DAE embedded scalar reference subscript";
        let subscripts = scalar_name
            .indices
            .into_iter()
            .map(|index| {
                rumoca_core::Subscript::try_generated_index(index, owner, subscript_context)
                    .map_err(|err| missing_generated_subscript_provenance(err.to_string(), owner))
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(scalar_name.base, component_ref),
            subscripts,
            span,
        }))
    }
}

fn scalar_name_owner_span(
    expr_span: rumoca_core::Span,
    variable_span: rumoca_core::Span,
) -> rumoca_core::Span {
    if expr_span.is_dummy() {
        variable_span
    } else {
        expr_span
    }
}

fn missing_generated_subscript_provenance(detail: String, span: rumoca_core::Span) -> ToDaeError {
    if span.is_dummy() {
        ToDaeError::runtime_metadata_violation(detail)
    } else {
        ToDaeError::runtime_metadata_violation_at(detail, span)
    }
}

#[derive(Clone)]
struct DaeReferenceMetadata {
    component_ref: Option<rumoca_core::ComponentReference>,
    origin: dae::VariableOrigin,
    source_span: rumoca_core::Span,
}

struct DaeReferenceScope {
    variables: IndexMap<rumoca_core::VarName, DaeReferenceMetadata>,
    aggregate_prefixes: IndexMap<rumoca_core::VarName, DaeReferenceMetadata>,
    enum_literal_ordinals: IndexMap<String, i64>,
}

impl DaeReferenceScope {
    fn new(dae: &dae::Dae) -> Self {
        let mut variables = IndexMap::new();
        let mut aggregate_prefixes = IndexMap::new();
        Self::insert_partition(&mut variables, &dae.variables.states);
        Self::insert_partition(&mut variables, &dae.variables.algebraics);
        Self::insert_partition(&mut variables, &dae.variables.inputs);
        Self::insert_partition(&mut variables, &dae.variables.outputs);
        Self::insert_partition(&mut variables, &dae.variables.parameters);
        Self::insert_partition(&mut variables, &dae.variables.constants);
        Self::insert_partition(&mut variables, &dae.variables.discrete_reals);
        Self::insert_partition(&mut variables, &dae.variables.discrete_valued);
        for metadata in variables.values() {
            Self::insert_aggregate_prefixes(&mut aggregate_prefixes, metadata);
        }
        Self {
            variables,
            aggregate_prefixes,
            enum_literal_ordinals: dae.symbols.enum_literal_ordinals.clone(),
        }
    }

    fn insert_partition(
        variables: &mut IndexMap<rumoca_core::VarName, DaeReferenceMetadata>,
        partition: &IndexMap<rumoca_core::VarName, dae::Variable>,
    ) {
        variables.extend(partition.iter().map(|(name, variable)| {
            (
                name.clone(),
                DaeReferenceMetadata {
                    component_ref: variable.component_ref.clone(),
                    origin: variable.origin,
                    source_span: variable.source_span,
                },
            )
        }));
    }

    fn insert_aggregate_prefixes(
        aggregate_prefixes: &mut IndexMap<rumoca_core::VarName, DaeReferenceMetadata>,
        metadata: &DaeReferenceMetadata,
    ) {
        let Some(component_ref) = metadata.component_ref.as_ref() else {
            return;
        };
        if component_ref.parts.len() < 2 {
            return;
        }
        for end in 1..component_ref.parts.len() {
            let prefix_parts = &component_ref.parts[..end];
            if end == 1 && prefix_parts[0].subs.is_empty() {
                continue;
            }
            let prefix = rumoca_core::ComponentReference {
                local: component_ref.local,
                span: component_ref.span,
                parts: prefix_parts.to_vec(),
                def_id: None,
            };
            let key = prefix.to_var_name();
            aggregate_prefixes
                .entry(key)
                .or_insert_with(|| DaeReferenceMetadata {
                    component_ref: Some(prefix),
                    origin: metadata.origin,
                    source_span: metadata.source_span,
                });
        }
    }

    fn reference_for(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Reference, ToDaeError> {
        if name.is_generated() {
            return Ok(name.clone());
        }
        if name.as_str() == "time" {
            return Ok(rumoca_core::Reference::generated("time"));
        }
        if self.enum_literal_ordinals.contains_key(name.as_str()) {
            return Ok(name.clone());
        }
        if let Some(metadata) = self.variables.get(name.var_name()) {
            return self.reference_from_metadata(name.var_name(), name.as_str(), metadata, span);
        }
        if let Some(metadata) = self.aggregate_prefixes.get(name.var_name()) {
            if name.target_def_id().is_some() {
                return Ok(name.clone());
            }
            return self.reference_from_metadata(name.var_name(), name.as_str(), metadata, span);
        }
        // Element reference to an aggregate variable (`sum.u[2]` while the
        // declared variable is `sum.u`): derive the base structurally from
        // the parts (last part without its subscripts) and enrich def-ids
        // from the base variable's metadata, exactly as exact-name hits do.
        if let Some(enriched) = self.enrich_element_reference(name, span)? {
            return Ok(enriched);
        }
        if name.has_structure() {
            return Ok(name.clone());
        }
        // Flatten's exit pass attaches structured references to every
        // rendered variable reference, so an unstructured leftover is a
        // producer bug, not something to recover by parsing the name.
        Err(ToDaeError::UnresolvedReference {
            name: name.as_str().to_string(),
            span: rumoca_core::span_to_source_span(span),
        })
    }

    /// Enrich an element reference whose base is a declared aggregate
    /// variable: the base is derived from the structured parts (no name
    /// parsing), its metadata supplies the def-ids, and the element's own
    /// subscripts are re-applied.
    fn enrich_element_reference(
        &self,
        name: &rumoca_core::Reference,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Reference>, ToDaeError> {
        let Some(component_ref) = name.component_ref() else {
            return Ok(None);
        };
        let Some(last) = component_ref.parts.last() else {
            return Ok(None);
        };
        if last.subs.is_empty() {
            return Ok(None);
        }
        let mut base_ref = component_ref.clone();
        let element_subs = match base_ref.parts.last_mut() {
            Some(part) => std::mem::take(&mut part.subs),
            None => return Ok(None),
        };
        let base_name = rumoca_core::VarName::new(
            rumoca_core::ComponentPath::from_component_reference(&base_ref).to_flat_string(),
        );
        let Some(metadata) = self.variables.get(&base_name) else {
            return Ok(None);
        };
        if metadata.origin == dae::VariableOrigin::Generated {
            return Ok(Some(rumoca_core::Reference::generated(name.as_str())));
        }
        let Some(mut enriched) = metadata.component_ref.clone() else {
            return Ok(None);
        };
        let Some(part) = enriched.parts.last_mut() else {
            return Ok(None);
        };
        part.subs.extend(element_subs);
        let _ = span;
        Ok(Some(rumoca_core::Reference::with_component_reference(
            name.as_str(),
            enriched,
        )))
    }

    fn reference_from_metadata(
        &self,
        target: &rumoca_core::VarName,
        rendered: &str,
        metadata: &DaeReferenceMetadata,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Reference, ToDaeError> {
        match metadata.origin {
            dae::VariableOrigin::Generated => Ok(rumoca_core::Reference::generated(rendered)),
            dae::VariableOrigin::Source => {
                let component_ref =
                    metadata.component_ref.clone().ok_or_else(|| {
                        ToDaeError::runtime_contract_violation_at(
                            format!(
                                "source DAE variable `{target}` lost structured component-reference metadata"
                            ),
                            span,
                        )
                    })?;
                Ok(rumoca_core::Reference::with_component_reference(
                    rendered,
                    component_ref,
                ))
            }
        }
    }
}

struct DaeMetadataReferenceRewriter {
    scope: DaeReferenceScope,
    local_names: Vec<IndexSet<String>>,
    error: Option<ToDaeError>,
}

impl DaeMetadataReferenceRewriter {
    fn is_local_name(&self, name: &rumoca_core::Reference) -> bool {
        self.local_names
            .iter()
            .rev()
            .any(|scope| scope.contains(name.as_str()))
    }

    fn rewrite_equations(&mut self, equations: &mut [dae::Equation]) {
        for equation in equations {
            if let Err(error) = self.rewrite_equation_lhs(equation) {
                self.error = Some(error);
                return;
            }
            equation.rhs = self.rewrite_expression(&equation.rhs);
            if self.error.is_some() {
                return;
            }
        }
    }

    fn rewrite_equation_lhs(&self, equation: &mut dae::Equation) -> Result<(), ToDaeError> {
        let Some(lhs) = equation.lhs.as_ref() else {
            return Ok(());
        };
        equation.lhs = Some(self.scope.reference_for(lhs, equation.span)?);
        Ok(())
    }

    fn rewrite_expression_slots(&mut self, expressions: &mut [rumoca_core::Expression]) {
        for expression in expressions {
            *expression = self.rewrite_expression(expression);
            if self.error.is_some() {
                return;
            }
        }
    }

    fn rewrite_event_actions(&mut self, actions: &mut [dae::DaeEventAction]) {
        for action in actions {
            action.condition = self.rewrite_expression(&action.condition);
            if self.error.is_some() {
                return;
            }
        }
    }

    fn rewrite_variable_attributes(&mut self, variables: &mut dae::DaeVariables) {
        self.rewrite_variable_partition_attributes(&mut variables.states);
        self.rewrite_variable_partition_attributes(&mut variables.algebraics);
        self.rewrite_variable_partition_attributes(&mut variables.inputs);
        self.rewrite_variable_partition_attributes(&mut variables.outputs);
        self.rewrite_variable_partition_attributes(&mut variables.parameters);
        self.rewrite_variable_partition_attributes(&mut variables.constants);
        self.rewrite_variable_partition_attributes(&mut variables.discrete_reals);
        self.rewrite_variable_partition_attributes(&mut variables.discrete_valued);
    }

    fn rewrite_variable_partition_attributes(
        &mut self,
        partition: &mut IndexMap<rumoca_core::VarName, dae::Variable>,
    ) {
        for variable in partition.values_mut() {
            self.rewrite_optional_expression(&mut variable.start);
            self.rewrite_optional_expression(&mut variable.min);
            self.rewrite_optional_expression(&mut variable.max);
            self.rewrite_optional_expression(&mut variable.nominal);
            if self.error.is_some() {
                return;
            }
        }
    }

    fn rewrite_optional_expression(&mut self, expression: &mut Option<rumoca_core::Expression>) {
        if let Some(value) = expression {
            *value = self.rewrite_expression(value);
        }
    }
}

impl ExpressionRewriter for DaeMetadataReferenceRewriter {
    fn walk_array_comprehension_expression(
        &mut self,
        expr: &rumoca_core::Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&rumoca_core::Expression>,
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rewritten_indices = indices
            .iter()
            .map(|index| rumoca_core::ComprehensionIndex {
                name: index.name.clone(),
                range: self.rewrite_expression(&index.range),
            })
            .collect::<Vec<_>>();
        self.local_names
            .push(indices.iter().map(|index| index.name.clone()).collect());
        let rewritten_expr = self.rewrite_expression(expr);
        let rewritten_filter = filter.map(|filter| Box::new(self.rewrite_expression(filter)));
        self.local_names.pop();
        rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(rewritten_expr),
            indices: rewritten_indices,
            filter: rewritten_filter,
            span,
        }
    }

    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if self.error.is_some() {
            return rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            };
        }
        if self.is_local_name(name) {
            return rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            };
        }
        match self.scope.reference_for(name, span) {
            Ok(reference) => rumoca_core::Expression::VarRef {
                name: reference,
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            },
            Err(error) => {
                self.error = Some(error);
                rumoca_core::Expression::VarRef {
                    name: name.clone(),
                    subscripts: self.rewrite_subscripts(subscripts),
                    span,
                }
            }
        }
    }
}

pub(crate) fn remap_flat_structured_equations(
    structured_equations: &[flat::StructuredEquationFamily],
    flat_to_dae_index: &IndexMap<usize, usize>,
) -> Vec<dae::StructuredEquationFamily> {
    structured_equations
        .iter()
        .filter_map(|family| remap_flat_structured_equation(family, flat_to_dae_index))
        .collect()
}

fn remap_flat_structured_equation(
    family: &flat::StructuredEquationFamily,
    flat_to_dae_index: &IndexMap<usize, usize>,
) -> Option<dae::StructuredEquationFamily> {
    let mut flat_idx = family.first_equation_index;
    let mut first_dae_index = None;
    let mut expected_next_dae_index = None;
    let point_count = family.domain.scalar_count().ok()?;
    let mut equations_per_point = None;

    for _ in 0..point_count {
        let mut dae_equation_count = 0;
        for source_idx in flat_idx..flat_idx + family.equations_per_point {
            let dae_idx = *flat_to_dae_index.get(&source_idx)?;
            if let Some(expected) = expected_next_dae_index
                && dae_idx != expected
            {
                return None;
            }
            first_dae_index.get_or_insert(dae_idx);
            expected_next_dae_index = Some(dae_idx + 1);
            dae_equation_count += 1;
        }
        if dae_equation_count == 0 {
            return None;
        }
        if equations_per_point
            .replace(dae_equation_count)
            .is_some_and(|count| count != dae_equation_count)
        {
            return None;
        }
        flat_idx += family.equations_per_point;
    }

    Some(dae::StructuredEquationFamily {
        domain: family.domain.clone(),
        first_equation_index: first_dae_index?,
        equations_per_point: equations_per_point?,
        span: family.span,
        origin: family.origin.to_string(),
        regular: family.regular.clone(),
        template: family.template.clone(),
        interiors_materialized: family.interiors_materialized,
    })
}

pub(crate) fn flat_to_dae_function_map(
    functions: &rumoca_ir_flat::VarNameIndexMap<rumoca_core::Function>,
) -> IndexMap<rumoca_core::VarName, rumoca_core::Function> {
    functions
        .iter()
        .map(|(name, function)| (name.clone(), function.clone()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span(start: usize, end: usize) -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("convert_test.mo"),
            start,
            end,
        )
    }

    fn simple_component_ref(
        name: &str,
        span: rumoca_core::Span,
    ) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span,
                subs: Vec::new(),
            }],
            def_id: Some(rumoca_core::DefId::new(1)),
        }
    }

    fn array_flat_variable(name: &str, span: rumoca_core::Span) -> flat::Variable {
        flat::Variable {
            name: rumoca_core::VarName::new(name),
            component_ref: Some(simple_component_ref(name, span)),
            source_span: span,
            dims: vec![3],
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }
    }

    #[test]
    fn structured_target_reference_uses_flat_variable_span_for_dummy_owner() {
        let span = test_span(4, 9);
        let mut flat = flat::Model::new();
        flat.variables.insert(
            rumoca_core::VarName::new("x"),
            array_flat_variable("x", span),
        );

        let reference = structured_target_reference_with_flat_metadata(
            &rumoca_core::VarName::new("x[2]"),
            rumoca_core::Span::DUMMY,
            &flat,
        )
        .expect("scalar target should inherit source-backed variable provenance");

        let component_ref = reference
            .component_ref()
            .expect("scalar target should keep structured metadata");
        assert_eq!(component_ref.span, span);
        assert!(matches!(
            component_ref.parts.as_slice(),
            [rumoca_core::ComponentRefPart { ident, subs, span: part_span }]
                if ident == "x"
                    && *part_span == span
                    && matches!(
                        subs.as_slice(),
                        [rumoca_core::Subscript::Index { value: 2, span: subscript_span }]
                            if *subscript_span == span
                    )
        ));
    }

    #[test]
    fn structured_target_reference_rejects_dummy_owner_without_flat_provenance() {
        let flat = flat::Model::new();

        let err = structured_target_reference_with_flat_metadata(
            &rumoca_core::VarName::new("x[2]"),
            rumoca_core::Span::DUMMY,
            &flat,
        )
        .expect_err("unprovenanced source-derived target should fail");

        assert!(
            matches!(err, ToDaeError::RuntimeMetadataViolation { ref detail } if
                detail.contains("missing source provenance for DAE structured target reference")
                    && detail.contains("x[2]")),
            "unexpected provenance error: {err:?}"
        );
    }

    #[test]
    fn structured_target_reference_uses_dae_variable_span_for_dummy_owner() {
        let span = test_span(12, 18);
        let mut dae = dae::Dae::new();
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new("flag"),
            dae::Variable {
                name: rumoca_core::VarName::new("flag"),
                source_span: span,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let reference = structured_target_reference_with_dae_metadata(
            &rumoca_core::VarName::new("flag"),
            rumoca_core::Span::DUMMY,
            &dae,
        )
        .expect("guarded event target should inherit DAE variable provenance");

        let component_ref = reference
            .component_ref()
            .expect("target should keep structured metadata");
        assert_eq!(component_ref.span, span);
    }

    #[test]
    fn flat_to_dae_expression_attaches_exact_variable_component_ref() -> Result<(), ToDaeError> {
        let mut flat = flat::Model::new();
        let span = test_span(20, 27);
        let component_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "vehicle".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "motor".to_string(),
                    span,
                    subs: vec![rumoca_core::Subscript::generated_index(1, span)],
                },
                rumoca_core::ComponentRefPart {
                    ident: "tau_inv".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(166)),
        };
        flat.variables.insert(
            rumoca_core::VarName::new("vehicle.motor[1].tau_inv"),
            flat::Variable {
                name: rumoca_core::VarName::new("vehicle.motor[1].tau_inv"),
                component_ref: Some(component_ref),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let expr = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("vehicle.motor[1].tau_inv"),
            subscripts: Vec::new(),
            span,
        };

        let rewritten = flat_to_dae_expression_with_refs(&expr, &flat)?;

        assert!(
            matches!(
                rewritten,
                rumoca_core::Expression::VarRef { name, .. }
                    if name.component_ref().is_some_and(|component_ref| {
                        component_ref.def_id == Some(rumoca_core::DefId::new(166))
                            && component_ref.parts[1].ident == "motor"
                            && matches!(
                                component_ref.parts[1].subs.as_slice(),
                                [rumoca_core::Subscript::Index { value: 1, .. }]
                            )
                    })
            ),
            "DAE expression conversion should preserve exact Flat variable component references"
        );
        Ok(())
    }

    #[test]
    fn flat_to_dae_expression_attaches_exact_record_instance_component_ref()
    -> Result<(), ToDaeError> {
        let mut flat = flat::Model::new();
        let span = test_span(28, 35);
        let aggregate_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "source".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "value".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(177)),
        };
        flat.record_instances.insert(
            rumoca_core::VarName::new("source.value"),
            flat::RecordInstance {
                component_ref: aggregate_ref,
                source_span: span,
                canonical_type_id: rumoca_core::TypeId::new(7),
                type_name: "Complex".to_string(),
                type_def_id: rumoca_core::DefId::new(70),
                dims: Vec::new(),
            },
        );
        let expr = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("source.value"),
            subscripts: Vec::new(),
            span,
        };

        let rewritten = flat_to_dae_expression_with_refs(&expr, &flat)?;

        assert!(matches!(
            rewritten,
            rumoca_core::Expression::VarRef { name, .. }
                if name.target_def_id() == Some(rumoca_core::DefId::new(177))
        ));
        Ok(())
    }

    #[test]
    fn dae_metadata_attachment_preserves_resolved_record_aggregate_identity() {
        let span = test_span(36, 43);
        let aggregate_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "source".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "value".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(177)),
        };
        let mut field_ref = aggregate_ref.clone();
        field_ref.parts.push(rumoca_core::ComponentRefPart {
            ident: "re".to_string(),
            span,
            subs: Vec::new(),
        });
        field_ref.def_id = Some(rumoca_core::DefId::new(178));
        let mut dae = dae::Dae::new();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("source.value.re"),
            dae::Variable {
                name: rumoca_core::VarName::new("source.value.re"),
                component_ref: Some(field_ref),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(span)
            },
        );
        dae.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "source.value",
                    aggregate_ref,
                ),
                subscripts: Vec::new(),
                span,
            },
            span,
            origin: "record aggregate fixture".to_string(),
            scalar_count: 1,
        });

        attach_dae_reference_metadata(&mut dae).expect("aggregate identity should remain resolved");

        let rumoca_core::Expression::VarRef { name, .. } = &dae.continuous.equations[0].rhs else {
            panic!("expected aggregate variable reference");
        };
        assert_eq!(name.target_def_id(), Some(rumoca_core::DefId::new(177)));
    }

    #[test]
    fn flat_to_dae_expression_uses_owner_span_for_embedded_scalar_indices() -> Result<(), ToDaeError>
    {
        let mut flat = flat::Model::new();
        let declaration_span = test_span(1, 4);
        let expr_span = test_span(10, 16);
        flat.variables.insert(
            rumoca_core::VarName::new("arr"),
            array_flat_variable("arr", declaration_span),
        );
        let expr = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("arr[2]"),
            subscripts: Vec::new(),
            span: expr_span,
        };

        let rewritten = flat_to_dae_expression_with_refs(&expr, &flat)?;

        assert!(
            matches!(
                rewritten,
                rumoca_core::Expression::VarRef { name, subscripts, span }
                    if name.as_str() == "arr"
                        && span == expr_span
                        && matches!(
                            subscripts.as_slice(),
                            [rumoca_core::Subscript::Index { value: 2, span }]
                                if *span == expr_span
                        )
            ),
            "embedded scalar reference subscripts should inherit expression provenance"
        );
        Ok(())
    }

    #[test]
    fn flat_to_dae_expression_rejects_unspanned_embedded_scalar_indices() {
        let mut flat = flat::Model::new();
        flat.variables.insert(
            rumoca_core::VarName::new("arr"),
            array_flat_variable("arr", rumoca_core::Span::DUMMY),
        );
        let expr = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("arr[2]"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        let result = flat_to_dae_expression_with_refs(&expr, &flat);
        assert!(
            result.is_err(),
            "unspanned embedded scalar reference should fail"
        );
        let err = match result {
            Err(err) => err,
            Ok(_) => return,
        };

        assert!(
            matches!(err, ToDaeError::RuntimeMetadataViolation { ref detail } if
                detail.contains("DAE embedded scalar reference subscript")),
            "unexpected missing-provenance error: {err:?}"
        );
    }

    #[test]
    fn dae_metadata_attachment_replaces_partial_source_component_ref() {
        let name = rumoca_core::VarName::new("adder.gate.x");
        let span = test_span(40, 47);
        let authoritative_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "adder".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "gate".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "x".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(6733)),
        };
        let partial_ref = rumoca_core::ComponentReference {
            def_id: None,
            ..authoritative_ref.clone()
        };
        let mut dae = dae::Dae::new();
        dae.variables.discrete_valued.insert(
            name.clone(),
            dae::Variable {
                name: name.clone(),
                component_ref: Some(authoritative_ref),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        dae.discrete.valued_updates.push(dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(name.as_str(), partial_ref),
                subscripts: Vec::new(),
                span,
            },
            span,
            origin: "test".to_string(),
            scalar_count: 1,
        });

        attach_dae_reference_metadata(&mut dae).expect("metadata attachment should succeed");

        let rumoca_core::Expression::VarRef { name, .. } = &dae.discrete.valued_updates[0].rhs
        else {
            panic!("expected variable reference");
        };
        assert_eq!(name.target_def_id(), Some(rumoca_core::DefId::new(6733)));
    }

    #[test]
    fn dae_metadata_attachment_recovers_record_prefix_from_scalar_fields() {
        let field_name = rumoca_core::VarName::new("mp.modelcard.VTO");
        let span = test_span(60, 67);
        let field_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "mp".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "modelcard".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "VTO".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(91)),
        };
        let mut dae = dae::Dae::new();
        dae.variables.parameters.insert(
            field_name.clone(),
            dae::Variable {
                name: field_name,
                component_ref: Some(field_ref),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        dae.variables.parameters.insert(
            rumoca_core::VarName::new("mp.p.m_vt0"),
            dae::Variable {
                name: rumoca_core::VarName::new("mp.p.m_vt0"),
                start: Some(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("renameParameters"),
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("mp.modelcard"),
                        subscripts: Vec::new(),
                        span,
                    }],
                    is_constructor: false,
                    span,
                }),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        attach_dae_reference_metadata(&mut dae).expect("record prefix should resolve from fields");

        let Some(rumoca_core::Expression::FunctionCall { args, .. }) = dae
            .variables
            .parameters
            .get(&rumoca_core::VarName::new("mp.p.m_vt0"))
            .and_then(|variable| variable.start.as_ref())
        else {
            panic!("expected function-call start expression");
        };
        let Some(rumoca_core::Expression::VarRef { name, .. }) = args.first() else {
            panic!("expected record argument");
        };
        let Some(component_ref) = name.component_ref() else {
            panic!("record argument should have structured component metadata");
        };
        assert_eq!(component_ref.parts.len(), 2);
        assert_eq!(component_ref.parts[0].ident, "mp");
        assert_eq!(component_ref.parts[1].ident, "modelcard");
    }

    #[test]
    fn dae_metadata_attachment_recovers_subscripted_array_prefix() {
        let field_name = rumoca_core::VarName::new("J[1,1].value");
        let span = test_span(80, 87);
        let field_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "J".to_string(),
                    span,
                    subs: vec![
                        rumoca_core::Subscript::Index { value: 1, span },
                        rumoca_core::Subscript::Index { value: 1, span },
                    ],
                },
                rumoca_core::ComponentRefPart {
                    ident: "value".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(92)),
        };
        let mut dae = dae::Dae::new();
        dae.variables.parameters.insert(
            field_name.clone(),
            dae::Variable {
                name: field_name,
                component_ref: Some(field_ref),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        dae.variables.parameters.insert(
            rumoca_core::VarName::new("omega"),
            dae::Variable {
                name: rumoca_core::VarName::new("omega"),
                start: Some(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("J[1,1]"),
                    subscripts: Vec::new(),
                    span,
                }),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        attach_dae_reference_metadata(&mut dae).expect("array prefix should resolve from fields");

        let Some(rumoca_core::Expression::VarRef { name, .. }) = dae
            .variables
            .parameters
            .get(&rumoca_core::VarName::new("omega"))
            .and_then(|variable| variable.start.as_ref())
        else {
            panic!("expected variable-reference start expression");
        };
        let Some(component_ref) = name.component_ref() else {
            panic!("array element should have structured component metadata");
        };
        assert_eq!(component_ref.parts.len(), 1);
        assert_eq!(component_ref.parts[0].ident, "J");
        assert_eq!(component_ref.parts[0].subs.len(), 2);
    }

    #[test]
    fn dae_metadata_attachment_does_not_invent_bare_component_prefixes() {
        let field_name = rumoca_core::VarName::new("mp.modelcard.VTO");
        let span = test_span(100, 107);
        let field_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "mp".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "modelcard".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "VTO".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(93)),
        };
        let mut dae = dae::Dae::new();
        dae.variables.parameters.insert(
            field_name.clone(),
            dae::Variable {
                name: field_name,
                component_ref: Some(field_ref),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        dae.variables.parameters.insert(
            rumoca_core::VarName::new("uses_mp"),
            dae::Variable {
                name: rumoca_core::VarName::new("uses_mp"),
                start: Some(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("mp"),
                    subscripts: Vec::new(),
                    span,
                }),
                origin: dae::VariableOrigin::Source,
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let err =
            attach_dae_reference_metadata(&mut dae).expect_err("bare component should not resolve");
        assert!(
            matches!(&err, ToDaeError::UnresolvedReference { name, .. } if name == "mp"),
            "expected unresolved reference diagnostic, got {err:?}"
        );
    }
}
