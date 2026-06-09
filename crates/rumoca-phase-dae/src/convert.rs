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
) -> rumoca_core::Expression {
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
pub(crate) fn structured_target_reference(target: &rumoca_core::VarName) -> rumoca_core::Reference {
    match rumoca_core::component_reference_from_flat_name(target, rumoca_core::Span::DUMMY) {
        Some(component_ref) => rumoca_core::Reference::from_component_reference(component_ref),
        None => target.clone().into(),
    }
}

pub(crate) fn dae_to_flat_expression(expr: &rumoca_core::Expression) -> rumoca_core::Expression {
    expr.clone()
}

struct DaeReferenceRewriter<'a> {
    flat: &'a flat::Model,
}

impl ExpressionRewriter for DaeReferenceRewriter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        if let Some(rewritten) = self.rewrite_embedded_scalar_ref(name, subscripts, span) {
            return rewritten;
        }
        let rewritten_name = self.reference_with_flat_variable_metadata(name);
        rumoca_core::Expression::VarRef {
            name: rewritten_name,
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

impl DaeReferenceRewriter<'_> {
    fn reference_with_flat_variable_metadata(
        &self,
        name: &rumoca_core::Reference,
    ) -> rumoca_core::Reference {
        self.flat
            .variables
            .get(name.var_name())
            .and_then(|var| var.component_ref.clone())
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
    ) -> Option<rumoca_core::Expression> {
        if !subscripts.is_empty() {
            return None;
        }
        let scalar_name = rumoca_core::parse_scalar_name(name.as_str())?;
        let base_var = self
            .flat
            .variables
            .get(&rumoca_core::VarName::new(scalar_name.base))?;
        if base_var.dims.is_empty() {
            return None;
        }
        let component_ref = base_var.component_ref.clone()?;
        Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(scalar_name.base, component_ref),
            subscripts: scalar_name
                .indices
                .into_iter()
                .map(|index| rumoca_core::Subscript::generated_index(index, span))
                .collect(),
            span,
        })
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
            return self.reference_from_metadata(name.var_name(), name.as_str(), metadata, span);
        }
        if name.has_structure() {
            return Ok(name.clone());
        }
        // Bridge for the remaining name-encoded scalarized references:
        // flatten's connector/array expansion still renders element names
        // such as `sum.u[2]` without structured parts. Once those producers
        // emit structured references, this parse goes away and any
        // unstructured leftover becomes an unresolved-reference error.
        if let Some(scalar_name) = rumoca_core::parse_scalar_name(name.as_str()) {
            let base = rumoca_core::VarName::new(scalar_name.base);
            if let Some(metadata) = self.variables.get(&base) {
                return self.scalar_reference_from_metadata(
                    name.as_str(),
                    metadata,
                    &scalar_name.indices,
                    span,
                );
            }
        }
        Err(ToDaeError::UnresolvedReference {
            name: name.as_str().to_string(),
            span: rumoca_core::span_to_source_span(span),
        })
    }

    fn scalar_reference_from_metadata(
        &self,
        rendered: &str,
        metadata: &DaeReferenceMetadata,
        indices: &[i64],
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Reference, ToDaeError> {
        match metadata.origin {
            dae::VariableOrigin::Generated => Ok(rumoca_core::Reference::generated(rendered)),
            dae::VariableOrigin::Source => {
                let mut component_ref = metadata.component_ref.clone().ok_or_else(|| {
                    ToDaeError::RuntimeContractViolation {
                        detail: format!(
                            "source DAE variable `{rendered}` lost structured component-reference metadata"
                        ),
                        span: rumoca_core::span_to_source_span(span),
                    }
                })?;
                append_generated_subscripts(
                    rendered,
                    &mut component_ref,
                    indices,
                    metadata.source_span,
                    span,
                )?;
                Ok(rumoca_core::Reference::with_component_reference(
                    rendered,
                    component_ref,
                ))
            }
        }
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
                    metadata
                        .component_ref
                        .clone()
                        .ok_or_else(|| ToDaeError::RuntimeContractViolation {
                            detail: format!(
                                "source DAE variable `{target}` lost structured component-reference metadata"
                            ),
                            span: rumoca_core::span_to_source_span(span),
                        })?;
                Ok(rumoca_core::Reference::with_component_reference(
                    rendered,
                    component_ref,
                ))
            }
        }
    }
}

fn append_generated_subscripts(
    rendered: &str,
    component_ref: &mut rumoca_core::ComponentReference,
    indices: &[i64],
    source_span: rumoca_core::Span,
    diagnostic_span: rumoca_core::Span,
) -> Result<(), ToDaeError> {
    let Some(part) = component_ref.parts.last_mut() else {
        return Err(ToDaeError::RuntimeContractViolation {
            detail: format!(
                "source DAE variable `{rendered}` has empty component-reference metadata"
            ),
            span: rumoca_core::span_to_source_span(diagnostic_span),
        });
    };
    part.subs.extend(
        indices
            .iter()
            .map(|index| rumoca_core::Subscript::generated_index(*index, source_span)),
    );
    Ok(())
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

pub(crate) fn remap_flat_for_equations(
    for_equations: &[flat::ForEquation],
    flat_to_dae_index: &IndexMap<usize, usize>,
) -> Vec<dae::ForEquation> {
    for_equations
        .iter()
        .filter_map(|for_eq| remap_flat_for_equation(for_eq, flat_to_dae_index))
        .collect()
}

fn remap_flat_for_equation(
    for_eq: &flat::ForEquation,
    flat_to_dae_index: &IndexMap<usize, usize>,
) -> Option<dae::ForEquation> {
    let mut flat_idx = for_eq.first_equation_index;
    let mut first_dae_index = None;
    let mut expected_next_dae_index = None;
    let mut iterations = Vec::with_capacity(for_eq.iterations.len());

    for iteration in &for_eq.iterations {
        let mut dae_equation_count = 0;
        for source_idx in flat_idx..flat_idx + iteration.equation_count {
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
        iterations.push(dae::ForEquationIteration {
            index_values: iteration.index_values.clone(),
            equation_count: dae_equation_count,
        });
        flat_idx += iteration.equation_count;
    }

    Some(dae::ForEquation {
        index_names: for_eq.index_names.clone(),
        first_equation_index: first_dae_index?,
        iterations,
        span: for_eq.span,
        origin: for_eq.origin.to_string(),
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

    #[test]
    fn flat_to_dae_expression_attaches_exact_variable_component_ref() {
        let mut flat = flat::Model::new();
        let component_ref = rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "vehicle".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "motor".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: vec![rumoca_core::Subscript::generated_index(
                        1,
                        rumoca_core::Span::DUMMY,
                    )],
                },
                rumoca_core::ComponentRefPart {
                    ident: "tau_inv".to_string(),
                    span: rumoca_core::Span::DUMMY,
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
                ..Default::default()
            },
        );

        let expr = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("vehicle.motor[1].tau_inv"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        let rewritten = flat_to_dae_expression_with_refs(&expr, &flat);

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
    }

    #[test]
    fn dae_metadata_attachment_replaces_partial_source_component_ref() {
        let name = rumoca_core::VarName::new("adder.gate.x");
        let authoritative_ref = rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "adder".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "gate".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "x".to_string(),
                    span: rumoca_core::Span::DUMMY,
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
                ..Default::default()
            },
        );
        dae.discrete.valued_updates.push(dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(name.as_str(), partial_ref),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
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
        let field_ref = rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "mp".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "modelcard".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "VTO".to_string(),
                    span: rumoca_core::Span::DUMMY,
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
                ..Default::default()
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
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                origin: dae::VariableOrigin::Source,
                ..Default::default()
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
        let field_ref = rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "J".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: vec![
                        rumoca_core::Subscript::Index {
                            value: 1,
                            span: rumoca_core::Span::DUMMY,
                        },
                        rumoca_core::Subscript::Index {
                            value: 1,
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                },
                rumoca_core::ComponentRefPart {
                    ident: "value".to_string(),
                    span: rumoca_core::Span::DUMMY,
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
                ..Default::default()
            },
        );
        dae.variables.parameters.insert(
            rumoca_core::VarName::new("omega"),
            dae::Variable {
                name: rumoca_core::VarName::new("omega"),
                start: Some(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("J[1,1]"),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }),
                origin: dae::VariableOrigin::Source,
                ..Default::default()
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
        let field_ref = rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "mp".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "modelcard".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "VTO".to_string(),
                    span: rumoca_core::Span::DUMMY,
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
                ..Default::default()
            },
        );
        dae.variables.parameters.insert(
            rumoca_core::VarName::new("uses_mp"),
            dae::Variable {
                name: rumoca_core::VarName::new("uses_mp"),
                start: Some(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("mp"),
                    subscripts: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                }),
                origin: dae::VariableOrigin::Source,
                ..Default::default()
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
