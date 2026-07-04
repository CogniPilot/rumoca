use indexmap::IndexSet;

use rumoca_core::{
    ComponentRefPart, ComponentReference, Expression, Reference, Span, Subscript, VarName,
};
use rumoca_ir_dae as dae;

use crate::StructuralError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DaeVariableShape {
    Dimensions(Vec<i64>),
    StructuredAggregate,
}

pub(crate) struct DaeVariableScope<'a> {
    dae: &'a dae::Dae,
}

impl<'a> DaeVariableScope<'a> {
    pub(crate) fn new(dae: &'a dae::Dae) -> Self {
        Self { dae }
    }

    pub(crate) fn exact(&self, name: &VarName) -> Option<&'a dae::Variable> {
        self.dae
            .variables
            .states
            .get(name)
            .or_else(|| self.dae.variables.algebraics.get(name))
            .or_else(|| self.dae.variables.outputs.get(name))
            .or_else(|| self.dae.variables.inputs.get(name))
            .or_else(|| self.dae.variables.parameters.get(name))
            .or_else(|| self.dae.variables.constants.get(name))
            .or_else(|| self.dae.variables.discrete_reals.get(name))
            .or_else(|| self.dae.variables.discrete_valued.get(name))
    }

    pub(crate) fn dims(&self, name: &VarName) -> Result<Vec<i64>, StructuralError> {
        if let Some(var) = self.exact(name) {
            return Ok(var.dims.clone());
        }
        // MLS §3.6.7: `time` is the predefined scalar independent variable.
        if name.as_str() == "time" {
            return Ok(Vec::new());
        }
        if let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) {
            let base_name = VarName::new(scalar.base);
            let Some(base_var) = self.exact(&base_name) else {
                return Err(missing_dae_variable_metadata(name, None));
            };
            if scalar.indices.len() > base_var.dims.len() {
                return Err(invalid_scalarized_reference(name, &base_var.dims, None));
            }
            validate_scalarized_indices(name, &scalar.indices, &base_var.dims, None)?;
            return Ok(base_var.dims[scalar.indices.len()..].to_vec());
        }
        if let Some(dims) = self.indexed_descendant_aggregate_dims(name) {
            return Ok(dims);
        }
        Err(missing_dae_variable_metadata(name, None))
    }

    pub(crate) fn size(&self, name: &VarName) -> Result<usize, StructuralError> {
        scalar_count_from_dims(name, &self.dims(name)?)
    }

    pub(crate) fn shape_for_reference(
        &self,
        name: &Reference,
    ) -> Result<DaeVariableShape, StructuralError> {
        if let Some(var) = self.exact_reference(name) {
            return Ok(DaeVariableShape::Dimensions(var.dims.clone()));
        }
        if let Some(dims) = self.indexed_reference_dims(name)? {
            return Ok(DaeVariableShape::Dimensions(dims));
        }
        if let Some(dims) = self.indexed_descendant_aggregate_dims(name.var_name()) {
            return Ok(DaeVariableShape::Dimensions(dims));
        }
        if name.as_str() == "time"
            || self.has_descendant_reference(name)
            || name
                .component_ref()
                .is_some_and(|component_ref| component_ref.parts.len() > 1)
        {
            return Ok(DaeVariableShape::StructuredAggregate);
        }
        Err(missing_dae_variable_metadata(name.var_name(), name.span()))
    }

    pub(crate) fn dims_for_reference(
        &self,
        name: &Reference,
    ) -> Result<Option<Vec<i64>>, StructuralError> {
        match self.shape_for_reference(name)? {
            DaeVariableShape::Dimensions(dims) => Ok(Some(dims)),
            DaeVariableShape::StructuredAggregate => Ok(None),
        }
    }

    pub(crate) fn size_for_reference(
        &self,
        name: &Reference,
    ) -> Result<Option<usize>, StructuralError> {
        self.dims_for_reference(name)?
            .map(|dims| scalar_count_from_dims(name.var_name(), &dims))
            .transpose()
    }

    pub(crate) fn is_indexed_component_variable(&self, name: &VarName) -> bool {
        self.exact(name)
            .and_then(|var| var.component_ref.as_ref())
            .is_some_and(component_ref_has_scalar_subscript)
    }

    pub(crate) fn reference_shares_component_base(
        &self,
        name: &Reference,
        unknown: &VarName,
    ) -> bool {
        let Some(name_ref) = name.component_ref() else {
            return false;
        };
        let Some(unknown_ref) = self
            .exact(unknown)
            .and_then(|var| var.component_ref.as_ref())
        else {
            return false;
        };
        if component_ref_has_scalar_subscript(name_ref) {
            component_refs_match_exactly(name_ref, unknown_ref)
        } else {
            component_refs_share_base(name_ref, unknown_ref)
        }
    }

    pub(crate) fn has_descendant_reference(&self, prefix: &Reference) -> bool {
        let Some(prefix) = prefix.component_ref() else {
            return false;
        };
        self.all_variables().any(|variable| {
            variable
                .component_ref
                .as_ref()
                .is_some_and(|candidate| component_ref_has_prefix(candidate, prefix))
        })
    }

    pub(crate) fn indexed_component_field_dims(
        &self,
        base: &Expression,
        field: &str,
    ) -> Option<Vec<i64>> {
        let Expression::Index {
            base, subscripts, ..
        } = base
        else {
            return None;
        };
        let Expression::VarRef {
            name,
            subscripts: base_subscripts,
            ..
        } = base.as_ref()
        else {
            return None;
        };
        if !base_subscripts.is_empty() {
            return None;
        }
        let prefix = name.component_ref()?;
        let mut matched_indices = IndexSet::new();
        for variable in self.all_variables() {
            let Some(component_ref) = &variable.component_ref else {
                continue;
            };
            if component_ref_matches_indexed_field(component_ref, prefix, subscripts, field) {
                matched_indices.insert(index_key(
                    component_ref,
                    prefix.parts.len().checked_sub(1)?,
                )?);
            }
        }
        if matched_indices.is_empty() {
            return None;
        }
        if subscripts
            .iter()
            .any(|subscript| matches!(subscript, Subscript::Colon { .. }))
        {
            Some(vec![matched_indices.len() as i64])
        } else {
            Some(Vec::new())
        }
    }

    fn all_variables(&self) -> impl Iterator<Item = &'a dae::Variable> {
        self.dae
            .variables
            .states
            .values()
            .chain(self.dae.variables.algebraics.values())
            .chain(self.dae.variables.outputs.values())
            .chain(self.dae.variables.inputs.values())
            .chain(self.dae.variables.parameters.values())
            .chain(self.dae.variables.constants.values())
            .chain(self.dae.variables.discrete_reals.values())
            .chain(self.dae.variables.discrete_valued.values())
    }

    fn indexed_descendant_aggregate_dims(&self, name: &VarName) -> Option<Vec<i64>> {
        let mut max_indices: Vec<i64> = Vec::new();
        let mut matched = false;
        for variable in self.all_variables() {
            let Some(component_ref) = &variable.component_ref else {
                continue;
            };
            let Some((stripped_name, indices)) =
                stripped_component_ref_name_and_indices(component_ref)
            else {
                continue;
            };
            if stripped_name != name.as_str() {
                continue;
            }
            matched = true;
            if max_indices.len() < indices.len() {
                max_indices.resize(indices.len(), 0);
            }
            for (slot, index) in max_indices.iter_mut().zip(indices) {
                *slot = (*slot).max(index);
            }
        }
        (matched && !max_indices.is_empty()).then_some(max_indices)
    }

    fn exact_reference(&self, name: &Reference) -> Option<&'a dae::Variable> {
        if name
            .component_ref()
            .is_some_and(component_ref_has_scalar_subscript)
        {
            self.exact(&VarName::new(name.as_str()))
                .or_else(|| self.exact(name.var_name()))
        } else {
            self.exact(name.var_name())
                .or_else(|| self.exact(&VarName::new(name.as_str())))
        }
    }

    fn indexed_reference_dims(
        &self,
        name: &Reference,
    ) -> Result<Option<Vec<i64>>, StructuralError> {
        let Some(component_ref) = name.component_ref() else {
            return Ok(None);
        };
        let mut base_ref = component_ref.clone();
        let Some(leaf) = base_ref.parts.last_mut() else {
            return Ok(None);
        };
        let scalar_indices = leaf
            .subs
            .iter()
            .filter_map(scalar_subscript_index)
            .collect::<Vec<_>>();
        leaf.subs.clear();
        if scalar_indices.is_empty() {
            return Ok(None);
        }
        let base_name = base_ref.to_var_name();
        let Some(base_var) = self.exact(&base_name) else {
            return Ok(None);
        };
        if scalar_indices.len() > base_var.dims.len() {
            return Err(invalid_scalarized_reference(
                name.var_name(),
                &base_var.dims,
                Some(component_ref.span),
            ));
        }
        validate_scalarized_indices(
            name.var_name(),
            &scalar_indices,
            &base_var.dims,
            Some(component_ref.span),
        )?;
        Ok(Some(base_var.dims[scalar_indices.len()..].to_vec()))
    }
}

pub(crate) fn scalar_count_from_dims(
    name: &VarName,
    dims: &[i64],
) -> Result<usize, StructuralError> {
    let mut count = 1usize;
    for dim in dims {
        let dim = usize::try_from(*dim).map_err(|_| invalid_dae_dimensions(name, dims))?;
        count = count
            .checked_mul(dim)
            .ok_or_else(|| invalid_dae_dimensions(name, dims))?;
    }
    Ok(count)
}

fn missing_dae_variable_metadata(name: &VarName, span: Option<Span>) -> StructuralError {
    structural_contract_violation(
        format!("missing DAE variable metadata for `{}`", name.as_str()),
        span,
    )
}

fn invalid_dae_dimensions(name: &VarName, dims: &[i64]) -> StructuralError {
    structural_contract_violation(
        format!("invalid DAE dimensions {:?} for `{}`", dims, name.as_str()),
        None,
    )
}

fn invalid_scalarized_reference(
    name: &VarName,
    base_dims: &[i64],
    span: Option<Span>,
) -> StructuralError {
    structural_contract_violation(
        format!(
            "scalarized DAE reference `{}` does not match base dimensions {:?}",
            name.as_str(),
            base_dims
        ),
        span,
    )
}

fn structural_contract_violation(reason: String, span: Option<Span>) -> StructuralError {
    match span {
        Some(span) if !span.is_dummy() => StructuralError::ContractViolation { reason, span },
        Some(_) | None => StructuralError::UnspannedContractViolation { reason },
    }
}

fn validate_scalarized_indices(
    name: &VarName,
    indices: &[i64],
    base_dims: &[i64],
    span: Option<Span>,
) -> Result<(), StructuralError> {
    for (index, dim) in indices.iter().zip(base_dims) {
        if *index < 1 || *index > *dim {
            return Err(invalid_scalarized_reference(name, base_dims, span));
        }
    }
    Ok(())
}

fn component_ref_has_prefix(candidate: &ComponentReference, prefix: &ComponentReference) -> bool {
    candidate.parts.len() > prefix.parts.len()
        && candidate
            .parts
            .iter()
            .zip(&prefix.parts)
            .all(|(candidate, prefix)| prefix_part_matches_descendant(candidate, prefix))
}

fn component_refs_share_base(lhs: &ComponentReference, rhs: &ComponentReference) -> bool {
    lhs.parts.len() == rhs.parts.len()
        && lhs
            .parts
            .iter()
            .zip(&rhs.parts)
            .all(|(lhs, rhs)| lhs.ident == rhs.ident)
}

fn component_refs_match_exactly(lhs: &ComponentReference, rhs: &ComponentReference) -> bool {
    lhs.parts.len() == rhs.parts.len()
        && lhs
            .parts
            .iter()
            .zip(&rhs.parts)
            .all(|(lhs, rhs)| part_matches_without_span(lhs, rhs))
}

fn component_ref_has_scalar_subscript(reference: &ComponentReference) -> bool {
    reference
        .parts
        .iter()
        .flat_map(|part| &part.subs)
        .any(|subscript| scalar_subscript_index(subscript).is_some())
}

fn component_ref_matches_indexed_field(
    candidate: &ComponentReference,
    prefix: &ComponentReference,
    subscripts: &[Subscript],
    field: &str,
) -> bool {
    candidate.parts.len() == prefix.parts.len() + 1
        && candidate
            .parts
            .last()
            .is_some_and(|part| part.ident == field)
        && prefix_parts_match(
            &candidate.parts[..prefix.parts.len()],
            &prefix.parts,
            subscripts,
        )
}

fn prefix_parts_match(
    candidate: &[ComponentRefPart],
    prefix: &[ComponentRefPart],
    indexed_subscripts: &[Subscript],
) -> bool {
    let Some((candidate_indexed, candidate_parents)) = candidate.split_last() else {
        return false;
    };
    let Some((prefix_indexed, prefix_parents)) = prefix.split_last() else {
        return false;
    };
    candidate_parents
        .iter()
        .zip(prefix_parents)
        .all(|(lhs, rhs)| part_matches_without_span(lhs, rhs))
        && candidate_indexed.ident == prefix_indexed.ident
        && subscripts_match_pattern(&candidate_indexed.subs, indexed_subscripts)
}

fn part_matches_without_span(lhs: &ComponentRefPart, rhs: &ComponentRefPart) -> bool {
    lhs.ident == rhs.ident && subscripts_match_pattern(&lhs.subs, &rhs.subs)
}

fn prefix_part_matches_descendant(candidate: &ComponentRefPart, prefix: &ComponentRefPart) -> bool {
    candidate.ident == prefix.ident
        && (prefix.subs.is_empty() || subscripts_match_pattern(&candidate.subs, &prefix.subs))
}

fn subscripts_match_pattern(candidate: &[Subscript], pattern: &[Subscript]) -> bool {
    candidate.len() == pattern.len()
        && candidate
            .iter()
            .zip(pattern)
            .all(|(candidate, pattern)| subscript_matches_pattern(candidate, pattern))
}

fn subscript_matches_pattern(candidate: &Subscript, pattern: &Subscript) -> bool {
    match pattern {
        Subscript::Colon { .. } => scalar_subscript_index(candidate).is_some(),
        Subscript::Index { .. } | Subscript::Expr { .. } => {
            scalar_subscript_index(candidate) == scalar_subscript_index(pattern)
        }
    }
}

fn index_key(component_ref: &ComponentReference, indexed_part: usize) -> Option<Vec<i64>> {
    component_ref
        .parts
        .get(indexed_part)?
        .subs
        .iter()
        .map(scalar_subscript_index)
        .collect()
}

fn stripped_component_ref_name_and_indices(
    component_ref: &ComponentReference,
) -> Option<(String, Vec<i64>)> {
    let mut stripped = component_ref.clone();
    let mut indices = Vec::new();
    for part in &mut stripped.parts {
        if part.subs.is_empty() {
            continue;
        }
        for subscript in &part.subs {
            indices.push(scalar_subscript_index(subscript)?);
        }
        part.subs.clear();
    }
    (!indices.is_empty()).then(|| (stripped.to_var_name().as_str().to_string(), indices))
}

fn scalar_subscript_index(subscript: &Subscript) -> Option<i64> {
    match subscript {
        Subscript::Index { value, .. } if *value > 0 => Some(*value),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } if *value > 0 => Some(*value),
            Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if value.is_finite() && value.fract() == 0.0 && *value > 0.0 => Some(*value as i64),
            _ => None,
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn part(ident: &str, subs: Vec<Subscript>) -> ComponentRefPart {
        ComponentRefPart {
            ident: ident.to_string(),
            span: Span::DUMMY,
            subs,
        }
    }

    fn index(value: i64) -> Subscript {
        Subscript::Index {
            value,
            span: Span::DUMMY,
        }
    }

    fn component_ref(parts: Vec<ComponentRefPart>) -> ComponentReference {
        ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts,
            def_id: None,
        }
    }

    fn component_ref_with_span(parts: Vec<ComponentRefPart>, span: Span) -> ComponentReference {
        ComponentReference {
            local: false,
            span,
            parts,
            def_id: None,
        }
    }

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("variable_scope_test.mo"),
            4,
            9,
        )
    }

    #[test]
    fn aggregate_prefix_matches_indexed_descendant_component_ref() {
        let prefix = component_ref(vec![
            part("rectifier", Vec::new()),
            part("ac", Vec::new()),
            part("pin", Vec::new()),
        ]);
        let candidate = component_ref(vec![
            part("rectifier", Vec::new()),
            part("ac", Vec::new()),
            part("pin", vec![index(1)]),
            part("v", Vec::new()),
        ]);

        assert!(component_ref_has_prefix(&candidate, &prefix));
    }

    #[test]
    fn indexed_prefix_requires_matching_indexed_descendant_component_ref() {
        let prefix = component_ref(vec![part("pin", vec![index(2)])]);
        let matching = component_ref(vec![part("pin", vec![index(2)]), part("v", Vec::new())]);
        let non_matching = component_ref(vec![part("pin", vec![index(1)]), part("v", Vec::new())]);

        assert!(component_ref_has_prefix(&matching, &prefix));
        assert!(!component_ref_has_prefix(&non_matching, &prefix));
    }

    #[test]
    fn scalar_count_accepts_zero_dimension_arrays() {
        assert_eq!(
            scalar_count_from_dims(&VarName::new("empty"), &[0])
                .expect("zero-size array dimensions are legal"),
            0
        );
        assert_eq!(
            scalar_count_from_dims(&VarName::new("empty_matrix"), &[2, 0])
                .expect("any zero dimension gives zero scalar elements"),
            0
        );
    }

    #[test]
    fn zero_dimension_array_rejects_scalarized_element_access() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            VarName::new("x"),
            dae::Variable {
                name: VarName::new("x"),
                dims: vec![0],
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        let scope = DaeVariableScope::new(&dae_model);

        assert!(matches!(
            scope.dims(&VarName::new("x[1]")),
            Err(StructuralError::UnspannedContractViolation { reason })
                if reason.contains("does not match base dimensions [0]")
        ));
    }

    #[test]
    fn missing_structured_reference_metadata_preserves_reference_span() {
        let span = test_span();
        let reference = Reference::from_component_reference(component_ref_with_span(
            vec![ComponentRefPart {
                ident: "missing".to_string(),
                span,
                subs: Vec::new(),
            }],
            span,
        ));
        let dae_model = dae::Dae::default();
        let scope = DaeVariableScope::new(&dae_model);

        assert!(matches!(
            scope.shape_for_reference(&reference),
            Err(StructuralError::ContractViolation {
                reason,
                span: actual
            }) if reason.contains("missing DAE variable metadata for `missing`") && actual == span
        ));
    }

    #[test]
    fn hierarchical_structured_reference_without_leaf_metadata_is_aggregate() {
        let span = test_span();
        let reference = Reference::from_component_reference(component_ref_with_span(
            vec![
                part("machine", Vec::new()),
                part("plug", Vec::new()),
                part("pin", Vec::new()),
                part("i", Vec::new()),
            ],
            span,
        ));
        let dae_model = dae::Dae::default();
        let scope = DaeVariableScope::new(&dae_model);

        assert_eq!(
            scope
                .shape_for_reference(&reference)
                .expect("hierarchical source reference should retain aggregate shape"),
            DaeVariableShape::StructuredAggregate
        );
    }

    #[test]
    fn indexed_component_reference_prefers_scalar_variable_metadata() {
        let span = test_span();
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            VarName::new("machine.plug.pin[1].i"),
            dae::Variable {
                name: VarName::new("machine.plug.pin[1].i"),
                dims: Vec::new(),
                component_ref: Some(component_ref_with_span(
                    vec![
                        part("machine", Vec::new()),
                        part("plug", Vec::new()),
                        part("pin", vec![index(1)]),
                        part("i", Vec::new()),
                    ],
                    span,
                )),
                ..rumoca_ir_dae::Variable::empty_with_span(span)
            },
        );
        let reference = Reference::from_component_reference(component_ref_with_span(
            vec![
                part("machine", Vec::new()),
                part("plug", Vec::new()),
                part("pin", vec![index(1)]),
                part("i", Vec::new()),
            ],
            span,
        ));
        let scope = DaeVariableScope::new(&dae_model);

        assert_eq!(
            scope
                .dims_for_reference(&reference)
                .expect("indexed scalar reference should resolve"),
            Some(Vec::new())
        );
    }

    #[test]
    fn aggregate_dims_can_be_inferred_from_indexed_scalar_descendants() {
        let span = test_span();
        let mut dae_model = dae::Dae::default();
        for pin_index in [1, 2, 3] {
            dae_model.variables.algebraics.insert(
                VarName::new(format!("machine.plug.pin[{pin_index}].i")),
                dae::Variable {
                    name: VarName::new(format!("machine.plug.pin[{pin_index}].i")),
                    dims: Vec::new(),
                    component_ref: Some(component_ref_with_span(
                        vec![
                            part("machine", Vec::new()),
                            part("plug", Vec::new()),
                            part("pin", vec![index(pin_index)]),
                            part("i", Vec::new()),
                        ],
                        span,
                    )),
                    ..rumoca_ir_dae::Variable::empty_with_span(span)
                },
            );
        }
        let scope = DaeVariableScope::new(&dae_model);

        assert_eq!(
            scope
                .dims(&VarName::new("machine.plug.pin.i"))
                .expect("aggregate shape should be inferred from scalar descendants"),
            vec![3]
        );
    }

    #[test]
    fn indexed_reference_dims_preserves_parent_component_indices() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            VarName::new("analysatorAC.iH1[1].product2.u"),
            dae::Variable {
                name: VarName::new("analysatorAC.iH1[1].product2.u"),
                dims: vec![2],
                component_ref: Some(component_ref(vec![
                    part("analysatorAC", Vec::new()),
                    part("iH1", vec![index(1)]),
                    part("product2", Vec::new()),
                    part("u", Vec::new()),
                ])),
                ..rumoca_ir_dae::Variable::empty_with_span(test_span())
            },
        );
        let reference = Reference::from_component_reference(component_ref(vec![
            part("analysatorAC", Vec::new()),
            part("iH1", vec![index(1)]),
            part("product2", Vec::new()),
            part("u", vec![index(2)]),
        ]));
        let scope = DaeVariableScope::new(&dae_model);

        assert_eq!(
            scope
                .shape_for_reference(&reference)
                .expect("indexed component element should resolve"),
            DaeVariableShape::Dimensions(Vec::new())
        );
    }
}
