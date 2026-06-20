use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum DynamicSubscriptSemantics {
    VarRef,
    Index,
}

#[derive(Debug, Clone)]
pub(super) struct DynamicBindingTarget {
    pub(super) display_key: String,
    pub(super) source_key: Option<ComponentReferenceKey>,
    pub(super) source_span: Option<rumoca_core::Span>,
    pub(super) generated: bool,
}

impl DynamicBindingTarget {
    pub(super) fn generated(display_key: impl Into<String>) -> Self {
        Self {
            display_key: display_key.into(),
            source_key: None,
            source_span: None,
            generated: true,
        }
    }

    pub(super) fn source_reference(
        reference: &rumoca_core::Reference,
        fallback_span: rumoca_core::Span,
    ) -> Result<Self, LowerError> {
        let source_span = reference_context_span(reference, fallback_span);
        let source_span = (!source_span.is_dummy()).then_some(source_span);
        if reference.is_generated() {
            return Ok(Self {
                display_key: reference.as_str().to_string(),
                source_key: None,
                source_span,
                generated: true,
            });
        }
        Ok(Self {
            display_key: reference.as_str().to_string(),
            source_key: reference
                .component_ref()
                .cloned()
                .map(component_reference_key)
                .transpose()?,
            source_span,
            generated: false,
        })
    }

    pub(super) fn field(
        display_key: impl Into<String>,
        source_key: Option<ComponentReferenceKey>,
        source_span: Option<rumoca_core::Span>,
    ) -> Self {
        Self {
            display_key: display_key.into(),
            source_key,
            source_span,
            generated: false,
        }
    }

    pub(super) fn emit_span(
        &self,
        fallback_span: Option<rumoca_core::Span>,
    ) -> Result<rumoca_core::Span, LowerError> {
        if let Some(span) = fallback_span.or(self.source_span) {
            return Ok(span);
        }
        Err(LowerError::UnspannedContractViolation {
            reason: format!(
                "dynamic indexed binding for `{}` requires source span metadata",
                self.display_key
            ),
        })
    }
}

pub(super) fn scope_key_from_reference(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
) -> Result<ComponentReferenceKey, LowerError> {
    if name.is_generated() {
        return Ok(ComponentReferenceKey::generated(name.as_str()));
    }
    #[cfg(test)]
    if let Some(key) = crate::test_support::fixture_key_for_reference(name) {
        return Ok(key);
    }
    let Some(component_ref) = name.component_ref() else {
        return Err(LowerError::contract_violation(
            format!(
                "Solve lowering requires structured component reference metadata for `{}`",
                name.as_str()
            ),
            span,
        ));
    };
    ComponentReferenceKey::from_component_reference(component_ref).map_err(|err| {
        LowerError::contract_violation(
            format!(
                "Solve lowering requires static component-reference metadata for `{}`: {err}",
                name.as_str(),
            ),
            err.span,
        )
    })
}

pub(super) fn dynamic_subscript_unsupported(
    base_key: &str,
    subscripts: &[rumoca_core::Subscript],
    semantics: DynamicSubscriptSemantics,
) -> LowerError {
    let mode = match semantics {
        DynamicSubscriptSemantics::VarRef => "dynamic VarRef subscript",
        DynamicSubscriptSemantics::Index => "dynamic index subscript",
    };
    let reason =
        format!("{mode} for `{base_key}` requires a bounds-checked Solve-IR dynamic select");
    let span = subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy());
    match span {
        Some(span) => LowerError::UnsupportedAt {
            reason,
            contexts: Vec::new(),
            span,
        },
        None => LowerError::Unsupported { reason },
    }
}

pub(super) fn subscript_fallback_span(
    subscripts: &[rumoca_core::Subscript],
) -> Option<rumoca_core::Span> {
    subscripts
        .iter()
        .map(rumoca_core::Subscript::span)
        .find(|span| !span.is_dummy())
}

pub(super) fn reference_context_span(
    reference: &rumoca_core::Reference,
    fallback: rumoca_core::Span,
) -> rumoca_core::Span {
    reference
        .span()
        .or_else(|| (!fallback.is_dummy()).then_some(fallback))
        .unwrap_or(fallback)
}

pub(super) fn dae_variable<'a>(
    variables: &'a dae::DaeVariables,
    name: &rumoca_core::VarName,
) -> Option<&'a dae::Variable> {
    variables
        .states
        .get(name)
        .or_else(|| variables.algebraics.get(name))
        .or_else(|| variables.inputs.get(name))
        .or_else(|| variables.outputs.get(name))
        .or_else(|| variables.parameters.get(name))
        .or_else(|| variables.constants.get(name))
        .or_else(|| variables.discrete_reals.get(name))
        .or_else(|| variables.discrete_valued.get(name))
}

pub(super) fn component_reference_for_source_reference(
    reference: &rumoca_core::Reference,
) -> Option<rumoca_core::ComponentReference> {
    reference.component_ref().cloned()
}

pub(super) fn component_reference_key_for_expr(
    expr: &rumoca_core::Expression,
) -> Result<Option<ComponentReferenceKey>, LowerError> {
    let Some(component_ref) = component_reference_for_expr(expr)? else {
        return Ok(None);
    };
    component_reference_key(component_ref).map(Some)
}

pub(super) fn component_reference_key_for_field_base(
    base: &rumoca_core::Expression,
    field: &str,
) -> Result<Option<ComponentReferenceKey>, LowerError> {
    let Some(component_ref) = component_reference_for_field_base(base, field)? else {
        return Ok(None);
    };
    component_reference_key(component_ref).map(Some)
}

pub(super) fn component_reference_for_field_base(
    base: &rumoca_core::Expression,
    field: &str,
) -> Result<Option<rumoca_core::ComponentReference>, LowerError> {
    let Some(mut component_ref) = component_reference_for_expr(base)? else {
        return Ok(None);
    };
    let span = base.span().unwrap_or(component_ref.span);
    component_ref.parts.push(rumoca_core::ComponentRefPart {
        ident: field.to_string(),
        span,
        subs: Vec::new(),
    });
    Ok(Some(component_ref))
}

fn component_reference_for_expr(
    expr: &rumoca_core::Expression,
) -> Result<Option<rumoca_core::ComponentReference>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let Some(mut component_ref) = component_reference_for_source_reference(name) else {
                return Ok(None);
            };
            append_component_reference_subscripts(&mut component_ref, subscripts)?;
            Ok(Some(component_ref))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let Some(mut component_ref) = component_reference_for_expr(base)? else {
                return Ok(None);
            };
            append_component_reference_subscripts(&mut component_ref, subscripts)?;
            Ok(Some(component_ref))
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            component_reference_for_field_base(base, field)
        }
        _ => Ok(None),
    }
}

fn append_component_reference_subscripts(
    component_ref: &mut rumoca_core::ComponentReference,
    subscripts: &[rumoca_core::Subscript],
) -> Result<(), LowerError> {
    if subscripts.is_empty() {
        return Ok(());
    }
    let Some(last) = component_ref.parts.last_mut() else {
        return Err(LowerError::contract_violation(
            "component reference has no parts for subscripted access",
            component_ref.span,
        ));
    };
    last.subs.extend(subscripts.iter().cloned());
    Ok(())
}

pub(super) fn component_reference_key(
    component_ref: rumoca_core::ComponentReference,
) -> Result<ComponentReferenceKey, LowerError> {
    #[cfg(test)]
    {
        let display_name =
            rumoca_core::ComponentPath::from_component_reference(&component_ref).to_flat_string();
        if let Some(key) =
            crate::test_support::fixture_key_for_component_ref(&component_ref, &display_name)
        {
            return Ok(key);
        }
    }
    ComponentReferenceKey::from_component_reference(&component_ref).map_err(|err| {
        LowerError::contract_violation(
            format!("indexed solve-layout lookup has non-static component reference: {err}"),
            err.span,
        )
    })
}
