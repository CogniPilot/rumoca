//! Aggregate record normalization for flattened functions.
//!
//! Record-typed function values remain aggregate through Flat and DAE. This
//! module performs only source-shape normalization and MLS-defined default
//! materialization; it does not define a target ABI.

#[cfg(test)]
mod tests;

use crate::errors::FlattenError;
use rumoca_core::{FallibleExpressionRewriter, FallibleStatementRewriter};
use rumoca_ir_flat as flat;
use std::collections::HashMap;

/// Normalize record-field component references in a function body.
///
/// Modelica record field syntax can arrive as a structured component reference
/// (`state.x`) rather than an explicit `FieldAccess`. Preserve the exact root
/// declaration identity, source span, and locality while making the aggregate
/// projection explicit for DAE construction.
#[cfg(test)]
pub(super) fn rewrite_record_field_access_in_body(
    func: &mut rumoca_core::Function,
    model: &flat::Model,
) -> Result<(), FlattenError> {
    let normalized = normalized_record_field_access_body(func, model)?;
    func.body = normalized;
    Ok(())
}

pub(super) fn rewrite_all_record_field_access_bodies(
    model: &mut flat::Model,
) -> Result<(), FlattenError> {
    let normalized = model
        .functions
        .iter()
        .map(|(name, function)| {
            normalized_record_field_access_body(function, model)
                .map(|body| (name.clone(), function.span, body))
        })
        .collect::<Result<Vec<_>, _>>()?;
    for (name, span, body) in normalized {
        let function = model.functions.get_mut(&name).ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                name.as_str(),
                "record field normalization function owner",
                span,
            )
        })?;
        function.body = body;
    }
    Ok(())
}

fn normalized_record_field_access_body(
    func: &rumoca_core::Function,
    model: &flat::Model,
) -> Result<Vec<rumoca_core::Statement>, FlattenError> {
    let mut record_params = HashMap::new();
    for input in func
        .inputs
        .iter()
        .filter(|input| input.type_class == Some(rumoca_core::ClassType::Record))
    {
        let def_id = input.def_id.ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                &input.name,
                "record function input declaration identity",
                input.span,
            )
        })?;
        let param = checked_record_syntax_param(input, model)?;
        if record_params.insert(def_id, param).is_some() {
            return Err(FlattenError::missing_resolved_class_metadata(
                &input.name,
                "duplicate record function input declaration identity",
                input.span,
            ));
        }
    }
    if record_params.is_empty() {
        return Ok(func.body.clone());
    }
    let mut normalizer = RecordFieldAccessNormalizer {
        params: &record_params,
    };
    func.body
        .iter()
        .map(|statement| normalizer.rewrite_statement(statement))
        .collect()
}

struct CheckedRecordSyntaxParam {
    name: String,
    fields: HashMap<rumoca_core::DefId, String>,
}

fn checked_record_syntax_param(
    input: &rumoca_core::FunctionParam,
    model: &flat::Model,
) -> Result<CheckedRecordSyntaxParam, FlattenError> {
    let record = input.type_def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            &input.name,
            "record input type declaration identity",
            input.span,
        )
    })?;
    validate_record_value_type_identity(input, model, "record input exact type identity")?;
    let layout = model.record_types.get(&record).ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            &input.name,
            "record input retained field layout",
            input.span,
        )
    })?;
    let mut fields = HashMap::new();
    for field in &layout.fields {
        if fields.insert(field.def_id, field.name.clone()).is_some() {
            return Err(FlattenError::missing_resolved_class_metadata(
                &input.name,
                "duplicate record input field declaration identity",
                input.span,
            ));
        }
    }
    Ok(CheckedRecordSyntaxParam {
        name: input.name.clone(),
        fields,
    })
}

struct RecordFieldAccessNormalizer<'a> {
    params: &'a HashMap<rumoca_core::DefId, CheckedRecordSyntaxParam>,
}

impl FallibleExpressionRewriter for RecordFieldAccessNormalizer<'_> {
    type Error = FlattenError;

    fn rewrite_expression(
        &mut self,
        expression: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Expression, Self::Error> {
        match component_ref_record_field_access(expression, self.params)? {
            Some(normalized) => Ok(normalized),
            None => self.walk_expression(expression),
        }
    }
}

impl FallibleStatementRewriter for RecordFieldAccessNormalizer<'_> {}

fn component_ref_record_field_access(
    expression: &rumoca_core::Expression,
    params: &HashMap<rumoca_core::DefId, CheckedRecordSyntaxParam>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = expression
    else {
        return Ok(None);
    };
    if !subscripts.is_empty() {
        return Ok(None);
    }

    let Some(reference) = name.component_ref() else {
        return Ok(None);
    };
    let Some(record) = reference.parts().first() else {
        return Ok(None);
    };
    let Some(param) = params.get(&record.def_id) else {
        return Ok(None);
    };
    if record.ident != param.name {
        return Err(FlattenError::missing_resolved_class_metadata(
            &record.ident,
            "record input reference spelling contradicts its declaration identity",
            record.span,
        ));
    }
    if reference.parts().len() == 1 {
        return Ok(None);
    }
    let [record, field] = reference.parts() else {
        return Err(FlattenError::missing_resolved_class_metadata(
            name.as_str(),
            "record input field selection must identify one exact field",
            *span,
        ));
    };
    if !record.subs.is_empty() || !field.subs.is_empty() || field.def_id.index() == 0 {
        return Err(FlattenError::missing_resolved_class_metadata(
            name.as_str(),
            "record input field selection lacks exact unsubscripted field identity",
            *span,
        ));
    }
    if param.fields.get(&field.def_id) != Some(&field.ident) {
        return Err(FlattenError::missing_resolved_class_metadata(
            name.as_str(),
            "record input field identity is absent from its exact retained layout",
            *span,
        ));
    }
    let base = reference
        .with_replaced_parts(vec![record.clone()])
        .map_err(|_| {
            FlattenError::missing_resolved_class_metadata(
                name.as_str(),
                "record input field base cannot preserve its structured reference",
                *span,
            )
        })?;
    Ok(Some(rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(base),
            subscripts: Vec::new(),
            span: record.span,
        }),
        field: field.ident.clone(),
        field_def_id: field.def_id,
        span: *span,
    }))
}

/// Materialize the MLS record value default for outputs and locals whose
/// complete constructor field list has defaults.
///
/// This runs after the final function inventory and before final call-argument
/// materialization. The empty constructor call is aggregate: the ordinary
/// function-argument owner fills its declared field defaults exactly once.
pub(crate) fn materialize_complete_record_value_defaults(
    flat: &mut flat::Model,
) -> Result<(), FlattenError> {
    let constructors = checked_constructor_default_capabilities(flat)?;

    for function in flat
        .functions
        .values()
        .filter(|function| !function.is_constructor)
    {
        for value in function.outputs.iter().chain(&function.locals) {
            validate_record_value_default_owner(value, flat, &constructors)?;
        }
    }

    for function in flat
        .functions
        .values_mut()
        .filter(|function| !function.is_constructor)
    {
        for value in function.outputs.iter_mut().chain(&mut function.locals) {
            if value.default.is_some() || value.type_class != Some(rumoca_core::ClassType::Record) {
                continue;
            }
            let type_def_id = value.type_def_id.ok_or_else(|| {
                FlattenError::missing_resolved_class_metadata(
                    &value.name,
                    "record output/local type declaration identity",
                    value.span,
                )
            })?;
            let Some(ConstructorDefaultCapability::Complete {
                name: constructor_name,
                instance_id,
            }) = constructors.get(&type_def_id)
            else {
                continue;
            };
            value.default = Some(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_var_name(constructor_name.clone())
                    .with_resolved_function(rumoca_core::ResolvedFunctionReference {
                        instance_id: *instance_id,
                        base_part_count: 0,
                        transitively_non_replaceable: false,
                    }),
                args: Vec::new(),
                is_constructor: true,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: value.span,
            });
        }
    }
    Ok(())
}

enum ConstructorDefaultCapability {
    NoImplicitDefault,
    Complete {
        name: rumoca_core::VarName,
        instance_id: rumoca_core::FunctionInstanceId,
    },
}

fn checked_constructor_default_capabilities(
    flat: &flat::Model,
) -> Result<HashMap<rumoca_core::DefId, ConstructorDefaultCapability>, FlattenError> {
    let mut constructors = HashMap::new();
    for function in flat
        .functions
        .values()
        .filter(|function| function.is_constructor)
    {
        let def_id = function.def_id.ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                function.name.as_str(),
                "record constructor declaration identity",
                function.span,
            )
        })?;
        let instance_id = function.instance_id.ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(
                function.name.as_str(),
                "record constructor function instance identity",
                function.span,
            )
        })?;
        validate_constructor_layout(flat, function, def_id)?;
        let capability = if function.inputs.iter().all(|input| input.default.is_some()) {
            ConstructorDefaultCapability::Complete {
                name: function.name.clone(),
                instance_id,
            }
        } else {
            ConstructorDefaultCapability::NoImplicitDefault
        };
        if constructors.insert(def_id, capability).is_some() {
            return Err(FlattenError::missing_resolved_class_metadata(
                function.name.as_str(),
                "duplicate record constructor declaration identity",
                function.span,
            ));
        }
    }
    Ok(constructors)
}

fn validate_constructor_layout(
    flat: &flat::Model,
    constructor: &rumoca_core::Function,
    record: rumoca_core::DefId,
) -> Result<(), FlattenError> {
    let layout = flat.record_types.get(&record).ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            constructor.name.as_str(),
            "record constructor retained layout",
            constructor.span,
        )
    })?;
    let record_nominal = flat.type_ids_by_def_id.get(&record).copied();
    let exact = record_nominal.is_some_and(|nominal| {
        flat.type_roots
            .get(&nominal)
            .is_some_and(|canonical| flat.type_roots.get(canonical) == Some(canonical))
    }) && constructor.name.as_str() == layout.name
        && constructor.outputs.is_empty()
        && constructor.inputs.len() == layout.fields.len()
        && constructor
            .inputs
            .iter()
            .zip(&layout.fields)
            .all(|(input, field)| {
                input.name == field.name
                    && input.def_id == Some(field.def_id)
                    && input.type_def_id == Some(field.type_def_id)
                    && input.effective_type == field.effective_type
                    && input.dimensions() == field.dims
                    && declared_field_type_identity(flat, field)
                    && (input.type_class == Some(rumoca_core::ClassType::Record))
                        == flat.record_types.contains_key(&field.type_def_id)
            });
    if exact {
        Ok(())
    } else {
        Err(FlattenError::missing_resolved_class_metadata(
            constructor.name.as_str(),
            "record constructor exact ordered field layout",
            constructor.span,
        ))
    }
}

fn declared_field_type_identity(model: &flat::Model, field: &flat::RecordField) -> bool {
    model.type_ids_by_def_id.get(&field.type_def_id) == Some(&field.effective_type.nominal_type())
        && model.type_roots.get(&field.effective_type.nominal_type())
            == Some(&field.effective_type.canonical_type())
        && model.type_roots.get(&field.effective_type.canonical_type())
            == Some(&field.effective_type.canonical_type())
}

fn validate_record_value_default_owner(
    value: &rumoca_core::FunctionParam,
    model: &flat::Model,
    constructors: &HashMap<rumoca_core::DefId, ConstructorDefaultCapability>,
) -> Result<(), FlattenError> {
    if value.type_class != Some(rumoca_core::ClassType::Record) {
        return Ok(());
    }
    let type_def_id = value.type_def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(
            &value.name,
            "record output/local type declaration identity",
            value.span,
        )
    })?;
    validate_record_value_type_identity(value, model, "record output/local exact type identity")?;
    if constructors.contains_key(&type_def_id) {
        Ok(())
    } else {
        Err(FlattenError::missing_resolved_class_metadata(
            &value.name,
            "record output/local exact constructor identity",
            value.span,
        ))
    }
}

fn validate_record_value_type_identity(
    value: &rumoca_core::FunctionParam,
    model: &flat::Model,
    evidence: &'static str,
) -> Result<(), FlattenError> {
    let declaration = value.type_def_id.ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(&value.name, evidence, value.span)
    })?;
    let nominal = model
        .type_ids_by_def_id
        .get(&declaration)
        .copied()
        .ok_or_else(|| {
            FlattenError::missing_resolved_class_metadata(&value.name, evidence, value.span)
        })?;
    let canonical = model.type_roots.get(&nominal).copied().ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(&value.name, evidence, value.span)
    })?;
    let layout = model.record_types.get(&declaration).ok_or_else(|| {
        FlattenError::missing_resolved_class_metadata(&value.name, evidence, value.span)
    })?;
    let exact = value.effective_type.nominal_type() == nominal
        && value.effective_type.canonical_type() == canonical
        && model.type_roots.get(&canonical) == Some(&canonical)
        && layout.name == value.type_name;
    if exact {
        Ok(())
    } else {
        Err(FlattenError::missing_resolved_class_metadata(
            &value.name,
            evidence,
            value.span,
        ))
    }
}
