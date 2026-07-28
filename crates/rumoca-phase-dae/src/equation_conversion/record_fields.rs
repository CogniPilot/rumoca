use crate::errors::ToDaeError;
use rumoca_ir_flat as flat;

pub(super) fn record_field_specs_for_call(
    name: &rumoca_core::Reference,
    is_constructor: bool,
    flat: &flat::Model,
) -> Result<Option<Vec<RecordFieldSpec>>, ToDaeError> {
    let Some(resolved) = name.resolved_function() else {
        return Ok(None);
    };
    let function =
        rumoca_core::resolve_function_instance(flat.functions.values(), resolved.instance_id)
            .map_err(|error| {
                name.span().map_or_else(
                    || ToDaeError::runtime_contract_violation(error.to_string()),
                    |span| ToDaeError::runtime_contract_violation_at(error.to_string(), span),
                )
            })?;
    let fields = if is_constructor || function.is_constructor {
        function.inputs.clone()
    } else {
        let [output] = function.outputs.as_slice() else {
            return Ok(None);
        };
        if output.type_class != Some(rumoca_core::ClassType::Record) {
            return Ok(None);
        }
        let type_def_id = output.type_def_id.ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!(
                    "record output `{}.{}` lacks resolved type identity",
                    function.name, output.name
                ),
                output.span,
            )
        })?;
        rumoca_core::resolve_record_constructor(
            flat.functions.values(),
            &output.type_name,
            type_def_id,
        )
        .map(|constructor| constructor.inputs.clone())
        .map_err(|error| {
            ToDaeError::runtime_contract_violation_at(
                format!(
                    "record output `{}.{}` constructor lookup failed: {error}",
                    function.name, output.name,
                ),
                output.span,
            )
        })?
    };
    RecordFieldSpec::from_params(fields)
}

#[derive(Debug, Clone)]
pub(super) struct RecordFieldSpec {
    name: String,
    def_id: rumoca_core::DefId,
    dims: Vec<i64>,
    default: Option<rumoca_core::Expression>,
}

impl RecordFieldSpec {
    fn from_params(
        params: Vec<rumoca_core::FunctionParam>,
    ) -> Result<Option<Vec<Self>>, ToDaeError> {
        for param in &params {
            if param.def_id.is_none() {
                return Err(ToDaeError::runtime_contract_violation_at(
                    format!(
                        "record field `{}` lacks resolved identity required for equation expansion",
                        param.name
                    ),
                    param.span,
                ));
            }
        }
        Ok((!params.is_empty()).then(|| {
            params
                .into_iter()
                .map(|param| Self {
                    name: param.name,
                    def_id: param.def_id.expect("record field identity checked above"),
                    dims: param.dims,
                    default: param.default,
                })
                .collect::<Vec<_>>()
        }))
    }

    fn from_record_type(record_type: &flat::RecordType) -> Vec<Self> {
        record_type
            .fields
            .iter()
            .map(|field| Self {
                name: field.name.clone(),
                def_id: field.def_id,
                dims: field.dims.clone(),
                default: None,
            })
            .collect()
    }

    pub(super) fn name(&self) -> &str {
        self.name.as_str()
    }

    pub(super) fn default(&self) -> Option<rumoca_core::Expression> {
        self.default.clone()
    }

    pub(super) fn is_statically_empty(&self) -> bool {
        !self.dims.is_empty() && self.dims.contains(&0)
    }

    pub(super) fn matches_component_ref(
        &self,
        field_ref: &rumoca_core::ComponentReference,
        symbol_ancestry: &flat::SymbolAncestryMap,
    ) -> bool {
        let expected = self.def_id;
        field_ref.def_id == Some(expected)
            || field_ref.def_id.is_some_and(|actual| {
                symbol_ancestry
                    .get(&actual)
                    .is_some_and(|ancestry| ancestry.contains(&expected))
            })
    }
}

pub(super) fn record_field_specs_for_reference_equation(
    lhs_name: &rumoca_core::Reference,
    rhs_name: &rumoca_core::Reference,
    flat: &flat::Model,
    span: rumoca_core::Span,
) -> Result<Option<Vec<RecordFieldSpec>>, ToDaeError> {
    let Some(lhs_record) = flat.record_instances.get(lhs_name.var_name()) else {
        return Ok(None);
    };
    let Some(rhs_record) = flat.record_instances.get(rhs_name.var_name()) else {
        return Ok(None);
    };
    if lhs_record.canonical_type_id.is_unknown()
        || rhs_record.canonical_type_id.is_unknown()
        || lhs_record.canonical_type_id != rhs_record.canonical_type_id
        || lhs_record.dims != rhs_record.dims
    {
        return Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "record equation `{}` = `{}` lacks compatible resolved type and shape identity",
                lhs_name.as_str(),
                rhs_name.as_str()
            ),
            span,
        ));
    }
    let record_type = flat
        .record_types
        .get(&lhs_record.type_def_id)
        .ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!(
                    "record equation for `{}` lacks field metadata for `{}` ({})",
                    lhs_name.as_str(),
                    lhs_record.type_name,
                    lhs_record.type_def_id,
                ),
                span,
            )
        })?;
    Ok(Some(RecordFieldSpec::from_record_type(record_type)))
}
