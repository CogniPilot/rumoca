//! Manifest `Variables` population from classified DAE variables (GAL-020).
//!
//! - **Id scheme**: positional `V1`, `V2`, … in classification order over
//!   the manifest-listed variables (projection-internal variables are
//!   skipped and consume no id). Deterministic because classification walks
//!   partitions in canonical order.
//! - **Names**: the manifest spelling of the mangled GALEC name
//!   ([`crate::mangle::manifest_name`]) — unique by mangling injectivity.
//! - **Types**: from classification only (S8: never inferred from start
//!   literals). Start/min/max/nominal *values* are evaluated by the
//!   [`const_eval`] evaluator and coerced to the declared type (Integer
//!   literals widen to Real for Real variables — a value conversion, not
//!   type inference).
//! - **Starts**: scalar expressions broadcast over array variables; array
//!   expressions flatten row-major and must match the dimension product.
//!   A missing `start` takes the MLS-defined default for the declared type
//!   (MLS §4.8.4: Real `0.0`, Integer `0`, Boolean `false`) — an explicitly
//!   modeled language default, not error recovery. Tunable parameters stay
//!   tunable in code; the manifest start is the nominal default only.

use std::collections::HashMap;

use rumoca_efmi::algorithm_code_manifest::{
    BooleanVariable, IntegerVariable, RealVariable, StartValue, Variable as ManifestVariable,
    VariableCommon,
};
use rumoca_efmi::{Identifier, NormalizedText};
use rumoca_galec::ast::ScalarType;

use crate::classify::{Classification, ClassifiedVariable};
use crate::diagnostic::GalecTargetError;
use crate::mangle;

pub mod const_eval;

use const_eval::{ConstEnv, ConstValue};

/// Manifest variables plus the DAE-name → manifest-id correspondence the
/// lowering slice needs (e.g. to wire `Clock/@variableRefId` to the sample
/// period constant).
#[derive(Debug, Clone, Default)]
pub struct ManifestVariables {
    /// Ordered manifest `Variables` list.
    pub variables: Vec<ManifestVariable>,
    /// Manifest id per DAE variable name (manifest-listed variables only).
    pub ids_by_dae_name: HashMap<String, String>,
}

/// Build the manifest `Variables` list from a classification, collecting all
/// failures. Projection-internal variables are excluded (see
/// [`crate::classify`] module docs).
pub fn build_manifest_variables(
    classification: &Classification<'_>,
) -> Result<ManifestVariables, Vec<GalecTargetError>> {
    let env = ConstEnv::from_classification(classification);
    let mut result = ManifestVariables::default();
    let mut errors = Vec::new();
    for classified in classification
        .variables
        .iter()
        .filter(|classified| !classified.projection_internal)
    {
        let id = format!("V{}", result.variables.len() + 1);
        match build_one(classified, &id, &env) {
            Ok(variable) => {
                result
                    .ids_by_dae_name
                    .insert(classified.variable.name.as_str().to_owned(), id);
                result.variables.push(variable);
            }
            Err(mut variable_errors) => errors.append(&mut variable_errors),
        }
    }
    if errors.is_empty() {
        Ok(result)
    } else {
        Err(errors)
    }
}

fn build_one(
    classified: &ClassifiedVariable<'_>,
    id: &str,
    env: &ConstEnv<'_>,
) -> Result<ManifestVariable, Vec<GalecTargetError>> {
    let common = build_common(classified, id).map_err(|error| vec![error])?;
    let mut errors = Vec::new();
    let variable = assemble_typed(classified, common, env, &mut errors);
    match variable {
        Some(variable) if errors.is_empty() => Ok(variable),
        _ => Err(errors),
    }
}

fn build_common(
    classified: &ClassifiedVariable<'_>,
    id: &str,
) -> Result<VariableCommon, GalecTargetError> {
    let variable = classified.variable;
    let mut dimensions = Vec::with_capacity(variable.dims.len());
    for (index, size) in variable.dims.iter().enumerate() {
        let size_u64 = u64::try_from(*size)
            .ok()
            .filter(|s| *s >= 1)
            .ok_or_else(|| GalecTargetError::NonPositiveDimension {
                variable: variable.name.as_str().to_owned(),
                dimension: index + 1,
                size: *size,
                span: variable.source_span,
            })?;
        dimensions.push(size_u64);
    }
    let description = match &variable.description {
        Some(text) if !text.is_empty() => Some(NormalizedText::new(text.clone())?),
        _ => None,
    };
    Ok(VariableCommon {
        id: Identifier::new(id)?,
        name: NormalizedText::new(mangle::manifest_name(&classified.galec_name))?,
        description,
        block_causality: classified.class.block_causality(),
        dimensions,
        annotations: Vec::new(),
    })
}

fn assemble_typed(
    classified: &ClassifiedVariable<'_>,
    common: VariableCommon,
    env: &ConstEnv<'_>,
    errors: &mut Vec<GalecTargetError>,
) -> Option<ManifestVariable> {
    match classified.scalar_type {
        ScalarType::Real => assemble_real(classified, common, env, errors),
        ScalarType::Integer => assemble_integer(classified, common, env, errors),
        ScalarType::Boolean => assemble_boolean(classified, common, env, errors),
    }
}

fn assemble_real(
    classified: &ClassifiedVariable<'_>,
    common: VariableCommon,
    env: &ConstEnv<'_>,
    errors: &mut Vec<GalecTargetError>,
) -> Option<ManifestVariable> {
    let variable = classified.variable;
    let start = evaluate_start(classified, env, errors, ConstValue::Real(0.0), |value| {
        value.as_real()
    });
    let mut bound = |attribute, expr: &Option<rumoca_core::Expression>| {
        evaluate_optional_scalar(classified, attribute, expr, env, errors, |value| {
            value.as_real()
        })
    };
    let min = bound("min", &variable.min);
    let max = bound("max", &variable.max);
    let nominal = bound("nominal", &variable.nominal);
    Some(ManifestVariable::Real(RealVariable {
        common,
        start: start?,
        unit_ref_id: None,
        relative_quantity: false,
        min: min?,
        max: max?,
        nominal: nominal?,
    }))
}

fn assemble_integer(
    classified: &ClassifiedVariable<'_>,
    common: VariableCommon,
    env: &ConstEnv<'_>,
    errors: &mut Vec<GalecTargetError>,
) -> Option<ManifestVariable> {
    let variable = classified.variable;
    let start = evaluate_start(classified, env, errors, ConstValue::Integer(0), |value| {
        value.as_i32()
    });
    let mut bound = |attribute, expr: &Option<rumoca_core::Expression>| {
        evaluate_optional_scalar(classified, attribute, expr, env, errors, |value| {
            value.as_i32()
        })
    };
    let min = bound("min", &variable.min);
    let max = bound("max", &variable.max);
    if variable.nominal.is_some() {
        errors.push(attribute_mismatch(classified, "nominal", "Integer", "Real"));
        return None;
    }
    Some(ManifestVariable::Integer(IntegerVariable {
        common,
        start: start?,
        min: min?,
        max: max?,
    }))
}

fn assemble_boolean(
    classified: &ClassifiedVariable<'_>,
    common: VariableCommon,
    env: &ConstEnv<'_>,
    errors: &mut Vec<GalecTargetError>,
) -> Option<ManifestVariable> {
    let variable = classified.variable;
    for (attribute, expr) in [
        ("min", &variable.min),
        ("max", &variable.max),
        ("nominal", &variable.nominal),
    ] {
        if expr.is_some() {
            errors.push(attribute_mismatch(
                classified, attribute, "Boolean", "numeric",
            ));
        }
    }
    let start = evaluate_start(
        classified,
        env,
        errors,
        ConstValue::Boolean(false),
        |value| value.as_boolean(),
    );
    if !errors.is_empty() {
        return None;
    }
    Some(ManifestVariable::Boolean(BooleanVariable {
        common,
        start: start?,
    }))
}

/// Evaluate the `start` expression to a typed [`StartValue`]: scalars
/// broadcast, arrays flatten row-major and must match the dimension product;
/// a missing start takes the MLS default (module docs).
fn evaluate_start<T>(
    classified: &ClassifiedVariable<'_>,
    env: &ConstEnv<'_>,
    errors: &mut Vec<GalecTargetError>,
    mls_default: ConstValue,
    coerce: impl Fn(&ConstValue) -> Result<T, &'static str>,
) -> Option<StartValue<T>> {
    let variable = classified.variable;
    let coerce_reported = |value: &ConstValue, errors: &mut Vec<GalecTargetError>| {
        coerce(value)
            .map_err(|expected| {
                errors.push(attribute_mismatch(
                    classified,
                    "start",
                    expected,
                    value.type_name(),
                ));
            })
            .ok()
    };
    let Some(expr) = &variable.start else {
        return coerce_reported(&mls_default, errors).map(StartValue::Scalar);
    };
    match env.evaluate_start_shape(expr) {
        Ok(const_eval::StartShape::Scalar(value)) => {
            coerce_reported(&value, errors).map(StartValue::Scalar)
        }
        Ok(const_eval::StartShape::Array(values)) => {
            // Dims were validated positive by `build_common` before any
            // start evaluation runs; a breach is a projection bug (never a
            // silent 0-product that would mask a wrong-size array).
            let mut expected: u64 = 1;
            for size in &variable.dims {
                let product = u64::try_from(*size)
                    .ok()
                    .and_then(|size| expected.checked_mul(size));
                let Some(product) = product else {
                    errors.push(GalecTargetError::LoweringInternal {
                        detail: format!(
                            "dimension product of `{}` (dims {:?}) is not a valid \
                             element count",
                            variable.name.as_str(),
                            variable.dims
                        ),
                    });
                    return None;
                };
                expected = product;
            }
            if values.len() as u64 != expected {
                errors.push(GalecTargetError::AttributeNotEvaluable {
                    variable: variable.name.as_str().to_owned(),
                    attribute: "start",
                    reason: format!(
                        "array start has {} element(s) but the dimensions require {expected}",
                        values.len()
                    ),
                    span: variable.start_span,
                });
                return None;
            }
            let mut coerced = Vec::with_capacity(values.len());
            for value in &values {
                coerced.push(coerce_reported(value, errors)?);
            }
            Some(StartValue::Array(coerced))
        }
        Err(failure) => {
            errors.push(failure.into_error(variable, "start"));
            None
        }
    }
}

fn evaluate_optional_scalar<T>(
    classified: &ClassifiedVariable<'_>,
    attribute: &'static str,
    expr: &Option<rumoca_core::Expression>,
    env: &ConstEnv<'_>,
    errors: &mut Vec<GalecTargetError>,
    coerce: impl Fn(&ConstValue) -> Result<T, &'static str>,
) -> Option<Option<T>> {
    let Some(expr) = expr else {
        return Some(None);
    };
    match env.evaluate_scalar(expr) {
        Ok(value) => match coerce(&value) {
            Ok(coerced) => Some(Some(coerced)),
            Err(expected) => {
                errors.push(attribute_mismatch(
                    classified,
                    attribute,
                    expected,
                    value.type_name(),
                ));
                None
            }
        },
        Err(failure) => {
            errors.push(failure.into_error(classified.variable, attribute));
            None
        }
    }
}

fn attribute_mismatch(
    classified: &ClassifiedVariable<'_>,
    attribute: &'static str,
    expected: &'static str,
    found: &'static str,
) -> GalecTargetError {
    GalecTargetError::AttributeTypeMismatch {
        variable: classified.variable.name.as_str().to_owned(),
        attribute,
        expected,
        found,
        span: None,
    }
}
