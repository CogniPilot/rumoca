//! Value types for constant expression evaluation.
//!
//! This module defines the runtime value types used during constant
//! expression evaluation in the Modelica compiler.

use indexmap::IndexMap;
use rumoca_core::{DefId, Span};

use super::DEFAULT_EVAL_BUDGET;
use super::errors::EvalError;

/// A runtime value during constant expression evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Real (floating-point) value
    Real(f64),
    /// Integer value
    Integer(i64),
    /// Boolean value
    Bool(bool),
    /// String value
    String(String),
    /// Enumeration value (type name, literal name)
    Enum(String, String),
    /// Enumeration value whose identity was proved by Resolve.
    ResolvedEnum(ResolvedEnumValue),
    /// Array of values (homogeneous)
    Array(Vec<Value>),
    /// Record with named fields
    Record(IndexMap<String, Value>),
}

/// Closed resolved-enumeration carrier. Display spelling does not participate
/// in semantic equality; only the declaration and literal do.
#[derive(Debug, Clone)]
pub struct ResolvedEnumValue {
    declaration: DefId,
    literal: String,
    display_type: String,
    ordinal: i64,
}

impl PartialEq for ResolvedEnumValue {
    fn eq(&self, other: &Self) -> bool {
        self.declaration == other.declaration && self.literal == other.literal
    }
}

impl ResolvedEnumValue {
    pub(crate) fn issued(
        declaration: DefId,
        display_type: String,
        literal: String,
        ordinal: i64,
    ) -> Self {
        Self {
            declaration,
            literal,
            display_type,
            ordinal,
        }
    }

    pub fn declaration(&self) -> DefId {
        self.declaration
    }

    pub fn literal(&self) -> &str {
        &self.literal
    }

    pub fn display_type(&self) -> &str {
        &self.display_type
    }

    pub fn ordinal(&self) -> i64 {
        self.ordinal
    }
}

/// Exact rectangular materialized shape after checking every scalar leaf
/// against the formal's primitive scalar type. Integer-to-Real is the sole
/// numeric coercion admitted by MLS ARR-009.
pub(crate) fn checked_rectangular_shape(
    value: &Value,
    expected_type: &str,
    expected_rank: usize,
    span: Span,
) -> Result<Vec<i64>, EvalError> {
    if !matches!(expected_type, "Integer" | "Real" | "Boolean" | "String") {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "materialized `{expected_type}` argument has no exact scalar type identity"
            ),
            span,
        });
    }
    let proof = inspect_rectangular_shape(value, expected_type, span)?;
    if proof.trailing_shape_unknown && expected_rank > proof.dimensions.len() {
        return Err(EvalError::UnsupportedExpression {
            kind: format!(
                "empty materialized argument has shape prefix {:?}, but rank {expected_rank} requires trailing extent metadata",
                proof.dimensions
            ),
            span,
        });
    }
    Ok(proof.dimensions)
}

struct MaterializedShapeProof {
    dimensions: Vec<i64>,
    trailing_shape_unknown: bool,
}

fn inspect_rectangular_shape(
    value: &Value,
    expected_type: &str,
    span: Span,
) -> Result<MaterializedShapeProof, EvalError> {
    let Value::Array(elements) = value else {
        check_scalar_type(value, expected_type, span)?;
        return Ok(MaterializedShapeProof {
            dimensions: Vec::new(),
            trailing_shape_unknown: false,
        });
    };
    let mut child_proof: Option<MaterializedShapeProof> = None;
    for element in elements {
        let proof = inspect_rectangular_shape(element, expected_type, span)?;
        match &child_proof {
            Some(expected) if expected.dimensions != proof.dimensions => {
                return Err(EvalError::function_error("ragged array argument", span));
            }
            Some(expected) if expected.trailing_shape_unknown != proof.trailing_shape_unknown => {
                return Err(EvalError::function_error("ragged array argument", span));
            }
            None => child_proof = Some(proof),
            _ => {}
        }
    }
    let extent = i64::try_from(elements.len())
        .map_err(|_| EvalError::function_error("array extent exceeds i64", span))?;
    let mut dimensions = vec![extent];
    let trailing_shape_unknown = match child_proof {
        Some(child) => {
            dimensions.extend(child.dimensions);
            child.trailing_shape_unknown
        }
        None => true,
    };
    Ok(MaterializedShapeProof {
        dimensions,
        trailing_shape_unknown,
    })
}

fn check_scalar_type(value: &Value, expected: &str, span: Span) -> Result<(), EvalError> {
    let compatible = match expected {
        "Integer" => matches!(value, Value::Integer(_)),
        "Real" => matches!(value, Value::Integer(_) | Value::Real(_)),
        "Boolean" => matches!(value, Value::Bool(_)),
        "String" => matches!(value, Value::String(_)),
        _ => false,
    };
    if compatible {
        Ok(())
    } else {
        Err(EvalError::type_mismatch(expected, value.type_name(), span))
    }
}

/// One settled, one-based selection along an array axis.
pub(super) enum ValueSelection {
    Element(i64),
    Slice(Vec<i64>),
}

/// Materialize a checked Cartesian array selection under the shared evaluator
/// node budget.
///
/// Selector expressions remain evaluator-owned because direct expressions and
/// user-function bodies have different name environments. Result construction
/// is shared so neither path can omit aggregate containers or multiply several
/// individually bounded selector vectors into an unbounded retained value.
pub(super) fn materialize_value_selection<S>(
    value: &Value,
    subscripts: &[S],
    span: Span,
    mut resolve: impl FnMut(&S, usize) -> Result<ValueSelection, EvalError>,
) -> Result<Value, EvalError> {
    let mut remaining = DEFAULT_EVAL_BUDGET;
    materialize_value_selection_inner(value, subscripts, span, &mut remaining, &mut resolve)
}

fn materialize_value_selection_inner<S>(
    value: &Value,
    subscripts: &[S],
    span: Span,
    remaining: &mut usize,
    resolve: &mut impl FnMut(&S, usize) -> Result<ValueSelection, EvalError>,
) -> Result<Value, EvalError> {
    let Some((subscript, trailing)) = subscripts.split_first() else {
        retain_selection_nodes(materialized_node_count(value), remaining, span)?;
        return Ok(value.clone());
    };
    let elements = value
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", value.type_name(), span))?;
    match resolve(subscript, elements.len())? {
        ValueSelection::Element(index) => {
            let slot = checked_selection_slot(index, elements.len(), span)?;
            materialize_value_selection_inner(&elements[slot], trailing, span, remaining, resolve)
        }
        ValueSelection::Slice(indices) => {
            retain_selection_nodes(Some(1), remaining, span)?;
            if indices.len() > *remaining {
                return Err(selection_budget_error(span));
            }
            let mut selected = Vec::new();
            selected
                .try_reserve_exact(indices.len())
                .map_err(|_| selection_budget_error(span))?;
            for index in indices {
                let slot = checked_selection_slot(index, elements.len(), span)?;
                selected.push(materialize_value_selection_inner(
                    &elements[slot],
                    trailing,
                    span,
                    remaining,
                    resolve,
                )?);
            }
            Ok(Value::Array(selected))
        }
    }
}

fn retain_selection_nodes(
    nodes: Option<usize>,
    remaining: &mut usize,
    span: Span,
) -> Result<(), EvalError> {
    let nodes = nodes.ok_or_else(|| selection_budget_error(span))?;
    *remaining = remaining
        .checked_sub(nodes)
        .ok_or_else(|| selection_budget_error(span))?;
    Ok(())
}

fn checked_selection_slot(index: i64, len: usize, span: Span) -> Result<usize, EvalError> {
    let slot = usize::try_from(index).map_err(|_| EvalError::IndexOutOfBounds {
        index,
        size: len,
        span,
    })?;
    if !(1..=len).contains(&slot) {
        return Err(EvalError::IndexOutOfBounds {
            index,
            size: len,
            span,
        });
    }
    Ok(slot - 1)
}

fn selection_budget_error(span: Span) -> EvalError {
    EvalError::UnsupportedExpression {
        kind: "array selection result is beyond the constant-evaluation node budget".to_string(),
        span,
    }
}

pub(super) fn integer_subscript_index(value: &Value, span: Span) -> Result<i64, EvalError> {
    match value {
        Value::Integer(index) => Ok(*index),
        Value::Bool(_) | Value::Enum(_, _) | Value::ResolvedEnum(_) => {
            Err(EvalError::UnsupportedExpression {
                kind: format!(
                    "{} indexing requires retained Boolean/enumeration domain metadata",
                    value.type_name()
                ),
                span,
            })
        }
        other => Err(EvalError::type_mismatch(
            "Integer, Boolean, or Enumeration subscript",
            other.type_name(),
            span,
        )),
    }
}

/// Number of value nodes retained by one materialized value, including every
/// aggregate container. `None` means the count overflowed the host index type.
pub(super) fn materialized_node_count(value: &Value) -> Option<usize> {
    match value {
        Value::Array(elements) => elements.iter().try_fold(1_usize, |total, element| {
            total.checked_add(materialized_node_count(element)?)
        }),
        Value::Record(fields) => fields.values().try_fold(1_usize, |total, field| {
            total.checked_add(materialized_node_count(field)?)
        }),
        _ => Some(1),
    }
}

/// Number of nodes built by recursively cloning `payload_nodes` through a
/// rectangular array. Every array container at every rank counts separately.
pub(super) fn rectangular_materialized_node_count(
    dimensions: &[usize],
    payload_nodes: usize,
) -> Option<usize> {
    let mut containers = 0_usize;
    let mut instances_at_depth = 1_usize;
    for extent in dimensions {
        containers = containers.checked_add(instances_at_depth)?;
        instances_at_depth = instances_at_depth.checked_mul(*extent)?;
    }
    containers.checked_add(instances_at_depth.checked_mul(payload_nodes)?)
}

/// Whether nested `Value::Array` containers preserve every declared extent.
/// Once an empty container is reached there is no child value that can retain
/// later dimensions, so only a final zero extent is representable exactly.
pub(super) fn rectangular_shape_is_representable(dimensions: &[usize]) -> bool {
    dimensions
        .iter()
        .position(|extent| *extent == 0)
        .is_none_or(|position| position + 1 == dimensions.len())
}

impl Value {
    /// Try to get this value as a Real.
    pub fn as_real(&self) -> Option<f64> {
        match self {
            Self::Real(v) => Some(*v),
            _ => None,
        }
    }

    /// Try to get this value as an Integer.
    /// Also converts Real values that are whole numbers to Integer.
    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Self::Integer(v) => Some(*v),
            Self::Real(v)
                if v.fract() == 0.0
                    && v.is_finite()
                    && *v >= i64::MIN as f64
                    && *v < i64::MAX as f64 =>
            {
                Some(*v as i64)
            }
            _ => None,
        }
    }

    /// Try to get this value as a Bool.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(v) => Some(*v),
            _ => None,
        }
    }

    /// Try to get this value as an Enum (returns type_name.literal_name).
    pub fn as_enum(&self) -> Option<(&str, &str)> {
        match self {
            Self::Enum(type_name, literal) => Some((type_name, literal)),
            Self::ResolvedEnum(value) => Some((value.display_type(), value.literal())),
            _ => None,
        }
    }

    /// Try to get this value as an Array.
    pub fn as_array(&self) -> Option<&Vec<Value>> {
        match self {
            Self::Array(arr) => Some(arr),
            _ => None,
        }
    }

    /// Try to get this value as a mutable Array.
    pub fn as_array_mut(&mut self) -> Option<&mut Vec<Value>> {
        match self {
            Self::Array(arr) => Some(arr),
            _ => None,
        }
    }

    /// Try to get this value as a Record.
    pub fn as_record(&self) -> Option<&IndexMap<String, Value>> {
        match self {
            Self::Record(rec) => Some(rec),
            _ => None,
        }
    }

    /// Convert this value to a Real, with automatic Integer -> Real coercion.
    pub fn to_real(&self) -> Option<f64> {
        match self {
            Self::Real(v) => Some(*v),
            Self::Integer(v) => Some(*v as f64),
            _ => None,
        }
    }

    /// Get the type name of this value.
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Real(_) => "Real",
            Self::Integer(_) => "Integer",
            Self::Bool(_) => "Boolean",
            Self::String(_) => "String",
            Self::Enum(_, _) | Self::ResolvedEnum(_) => "Enumeration",
            Self::Array(_) => "Array",
            Self::Record(_) => "Record",
        }
    }

    /// Check if this value is numeric (Real or Integer).
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Real(_) | Self::Integer(_))
    }

    /// Get the length of an array, or None if not an array.
    pub fn len(&self) -> Option<usize> {
        self.as_array().map(|a| a.len())
    }

    /// Check if the array is empty, or None if not an array.
    pub fn is_empty(&self) -> Option<bool> {
        self.as_array().map(|a| a.is_empty())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Real(v) => write!(f, "{v}"),
            Self::Integer(v) => write!(f, "{v}"),
            Self::Bool(v) => write!(f, "{v}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Enum(type_name, literal) => write!(f, "{type_name}.{literal}"),
            Self::ResolvedEnum(value) => {
                write!(f, "{}.{}", value.display_type(), value.literal())
            }
            Self::Array(arr) => write_array(f, arr),
            Self::Record(rec) => write_record(f, rec),
        }
    }
}

fn write_array(f: &mut std::fmt::Formatter<'_>, arr: &[Value]) -> std::fmt::Result {
    write!(f, "{{")?;
    let mut first = true;
    for v in arr {
        if !first {
            write!(f, ", ")?;
        }
        first = false;
        write!(f, "{v}")?;
    }
    write!(f, "}}")
}

fn write_record(
    f: &mut std::fmt::Formatter<'_>,
    rec: &IndexMap<String, Value>,
) -> std::fmt::Result {
    write!(f, "Record(")?;
    let mut first = true;
    for (k, v) in rec {
        if !first {
            write!(f, ", ")?;
        }
        first = false;
        write!(f, "{k} = {v}")?;
    }
    write!(f, ")")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_coercion() {
        let int_val = Value::Integer(42);
        assert_eq!(int_val.to_real(), Some(42.0));

        let real_val = Value::Real(2.5);
        assert_eq!(real_val.to_real(), Some(2.5));

        let bool_val = Value::Bool(true);
        assert_eq!(bool_val.to_real(), None);
    }

    #[test]
    fn integral_real_to_integer_never_saturates_out_of_range_values() {
        assert_eq!(Value::Real(i64::MIN as f64).as_integer(), Some(i64::MIN));
        assert_eq!(Value::Real(i64::MAX as f64).as_integer(), None);
        assert_eq!(Value::Real(f64::INFINITY).as_integer(), None);
        assert_eq!(Value::Real(f64::NEG_INFINITY).as_integer(), None);
        assert_eq!(Value::Real(f64::NAN).as_integer(), None);
    }

    #[test]
    fn empty_materialized_arrays_do_not_invent_trailing_extents() {
        let empty_matrix = Value::Array(Vec::new());
        let empty_tensor = Value::Array(vec![Value::Array(Vec::new()); 2]);
        assert!(matches!(
            checked_rectangular_shape(&empty_matrix, "Real", 1, Span::DUMMY),
            Ok(shape) if shape == vec![0]
        ));
        for (value, rank) in [(&empty_matrix, 2), (&empty_tensor, 3)] {
            assert!(matches!(
                checked_rectangular_shape(value, "Real", rank, Span::DUMMY),
                Err(EvalError::UnsupportedExpression { .. })
            ));
        }
    }

    #[test]
    fn materialized_nonprimitive_arrays_have_no_nominal_type_proof() {
        for (value, formal) in [
            (
                Value::Array(vec![Value::Enum("Mode".to_string(), "on".to_string())]),
                "Mode",
            ),
            (
                Value::Array(vec![Value::Record(IndexMap::new())]),
                "RecordType",
            ),
        ] {
            assert!(matches!(
                checked_rectangular_shape(&value, formal, 1, Span::DUMMY),
                Err(EvalError::UnsupportedExpression { .. })
            ));
        }
    }

    #[test]
    fn test_value_display() {
        assert_eq!(Value::Real(2.5).to_string(), "2.5");
        assert_eq!(Value::Integer(42).to_string(), "42");
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::String("hello".into()).to_string(), "\"hello\"");
        assert_eq!(
            Value::Enum("FilterType".into(), "LowPass".into()).to_string(),
            "FilterType.LowPass"
        );

        let arr = Value::Array(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        assert_eq!(arr.to_string(), "{1, 2, 3}");
    }

    #[test]
    fn test_value_array_len() {
        let arr = Value::Array(vec![Value::Integer(1), Value::Integer(2)]);
        assert_eq!(arr.len(), Some(2));
        assert_eq!(arr.is_empty(), Some(false));

        let scalar = Value::Integer(42);
        assert_eq!(scalar.len(), None);
    }
}
