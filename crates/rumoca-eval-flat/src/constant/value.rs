//! Value types for constant expression evaluation.
//!
//! This module defines the runtime value types used during constant
//! expression evaluation in the Modelica compiler.

use indexmap::IndexMap;

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
    /// Array of values (homogeneous)
    Array(Vec<Value>),
    /// Record with named fields
    Record(IndexMap<String, Value>),
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
            Self::Real(v) if v.fract() == 0.0 && v.is_finite() => Some(*v as i64),
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

    /// Try to get this value as a String.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s),
            _ => None,
        }
    }

    /// Try to get this value as an Enum (returns type_name.literal_name).
    pub fn as_enum(&self) -> Option<(&str, &str)> {
        match self {
            Self::Enum(type_name, literal) => Some((type_name, literal)),
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
            Self::Enum(_, _) => "Enumeration",
            Self::Array(_) => "Array",
            Self::Record(_) => "Record",
        }
    }

    /// Check if this value is numeric (Real or Integer).
    pub fn is_numeric(&self) -> bool {
        matches!(self, Self::Real(_) | Self::Integer(_))
    }

    /// Create a zero value of the same type.
    pub fn zero(&self) -> Self {
        match self {
            Self::Real(_) => Self::Real(0.0),
            Self::Integer(_) => Self::Integer(0),
            Self::Bool(_) => Self::Bool(false),
            Self::String(_) => Self::String(String::new()),
            Self::Enum(t, _) => Self::Enum(t.clone(), String::new()),
            Self::Array(arr) => Self::Array(arr.iter().map(|v| v.zero()).collect()),
            Self::Record(rec) => {
                Self::Record(rec.iter().map(|(k, v)| (k.clone(), v.zero())).collect())
            }
        }
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
    fn test_value_zero() {
        assert_eq!(Value::Real(2.5).zero(), Value::Real(0.0));
        assert_eq!(Value::Integer(42).zero(), Value::Integer(0));
        assert_eq!(Value::Bool(true).zero(), Value::Bool(false));
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
