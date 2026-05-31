//! Modelica language builtins that are defined by the MLS, not by the MSL.
//!
//! The predefined type declarations below are intentionally compiler-owned.
//! The Modelica specification presents these types with Modelica syntax even
//! though their primitive values and attribute access restrictions are special.

use crate::BuiltinFunction;

/// Synthetic source name used for compiler-owned MLS-shaped predefined declarations.
pub const PREDEFINED_MODELICA_SOURCE_NAME: &str = "<rumoca-predefined-mls>";

/// Modelica-shaped source for the predefined type/class declarations.
///
/// This is the single source of truth for the implicit predefined type shape
/// from MLS §4.9. The primitive `*Type` value declarations are specification
/// mnemonics and are not user-visible dot fields.
pub const PREDEFINED_MODELICA_SOURCE: &str = include_str!("../modelica/Predefined.mls");

/// Predefined primitive type class names with MLS §4.9 attribute shape.
pub const PREDEFINED_COMPONENT_TYPES: &[&str] = &["Real", "Integer", "Boolean", "String"];

/// Predefined enumeration type names supplied by the language.
pub const PREDEFINED_ENUM_TYPES: &[&str] = &["StateSelect", "AssertionLevel"];

const REAL_ATTRIBUTES: &[&str] = &[
    "quantity",
    "unit",
    "displayUnit",
    "min",
    "max",
    "start",
    "fixed",
    "nominal",
    "unbounded",
    "stateSelect",
];

const INTEGER_ATTRIBUTES: &[&str] = &["quantity", "min", "max", "start", "fixed"];
const BOOLEAN_ATTRIBUTES: &[&str] = &["quantity", "start", "fixed"];
const STRING_ATTRIBUTES: &[&str] = &["quantity", "start", "fixed"];

/// All predefined component attribute names across the primitive types.
pub const PREDEFINED_COMPONENT_ATTRIBUTES: &[&str] = &[
    "quantity",
    "unit",
    "displayUnit",
    "min",
    "max",
    "start",
    "fixed",
    "nominal",
    "unbounded",
    "stateSelect",
];

/// Return true if `name` is one of the MLS predefined primitive component types.
pub fn is_predefined_component_type(name: &str) -> bool {
    PREDEFINED_COMPONENT_TYPES.contains(&name)
}

/// Return true if user source may not redeclare `name`.
pub fn is_predefined_user_redeclaration(name: &str) -> bool {
    is_predefined_component_type(name)
}

/// Return the MLS attribute names available on the predefined component type.
pub fn predefined_component_attribute_names(type_name: &str) -> &'static [&'static str] {
    match type_name {
        "Real" => REAL_ATTRIBUTES,
        "Integer" => INTEGER_ATTRIBUTES,
        "Boolean" => BOOLEAN_ATTRIBUTES,
        "String" => STRING_ATTRIBUTES,
        _ => &[],
    }
}

/// Return true if `attribute_name` is an attribute of the predefined type.
pub fn is_predefined_component_attribute(type_name: &str, attribute_name: &str) -> bool {
    predefined_component_attribute_names(type_name).contains(&attribute_name)
}

/// Return true if `attribute_name` is any predefined primitive-type attribute.
pub fn is_any_predefined_component_attribute(attribute_name: &str) -> bool {
    PREDEFINED_COMPONENT_ATTRIBUTES.contains(&attribute_name)
}

/// Modification names accepted by the parser on primitive typed components.
///
/// `each` and `final` are modification prefixes rather than attributes, but
/// older parser paths surface them through the same validation hook.
pub fn predefined_component_modification_names(type_name: &str) -> Vec<&'static str> {
    let mut names = predefined_component_attribute_names(type_name).to_vec();
    if !names.is_empty() {
        names.extend(["each", "final"]);
    }
    names
}

impl BuiltinFunction {
    /// True for unary math functions whose compile-time result is always Real.
    ///
    /// `abs`, `sign`, `floor`, and `ceil` need type-preserving or Integer
    /// result handling in constant evaluators, so they are intentionally
    /// excluded from this Real-only predicate.
    pub fn is_unary_real_math(self) -> bool {
        matches!(
            self,
            BuiltinFunction::Sqrt
                | BuiltinFunction::Sin
                | BuiltinFunction::Cos
                | BuiltinFunction::Tan
                | BuiltinFunction::Asin
                | BuiltinFunction::Acos
                | BuiltinFunction::Atan
                | BuiltinFunction::Sinh
                | BuiltinFunction::Cosh
                | BuiltinFunction::Tanh
                | BuiltinFunction::Exp
                | BuiltinFunction::Log
                | BuiltinFunction::Log10
        )
    }
}

/// Apply a unary scalar math builtin.
pub fn apply_scalar_unary_math(function: BuiltinFunction, arg: f64) -> Option<f64> {
    match function {
        BuiltinFunction::Abs => Some(arg.abs()),
        BuiltinFunction::Sign => Some(arg.signum()),
        BuiltinFunction::Sqrt => Some(arg.sqrt()),
        BuiltinFunction::Floor => Some(arg.floor()),
        BuiltinFunction::Ceil => Some(arg.ceil()),
        BuiltinFunction::Sin => Some(arg.sin()),
        BuiltinFunction::Cos => Some(arg.cos()),
        BuiltinFunction::Tan => Some(arg.tan()),
        BuiltinFunction::Asin => Some(arg.asin()),
        BuiltinFunction::Acos => Some(arg.acos()),
        BuiltinFunction::Atan => Some(arg.atan()),
        BuiltinFunction::Sinh => Some(arg.sinh()),
        BuiltinFunction::Cosh => Some(arg.cosh()),
        BuiltinFunction::Tanh => Some(arg.tanh()),
        BuiltinFunction::Exp => Some(arg.exp()),
        BuiltinFunction::Log => Some(arg.ln()),
        BuiltinFunction::Log10 => Some(arg.log10()),
        _ => None,
    }
}

/// Apply a binary scalar math builtin.
pub fn apply_scalar_binary_math(function: BuiltinFunction, lhs: f64, rhs: f64) -> Option<f64> {
    match function {
        BuiltinFunction::Atan2 => Some(lhs.atan2(rhs)),
        BuiltinFunction::Min => Some(lhs.min(rhs)),
        BuiltinFunction::Max => Some(lhs.max(rhs)),
        BuiltinFunction::Div => (rhs.abs() > f64::EPSILON).then_some((lhs / rhs).trunc()),
        BuiltinFunction::Mod => {
            (rhs.abs() > f64::EPSILON).then_some(lhs - (lhs / rhs).floor() * rhs)
        }
        BuiltinFunction::Rem => {
            (rhs.abs() > f64::EPSILON).then_some(lhs - (lhs / rhs).trunc() * rhs)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn predefined_source_contains_all_component_type_shapes() {
        for type_name in PREDEFINED_COMPONENT_TYPES {
            assert!(
                PREDEFINED_MODELICA_SOURCE.contains(&format!("type {type_name}")),
                "missing predefined source for {type_name}"
            );
            for attribute in predefined_component_attribute_names(type_name) {
                assert!(
                    PREDEFINED_MODELICA_SOURCE.contains(attribute),
                    "predefined source for {type_name} should mention {attribute}"
                );
            }
        }
    }

    #[test]
    fn boolean_shape_does_not_inherit_real_only_attributes() {
        assert!(is_predefined_component_attribute("Boolean", "start"));
        assert!(!is_predefined_component_attribute("Boolean", "min"));
        assert!(!is_predefined_component_attribute("Boolean", "unit"));
    }
}
