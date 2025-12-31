//! Clock type restrictions.
//!
//! MLS §16.1: Clock type semantics

use crate::ir::ast::{ClassDefinition, Connection, Variability};

use crate::ir::analysis::type_inference::SymbolType;

use super::super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Check that Clock variables don't have flow, stream, or discrete prefixes (MLS §16.1).
///
/// MLS §16.1: "Clock shall not be combined with flow, stream, or discrete."
/// This validates that Clock type variables don't use these incompatible prefixes.
pub fn check_clock_restrictions(class: &ClassDefinition) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();

    for (name, comp) in &class.components {
        // Check if this is a Clock type
        let type_name = comp.type_name.to_string();
        if type_name != "Clock" {
            continue;
        }

        // Check for flow prefix
        if let Connection::Flow(token) = &comp.connection {
            result.add_error(TypeError::new(
                token.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Clock variable '{}' cannot have flow prefix - Clock is not a physical quantity",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }

        // Check for stream prefix
        if let Connection::Stream(token) = &comp.connection {
            result.add_error(TypeError::new(
                token.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Clock variable '{}' cannot have stream prefix - Clock is not a physical quantity",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }

        // Check for discrete prefix
        if let Variability::Discrete(token) = &comp.variability {
            result.add_error(TypeError::new(
                token.location.clone(),
                SymbolType::Unknown,
                SymbolType::Unknown,
                format!(
                    "Clock variable '{}' cannot have discrete prefix - Clock has its own timing semantics",
                    name
                ),
                TypeErrorSeverity::Error,
            ));
        }
    }

    result
}
