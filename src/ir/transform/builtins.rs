//! Built-in Modelica types and classes.
//!
//! This module provides predefined Modelica types (like Complex) that should be
//! available in all scopes without explicit import. The definitions are written
//! in Modelica syntax and parsed at startup.

use crate::ir::ast::StoredDefinition;
use std::sync::LazyLock;

/// Built-in Modelica definitions as source code.
///
/// These types are predefined in the Modelica language specification and should
/// be available without explicit import. Add new built-in types here.
const BUILTIN_SOURCE: &str = r#"
// Complex number operator record (Modelica 3.4+ built-in)
// This is a simplified version - the full version includes operator overloading
operator record Complex "Complex number with overloaded operators"
  Real re "Real part of complex number";
  Real im "Imaginary part of complex number";
end Complex;

// ModelicaServices stub - provides tool-specific types
// This is a minimal implementation to allow MSL models to compile
package ModelicaServices "ModelicaServices (default implementation)"
  package Types "Type definitions"
    type SolverMethod = String "Solver method for clocked partitions";
  end Types;
end ModelicaServices;
"#;

/// Parsed built-in definitions, lazily initialized on first access.
static BUILTIN_DEFS: LazyLock<Option<StoredDefinition>> =
    LazyLock::new(|| crate::compiler::parse_source_simple(BUILTIN_SOURCE, "<builtins>"));

/// Returns the parsed built-in definitions, if available.
pub fn get_builtin_definitions() -> Option<&'static StoredDefinition> {
    BUILTIN_DEFS.as_ref()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_builtin_source_parses() {
        let defs = get_builtin_definitions();
        assert!(defs.is_some(), "Built-in source should parse successfully");

        let defs = defs.unwrap();
        assert!(
            defs.class_list.contains_key("Complex"),
            "Should contain Complex type"
        );

        let complex = &defs.class_list["Complex"];
        assert!(
            complex.components.contains_key("re"),
            "Complex should have 're' component"
        );
        assert!(
            complex.components.contains_key("im"),
            "Complex should have 'im' component"
        );

        // Check ModelicaServices package
        assert!(
            defs.class_list.contains_key("ModelicaServices"),
            "Should contain ModelicaServices package"
        );

        // Check ModelicaServices.Types nested package
        let modelica_services = &defs.class_list["ModelicaServices"];
        assert!(
            modelica_services.classes.contains_key("Types"),
            "ModelicaServices should have 'Types' nested package"
        );

        // Check ModelicaServices.Types.SolverMethod type
        let types_pkg = &modelica_services.classes["Types"];
        assert!(
            types_pkg.classes.contains_key("SolverMethod"),
            "Types should have 'SolverMethod' nested type"
        );
    }
}
