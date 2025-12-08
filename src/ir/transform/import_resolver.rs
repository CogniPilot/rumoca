//! Import resolver visitor
//!
//! This visitor resolves import clauses by rewriting function calls
//! to use their fully qualified names based on the imports in scope.

use crate::ir::ast::{ClassDefinition, ComponentReference, Expression, Import, StoredDefinition};
use crate::ir::visitor::MutVisitor;
use indexmap::IndexMap;

/// Visitor that resolves imported names to their fully qualified forms
pub struct ImportResolver {
    /// Map from short name to fully qualified name
    name_map: IndexMap<String, String>,
}

impl ImportResolver {
    /// Create a new import resolver for a class with the given stored definition context
    pub fn new(class: &ClassDefinition, stored_def: &StoredDefinition) -> Self {
        let mut name_map = IndexMap::new();

        for import in &class.imports {
            match import {
                Import::Qualified { path, .. } => {
                    // import A.B.C; -> C maps to A.B.C
                    let full_path = path.to_string();
                    if let Some(last_part) = path.name.last() {
                        name_map.insert(last_part.text.clone(), full_path);
                    }
                }
                Import::Renamed { alias, path, .. } => {
                    // import D = A.B.C; -> D maps to A.B.C
                    let full_path = path.to_string();
                    name_map.insert(alias.text.clone(), full_path);
                }
                Import::Unqualified { path, .. } => {
                    // import A.B.*; -> all names from A.B are imported
                    let package_path = path.to_string();
                    // Find the package and import all its public names
                    if let Some(package) = find_class_by_path(stored_def, &package_path) {
                        // Import all functions and classes from the package
                        for (name, _) in &package.classes {
                            let full_path = format!("{}.{}", package_path, name);
                            name_map.insert(name.clone(), full_path);
                        }
                    }
                }
                Import::Selective { path, names, .. } => {
                    // import A.B.{C, D}; -> C maps to A.B.C, D maps to A.B.D
                    let package_path = path.to_string();
                    for name_token in names {
                        let full_path = format!("{}.{}", package_path, name_token.text);
                        name_map.insert(name_token.text.clone(), full_path);
                    }
                }
            }
        }

        Self { name_map }
    }

    /// Resolve a function name using the import map
    pub fn resolve(&self, name: &str) -> Option<&String> {
        self.name_map.get(name)
    }
}

/// Find a class by its dot-separated path in the stored definition
fn find_class_by_path<'a>(
    stored_def: &'a StoredDefinition,
    path: &str,
) -> Option<&'a ClassDefinition> {
    let parts: Vec<&str> = path.split('.').collect();
    if parts.is_empty() {
        return None;
    }

    // Start with the first part at the top level
    let mut current = stored_def.class_list.get(parts[0])?;

    // Navigate through the remaining parts
    for part in parts.iter().skip(1) {
        current = current.classes.get(*part)?;
    }

    Some(current)
}

impl MutVisitor for ImportResolver {
    fn exit_expression(&mut self, expr: &mut Expression) {
        if let Expression::FunctionCall { comp, args: _ } = expr {
            // Get the function name
            let func_name = comp.to_string();

            // If this is a simple name (no dots) and we have a mapping, resolve it
            if !func_name.contains('.')
                && let Some(full_path) = self.name_map.get(&func_name)
            {
                // Rewrite the component reference to use the full path
                let parts: Vec<&str> = full_path.split('.').collect();
                let new_parts: Vec<crate::ir::ast::ComponentRefPart> = parts
                    .iter()
                    .map(|p| crate::ir::ast::ComponentRefPart {
                        ident: crate::ir::ast::Token {
                            text: p.to_string(),
                            ..Default::default()
                        },
                        subs: None,
                    })
                    .collect();

                *comp = ComponentReference {
                    local: false,
                    parts: new_parts,
                };
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_class_by_path() {
        // Create a simple stored definition with nested packages
        let mut stored_def = StoredDefinition::default();

        let mut math_package = ClassDefinition::default();
        math_package.name.text = "MathLib".to_string();

        let mut add_func = ClassDefinition::default();
        add_func.name.text = "add".to_string();
        math_package.classes.insert("add".to_string(), add_func);

        stored_def
            .class_list
            .insert("MathLib".to_string(), math_package);

        // Test finding MathLib
        let found = find_class_by_path(&stored_def, "MathLib");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name.text, "MathLib");

        // Test finding MathLib.add
        let found = find_class_by_path(&stored_def, "MathLib.add");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name.text, "add");

        // Test not finding non-existent path
        let found = find_class_by_path(&stored_def, "NonExistent");
        assert!(found.is_none());
    }
}
