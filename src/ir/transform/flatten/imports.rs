//! Import and package resolution for flatten operations.
//!
//! This module handles Modelica's name lookup rules:
//! - Import alias resolution (qualified, renamed, selective imports)
//! - Package alias resolution (replaceable packages)
//! - Extends chain type lookup
//! - Scope-based name resolution

use crate::ir;
use crate::ir::analysis::reference_checker::collect_imported_packages;
use crate::ir::ast::{Expression, Import, OpBinary};
use crate::ir::error::IrError;
use crate::ir::transform::constants::is_primitive_type;
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};

use super::{ClassDict, EXTENDS_CHAIN_CACHE, is_cache_enabled};

// =============================================================================
// Extends Modifications
// =============================================================================

/// Extract name=value pairs from extends clause modifications.
///
/// Extends modifications are stored as a Vec<Expression> containing Binary expressions
/// like `L = 1e-3` which are `Binary { op: Eq, lhs: ComponentReference("L"), rhs: value }`.
///
/// This function extracts these into an IndexMap for easy lookup.
pub(super) fn extract_extends_modifications(
    modifications: &[Expression],
) -> IndexMap<String, Expression> {
    let mut result = IndexMap::new();

    for expr in modifications {
        if let Expression::Binary { op, lhs, rhs } = expr
            && matches!(op, OpBinary::Eq(_))
            && let Expression::ComponentReference(comp_ref) = &**lhs
        {
            let param_name = comp_ref.to_string();
            result.insert(param_name, (**rhs).clone());
        }
    }

    result
}

/// Extract component type redeclarations from extends clause modifications.
///
/// Component redeclarations in extends clauses like:
///   extends VoltageSource(redeclare Modelica.Blocks.Sources.Sine signalSource(...))
///
/// are stored as Binary expressions:
///   Binary { op: Assign, lhs: ComponentReference("signalSource"), rhs: FunctionCall { comp: "Sine", args } }
///
/// This function extracts these into an IndexMap mapping component name to new type name.
/// It also returns a separate map with any nested modifications for each redeclared component.
///
/// Note: We exclude cases where the RHS is a builtin function call like `fill(0, n)` or `size(A, 1)`.
/// These are value assignments, not type redeclarations.
pub(super) fn extract_type_redeclarations(
    modifications: &[Expression],
) -> (IndexMap<String, String>, IndexMap<String, Vec<Expression>>) {
    use crate::ir::transform::constants::is_builtin_function;

    let mut type_changes = IndexMap::new();
    let mut nested_mods = IndexMap::new();

    for expr in modifications {
        if let Expression::Binary { op, lhs, rhs } = expr
            && matches!(op, OpBinary::Assign(_))
            && let Expression::ComponentReference(comp_ref) = &**lhs
            && let Expression::FunctionCall {
                comp: new_type,
                args,
            } = &**rhs
        {
            let comp_name = comp_ref.to_string();
            let new_type_name = new_type.to_string();

            // Skip builtin function calls - these are value assignments, not type redeclarations
            // e.g., T = fill(293.15, m) or n = size(A, 1)
            if is_builtin_function(&new_type_name) {
                continue;
            }

            type_changes.insert(comp_name.clone(), new_type_name);
            if !args.is_empty() {
                nested_mods.insert(comp_name, args.clone());
            }
        }
    }

    (type_changes, nested_mods)
}

/// Extract package/type modifications from component modifications.
///
/// Component modifications like `tank(redeclare package Medium = AirData)` are stored
/// in the modifications IndexMap with the key as the package name ("Medium") and
/// the value as a FunctionCall expression representing the new type ("AirData").
///
/// This function extracts these package redeclarations so they can be used as
/// import aliases when resolving types inside the component.
///
/// Note: We only extract FunctionCall with empty arguments, as those represent
/// type/package replacements (e.g., `Medium = AirData()`). FunctionCall with
/// arguments are actual function calls like `start = fill(0, n)` which should
/// not be treated as type modifications.
///
/// Returns a map from package name to the new type name.
pub(super) fn extract_package_modifications_from_component(
    modifications: &IndexMap<String, Expression>,
) -> IndexMap<String, String> {
    let mut result = IndexMap::new();

    for (name, expr) in modifications {
        // Package modifications are stored as FunctionCall expressions with no arguments
        // e.g., Medium = FunctionCall { comp: "AirData", args: [] }
        // We exclude FunctionCall with arguments as those are actual function calls
        // like start = fill(0, n) which are not type replacements.
        if let Expression::FunctionCall {
            comp: new_type,
            args,
        } = expr
            && args.is_empty()
        {
            result.insert(name.clone(), new_type.to_string());
        }
    }

    result
}

// =============================================================================
// Import Aliases
// =============================================================================

/// Builds a map of import aliases from a class's imports.
///
/// For renamed imports like `import D = Modelica.Electrical.Digital;`,
/// this creates an entry mapping "D" -> "Modelica.Electrical.Digital".
///
/// For qualified imports like `import Modelica.Electrical.Digital;`,
/// this creates an entry mapping "Digital" -> "Modelica.Electrical.Digital".
pub(super) fn build_import_aliases(imports: &[Import]) -> IndexMap<String, String> {
    let mut aliases = IndexMap::new();

    for import in imports {
        match import {
            Import::Renamed { alias, path, .. } => {
                // import D = A.B.C; => D -> A.B.C
                aliases.insert(alias.text.clone(), path.to_string());
            }
            Import::Qualified { path, .. } => {
                // import A.B.C; => C -> A.B.C
                if let Some(last) = path.name.last() {
                    aliases.insert(last.text.clone(), path.to_string());
                }
            }
            Import::Unqualified { .. } => {
                // import A.B.*; - handled differently (need to check all classes in A.B)
                // For now, skip as this requires checking the class dictionary
            }
            Import::Selective { path, names, .. } => {
                // import A.B.{C, D}; => C -> A.B.C, D -> A.B.D
                let base_path = path.to_string();
                for name in names {
                    aliases.insert(name.text.clone(), format!("{}.{}", base_path, name.text));
                }
            }
        }
    }

    aliases
}

/// Applies import aliases to a type name.
///
/// If the first part of the name is an import alias, replace it with the full path.
/// For example, with alias "D" -> "Modelica.Electrical.Digital":
/// - "D.Basic.Nor" becomes "Modelica.Electrical.Digital.Basic.Nor"
/// - "Real" stays "Real" (no alias)
pub(super) fn apply_import_aliases(name: &str, aliases: &IndexMap<String, String>) -> String {
    let parts: Vec<&str> = name.split('.').collect();
    if parts.is_empty() {
        return name.to_string();
    }

    let first = parts[0];
    if let Some(target) = aliases.get(first) {
        // Replace the first part with the full path
        if parts.len() == 1 {
            target.clone()
        } else {
            format!("{}.{}", target, parts[1..].join("."))
        }
    } else {
        name.to_string()
    }
}

/// Builds a combined import alias map from a class and all its enclosing scopes.
///
/// This collects imports from the class itself and all parent packages up to the root.
/// It also includes local package type aliases (replaceable packages), including
/// those inherited from extended classes.
pub(super) fn build_import_aliases_for_class(
    class_path: &str,
    class_dict: &ClassDict,
) -> IndexMap<String, String> {
    let mut all_aliases = IndexMap::new();

    // Collect imports and package aliases from each level of the class hierarchy (most specific first)
    let parts: Vec<&str> = class_path.split('.').collect();
    for i in (1..=parts.len()).rev() {
        let path = parts[..i].join(".");
        if let Some(class) = class_dict.get(&path) {
            // Add import aliases
            let aliases = build_import_aliases(&class.imports);
            for (alias, target) in aliases {
                all_aliases.entry(alias).or_insert(target);
            }

            // Add package type aliases (replaceable packages), including from extended classes
            let mut visited = IndexSet::new();
            let pkg_aliases =
                collect_package_aliases_recursive(class, &path, class_dict, &mut visited);
            for (alias, target) in pkg_aliases {
                all_aliases.entry(alias).or_insert(target);
            }
        }
    }

    all_aliases
}

// =============================================================================
// Import Validation
// =============================================================================

/// Checks if a path refers to a class or a component (constant/variable) within a class.
///
/// For example:
/// - "Modelica.Constants" -> true (it's a class)
/// - "Modelica.Constants.pi" -> true (pi is a component in Constants class)
/// - "Modelica.DoesNotExist" -> false
pub(super) fn path_exists_in_dict(path: &str, class_dict: &ClassDict) -> bool {
    // First check if it's a class
    if class_dict.contains_key(path) {
        return true;
    }

    // If not a class, check if it's a component in a parent class
    // Split path into parent.component
    if let Some(dot_pos) = path.rfind('.') {
        let parent_path = &path[..dot_pos];
        let component_name = &path[dot_pos + 1..];

        if let Some(parent_class) = class_dict.get(parent_path) {
            // Check if the component exists in the parent class
            if parent_class.components.contains_key(component_name) {
                return true;
            }
            // Also check nested classes (for cases like importing a nested type)
            if parent_class.classes.contains_key(component_name) {
                return true;
            }
        }
    }

    false
}

/// Validates that all imported classes exist in the class dictionary.
/// Returns an error if any import refers to a non-existent class or component.
pub(super) fn validate_imports(imports: &[Import], class_dict: &ClassDict) -> Result<()> {
    for import in imports {
        match import {
            Import::Renamed { path, .. } | Import::Qualified { path, .. } => {
                let target = path.to_string();
                if !path_exists_in_dict(&target, class_dict) {
                    return Err(IrError::ImportClassNotFound(target).into());
                }
            }
            Import::Selective { path, names, .. } => {
                let base_path = path.to_string();
                for name in names {
                    let full_path = format!("{}.{}", base_path, name.text);
                    if !path_exists_in_dict(&full_path, class_dict) {
                        return Err(IrError::ImportClassNotFound(full_path).into());
                    }
                }
            }
            Import::Unqualified { path, .. } => {
                // For unqualified imports (import A.B.*;), just check the base path exists
                let target = path.to_string();
                if !class_dict.contains_key(&target) {
                    return Err(IrError::ImportClassNotFound(target).into());
                }
            }
        }
    }
    Ok(())
}

// =============================================================================
// Package Aliases
// =============================================================================

/// Extracts package/model aliases from nested class definitions.
///
/// Replaceable packages like `replaceable package Medium = Modelica.Media.Interfaces.PartialMedium`
/// are represented as nested classes with an extends clause. This function extracts these as aliases
/// so that `Medium.X` can be resolved to `Modelica.Media.Interfaces.PartialMedium.X`.
///
/// Note: Type aliases (like `type MassFraction = Real` or `type AbsolutePressure = SI.AbsolutePressure`)
/// are NOT included. These are types that should be looked up in the class dictionary directly,
/// not used as package prefixes for resolving other names.
pub(super) fn build_package_aliases(class: &ir::ast::ClassDefinition) -> IndexMap<String, String> {
    use ir::ast::ClassType;
    let mut aliases = IndexMap::new();

    for (name, nested_class) in &class.classes {
        // Skip type definitions and connectors - they should be resolved as types
        // in the class dictionary, not as package prefixes.
        // Type aliases like "type Voltage = Real" and connector aliases like
        // "connector RealInput = input Real" or "connector ComplexInput = input Complex"
        // are type aliases, not package aliases.
        match nested_class.class_type {
            ClassType::Type | ClassType::Connector => continue,
            _ => {}
        }

        // Check if this is a short class definition (alias):
        // - Has exactly one extends clause
        // - Has no components or equations (just an alias)
        // This handles: package Medium = ..., model FlowModel = ..., etc.
        if nested_class.extends.len() == 1
            && nested_class.components.is_empty()
            && nested_class.equations.is_empty()
        {
            let target = nested_class.extends[0].comp.to_string();
            aliases.insert(name.clone(), target);
        }
    }

    aliases
}

/// Recursively collects package aliases from a class and all its extended classes.
pub(super) fn collect_package_aliases_recursive(
    class: &ir::ast::ClassDefinition,
    class_path: &str,
    class_dict: &ClassDict,
    visited: &mut IndexSet<String>,
) -> IndexMap<String, String> {
    let mut aliases = IndexMap::new();

    // Prevent infinite recursion
    if visited.contains(class_path) {
        return aliases;
    }
    visited.insert(class_path.to_string());

    // Add this class's package aliases, resolving relative paths to full paths
    let pkg_aliases = build_package_aliases(class);
    for (alias, target) in pkg_aliases {
        // Resolve the target path relative to this class's scope
        // e.g., "Air.MoistAir" in "Modelica.Media.Examples.MoistAir" -> "Modelica.Media.Air.MoistAir"
        let resolved_target =
            resolve_relative_to_package(&target, class_path, class_dict).unwrap_or(target);
        aliases.entry(alias).or_insert(resolved_target);
    }

    // Recursively collect from extended classes
    for extend in &class.extends {
        let parent_name = extend.comp.to_string();
        if is_primitive_type(&parent_name) {
            continue;
        }

        // Extract package redeclarations from extends clause modifications.
        // For example: extends PartialTestModel(redeclare package Medium = R134a_ph)
        // should add Medium -> R134a_ph to the aliases.
        let (extends_type_redecls, _) = extract_type_redeclarations(&extend.modifications);
        for (pkg_name, new_type) in extends_type_redecls {
            // Resolve the new type relative to this class's scope
            let resolved_target =
                resolve_relative_to_package(&new_type, class_path, class_dict).unwrap_or(new_type);
            // Insert with higher priority (child class overrides parent)
            aliases.insert(pkg_name, resolved_target);
        }

        // Resolve the parent name in the context of this class
        let parent_parts: Vec<&str> = class_path.split('.').collect();
        let mut resolved_parent = None;
        for i in (0..=parent_parts.len()).rev() {
            let prefix = parent_parts[..i].join(".");
            let candidate = if prefix.is_empty() {
                parent_name.clone()
            } else {
                format!("{}.{}", prefix, parent_name)
            };
            if class_dict.contains_key(&candidate) {
                resolved_parent = Some(candidate);
                break;
            }
        }

        if let Some(parent_class) = resolved_parent.as_ref().and_then(|p| class_dict.get(p)) {
            let parent_path = resolved_parent.unwrap();
            let parent_aliases =
                collect_package_aliases_recursive(parent_class, &parent_path, class_dict, visited);
            for (alias, target) in parent_aliases {
                aliases.entry(alias).or_insert(target);
            }
        }
    }

    aliases
}

/// Collects all names that should be treated as global (not prefixed) for a class.
///
/// This includes:
/// 1. Imported package root names (e.g., "Modelica" from `import Modelica.SIunits;`)
/// 2. Import alias names (e.g., "Types" from `import Types = Modelica.Blocks.Types;`)
/// 3. Nested class names from the class and enclosing packages
///
/// This is used by ScopeRenamer to avoid incorrectly prefixing package/type references.
pub(super) fn collect_imported_packages_for_class(
    class_path: &str,
    class_dict: &ClassDict,
) -> std::collections::HashSet<String> {
    let mut globals = std::collections::HashSet::new();

    // Walk up the class path hierarchy
    let parts: Vec<&str> = class_path.split('.').collect();
    for i in (1..=parts.len()).rev() {
        let path = parts[..i].join(".");
        if let Some(class) = class_dict.get(&path) {
            // 1. Collect imported package roots from this level
            let level_packages = collect_imported_packages(&class.imports);
            globals.extend(level_packages);

            // 2. Collect import alias names (the LHS of import X = Y;)
            for import in &class.imports {
                match import {
                    Import::Renamed { alias, .. } => {
                        globals.insert(alias.text.clone());
                    }
                    Import::Qualified { path, .. } => {
                        // import A.B.C; => C is an alias for A.B.C
                        if let Some(last) = path.name.last() {
                            globals.insert(last.text.clone());
                        }
                    }
                    Import::Selective { names, .. } => {
                        // import A.B.{C, D}; => C and D are aliases
                        for name in names {
                            globals.insert(name.text.clone());
                        }
                    }
                    Import::Unqualified { .. } => {
                        // import A.B.*; - would need to check all classes in A.B
                        // For now, skip as this is complex
                    }
                }
            }

            // 3. Collect nested class names
            for nested_name in class.classes.keys() {
                globals.insert(nested_name.clone());
            }
        }
    }

    globals
}

// =============================================================================
// Package Resolution
// =============================================================================

/// Resolves a relative type name in the context of a package.
///
/// For example, if `parent_name` is "Types" and `package_context` is
/// "Modelica.Media.Interfaces.PartialMedium", it will try:
/// - "Modelica.Media.Interfaces.PartialMedium.Types"
/// - "Modelica.Media.Interfaces.Types" (found!)
/// - "Modelica.Media.Types"
/// - "Modelica.Types"
/// - "Types"
pub(super) fn resolve_relative_to_package(
    parent_name: &str,
    package_context: &str,
    class_dict: &ClassDict,
) -> Option<String> {
    // If already fully qualified and exists, return it
    if class_dict.contains_key(parent_name) {
        return Some(parent_name.to_string());
    }

    // Try in enclosing scopes of the package context
    let parts: Vec<&str> = package_context.split('.').collect();
    for i in (0..=parts.len()).rev() {
        let prefix = parts[..i].join(".");
        let candidate = if prefix.is_empty() {
            parent_name.to_string()
        } else {
            format!("{}.{}", prefix, parent_name)
        };
        if class_dict.contains_key(&candidate) {
            return Some(candidate);
        }
    }

    None
}

// =============================================================================
// Extends Chain Lookup
// =============================================================================

/// Recursively searches for a type in a class's extends chain.
///
/// This walks the inheritance hierarchy looking for the type at each level:
/// 1. Check if type exists directly in the class
/// 2. Check parent scopes of the class
/// 3. Recursively search each extended class
///
/// Returns the fully qualified path to the type if found.
pub(super) fn find_type_in_extends_chain(
    type_name: &str,
    class_path: &str,
    class_dict: &ClassDict,
    visited: &mut IndexSet<String>,
) -> Option<String> {
    // Check cache first (only for top-level calls, not recursive ones)
    if visited.is_empty() && is_cache_enabled() {
        let cache_key = (type_name.to_string(), class_path.to_string());
        if let Some(result) = EXTENDS_CHAIN_CACHE
            .read()
            .ok()
            .and_then(|cache| cache.get(&cache_key).cloned())
        {
            return result;
        }
    }

    let result = find_type_in_extends_chain_uncached(type_name, class_path, class_dict, visited);

    // Store in cache (only for top-level calls)
    if visited.len() == 1 && is_cache_enabled() {
        // visited.len() == 1 means we just added this class_path (top-level call)
        let cache_key = (type_name.to_string(), class_path.to_string());
        if let Ok(mut cache) = EXTENDS_CHAIN_CACHE.write() {
            cache.insert(cache_key, result.clone());
        }
    }

    result
}

fn find_type_in_extends_chain_uncached(
    type_name: &str,
    class_path: &str,
    class_dict: &ClassDict,
    visited: &mut IndexSet<String>,
) -> Option<String> {
    // Prevent infinite recursion
    if visited.contains(class_path) {
        return None;
    }
    visited.insert(class_path.to_string());

    // 1. Check if type exists directly in this class
    let candidate = format!("{}.{}", class_path, type_name);
    if class_dict.contains_key(&candidate) {
        return Some(candidate);
    }

    // 2. Check parent scopes of this class
    let parts: Vec<&str> = class_path.split('.').collect();
    for i in (0..parts.len()).rev() {
        let prefix = parts[..i].join(".");
        let candidate = if prefix.is_empty() {
            type_name.to_string()
        } else {
            format!("{}.{}", prefix, type_name)
        };
        if class_dict.contains_key(&candidate) {
            return Some(candidate);
        }
    }

    // 3. Recursively search extended classes
    if let Some(class_def) = class_dict.get(class_path) {
        for extend in &class_def.extends {
            let parent_name = extend.comp.to_string();
            // Resolve the parent name relative to this class's scope and search recursively
            if let Some(found) = resolve_relative_to_package(&parent_name, class_path, class_dict)
                .and_then(|parent_path| {
                    find_type_in_extends_chain_uncached(
                        type_name,
                        &parent_path,
                        class_dict,
                        visited,
                    )
                })
            {
                return Some(found);
            }
        }
    }

    None
}

// =============================================================================
// Class Name Resolution
// =============================================================================

/// Resolves a class name by searching the enclosing scope hierarchy.
///
/// This function implements Modelica's name lookup rules for extends clauses:
/// 1. First applies import aliases to resolve aliased names
/// 2. Then tries an exact match (for fully qualified names)
/// 3. Then tries prepending enclosing package prefixes from most specific to least
/// 4. For package aliases (Medium.X), also searches parent scopes of the aliased package
///
/// For example, if `current_class_path` is `Modelica.Blocks.Continuous.Derivative`
/// and `name` is `Interfaces.SISO`, it will try:
/// - `Interfaces.SISO` (exact match)
/// - `Modelica.Blocks.Continuous.Interfaces.SISO`
/// - `Modelica.Blocks.Interfaces.SISO` (found!)
/// - `Modelica.Interfaces.SISO`
/// - `Interfaces.SISO` (at root level)
pub(super) fn resolve_class_name_with_imports(
    name: &str,
    current_class_path: &str,
    class_dict: &ClassDict,
    import_aliases: &IndexMap<String, String>,
) -> Option<String> {
    // 0. Apply import aliases first
    let resolved_name = apply_import_aliases(name, import_aliases);

    // 1. Try exact match first (handles fully qualified names)
    if class_dict.contains_key(&resolved_name) {
        return Some(resolved_name);
    }

    // 1.5. For package aliases like Medium.X where Medium -> Pkg.SubPkg.PartialMedium,
    // we need to search:
    // 1. The target package itself: PartialMedium.X
    // 2. Recursively search through extends chain for the type
    // 3. Parent scopes of the target: Pkg.SubPkg.X, Pkg.X, X
    let name_parts: Vec<&str> = name.split('.').collect();
    if name_parts.len() >= 2 {
        let first = name_parts[0];
        if let Some(target) = import_aliases.get(first) {
            let remainder = &name_parts[1..].join(".");

            // 1.5.1. Recursively search the target package and its entire extends chain
            // This handles cases like Medium.SpecificHeatCapacity where Medium extends
            // MixtureGasNasa extends PartialMixtureMedium extends PartialMedium,
            // and SpecificHeatCapacity is defined in PartialMedium.
            let mut visited = IndexSet::new();
            if let Some(found) =
                find_type_in_extends_chain(remainder, target, class_dict, &mut visited)
            {
                return Some(found);
            }

            // 1.5.2. Try parent scopes of the target (fallback for types defined
            // in sibling or parent packages, not through extends)
            let target_parts: Vec<&str> = target.split('.').collect();
            for i in (0..target_parts.len()).rev() {
                let prefix = target_parts[..i].join(".");
                let candidate = if prefix.is_empty() {
                    remainder.to_string()
                } else {
                    format!("{}.{}", prefix, remainder)
                };
                if class_dict.contains_key(&candidate) {
                    return Some(candidate);
                }
            }
        }
    }

    // 2. Try prepending enclosing scope prefixes (including current class)
    // For a class path like "SwitchController", we need to try:
    //   - "SwitchController.SwitchState" (nested type in current class)
    //   - "SwitchState" (at root level)
    // For a path like "Modelica.Blocks.Continuous.PID", we try:
    //   - "Modelica.Blocks.Continuous.PID.TypeName"
    //   - "Modelica.Blocks.Continuous.TypeName"
    //   - "Modelica.Blocks.TypeName"
    //   - "Modelica.TypeName"
    //   - "TypeName"
    let parts: Vec<&str> = current_class_path.split('.').collect();
    for i in (0..=parts.len()).rev() {
        let prefix = parts[..i].join(".");
        let candidate = if prefix.is_empty() {
            resolved_name.clone()
        } else {
            format!("{}.{}", prefix, resolved_name)
        };
        if class_dict.contains_key(&candidate) {
            return Some(candidate);
        }
    }

    // 2.5. Recursively search the extends chain of enclosing classes
    // This handles cases like MoistAir.BaseProperties using MassFraction,
    // where MassFraction is defined deep in the inheritance chain:
    // MoistAir extends PartialCondensingGases extends PartialMixtureMedium extends PartialMedium
    // and MassFraction is defined in PartialMedium.
    for i in (1..=parts.len()).rev() {
        let class_path = parts[..i].join(".");
        // Use the recursive helper to search through the entire extends chain
        let mut visited = IndexSet::new();
        if let Some(found) =
            find_type_in_extends_chain(&resolved_name, &class_path, class_dict, &mut visited)
        {
            return Some(found);
        }
    }

    // 3. If import alias resolution changed the name but still not found,
    //    try with the original name too
    if resolved_name != name {
        if class_dict.contains_key(name) {
            return Some(name.to_string());
        }
        for i in (0..=parts.len()).rev() {
            let prefix = parts[..i].join(".");
            let candidate = if prefix.is_empty() {
                name.to_string()
            } else {
                format!("{}.{}", prefix, name)
            };
            if class_dict.contains_key(&candidate) {
                return Some(candidate);
            }
        }
    }

    None
}
