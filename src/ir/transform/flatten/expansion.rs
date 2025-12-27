//! Component expansion during flattening.
//!
//! This module provides the ExpansionContext which handles recursive component
//! expansion, including inner/outer resolution and subcomponent propagation.

use crate::ir;
use crate::ir::ast::{ComponentRefPart, Expression, Token};
use crate::ir::error::IrError;
use crate::ir::transform::constants::is_primitive_type;
use crate::ir::transform::sub_comp_namer::SubCompNamer;
use crate::ir::visitor::{MutVisitable, MutVisitor};
use anyhow::Result;
use indexmap::{IndexMap, IndexSet};

use super::hash::FileDependencies;
use super::helpers::{
    is_operator_record_type, is_simple_literal, make_binding_eq, try_evaluate_modification,
};
use super::imports::{
    build_import_aliases_for_class, extract_package_modifications_from_component,
    find_type_in_extends_chain, resolve_class_name_with_imports,
};
use super::validation::check_nested_component_subscripts;
use super::{ClassDict, ScopeRenamer, SymbolTable, resolve_class};

// =============================================================================
// Inner/Outer Resolution
// =============================================================================

/// Track inner components for inner/outer resolution.
/// Maps (type_name, component_name) -> flattened component name
/// For example, ("World", "world") -> "world" means there's an inner World world at the top level
pub(super) type InnerMap = IndexMap<(String, String), String>;

/// Visitor that renames outer component references to point to their inner counterparts.
/// For example, if "child.world" is outer and maps to inner "world",
/// then "child.world.g" gets renamed to "world.g".
#[derive(Debug, Clone, Default)]
pub(super) struct OuterRenamer {
    /// Maps outer component path prefix to inner component path
    /// e.g., "child.world" -> "world"
    outer_to_inner: IndexMap<String, String>,
}

impl OuterRenamer {
    pub(super) fn add_mapping(&mut self, outer_path: &str, inner_path: &str) {
        self.outer_to_inner
            .insert(outer_path.to_string(), inner_path.to_string());
    }
}

impl MutVisitor for OuterRenamer {
    fn exit_component_reference(&mut self, node: &mut ir::ast::ComponentReference) {
        let ref_name = node.to_string();

        // Check if this reference starts with an outer path that needs renaming
        for (outer_path, inner_path) in &self.outer_to_inner {
            if ref_name == *outer_path {
                // Exact match - replace entire reference
                node.parts = vec![ComponentRefPart {
                    ident: Token {
                        text: inner_path.clone(),
                        ..Default::default()
                    },
                    subs: None,
                }];
                return;
            } else if ref_name.starts_with(&format!("{}.", outer_path)) {
                // Reference to subcomponent of outer - replace prefix
                let suffix = &ref_name[outer_path.len()..]; // includes the leading "."
                let new_ref = format!("{}{}", inner_path, suffix);
                node.parts = vec![ComponentRefPart {
                    ident: Token {
                        text: new_ref,
                        ..Default::default()
                    },
                    subs: None,
                }];
                return;
            }
        }
    }
}

// =============================================================================
// Expansion Context
// =============================================================================

/// Context for component expansion during flattening.
/// Groups all the mutable state needed during recursive expansion.
pub(super) struct ExpansionContext<'a> {
    /// The flattened class being built
    pub(super) fclass: &'a mut ir::ast::ClassDefinition,
    /// Dictionary of all available classes
    pub(super) class_dict: &'a ClassDict,
    /// Symbol table for scope tracking
    pub(super) symbol_table: &'a SymbolTable,
    /// Maps flattened pin names to their connector types
    pub(super) pin_types: IndexMap<String, String>,
    /// Maps (type_name, component_name) -> inner component's flattened name
    pub(super) inner_map: InnerMap,
    /// Tracks outer->inner mappings for equation rewriting
    pub(super) outer_renamer: OuterRenamer,
    /// Content hash of StoredDefinition for cache key stability
    pub(super) def_hash: u64,
    /// Collected file dependencies from all resolved classes
    pub(super) deps: FileDependencies,
}

impl<'a> ExpansionContext<'a> {
    pub(super) fn new(
        fclass: &'a mut ir::ast::ClassDefinition,
        class_dict: &'a ClassDict,
        symbol_table: &'a SymbolTable,
        def_hash: u64,
    ) -> Self {
        Self {
            fclass,
            class_dict,
            symbol_table,
            pin_types: IndexMap::new(),
            inner_map: IndexMap::new(),
            outer_renamer: OuterRenamer::default(),
            def_hash,
            deps: FileDependencies::new(),
        }
    }

    /// Register top-level inner components
    pub(super) fn register_inner_components(
        &mut self,
        components: &IndexMap<String, ir::ast::Component>,
    ) {
        for (comp_name, comp) in components {
            if comp.inner {
                let key = (comp.type_name.to_string(), comp_name.clone());
                self.inner_map.insert(key, comp_name.clone());
            }
        }
    }

    /// Apply outer renaming to all equations
    pub(super) fn apply_outer_renaming(&mut self) {
        self.fclass.accept_mut(&mut self.outer_renamer);
    }

    /// Expand a component recursively
    pub(super) fn expand_component(
        &mut self,
        comp_name: &str,
        comp: &ir::ast::Component,
        current_class_path: &str,
    ) -> Result<()> {
        let type_name = comp.type_name.to_string();

        // Skip primitive types - they don't need expansion
        if is_primitive_type(&type_name) {
            return Ok(());
        }

        // Build import aliases for the current class path
        let import_aliases = build_import_aliases_for_class(current_class_path, self.class_dict);

        // Resolve the type name using enclosing scope search and import aliases
        let resolved_type_name = match resolve_class_name_with_imports(
            &type_name,
            current_class_path,
            self.class_dict,
            &import_aliases,
        ) {
            Some(name) => name,
            None => {
                // Type is not primitive and not found in class dictionary - this is an error
                return Err(IrError::ComponentClassNotFound(type_name).into());
            }
        };

        // Get the component class
        let comp_class_raw = match self.class_dict.get(&resolved_type_name) {
            Some(c) => c,
            None => return Ok(()), // Should not happen after resolve_class_name succeeded
        };

        // Check that we're not instantiating a partial class (MLS §4.5)
        if comp_class_raw.partial {
            return Err(anyhow::anyhow!(
                "Cannot instantiate partial class '{}' as component '{}' at line {}",
                resolved_type_name,
                comp_name,
                comp.name_token.location.start_line
            ));
        }

        // Resolve the component class (handle its extends clauses)
        let (comp_class, comp_deps) = resolve_class(
            comp_class_raw,
            &resolved_type_name,
            self.class_dict,
            self.def_hash,
        )?;

        // Collect dependencies from this resolved class
        for (file, hash) in &comp_deps.files {
            self.deps.record(file, hash);
        }

        // Record the connector type for this component BEFORE checking if it has sub-components.
        // This is critical for connectors like Pin that have only primitive types (Real v, Real i).
        // These connectors have no class-type sub-components but are still used in connect equations.
        self.pin_types
            .insert(comp_name.to_string(), resolved_type_name.clone());

        // If the resolved class has no components, it's effectively a type alias (like Voltage = Real)
        // or a "leaf" connector with only primitive types.
        // Don't remove the component, just add any equations and algorithms it might have.
        if comp_class.components.is_empty() {
            // Still add any equations from the type alias (though rare)
            // Use with_class_imports to handle references to packages from the class hierarchy
            let mut renamer = ScopeRenamer::with_class_imports(
                self.symbol_table,
                comp_name,
                &resolved_type_name,
                self.class_dict,
            );
            for eq in &comp_class.equations {
                let mut feq = eq.clone();
                feq.accept_mut(&mut renamer);
                self.fclass.equations.push(feq);
            }
            // Add algorithm sections from leaf component
            for algo_section in &comp_class.algorithms {
                let mut scoped_section = Vec::new();
                for stmt in algo_section {
                    let mut fstmt = stmt.clone();
                    fstmt.accept_mut(&mut renamer);
                    scoped_section.push(fstmt);
                }
                self.fclass.algorithms.push(scoped_section);
            }
            return Ok(());
        }

        // Create a scope renamer for this component with imports from its class hierarchy.
        // This ensures references like "Modelica.Constants.pi" are not prefixed with the component name.
        let mut renamer = ScopeRenamer::with_class_imports(
            self.symbol_table,
            comp_name,
            &resolved_type_name,
            self.class_dict,
        );

        // Add equations from component class, with scoped variable references
        for eq in &comp_class.equations {
            let mut feq = eq.clone();
            feq.accept_mut(&mut renamer);
            self.fclass.equations.push(feq);
        }

        // Add algorithm sections from component class, with scoped variable references
        for algo_section in &comp_class.algorithms {
            let mut scoped_section = Vec::new();
            for stmt in algo_section {
                let mut fstmt = stmt.clone();
                fstmt.accept_mut(&mut renamer);
                scoped_section.push(fstmt);
            }
            self.fclass.algorithms.push(scoped_section);
        }

        // Check if this is an operator record (like Complex) that needs special subscript handling.
        // For operator records, array subscripts move from the component to its fields:
        // u[1].re becomes u.re[1] because after flattening, u.re is an array.
        let is_operator_record = is_operator_record_type(&comp_class, &resolved_type_name);

        // Check for out-of-bounds subscripts on nested component references BEFORE SubCompNamer
        // transforms them. After SubCompNamer, subscripts like a[3] in a[3].x are lost.
        // This handles cases like: A a[2]; Real y = a[3].x[1]; where a[3] is out of bounds.
        check_nested_component_subscripts(self.fclass, comp_name, comp)?;

        // Expand comp.sub_comp names to use dots in existing equations
        self.fclass.accept_mut(&mut SubCompNamer {
            comp: comp_name.to_string(),
            is_operator_record,
        });

        // Extract package modifications from the parent component.
        // For example, if the parent has `tank(redeclare package Medium = AirData)`,
        // we need to resolve types like `Medium.BaseProperties` inside the tank
        // to `AirData.BaseProperties` instead of `PartialMedium.BaseProperties`.
        let package_mods = extract_package_modifications_from_component(&comp.modifications);

        // Build augmented import aliases that include resolved package modifications.
        // These are used to resolve subcomponent types.
        let comp_class_import_aliases =
            build_import_aliases_for_class(&resolved_type_name, self.class_dict);
        let mut augmented_aliases = comp_class_import_aliases.clone();

        // Also build a mapping from the original fully qualified package names to new types.
        // This is needed because component types may already be fully qualified, e.g.,
        // Medium.BaseProperties was resolved to Modelica.Media.Interfaces.PartialMedium.BaseProperties
        // before this expansion. We need to map that back to the new package.
        let mut fq_package_replacements: IndexMap<String, String> = IndexMap::new();

        for (pkg_name, new_type) in &package_mods {
            // Resolve the new type in the context of the parent's scope (current_class_path)
            let resolved_new_type = resolve_class_name_with_imports(
                new_type,
                current_class_path,
                self.class_dict,
                &import_aliases,
            )
            .unwrap_or_else(|| new_type.clone());
            augmented_aliases.insert(pkg_name.clone(), resolved_new_type.clone());

            // Also look up the original package definition in the component's class
            // and add its fully qualified name as a replacement target.
            // For example, if the class has "replaceable package Medium = PartialMedium",
            // we need to also add "Modelica.Media.Interfaces.PartialMedium" -> resolved_new_type

            // First try the local class definitions
            if let Some(nested_class) = comp_class.classes.get(pkg_name) {
                // If this nested class extends another package, get the original
                if let Some(ext) = nested_class.extends.first() {
                    let original_pkg = ext.comp.to_string();
                    // Resolve the original package to fully qualified name
                    if let Some(fq_original) = resolve_class_name_with_imports(
                        &original_pkg,
                        &resolved_type_name,
                        self.class_dict,
                        &comp_class_import_aliases,
                    ) {
                        fq_package_replacements.insert(fq_original, resolved_new_type.clone());
                    }
                }
            } else {
                // If not found locally, search the extends chain to find the inherited package.
                // For example, TeeJunctionVolume extends PartialLumpedVolume, and Medium is
                // defined in PartialLumpedVolume. We need to find PartialLumpedVolume.Medium
                // and add it to the replacement map.
                let mut search_visited = IndexSet::new();
                if let Some(found_pkg_path) = find_type_in_extends_chain(
                    pkg_name,
                    &resolved_type_name,
                    self.class_dict,
                    &mut search_visited,
                ) {
                    // found_pkg_path is like "Modelica.Fluid.Interfaces.PartialLumpedVolume.Medium"
                    // Get the original package this aliases to
                    if let Some(class_def) = self.class_dict.get(&found_pkg_path)
                        && let Some(ext) = class_def.extends.first()
                    {
                        let original_pkg = ext.comp.to_string();
                        // Resolve the original package to fully qualified name
                        // We need to use the context of where the package was found
                        let pkg_context = found_pkg_path
                            .rsplit_once('.')
                            .map(|(prefix, _)| prefix.to_string())
                            .unwrap_or_default();
                        let pkg_context_aliases =
                            build_import_aliases_for_class(&pkg_context, self.class_dict);

                        if let Some(fq_original) = resolve_class_name_with_imports(
                            &original_pkg,
                            &pkg_context,
                            self.class_dict,
                            &pkg_context_aliases,
                        ) {
                            fq_package_replacements.insert(fq_original, resolved_new_type.clone());
                        }
                    }
                }
            }
        }

        // Collect subcomponents, handling inner/outer
        let mut subcomponents: Vec<(String, ir::ast::Component)> = Vec::new();
        for (subcomp_name, subcomp) in &comp_class.components {
            // Handle outer components: they reference an inner component from enclosing scope
            if subcomp.outer {
                let subcomp_type = subcomp.type_name.to_string();
                // Look for matching inner component
                let key = (subcomp_type, subcomp_name.clone());
                if let Some(inner_name) = self.inner_map.get(&key) {
                    // Outer component resolves to inner - don't create a new variable
                    // Record the mapping for equation rewriting
                    let outer_path = format!("{}.{}", comp_name, subcomp_name);
                    self.outer_renamer.add_mapping(&outer_path, inner_name);
                    continue;
                }
                // No matching inner found - could be an error or external dependency
                // For now, create the component anyway
            }

            let mut scomp = subcomp.clone();
            let name = format!("{}.{}", comp_name, subcomp_name);
            scomp.name = name.clone();

            // If there are package modifications, resolve the subcomponent's type using
            // the augmented aliases and update the type_name to the fully resolved path.
            // This ensures that when we recursively expand the subcomponent, it uses
            // the concrete type (e.g., AirData.BaseProperties) instead of the partial
            // type (e.g., PartialMedium.BaseProperties).
            if !package_mods.is_empty() {
                let original_type = scomp.type_name.to_string();
                let mut resolved_type = original_type.clone();

                // First, check if the type starts with any of the fully qualified original packages
                // that we're replacing. This handles cases where types were already fully qualified
                // during class resolution (e.g., Modelica.Media.Interfaces.PartialMedium.BaseProperties)
                for (fq_original, fq_new) in &fq_package_replacements {
                    if original_type.starts_with(fq_original) {
                        // Replace the prefix with the new package
                        let suffix = &original_type[fq_original.len()..];
                        resolved_type = format!("{}{}", fq_new, suffix);
                        break;
                    }
                }

                // If no fully-qualified replacement matched, try the standard alias resolution
                if resolved_type == original_type
                    && let Some(alias_resolved) = resolve_class_name_with_imports(
                        &original_type,
                        &resolved_type_name,
                        self.class_dict,
                        &augmented_aliases,
                    )
                {
                    resolved_type = alias_resolved;
                }

                // Only update if the resolved type is different and not primitive
                if resolved_type != original_type && !is_primitive_type(&resolved_type) {
                    // Convert the resolved type string to a Name struct
                    scomp.type_name = ir::ast::Name {
                        name: resolved_type
                            .split('.')
                            .map(|part| ir::ast::Token {
                                text: part.to_string(),
                                ..Default::default()
                            })
                            .collect(),
                    };
                }
            }

            // Propagate causality from parent component to subcomponents.
            // For example, if `u` is a ComplexInput, then `u.re` and `u.im`
            // should also be inputs. This is critical for balance checking.
            if matches!(scomp.causality, ir::ast::Causality::Empty) {
                scomp.causality = comp.causality.clone();
            }

            // Propagate variability from parent component to subcomponents.
            // For example, if `k` is a `parameter Complex`, then `k.re` and `k.im`
            // should also be parameters. This is critical for balance checking.
            if matches!(scomp.variability, ir::ast::Variability::Empty) {
                scomp.variability = comp.variability.clone();
            }

            // Propagate connection (flow/stream) from parent component to subcomponents.
            // For example, if `i` is a `flow Complex`, then `i.re` and `i.im`
            // should also be flow variables. This is critical for balance checking.
            if matches!(scomp.connection, ir::ast::Connection::Empty) {
                scomp.connection = comp.connection.clone();
            }

            // Propagate array shape from parent component to subcomponents.
            // For example, if `u[3]` is expanded, then `u.re` and `u.im` should
            // both have shape [3]. This is critical for balance checking.
            // Note: shape_expr propagation is done AFTER the renamer runs, because
            // the parent's shape_expr references variables in the parent scope
            // (which don't need renaming), not internal component references.
            if !comp.shape.is_empty() && scomp.shape.is_empty() {
                scomp.shape = comp.shape.clone();
            }

            // Propagate condition from parent component to subcomponents.
            // If the parent has a condition (e.g., `block if useConstant`), all subcomponents
            // should inherit that condition (e.g., `block.y if useConstant`).
            // If both parent and child have conditions, combine them with AND.
            if let Some(ref parent_cond) = comp.condition {
                scomp.condition = match scomp.condition.take() {
                    None => Some(parent_cond.clone()),
                    Some(child_cond) => {
                        // Combine parent and child conditions with AND
                        Some(ir::ast::Expression::Binary {
                            op: ir::ast::OpBinary::And(ir::ast::Token::default()),
                            lhs: Box::new(parent_cond.clone()),
                            rhs: Box::new(child_cond),
                        })
                    }
                };
            }

            // If this is an inner component, register it
            if subcomp.inner {
                let key = (subcomp.type_name.to_string(), subcomp_name.clone());
                self.inner_map.insert(key, name.clone());
            }

            // Propagate hierarchical modifications from parent to subcomponent.
            // For example, if parent `o` has modification `sub.flag = true`, and we're
            // expanding subcomponent `sub`, add `flag = true` to sub's modifications.
            // This allows dot-notation modifications like `o(sub.flag = true)` to work.
            let prefix = format!("{}.", subcomp_name);
            for (mod_key, mod_expr) in &comp.modifications {
                if let Some(rest) = mod_key.strip_prefix(&prefix) {
                    // Found a hierarchical modification targeting this subcomponent
                    scomp
                        .modifications
                        .insert(rest.to_string(), mod_expr.clone());
                }
            }

            // Apply modifications from parent component
            // For simple literals or evaluable expressions, use as start value
            // For complex expressions, generate binding equations
            if let Some(mod_expr) = comp.modifications.get(subcomp_name) {
                if is_simple_literal(mod_expr) {
                    // Direct literal - use as start value
                    scomp.start = mod_expr.clone();
                    scomp.start_is_modification = true;
                } else if let Some(evaluated) =
                    try_evaluate_modification(mod_expr, &self.fclass.components)
                {
                    // Expression evaluated to a literal - use as start value
                    scomp.start = evaluated;
                    scomp.start_is_modification = true;
                } else {
                    // Complex expression - generate binding equation
                    // The binding equation references parent scope variables (not renamed)
                    let binding_eq = make_binding_eq(&name, mod_expr.clone());
                    // Parameter and constant bindings go to initial equations (computed once at init)
                    // Other bindings go to regular equations
                    if matches!(
                        subcomp.variability,
                        ir::ast::Variability::Parameter(_) | ir::ast::Variability::Constant(_)
                    ) {
                        self.fclass.initial_equations.push(binding_eq);
                    } else {
                        self.fclass.equations.push(binding_eq);
                    }
                    // Clear the original start value so it doesn't become a duplicate
                    // binding equation in extract_binding_equations. The modification
                    // provides the defining equation instead.
                    scomp.start = Expression::Empty;
                }
            }

            // Apply scope renaming to the component's start expression
            // This prefixes internal references like `x_start` to `comp.x_start`
            scomp.start.accept_mut(&mut renamer);

            // Apply scope renaming to the component's modifications
            // This prefixes internal references like `unitTime/Ti` to `comp.unitTime/comp.Ti`
            for mod_expr in scomp.modifications.values_mut() {
                mod_expr.accept_mut(&mut renamer);
            }

            // Apply scope renaming to shape expressions (for subcomponent's own dimensions)
            // This prefixes internal references like `na` to `comp.na`
            for sub in &mut scomp.shape_expr {
                if let ir::ast::Subscript::Expression(expr) = sub {
                    expr.accept_mut(&mut renamer);
                }
            }

            // Now propagate shape_expr from parent component AFTER renaming.
            // The parent's shape_expr references variables in the parent scope,
            // which are already correctly scoped and don't need renaming.
            // For example, if `u[m]` where `m` is a parameter, the subcomponents
            // `u.re` and `u.im` should also have shape_expr=[m], not [u.m].
            if !comp.shape_expr.is_empty() && scomp.shape_expr.is_empty() {
                scomp.shape_expr = comp.shape_expr.clone();
            }

            // For parameters with non-simple start expressions (binding equations like
            // `zeroGain = abs(k) < eps`), generate an initial equation if no parent
            // modification was applied. The start expression has already been scope-renamed.
            let has_parent_mod = comp.modifications.contains_key(subcomp_name);
            if !has_parent_mod
                && matches!(
                    scomp.variability,
                    ir::ast::Variability::Parameter(_) | ir::ast::Variability::Constant(_)
                )
                && !is_simple_literal(&scomp.start)
                && !matches!(scomp.start, Expression::Empty)
            {
                let binding_eq = make_binding_eq(&name, scomp.start.clone());
                self.fclass.initial_equations.push(binding_eq);
                // Clear the start expression since it's now an initial equation
                scomp.start = Expression::Empty;
            }

            subcomponents.push((name, scomp));
        }

        // Insert all subcomponents
        for (name, scomp) in &subcomponents {
            self.fclass.components.insert(name.clone(), scomp.clone());
        }

        // Generate binding equation for the parent component if it has a binding expression.
        // For example, if `Complex u1Internal = expr`, generate `u1Internal = expr`.
        // This equation will later be expanded by operator_expand to:
        //   u1Internal.re = expr.re
        //   u1Internal.im = expr.im
        // This only applies when the parent has a binding (not a modification/start=).
        if !comp.start_is_modification
            && !matches!(comp.start, ir::ast::Expression::Empty)
            && !is_simple_literal(&comp.start)
        {
            // Don't generate binding equations for parameters/constants - they use initial equations
            // Don't generate for inputs - they don't need equations
            if !matches!(
                comp.variability,
                ir::ast::Variability::Parameter(_) | ir::ast::Variability::Constant(_)
            ) && !matches!(comp.causality, ir::ast::Causality::Input(..))
            {
                let binding_eq = make_binding_eq(comp_name, comp.start.clone());
                self.fclass.equations.push(binding_eq);
            }
        }

        // Remove the parent component from flat class (it's been expanded into subcomponents)
        self.fclass.components.swap_remove(comp_name);

        // Recursively expand any subcomponents that are also class types.
        // Use the augmented_aliases (which include package modifications) to check
        // if subcomponent types can be resolved.
        for (subcomp_name, subcomp) in &subcomponents {
            // Use resolved_type_name as context for resolving nested component types
            if resolve_class_name_with_imports(
                &subcomp.type_name.to_string(),
                &resolved_type_name,
                self.class_dict,
                &augmented_aliases,
            )
            .is_some()
            {
                self.expand_component(subcomp_name, subcomp, &resolved_type_name)?;
            }
        }

        Ok(())
    }
}
