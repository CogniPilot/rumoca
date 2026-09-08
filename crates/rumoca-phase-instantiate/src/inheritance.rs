//! Inheritance processing for the instantiate phase (MLS §7.1).
//!
//! This module handles the `extends` clause processing, merging inherited
//! components and equations into the derived class.
//!
//! MLS support status is owned by the `rumoca-contracts` registry rather than
//! duplicated in implementation comments.

use crate::path_utils;
use indexmap::IndexSet;
use rumoca_core::{ComponentPath, DefId, Span};
use rumoca_core::{SourceMap, is_builtin_type};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

#[cfg(test)]
use rumoca_ir_ast::{
    classes_are_semantically_compatible as classes_are_compatible,
    components_are_semantically_compatible as components_are_compatible,
};

mod duplicate_identity;
mod redeclaration;

use crate::errors::{InstantiateError, InstantiateResult};
use crate::traversal_adapter::{
    expression_contains_redeclare, redeclare_target_value, walk_extend_modifications,
    walk_nested_classes,
};
use crate::type_overrides::{find_nested_class_by_def_id_in_hierarchy, resolve_cref_def_id};
use duplicate_identity::{
    inherited_components_are_identical, merged_declared_names, merged_element_names,
};
use redeclaration::*;

/// Cache for inheritance results to avoid recomputation.
///
/// This is particularly important for diamond inheritance patterns where
/// a base class may be inherited through multiple paths.
///
/// The cache avoids repeating inheritance traversal in diamond patterns. Its
/// public API still returns owned content, so a cache hit clones that content;
/// the pending effective-specialization graph owns eliminating that clone.
pub type InheritanceCache = IndexMap<DefId, Arc<InheritedContent>>;

/// Cache for subtype check results to avoid recomputation.
///
/// Maps exact resolved (subtype, supertype) declaration identities to the
/// result of the subtype check. Unresolved compatibility paths are deliberately
/// not cached: rendered class names are not semantic identity (SPEC_0001).
pub type SubtypeCache = IndexMap<(DefId, DefId), bool>;

/// Return the predefined type named by an extends edge, using resolved
/// identity rather than source spelling. A user class may legally carry the
/// same leaf spelling in a nested scope, so spelling alone cannot terminate a
/// semantic graph walk.
pub(crate) fn predefined_extend_name(
    tree: &ast::ClassTree,
    extend: &ast::Extend,
) -> InstantiateResult<Option<String>> {
    let base_name = extend.base_name.to_string();
    let base_def_id = extend
        .base_def_id
        .or(extend.base_name.def_id)
        .ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("extends edge `{base_name}`"),
                extend.location.span(),
            ))
        })?;
    Ok(rumoca_core::BUILTIN_TYPES.iter().find_map(|name| {
        (tree
            .scope_tree
            .predefined_member(&ComponentPath::from_flat_path(name))
            == Some(base_def_id))
        .then(|| (*name).to_string())
    }))
}

fn required_class_identity(class: &ast::ClassDef, context: &str) -> InstantiateResult<DefId> {
    class.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("{context} `{}`", class.name.text),
            class.location.span(),
        ))
    })
}

/// Result of processing inheritance for a class.
#[derive(Debug, Clone, Default)]
pub struct InheritedContent {
    /// Components inherited from all base classes.
    pub components: IndexMap<String, ast::Component>,
    /// Equations inherited from all base classes.
    pub equations: Vec<ast::Equation>,
    /// Initial equations inherited from all base classes.
    pub initial_equations: Vec<ast::Equation>,
    /// Algorithm sections inherited from all base classes.
    pub algorithms: Vec<Vec<ast::Statement>>,
    /// Initial algorithm sections inherited from all base classes.
    pub initial_algorithms: Vec<Vec<ast::Statement>>,
    /// Nested classes inherited from all base classes.
    pub classes: IndexMap<String, ast::ClassDef>,
}

/// Apply protected visibility to a component if the extend is protected.
///
/// MLS §7.1.2: Protected extends makes inherited elements protected.
fn apply_protected_visibility(comp: &mut ast::Component, is_protected: bool) {
    if is_protected {
        comp.is_protected = true;
    }
}

/// Apply protected visibility to a class if the extend is protected.
///
/// MLS §7.1.2: Protected extends makes inherited elements protected.
fn apply_protected_class_visibility(class: &mut ast::ClassDef, is_protected: bool) {
    if is_protected {
        class.is_protected = true;
    }
}

#[derive(Clone)]
struct RedeclaredType {
    source_name: String,
    def_id: DefId,
}

/// Extract the replacement type without converting Resolve's identity proof
/// back into source spelling.
fn extract_redeclared_type(
    expr: &ast::Expression,
    span: Span,
) -> InstantiateResult<RedeclaredType> {
    let reference = match expr {
        ast::Expression::Modification {
            value: Some(value), ..
        }
        | ast::Expression::NamedArgument { value, .. } => match value.as_ref() {
            ast::Expression::ComponentReference(reference) => Some(reference),
            ast::Expression::ClassModification { target, .. } => Some(target),
            _ => None,
        },
        ast::Expression::ClassModification { target, .. } => Some(target),
        _ => None,
    }
    .ok_or_else(|| {
        Box::new(InstantiateError::redeclare_error(
            "<unknown>",
            "redeclare replacement has no exact type reference",
            span,
        ))
    })?;
    let def_id = redeclare_reference_target(reference).ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("redeclare replacement `{reference}`"),
            span,
        ))
    })?;
    Ok(RedeclaredType {
        source_name: reference.to_string(),
        def_id,
    })
}

/// Validate a redeclaration against the base class component.
///
/// MLS §7.3: Redeclarations are only valid for replaceable elements.
/// MLS §7.2.6: Final elements cannot be redeclared.
/// MLS §7.3.2: Redeclared type must satisfy constrainedby.
///
/// # Arguments
/// * `tree` - The class tree for type compatibility checking
/// * `component` - The base class component being redeclared
/// * `target_name` - Name of the component being redeclared
/// * `new_type` - The new type being redeclared to (if known)
/// * `span` - Source location for error reporting
fn validate_redeclaration(
    tree: &ast::ClassTree,
    component: &ast::Component,
    target_name: &str,
    new_type: Option<&RedeclaredType>,
    span: Span,
) -> InstantiateResult<()> {
    // MLS §7.3.3: constants cannot be redeclared.
    if matches!(component.variability, rumoca_core::Variability::Constant(_)) {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "constant elements cannot be redeclared",
            span,
        )));
    }

    // MLS §7.2.6: Check if component is final
    if component.is_final {
        return Err(Box::new(InstantiateError::redeclare_final(
            target_name,
            span,
        )));
    }

    // MLS §7.3: Check if component is replaceable
    if !component.is_replaceable {
        return Err(Box::new(InstantiateError::redeclare_non_replaceable(
            target_name,
            span,
        )));
    }

    // MLS §7.3.2: Validate constrainedby
    // The redeclared type must be a subtype of the constraining type.
    // If no constrainedby is specified, the original type is the constraint.
    if let Some(new_type) = new_type {
        let (constraint_def_id, constraint_source_name) =
            if let Some(constraint) = component.constrainedby.as_ref() {
                let def_id = constraint.def_id.ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("constraining type `{constraint}` for `{target_name}`"),
                        span,
                    ))
                })?;
                (def_id, constraint.to_string())
            } else {
                let def_id = component
                    .type_def_id
                    .or(component.type_name.def_id)
                    .ok_or_else(|| {
                        Box::new(InstantiateError::missing_resolved_identity(
                            format!("default constraining type for `{target_name}`"),
                            span,
                        ))
                    })?;
                (def_id, component.type_name.to_string())
            };
        let mut cache = SubtypeCache::default();
        if !is_type_subtype_by_def_id(tree, new_type.def_id, constraint_def_id, &mut cache)? {
            let replacement_name = tree
                .def_map
                .get(&new_type.def_id)
                .cloned()
                .unwrap_or_else(|| new_type.source_name.clone());
            let constraint_name = tree
                .def_map
                .get(&constraint_def_id)
                .cloned()
                .unwrap_or(constraint_source_name);
            return Err(Box::new(InstantiateError::redeclare_constraint_violation(
                target_name,
                &replacement_name,
                &constraint_name,
                span,
            )));
        }
    }

    Ok(())
}

/// Validate one occurrence-local component type selection proved from a
/// resolved component redeclare modifier.
pub(super) fn validate_component_redeclaration_selection(
    tree: &ast::ClassTree,
    component: &ast::Component,
    target_name: &str,
    replacement_def_id: DefId,
    span: Span,
) -> InstantiateResult<()> {
    let source_name = tree
        .def_map
        .get(&replacement_def_id)
        .cloned()
        .unwrap_or_else(|| format!("{replacement_def_id:?}"));
    validate_redeclaration(
        tree,
        component,
        target_name,
        Some(&RedeclaredType {
            source_name,
            def_id: replacement_def_id,
        }),
        span,
    )
}

/// Validate a redeclared nested class/package target.
///
/// MLS §7.3: only replaceable classes may be redeclared.
/// MLS §7.2.6: final classes may not be redeclared.
/// MLS §7.3.2: class redeclarations must satisfy constrainedby.
fn validate_class_redeclaration(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    target_name: &str,
    new_type: Option<&RedeclaredType>,
    span: Span,
) -> InstantiateResult<()> {
    if class.is_final {
        return Err(Box::new(InstantiateError::redeclare_final(
            target_name,
            span,
        )));
    }

    if !class.is_replaceable {
        return Err(Box::new(InstantiateError::redeclare_non_replaceable(
            target_name,
            span,
        )));
    }

    if let Some(new_type) = new_type {
        // MLS §7.3.2 default constraint for class/package redeclare: if
        // constrainedby is omitted, use the original declared base.
        let (constraint_def_id, constraint_source_name) =
            if let Some(constraint) = class.constrainedby.as_ref() {
                let def_id = constraint.def_id.ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("constraining type `{constraint}` for `{target_name}`"),
                        span,
                    ))
                })?;
                (def_id, constraint.to_string())
            } else if let Some(extend) = class.extends.first() {
                let def_id = extend
                    .base_def_id
                    .or(extend.base_name.def_id)
                    .ok_or_else(|| {
                        Box::new(InstantiateError::missing_resolved_identity(
                            format!("default constraining extends edge for `{target_name}`"),
                            span,
                        ))
                    })?;
                (def_id, extend.base_name.to_string())
            } else {
                let def_id = class.def_id.ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("default constraining class `{target_name}`"),
                        span,
                    ))
                })?;
                (def_id, class.name.text.to_string())
            };
        let mut cache = SubtypeCache::default();
        if !is_type_subtype_by_def_id(tree, new_type.def_id, constraint_def_id, &mut cache)? {
            let replacement_name = tree
                .def_map
                .get(&new_type.def_id)
                .cloned()
                .unwrap_or_else(|| new_type.source_name.clone());
            let constraint_name = tree
                .def_map
                .get(&constraint_def_id)
                .cloned()
                .unwrap_or(constraint_source_name);
            return Err(Box::new(InstantiateError::redeclare_constraint_violation(
                target_name,
                &replacement_name,
                &constraint_name,
                span,
            )));
        }
    }

    Ok(())
}

fn redeclare_target_span(
    tree: &ast::ClassTree,
    target_name: &str,
    target: &ast::ComponentReference,
    extend_span: Span,
) -> InstantiateResult<Span> {
    let Some(part) = target.parts.first() else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "redeclare target is missing source span",
            extend_span,
        )));
    };

    location_to_span(
        &part.ident.location,
        &tree.source_map,
        "extends redeclare target name",
    )
}

/// Check if `subtype` is a subtype of `supertype`.
///
/// MLS §7.3.2: A type is a subtype if it's the same type or extends the supertype.
/// MLS §5.4: Also used for inner/outer type compatibility checking.
///
/// This is a simplified check that handles:
/// 1. Exact type match
/// 2. Built-in type matching (Real, Integer, Boolean, String)
/// 3. Class inheritance via extends
///
/// For performance-critical code with deeply nested inheritance, use
/// `is_type_subtype_cached` instead.
pub fn is_type_subtype(
    tree: &ast::ClassTree,
    subtype: &str,
    supertype: &str,
) -> InstantiateResult<bool> {
    let mut cache = SubtypeCache::default();
    is_type_subtype_cached(tree, subtype, supertype, &mut cache)
}

/// Check if `subtype` is a subtype of `supertype` with caching.
///
/// This cached version avoids recomputation for deeply nested inheritance
/// hierarchies. The cache maps resolved (subtype, supertype) declaration pairs
/// to their results.
///
/// For replaceable component redeclarations, this function also considers
/// "sibling types" as compatible. Two types A and B are siblings if they
/// both directly extend the same base class. This supports common MSL patterns
/// where CellStack and CellRCStack both extend BaseCellStack.
pub fn is_type_subtype_cached(
    tree: &ast::ClassTree,
    subtype: &str,
    supertype: &str,
    cache: &mut SubtypeCache,
) -> InstantiateResult<bool> {
    let subtype_class = resolved_type_class(tree, subtype)?;
    let supertype_class = resolved_type_class(tree, supertype)?;
    match (subtype_class, supertype_class) {
        (None, None) => return Ok(subtype == supertype),
        (None, Some(_)) => return Ok(false),
        (Some(subtype_class), None) => {
            return class_extends_builtin(tree, subtype_class, supertype);
        }
        (Some(_), Some(_)) => {}
    }
    let subtype_class = subtype_class.expect("builtin case returned above");
    let supertype_class = supertype_class.expect("builtin case returned above");
    let subtype_def_id = required_class_identity(subtype_class, "subtype")?;
    let supertype_def_id = required_class_identity(supertype_class, "supertype")?;
    if let Some(&result) = cache.get(&(subtype_def_id, supertype_def_id)) {
        return Ok(result);
    }

    if subtype_def_id == supertype_def_id {
        cache.insert((subtype_def_id, supertype_def_id), true);
        return Ok(true);
    }

    let accepted = if class_extends_def_id(tree, subtype_class, supertype_def_id)? {
        true
    } else {
        // Check for sibling types: both extend the same base class.
        // Siblinghood alone does not make the interfaces compatible, so the
        // MLS §6.5 member comparator must also pass.
        types_share_common_base(tree, subtype_class, supertype_class)?
            && crate::plug_compat::members_plug_compatible(tree, subtype_class, supertype_class)?
    };
    let result = accepted
        && crate::plug_compat::class_flags_compatible(tree, subtype_class, Some(supertype_class))?;

    cache.insert((subtype_def_id, supertype_def_id), result);
    Ok(result)
}

fn resolved_type_class<'a>(
    tree: &'a ast::ClassTree,
    type_name: &str,
) -> InstantiateResult<Option<&'a ast::ClassDef>> {
    if is_builtin_type(type_name) {
        return Ok(None);
    }
    let class = find_class_in_tree(tree, type_name)
        .ok_or_else(|| Box::new(InstantiateError::ModelNotFound(type_name.to_string())))?;
    required_class_identity(class, "resolved type")?;
    Ok(Some(class))
}

pub(crate) fn is_type_subtype_by_def_id(
    tree: &ast::ClassTree,
    subtype_def_id: DefId,
    supertype_def_id: DefId,
    cache: &mut SubtypeCache,
) -> InstantiateResult<bool> {
    let is_predefined = |def_id| {
        rumoca_core::BUILTIN_TYPES.iter().any(|name| {
            tree.scope_tree
                .predefined_member(&ComponentPath::from_flat_path(name))
                == Some(def_id)
        })
    };
    let subtype_predefined = is_predefined(subtype_def_id);
    let supertype_predefined = is_predefined(supertype_def_id);
    let subtype_class = tree.get_class_by_def_id(subtype_def_id);
    let supertype_class = tree.get_class_by_def_id(supertype_def_id);
    if !subtype_predefined && subtype_class.is_none() {
        return Err(Box::new(InstantiateError::ModelNotFound(format!(
            "resolved subtype {subtype_def_id:?}"
        ))));
    }
    if !supertype_predefined && supertype_class.is_none() {
        return Err(Box::new(InstantiateError::ModelNotFound(format!(
            "resolved supertype {supertype_def_id:?}"
        ))));
    }
    if subtype_predefined {
        return Ok(supertype_predefined && subtype_def_id == supertype_def_id);
    }
    if supertype_predefined {
        let subtype_class = subtype_class.ok_or_else(|| {
            Box::new(InstantiateError::ModelNotFound(format!(
                "resolved subtype {subtype_def_id:?}"
            )))
        })?;
        let supertype_name = rumoca_core::BUILTIN_TYPES
            .iter()
            .find(|name| {
                tree.scope_tree
                    .predefined_member(&ComponentPath::from_flat_path(name))
                    == Some(supertype_def_id)
            })
            .expect("predefined identity was established above");
        let result = class_extends_builtin(tree, subtype_class, supertype_name)?;
        cache.insert((subtype_def_id, supertype_def_id), result);
        return Ok(result);
    }
    let subtype_class = subtype_class.ok_or_else(|| {
        Box::new(InstantiateError::ModelNotFound(format!(
            "resolved subtype {subtype_def_id:?}"
        )))
    })?;
    let supertype_class = supertype_class.ok_or_else(|| {
        Box::new(InstantiateError::ModelNotFound(format!(
            "resolved supertype {supertype_def_id:?}"
        )))
    })?;
    if subtype_def_id == supertype_def_id {
        return Ok(true);
    }
    if let Some(result) = cache.get(&(subtype_def_id, supertype_def_id)) {
        return Ok(*result);
    }
    let accepted = if class_extends_def_id(tree, subtype_class, supertype_def_id)? {
        true
    } else {
        types_share_common_base(tree, subtype_class, supertype_class)?
            && crate::plug_compat::members_plug_compatible(tree, subtype_class, supertype_class)?
    };
    let result = accepted
        && crate::plug_compat::class_flags_compatible(tree, subtype_class, Some(supertype_class))?;
    cache.insert((subtype_def_id, supertype_def_id), result);
    Ok(result)
}

/// Check if two types share a common direct base class.
///
/// This is used for replaceable component redeclarations where sibling types
/// (both extending the same base) should be considered compatible per MLS §6.4's
/// interface compatibility requirements.
fn types_share_common_base(
    tree: &ast::ClassTree,
    type_a: &ast::ClassDef,
    type_b: &ast::ClassDef,
) -> InstantiateResult<bool> {
    let mut type_b_ids = IndexSet::new();
    let mut type_b_builtins = IndexSet::new();
    let mut pending = vec![type_b];
    while let Some(class) = pending.pop() {
        let class_def_id = required_class_identity(class, "sibling type")?;
        if !type_b_ids.insert(class_def_id) {
            continue;
        }
        for extend in &class.extends {
            if let Some(predefined) = predefined_extend_name(tree, extend)? {
                type_b_builtins.insert(predefined);
                continue;
            }
            let base_name = extend.base_name.to_string();
            let base_def_id = extend
                .base_def_id
                .or(extend.base_name.def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("sibling base `{base_name}`"),
                        extend.location.span(),
                    ))
                })?;
            let base = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("sibling base `{base_name}` ({base_def_id:?})"),
                    extend.location.span(),
                ))
            })?;
            pending.push(base);
        }
    }
    for extend_a in &type_a.extends {
        let base_a_name = extend_a.base_name.to_string();
        if let Some(predefined_name) = predefined_extend_name(tree, extend_a)? {
            if type_b_builtins.contains(&predefined_name) {
                return Ok(true);
            }
            continue;
        }
        let base_a_def_id = extend_a
            .base_def_id
            .or(extend_a.base_name.def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("sibling base `{base_a_name}`"),
                    extend_a.location.span(),
                ))
            })?;
        if type_b_ids.contains(&base_a_def_id) {
            return Ok(true);
        }
    }

    Ok(false)
}

/// Check if a class transitively extends a built-in type (Real, Integer, Boolean, String).
///
/// Built-in types are not stored in the class tree, so the checked class graph
/// records their terminal edges separately from class declaration identities.
///
/// This handles type alias chains like:
/// ```modelica
/// type Resistance = Real(final quantity="ElectricalResistance", final unit="Ohm");
/// ```
fn class_extends_builtin(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    builtin: &str,
) -> InstantiateResult<bool> {
    let expected_def_id = tree
        .scope_tree
        .predefined_member(&ComponentPath::from_flat_path(builtin));
    let mut pending = vec![class];
    let mut visited = IndexSet::new();
    while let Some(owner) = pending.pop() {
        let owner_def_id = required_class_identity(owner, "builtin subtype")?;
        if !visited.insert(owner_def_id) {
            continue;
        }
        for extend in &owner.extends {
            let base_def_id = extend
                .base_def_id
                .or(extend.base_name.def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("builtin extends edge `{}`", extend.base_name),
                        extend.location.span(),
                    ))
                })?;
            match (
                predefined_extend_name(tree, extend)?,
                Some(base_def_id) == expected_def_id,
            ) {
                (Some(_), true) => return Ok(true),
                (Some(_), false) => continue,
                (None, _) => {}
            }
            let base = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!(
                        "builtin extends edge `{}` ({base_def_id:?})",
                        extend.base_name
                    ),
                    extend.location.span(),
                ))
            })?;
            pending.push(base);
        }
    }
    Ok(false)
}

/// Find a class by resolved name in the tree (top-level or nested).
///
/// Uses O(1) lookup via the name_map (populated during resolve phase).
/// For nested classes, use the qualified name (e.g., "Package.Inner").
///
/// # Panics
/// Debug builds panic if name_map is empty (indicates resolve phase wasn't run).
pub fn find_class_in_tree<'a>(tree: &'a ast::ClassTree, name: &str) -> Option<&'a ast::ClassDef> {
    // O(1) lookup via name_map (populated during resolve phase)
    if let Some(&def_id) = tree.name_map.get(name) {
        return tree.get_class_by_def_id(def_id);
    }

    // Also check top-level classes directly (handles cases where name_map
    // uses qualified names but caller uses short names for top-level classes)
    if let Some(class) = tree.definitions.classes.get(name) {
        return Some(class);
    }

    None
}

fn redeclare_reference_target(reference: &ast::ComponentReference) -> Option<DefId> {
    if reference.parts.len() > 1 {
        reference.target_def_id()
    } else {
        reference.root_def_id()
    }
}

/// Check if a class is effectively primitive (a short class definition extending a primitive type).
///
/// Short class definitions like `connector BooleanInput = input Boolean;` are syntactic sugar
/// for `connector BooleanInput extends Boolean; end BooleanInput;` with causality.
/// Such classes should be treated as primitive for variable creation purposes.
///
/// Components using such types become flat variables with the type's causality
/// applied.
///
/// Check if a type is effectively primitive, resolving type alias chains transitively.
///
/// This handles cases like:
/// ```modelica
/// type SpecificHeatCapacity = Real(...);
/// type SpecificHeatCapacityAtConstantPressure = SpecificHeatCapacity;
/// ```
///
/// Where `SpecificHeatCapacityAtConstantPressure` should be considered primitive
/// because it ultimately resolves to `Real`.
///
/// MLS §4.6: Type classes (short class definitions) create type aliases.
pub(crate) fn is_effectively_primitive_transitive(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<bool> {
    // A class is effectively primitive if it:
    // 1. Has no components (not a container)
    // 2. Has no equations (not a model with behavior)
    // 3. Either:
    //    a. Has exactly one extends clause that transitively leads to a built-in type
    //    b. Is an enumeration type (has enum_literals)
    if !class.components.is_empty() {
        return Ok(false);
    }
    if !class.equations.is_empty() || !class.initial_equations.is_empty() {
        return Ok(false);
    }

    if !class.enum_literals.is_empty() {
        return Ok(true);
    }

    // Check for extends to a type that is primitive (built-in or transitively primitive)
    if class.extends.len() != 1 {
        return Ok(false);
    }
    let mut current = class;
    let mut visited = IndexSet::new();
    loop {
        let current_def_id = required_class_identity(current, "primitive type")?;
        if !visited.insert(current_def_id) {
            return Ok(false);
        }
        let extend = &current.extends[0];
        if let Some(predefined) = predefined_extend_name(tree, extend)? {
            return Ok(predefined != "ExternalObject");
        }
        let base_def_id = extend
            .base_def_id
            .or(extend.base_name.def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("primitive extends edge `{}`", extend.base_name),
                    extend.location.span(),
                ))
            })?;
        let bc = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!(
                    "primitive extends edge `{}` ({base_def_id:?})",
                    extend.base_name
                ),
                extend.location.span(),
            ))
        })?;
        // If this class has components or equations, not primitive
        if !bc.components.is_empty() || !bc.equations.is_empty() || !bc.initial_equations.is_empty()
        {
            return Ok(false);
        }
        // If this is an enumeration type, it's primitive
        if !bc.enum_literals.is_empty() {
            return Ok(true);
        }
        // If it extends exactly one thing, follow the chain
        if bc.extends.len() != 1 {
            return Ok(false);
        }
        current = bc;
    }
}

/// Check if a type is discrete-valued by its base type (MLS §3.8.3).
///
/// This function resolves type alias chains to determine if the base type is
/// Integer, Boolean, String, or an enumeration, all of which are discrete-time
/// even without an explicit `discrete` variability prefix.
///
/// MLS §3.8.3: a variable is discrete-time when it is discrete-valued, that is
/// when its base type is not `Real`. Only `Real` (and `Clock`, which carries
/// its own clocked semantics) is excluded here.
pub(crate) fn is_discrete_by_type(
    tree: &ast::ClassTree,
    type_name: &str,
    class_def: Option<&ast::ClassDef>,
) -> InstantiateResult<bool> {
    // Helper to check if a name is a discrete-valued predefined type
    fn is_discrete_builtin(name: &str) -> bool {
        let simple_name = path_utils::class_name_leaf(name);
        matches!(simple_name, "Integer" | "Boolean" | "String")
    }

    // Direct check on the type name
    if class_def.is_none() && is_discrete_builtin(type_name) {
        return Ok(true);
    }

    // If we have a class definition, check its inheritance chain
    let Some(class) = class_def else {
        return Ok(false);
    };

    // Enumerations are discrete values
    if !class.enum_literals.is_empty() {
        return Ok(true);
    }

    // If the class extends something, follow the chain
    if class.extends.len() == 1 {
        let extend = &class.extends[0];
        if let Some(predefined) = predefined_extend_name(tree, extend)? {
            return Ok(is_discrete_builtin(&predefined));
        }

        let base_def_id = extend
            .base_def_id
            .or(extend.base_name.def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("discrete-type extends edge `{}`", extend.base_name),
                    extend.location.span(),
                ))
            })?;
        let mut current_class = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!(
                    "discrete-type extends edge `{}` ({base_def_id:?})",
                    extend.base_name
                ),
                extend.location.span(),
            ))
        })?;
        let mut visited = IndexSet::new();

        loop {
            let bc = current_class;
            let bc_def_id = required_class_identity(bc, "discrete type")?;
            if !visited.insert(bc_def_id) {
                return Ok(false);
            }

            // Enumerations are discrete
            if !bc.enum_literals.is_empty() {
                return Ok(true);
            }

            // Follow the chain if there's exactly one extends
            if bc.extends.len() != 1 {
                return Ok(false);
            }
            let next_extend = &bc.extends[0];
            if let Some(predefined) = predefined_extend_name(tree, next_extend)? {
                return Ok(is_discrete_builtin(&predefined));
            }
            let next_def_id = next_extend
                .base_def_id
                .or(next_extend.base_name.def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("discrete-type extends edge `{}`", next_extend.base_name),
                        next_extend.location.span(),
                    ))
                })?;
            current_class = tree.get_class_by_def_id(next_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!(
                        "discrete-type extends edge `{}` ({next_def_id:?})",
                        next_extend.base_name
                    ),
                    next_extend.location.span(),
                ))
            })?;
        }
    }

    Ok(false)
}

/// Check if a class extends a base class (by name) directly or transitively.
///
/// MLS §7.1: A class that extends another inherits all its contents.
/// This creates a subtype relationship.
///
/// For performance-critical code with deeply nested inheritance, use
/// `class_extends_cached` instead.
pub fn class_extends(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    base_name: &str,
) -> InstantiateResult<bool> {
    let mut cache = SubtypeCache::default();
    class_extends_cached(tree, class, base_name, &mut cache)
}

/// Check if a class extends a base class (by resolved name) directly or transitively, with caching.
pub fn class_extends_cached(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    base_name: &str,
    cache: &mut SubtypeCache,
) -> InstantiateResult<bool> {
    if is_builtin_type(base_name) {
        return class_extends_builtin(tree, class, base_name);
    }
    let target =
        resolved_type_class(tree, base_name)?.expect("non-builtin resolved type must be a class");
    let target_def_id = required_class_identity(target, "extends target")?;
    let class_def_id = required_class_identity(class, "extends source")?;
    if let Some(result) = cache.get(&(class_def_id, target_def_id)) {
        return Ok(*result);
    }
    let result = class_extends_def_id(tree, class, target_def_id)?;
    cache.insert((class_def_id, target_def_id), result);
    Ok(result)
}

fn class_extends_def_id(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    target_def_id: DefId,
) -> InstantiateResult<bool> {
    let mut pending = vec![class];
    let mut visited = IndexSet::new();
    while let Some(owner) = pending.pop() {
        let owner_def_id = required_class_identity(owner, "extends source")?;
        if !visited.insert(owner_def_id) {
            continue;
        }
        for extend in &owner.extends {
            if predefined_extend_name(tree, extend)?.is_some() {
                continue;
            }
            let base_def_id = extend
                .base_def_id
                .or(extend.base_name.def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("extends edge `{}`", extend.base_name),
                        extend.location.span(),
                    ))
                })?;
            if base_def_id == target_def_id {
                return Ok(true);
            }
            let base = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("extends edge `{}` ({base_def_id:?})", extend.base_name),
                    extend.location.span(),
                ))
            })?;
            pending.push(base);
        }
    }
    Ok(false)
}

/// Process extends clauses and collect inherited content.
///
/// MLS §7.1: "The extends-clause results in including the contents of the
/// base class at the point of the extends-clause."
///
/// MLS §7.1: "The ordering of multiple extends-clauses defines the order
/// in which the base-class contents are merged."
///
/// The merge order per MLS is: first the base class's own content, then
/// recursively its base classes. This ensures shallow inheritance takes
/// precedence over deep inheritance.
///
/// This function creates a fresh cache for each call. For processing multiple
/// classes that share base classes, use `process_extends_with_cache` instead.
pub fn process_extends(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<InheritedContent> {
    let mut cache = InheritanceCache::default();
    process_extends_with_cache(tree, class, &mut cache)
}

/// Process extends clauses with caching to avoid recomputation.
///
/// This is the internal implementation that uses a cache to handle diamond
/// inheritance efficiently. The cache stores processed inheritance results
/// keyed by DefId.
///
/// ## Diamond Inheritance
///
/// Consider: D extends B, C; B extends A; C extends A;
/// Without caching, A's content would be processed twice.
/// With caching, A's content is computed once and reused.
pub fn process_extends_with_cache(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    cache: &mut InheritanceCache,
) -> InstantiateResult<InheritedContent> {
    // Check cache first (requires class to have a DefId)
    if let Some(def_id) = class.def_id
        && let Some(cached) = cache.get(&def_id)
    {
        // Cache hit: clone the inner InheritedContent
        // Note: We can't avoid cloning here because the cache keeps the Arc
        // and we need to return an owned InheritedContent for mutation
        return Ok((**cached).clone());
    }

    let mut inherited = InheritedContent::default();

    for extend in &class.extends {
        // Skip built-in types (Real, Integer, Boolean, String, ExternalObject)
        // They don't have components/equations to inherit, just type properties
        if predefined_extend_name(tree, extend)?.is_some() {
            continue;
        }

        // Look up the base class
        let base_class = resolve_base_class(tree, extend)?;

        // MLS §7.1: First merge the base class's own content
        merge_class_content(tree, &mut inherited, base_class, extend)?;

        // Then recursively process the base class's extends (with cache)
        let base_inherited = process_extends_with_cache(tree, base_class, cache)?;
        merge_inherited(&mut inherited, base_inherited, extend, &tree.source_map)?;

        // MLS §7.2: Apply extends modifications after recursive merge so
        // transitively inherited targets are available.
        apply_extends_modifications(tree, &mut inherited, base_class, extend)?;
    }

    // The public API returns owned content while the cache retains a shared
    // snapshot, so publication currently requires one deep clone. The pending
    // effective-specialization graph will replace this split ownership.
    if let Some(def_id) = class.def_id {
        cache.insert(def_id, Arc::new(inherited.clone()));
    }
    Ok(inherited)
}

/// Apply non-redeclare extends modifications to merged inherited components.
///
/// This post-merge pass ensures modifications like `extends Mid(c(k=2))` also
/// apply when `c` is declared in a grandparent class.
///
/// MLS §4.4.4 / §7.2: a value modification (`x = expr`) updates the *binding
/// equation* of the inherited component, not its `start` attribute. Conflating
/// the two corrupts attribute source-scope tracking and causes flatten to
/// qualify the start expression with the parent type's lexical scope instead
/// of the component instance prefix.
fn apply_extends_modifications(
    tree: &ast::ClassTree,
    target: &mut InheritedContent,
    base_class: &ast::ClassDef,
    extend: &ast::Extend,
) -> InstantiateResult<()> {
    let mut final_override: Option<String> = None;
    walk_extend_modifications(extend, |modification| {
        let Some((name, value, is_final)) =
            try_extract_value_modification_any(modification, extend)
        else {
            return;
        };
        if base_class.components.contains_key(&name) {
            return;
        }
        let Some(comp) = target.components.get_mut(&name) else {
            return;
        };
        if comp.is_final {
            final_override = Some(name);
            return;
        }
        comp.binding = Some(value);
        comp.has_explicit_binding = true;
        if is_final {
            comp.is_final = true;
        }
    });
    if let Some(name) = final_override {
        let extend_span = location_to_span(
            &extend.location,
            &tree.source_map,
            "extends modification final override",
        )?;
        return Err(Box::new(InstantiateError::redeclare_final(
            name,
            extend_span,
        )));
    }

    merge_nested_extends_modifications(target, extend);
    Ok(())
}

/// Resolve a base class from an extends clause.
///
/// Uses O(1) DefId lookup via ast::ClassTree.get_class_by_def_id().
/// Requires base_def_id to be set (done during resolve phase).
fn resolve_base_class<'a>(
    tree: &'a ast::ClassTree,
    extend: &ast::Extend,
) -> InstantiateResult<&'a ast::ClassDef> {
    let base_name = extend.base_name.to_string();
    let def_id = extend
        .base_def_id
        .ok_or_else(|| Box::new(InstantiateError::ModelNotFound(base_name.clone())))?;

    tree.get_class_by_def_id(def_id)
        .ok_or_else(|| Box::new(InstantiateError::ModelNotFound(base_name)))
}

/// Create a Span from a rumoca_core::Location using the source map for file resolution.
pub fn location_to_span(
    loc: &rumoca_core::Location,
    source_map: &SourceMap,
    context: &str,
) -> InstantiateResult<Span> {
    if !loc.has_source() {
        return Err(Box::new(InstantiateError::missing_source_context(format!(
            "{context} is missing a non-empty source location"
        ))));
    }
    source_map
        .try_span(loc.source, loc.start as usize, loc.end as usize)
        .ok_or_else(|| {
            let file_name = source_map
                .name(loc.source)
                .unwrap_or(UNKNOWN_SOURCE_DISPLAY_NAME);
            Box::new(InstantiateError::missing_source_context(format!(
                "source file `{file_name}` for {context} was not found"
            )))
        })
}

/// Placeholder used when a `SourceId` has no registered name in the source map.
pub(crate) const UNKNOWN_SOURCE_DISPLAY_NAME: &str = "<unknown source>";

/// Create a Span from an Option<rumoca_core::Location> using the source map.
pub(crate) fn required_location_to_span(
    loc: Option<&rumoca_core::Location>,
    source_map: &SourceMap,
    context: &str,
) -> InstantiateResult<Span> {
    let loc = loc.ok_or_else(|| {
        Box::new(InstantiateError::missing_source_context(format!(
            "{context} is missing source provenance"
        )))
    })?;
    location_to_span(loc, source_map, context)
}

fn nested_class_redeclaration_replaces_existing(
    existing: &ast::ClassDef,
    incoming: &ast::ClassDef,
) -> bool {
    if !existing.is_replaceable {
        return false;
    }

    existing.name.text == incoming.name.text && existing.class_type == incoming.class_type
}

fn nested_class_existing_redeclaration_shadows_inherited(
    existing: &ast::ClassDef,
    incoming: &ast::ClassDef,
) -> bool {
    incoming.is_replaceable
        && existing.name.text == incoming.name.text
        && existing.class_type == incoming.class_type
}

/// Merge inherited content from a base class.
fn merge_inherited(
    target: &mut InheritedContent,
    base: InheritedContent,
    extend: &ast::Extend,
    source_map: &SourceMap,
) -> InstantiateResult<()> {
    // MLS §5.6.1.4 collapses same-named elements from several bases into one,
    // so identity is decided on the merged class, not on each base in isolation.
    let merged = merged_element_names(target, &base);

    // Merge components, checking for conflicts
    for (name, comp) in base.components {
        // Check if this component is deselected via `break`
        if extend.break_names.contains(&name) {
            continue;
        }

        if let Some(existing) = target.components.get(&name) {
            // MLS §5.6: Check if components are from same origin or have compatible types
            if !inherited_components_are_identical(existing, &comp, &merged) {
                return Err(Box::new(InstantiateError::conflicting_inheritance(
                    name.clone(),
                    "previous base",
                    extend.base_name.to_string(),
                    location_to_span(
                        &extend.location,
                        source_map,
                        "conflicting inherited component extends clause",
                    )?,
                )));
            }
            // Compatible - diamond inheritance is OK, keep existing
        } else {
            let mut inherited_comp = comp;
            apply_protected_visibility(&mut inherited_comp, extend.is_protected);
            target.components.insert(name, inherited_comp);
        }
    }

    // Merge equations. MLS §7.1 / INST-025: equations syntactically
    // equivalent to already-inherited ones are discarded (diamond
    // inheritance of a common base must not duplicate its equations; the
    // duplicates are clones of the same source AST and compare equal).
    extend_without_duplicates(&mut target.equations, base.equations);
    extend_without_duplicates(&mut target.initial_equations, base.initial_equations);

    // Merge algorithms with the same syntactic-equivalence rule.
    extend_without_duplicates(&mut target.algorithms, base.algorithms);
    extend_without_duplicates(&mut target.initial_algorithms, base.initial_algorithms);

    // Merge nested classes
    for (name, class) in base.classes {
        match merge_inherited_nested_class(target, extend, source_map, name, class)? {
            NestedClassMerge::Inserted | NestedClassMerge::Skipped => {}
        }
    }

    Ok(())
}

enum NestedClassMerge {
    Inserted,
    Skipped,
}

/// Append `source` items to `target`, discarding items already present.
/// Inherited duplicates from diamond inheritance are clones of the same
/// source AST, so syntactic equivalence is plain equality here.
fn extend_without_duplicates<T: PartialEq>(target: &mut Vec<T>, source: Vec<T>) {
    for item in source {
        if !target.contains(&item) {
            target.push(item);
        }
    }
}

fn merge_inherited_nested_class(
    target: &mut InheritedContent,
    extend: &ast::Extend,
    source_map: &SourceMap,
    name: String,
    class: ast::ClassDef,
) -> InstantiateResult<NestedClassMerge> {
    let Some(existing) = target.classes.get(&name) else {
        let mut inherited_class = class;
        apply_protected_class_visibility(&mut inherited_class, extend.is_protected);
        target.classes.insert(name, inherited_class);
        return Ok(NestedClassMerge::Inserted);
    };

    if ast::classes_are_semantically_compatible(existing, &class)
        || nested_class_existing_redeclaration_shadows_inherited(existing, &class)
    {
        return Ok(NestedClassMerge::Skipped);
    }
    if nested_class_redeclaration_replaces_existing(existing, &class) {
        target.classes.insert(name, class);
        return Ok(NestedClassMerge::Inserted);
    }
    Err(Box::new(InstantiateError::conflicting_inheritance(
        name,
        "previous base",
        extend.base_name.to_string(),
        location_to_span(
            &extend.location,
            source_map,
            "conflicting inherited nested class extends clause",
        )?,
    )))
}

/// Name of the component an extends modification modifies, when a redeclaration
/// appears anywhere inside that modification (MLS §7.3).
///
/// `extends Wrap(h(redeclare C a[2]))` carries the redeclaration one level down:
/// the extends modification itself is an ordinary modification of `h`, and the
/// redeclare flag lives on the nested class-modification argument.
/// [`collect_redeclarations`] only reads redeclarations written directly on an
/// extends modification, so for this nested form neither the redeclared type nor
/// its dimensions are consumed. The enclosing component `h` is what must be
/// recorded — everything instantiated beneath it inherits the dropped
/// dimensions.
fn enclosing_component_of_nested_redeclare(modification: &ast::ExtendModification) -> Option<&str> {
    let ast::Expression::ClassModification { target, .. } = &modification.expr else {
        return None;
    };
    if !expression_contains_redeclare(&modification.expr) {
        return None;
    }
    target.parts.first().map(|part| part.ident.text.as_ref())
}

/// What an `extends` modification's redeclarations state about the inherited
/// components they replace (MLS §7.3).
struct CollectedRedeclarations {
    /// Redeclared component name -> exact resolved replacement type.
    types: IndexMap<String, RedeclaredType>,
    /// Redeclared component name -> the array dimensions the redeclaration
    /// states, for the redeclarations that state any.
    ///
    /// A missing entry means the redeclaration wrote no subscripts at all,
    /// which is not the same as declaring it scalar: MLS §7.3 leaves the
    /// replaced declaration's dimensions standing in that case, so only the
    /// entries present here reshape anything (see
    /// [`apply_redeclared_dimensions`]).
    dims: IndexMap<String, Vec<ast::Subscript>>,
    /// Every inherited component an extends modification redeclared, including
    /// the ones that contributed no type change.
    components: IndexSet<String>,
}

/// Apply the array dimensions a redeclaration states to the component it
/// replaces (MLS §7.3).
///
/// An element-redeclaration is a whole component declaration (MLS §A.2.5:
/// `component-clause1` -> `declaration` -> `IDENT [ array-subscripts ]`), so the
/// subscripts it writes are its own statement of the component's shape and
/// *replace* the replaced declaration's dimensions — rank and extent alike.
/// `extends Base(redeclare C a[4])` over `replaceable C a[2]` yields `a[4]`, and
/// over a scalar `replaceable C a` it yields an array; neither is an error.
/// OpenModelica agrees on every one of those (probe matrix in the task record:
/// scalar -> `[3]`, `[3]` -> `[4]`, `[2]` -> `[4]` through `extends`,
/// scalar -> `[2,2]`, `[2,2]` -> `[4]`), and a redeclaration that writes no
/// subscripts leaves the replaced dimensions standing — which is why this is
/// only ever called for a redeclaration that wrote some.
///
/// The dimension *expressions* are evaluated later against the class that owns
/// the `extends` clause, which is the scope the redeclaration was written in, as
/// MLS §7.3 requires (OMC probe: `Holder h(n = 5, redeclare B a[k])` with a
/// local `k = 2` yields `h.a[1..2]` while `h.n` stays 5).
///
/// ## Latent risk: the subscripts arrive carrying base-scope `def_id`s
///
/// These subscripts reach us through the extends modification, whose target
/// reference Resolve walks in the *base* class's scope
/// (`resolve_extend_modification`). So a `def_id` already attached to a
/// dimension expression here may point at a declaration of the base class, not
/// at the enclosing class the expression must actually be read in. Nothing
/// consumes those `def_id`s today — `resolve_component_dimensions` re-evaluates
/// `shape_expr` by name against the enclosing class's effective components,
/// which is why the two-scope probe above gets the right extent. A future
/// consumer that trusted them would silently take the base class's binding.
/// Clearing or re-resolving them belongs with that consumer, which can say what
/// the right scope is; guessing here would only move the trap.
///
/// ## `:` in a redeclaration is not judged here
///
/// `extends Base(redeclare C a[:])` leaves a `Subscript::Range`, which states no
/// extent and no binding follows it, so the component ends up rank-zero and the
/// model is accepted (probe C15). OpenModelica rejects it — "Failed to deduce
/// dimension 1 of a due to missing binding equation". This is *not* specific to
/// redeclarations: the identical declaration `C a[:]` with no binding takes the
/// same silent rank-zero path in this compiler (probe C14/C15 control), so
/// rejecting it only for redeclarations would split one gap into two behaviours.
/// The whole `:`-without-binding rule belongs to whoever closes the declaration
/// path; this function deliberately matches it rather than diverging.
fn apply_redeclared_dimensions(comp: &mut ast::Component, dims: &[ast::Subscript]) {
    comp.shape.clear();
    comp.shape_expr.clear();
    // Mirror the parser's declaration convention (`process_component_clause`):
    // every subscript is kept symbolically, and `shape` additionally records the
    // ones [`ast::Subscript::literal_dimension`] can decide on sight. That
    // helper is shared with the parser on purpose — a private copy here once
    // dropped its `Boolean` arm, so `redeclare C a[Boolean]` produced a scalar
    // while the identical declaration produced two elements.
    for subscript in dims {
        comp.shape_expr.push(subscript.clone());
        if let Some(dim) = subscript.literal_dimension() {
            comp.shape.push(dim);
        }
    }
}

/// The array dimensions a redeclare modification states, if any.
///
/// The parser keeps them on the redeclared name's own `ComponentRefPart`
/// (`redeclare C a[2]` -> target `a[2]`), so an empty subscript list and an
/// absent one are both reported as "stated nothing".
fn redeclared_dimensions(modification: &ast::ExtendModification) -> Option<Vec<ast::Subscript>> {
    let ast::Expression::Modification { target, .. } = &modification.expr else {
        return None;
    };
    let subs = target.parts.first()?.subs.as_ref()?;
    (!subs.is_empty()).then(|| subs.clone())
}

fn collect_redeclarations(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    extend: &ast::Extend,
    extend_span: Span,
) -> InstantiateResult<CollectedRedeclarations> {
    let mut redeclare_types = IndexMap::default();
    let mut redeclare_dims: IndexMap<String, Vec<ast::Subscript>> = IndexMap::default();
    let mut redeclared_components: IndexSet<String> = IndexSet::new();
    let mut validation_error: Option<Box<InstantiateError>> = None;

    walk_extend_modifications(extend, |modification| {
        // MLS §7.3: a redeclaration may sit *inside* an ordinary component
        // modification of the extends clause — `extends Wrap(h(redeclare C
        // a[2]))` modifies `h` and redeclares `h.a`. Only the outer `h(...)`
        // reaches this walk, so the redeclaration is recorded against `h`, the
        // enclosing component whose subtree inherits the dropped dimensions.
        if !modification.redeclare
            && let Some(enclosing) = enclosing_component_of_nested_redeclare(modification)
            && class.components.contains_key(enclosing)
        {
            redeclared_components.insert(enclosing.to_string());
        }
        let Some((target_name, target_ref, _value_expr)) = redeclare_target_value(modification)
        else {
            return;
        };
        if validation_error.is_some() {
            return;
        }
        let target_name_owned = target_name.to_string();
        let span = match redeclare_target_span(tree, &target_name_owned, target_ref, extend_span) {
            Ok(span) => span,
            Err(err) => {
                validation_error = Some(err);
                return;
            }
        };
        let new_type = match extract_redeclared_type(&modification.expr, span) {
            Ok(new_type) => new_type,
            Err(err) => {
                validation_error = Some(err);
                return;
            }
        };
        let Some(target_def_id) = resolve_cref_def_id(target_ref) else {
            validation_error = Some(Box::new(InstantiateError::missing_resolved_identity(
                format!("extends redeclare LHS `{target_name}`"),
                span,
            )));
            return;
        };
        let component_slot = class
            .components
            .iter()
            .find(|(_, component)| component.def_id == Some(target_def_id));
        let Some((component_name, component)) = component_slot else {
            let redeclared_class = match find_nested_class_by_def_id_in_hierarchy(
                tree,
                class,
                target_def_id,
            ) {
                Ok(Some(redeclared_class)) => redeclared_class,
                Ok(None) => {
                    validation_error = Some(Box::new(InstantiateError::redeclare_error(
                        &target_name_owned,
                        format!(
                            "resolved LHS identity {target_def_id:?} is absent from the base-class hierarchy"
                        ),
                        span,
                    )));
                    return;
                }
                Err(error) => {
                    validation_error = Some(error);
                    return;
                }
            };
            let redeclared_class_name = redeclared_class.name.text.as_ref();
            if let Err(err) = validate_class_redeclaration(
                tree,
                redeclared_class,
                redeclared_class_name,
                Some(&new_type),
                span,
            ) {
                validation_error = Some(err);
            }
            return;
        };

        if let Err(err) =
            validate_redeclaration(tree, component, component_name, Some(&new_type), span)
        {
            validation_error = Some(err);
            return;
        }

        let component_name = component_name.to_string();
        redeclared_components.insert(component_name.clone());
        // MLS §7.3: the redeclaration's own array dimensions, when it states
        // any, describe the component it replaces. Shape is stated
        // independently of the exact replacement identity proved above.
        if let Some(dims) = redeclared_dimensions(modification) {
            redeclare_dims.insert(component_name.clone(), dims);
        }
        redeclare_types.insert(component_name, new_type);
    });

    if let Some(err) = validation_error {
        return Err(err);
    }

    Ok(CollectedRedeclarations {
        types: redeclare_types,
        dims: redeclare_dims,
        components: redeclared_components,
    })
}

/// MLS §7.3.2: Validates constrainedby type constraints.
/// Full type replacement is deferred to later phases; here we validate structural constraints.
///
/// # Performance Note
///
/// This function clones components, equations, algorithms, and nested classes from the
/// borrowed `&ast::ClassDef`. Cloning is necessary because:
/// 1. We borrow from the ast::ClassTree which must remain immutable during compilation
/// 2. Inherited content may need mutations (e.g., applying protected visibility)
/// 3. The same base class may be inherited through multiple paths (diamond inheritance)
///
/// The inheritance cache (`InheritanceCache`) mitigates the cost by caching results
/// per DefId, avoiding redundant processing of the same base class.
fn merge_class_content(
    tree: &ast::ClassTree,
    target: &mut InheritedContent,
    class: &ast::ClassDef,
    extend: &ast::Extend,
) -> InstantiateResult<()> {
    let extend_span = location_to_span(&extend.location, &tree.source_map, "extends clause")?;
    let mut validation_error: Option<Box<InstantiateError>> = None;

    validate_break_names(class, extend, extend_span)?;

    // MLS §7.2: Collect value modifications (non-redeclare) from extends clause
    // These override default bindings in inherited components, e.g., extends Foo(n=2)
    let value_modifications = collect_value_modifications(extend, class);

    // MLS §7.3: Validate redeclarations and collect what they state
    let redeclarations = collect_redeclarations(tree, class, extend, extend_span)?;

    // MLS §5.6.1.4: same-named elements from several bases become one element,
    // so identity is decided on the merged class rather than on each base.
    let merged = merged_declared_names(target, class);

    // Merge components
    for (name, comp) in &class.components {
        // Check if this component is deselected via `break`
        if extend.break_names.contains(name) {
            continue;
        }

        if let Some(existing) = target.components.get(name) {
            // MLS §5.6: Check if components are from same origin or have compatible types
            if !inherited_components_are_identical(existing, comp, &merged) {
                return Err(Box::new(InstantiateError::conflicting_inheritance(
                    name.clone(),
                    "previous base",
                    extend.base_name.to_string(),
                    location_to_span(
                        &extend.location,
                        &tree.source_map,
                        "conflicting class content extends clause",
                    )?,
                )));
            }
            // Compatible - diamond inheritance is OK, keep existing
        } else {
            let mut inherited_comp = comp.clone();
            apply_protected_visibility(&mut inherited_comp, extend.is_protected);
            target.components.insert(name.clone(), inherited_comp);
        }
    }

    // MLS §7.3: record every redeclared inherited component *before* applying
    // the type changes. The redeclared type and its array dimensions are
    // consumed below; anything else the redeclaration stated is still lost
    // here, and the mark keeps later phases from reading the surviving
    // declaration as evidence about the source.
    //
    // The mark is deliberately *not* narrowed by the dimension propagation
    // below: a redeclaration reaching a component through a modifier on an
    // enclosing declaration (`Holder h(redeclare C a[2])`) still loses its
    // dimensions — and its type — on a path this function does not own, so
    // `InstanceData::had_redeclare` must keep covering it.
    for comp_name in &redeclarations.components {
        if let Some(comp) = target.components.get_mut(comp_name) {
            comp.redeclared_by_modification = true;
        }
    }

    // MLS §7.3: a redeclaration is a whole declaration, so the dimensions it
    // states replace the replaced declaration's. This is keyed independently
    // of the exact type change below because a redeclaration states both facts.
    for (comp_name, dims) in &redeclarations.dims {
        if let Some(comp) = target.components.get_mut(comp_name) {
            apply_redeclared_dimensions(comp, dims);
        }
    }

    // MLS §7.3: Apply redeclared types to inherited components
    // This updates the component's type so that instantiation uses the new type's fields
    for (comp_name, new_type) in &redeclarations.types {
        if let Some(comp) = target.components.get_mut(comp_name) {
            comp.type_name = rumoca_ir_ast::Name::from_string(&new_type.source_name);
            comp.type_name.def_id = Some(new_type.def_id);
            comp.type_def_id = Some(new_type.def_id);
        }
    }

    apply_value_modifications(target, value_modifications, extend_span)?;

    merge_nested_extends_modifications(target, extend);

    // Merge equations
    target.equations.extend(class.equations.clone());
    target
        .initial_equations
        .extend(class.initial_equations.clone());

    // Merge algorithms
    target.algorithms.extend(class.algorithms.clone());
    target
        .initial_algorithms
        .extend(class.initial_algorithms.clone());

    // Merge nested classes
    walk_nested_classes(class, |name, nested| {
        if let Some(existing) = target.classes.get(name) {
            if ast::classes_are_semantically_compatible(existing, nested) {
                return;
            }
            if nested_class_redeclaration_replaces_existing(existing, nested) {
                target.classes.insert(name.to_string(), nested.clone());
                return;
            }
            let span = match location_to_span(
                &extend.location,
                &tree.source_map,
                "conflicting nested class extends clause",
            ) {
                Ok(span) => span,
                Err(err) => {
                    validation_error = Some(err);
                    return;
                }
            };
            validation_error = Some(Box::new(InstantiateError::conflicting_inheritance(
                name.to_string(),
                "previous base",
                extend.base_name.to_string(),
                span,
            )));
        } else {
            let mut inherited_class = nested.clone();
            apply_protected_class_visibility(&mut inherited_class, extend.is_protected);
            target.classes.insert(name.to_string(), inherited_class);
        }
    });

    if let Some(err) = validation_error {
        return Err(err);
    }

    Ok(())
}

fn collect_value_modifications(
    extend: &ast::Extend,
    class: &ast::ClassDef,
) -> IndexMap<String, (ast::Expression, bool)> {
    let mut value_modifications = IndexMap::default();
    walk_extend_modifications(extend, |modification| {
        if let Some((name, value, is_final)) =
            try_extract_value_modification(modification, extend, class)
        {
            value_modifications.insert(name, (value, is_final));
        }
    });
    value_modifications
}

fn apply_value_modifications(
    target: &mut InheritedContent,
    value_modifications: IndexMap<String, (ast::Expression, bool)>,
    span: Span,
) -> InstantiateResult<()> {
    // MLS §7.2 / §4.4.4: value modifications (`x = expr`) update the binding
    // equation only. `start` is a distinct attribute (MLS §4.8.6).
    for (comp_name, (new_value, is_final)) in value_modifications {
        let Some(comp) = target.components.get_mut(&comp_name) else {
            continue;
        };
        if comp.is_final {
            return Err(Box::new(InstantiateError::redeclare_final(comp_name, span)));
        }
        comp.binding = Some(new_value);
        comp.has_explicit_binding = true;
        if is_final {
            comp.is_final = true;
        }
    }
    Ok(())
}

fn validate_break_names(
    class: &ast::ClassDef,
    extend: &ast::Extend,
    extend_span: Span,
) -> InstantiateResult<()> {
    let base_class_name = extend.base_name.to_string();
    for break_name in &extend.break_names {
        let exists_as_component = class.components.contains_key(break_name);
        let exists_as_class = class.classes.contains_key(break_name);
        if !exists_as_component && !exists_as_class {
            return Err(Box::new(InstantiateError::invalid_break_name(
                break_name,
                &base_class_name,
                extend_span,
            )));
        }
    }
    Ok(())
}

/// Merge nested class modifications from extends clause into inherited components.
///
/// MLS §7.2: When an extends clause has modifications like
/// `extends Foo(friction(useHeatPort=true))`, the nested modifications should be
/// merged into the inherited `friction` component's `modifications` map. This ensures
/// that when `friction` is later instantiated, the modification `useHeatPort=true`
/// is visible via `shift_modifications_down` and `populate_modification_environment`.
fn merge_nested_extends_modifications(target: &mut InheritedContent, extend: &ast::Extend) {
    walk_extend_modifications(extend, |modification| {
        // Extract target name and nested modifications from the expression.
        // Two formats exist:
        //   1. ClassModification { target: comp_name, modifications: [...] }
        //      For: extends Foo(friction(useHeatPort=true))
        //   2. Modification { target: comp_name, value: ClassModification { target: TypeName, modifications: [...] } }
        //      For: extends Foo(redeclare final NewType comp(nested=val))
        //      Type changes are handled by collect_redeclarations(); here we merge nested mods.
        let Some((target_name, modifications)) =
            extend_nested_target_modifications(extend, modification)
        else {
            return;
        };
        let Some(comp) = target.components.get_mut(&target_name) else {
            return;
        };
        for nested_mod in modifications {
            insert_nested_modification(comp, nested_mod);
        }
    });
}

fn extend_nested_target_modifications<'a>(
    extend: &ast::Extend,
    modification: &'a ast::ExtendModification,
) -> Option<(String, &'a [ast::Expression])> {
    match &modification.expr {
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => Some((
            extend_relative_component_target(extend, target)?,
            modifications.as_slice(),
        )),
        ast::Expression::Modification {
            target,
            value: Some(value),
            ..
        } => {
            let ast::Expression::ClassModification { modifications, .. } = value.as_ref() else {
                return None;
            };
            Some((
                extend_relative_component_target(extend, target)?,
                modifications.as_slice(),
            ))
        }
        _ => None,
    }
}

/// Insert a single nested modification into a component's modifications map.
fn insert_nested_modification(comp: &mut ast::Component, nested_mod: &ast::Expression) {
    match nested_mod {
        ast::Expression::Modification {
            target: t,
            value: Some(value),
            ..
        } => {
            if let Some(name) = t.parts.first().map(|p| p.ident.text.to_string()) {
                comp.modifications.insert(name, value.as_ref().clone());
            }
        }
        // A value-less modifier binds nothing (MLS §7.2).
        ast::Expression::Modification { value: None, .. } => {}
        ast::Expression::NamedArgument { name, value, .. } => {
            comp.modifications
                .insert(name.text.to_string(), value.as_ref().clone());
        }
        ast::Expression::ClassModification { .. } => {
            if let Some(name) = extract_modification_target(nested_mod) {
                comp.modifications.insert(name, nested_mod.clone());
            }
        }
        _ => {}
    }
}

/// Get the effective components for a class (own + inherited).
pub fn get_effective_components(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<IndexMap<String, ast::Component>> {
    let mut cache = InheritanceCache::default();
    get_effective_components_with_cache(tree, class, &mut cache)
}

/// Callback for resolving effective components.
/// Suitable for use as `InstantiateEvalCtx::resolve_class_components`.
pub fn resolve_effective_components_for_eval(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> IndexMap<String, ast::Component> {
    get_effective_components(tree, class)
        .expect("inheritance must be validated before resolving components for eval")
}

/// Get the effective components for a class with caching.
pub fn get_effective_components_with_cache(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    cache: &mut InheritanceCache,
) -> InstantiateResult<IndexMap<String, ast::Component>> {
    let mut inherited = process_extends_with_cache(tree, class, cache)?;

    // The class's own components override inherited ones
    for (name, comp) in &class.components {
        inherited.components.insert(name.clone(), comp.clone());
    }

    // MLS §7.1/§7.3: local class names (including inherited replaceable classes)
    // are valid type names for component declarations in the effective class scope.
    // Preserve their resolved DefIds so later phases don't treat names like
    // `FlowModel` as undefined global types.
    let local_type_def_ids =
        collect_local_type_def_ids(&inherited.classes, &class.classes, &inherited.components);
    populate_local_component_type_def_ids(&mut inherited.components, &local_type_def_ids);

    Ok(inherited.components)
}

fn collect_local_type_def_ids(
    inherited_classes: &IndexMap<String, ast::ClassDef>,
    own_classes: &IndexMap<String, ast::ClassDef>,
    components: &IndexMap<String, ast::Component>,
) -> IndexMap<String, DefId> {
    let mut local = IndexMap::default();

    for (name, class) in inherited_classes {
        if let Some(def_id) = class.def_id {
            local.insert(name.clone(), def_id);
        }
    }

    for (name, class) in own_classes {
        if let Some(def_id) = class.def_id {
            local.insert(name.clone(), def_id);
        }
    }

    // Components with explicit type_def_id can also anchor short local names
    // during inherited-content synthesis.
    for comp in components.values() {
        if let Some(def_id) = comp.type_def_id {
            let short = comp
                .type_name
                .name
                .last()
                .map(|token| token.text.as_ref())
                .unwrap_or_default();
            if !short.is_empty() {
                local.entry(short.to_string()).or_insert(def_id);
            }
        }
    }

    local
}

fn populate_local_component_type_def_ids(
    components: &mut IndexMap<String, ast::Component>,
    local_type_def_ids: &IndexMap<String, DefId>,
) {
    for comp in components.values_mut() {
        if comp.type_def_id.is_some() {
            continue;
        }

        let type_name = comp.type_name.to_string();
        if type_name.is_empty() {
            continue;
        }
        let is_dotted = comp.type_name.name.len() > 1;

        // `type_name.def_id` may be a partial first-segment anchor (e.g. `Medium`
        // for `Medium.AbsolutePressure`). Promote it only for short names.
        if let Some(def_id) = comp.type_name.def_id {
            if !is_dotted {
                comp.type_def_id = Some(def_id);
            }
            continue;
        }

        // Dotted names are already scope-qualified or package-member references;
        // this fix only resolves local short names in the effective class scope.
        if is_dotted {
            continue;
        }

        if let Some(def_id) = local_type_def_ids.get(&type_name).copied() {
            comp.type_def_id = Some(def_id);
            comp.type_name.def_id = Some(def_id);
        }
    }
}

/// Get the effective equations for a class (own + inherited).
pub fn get_effective_equations(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> InstantiateResult<Vec<ast::Equation>> {
    let mut cache = InheritanceCache::default();
    get_effective_equations_with_cache(tree, class, &mut cache)
}

/// Get the effective equations for a class with caching.
pub fn get_effective_equations_with_cache(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    cache: &mut InheritanceCache,
) -> InstantiateResult<Vec<ast::Equation>> {
    let mut inherited = process_extends_with_cache(tree, class, cache)?;
    inherited.equations.extend(class.equations.clone());
    Ok(inherited.equations)
}

#[cfg(test)]
mod tests;
