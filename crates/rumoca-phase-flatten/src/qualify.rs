//! Shared expression and component reference qualification utilities.
//!
//! This module provides functions for qualifying variable names with instance prefixes,
//! used by both equation flattening and algorithm processing.

use rumoca_core::Token;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::{
    ComponentRefPart, ComponentReference, Expression, ForIndex, QualifiedName, Subscript,
    TerminalType,
};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::HashSet;
use std::sync::Arc;

/// Resolved import map: short name → fully-qualified name.
///
/// Built from `ClassDef.imports` during instantiation. For example,
/// `import Modelica.Constants.pi;` produces `("pi", "Modelica.Constants.pi")`.
pub type ImportMap = FxHashMap<String, String>;

type MemberDefIdMap<'tree> = Arc<FxHashMap<&'tree str, rumoca_core::DefId>>;

#[derive(Default)]
pub(crate) struct MemberDefIdCache<'tree> {
    maps: FxHashMap<rumoca_core::DefId, MemberDefIdMap<'tree>>,
}

/// Add imports visible from a lexical class scope.
///
/// The scope is structured, so callers do not recover hierarchy by splitting a
/// flattened variable name. Ancestor imports are added first and the current
/// class imports last, matching normal lexical shadowing.
pub(crate) fn collect_imports_for_source_scope(
    class_index: &ast::ClassDefIndex<'_>,
    source_scope: &QualifiedName,
    imports: &mut ImportMap,
) {
    let Some(source_def_id) = class_index.def_id_by_qualified_name(&source_scope.to_flat_string())
    else {
        return;
    };
    let mut chain = Vec::new();
    let mut current = Some(source_def_id);
    while let Some(def_id) = current {
        chain.push(def_id);
        current = class_index.parent_def_id(def_id);
    }
    for def_id in chain.into_iter().rev() {
        if let Some(class_def) = class_index.get(def_id) {
            resolve_import_pairs(&class_def.imports, class_index, imports);
        }
    }
}

fn resolve_import_pairs(
    imports: &[ast::Import],
    class_index: &ast::ClassDefIndex<'_>,
    map: &mut ImportMap,
) {
    for import in imports {
        match import {
            ast::Import::Qualified { path, .. } => {
                let Some(alias) = path.name.last() else {
                    continue;
                };
                map.insert(alias.text.to_string(), path.to_string());
            }
            ast::Import::Renamed { alias, path, .. } => {
                map.insert(alias.text.to_string(), path.to_string());
            }
            ast::Import::Unqualified { path, .. } => {
                let pkg_name = path.to_string();
                let Some(class_def) = class_index.get_by_qualified_name(&pkg_name) else {
                    continue;
                };
                for name in class_def.components.keys() {
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
                for name in class_def.classes.keys() {
                    map.insert(name.clone(), format!("{pkg_name}.{name}"));
                }
            }
            ast::Import::Selective { path, names, .. } => {
                let pkg_name = path.to_string();
                for name in names {
                    map.insert(name.text.to_string(), format!("{pkg_name}.{}", name.text));
                }
            }
        }
    }
}

/// Add lexical package aliases visible from `class_name` into the import map.
///
/// Modelica class/package names are visible through lexical scope nesting.
/// This helper materializes those package-name aliases so short package refs
/// (for example `Common.*` inside `Modelica.Media.IdealGases.*`) can be
/// qualified to their fully qualified names without heuristics.
pub(crate) fn collect_lexical_package_aliases(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class_name: &str,
    imports: &mut ImportMap,
) {
    let Some(source_def_id) = class_index.def_id_by_qualified_name(class_name) else {
        return;
    };
    collect_lexical_package_aliases_for_def_id(tree, class_index, source_def_id, imports);
}

pub(crate) fn collect_lexical_package_aliases_for_def_id(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    source_def_id: rumoca_core::DefId,
    imports: &mut ImportMap,
) {
    collect_lexical_package_aliases_for_def_id_with_member_cache(
        tree,
        class_index,
        source_def_id,
        imports,
        None,
    );
}

pub(crate) fn collect_lexical_package_aliases_for_def_id_with_member_cache<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    source_def_id: rumoca_core::DefId,
    imports: &mut ImportMap,
    mut member_cache: Option<&mut MemberDefIdCache<'tree>>,
) {
    let Some(source_class) = class_index.get(source_def_id) else {
        return;
    };
    let lookup_scope = source_class.scope_id;
    let mut source_member_def_ids = None;

    let mut ancestor_def_ids = Vec::new();
    let mut current = class_index.parent_def_id(source_def_id);
    while let Some(def_id) = current {
        ancestor_def_ids.push(def_id);
        current = class_index.parent_def_id(def_id);
    }

    for ancestor_def_id in ancestor_def_ids.into_iter().rev() {
        if !class_has_child_package(class_index, ancestor_def_id) {
            continue;
        }
        let source_member_def_ids = source_member_def_ids.get_or_insert_with(|| {
            collect_class_or_base_member_def_ids(
                class_index,
                source_class,
                member_cache.as_deref_mut(),
            )
        });
        collect_class_child_package_aliases(
            tree,
            class_index,
            lookup_scope,
            source_member_def_ids,
            ancestor_def_id,
            imports,
        );
    }
}

/// Add lexical class aliases visible from `class_name` into the import map.
///
/// MLS §5.3 lookup makes nested classes visible through enclosing scopes. This
/// is broader than package aliases: short class definitions such as
/// `record iter = Inverses.accuracy` can be used as qualified class scopes
/// (`iter.delp`) inside sibling functions.
pub(crate) fn collect_lexical_class_aliases(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class_name: &str,
    imports: &mut ImportMap,
) {
    let Some(source_def_id) = class_index.def_id_by_qualified_name(class_name) else {
        return;
    };
    collect_lexical_class_aliases_for_def_id(tree, class_index, source_def_id, imports);
}

pub(crate) fn collect_lexical_class_aliases_for_def_id(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    source_def_id: rumoca_core::DefId,
    imports: &mut ImportMap,
) {
    collect_lexical_class_aliases_for_def_id_with_member_cache(
        tree,
        class_index,
        source_def_id,
        imports,
        None,
    );
}

pub(crate) fn collect_lexical_class_aliases_for_def_id_with_member_cache<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    source_def_id: rumoca_core::DefId,
    imports: &mut ImportMap,
    member_cache: Option<&mut MemberDefIdCache<'tree>>,
) {
    let Some(source_class) = class_index.get(source_def_id) else {
        return;
    };
    let lookup_scope = source_class.scope_id;
    let Some(original_lookup_scope) = lookup_scope else {
        return;
    };
    let source_member_def_ids =
        collect_class_or_base_member_def_ids(class_index, source_class, member_cache);
    let mut scope_id = original_lookup_scope;

    while let Some(scope) = tree.scope_tree.get(scope_id) {
        for (alias, def_id) in &scope.members {
            let Some(class_def) = class_index.get(*def_id) else {
                continue;
            };
            if !is_expression_visible_class_type(&class_def.class_type) {
                continue;
            }
            let Some(qualified_name) = tree.def_map.get(def_id) else {
                continue;
            };
            let alias_name = alias.as_str();
            if member_map_declares_different_def_id(&source_member_def_ids, alias_name, *def_id) {
                continue;
            }
            if let Some(visible_def_id) = tree.scope_tree.lookup(original_lookup_scope, alias)
                && visible_def_id != *def_id
            {
                continue;
            }
            imports.insert(alias_name.to_string(), qualified_name.clone());
        }

        let Some(parent) = tree.scope_tree.parent(scope_id) else {
            break;
        };
        scope_id = parent;
    }
}

fn is_expression_visible_class_type(class_type: &rumoca_core::ClassType) -> bool {
    matches!(
        class_type,
        rumoca_core::ClassType::Package
            | rumoca_core::ClassType::Function
            | rumoca_core::ClassType::Record
            | rumoca_core::ClassType::Model
            | rumoca_core::ClassType::Block
            | rumoca_core::ClassType::Class
            | rumoca_core::ClassType::Connector
            | rumoca_core::ClassType::Type
            | rumoca_core::ClassType::Operator
    )
}

pub(crate) fn collect_lexical_constant_aliases_for_source_scope_with_packages(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    source_scope: &QualifiedName,
    active_packages: &[String],
    imports: &mut ImportMap,
) {
    let source_name = source_scope.to_flat_string();
    let Some(source_def_id) = class_index.def_id_by_qualified_name(&source_name) else {
        return;
    };
    collect_lexical_constant_aliases_for_def_id_with_packages(
        tree,
        class_index,
        source_def_id,
        active_packages,
        imports,
    );
}

pub(crate) fn collect_lexical_constant_aliases_for_def_id_with_packages(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    source_def_id: rumoca_core::DefId,
    active_packages: &[String],
    imports: &mut ImportMap,
) {
    collect_lexical_constant_aliases_for_def_id_with_packages_and_member_cache(
        tree,
        class_index,
        source_def_id,
        active_packages,
        imports,
        None,
    );
}

pub(crate) fn collect_lexical_constant_aliases_for_def_id_with_packages_and_member_cache<'tree>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'tree>,
    source_def_id: rumoca_core::DefId,
    active_packages: &[String],
    imports: &mut ImportMap,
    member_cache: Option<&mut MemberDefIdCache<'tree>>,
) {
    let Some(source_class) = class_index.get(source_def_id) else {
        return;
    };

    let source_member_def_ids =
        collect_class_or_base_member_def_ids(class_index, source_class, member_cache);
    let mut ancestor_def_ids = Vec::new();
    let mut current = class_index.parent_def_id(source_def_id);
    while let Some(def_id) = current {
        ancestor_def_ids.push(def_id);
        current = class_index.parent_def_id(def_id);
    }

    for ancestor_def_id in ancestor_def_ids.into_iter().rev() {
        let Some(ancestor_class) = class_index.get(ancestor_def_id) else {
            continue;
        };
        let ancestor_name = class_index
            .qualified_name(ancestor_def_id)
            .expect("class index ancestor must have a canonical qualified name");
        let target_scope =
            lexical_constant_target_scope(tree, class_index, ancestor_name, active_packages);
        // MLS §5.3.2: enclosing-scope lookup reaches class constants (and,
        // for package enclosers, package members). A non-package ancestor's
        // parameters are instance members and must never become
        // class-qualified alias targets.
        let ancestor_is_package =
            matches!(ancestor_class.class_type, rumoca_core::ClassType::Package);
        for (name, component) in &ancestor_class.components {
            let alias_visible = match component.variability {
                rumoca_core::Variability::Constant(_) => true,
                rumoca_core::Variability::Parameter(_) => ancestor_is_package,
                _ => false,
            };
            if !alias_visible {
                continue;
            }
            if source_member_def_ids.contains_key(name.as_str()) {
                continue;
            }
            imports.insert(name.clone(), format!("{target_scope}.{name}"));
        }
    }
}

fn lexical_constant_target_scope<'a>(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    canonical_scope: &'a str,
    active_packages: &'a [String],
) -> &'a str {
    for package_name in active_packages {
        if crate::pipeline::package_chain_contains(tree, class_index, package_name, canonical_scope)
        {
            return package_name;
        }
    }
    canonical_scope
}

fn collect_class_child_package_aliases(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    lookup_scope: Option<rumoca_core::ScopeId>,
    source_member_def_ids: &FxHashMap<&str, rumoca_core::DefId>,
    ancestor_def_id: rumoca_core::DefId,
    imports: &mut ImportMap,
) {
    let Some(ancestor_class) = class_index.get(ancestor_def_id) else {
        return;
    };
    for (child_name, child_class) in &ancestor_class.classes {
        if !matches!(child_class.class_type, rumoca_core::ClassType::Package) {
            continue;
        }
        let Some(def_id) = child_class.def_id else {
            continue;
        };
        let qualified_name = class_index
            .qualified_name(def_id)
            .expect("class index package child must have a canonical qualified name");
        insert_visible_lexical_package_alias(
            tree,
            lookup_scope,
            source_member_def_ids,
            child_name,
            def_id,
            qualified_name,
            imports,
        );
    }
}

fn class_has_child_package(
    class_index: &ast::ClassDefIndex<'_>,
    ancestor_def_id: rumoca_core::DefId,
) -> bool {
    class_index.get(ancestor_def_id).is_some_and(|class_def| {
        class_def
            .classes
            .values()
            .any(|child| matches!(child.class_type, rumoca_core::ClassType::Package))
    })
}

fn insert_visible_lexical_package_alias(
    tree: &ast::ClassTree,
    lookup_scope: Option<rumoca_core::ScopeId>,
    source_member_def_ids: &FxHashMap<&str, rumoca_core::DefId>,
    alias: &str,
    def_id: rumoca_core::DefId,
    qualified_name: &str,
    imports: &mut ImportMap,
) {
    if imports.contains_key(alias) {
        return;
    }
    if member_map_declares_different_def_id(source_member_def_ids, alias, def_id) {
        return;
    }
    if let Some(scope_id) = lookup_scope
        && let Some(visible_def_id) = tree
            .scope_tree
            .lookup(scope_id, &rumoca_core::ComponentPath::from_flat_path(alias))
        && visible_def_id != def_id
    {
        return;
    }
    imports.insert(alias.to_string(), qualified_name.to_string());
}

fn member_map_declares_different_def_id(
    source_member_def_ids: &FxHashMap<&str, rumoca_core::DefId>,
    alias: &str,
    lexical_def_id: rumoca_core::DefId,
) -> bool {
    source_member_def_ids
        .get(alias)
        .is_some_and(|member_def_id| *member_def_id != lexical_def_id)
}

fn collect_class_or_base_member_def_ids<'tree>(
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    member_cache: Option<&mut MemberDefIdCache<'tree>>,
) -> MemberDefIdMap<'tree> {
    if let Some(cache) = member_cache {
        return collect_class_or_base_member_def_ids_cached(class_index, class_def, cache);
    }
    let mut members = FxHashMap::default();
    let mut visited = FxHashSet::default();
    collect_class_or_base_member_def_ids_recursive(
        class_index,
        class_def,
        &mut visited,
        &mut members,
    );
    Arc::new(members)
}

fn collect_class_or_base_member_def_ids_cached<'tree>(
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    member_cache: &mut MemberDefIdCache<'tree>,
) -> MemberDefIdMap<'tree> {
    let Some(def_id) = class_def.def_id else {
        return collect_class_or_base_member_def_ids(class_index, class_def, None);
    };
    if let Some(cached) = member_cache.maps.get(&def_id) {
        return Arc::clone(cached);
    }
    let members = collect_class_or_base_member_def_ids(class_index, class_def, None);
    member_cache.maps.insert(def_id, Arc::clone(&members));
    members
}

fn collect_class_or_base_member_def_ids_recursive<'tree>(
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    visited: &mut FxHashSet<rumoca_core::DefId>,
    members: &mut FxHashMap<&'tree str, rumoca_core::DefId>,
) {
    members.reserve(class_def.components.len() + class_def.classes.len());
    for (name, component) in &class_def.components {
        if let Some(def_id) = component
            .def_id
            .or(component.type_def_id)
            .or(component.type_name.def_id)
        {
            members.entry(name.as_str()).or_insert(def_id);
        }
    }
    for (name, class_def) in &class_def.classes {
        if let Some(def_id) = class_def.def_id {
            members.entry(name.as_str()).or_insert(def_id);
        }
    }
    for ext in &class_def.extends {
        let Some(base_def_id) = ext.base_def_id.or(ext.base_name.def_id) else {
            continue;
        };
        if !visited.insert(base_def_id) {
            continue;
        }
        let Some(base_class) = class_index.get(base_def_id) else {
            continue;
        };
        collect_class_or_base_member_def_ids_recursive(class_index, base_class, visited, members);
    }
}

/// Options for component reference qualification.
#[derive(Default, Clone, Copy)]
pub struct QualifyOptions {
    /// Whether to skip qualification of local references.
    pub skip_local: bool,
    /// Whether to preserve the original def_id (false = reset to None).
    pub preserve_def_id: bool,
}

// ── Public API ──────────────────────────────────────────────────────────────

/// Qualify a component reference by prepending prefix parts.
pub fn qualify_component_ref(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
) -> ComponentReference {
    let locals = HashSet::new();
    let imports = ImportMap::default();
    qualify_cr_inner(cr, prefix, opts, &locals, &imports)
}

/// Qualify an expression by qualifying all component references within it.
pub fn qualify_expression(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
) -> Expression {
    let locals = HashSet::new();
    let imports = ImportMap::default();
    qualify_expr_inner(expr, prefix, opts, &locals, &imports)
}

// ── Public API (import-aware) ───────────────────────────────────────────────

/// Qualify a component reference, resolving imported short names via the import map.
pub fn qualify_component_ref_with_imports(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    imports: &ImportMap,
) -> ComponentReference {
    let locals = HashSet::new();
    qualify_cr_inner(cr, prefix, opts, &locals, imports)
}

/// Qualify a component reference with explicit local-scope identifiers.
///
/// This is used by algorithm `for`/comprehension handling where loop indices
/// must stay local inside nested expressions (MLS §11.2.2.2).
pub fn qualify_component_ref_with_imports_and_locals(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> ComponentReference {
    qualify_cr_inner(cr, prefix, opts, locals, imports)
}

/// Qualify an expression, resolving imported short names via the import map.
///
/// Single-part references that match an import entry are expanded to their
/// fully-qualified form instead of being prefixed with the component path.
pub fn qualify_expression_with_imports(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    imports: &ImportMap,
) -> Expression {
    let locals = HashSet::new();
    qualify_expr_inner(expr, prefix, opts, &locals, imports)
}

/// Qualify an expression with explicit local-scope identifiers.
///
/// This is used by algorithm `for`/comprehension handling where loop indices
/// must stay local inside nested expressions (MLS §11.2.2.2).
pub fn qualify_expression_with_imports_and_locals(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    qualify_expr_inner(expr, prefix, opts, locals, imports)
}

// ── Utility functions ───────────────────────────────────────────────────────

/// Convert integer subscripts to Subscript expressions.
pub fn subscripts_from_indices(
    indices: &[i64],
    owner_span: rumoca_core::Span,
) -> Option<Vec<Subscript>> {
    if indices.is_empty() {
        return None;
    }
    Some(
        indices
            .iter()
            .map(|&i| Subscript::Expression(int_expr_with_span(i, owner_span)))
            .collect(),
    )
}

/// Create an integer literal expression.
fn int_expr_with_span(value: i64, span: rumoca_core::Span) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: std::sync::Arc::from(value.to_string()),
            ..Default::default()
        },
        span,
    }
}

/// Create an integer literal expression for tests.
#[cfg(test)]
pub fn int_expr(value: i64) -> Expression {
    int_expr_with_span(
        value,
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_flatten_qualify_source_7.mo"),
            0,
            1,
        ),
    )
}

// ── Internal implementation ─────────────────────────────────────────────────

fn is_local_root_ref(cr: &ComponentReference, locals: &HashSet<String>) -> bool {
    cr.parts
        .first()
        .is_some_and(|part| locals.contains(part.ident.text.as_ref()))
}

/// Check if a component reference appears to be already fully-qualified.
///
/// A reference is considered fully-qualified if:
/// - The first part is a known package name like "Modelica"
///
/// This prevents instance prefixes from being added to global references like
/// `Modelica.Constants.eps` which would incorrectly become `instance.Modelica.Constants.eps`.
///
/// Note: We only check for known packages, not uppercase heuristics. References like
/// `ICP.di` (where ICP is a sub-component) must still get prefixed to become `L1.ICP.di`.
fn is_likely_fully_qualified(cr: &ComponentReference) -> bool {
    if cr.parts.is_empty() {
        return false;
    }

    let first_part = cr.parts[0].ident.text.as_ref();

    let known_packages = [
        "Modelica",
        "ModelicaTest",
        "ModelicaTestOverdetermined",
        "Complex",
        "ModelicaServices",
        "Modelica_DeviceDrivers",
        "Buildings",
        "OpenIPSL",
        "PowerSystems",
        "ThermoPower",
    ];

    known_packages.contains(&first_part)
}

/// Built-in enumeration literals are globally visible and must not be
/// qualified with instance prefixes (MLS §4.4.4.2, §8.3.7).
fn is_builtin_enum_literal_ref(cr: &ComponentReference) -> bool {
    if cr.parts.len() < 2 {
        return false;
    }
    matches!(
        cr.parts[0].ident.text.as_ref(),
        "StateSelect" | "AssertionLevel"
    )
}

/// Build component-reference parts from a dotted fully-qualified name.
fn fqn_component_ref_parts(fqn: &str) -> Vec<ComponentRefPart> {
    crate::path_utils::segments(fqn)
        .into_iter()
        .map(|seg| ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(seg),
                ..Default::default()
            },
            subs: None,
        })
        .collect()
}

fn qualify_component_part_subs(
    part: &ComponentRefPart,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> ComponentRefPart {
    ComponentRefPart {
        ident: part.ident.clone(),
        subs: part.subs.as_ref().map(|subs| {
            subs.iter()
                .map(|sub| qualify_sub_inner(sub, prefix, opts, locals, imports))
                .collect()
        }),
    }
}

fn resolve_import_alias_ref(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Option<ComponentReference> {
    let first_part = cr.parts.first()?;
    let alias = first_part.ident.text.as_ref();
    if locals.contains(alias) {
        return None;
    }
    let fqn = imports.get(alias)?;

    let mut imported_parts = fqn_component_ref_parts(fqn);
    let non_empty_subs = first_part.subs.as_ref().filter(|subs| !subs.is_empty());
    if let Some(subs) = non_empty_subs {
        let last_part = imported_parts.last_mut()?;
        last_part.subs = Some(
            subs.iter()
                .map(|sub| qualify_sub_inner(sub, prefix, opts, locals, imports))
                .collect(),
        );
    }
    if cr.parts.len() > 1 {
        imported_parts.extend(
            cr.parts
                .iter()
                .skip(1)
                .map(|part| qualify_component_part_subs(part, prefix, opts, locals, imports)),
        );
    }

    Some(ComponentReference {
        local: false,
        parts: imported_parts,
        def_id: if opts.preserve_def_id {
            cr.def_id
        } else {
            None
        },
        span: cr.span,
    })
}

fn qualify_cr_inner(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> ComponentReference {
    let qualify_part_subs =
        |part: &ComponentRefPart| qualify_component_part_subs(part, prefix, opts, locals, imports);

    if is_local_root_ref(cr, locals) {
        let mut local = cr.clone();
        local.parts = local.parts.iter().map(qualify_part_subs).collect();
        if !opts.preserve_def_id {
            local.def_id = None;
        }
        return local;
    }

    // Skip if local and option is set
    if opts.skip_local && cr.local {
        return cr.clone();
    }

    // Skip qualification if reference appears to be already fully-qualified
    if is_likely_fully_qualified(cr) {
        return ComponentReference {
            local: cr.local,
            parts: cr.parts.iter().map(qualify_part_subs).collect(),
            def_id: if opts.preserve_def_id {
                cr.def_id
            } else {
                None
            },
            span: cr.span,
        };
    }

    if is_builtin_enum_literal_ref(cr) {
        return ComponentReference {
            local: cr.local,
            parts: cr.parts.iter().map(qualify_part_subs).collect(),
            def_id: if opts.preserve_def_id {
                cr.def_id
            } else {
                None
            },
            span: cr.span,
        };
    }

    // MLS §3.7.3: `time` is a built-in variable, never a component member.
    if cr.parts.len() == 1 && cr.parts[0].ident.text.as_ref() == "time" {
        return cr.clone();
    }

    // MLS §13.2: Resolve imported short names to their fully-qualified form.
    // E.g., `pi` from `import Modelica.Constants.pi` → `Modelica.Constants.pi`,
    // and `L.'1'` from `import ...Interfaces.Logic as L` →
    // `Modelica.Electrical.Digital.Interfaces.Logic.'1'`.
    //
    // This must happen BEFORE the empty-prefix early-return so top-level and
    // modification bindings still get import resolution.
    if let Some(imported_ref) = resolve_import_alias_ref(cr, prefix, opts, locals, imports) {
        return imported_ref;
    }

    // No prefix: nothing to prepend, return as-is (imports already resolved above)
    if prefix.is_empty() {
        return cr.clone();
    }

    let mut parts = Vec::with_capacity(prefix.parts.len() + cr.parts.len());

    // Add prefix parts
    for (name, subs) in &prefix.parts {
        parts.push(ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(name.as_str()),
                ..Default::default()
            },
            subs: subscripts_from_indices(subs, cr.span),
        });
    }

    // Add original parts, qualifying any subscript expressions within them.
    for part in &cr.parts {
        parts.push(qualify_part_subs(part));
    }

    ComponentReference {
        local: if opts.skip_local { false } else { cr.local },
        parts,
        // Single-part references carry the resolved declaration for that
        // reference. Keep it when adding an instance prefix so later semantic
        // passes can still distinguish package-owned constants/functions from
        // ordinary instance fields.
        def_id: if opts.preserve_def_id || cr.parts.len() == 1 {
            cr.def_id
        } else {
            None
        },
        span: cr.span,
    }
}

fn qualify_function_call_ref(
    cr: &ComponentReference,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> ComponentReference {
    let qualify_part_subs =
        |part: &ComponentRefPart| qualify_component_part_subs(part, prefix, opts, locals, imports);

    if cr.local && is_unqualified_builtin_function_ref(cr) {
        return ComponentReference {
            local: cr.local,
            parts: cr.parts.iter().map(qualify_part_subs).collect(),
            def_id: if opts.preserve_def_id {
                cr.def_id
            } else {
                None
            },
            span: cr.span,
        };
    }

    if let Some(imported_ref) = resolve_import_alias_ref(cr, prefix, opts, locals, imports) {
        return imported_ref;
    }

    if is_unqualified_builtin_function_ref(cr) {
        return ComponentReference {
            local: cr.local,
            parts: cr.parts.iter().map(qualify_part_subs).collect(),
            def_id: if opts.preserve_def_id {
                cr.def_id
            } else {
                None
            },
            span: cr.span,
        };
    }

    ComponentReference {
        local: cr.local,
        parts: cr.parts.iter().map(qualify_part_subs).collect(),
        def_id: if opts.preserve_def_id {
            cr.def_id
        } else {
            None
        },
        span: cr.span,
    }
}

fn is_unqualified_builtin_function_ref(cr: &ComponentReference) -> bool {
    let Some(part) = cr.parts.first() else {
        return false;
    };
    cr.parts.len() == 1
        && (rumoca_core::BuiltinFunction::from_name(part.ident.text.as_ref()).is_some()
            || rumoca_core::is_builtin_function(part.ident.text.as_ref()))
}

fn qualify_vec_inner(
    exprs: &[Expression],
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Vec<Expression> {
    exprs
        .iter()
        .map(|e| qualify_expr_inner(e, prefix, opts, locals, imports))
        .collect()
}

fn qualify_opt_inner(
    expr: &Option<Arc<Expression>>,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Option<Arc<Expression>> {
    expr.as_ref().map(|e| {
        Arc::new(qualify_expr_inner(
            e.as_ref(),
            prefix,
            opts,
            locals,
            imports,
        ))
    })
}

fn locals_with_indices(locals: &HashSet<String>, indices: &[ForIndex]) -> HashSet<String> {
    let mut scoped = locals.clone();
    for index in indices {
        scoped.insert(index.ident.text.to_string());
    }
    scoped
}

fn qualify_if_inner(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    span: rumoca_core::Span,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, body)| {
                (
                    qualify_expr_inner(cond, prefix, opts, locals, imports),
                    qualify_expr_inner(body, prefix, opts, locals, imports),
                )
            })
            .collect(),
        else_branch: Arc::new(qualify_expr_inner(
            else_branch,
            prefix,
            opts,
            locals,
            imports,
        )),
        span,
    }
}

#[derive(Clone, Copy)]
struct QualifyExprEnv<'a> {
    prefix: &'a QualifiedName,
    opts: QualifyOptions,
    locals: &'a HashSet<String>,
    imports: &'a ImportMap,
}

fn qualify_array_comprehension_inner(
    inner_expr: &Expression,
    indices: &[ForIndex],
    filter: &Option<Arc<Expression>>,
    span: rumoca_core::Span,
    env: QualifyExprEnv<'_>,
) -> Expression {
    let comprehension_locals = locals_with_indices(env.locals, indices);
    Expression::ArrayComprehension {
        expr: Arc::new(qualify_expr_inner(
            inner_expr,
            env.prefix,
            env.opts,
            &comprehension_locals,
            env.imports,
        )),
        indices: qualify_for_indices_inner(indices, env.prefix, env.opts, env.locals, env.imports),
        filter: qualify_opt_inner(
            filter,
            env.prefix,
            env.opts,
            &comprehension_locals,
            env.imports,
        ),
        span,
    }
}

fn qualify_expr_inner(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    match expr {
        Expression::ComponentReference(cr) => {
            Expression::ComponentReference(qualify_cr_inner(cr, prefix, opts, locals, imports))
        }
        Expression::Binary { op, lhs, rhs, span } => Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(qualify_expr_inner(lhs, prefix, opts, locals, imports)),
            rhs: Arc::new(qualify_expr_inner(rhs, prefix, opts, locals, imports)),
            span: *span,
        },
        Expression::Unary { op, rhs, span } => Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(qualify_expr_inner(rhs, prefix, opts, locals, imports)),
            span: *span,
        },
        Expression::FunctionCall { comp, args, span } => Expression::FunctionCall {
            comp: qualify_function_call_ref(comp, prefix, opts, locals, imports),
            args: qualify_vec_inner(args, prefix, opts, locals, imports),
            span: *span,
        },
        Expression::If {
            branches,
            else_branch,
            span,
        } => qualify_if_inner(branches, else_branch, *span, prefix, opts, locals, imports),
        Expression::Array {
            elements,
            is_matrix,
            span,
        } => Expression::Array {
            elements: qualify_vec_inner(elements, prefix, opts, locals, imports),
            is_matrix: *is_matrix,
            span: *span,
        },
        Expression::Range {
            start, step, end, ..
        } => Expression::Range {
            start: Arc::new(qualify_expr_inner(start, prefix, opts, locals, imports)),
            step: qualify_opt_inner(step, prefix, opts, locals, imports),
            end: Arc::new(qualify_expr_inner(end, prefix, opts, locals, imports)),
            span: expr.span(),
        },
        Expression::Tuple { elements, span } => Expression::Tuple {
            elements: qualify_vec_inner(elements, prefix, opts, locals, imports),
            span: *span,
        },
        Expression::Parenthesized { inner, span } => Expression::Parenthesized {
            inner: Arc::new(qualify_expr_inner(inner, prefix, opts, locals, imports)),
            span: *span,
        },
        Expression::Terminal { .. } | Expression::Empty { .. } => expr.clone(),
        _ => qualify_expr_inner_tail(expr, prefix, opts, locals, imports),
    }
}

fn qualify_expr_inner_tail(
    expr: &Expression,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Expression {
    match expr {
        Expression::ClassModification {
            target,
            modifications,
            span,
            each_flags,
            final_flags,
            redeclare_flags,
        } => Expression::ClassModification {
            target: target.clone(),
            modifications: qualify_vec_inner(modifications, prefix, opts, locals, imports),
            each_flags: each_flags.clone(),
            final_flags: final_flags.clone(),
            redeclare_flags: redeclare_flags.clone(),
            span: *span,
        },
        Expression::NamedArgument { name, value, span } => Expression::NamedArgument {
            name: name.clone(),
            value: Arc::new(qualify_expr_inner(value, prefix, opts, locals, imports)),
            span: *span,
        },
        Expression::Modification {
            target,
            value,
            span,
        } => Expression::Modification {
            target: target.clone(),
            value: Arc::new(qualify_expr_inner(value, prefix, opts, locals, imports)),
            span: *span,
        },
        Expression::ArrayComprehension {
            expr: inner_expr,
            indices,
            filter,
            span,
        } => qualify_array_comprehension_inner(
            inner_expr,
            indices,
            filter,
            *span,
            QualifyExprEnv {
                prefix,
                opts,
                locals,
                imports,
            },
        ),
        Expression::ArrayIndex {
            base,
            subscripts,
            span,
        } => Expression::ArrayIndex {
            base: Arc::new(qualify_expr_inner(base, prefix, opts, locals, imports)),
            subscripts: subscripts
                .iter()
                .map(|s| qualify_sub_inner(s, prefix, opts, locals, imports))
                .collect(),
            span: *span,
        },
        Expression::FieldAccess { base, field, span } => Expression::FieldAccess {
            base: Arc::new(qualify_expr_inner(base, prefix, opts, locals, imports)),
            field: field.clone(),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn qualify_for_indices_inner(
    indices: &[ForIndex],
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> Vec<ForIndex> {
    let mut active_locals = locals.clone();
    let mut qualified_indices = Vec::with_capacity(indices.len());

    // MLS §10.4.1: range expressions are evaluated in lexical order where each
    // index introduces a new local name visible to subsequent ranges.
    for index in indices {
        let qualified_range =
            qualify_expr_inner(&index.range, prefix, opts, &active_locals, imports);
        qualified_indices.push(ForIndex {
            ident: index.ident.clone(),
            range: qualified_range,
        });
        active_locals.insert(index.ident.text.to_string());
    }

    qualified_indices
}

fn qualify_sub_inner(
    sub: &rumoca_ir_ast::Subscript,
    prefix: &QualifiedName,
    opts: QualifyOptions,
    locals: &HashSet<String>,
    imports: &ImportMap,
) -> rumoca_ir_ast::Subscript {
    match sub {
        rumoca_ir_ast::Subscript::Expression(e) => rumoca_ir_ast::Subscript::Expression(
            qualify_expr_inner(e, prefix, opts, locals, imports),
        ),
        rumoca_ir_ast::Subscript::Range { token } => rumoca_ir_ast::Subscript::Range {
            token: token.clone(),
        },
        rumoca_ir_ast::Subscript::Empty => rumoca_ir_ast::Subscript::Empty,
    }
}

#[cfg(test)]
mod tests;
