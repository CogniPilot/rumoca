use indexmap::IndexMap;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::collections::HashSet;
use std::sync::Arc;

use crate::InstantiateResult;
use crate::inheritance::{
    InheritanceCache, get_effective_components_with_cache, get_effective_equations_with_cache,
    process_extends_with_cache,
};

/// Cached class template for efficient instantiation of multiple instances.
///
/// When a class like `Resistor` is instantiated multiple times (e.g., `Resistor r[100]`
/// or multiple `Resistor` components), we cache the structural template and only
/// apply per-instance modifications.
#[derive(Debug, Clone)]
pub struct ClassTemplate {
    /// Effective components after inheritance resolution (inherited + own).
    pub effective_components: IndexMap<String, ast::Component>,
    /// Effective equations after inheritance resolution (inherited + own).
    pub effective_equations: Vec<ast::Equation>,
    /// Initial equations (inherited + own).
    pub initial_equations: Vec<ast::Equation>,
    /// Algorithm sections (inherited + own).
    pub algorithms: Vec<Vec<ast::Statement>>,
    /// Initial algorithm sections (inherited + own).
    pub initial_algorithms: Vec<Vec<ast::Statement>>,
    /// Resolved import map: short name → FQN, from class + inheritance chain.
    pub resolved_imports: Vec<(String, String)>,
}

/// Cache for class templates, keyed by DefId.
pub type ClassTemplateCache = IndexMap<DefId, Arc<ClassTemplate>>;

fn cached_template_for_def_id(
    class_def_id: Option<DefId>,
    cache: &ClassTemplateCache,
) -> Option<Arc<ClassTemplate>> {
    class_def_id.and_then(|def_id| cache.get(&def_id).map(Arc::clone))
}

fn store_cached_template(
    class_def_id: Option<DefId>,
    cache: &mut ClassTemplateCache,
    template: &Arc<ClassTemplate>,
) {
    if let Some(def_id) = class_def_id {
        cache.insert(def_id, Arc::clone(template));
    }
}

/// Get or compute the class template for a class.
///
/// If the class has a DefId and is already cached, returns the cached template.
/// Otherwise, computes the template (inheritance resolution, effective components/equations)
/// and caches it for future use.
pub(crate) fn get_or_compute_template(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    template_cache: &mut ClassTemplateCache,
) -> InstantiateResult<Arc<ClassTemplate>> {
    if let Some(cached) = cached_template_for_def_id(class.def_id, template_cache) {
        return Ok(cached);
    }

    let mut inheritance_cache = InheritanceCache::new();
    let inherited = process_extends_with_cache(tree, class, &mut inheritance_cache)?;
    let effective_components =
        get_effective_components_with_cache(tree, class, &mut inheritance_cache)?;
    let effective_equations =
        get_effective_equations_with_cache(tree, class, &mut inheritance_cache)?;

    let mut initial_equations = inherited.initial_equations;
    initial_equations.extend(class.initial_equations.clone());

    let mut algorithms = inherited.algorithms;
    algorithms.extend(class.algorithms.clone());

    let mut initial_algorithms = inherited.initial_algorithms;
    initial_algorithms.extend(class.initial_algorithms.clone());

    let resolved_imports = collect_all_imports(tree, class);

    let template = Arc::new(ClassTemplate {
        effective_components,
        effective_equations,
        initial_equations,
        algorithms,
        initial_algorithms,
        resolved_imports,
    });

    store_cached_template(class.def_id, template_cache, &template);
    Ok(template)
}

/// Collect resolved imports (short name → FQN) from a class and its inheritance chain.
///
/// MLS §13.2: ast::Import clauses are local to the class they appear in and are NOT
/// inherited. However, effective equations include equations from base classes
/// that use base-class imports. We collect imports from the entire chain so
/// those short names can be resolved during qualification.
pub(crate) fn collect_all_imports(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
) -> Vec<(String, String)> {
    let mut pairs = Vec::new();
    if let Some(class_name) = class.def_id.and_then(|def_id| tree.def_map.get(&def_id)) {
        collect_lexical_ancestor_imports(tree, class_name, &mut pairs);
    }
    let mut visited = HashSet::new();
    collect_imports_recursive(tree, class, &mut pairs, &mut visited);
    pairs
}

fn collect_lexical_ancestor_imports(
    tree: &ast::ClassTree,
    class_name: &str,
    pairs: &mut Vec<(String, String)>,
) {
    let mut ancestors = Vec::new();
    let mut scope = class_name;
    while let Some((parent, _)) = scope.rsplit_once('.') {
        ancestors.push(parent.to_string());
        scope = parent;
    }
    ancestors.reverse();
    for ancestor in ancestors {
        let Some(ancestor_class) = tree.get_class_by_qualified_name(&ancestor) else {
            continue;
        };
        resolve_import_pairs(&ancestor_class.imports, tree, pairs);
    }
}

fn collect_imports_recursive(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    pairs: &mut Vec<(String, String)>,
    visited: &mut HashSet<DefId>,
) {
    if let Some(def_id) = class.def_id
        && !visited.insert(def_id)
    {
        return;
    }
    for ext in &class.extends {
        let base_name = ext.base_name.to_string();
        let base_class = ext
            .base_def_id
            .and_then(|id| tree.get_class_by_def_id(id))
            .or_else(|| tree.get_class_by_qualified_name(&base_name));
        if let Some(base) = base_class {
            collect_imports_recursive(tree, base, pairs, visited);
        }
    }
    resolve_import_pairs(&class.imports, tree, pairs);
}

/// Resolve AST import declarations to (short_name, fqn) pairs.
fn resolve_import_pairs(
    imports: &[ast::Import],
    tree: &ast::ClassTree,
    pairs: &mut Vec<(String, String)>,
) {
    for import in imports {
        resolve_single_import(import, tree, pairs);
    }
}

fn resolve_single_import(
    import: &ast::Import,
    tree: &ast::ClassTree,
    pairs: &mut Vec<(String, String)>,
) {
    match import {
        ast::Import::Qualified { path, .. } => {
            let fqn = path.to_string();
            if let Some(short) = fqn.rsplit('.').next() {
                pairs.push((short.to_string(), fqn));
            }
        }
        ast::Import::Renamed { alias, path, .. } => {
            pairs.push((alias.text.to_string(), path.to_string()));
        }
        ast::Import::Unqualified { path, .. } => {
            resolve_unqualified_import(path, tree, pairs);
        }
        ast::Import::Selective { path, names, .. } => {
            let pkg_name = path.to_string();
            for name_tok in names {
                let name = name_tok.text.to_string();
                pairs.push((name.clone(), format!("{pkg_name}.{name}")));
            }
        }
    }
}

fn resolve_unqualified_import(
    path: &rumoca_ir_ast::Name,
    tree: &ast::ClassTree,
    pairs: &mut Vec<(String, String)>,
) {
    let pkg_name = path.to_string();
    let Some(class_def) = tree.get_class_by_qualified_name(&pkg_name) else {
        return;
    };
    for name in class_def.components.keys() {
        pairs.push((name.clone(), format!("{pkg_name}.{name}")));
    }
    // Also include nested classes (for enum types, etc.)
    for name in class_def.classes.keys() {
        pairs.push((name.clone(), format!("{pkg_name}.{name}")));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_template() -> Arc<ClassTemplate> {
        Arc::new(ClassTemplate {
            effective_components: IndexMap::new(),
            effective_equations: Vec::new(),
            initial_equations: Vec::new(),
            algorithms: Vec::new(),
            initial_algorithms: Vec::new(),
            resolved_imports: Vec::new(),
        })
    }

    #[test]
    fn test_store_cached_template_requires_def_id() {
        let mut cache = ClassTemplateCache::new();
        let template = empty_template();

        store_cached_template(None, &mut cache, &template);

        assert!(
            cache.is_empty(),
            "templates without DefId must not be cached"
        );
    }

    #[test]
    fn test_cached_template_roundtrip_returns_same_arc() {
        let mut cache = ClassTemplateCache::new();
        let template = empty_template();
        let def_id = DefId(42);

        store_cached_template(Some(def_id), &mut cache, &template);
        let retrieved = cached_template_for_def_id(Some(def_id), &cache)
            .expect("template should be present in cache");

        assert!(
            Arc::ptr_eq(&template, &retrieved),
            "cache should return the original shared Arc"
        );
    }
}
