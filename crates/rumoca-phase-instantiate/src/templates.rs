use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
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

    let mut inheritance_cache = InheritanceCache::default();
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

    let template = Arc::new(ClassTemplate {
        effective_components,
        effective_equations,
        initial_equations,
        algorithms,
        initial_algorithms,
    });

    store_cached_template(class.def_id, template_cache, &template);
    Ok(template)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_template() -> Arc<ClassTemplate> {
        Arc::new(ClassTemplate {
            effective_components: IndexMap::default(),
            effective_equations: Vec::new(),
            initial_equations: Vec::new(),
            algorithms: Vec::new(),
            initial_algorithms: Vec::new(),
        })
    }

    #[test]
    fn test_store_cached_template_requires_def_id() {
        let mut cache = ClassTemplateCache::default();
        let template = empty_template();

        store_cached_template(None, &mut cache, &template);

        assert!(
            cache.is_empty(),
            "templates without DefId must not be cached"
        );
    }

    #[test]
    fn test_cached_template_roundtrip_returns_same_arc() {
        let mut cache = ClassTemplateCache::default();
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
