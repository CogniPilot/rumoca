use rumoca_core::DefId;
use rumoca_core::Location;
use rumoca_ir_ast as ast;
use rustc_hash::FxHashMap;

use super::InstantiateContext;

pub(super) fn register_zero_sized_array_component(
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    name: &str,
    dims: &[i64],
) {
    ctx.push_path(name);
    let parent_path = ctx.current_path().to_string();
    ctx.pop_path();
    overlay.array_parent_dims.insert(parent_path, dims.to_vec());
}

#[derive(Debug, Clone, Default)]
pub(super) struct SourceScopeIndex {
    component_scopes: FxHashMap<DefId, ast::QualifiedName>,
    class_scopes: FxHashMap<DefId, ast::QualifiedName>,
    class_ranges: Vec<SourceScopeRange>,
}

#[derive(Debug, Clone)]
struct SourceScopeRange {
    file_name: String,
    start: u32,
    end: u32,
    scope: ast::QualifiedName,
}

impl SourceScopeIndex {
    pub(super) fn from_tree(tree: &ast::ClassTree) -> Self {
        let mut index = Self::default();
        let mut path = Vec::new();
        index.collect_classes(&tree.definitions.classes, &mut path);
        index
    }

    fn collect_classes(
        &mut self,
        classes: &ast::AstIndexMap<String, ast::ClassDef>,
        path: &mut Vec<(String, Vec<i64>)>,
    ) {
        for class in classes.values() {
            path.push((class.name.text.to_string(), Vec::new()));
            let class_scope = ast::QualifiedName {
                parts: path.clone(),
            };
            if let Some(def_id) = class.def_id {
                self.class_scopes.insert(def_id, class_scope.clone());
            }
            if has_source_range(&class.location) {
                self.class_ranges.push(SourceScopeRange {
                    file_name: class.location.file_name.clone(),
                    start: class.location.start,
                    end: class.location.end,
                    scope: class_scope.clone(),
                });
            }
            self.collect_component_scopes(class, &class_scope);
            self.collect_classes(&class.classes, path);
            let _ = path.pop();
        }
    }

    fn collect_component_scopes(
        &mut self,
        class: &ast::ClassDef,
        class_scope: &ast::QualifiedName,
    ) {
        for component in class.components.values() {
            let Some(def_id) = component.def_id else {
                continue;
            };
            self.component_scopes.insert(def_id, class_scope.clone());
        }
    }

    fn component_scope(&self, comp: &ast::Component) -> Option<ast::QualifiedName> {
        self.component_scopes.get(&comp.def_id?).cloned()
    }

    fn class_scope(&self, class: &ast::ClassDef) -> Option<ast::QualifiedName> {
        self.class_scopes.get(&class.def_id?).cloned()
    }

    fn scope_for_location(&self, location: &Location) -> Option<ast::QualifiedName> {
        if !has_source_range(location) {
            return None;
        }
        self.class_ranges
            .iter()
            .filter(|range| {
                range.file_name == location.file_name
                    && range.start <= location.start
                    && location.end <= range.end
            })
            .min_by_key(|range| range.end.saturating_sub(range.start))
            .map(|range| range.scope.clone())
    }
}

fn has_source_range(location: &Location) -> bool {
    !location.file_name.is_empty() && location.start < location.end
}

pub(super) fn component_declaration_source_scope(
    ctx: &InstantiateContext,
    comp: &ast::Component,
) -> Option<ast::QualifiedName> {
    ctx.source_scope_index.component_scope(comp)
}

pub(super) fn class_declaration_source_scope(
    ctx: &InstantiateContext,
    class: &ast::ClassDef,
) -> Option<ast::QualifiedName> {
    ctx.source_scope_index.class_scope(class)
}

pub(super) fn expression_source_scope(
    ctx: &InstantiateContext,
    expr: &ast::Expression,
) -> Option<ast::QualifiedName> {
    ctx.source_scope_index
        .scope_for_location(expr.get_location()?)
}
