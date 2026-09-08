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
    let parent_path = ctx.current_path().to_component_path();
    ctx.pop_path();
    overlay.array_parent_dims.insert(parent_path, dims.to_vec());
}

#[derive(Debug, Clone, Default)]
pub(super) struct SourceScopeIndex {
    component_scopes: FxHashMap<DefId, ComponentDeclarationScope>,
    class_scopes: FxHashMap<DefId, ast::QualifiedName>,
    class_ranges: FxHashMap<rumoca_core::SourceId, SourceScopeFile>,
}

/// The declaring class of one component, as both its qualified name and its
/// resolved lexical scope.
///
/// For an inherited component this names the base class that wrote the
/// declaration, so import-sensitive projections of the declaration use the
/// base class's own imports (MLS §13.2: imports are not inherited).
#[derive(Debug, Clone)]
struct ComponentDeclarationScope {
    scope: ast::QualifiedName,
    scope_id: Option<rumoca_core::ScopeId>,
}

#[derive(Debug, Clone)]
struct SourceScopeRange {
    start: u32,
    end: u32,
    scope: ast::QualifiedName,
    scope_id: Option<rumoca_core::ScopeId>,
    children: Vec<usize>,
}

#[derive(Debug, Clone, Default)]
struct SourceScopeFile {
    ranges: Vec<SourceScopeRange>,
    roots: Vec<usize>,
}

impl SourceScopeIndex {
    pub(super) fn from_tree(tree: &ast::ClassTree) -> Self {
        let mut index = Self::default();
        let mut path = Vec::new();
        index.collect_classes(&tree.definitions.classes, &mut path);
        for file in index.class_ranges.values_mut() {
            file.construct_interval_hierarchy();
        }
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
                self.class_ranges
                    .entry(class.location.source)
                    .or_default()
                    .ranges
                    .push(SourceScopeRange {
                        start: class.location.start,
                        end: class.location.end,
                        scope: class_scope.clone(),
                        scope_id: class.scope_id,
                        children: Vec::new(),
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
            self.component_scopes.insert(
                def_id,
                ComponentDeclarationScope {
                    scope: class_scope.clone(),
                    scope_id: class.scope_id,
                },
            );
        }
    }

    fn component_scope(&self, comp: &ast::Component) -> Option<ast::QualifiedName> {
        self.component_scopes
            .get(&comp.def_id?)
            .map(|declaration| declaration.scope.clone())
    }

    fn component_scope_id(&self, comp: &ast::Component) -> Option<rumoca_core::ScopeId> {
        self.component_scopes
            .get(&comp.def_id?)
            .and_then(|declaration| declaration.scope_id)
    }

    fn class_scope(&self, class: &ast::ClassDef) -> Option<ast::QualifiedName> {
        self.class_scopes.get(&class.def_id?).cloned()
    }

    fn scope_for_location(
        &self,
        location: &Location,
    ) -> Option<(ast::QualifiedName, Option<rumoca_core::ScopeId>)> {
        if !has_source_range(location) {
            return None;
        }
        self.class_ranges
            .get(&location.source)?
            .scope_for(location)
            .map(|range| (range.scope.clone(), range.scope_id))
    }
}

impl SourceScopeFile {
    fn construct_interval_hierarchy(&mut self) {
        self.ranges
            .sort_by_key(|range| (range.start, std::cmp::Reverse(range.end)));
        let mut ancestors = Vec::<usize>::new();
        for index in 0..self.ranges.len() {
            let start = self.ranges[index].start;
            let end = self.ranges[index].end;
            while ancestors.last().is_some_and(|ancestor| {
                let range = &self.ranges[*ancestor];
                !(range.start <= start && end <= range.end)
            }) {
                let _ = ancestors.pop();
            }
            if let Some(parent) = ancestors.last().copied() {
                self.ranges[parent].children.push(index);
            } else {
                self.roots.push(index);
            }
            ancestors.push(index);
        }
    }

    fn scope_for(&self, location: &Location) -> Option<&SourceScopeRange> {
        let mut current = containing_interval(&self.ranges, &self.roots, location)?;
        loop {
            let Some(child) =
                containing_interval(&self.ranges, &self.ranges[current].children, location)
            else {
                return Some(&self.ranges[current]);
            };
            current = child;
        }
    }
}

fn containing_interval(
    ranges: &[SourceScopeRange],
    candidates: &[usize],
    location: &Location,
) -> Option<usize> {
    let end = candidates.partition_point(|index| ranges[*index].start <= location.start);
    let candidate = *candidates.get(end.checked_sub(1)?)?;
    (location.end <= ranges[candidate].end).then_some(candidate)
}

fn has_source_range(location: &Location) -> bool {
    location.has_source()
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
) -> Option<(ast::QualifiedName, Option<rumoca_core::ScopeId>)> {
    ctx.source_scope_index
        .scope_for_location(expr.get_location()?)
}

/// Effective import bindings of the class that declared `comp` (MLS §13.2).
///
/// Returns `None` when the component carries no resolved declaring scope,
/// which only synthetic components without a source declaration do.
pub(super) fn component_effective_imports(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    comp: &ast::Component,
) -> Option<ast::EffectiveImports> {
    let scope_id = ctx.source_scope_index.component_scope_id(comp)?;
    Some(tree.effective_imports(scope_id))
}

pub(super) fn location_source_scope(
    ctx: &InstantiateContext,
    location: Option<&Location>,
) -> Option<(ast::QualifiedName, Option<rumoca_core::ScopeId>)> {
    ctx.source_scope_index.scope_for_location(location?)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn range(start: u32, end: u32, name: &str, scope: u32) -> SourceScopeRange {
        SourceScopeRange {
            start,
            end,
            scope: ast::QualifiedName::from_dotted(name),
            scope_id: Some(rumoca_core::ScopeId::new(scope)),
            children: Vec::new(),
        }
    }

    fn location(start: u32, end: u32) -> Location {
        Location {
            source: rumoca_core::SourceId::from_source_name("nested.mo"),
            start,
            end,
            ..Default::default()
        }
    }

    #[test]
    fn interval_hierarchy_returns_one_consistent_innermost_scope() {
        let mut file = SourceScopeFile {
            ranges: vec![
                range(110, 150, "Sibling", 4),
                range(0, 100, "Outer", 1),
                range(10, 90, "Outer.Inner", 2),
                range(20, 30, "Outer.Inner.Deep", 3),
            ],
            roots: Vec::new(),
        };
        file.construct_interval_hierarchy();

        let deep = file.scope_for(&location(25, 26)).expect("deep scope");
        assert_eq!(deep.scope.to_flat_string(), "Outer.Inner.Deep");
        assert_eq!(deep.scope_id, Some(rumoca_core::ScopeId::new(3)));

        let sibling = file.scope_for(&location(120, 121)).expect("sibling scope");
        assert_eq!(sibling.scope.to_flat_string(), "Sibling");
        assert_eq!(sibling.scope_id, Some(rumoca_core::ScopeId::new(4)));
        assert!(file.scope_for(&location(105, 106)).is_none());
    }
}
