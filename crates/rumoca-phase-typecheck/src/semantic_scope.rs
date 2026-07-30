//! Resolved declaration/instance identity used by type, shape, and variability checks.
//!
//! Component metadata is keyed only by source `DefId` or concrete `InstanceId`.
//! The path indexes in this module are a boundary for AST references that carry
//! only the first segment's `DefId`.
//! Those indexes retain candidate sets and report ambiguity; they never select
//! one component because its rendered name happened to be inserted first.

use rumoca_core::{ComponentPath, DefId, InstanceId, TypeId};
use rumoca_eval_ast::eval::VariabilityLevel;
use rumoca_ir_ast::{
    Component, ComponentRefPart, ComponentReference, Expression, InstanceOverlay, Subscript,
    TerminalType,
};
use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ComponentSemantics {
    pub(crate) type_id: TypeId,
    pub(crate) shape: Option<Vec<usize>>,
    pub(crate) variability: VariabilityLevel,
}

impl ComponentSemantics {
    pub(crate) fn from_declaration(component: &Component) -> Option<(DefId, Self)> {
        Some((
            component.def_id?,
            Self::from_declaration_with_type(component, component.type_id?),
        ))
    }

    pub(crate) fn from_declaration_with_type(component: &Component, type_id: TypeId) -> Self {
        Self {
            type_id,
            shape: component_shape(&component.shape, component.shape_expr.is_empty()),
            variability: VariabilityLevel::from_variability(&component.variability),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SemanticLookup<T> {
    Found(T),
    Missing,
    Ambiguous,
}

impl<T> SemanticLookup<T> {
    pub(crate) fn map<U>(self, f: impl FnOnce(T) -> U) -> SemanticLookup<U> {
        match self {
            Self::Found(value) => SemanticLookup::Found(f(value)),
            Self::Missing => SemanticLookup::Missing,
            Self::Ambiguous => SemanticLookup::Ambiguous,
        }
    }
}

#[derive(Debug, Clone)]
enum CandidateSet {
    Resolved(Vec<InstanceId>),
    Ambiguous,
}

impl CandidateSet {
    fn one(instance_id: InstanceId) -> Self {
        Self::Resolved(vec![instance_id])
    }

    fn ids(&self) -> SemanticLookup<&[InstanceId]> {
        match self {
            Self::Resolved(ids) => SemanticLookup::Found(ids),
            Self::Ambiguous => SemanticLookup::Ambiguous,
        }
    }
}

/// One unresolved member segment after the reference's resolved root `DefId`.
///
/// The AST currently carries identity only for the first component-reference
/// segment. Keeping this boundary typed prevents it from being mistaken for a
/// rendered component path; every matching segment retains all candidate
/// `InstanceId`s and is resolved only by metadata consensus.
#[derive(Debug, Clone, PartialEq, Eq)]
struct UnresolvedMemberSegment(Box<str>);

impl Hash for UnresolvedMemberSegment {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.len().hash(state);
    }
}

impl From<&str> for UnresolvedMemberSegment {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

impl Borrow<str> for UnresolvedMemberSegment {
    fn borrow(&self) -> &str {
        &self.0
    }
}

/// Identity-backed semantic metadata for the currently checked instance tree.
#[derive(Debug, Default)]
pub(crate) struct InstanceSemanticScope {
    by_id: HashMap<InstanceId, ComponentSemantics>,
    path_by_id: HashMap<InstanceId, ComponentPath>,
    terminal_subscripts_by_id: HashMap<InstanceId, Vec<i64>>,
    source_def_by_id: HashMap<InstanceId, DefId>,
    owner_class_by_id: HashMap<InstanceId, InstanceId>,
    exact_paths: HashMap<ComponentPath, CandidateSet>,
    family_paths: HashMap<ComponentPath, Vec<InstanceId>>,
    by_source_def: HashMap<DefId, Vec<InstanceId>>,
    class_children: HashMap<(InstanceId, DefId), Vec<InstanceId>>,
    children_by_def: HashMap<InstanceId, HashMap<DefId, Vec<InstanceId>>>,
    children_by_name: HashMap<InstanceId, HashMap<UnresolvedMemberSegment, Vec<InstanceId>>>,
    family_shape_by_owner_def: HashMap<(InstanceId, DefId), Vec<usize>>,
    class_path_ids: HashMap<ComponentPath, CandidateSet>,
}

impl InstanceSemanticScope {
    pub(crate) fn from_overlay(overlay: &InstanceOverlay) -> Self {
        let mut scope = Self::default();
        scope.index_classes(overlay);
        scope.index_components(overlay);
        scope.index_relationships(overlay);
        scope.index_component_family_shapes(overlay);
        scope
    }

    pub(crate) fn lookup_reference(
        &self,
        reference: &ComponentReference,
        prefix_len: usize,
        class_instance_id: Option<InstanceId>,
        current_scope: Option<&ComponentPath>,
    ) -> SemanticLookup<ComponentSemantics> {
        let candidates =
            self.resolve_reference(reference, prefix_len, class_instance_id, current_scope);
        self.consensus(candidates)
    }

    pub(crate) fn lookup_declaration(
        &self,
        class_instance_id: InstanceId,
        declaration: DefId,
    ) -> SemanticLookup<ComponentSemantics> {
        self.consensus(
            self.class_children
                .get(&(class_instance_id, declaration))
                .cloned()
                .map_or(SemanticLookup::Missing, SemanticLookup::Found),
        )
    }

    pub(crate) fn lookup_expression(
        &self,
        expression: &Expression,
        class_instance_id: Option<InstanceId>,
        current_scope: Option<&ComponentPath>,
    ) -> SemanticLookup<ComponentSemantics> {
        let candidates = self.resolve_expression(expression, class_instance_id, current_scope);
        self.consensus(candidates)
    }

    pub(crate) fn lookup_reference_shape(
        &self,
        reference: &ComponentReference,
        prefix_len: usize,
        class_instance_id: Option<InstanceId>,
        current_scope: Option<&ComponentPath>,
    ) -> SemanticLookup<Option<Vec<usize>>> {
        match self.resolve_reference_by_identity(reference, prefix_len, class_instance_id) {
            SemanticLookup::Found(ids) => {
                let shape = self.identity_shape(&ids);
                if !matches!(shape, SemanticLookup::Missing) {
                    return shape;
                }
            }
            SemanticLookup::Ambiguous => return SemanticLookup::Ambiguous,
            SemanticLookup::Missing
                if class_instance_id.is_some()
                    && reference
                        .parts
                        .iter()
                        .take(prefix_len)
                        .all(|part| part.def_id.is_some()) =>
            {
                return SemanticLookup::Missing;
            }
            SemanticLookup::Missing => {}
        }
        let path = component_reference_prefix_path(reference, prefix_len, false);
        // Search one lexical candidate at a time. An exact local scalar must
        // stop the search before a same-named array in an enclosing instance
        // can contribute its domain.
        for candidate in scoped_path_candidates(&path, current_scope) {
            match self.exact_paths.get(&candidate).map(CandidateSet::ids) {
                Some(SemanticLookup::Found(ids)) => {
                    return self
                        .consensus(SemanticLookup::Found(ids.to_vec()))
                        .map(|semantics| semantics.shape);
                }
                Some(SemanticLookup::Ambiguous) => return SemanticLookup::Ambiguous,
                _ => {}
            }
        }
        self.lookup_reference(reference, prefix_len, class_instance_id, current_scope)
            .map(|semantics| semantics.shape)
    }

    fn index_classes(&mut self, overlay: &InstanceOverlay) {
        for (&map_id, data) in &overlay.classes {
            insert_candidate(
                &mut self.class_path_ids,
                data.qualified_name.to_component_path(),
                map_id,
            );
        }
    }

    fn index_components(&mut self, overlay: &InstanceOverlay) {
        for (&map_id, data) in &overlay.components {
            let semantics = ComponentSemantics {
                type_id: data.type_id,
                shape: component_shape(&data.dims, data.dims_expr.is_empty()),
                variability: VariabilityLevel::from_variability(&data.variability),
            };
            let path = data.qualified_name.to_component_path();
            let terminal_subscripts = terminal_instance_subscripts(&data.qualified_name);
            self.by_id.insert(map_id, semantics);
            self.path_by_id.insert(map_id, path.clone());
            self.terminal_subscripts_by_id
                .insert(map_id, terminal_subscripts);
            if let Some(owner_class_id) = data.owner_class_id {
                self.owner_class_by_id.insert(map_id, owner_class_id);
            }
            insert_candidate(&mut self.exact_paths, path.clone(), map_id);
            let family_path = ComponentPath::from_parts(
                data.qualified_name
                    .parts
                    .iter()
                    .map(|(name, _)| rumoca_core::strip_all_subscripts(name)),
            );
            let family = self.family_paths.entry(family_path).or_default();
            if !family.contains(&map_id) {
                family.push(map_id);
            }
            if let Some(def_id) = data
                .component_ref
                .as_ref()
                .map(|reference| reference.target_def_id())
            {
                self.source_def_by_id.insert(map_id, def_id);
                self.by_source_def.entry(def_id).or_default().push(map_id);
            }
        }
    }

    fn index_relationships(&mut self, overlay: &InstanceOverlay) {
        let class_owner_components = overlay
            .classes
            .iter()
            .filter_map(|(&class_id, class)| {
                class
                    .owner_component_id
                    .map(|component_id| (class_id, component_id))
            })
            .collect::<HashMap<_, _>>();
        for (&instance_id, data) in &overlay.components {
            let path = data.qualified_name.to_component_path();
            let Some(parent_path) = path.parent() else {
                continue;
            };
            let source_def_id = data
                .component_ref
                .as_ref()
                .map(|reference| reference.target_def_id());
            if let (Some(class_id), Some(def_id)) = (data.owner_class_id, source_def_id) {
                self.class_children
                    .entry((class_id, def_id))
                    .or_default()
                    .push(instance_id);
            }
            if let (Some(def_id), Some(parent_component_id)) = (
                source_def_id,
                data.owner_class_id
                    .and_then(|class_id| class_owner_components.get(&class_id)),
            ) {
                self.children_by_def
                    .entry(*parent_component_id)
                    .or_default()
                    .entry(def_id)
                    .or_default()
                    .push(instance_id);
            }
            if data.owner_class_id.is_none()
                && let Some(class_id) = unique_candidate_id(self.class_path_ids.get(&parent_path))
                && let Some(def_id) = source_def_id
            {
                self.class_children
                    .entry((class_id, def_id))
                    .or_default()
                    .push(instance_id);
            }
            if let Some(parent_id) = unique_candidate_id(self.exact_paths.get(&parent_path))
                && let Some((name, _)) = data.qualified_name.parts.last()
            {
                self.children_by_name
                    .entry(parent_id)
                    .or_default()
                    .entry(name.as_str().into())
                    .or_default()
                    .push(instance_id);
            }
        }
    }

    fn index_component_family_shapes(&mut self, overlay: &InstanceOverlay) {
        for family in &overlay.component_families {
            let Some(root_id) = family.template_components.first().copied() else {
                continue;
            };
            let (Some(owner_class_id), Some(def_id)) = (
                self.owner_class_by_id.get(&root_id).copied(),
                self.source_def_by_id.get(&root_id).copied(),
            ) else {
                continue;
            };
            if let Ok(shape) = family.domain.extents() {
                self.family_shape_by_owner_def
                    .insert((owner_class_id, def_id), shape);
            }
        }
    }

    fn resolve_expression(
        &self,
        expression: &Expression,
        class_instance_id: Option<InstanceId>,
        current_scope: Option<&ComponentPath>,
    ) -> SemanticLookup<Vec<InstanceId>> {
        match expression {
            Expression::ComponentReference(reference) => self.resolve_reference(
                reference,
                reference.parts.len(),
                class_instance_id,
                current_scope,
            ),
            Expression::FieldAccess { base, field, .. } => {
                let parents = self.resolve_expression(base, class_instance_id, current_scope);
                self.resolve_named_children(parents, field, None)
            }
            Expression::ArrayIndex {
                base, subscripts, ..
            } => {
                let candidates = self.resolve_expression(base, class_instance_id, current_scope);
                filter_candidates_by_subscripts(
                    candidates,
                    subscripts,
                    &self.terminal_subscripts_by_id,
                )
            }
            Expression::Parenthesized { inner, .. } => {
                self.resolve_expression(inner, class_instance_id, current_scope)
            }
            _ => SemanticLookup::Missing,
        }
    }

    fn resolve_reference(
        &self,
        reference: &ComponentReference,
        prefix_len: usize,
        class_instance_id: Option<InstanceId>,
        current_scope: Option<&ComponentPath>,
    ) -> SemanticLookup<Vec<InstanceId>> {
        let exact = self.resolve_reference_by_identity(reference, prefix_len, class_instance_id);
        match exact {
            SemanticLookup::Found(_) | SemanticLookup::Ambiguous => return exact,
            SemanticLookup::Missing
                if class_instance_id.is_some()
                    && reference
                        .parts
                        .iter()
                        .take(prefix_len)
                        .all(|part| part.def_id.is_some()) =>
            {
                return SemanticLookup::Missing;
            }
            SemanticLookup::Missing => {}
        }
        if prefix_len == 0 || prefix_len > reference.parts.len() {
            return SemanticLookup::Missing;
        }
        let first = &reference.parts[0];
        let mut candidates = self.resolve_first_part(
            reference.root_def_id(),
            first,
            class_instance_id,
            current_scope,
        );
        for part in reference.parts.iter().take(prefix_len).skip(1) {
            candidates =
                self.resolve_named_children(candidates, part.ident.text.as_ref(), Some(part));
        }
        if matches!(candidates, SemanticLookup::Missing) {
            let path = component_reference_prefix_path(reference, prefix_len, true);
            return self.lookup_scoped_path(&path, current_scope);
        }
        candidates
    }

    fn resolve_reference_by_identity(
        &self,
        reference: &ComponentReference,
        prefix_len: usize,
        class_instance_id: Option<InstanceId>,
    ) -> SemanticLookup<Vec<InstanceId>> {
        if prefix_len == 0 || prefix_len > reference.parts.len() {
            return SemanticLookup::Missing;
        }
        let Some(root_def_id) = reference.parts.first().and_then(|part| part.def_id) else {
            return SemanticLookup::Missing;
        };
        let Some(class_instance_id) = class_instance_id else {
            return SemanticLookup::Missing;
        };
        let Some(root) = self.class_children.get(&(class_instance_id, root_def_id)) else {
            return SemanticLookup::Missing;
        };
        let roots = filter_candidates_by_part(
            root.clone(),
            &reference.parts[0],
            &self.terminal_subscripts_by_id,
        );
        let SemanticLookup::Found(roots) = roots else {
            return roots;
        };
        if prefix_len == 1 {
            return SemanticLookup::Found(roots);
        }
        let mut candidates = SemanticLookup::Found(roots);
        for part in reference.parts.iter().take(prefix_len).skip(1) {
            let Some(member_def_id) = part.def_id else {
                return SemanticLookup::Missing;
            };
            candidates = self.resolve_children_by_def(candidates, member_def_id);
            let SemanticLookup::Found(ids) = candidates else {
                return candidates;
            };
            candidates = filter_candidates_by_part(ids, part, &self.terminal_subscripts_by_id);
        }
        candidates
    }

    fn resolve_children_by_def(
        &self,
        parents: SemanticLookup<Vec<InstanceId>>,
        member_def_id: DefId,
    ) -> SemanticLookup<Vec<InstanceId>> {
        let SemanticLookup::Found(parents) = parents else {
            return parents;
        };
        let children = parents
            .iter()
            .filter_map(|parent| self.children_by_def.get(parent))
            .filter_map(|children| children.get(&member_def_id))
            .flatten()
            .copied()
            .collect::<Vec<_>>();
        if children.is_empty() {
            SemanticLookup::Missing
        } else {
            SemanticLookup::Found(children)
        }
    }

    fn identity_shape(&self, ids: &[InstanceId]) -> SemanticLookup<Option<Vec<usize>>> {
        let family_shapes = ids
            .iter()
            .filter_map(|id| {
                let key = (
                    *self.owner_class_by_id.get(id)?,
                    *self.source_def_by_id.get(id)?,
                );
                self.family_shape_by_owner_def.get(&key)
            })
            .collect::<std::collections::HashSet<_>>();
        if family_shapes.len() == 1 {
            return SemanticLookup::Found(Some(
                family_shapes
                    .into_iter()
                    .next()
                    .expect("one family shape")
                    .clone(),
            ));
        }
        if family_shapes.len() > 1 {
            return SemanticLookup::Ambiguous;
        }
        self.consensus(SemanticLookup::Found(ids.to_vec()))
            .map(|semantics| semantics.shape)
    }

    fn resolve_first_part(
        &self,
        def_id: Option<DefId>,
        part: &ComponentRefPart,
        class_instance_id: Option<InstanceId>,
        current_scope: Option<&ComponentPath>,
    ) -> SemanticLookup<Vec<InstanceId>> {
        if let (Some(class_id), Some(def_id)) = (class_instance_id, def_id)
            && let Some(ids) = self.class_children.get(&(class_id, def_id))
        {
            return filter_candidates_by_part(ids.clone(), part, &self.terminal_subscripts_by_id);
        }
        if let Some(def_id) = def_id
            && let Some(ids) = self.by_source_def.get(&def_id)
            && let Some(nearest) = self.nearest_parent_candidates(ids, current_scope)
        {
            return filter_candidates_by_part(nearest, part, &self.terminal_subscripts_by_id);
        }
        let path = ComponentPath::from_parts([part.to_string()]);
        match self.lookup_scoped_path(&path, current_scope) {
            SemanticLookup::Missing if part.subs.is_some() => {
                let owner_path = ComponentPath::from_parts([part.ident.text.to_string()]);
                self.lookup_scoped_path(&owner_path, current_scope)
            }
            result => result,
        }
    }

    fn resolve_named_children(
        &self,
        parents: SemanticLookup<Vec<InstanceId>>,
        name: &str,
        part: Option<&ComponentRefPart>,
    ) -> SemanticLookup<Vec<InstanceId>> {
        let SemanticLookup::Found(parents) = parents else {
            return parents;
        };
        let mut children = Vec::new();
        for parent in parents {
            let Some(named) = self
                .children_by_name
                .get(&parent)
                .and_then(|children| children.get(name))
            else {
                continue;
            };
            children.extend(named.iter().copied());
        }
        if children.is_empty() {
            return SemanticLookup::Missing;
        }
        match part {
            Some(part) => {
                filter_candidates_by_part(children, part, &self.terminal_subscripts_by_id)
            }
            None => SemanticLookup::Found(children),
        }
    }

    fn nearest_parent_candidates(
        &self,
        ids: &[InstanceId],
        current_scope: Option<&ComponentPath>,
    ) -> Option<Vec<InstanceId>> {
        let mut scope = current_scope.cloned().unwrap_or_else(ComponentPath::root);
        loop {
            let matches = ids
                .iter()
                .copied()
                .filter(|instance_id| {
                    self.path_by_id
                        .get(instance_id)
                        .and_then(ComponentPath::parent)
                        .is_some_and(|parent| parent == scope)
                })
                .collect::<Vec<_>>();
            if !matches.is_empty() {
                return Some(matches);
            }
            let Some(parent) = scope.parent() else {
                break;
            };
            scope = parent;
        }
        None
    }

    fn lookup_scoped_path(
        &self,
        path: &ComponentPath,
        current_scope: Option<&ComponentPath>,
    ) -> SemanticLookup<Vec<InstanceId>> {
        for candidate in scoped_path_candidates(path, current_scope) {
            match self.exact_paths.get(&candidate).map(CandidateSet::ids) {
                Some(SemanticLookup::Found(ids)) => {
                    return SemanticLookup::Found(ids.to_vec());
                }
                Some(SemanticLookup::Ambiguous) => return SemanticLookup::Ambiguous,
                _ => {}
            }
            if let Some(ids) = self.family_paths.get(&candidate) {
                return SemanticLookup::Found(ids.clone());
            }
        }
        SemanticLookup::Missing
    }

    fn consensus(
        &self,
        candidates: SemanticLookup<Vec<InstanceId>>,
    ) -> SemanticLookup<ComponentSemantics> {
        let ids = match candidates {
            SemanticLookup::Found(ids) => ids,
            SemanticLookup::Missing => return SemanticLookup::Missing,
            SemanticLookup::Ambiguous => return SemanticLookup::Ambiguous,
        };
        let mut semantics = ids
            .iter()
            .filter_map(|instance_id| self.by_id.get(instance_id));
        let Some(first) = semantics.next() else {
            return SemanticLookup::Missing;
        };
        if semantics.all(|candidate| candidate == first) {
            SemanticLookup::Found(first.clone())
        } else {
            SemanticLookup::Ambiguous
        }
    }
}

fn component_shape<T>(dimensions: &[T], has_no_dimension_expressions: bool) -> Option<Vec<usize>>
where
    T: Copy + TryInto<usize>,
{
    if dimensions.is_empty() {
        return has_no_dimension_expressions.then(Vec::new);
    }
    dimensions
        .iter()
        .copied()
        .map(TryInto::try_into)
        .collect::<Result<Vec<_>, _>>()
        .ok()
}

fn terminal_instance_subscripts(qualified_name: &rumoca_ir_ast::QualifiedName) -> Vec<i64> {
    let Some((_, subscripts)) = qualified_name.parts.last() else {
        return Vec::new();
    };
    subscripts.clone()
}

fn insert_candidate(
    index: &mut HashMap<ComponentPath, CandidateSet>,
    path: ComponentPath,
    instance_id: InstanceId,
) {
    match index.entry(path) {
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(CandidateSet::one(instance_id));
        }
        std::collections::hash_map::Entry::Occupied(mut entry) => match entry.get_mut() {
            CandidateSet::Resolved(ids) if !ids.contains(&instance_id) => {
                *entry.get_mut() = CandidateSet::Ambiguous;
            }
            _ => {}
        },
    }
}

fn unique_candidate_id(candidates: Option<&CandidateSet>) -> Option<InstanceId> {
    let CandidateSet::Resolved(ids) = candidates? else {
        return None;
    };
    (ids.len() == 1).then_some(ids[0])
}

fn scoped_path_candidates(
    path: &ComponentPath,
    current_scope: Option<&ComponentPath>,
) -> Vec<ComponentPath> {
    let mut candidates = Vec::new();
    let mut scope = current_scope.cloned();
    while let Some(current) = scope {
        let candidate = current.join(path);
        if !candidates.contains(&candidate) {
            candidates.push(candidate);
        }
        scope = current.parent();
    }
    if !candidates.contains(path) {
        candidates.push(path.clone());
    }
    candidates
}

fn component_reference_prefix_path(
    reference: &ComponentReference,
    prefix_len: usize,
    retain_subscripts: bool,
) -> ComponentPath {
    ComponentPath::from_parts(reference.parts.iter().take(prefix_len).map(|part| {
        if retain_subscripts {
            part.to_string()
        } else {
            part.ident.text.to_string()
        }
    }))
}

fn filter_candidates_by_part(
    candidates: Vec<InstanceId>,
    part: &ComponentRefPart,
    terminal_subscripts: &HashMap<InstanceId, Vec<i64>>,
) -> SemanticLookup<Vec<InstanceId>> {
    let Some(expected) = literal_subscripts(part.subs.as_deref()) else {
        return SemanticLookup::Found(candidates);
    };
    let has_expanded_candidates = candidates.iter().any(|instance_id| {
        terminal_subscripts
            .get(instance_id)
            .is_some_and(|subscripts| !subscripts.is_empty())
    });
    if !has_expanded_candidates {
        // The component remains compact in the overlay. Source subscripts
        // select its value domain, not a different component instance.
        return SemanticLookup::Found(candidates);
    }
    let filtered = candidates
        .into_iter()
        .filter(|instance_id| terminal_subscripts.get(instance_id) == Some(&expected))
        .collect::<Vec<_>>();
    if filtered.is_empty() {
        SemanticLookup::Missing
    } else {
        SemanticLookup::Found(filtered)
    }
}

fn filter_candidates_by_subscripts(
    candidates: SemanticLookup<Vec<InstanceId>>,
    subscripts: &[Subscript],
    terminal_subscripts: &HashMap<InstanceId, Vec<i64>>,
) -> SemanticLookup<Vec<InstanceId>> {
    let SemanticLookup::Found(candidates) = candidates else {
        return candidates;
    };
    let Some(expected) = literal_subscripts(Some(subscripts)) else {
        return SemanticLookup::Found(candidates);
    };
    let has_expanded_candidates = candidates.iter().any(|instance_id| {
        terminal_subscripts
            .get(instance_id)
            .is_some_and(|subscripts| !subscripts.is_empty())
    });
    if !has_expanded_candidates {
        return SemanticLookup::Found(candidates);
    }
    let filtered = candidates
        .into_iter()
        .filter(|instance_id| terminal_subscripts.get(instance_id) == Some(&expected))
        .collect::<Vec<_>>();
    if filtered.is_empty() {
        SemanticLookup::Missing
    } else {
        SemanticLookup::Found(filtered)
    }
}

fn literal_subscripts(subscripts: Option<&[Subscript]>) -> Option<Vec<i64>> {
    let subscripts = subscripts?;
    subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Expression(expression) => literal_integer(expression),
            Subscript::Range { .. } | Subscript::Empty => None,
        })
        .collect()
}

fn literal_integer(expression: &Expression) -> Option<i64> {
    let Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token,
        ..
    } = expression
    else {
        return None;
    };
    token.text.parse().ok()
}
