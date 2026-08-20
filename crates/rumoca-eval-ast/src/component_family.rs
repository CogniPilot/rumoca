//! Path reindexing for template-derived component arrays (SPEC_0032 §1).
//!
//! Instantiation resolves one template domain point of a homogeneous array of
//! structured components and derives every other point from it with the helpers
//! below, so the per-element cost is a structural rewrite of the family
//! subscript rather than a full re-resolution of the element's type, modifiers
//! and attributes. The derived entries are ordinary instances: nothing in
//! Instance IR records that they were produced this way, because SPEC_0032 §1
//! requires the result to be indistinguishable from element-by-element
//! expansion.
//!
//! Every rewrite here is deliberately guarded: a path is only reindexed when its
//! segment at `depth` matches the template domain point by name *and*
//! subscripts, and when every segment before `depth` matches the family's own
//! ancestor prefix. That keeps lexical scopes (which never carry instance
//! subscripts) untouched, and keeps a sibling instance that happens to contain
//! an identically named array — `left.pin[1]` next to a family rooted at
//! `right.pin` — out of the rewrite.

use rumoca_core::{AffineForm, ComponentPath, ComponentReference, Subscript};
use rumoca_ir_ast as ast;

/// Rewrites the family subscript of instance-tree paths from a family's
/// template domain point to another domain point.
#[derive(Debug, Clone, Copy)]
pub struct FamilyReindex<'a> {
    /// Rendered instance segments enclosing the family (`["plug_p"]` for a
    /// family rooted at `plug_p.pin`). Must have exactly `depth` entries; a
    /// path whose leading segments differ is not a member of this family.
    pub ancestors: &'a [String],
    /// Name of the family-subscripted segment (`pin` for `plug_p.pin[3]`).
    pub segment: &'a str,
    /// Zero-based index of that segment inside member paths.
    pub depth: usize,
    /// Subscripts of the template domain point.
    pub template_tuple: &'a [i64],
    /// Subscripts of the domain point being derived.
    pub tuple: &'a [i64],
}

impl FamilyReindex<'_> {
    /// True when a structured path part is the family's template segment.
    fn part_matches(&self, name: &str, subs: &[i64]) -> bool {
        name == self.segment && subs == self.template_tuple
    }

    /// True when the segments before `depth` are this family's own ancestors.
    ///
    /// Without this check a family at `depth >= 1` would rewrite the matching
    /// segment of any sibling instance that reuses the segment name.
    fn ancestors_match(&self, rendered: impl Fn(usize) -> Option<String>) -> bool {
        if self.ancestors.len() != self.depth {
            return false;
        }
        (0..self.depth).all(|position| rendered(position).as_ref() == self.ancestors.get(position))
    }

    /// Ancestor check for structured `(name, subscripts)` path parts.
    fn structured_ancestors_match(&self, parts: &[(String, Vec<i64>)]) -> bool {
        self.ancestors_match(|position| {
            parts
                .get(position)
                .map(|(name, subs)| render_segment(name, subs))
        })
    }

    /// Ancestor check for already-rendered path segments.
    fn rendered_ancestors_match(&self, parts: &[impl AsRef<str>]) -> bool {
        self.ancestors_match(|position| parts.get(position).map(|part| part.as_ref().to_string()))
    }

    /// Ancestor check for resolved component-reference parts. An ancestor whose
    /// subscripts are not all constant can never be proven to be this family's,
    /// so it is treated as a mismatch.
    fn reference_ancestors_match(&self, parts: &[rumoca_core::ComponentRefPart]) -> bool {
        self.ancestors_match(|position| {
            let part = parts.get(position)?;
            let values: Vec<i64> = part.subs.iter().filter_map(index_value).collect();
            (values.len() == part.subs.len()).then(|| render_segment(&part.ident, &values))
        })
    }

    /// Ancestor check for compact connection endpoint parts.
    fn endpoint_ancestors_match(&self, parts: &[(String, Vec<AffineForm>)]) -> bool {
        self.ancestors_match(|position| {
            let (name, subscripts) = parts.get(position)?;
            Some(render_segment(name, &constant_subscripts(subscripts)?))
        })
    }

    /// The family segment rendered at the template domain point (`pin[1]`).
    pub fn template_segment(&self) -> String {
        render_segment(self.segment, self.template_tuple)
    }

    /// The family segment rendered at the derived domain point (`pin[2]`).
    pub fn member_segment(&self) -> String {
        render_segment(self.segment, self.tuple)
    }

    /// Reindex a structured instance path, leaving non-member paths unchanged.
    pub fn qualified_name(&self, name: &ast::QualifiedName) -> ast::QualifiedName {
        let mut result = name.clone();
        if !self.structured_ancestors_match(&result.parts) {
            return result;
        }
        let Some((part_name, subs)) = result.parts.get_mut(self.depth) else {
            return result;
        };
        if !self.part_matches(part_name, subs) {
            return result;
        }
        *subs = self.tuple.to_vec();
        result
    }

    /// Reindex an optional structured instance path.
    pub fn optional_qualified_name(
        &self,
        name: Option<&ast::QualifiedName>,
    ) -> Option<ast::QualifiedName> {
        name.map(|name| self.qualified_name(name))
    }

    /// Reindex a resolved component reference, preserving subscript spans.
    pub fn component_reference(&self, reference: &ComponentReference) -> ComponentReference {
        if !self.reference_ancestors_match(reference.parts()) {
            return reference.clone();
        }
        let Some(part) = reference.parts().get(self.depth) else {
            return reference.clone();
        };
        let template_values: Vec<i64> = part.subs.iter().filter_map(index_value).collect();
        if template_values.len() != part.subs.len()
            || !self.part_matches(&part.ident, &template_values)
        {
            return reference.clone();
        }
        let rewritten_subscripts = self
            .tuple
            .iter()
            .enumerate()
            .map(|(position, value)| Subscript::Index {
                value: *value,
                span: subscript_span(part, position),
            })
            .collect();
        let mut parts = reference.parts().to_vec();
        parts[self.depth].subs = rewritten_subscripts;
        reference
            .with_replaced_parts(parts)
            .expect("reindexing subscripts preserves every exact part identity")
    }

    /// Reindex a rendered component path.
    pub fn component_path(&self, path: &ComponentPath) -> ComponentPath {
        match self.rewrite_rendered_parts(path.parts()) {
            Some(parts) => ComponentPath::from_parts(parts),
            None => path.clone(),
        }
    }

    /// Reindex a rendered flat path, returning `None` when it is not a member
    /// of this family (so callers can distinguish "unchanged" from "rewritten").
    ///
    /// Membership means the whole prefix matches: the segments before `depth`
    /// are the family's `ancestors` and the segment at `depth` is the template
    /// domain point. A sibling instance carrying the same segment name is not a
    /// member and is returned as `None`.
    ///
    /// Segmentation goes through `ComponentPath`, never raw `split('.')`, so
    /// subscripts containing dots stay inside their own segment.
    pub fn flat_path(&self, path: &str) -> Option<String> {
        let parsed = ComponentPath::from_flat_path(path);
        let rewritten = self.rewrite_rendered_parts(parsed.parts())?;
        Some(ComponentPath::from_parts(rewritten).to_flat_string())
    }

    /// Reindex a rendered flat path, keeping non-member paths as-is.
    pub fn flat_path_or_same(&self, path: &str) -> String {
        self.flat_path(path).unwrap_or_else(|| path.to_string())
    }

    fn rewrite_rendered_parts(&self, parts: &[impl AsRef<str>]) -> Option<Vec<String>> {
        if !self.rendered_ancestors_match(parts) {
            return None;
        }
        let template = self.template_segment();
        if parts.get(self.depth)?.as_ref() != template {
            return None;
        }
        let mut rewritten: Vec<String> =
            parts.iter().map(|part| part.as_ref().to_string()).collect();
        rewritten[self.depth] = self.member_segment();
        Some(rewritten)
    }

    /// Reindex a compact connection endpoint whose family segment is a
    /// binder-free constant at the template domain point.
    pub fn connection_endpoint(
        &self,
        endpoint: &ast::InstanceConnectionEndpoint,
    ) -> ast::InstanceConnectionEndpoint {
        let mut result = endpoint.clone();
        if !self.endpoint_ancestors_match(&result.parts) {
            return result;
        }
        let Some((name, subscripts)) = result.parts.get_mut(self.depth) else {
            return result;
        };
        let Some(constants) = constant_subscripts(subscripts) else {
            return result;
        };
        if !self.part_matches(name, &constants) {
            return result;
        }
        let rank = subscripts.first().map_or(0, |form| form.coeffs.len());
        *subscripts = self
            .tuple
            .iter()
            .map(|value| AffineForm::constant(*value, rank))
            .collect();
        result
    }
}

fn render_segment(name: &str, subscripts: &[i64]) -> String {
    use std::fmt::Write as _;

    let mut rendered = name.to_string();
    if subscripts.is_empty() {
        return rendered;
    }
    rendered.push('[');
    for (position, value) in subscripts.iter().enumerate() {
        if position > 0 {
            rendered.push(',');
        }
        let _ = write!(rendered, "{value}");
    }
    rendered.push(']');
    rendered
}

fn index_value(subscript: &Subscript) -> Option<i64> {
    match subscript {
        Subscript::Index { value, .. } => Some(*value),
        _ => None,
    }
}

fn subscript_span(part: &rumoca_core::ComponentRefPart, position: usize) -> rumoca_core::Span {
    part.subs
        .get(position)
        .map_or(part.span, rumoca_core::Subscript::span)
}

fn constant_subscripts(subscripts: &[AffineForm]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|form| form.is_binder_free().then_some(form.constant))
        .collect()
}

/// Derive one component instance of a family from its template instance.
pub fn family_member_component(
    template: &ast::InstanceData,
    instance_id: rumoca_core::InstanceId,
    reindex: &FamilyReindex<'_>,
) -> ast::InstanceData {
    let mut member = template.clone();
    member.instance_id = instance_id;
    member.qualified_name = reindex.qualified_name(&template.qualified_name);
    member.component_ref = template
        .component_ref
        .as_ref()
        .map(|reference| reindex.component_reference(reference));
    member.declaration_source_scope =
        reindex.optional_qualified_name(template.declaration_source_scope.as_ref());
    member.binding_source_scope =
        reindex.optional_qualified_name(template.binding_source_scope.as_ref());
    member.attribute_source_scopes = template
        .attribute_source_scopes
        .iter()
        .map(|(attribute, scope)| (attribute.clone(), reindex.qualified_name(scope)))
        .collect();
    member.oc_record_path = template
        .oc_record_path
        .as_deref()
        .map(|path| reindex.flat_path_or_same(path));
    member
}

/// Derive one class instance of a family from its template instance.
pub fn family_member_class(
    template: &ast::ClassInstanceData,
    instance_id: rumoca_core::InstanceId,
    reindex: &FamilyReindex<'_>,
) -> ast::ClassInstanceData {
    let mut member = template.clone();
    member.instance_id = instance_id;
    member.qualified_name = reindex.qualified_name(&template.qualified_name);
    reindex_equations(&mut member.equations, reindex);
    reindex_equations(&mut member.initial_equations, reindex);
    reindex_algorithms(&mut member.algorithms, reindex);
    reindex_algorithms(&mut member.initial_algorithms, reindex);
    for connection in &mut member.connections {
        reindex_connection(connection, reindex);
    }
    member
}

fn reindex_equations(equations: &mut [ast::InstanceEquation], reindex: &FamilyReindex<'_>) {
    for equation in equations {
        equation.origin = reindex.qualified_name(&equation.origin);
    }
}

fn reindex_algorithms(algorithms: &mut [Vec<ast::InstanceStatement>], reindex: &FamilyReindex<'_>) {
    for section in algorithms {
        for statement in section {
            statement.origin = reindex.qualified_name(&statement.origin);
        }
    }
}

fn reindex_connection(connection: &mut ast::InstanceConnection, reindex: &FamilyReindex<'_>) {
    connection.a = reindex.qualified_name(&connection.a);
    connection.b = reindex.qualified_name(&connection.b);
    connection.scope = reindex.flat_path_or_same(&connection.scope);
    if let Some(family) = connection.family.as_mut() {
        family.a = reindex.connection_endpoint(&family.a);
        family.b = reindex.connection_endpoint(&family.b);
    }
}

#[cfg(test)]
mod tests;
