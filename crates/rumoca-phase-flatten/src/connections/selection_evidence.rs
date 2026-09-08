//! Required evidence for rendered selections used by connection lowering.
//!
//! Flat retains one declaration for a compact array while connection scalar
//! views render selected members.  This module resolves those rendered names to
//! the longest exact declaration owner and keeps occurrence-level indices on
//! the owner rather than consuming them as declaration dimensions.

use super::*;

/// Check whether a rendered name has proven array-selection evidence.
///
/// Missing evidence means the endpoint is not a selected Flat declaration;
/// malformed, ambiguous, or out-of-range evidence is a typed refusal and can
/// never be reinterpreted as a connector-level endpoint.
pub(super) fn has_proven_primitive_array_selection(
    var: &rumoca_core::VarName,
    flat: &flat::Model,
    span: rumoca_core::Span,
) -> Result<bool, FlattenError> {
    classify_connection_declaration(flat, var, span)
        .map(|evidence| evidence.is_some_and(|evidence| evidence.is_selected_primitive()))
}

/// Exact declaration token for one direct or selected connection member.
pub(super) struct ConnectionDeclarationEvidence<'flat> {
    base: rumoca_core::VarName,
    declaration: &'flat flat::Variable,
    indices: Vec<i64>,
}

impl<'flat> ConnectionDeclarationEvidence<'flat> {
    pub(super) fn base(&self) -> &rumoca_core::VarName {
        &self.base
    }

    pub(super) fn declaration(&self) -> &'flat flat::Variable {
        self.declaration
    }

    pub(super) fn indices(&self) -> &[i64] {
        &self.indices
    }

    pub(super) fn is_selected_primitive(&self) -> bool {
        !self.indices.is_empty() && self.declaration.is_primitive
    }
}

/// Classify a route-level endpoint. `None` means it may be a composite
/// connector; malformed selection syntax/evidence is always a typed refusal.
pub(super) fn classify_connection_declaration<'flat>(
    flat: &'flat flat::Model,
    var: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<Option<ConnectionDeclarationEvidence<'flat>>, FlattenError> {
    if let Some(declaration) = flat.variables.get(var) {
        return Ok(Some(ConnectionDeclarationEvidence {
            base: var.clone(),
            declaration,
            indices: Vec::new(),
        }));
    }
    declared_array_element_evidence(var, flat)
        .map(|selected| {
            selected.map(|selected| ConnectionDeclarationEvidence {
                base: selected.base,
                declaration: selected.declaration,
                indices: selected.indices,
            })
        })
        .map_err(|reason| {
            FlattenError::invalid_connection_evidence(
                format!(
                    "connection endpoint `{var}` has invalid declaration-selection evidence: {reason}"
                ),
                span,
            )
        })
}

/// Require the exact declaration token after primitive/member classification.
pub(super) fn require_connection_declaration<'flat>(
    flat: &'flat flat::Model,
    var: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<ConnectionDeclarationEvidence<'flat>, FlattenError> {
    classify_connection_declaration(flat, var, span)?.ok_or_else(|| {
        FlattenError::invalid_connection_evidence(
            format!("connection member `{var}` has no Flat declaration evidence"),
            span,
        )
    })
}

pub(crate) struct DeclaredArrayElement<'flat> {
    pub(crate) base: rumoca_core::VarName,
    pub(crate) declaration: &'flat flat::Variable,
    pub(crate) indices: Vec<i64>,
}

/// Prove that every rendered subscript in a connection member selects the
/// corresponding leading dimension of one Flat declaration.
///
/// Connector-array expansion can place an index on an inner rendered segment
/// (`a[1].e`) even when the retained Flat declaration is index-free (`a.e`).
/// Parsing only a suffix on the whole path therefore makes cardinality,
/// expression materialization and connected-state marking disagree. This one
/// proof supplies all semantic consumers.
pub(crate) fn declared_array_element_evidence<'flat>(
    var: &rumoca_core::VarName,
    flat: &'flat flat::Model,
) -> Result<Option<DeclaredArrayElement<'flat>>, String> {
    let Some(selection) = indexed_connection_selection(var.as_str(), flat)? else {
        return Ok(None);
    };
    let base = selection.base;
    let indices = selection.indices;
    let base_var = flat
        .variables
        .get(&base)
        .expect("connection selection owner came from the Flat variable map");
    if base_var.dims.is_empty() {
        return Err("subscripts select a declaration with no retained rank".to_string());
    }
    if indices.len() > base_var.dims.len() {
        return Err(format!(
            "{} subscripts select a declaration of rank {}",
            indices.len(),
            base_var.dims.len()
        ));
    }
    if indices
        .iter()
        .zip(&base_var.dims)
        .any(|(index, extent)| *extent < 1 || *index < 1 || *index > *extent)
    {
        return Err("endpoint subscript is outside its declared dimension".to_string());
    }
    Ok(Some(DeclaredArrayElement {
        base,
        declaration: base_var,
        indices,
    }))
}

struct IndexedConnectionSelection {
    base: rumoca_core::VarName,
    indices: Vec<i64>,
}

struct IndexedPathSegment {
    base: String,
    coordinates: Vec<i64>,
    index_groups: Vec<IndexedPathIndexGroup>,
}

struct IndexedPathIndexGroup {
    rendered: String,
    coordinate_count: usize,
}

pub(super) const MAX_DIRECT_CONNECTION_SELECTION_CANDIDATES: usize = 1_024;

/// Resolve a rendered element path to its longest exact Flat declaration.
///
/// A Flat name may retain occurrence-level indices while keeping a member array
/// compact. For example, `a[1].e[2]` must select from an exact `a[1].e`
/// declaration when one exists; stripping every index and looking only for
/// `a.e` confuses the component occurrence coordinate with the member-array
/// selection. Every declaration candidate retains a prefix of the coordinates
/// on each path segment. The candidate retaining the most coordinates owns the
/// selection; equal-length distinct owners are rejected instead of guessed.
fn indexed_connection_selection(
    path: &str,
    flat: &flat::Model,
) -> Result<Option<IndexedConnectionSelection>, String> {
    let segments = indexed_path_segments(path)?;
    if segments
        .iter()
        .all(|segment| segment.coordinates.is_empty())
    {
        return Ok(None);
    }

    let mut search = SelectionCandidateSearch {
        segments: &segments,
        flat,
        best_retained: None,
        owners: std::collections::BTreeMap::new(),
    };
    let candidate_count = segments.iter().fold(1usize, |count, segment| {
        count.saturating_mul(segment.coordinates.len() + 1)
    });
    if candidate_count <= MAX_DIRECT_CONNECTION_SELECTION_CANDIDATES {
        search.visit(
            0,
            &mut Vec::with_capacity(segments.len()),
            &mut Vec::new(),
            0,
        );
    } else {
        // Candidate enumeration is exponential in independently indexed path
        // segments. The fallback is deterministic and bounded by variables x
        // path rank instead of rendered coordinate cardinality.
        search.scan_declarations();
    }

    let Some(retained) = search.best_retained else {
        return Ok(None);
    };
    if search.owners.len() != 1 {
        return Err(format!(
            "connection endpoint `{path}` has ambiguous longest Flat declaration owners retaining {retained} occurrence coordinates: {}",
            search.owners.keys().cloned().collect::<Vec<_>>().join(", ")
        ));
    }
    let (base, indices) = search
        .owners
        .into_iter()
        .next()
        .expect("one longest connection selection owner was proved");
    Ok(Some(IndexedConnectionSelection {
        base: rumoca_core::VarName::new(base),
        indices,
    }))
}

fn indexed_path_segments(path: &str) -> Result<Vec<IndexedPathSegment>, String> {
    crate::path_utils::segments(path)
        .into_iter()
        .map(|segment| {
            let Some((base, groups)) = split_trailing_index_groups(segment) else {
                return Ok(IndexedPathSegment {
                    base: segment.to_string(),
                    coordinates: Vec::new(),
                    index_groups: Vec::new(),
                });
            };
            let mut coordinates = Vec::new();
            let mut index_groups = Vec::with_capacity(groups.len());
            for group in groups {
                let Some(values) = parse_literal_index_group_values(&group) else {
                    return Err("endpoint subscripts are not concrete integer indices".to_string());
                };
                index_groups.push(IndexedPathIndexGroup {
                    rendered: group,
                    coordinate_count: values.len(),
                });
                coordinates.extend(values);
            }
            Ok(IndexedPathSegment {
                base,
                coordinates,
                index_groups,
            })
        })
        .collect()
}

struct SelectionCandidateSearch<'a> {
    segments: &'a [IndexedPathSegment],
    flat: &'a flat::Model,
    best_retained: Option<usize>,
    owners: std::collections::BTreeMap<String, Vec<i64>>,
}

impl SelectionCandidateSearch<'_> {
    fn visit(
        &mut self,
        segment_index: usize,
        rendered: &mut Vec<String>,
        selected: &mut Vec<i64>,
        retained: usize,
    ) {
        if segment_index == self.segments.len() {
            if selected.is_empty() {
                return;
            }
            let candidate = rendered.join(".");
            if !self
                .flat
                .variables
                .contains_key(&rumoca_core::VarName::new(&candidate))
            {
                return;
            }
            self.record(candidate, selected.clone(), retained);
            return;
        }

        let segment = &self.segments[segment_index];
        for keep in (0..=segment.coordinates.len()).rev() {
            let selected_len = selected.len();
            selected.extend_from_slice(&segment.coordinates[keep..]);
            rendered.push(render_indexed_segment(segment, keep));
            self.visit(segment_index + 1, rendered, selected, retained + keep);
            rendered.pop();
            selected.truncate(selected_len);
        }
    }

    fn scan_declarations(&mut self) {
        for declaration in self.flat.variables.keys() {
            let Ok(candidate_segments) = indexed_path_segments(declaration.as_str()) else {
                continue;
            };
            let Some((selected, retained)) =
                match_declared_candidate(self.segments, &candidate_segments)
            else {
                continue;
            };
            self.record(declaration.as_str().to_string(), selected, retained);
        }
    }

    fn record(&mut self, candidate: String, selected: Vec<i64>, retained: usize) {
        match self.best_retained {
            Some(best) if retained < best => {}
            Some(best) if retained == best => {
                self.owners.entry(candidate).or_insert(selected);
            }
            _ => {
                self.best_retained = Some(retained);
                self.owners.clear();
                self.owners.insert(candidate, selected);
            }
        }
    }
}

fn match_declared_candidate(
    path_segments: &[IndexedPathSegment],
    candidate_segments: &[IndexedPathSegment],
) -> Option<(Vec<i64>, usize)> {
    if candidate_segments.len() != path_segments.len() {
        return None;
    }
    let mut retained = 0usize;
    let mut selected = Vec::new();
    for (path, candidate) in path_segments.iter().zip(candidate_segments) {
        let keep = candidate.coordinates.len();
        if path.base != candidate.base
            || keep > path.coordinates.len()
            || render_indexed_segment(candidate, keep) != render_indexed_segment(path, keep)
        {
            return None;
        }
        retained += keep;
        selected.extend_from_slice(&path.coordinates[keep..]);
    }
    (!selected.is_empty()).then_some((selected, retained))
}

fn render_indexed_segment(segment: &IndexedPathSegment, retained: usize) -> String {
    debug_assert!(retained <= segment.coordinates.len());
    let mut rendered = segment.base.clone();
    let mut coordinate_offset = 0usize;
    let mut remaining = retained;
    for group in &segment.index_groups {
        if remaining == 0 {
            break;
        }
        let take = remaining.min(group.coordinate_count);
        if take == group.coordinate_count {
            rendered.push_str(&group.rendered);
        } else {
            let coordinates = &segment.coordinates[coordinate_offset..coordinate_offset + take];
            rendered.push('[');
            rendered.push_str(
                &coordinates
                    .iter()
                    .map(i64::to_string)
                    .collect::<Vec<_>>()
                    .join(","),
            );
            rendered.push(']');
        }
        coordinate_offset += group.coordinate_count;
        remaining -= take;
    }
    debug_assert_eq!(remaining, 0);
    rendered
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn route_classification_distinguishes_missing_from_invalid_selection() {
        let mut model = flat::Model::new();
        model.add_variable(
            rumoca_core::VarName::new("a.v"),
            flat::Variable {
                dims: vec![2],
                ..flat::Variable::empty_with_span(rumoca_core::Span::DUMMY)
            },
        );

        let missing = classify_connection_declaration(
            &model,
            &rumoca_core::VarName::new("connector_without_flat_members"),
            rumoca_core::Span::DUMMY,
        )
        .expect("a composite route may have no direct Flat declaration");
        let invalid = match classify_connection_declaration(
            &model,
            &rumoca_core::VarName::new("a.v[3]"),
            rumoca_core::Span::DUMMY,
        ) {
            Err(error) => error,
            Ok(_) => panic!("an out-of-range selected declaration is invalid, not missing"),
        };

        assert!(missing.is_none());
        assert!(
            invalid
                .to_string()
                .contains("outside its declared dimension")
        );
    }

    #[test]
    fn selected_nonprimitive_declaration_cannot_enter_the_primitive_route() {
        let mut model = flat::Model::new();
        model.add_variable(
            rumoca_core::VarName::new("bus"),
            flat::Variable {
                dims: vec![2],
                is_primitive: false,
                ..flat::Variable::empty_with_span(rumoca_core::Span::DUMMY)
            },
        );

        let selected = rumoca_core::VarName::new("bus[1]");
        let evidence = require_connection_declaration(&model, &selected, rumoca_core::Span::DUMMY)
            .expect("the selected composite declaration still has exact evidence");

        assert!(!evidence.declaration().is_primitive);
        assert!(
            !has_proven_primitive_array_selection(&selected, &model, rumoca_core::Span::DUMMY,)
                .expect("valid composite selection remains a connector-level route")
        );
    }
}
