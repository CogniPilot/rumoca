//! Enhanced completion handler for Modelica files.

use lsp_types::{CompletionItem, CompletionItemKind, Position};
use rumoca_compile::Session;
use rumoca_compile::compile::ClassLocalCompletionKind;
#[cfg(feature = "server")]
use rumoca_compile::compile::SessionSnapshot;
use rumoca_compile::compile::core as rumoca_core;
use rumoca_compile::parsing::ast;
use rumoca_compile::parsing::ast::Visitor;
use rumoca_compile::parsing::ir_core as rumoca_ir_core;
use std::collections::{BTreeMap, HashSet};
use std::ops::ControlFlow;

use crate::helpers::{
    find_enclosing_class, find_enclosing_class_qualified_name, get_text_before_cursor,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum CompletionSemanticLayer {
    SyntaxFallback,
    BuiltinKeyword,
    PackageDefMap,
    ClassInterface,
}

impl CompletionSemanticLayer {
    #[cfg(feature = "server")]
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::SyntaxFallback => "syntax_fallback",
            Self::BuiltinKeyword => "builtin_keyword",
            Self::PackageDefMap => "package_def_map",
            Self::ClassInterface => "class_interface",
        }
    }
}

#[derive(Debug)]
pub(crate) struct CompletionResult {
    pub(crate) items: Vec<CompletionItem>,
    pub(crate) semantic_layer: CompletionSemanticLayer,
}

impl CompletionResult {
    fn new(items: Vec<CompletionItem>, semantic_layer: CompletionSemanticLayer) -> Self {
        Self {
            items,
            semantic_layer,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DotCompletionTarget {
    base_segments: Vec<String>,
    member_partial: String,
}

enum CompletionQuerySession<'a> {
    Host(&'a mut Session),
    #[cfg(feature = "server")]
    Snapshot(&'a SessionSnapshot),
}

impl CompletionQuerySession<'_> {
    fn namespace_index_query(&mut self, prefix: &str) -> Vec<(String, String, bool)> {
        match self {
            Self::Host(session) => session.namespace_index_query(prefix).unwrap_or_default(),
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => snapshot.namespace_index_query(prefix).unwrap_or_default(),
        }
    }

    fn namespace_class_names_cached(&self) -> Vec<String> {
        match self {
            Self::Host(session) => session.namespace_class_names_cached(),
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => snapshot.namespace_class_names_cached(),
        }
    }

    fn class_component_members_query(&mut self, class_name: &str) -> Vec<(String, String)> {
        match self {
            Self::Host(session) => session.class_component_members_query(class_name),
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => snapshot.class_component_members_query(class_name),
        }
    }

    fn class_type_resolution_candidates_query(
        &mut self,
        uri: &str,
        qualified_name: &str,
        raw_name: &str,
    ) -> Vec<String> {
        match self {
            Self::Host(session) => {
                session.class_type_resolution_candidates_query(uri, qualified_name, raw_name)
            }
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => {
                snapshot.class_type_resolution_candidates_query(uri, qualified_name, raw_name)
            }
        }
    }

    fn class_component_type_query(
        &mut self,
        uri: &str,
        qualified_name: &str,
        component_name: &str,
    ) -> Option<String> {
        match self {
            Self::Host(session) => {
                session.class_component_type_query(uri, qualified_name, component_name)
            }
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => {
                snapshot.class_component_type_query(uri, qualified_name, component_name)
            }
        }
    }

    fn class_type_resolution_candidates_in_class_query(
        &mut self,
        class_name: &str,
        raw_name: &str,
    ) -> Vec<String> {
        match self {
            Self::Host(session) => {
                session.class_type_resolution_candidates_in_class_query(class_name, raw_name)
            }
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => {
                snapshot.class_type_resolution_candidates_in_class_query(class_name, raw_name)
            }
        }
    }

    fn class_component_member_info_query(
        &mut self,
        class_name: &str,
        component_name: &str,
    ) -> Option<(String, String)> {
        match self {
            Self::Host(session) => {
                session.class_component_member_info_query(class_name, component_name)
            }
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => {
                snapshot.class_component_member_info_query(class_name, component_name)
            }
        }
    }

    fn class_local_completion_items_query(
        &mut self,
        uri: &str,
        qualified_name: &str,
    ) -> Vec<rumoca_compile::compile::ClassLocalCompletionItem> {
        match self {
            Self::Host(session) => session.class_local_completion_items_query(uri, qualified_name),
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => {
                snapshot.class_local_completion_items_query(uri, qualified_name)
            }
        }
    }

    fn enclosing_class_qualified_name_query(&mut self, uri: &str, line: u32) -> Option<String> {
        match self {
            Self::Host(session) => session.enclosing_class_qualified_name_query(uri, line),
            #[cfg(feature = "server")]
            Self::Snapshot(snapshot) => snapshot.enclosing_class_qualified_name_query(uri, line),
        }
    }
}

fn namespace_class_names(session: Option<&mut CompletionQuerySession<'_>>) -> Vec<String> {
    let Some(session) = session else {
        return Vec::new();
    };

    let namespace_entries = session.namespace_index_query("");
    let class_names = namespace_entries
        .into_iter()
        .map(|(_, full_name, _)| full_name)
        .collect::<Vec<_>>();

    if !class_names.is_empty() {
        return class_names;
    }

    session.namespace_class_names_cached()
}

fn namespace_children(
    session: Option<&mut CompletionQuerySession<'_>>,
    prefix: &str,
) -> Vec<(String, String, bool)> {
    let Some(session) = session else {
        return Vec::new();
    };
    let cached = session.namespace_index_query(prefix);
    if !cached.is_empty() {
        return cached;
    }

    let class_names = namespace_class_names(Some(session));
    if class_names.is_empty() {
        return Vec::new();
    }

    namespace_children_from_class_names(&class_names, prefix)
}

/// Handle completion request - returns keyword + scope-aware completions.
///
/// When a `session` is provided, also includes namespace/package/class
/// completions from the cached source-root namespace closure.
pub fn handle_completion(
    source: &str,
    ast: Option<&ast::StoredDefinition>,
    session: Option<&mut Session>,
    uri: Option<&str>,
    line: u32,
    character: u32,
) -> Vec<CompletionItem> {
    handle_completion_with_context(
        source,
        ast,
        session.map(CompletionQuerySession::Host),
        uri,
        line,
        character,
    )
    .items
}

#[cfg(feature = "server")]
pub(crate) fn handle_completion_with_snapshot_and_provenance(
    source: &str,
    ast: Option<&ast::StoredDefinition>,
    snapshot: Option<&SessionSnapshot>,
    uri: Option<&str>,
    line: u32,
    character: u32,
) -> CompletionResult {
    handle_completion_with_context(
        source,
        ast,
        snapshot.map(CompletionQuerySession::Snapshot),
        uri,
        line,
        character,
    )
}

fn handle_completion_with_context(
    source: &str,
    ast: Option<&ast::StoredDefinition>,
    mut session: Option<CompletionQuerySession<'_>>,
    uri: Option<&str>,
    line: u32,
    character: u32,
) -> CompletionResult {
    let position = Position { line, character };
    let prefix = get_text_before_cursor(source, position)
        .unwrap_or_default()
        .trim()
        .to_string();

    // Get the partial word being typed (may include dots for qualified names)
    let partial: String = prefix
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    let mut items = Vec::new();
    let mut semantic_layer = CompletionSemanticLayer::SyntaxFallback;
    let mut cached_namespace_class_names: Option<Vec<String>> = None;
    let active_model = match (session.as_mut(), uri) {
        (Some(session), Some(uri)) => session
            .enclosing_class_qualified_name_query(uri, line)
            .or_else(|| ast.and_then(|tree| find_enclosing_class_qualified_name(tree, line))),
        _ => ast.and_then(|tree| find_enclosing_class_qualified_name(tree, line)),
    };

    // Check for dot-completion (e.g., "Modelica.Blocks." or "pid.")
    if prefix.ends_with('.') || prefix.contains('.') {
        // Try local AST dot-completion first
        if let Some(dot_items) = dot_completion(
            source,
            ast,
            session.as_mut(),
            uri,
            active_model.as_deref(),
            position,
            &prefix,
        ) {
            return dot_items;
        }
        let namespace_items = namespace_dot_completion_from_namespace(session.as_mut(), &prefix);
        if !namespace_items.is_empty() {
            return CompletionResult::new(namespace_items, CompletionSemanticLayer::PackageDefMap);
        }
        // Try namespace/class dot-completion from the cached class graph.
        if cached_namespace_class_names.is_none() {
            cached_namespace_class_names = Some(namespace_class_names(session.as_mut()));
        }
        let class_names = cached_namespace_class_names
            .as_ref()
            .expect("populated cache");
        if !class_names.is_empty() {
            let class_name_refs: Vec<&str> = class_names.iter().map(|s| s.as_str()).collect();
            let namespace_items = namespace_dot_completion(&class_name_refs, &prefix);
            if !namespace_items.is_empty() {
                return CompletionResult::new(
                    namespace_items,
                    CompletionSemanticLayer::PackageDefMap,
                );
            }
        }
    }

    // Check for modifier completion (inside parentheses)
    if is_in_modification_context(&prefix) {
        let modifier_items = modification_context_completions(
            ast,
            session.as_mut(),
            uri,
            active_model.as_deref(),
            line,
            &prefix,
            &partial,
        );
        semantic_layer = semantic_layer.max(modifier_items.semantic_layer);
        items.extend(modifier_items.items);
    }

    if let (Some(session), Some(uri), Some(active_model)) =
        (session.as_mut(), uri, active_model.as_deref())
    {
        let local_items = query_local_completions(session, uri, active_model, &partial);
        if !local_items.is_empty() {
            semantic_layer = semantic_layer.max(CompletionSemanticLayer::ClassInterface);
        }
        items.extend(local_items);
    } else if let Some(ast) = ast {
        items.extend(ast_local_completions(ast, line, &partial));
    }

    extend_general_completion_items(
        &mut items,
        &mut semantic_layer,
        &mut cached_namespace_class_names,
        session,
        &partial,
    );

    CompletionResult::new(items, semantic_layer)
}

fn extend_general_completion_items(
    items: &mut Vec<CompletionItem>,
    semantic_layer: &mut CompletionSemanticLayer,
    cached_namespace_class_names: &mut Option<Vec<String>>,
    mut session: Option<CompletionQuerySession<'_>>,
    partial: &str,
) {
    if !partial.is_empty() {
        let namespace_items =
            namespace_prefix_completions_from_namespace(session.as_mut(), partial);
        if !namespace_items.is_empty() {
            *semantic_layer = (*semantic_layer).max(CompletionSemanticLayer::PackageDefMap);
            items.extend(namespace_items);
        } else {
            if cached_namespace_class_names.is_none() {
                *cached_namespace_class_names = Some(namespace_class_names(session.as_mut()));
            }
            let class_names = cached_namespace_class_names
                .as_ref()
                .expect("populated cache");
            let class_name_refs: Vec<&str> = class_names.iter().map(|s| s.as_str()).collect();
            let prefix_items = namespace_prefix_completions(&class_name_refs, partial);
            if !prefix_items.is_empty() {
                *semantic_layer = (*semantic_layer).max(CompletionSemanticLayer::PackageDefMap);
            }
            items.extend(prefix_items);
        }
    }

    let builtin_items = builtin_completions(partial);
    if !builtin_items.is_empty() {
        *semantic_layer = (*semantic_layer).max(CompletionSemanticLayer::BuiltinKeyword);
    }
    items.extend(builtin_items);

    let keyword_items = keyword_completions(partial);
    if !keyword_items.is_empty() {
        *semantic_layer = (*semantic_layer).max(CompletionSemanticLayer::BuiltinKeyword);
    }
    items.extend(keyword_items);
}

/// Dot-completion for cached namespace class names.
///
/// Given prefix "Modelica.Blocks." and cached class names like
/// "Modelica.Blocks.Continuous.PID", returns completion items for
/// the immediate children at that level (e.g., "Continuous", "Sources").
fn namespace_dot_completion(class_names: &[&str], prefix: &str) -> Vec<CompletionItem> {
    let (search_prefix, filter_partial) = extract_qualified_prefix(prefix);

    if search_prefix.is_empty() {
        return Vec::new();
    }

    let mut seen = std::collections::HashSet::new();
    let mut items = Vec::new();

    for name in class_names {
        let Some(rest) = name.strip_prefix(&search_prefix) else {
            continue;
        };
        let child = rest.split('.').next().unwrap_or(rest);
        if child.is_empty() {
            continue;
        }
        if !filter_partial.is_empty()
            && !child
                .to_lowercase()
                .starts_with(&filter_partial.to_lowercase())
        {
            continue;
        }
        if !seen.insert(child.to_string()) {
            continue;
        }
        let full_name = format!("{}{}", search_prefix, child);
        let has_children = class_names
            .iter()
            .any(|n| n.starts_with(&format!("{}.", full_name)));
        let kind = if has_children {
            CompletionItemKind::MODULE
        } else {
            CompletionItemKind::CLASS
        };
        items.push(CompletionItem {
            label: child.to_string(),
            kind: Some(kind),
            detail: Some(full_name),
            ..Default::default()
        });
    }

    items
}

fn namespace_dot_completion_from_namespace(
    session: Option<&mut CompletionQuerySession<'_>>,
    prefix: &str,
) -> Vec<CompletionItem> {
    let (search_prefix, filter_partial) = extract_qualified_prefix(prefix);
    if search_prefix.is_empty() {
        return Vec::new();
    }

    namespace_entries_to_completion_items(
        namespace_children(session, &search_prefix),
        &filter_partial,
    )
}

/// Extract the qualified prefix for dot-completion.
///
/// Returns (search_prefix, partial_filter):
/// - "Modelica.Blocks." -> ("Modelica.Blocks.", "")
/// - "Modelica.Blocks.Con" -> ("Modelica.Blocks.", "Con")
/// - "Modelica." -> ("Modelica.", "")
fn extract_qualified_prefix(prefix: &str) -> (String, String) {
    // Find the qualified name being typed (walk back from end through dots and idents)
    let qualified: String = prefix
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
        .collect::<String>()
        .chars()
        .rev()
        .collect();

    if let Some(last_dot) = qualified.rfind('.') {
        let base = &qualified[..=last_dot]; // includes trailing dot
        let partial = &qualified[last_dot + 1..];
        (base.to_string(), partial.to_string())
    } else {
        (String::new(), qualified)
    }
}

/// Top-level namespace class prefix completion.
///
/// Given partial "Model" and cached class names, returns "Modelica" etc.
fn namespace_prefix_completions(class_names: &[&str], partial: &str) -> Vec<CompletionItem> {
    let mut seen = std::collections::HashSet::new();
    let mut items = Vec::new();
    let partial_lower = partial.to_lowercase();

    for name in class_names {
        // Get the top-level name (before first dot)
        let top_level = name.split('.').next().unwrap_or(name);
        if top_level.to_lowercase().starts_with(&partial_lower)
            && seen.insert(top_level.to_string())
        {
            items.push(CompletionItem {
                label: top_level.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some("Namespace package".to_string()),
                ..Default::default()
            });
        }
    }

    items
}

fn namespace_prefix_completions_from_namespace(
    session: Option<&mut CompletionQuerySession<'_>>,
    partial: &str,
) -> Vec<CompletionItem> {
    namespace_entries_to_completion_items(namespace_children(session, ""), partial)
}

fn namespace_children_from_class_names(
    class_names: &[String],
    prefix: &str,
) -> Vec<(String, String, bool)> {
    let normalized_prefix = if prefix.is_empty() {
        String::new()
    } else if prefix.ends_with('.') {
        prefix.to_string()
    } else {
        format!("{prefix}.")
    };

    let mut seen = std::collections::HashSet::new();
    let mut items = Vec::new();

    for name in class_names {
        let candidate = if normalized_prefix.is_empty() {
            name.clone()
        } else {
            let Some(rest) = name.strip_prefix(&normalized_prefix) else {
                continue;
            };
            rest.to_string()
        };
        let child = candidate.split('.').next().unwrap_or(candidate.as_str());
        if child.is_empty() {
            continue;
        }
        let full_name = if normalized_prefix.is_empty() {
            child.to_string()
        } else {
            format!("{normalized_prefix}{child}")
        };
        let has_children = class_names
            .iter()
            .any(|candidate| candidate.starts_with(&format!("{full_name}.")));
        if seen.insert(full_name.clone()) {
            items.push((child.to_string(), full_name, has_children));
        }
    }

    items.sort_by(|left, right| left.0.cmp(&right.0).then_with(|| left.1.cmp(&right.1)));
    items
}

fn namespace_entries_to_completion_items(
    entries: Vec<(String, String, bool)>,
    partial: &str,
) -> Vec<CompletionItem> {
    let partial_lower = partial.to_lowercase();
    entries
        .into_iter()
        .filter(|(label, _, _)| {
            partial_lower.is_empty() || label.to_lowercase().starts_with(&partial_lower)
        })
        .map(|(label, full_name, has_children)| CompletionItem {
            label,
            kind: Some(if has_children {
                CompletionItemKind::MODULE
            } else {
                CompletionItemKind::CLASS
            }),
            detail: Some(full_name),
            ..Default::default()
        })
        .collect()
}

fn dot_completion(
    source: &str,
    ast: Option<&ast::StoredDefinition>,
    session: Option<&mut CompletionQuerySession<'_>>,
    uri: Option<&str>,
    active_model: Option<&str>,
    position: Position,
    prefix: &str,
) -> Option<CompletionResult> {
    let target = ast
        .and_then(|tree| ast_dot_completion_target(source, tree, position.line, position.character))
        .or_else(|| text_dot_completion_target(prefix))?;
    dot_completion_items(
        ast,
        session,
        uri,
        active_model,
        position.line,
        &target.base_segments,
        &target.member_partial,
    )
}

fn text_dot_completion_target(prefix: &str) -> Option<DotCompletionTarget> {
    let dot_pos = prefix.rfind('.')?;
    let member_partial = prefix[dot_pos + 1..].trim();
    let base = prefix[..dot_pos].trim();
    let base_path: String = base
        .chars()
        .rev()
        .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
        .collect::<String>()
        .chars()
        .rev()
        .collect();
    let base_segments = dotted_identifier_segments(&base_path)?;
    Some(DotCompletionTarget {
        base_segments,
        member_partial: member_partial.to_string(),
    })
}

fn ast_dot_completion_target(
    source: &str,
    ast: &ast::StoredDefinition,
    line: u32,
    character: u32,
) -> Option<DotCompletionTarget> {
    let mut finder = DotCompletionTargetFinder::new(source, line, character);
    let _ = finder.visit_stored_definition(ast);
    finder.best
}

struct DotCompletionTargetFinder<'a> {
    source_line: &'a str,
    line: u32,
    character: u32,
    best: Option<DotCompletionTarget>,
}

impl<'a> DotCompletionTargetFinder<'a> {
    fn new(source: &'a str, line: u32, character: u32) -> Self {
        Self {
            source_line: source.lines().nth(line as usize).unwrap_or_default(),
            line,
            character,
            best: None,
        }
    }

    fn consider_component_reference(&mut self, component_ref: &ast::ComponentReference) {
        for segment_end in 1..component_ref.parts.len() {
            let next_ident = &component_ref.parts[segment_end].ident;
            let token_line = next_ident.location.start_line.saturating_sub(1);
            if token_line != self.line {
                continue;
            }

            let member_start = next_ident.location.start_column.saturating_sub(1);
            let member_end = next_ident.location.end_column.saturating_sub(1);
            if self.character < member_start || self.character > member_end {
                continue;
            }

            let Some(member_partial) = slice_line(
                self.source_line,
                member_start as usize,
                self.character as usize,
            ) else {
                continue;
            };
            let base_segments = component_ref.parts[..segment_end]
                .iter()
                .map(|part| part.ident.text.to_string())
                .collect::<Vec<_>>();
            self.record(DotCompletionTarget {
                base_segments,
                member_partial: member_partial.to_string(),
            });
        }
    }

    fn record(&mut self, candidate: DotCompletionTarget) {
        if self
            .best
            .as_ref()
            .is_none_or(|best| candidate.base_segments.len() > best.base_segments.len())
        {
            self.best = Some(candidate);
        }
    }
}

impl ast::Visitor for DotCompletionTargetFinder<'_> {
    fn visit_component_reference_ctx(
        &mut self,
        cr: &ast::ComponentReference,
        _ctx: ast::ComponentReferenceContext,
    ) -> ControlFlow<()> {
        self.consider_component_reference(cr);
        self.visit_component_reference(cr)
    }
}

fn dotted_identifier_segments(path: &str) -> Option<Vec<String>> {
    let segments = path
        .split('.')
        .filter(|segment| !segment.is_empty())
        .map(ToString::to_string)
        .collect::<Vec<_>>();
    (!segments.is_empty()).then_some(segments)
}

fn slice_line(line: &str, start: usize, end: usize) -> Option<&str> {
    (start <= end && end <= line.len()).then(|| &line[start..end])
}

fn dot_completion_items(
    ast: Option<&ast::StoredDefinition>,
    mut session: Option<&mut CompletionQuerySession<'_>>,
    uri: Option<&str>,
    active_model: Option<&str>,
    line: u32,
    base_segments: &[String],
    member_partial: &str,
) -> Option<CompletionResult> {
    let type_candidates = component_path_type_candidates(
        ast,
        match &mut session {
            Some(session) => Some(&mut **session),
            None => None,
        },
        uri,
        active_model,
        line,
        base_segments,
    );
    if type_candidates.is_empty() {
        return None;
    }
    if let Some(items) = session_type_member_completions(
        match &mut session {
            Some(session) => Some(&mut **session),
            None => None,
        },
        &type_candidates,
        member_partial,
        false,
    ) {
        return Some(CompletionResult::new(
            items,
            CompletionSemanticLayer::ClassInterface,
        ));
    }
    if let Some(ast) = ast
        && let Some(items) =
            ast_type_member_completions(ast, &type_candidates, member_partial, false)
    {
        return Some(CompletionResult::new(
            items,
            CompletionSemanticLayer::SyntaxFallback,
        ));
    }
    None
}

fn component_completion_kind(comp: &ast::Component) -> CompletionItemKind {
    match (&comp.variability, &comp.causality) {
        (rumoca_ir_core::Variability::Parameter(_), _)
        | (rumoca_ir_core::Variability::Constant(_), _) => CompletionItemKind::CONSTANT,
        (_, rumoca_ir_core::Causality::Input(_)) | (_, rumoca_ir_core::Causality::Output(_)) => {
            CompletionItemKind::PROPERTY
        }
        _ => CompletionItemKind::VARIABLE,
    }
}

fn is_in_modification_context(prefix: &str) -> bool {
    // Simple heuristic: more open parens than close parens
    let opens = prefix.matches('(').count();
    let closes = prefix.matches(')').count();
    opens > closes
}

fn modification_context_completions(
    ast: Option<&ast::StoredDefinition>,
    mut session: Option<&mut CompletionQuerySession<'_>>,
    uri: Option<&str>,
    active_model: Option<&str>,
    line: u32,
    prefix: &str,
    partial: &str,
) -> CompletionResult {
    let Some(ctx) = modifier_context_from_prefix(prefix) else {
        return CompletionResult::new(
            modifier_completions(partial),
            CompletionSemanticLayer::BuiltinKeyword,
        );
    };
    if rumoca_core::is_builtin_type(&ctx.type_name) {
        return CompletionResult::new(
            modifier_completions(partial),
            CompletionSemanticLayer::BuiltinKeyword,
        );
    }

    let type_candidates = resolve_type_candidates(
        ast,
        match &mut session {
            Some(session) => Some(&mut **session),
            None => None,
        },
        uri,
        active_model,
        line,
        &ctx.type_name,
    );
    if let Some(items) = session_type_member_completions(
        match &mut session {
            Some(session) => Some(&mut **session),
            None => None,
        },
        &type_candidates,
        partial,
        true,
    ) {
        return CompletionResult::new(items, CompletionSemanticLayer::ClassInterface);
    }
    if let Some(ast) = ast
        && let Some(items) = ast_type_member_completions(ast, &type_candidates, partial, true)
    {
        return CompletionResult::new(items, CompletionSemanticLayer::SyntaxFallback);
    }

    CompletionResult::new(
        modifier_completions(partial),
        CompletionSemanticLayer::BuiltinKeyword,
    )
}

fn session_type_member_completions(
    session: Option<&mut CompletionQuerySession<'_>>,
    type_candidates: &[String],
    partial: &str,
    insert_assignment: bool,
) -> Option<Vec<CompletionItem>> {
    let session = session?;
    query_type_member_completions(session, type_candidates, partial, insert_assignment)
}

fn query_type_member_completions(
    session: &mut CompletionQuerySession<'_>,
    type_candidates: &[String],
    partial: &str,
    insert_assignment: bool,
) -> Option<Vec<CompletionItem>> {
    for type_name in type_candidates {
        let members = session.class_component_members_query(type_name);
        if members.is_empty() {
            continue;
        }
        return Some(member_completion_items(members, partial, insert_assignment));
    }
    None
}

fn ast_type_member_completions(
    ast: &ast::StoredDefinition,
    type_candidates: &[String],
    partial: &str,
    insert_assignment: bool,
) -> Option<Vec<CompletionItem>> {
    for type_name in type_candidates {
        let members = parsed_class_member_entries(ast, type_name);
        if members.is_empty() {
            continue;
        }
        return Some(member_completion_items(
            members.into_iter().collect(),
            partial,
            insert_assignment,
        ));
    }
    None
}

fn member_completion_items(
    members: Vec<(String, String)>,
    partial: &str,
    insert_assignment: bool,
) -> Vec<CompletionItem> {
    members
        .into_iter()
        .filter(|(name, _)| partial.is_empty() || name.starts_with(partial))
        .map(|(name, member_type)| CompletionItem {
            label: name.clone(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some(member_type),
            insert_text: Some(if insert_assignment {
                format!("{name} = ")
            } else {
                name.clone()
            }),
            ..Default::default()
        })
        .collect()
}

fn parsed_class_member_entries(
    ast: &ast::StoredDefinition,
    class_name: &str,
) -> Vec<(String, String)> {
    let Some((qualified_name, class)) = resolve_parsed_class_candidate(ast, class_name) else {
        return Vec::new();
    };
    let mut members = BTreeMap::<String, String>::new();
    let mut visiting = HashSet::<String>::new();
    collect_parsed_class_component_members(
        ast,
        &qualified_name,
        class,
        &mut members,
        &mut visiting,
    );
    members.into_iter().collect()
}

fn collect_parsed_class_component_members(
    ast: &ast::StoredDefinition,
    qualified_name: &str,
    class: &ast::ClassDef,
    members: &mut BTreeMap<String, String>,
    visiting: &mut HashSet<String>,
) {
    if !visiting.insert(qualified_name.to_string()) {
        return;
    }

    let class_line = class.location.start_line.saturating_sub(1);
    for ext in &class.extends {
        let base_name = ext.base_name.to_string();
        let base_candidates =
            resolve_type_candidates(Some(ast), None, None, None, class_line, &base_name);
        for candidate in base_candidates {
            let Some((base_qualified, base_class)) =
                resolve_parsed_class_candidate(ast, &candidate)
            else {
                continue;
            };
            collect_parsed_class_component_members(
                ast,
                &base_qualified,
                base_class,
                members,
                visiting,
            );
            for break_name in &ext.break_names {
                members.remove(break_name);
            }
            break;
        }
    }

    for (name, component) in &class.components {
        members.insert(name.clone(), component.type_name.to_string());
    }

    visiting.remove(qualified_name);
}

fn resolve_parsed_class_candidate<'a>(
    ast: &'a ast::StoredDefinition,
    class_name: &str,
) -> Option<(String, &'a ast::ClassDef)> {
    if class_name.contains('.') {
        return find_parsed_class_by_qualified_name(ast, class_name);
    }
    find_unique_parsed_class_by_simple_name(ast, class_name)
}

fn find_parsed_class_by_qualified_name<'a>(
    ast: &'a ast::StoredDefinition,
    class_name: &str,
) -> Option<(String, &'a ast::ClassDef)> {
    let within_prefix = ast
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|prefix| !prefix.is_empty());
    let relative_name = within_prefix
        .as_ref()
        .and_then(|prefix| class_name.strip_prefix(&format!("{prefix}.")))
        .unwrap_or(class_name);
    let mut parts = relative_name.split('.');
    let first = parts.next()?;
    let mut class = ast.classes.get(first)?;
    let mut qualified_name = within_prefix
        .map(|prefix| format!("{prefix}.{first}"))
        .unwrap_or_else(|| first.to_string());
    for part in parts {
        class = class.classes.get(part)?;
        qualified_name.push('.');
        qualified_name.push_str(part);
    }
    Some((qualified_name, class))
}

fn find_unique_parsed_class_by_simple_name<'a>(
    ast: &'a ast::StoredDefinition,
    class_name: &str,
) -> Option<(String, &'a ast::ClassDef)> {
    let prefix = ast
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|value| !value.is_empty());
    let mut found: Option<(String, &'a ast::ClassDef)> = None;
    for (name, class) in &ast.classes {
        let qualified_name = prefix
            .as_ref()
            .map(|value| format!("{value}.{name}"))
            .unwrap_or_else(|| name.clone());
        if !find_unique_parsed_class_by_simple_name_in_class(
            class_name,
            &qualified_name,
            class,
            &mut found,
        ) {
            return None;
        }
    }
    found
}

fn find_unique_parsed_class_by_simple_name_in_class<'a>(
    class_name: &str,
    qualified_name: &str,
    class: &'a ast::ClassDef,
    found: &mut Option<(String, &'a ast::ClassDef)>,
) -> bool {
    if class.name.text.as_ref() == class_name {
        if found.is_some() {
            return false;
        }
        *found = Some((qualified_name.to_string(), class));
    }
    for (nested_name, nested) in &class.classes {
        let nested_qualified = format!("{qualified_name}.{nested_name}");
        if !find_unique_parsed_class_by_simple_name_in_class(
            class_name,
            &nested_qualified,
            nested,
            found,
        ) {
            return false;
        }
    }
    true
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ModifierContext {
    type_name: String,
}

fn modifier_context_from_prefix(prefix: &str) -> Option<ModifierContext> {
    let paren_pos = prefix.rfind('(')?;
    let left = prefix[..paren_pos].trim_end();
    let mut parts = left.split_whitespace();
    let instance_token = parts.next_back()?;
    let type_token = parts.next_back()?;
    if instance_token.is_empty() || type_token.is_empty() {
        return None;
    }
    Some(ModifierContext {
        type_name: type_token
            .trim_matches(|c: char| c == ',' || c == ';')
            .to_string(),
    })
}

fn resolve_type_candidates(
    ast: Option<&ast::StoredDefinition>,
    session: Option<&mut CompletionQuerySession<'_>>,
    uri: Option<&str>,
    active_model: Option<&str>,
    line: u32,
    raw_type_name: &str,
) -> Vec<String> {
    let mut seen = HashSet::<String>::new();
    let mut candidates = Vec::<String>::new();
    let push = |name: String, seen: &mut HashSet<String>, candidates: &mut Vec<String>| {
        if !name.is_empty() && seen.insert(name.clone()) {
            candidates.push(name);
        }
    };

    push(raw_type_name.to_string(), &mut seen, &mut candidates);

    if let Some(session) = session {
        let queried = match (uri, active_model) {
            (Some(uri), Some(active_model)) => {
                session.class_type_resolution_candidates_query(uri, active_model, raw_type_name)
            }
            _ => {
                if raw_type_name.is_empty() {
                    Vec::new()
                } else {
                    vec![raw_type_name.to_string()]
                }
            }
        };
        if !queried.is_empty() {
            return queried;
        }
    }

    let Some(ast) = ast else {
        return candidates;
    };
    let Some(class) = find_enclosing_class(ast, line) else {
        return candidates;
    };

    for import in &class.imports {
        match import {
            ast::Import::Qualified { path, .. } => {
                let full = path.to_string();
                if import_simple_name(&full) == raw_type_name {
                    push(full, &mut seen, &mut candidates);
                }
            }
            ast::Import::Renamed { alias, path, .. } => {
                if alias.text.as_ref() == raw_type_name {
                    push(path.to_string(), &mut seen, &mut candidates);
                }
            }
            ast::Import::Selective { path, names, .. } => {
                if names.iter().any(|name| name.text.as_ref() == raw_type_name) {
                    push(
                        format!("{}.{}", path, raw_type_name),
                        &mut seen,
                        &mut candidates,
                    );
                }
            }
            ast::Import::Unqualified { path, .. } => {
                push(
                    format!("{}.{}", path, raw_type_name),
                    &mut seen,
                    &mut candidates,
                );
            }
        }
    }

    candidates
}

fn component_path_type_candidates(
    ast: Option<&ast::StoredDefinition>,
    mut session: Option<&mut CompletionQuerySession<'_>>,
    uri: Option<&str>,
    active_model: Option<&str>,
    line: u32,
    base_segments: &[String],
) -> Vec<String> {
    let Some(first_segment) = base_segments.first() else {
        return Vec::new();
    };
    let Some(component_type) = scoped_component_type(
        ast,
        match &mut session {
            Some(session) => Some(&mut **session),
            None => None,
        },
        uri,
        active_model,
        line,
        first_segment,
    ) else {
        return Vec::new();
    };
    let mut type_candidates = resolve_type_candidates(
        ast,
        match &mut session {
            Some(session) => Some(&mut **session),
            None => None,
        },
        uri,
        active_model,
        line,
        &component_type,
    );

    for member_name in base_segments.iter().skip(1) {
        type_candidates = resolve_member_type_candidates(
            ast,
            match &mut session {
                Some(session) => Some(&mut **session),
                None => None,
            },
            &type_candidates,
            member_name,
        );
        if type_candidates.is_empty() {
            break;
        }
    }

    type_candidates
}

fn resolve_member_type_candidates(
    ast: Option<&ast::StoredDefinition>,
    session: Option<&mut CompletionQuerySession<'_>>,
    owner_candidates: &[String],
    member_name: &str,
) -> Vec<String> {
    let mut seen = HashSet::<String>::new();
    let mut resolved = Vec::<String>::new();
    let push = |name: String, seen: &mut HashSet<String>, resolved: &mut Vec<String>| {
        if !name.is_empty() && seen.insert(name.clone()) {
            resolved.push(name);
        }
    };

    if let Some(session) = session {
        for owner_candidate in owner_candidates {
            let Some((declaring_class, raw_member_type)) =
                session.class_component_member_info_query(owner_candidate, member_name)
            else {
                continue;
            };
            for candidate in session
                .class_type_resolution_candidates_in_class_query(&declaring_class, &raw_member_type)
            {
                push(candidate, &mut seen, &mut resolved);
            }
        }
        if !resolved.is_empty() {
            return resolved;
        }
    }

    let Some(ast) = ast else {
        return resolved;
    };
    for owner_candidate in owner_candidates {
        let Some((declaring_class, raw_member_type)) =
            parsed_class_component_member_info(ast, owner_candidate, member_name)
        else {
            continue;
        };
        for candidate in
            parsed_class_type_resolution_candidates(ast, &declaring_class, &raw_member_type)
        {
            push(candidate, &mut seen, &mut resolved);
        }
    }

    resolved
}

fn parsed_class_component_member_info(
    ast: &ast::StoredDefinition,
    class_name: &str,
    component_name: &str,
) -> Option<(String, String)> {
    let (qualified_name, class) = resolve_parsed_class_candidate(ast, class_name)?;
    let mut visiting = HashSet::<String>::new();
    parsed_class_component_member_info_in_class(
        ast,
        &qualified_name,
        class,
        component_name,
        &mut visiting,
    )
}

fn parsed_class_component_member_info_in_class(
    ast: &ast::StoredDefinition,
    qualified_name: &str,
    class: &ast::ClassDef,
    component_name: &str,
    visiting: &mut HashSet<String>,
) -> Option<(String, String)> {
    if !visiting.insert(qualified_name.to_string()) {
        return None;
    }

    let mut inherited = None;
    for ext in &class.extends {
        let base_candidates = parsed_class_type_resolution_candidates(
            ast,
            qualified_name,
            &ext.base_name.to_string(),
        );
        for candidate in base_candidates {
            let Some((base_qualified, base_class)) =
                resolve_parsed_class_candidate(ast, &candidate)
            else {
                continue;
            };
            if let Some(info) = parsed_class_component_member_info_in_class(
                ast,
                &base_qualified,
                base_class,
                component_name,
                visiting,
            ) {
                inherited = Some(info);
                break;
            }
        }
        if ext.break_names.iter().any(|name| name == component_name) {
            inherited = None;
        }
    }

    let local = class
        .components
        .get(component_name)
        .map(|component| (qualified_name.to_string(), component.type_name.to_string()));
    visiting.remove(qualified_name);
    local.or(inherited)
}

fn parsed_class_type_resolution_candidates(
    ast: &ast::StoredDefinition,
    class_name: &str,
    raw_type_name: &str,
) -> Vec<String> {
    let Some((qualified_name, class)) = resolve_parsed_class_candidate(ast, class_name) else {
        return if raw_type_name.is_empty() {
            Vec::new()
        } else {
            vec![raw_type_name.to_string()]
        };
    };
    parsed_class_type_resolution_candidates_in_class(&qualified_name, class, raw_type_name)
}

fn parsed_class_type_resolution_candidates_in_class(
    qualified_name: &str,
    class: &ast::ClassDef,
    raw_type_name: &str,
) -> Vec<String> {
    if raw_type_name.is_empty() {
        return Vec::new();
    }
    let mut seen = HashSet::<String>::new();
    let mut candidates = Vec::<String>::new();
    let push = |name: String, seen: &mut HashSet<String>, candidates: &mut Vec<String>| {
        if !name.is_empty() && seen.insert(name.clone()) {
            candidates.push(name);
        }
    };

    if raw_type_name.contains('.') {
        push(raw_type_name.to_string(), &mut seen, &mut candidates);
        return candidates;
    }

    if class.classes.contains_key(raw_type_name) {
        push(
            format!("{qualified_name}.{raw_type_name}"),
            &mut seen,
            &mut candidates,
        );
    }

    for import in &class.imports {
        match import {
            ast::Import::Qualified { path, .. } => {
                let full = path.to_string();
                if import_simple_name(&full) == raw_type_name {
                    push(full, &mut seen, &mut candidates);
                }
            }
            ast::Import::Renamed { alias, path, .. } => {
                if alias.text.as_ref() == raw_type_name {
                    push(path.to_string(), &mut seen, &mut candidates);
                }
            }
            ast::Import::Selective { path, names, .. } => {
                if names.iter().any(|name| name.text.as_ref() == raw_type_name) {
                    push(
                        format!("{}.{}", path, raw_type_name),
                        &mut seen,
                        &mut candidates,
                    );
                }
            }
            ast::Import::Unqualified { path, .. } => {
                push(
                    format!("{}.{}", path, raw_type_name),
                    &mut seen,
                    &mut candidates,
                );
            }
        }
    }

    push(raw_type_name.to_string(), &mut seen, &mut candidates);
    candidates
}

fn scoped_component_type(
    ast: Option<&ast::StoredDefinition>,
    session: Option<&mut CompletionQuerySession<'_>>,
    uri: Option<&str>,
    active_model: Option<&str>,
    line: u32,
    component_name: &str,
) -> Option<String> {
    if let Some(session) = session
        && let Some(component_type) = match (uri, active_model) {
            (Some(uri), Some(active_model)) => {
                session.class_component_type_query(uri, active_model, component_name)
            }
            _ => None,
        }
    {
        return Some(component_type);
    }

    let class = ast.and_then(|tree| find_enclosing_class(tree, line))?;
    class
        .components
        .get(component_name)
        .map(|component| component.type_name.to_string())
}

fn import_simple_name(path: &str) -> &str {
    path.rsplit('.').next().unwrap_or(path)
}

fn modifier_completions(partial: &str) -> Vec<CompletionItem> {
    let modifiers = [
        ("start", "Initial value"),
        ("fixed", "Whether initial value is fixed"),
        ("min", "Minimum value"),
        ("max", "Maximum value"),
        ("nominal", "Nominal value for scaling"),
        ("unit", "Physical unit"),
        ("displayUnit", "Display unit"),
        ("quantity", "Physical quantity name"),
        ("stateSelect", "State selection hint"),
    ];

    modifiers
        .iter()
        .filter(|(label, _)| partial.is_empty() || label.starts_with(partial))
        .map(|(label, detail)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::PROPERTY),
            detail: Some(detail.to_string()),
            insert_text: Some(format!("{} = ", label)),
            ..Default::default()
        })
        .collect()
}

fn query_local_completions(
    session: &mut CompletionQuerySession<'_>,
    uri: &str,
    active_model: &str,
    partial: &str,
) -> Vec<CompletionItem> {
    session
        .class_local_completion_items_query(uri, active_model)
        .into_iter()
        .filter(|item| partial.is_empty() || item.name.starts_with(partial))
        .map(|item| CompletionItem {
            label: item.name,
            kind: Some(query_local_completion_kind(item.kind)),
            detail: Some(item.detail),
            ..Default::default()
        })
        .collect()
}

fn query_local_completion_kind(kind: ClassLocalCompletionKind) -> CompletionItemKind {
    match kind {
        ClassLocalCompletionKind::Constant => CompletionItemKind::CONSTANT,
        ClassLocalCompletionKind::Property => CompletionItemKind::PROPERTY,
        ClassLocalCompletionKind::Variable => CompletionItemKind::VARIABLE,
        ClassLocalCompletionKind::Class => CompletionItemKind::CLASS,
    }
}

fn ast_local_completions(
    ast: &ast::StoredDefinition,
    line: u32,
    partial: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    if let Some(class) = find_enclosing_class(ast, line) {
        for (name, comp) in &class.components {
            if !partial.is_empty() && !name.starts_with(partial) {
                continue;
            }
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(component_completion_kind(comp)),
                detail: Some(comp.type_name.to_string()),
                ..Default::default()
            });
        }
        // Also suggest nested class names as types
        for (name, nested) in &class.classes {
            if !partial.is_empty() && !name.starts_with(partial) {
                continue;
            }
            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some(format!("{:?}", nested.class_type)),
                ..Default::default()
            });
        }
    }
    items
}

fn builtin_completions(partial: &str) -> Vec<CompletionItem> {
    rumoca_core::BUILTIN_FUNCTIONS
        .iter()
        .filter(|name| {
            !partial.is_empty() && name.starts_with(partial) && !rumoca_core::is_builtin_type(name)
        })
        .map(|name| CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("Built-in function".to_string()),
            ..Default::default()
        })
        .collect()
}

fn keyword_completions(partial: &str) -> Vec<CompletionItem> {
    let keywords = [
        ("model", "Define a model class"),
        ("package", "Define a package"),
        ("function", "Define a function"),
        ("block", "Define a block class"),
        ("connector", "Define a connector class"),
        ("record", "Define a record class"),
        ("type", "Define a type alias"),
        ("class", "Define a general class"),
        ("operator", "Define an operator class/operator record"),
        ("equation", "Equation section"),
        ("algorithm", "Algorithm section"),
        ("parameter", "Parameter declaration prefix"),
        ("constant", "Constant declaration prefix"),
        ("input", "Input causality prefix"),
        ("output", "Output causality prefix"),
        ("extends", "Inherit from base class"),
        ("import", "Import declarations"),
        ("if", "Conditional expression/equation"),
        ("for", "For-loop"),
        ("when", "Event handling"),
        ("while", "While loop (in algorithms)"),
        ("der", "Time derivative operator"),
        ("connect", "Connect two connectors"),
        ("Real", "Real number type"),
        ("Integer", "Integer number type"),
        ("Boolean", "Boolean type"),
        ("String", "String type"),
    ];

    keywords
        .iter()
        .filter(|(label, _)| partial.is_empty() || label.starts_with(partial))
        .map(|(label, detail)| CompletionItem {
            label: label.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some(detail.to_string()),
            insert_text: Some(label.to_string()),
            ..Default::default()
        })
        .collect()
}

#[cfg(test)]
mod tests;
