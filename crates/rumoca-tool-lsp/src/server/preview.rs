use super::*;
use crate::helpers::location_to_range_in_optional_source;

pub(super) fn is_hover_preview_candidate(ast: &ast::StoredDefinition, word: &str) -> bool {
    ast.classes.get(word).is_some_and(|class| {
        matches!(
            class.class_type,
            rumoca_compile::parsing::ir_core::ClassType::Model
                | rumoca_compile::parsing::ir_core::ClassType::Block
                | rumoca_compile::parsing::ir_core::ClassType::Class
        )
    })
}

/// Build a goto-definition response for a class declaration.
///
/// `target_source` is the text of the *declaration's* file when it is loaded;
/// it is what turns the location's byte span into UTF-16 LSP columns. A target
/// in an unopened file falls back to the lexer's character columns.
/// [`class_target_definition`] with the target file's text pulled from the
/// session snapshot, which is where its UTF-16 columns are measured.
pub(super) fn class_target_definition_in_snapshot(
    snapshot: &SessionSnapshot,
    target_uri: &str,
    declaration_location: &rumoca_compile::parsing::ir_core::Location,
    fallback_uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let source = snapshot.get_document(target_uri).map(|doc| doc.content);
    class_target_definition(
        target_uri,
        declaration_location,
        fallback_uri,
        source.as_deref(),
    )
}

pub(super) fn class_target_definition(
    target_uri: &str,
    declaration_location: &rumoca_compile::parsing::ir_core::Location,
    fallback_uri: &Url,
    target_source: Option<&str>,
) -> Option<GotoDefinitionResponse> {
    let target_uri = Url::from_file_path(target_uri)
        .ok()
        .unwrap_or_else(|| fallback_uri.clone());
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: target_uri,
        range: location_to_range_in_optional_source(target_source, declaration_location),
    }))
}

pub(super) fn class_target_hover(
    info: &rumoca_compile::compile::NavigationClassTargetInfo,
) -> Hover {
    let mut value = format!(
        "```modelica\n{} {}\n```",
        class_type_keyword(&info.class_type),
        info.class_name
    );
    if let Some(description) = &info.description {
        value.push_str(&format!("\n\n{description}"));
    }
    if info.component_count > 0 || info.equation_count > 0 {
        value.push_str(&format!(
            "\n\n{} components, {} equations",
            info.component_count, info.equation_count
        ));
    }
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: None,
    }
}

pub(super) fn local_component_hover(info: &rumoca_compile::compile::LocalComponentInfo) -> Hover {
    let mut parts = Vec::new();
    if let Some(keyword_prefix) = &info.keyword_prefix {
        parts.push(keyword_prefix.clone());
    }
    parts.push(info.type_name.clone());
    let mut name = info.name.clone();
    if !info.shape.is_empty() {
        let dims = info
            .shape
            .iter()
            .map(|dim| dim.to_string())
            .collect::<Vec<_>>();
        name = format!("{name}[{}]", dims.join(", "));
    }
    parts.push(name);
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```modelica\n{}\n```", parts.join(" ")),
        }),
        range: None,
    }
}

fn class_type_keyword(class_type: &rumoca_compile::parsing::ir_core::ClassType) -> &'static str {
    match class_type {
        rumoca_compile::parsing::ir_core::ClassType::Model => "model",
        rumoca_compile::parsing::ir_core::ClassType::Block => "block",
        rumoca_compile::parsing::ir_core::ClassType::Connector => "connector",
        rumoca_compile::parsing::ir_core::ClassType::Record => "record",
        rumoca_compile::parsing::ir_core::ClassType::Type => "type",
        rumoca_compile::parsing::ir_core::ClassType::Package => "package",
        rumoca_compile::parsing::ir_core::ClassType::Function => "function",
        rumoca_compile::parsing::ir_core::ClassType::Class => "class",
        rumoca_compile::parsing::ir_core::ClassType::Operator => "operator",
    }
}

/// Render the hover markdown for an already-compiled model.
///
/// Pure formatting: the compile itself is owned by
/// [`super::preview_cache`], which caches results and runs off the session
/// write lock.
pub(super) fn render_flattened_preview(
    model_name: &str,
    result: &rumoca_compile::compile::DaeCompilationResult,
) -> String {
    let dae = &result.dae;
    let mut lines = vec![format!(
        "model={model_name} | f_x={} | f_z={} | f_m={} | m={} | balance={}",
        dae.continuous.equations.len(),
        dae.discrete.real_updates.len(),
        dae.discrete.valued_updates.len(),
        dae.variables.discrete_valued.len(),
        result.balance_detail.balance()
    )];
    push_equation_block(
        &mut lines,
        "f_x",
        &dae.continuous.equations,
        6,
        |idx, eq| render_equation_line(idx, eq.lhs.as_ref().map(ToString::to_string), &eq.rhs),
    );
    push_equation_block(
        &mut lines,
        "f_z",
        &dae.discrete.real_updates,
        4,
        |idx, eq| render_equation_line(idx, eq.lhs.as_ref().map(ToString::to_string), &eq.rhs),
    );
    push_equation_block(
        &mut lines,
        "f_m",
        &dae.discrete.valued_updates,
        4,
        |idx, eq| render_equation_line(idx, eq.lhs.as_ref().map(ToString::to_string), &eq.rhs),
    );
    let discrete_valued = &dae.variables.discrete_valued;
    push_discrete_valued_block(
        &mut lines,
        discrete_valued.len(),
        discrete_valued.iter(),
        6,
        |var| match var.start.as_ref() {
            Some(expr) => truncate_debug(expr, 80),
            None => "<none>".to_string(),
        },
    );

    format!(
        "**Flattened DAE Preview**\n\n```text\n{}\n```",
        lines.join("\n")
    )
}

fn render_equation_line<R: std::fmt::Debug>(idx: usize, lhs: Option<String>, rhs: &R) -> String {
    let lhs = match lhs {
        Some(rendered) => rendered,
        None => "0".to_string(),
    };
    format!("  {idx}: {lhs} = {}", truncate_debug(rhs, 140))
}

fn push_equation_block<E>(
    lines: &mut Vec<String>,
    label: &str,
    equations: &[E],
    limit: usize,
    render: impl Fn(usize, &E) -> String,
) {
    lines.push(format!("{label} ({}):", equations.len()));
    for (idx, eq) in equations.iter().take(limit).enumerate() {
        lines.push(render(idx, eq));
    }
    push_more_equations_line(lines, equations.len(), limit, label);
}

fn push_discrete_valued_block<'a, K, V, I>(
    lines: &mut Vec<String>,
    total: usize,
    entries: I,
    limit: usize,
    render_start: impl Fn(&V) -> String,
) where
    K: std::fmt::Display + 'a,
    V: 'a,
    I: Iterator<Item = (&'a K, &'a V)>,
{
    if total == 0 {
        return;
    }
    lines.push("m (discrete-valued variables):".to_string());
    for (idx, (name, var)) in entries.take(limit).enumerate() {
        lines.push(format!("{idx}: {name} start={}", render_start(var)));
    }
    if total > limit {
        lines.push(format!(
            "... {} more discrete-valued variables",
            total - limit
        ));
    }
}

fn push_more_equations_line(lines: &mut Vec<String>, total: usize, shown: usize, label: &str) {
    if total > shown {
        lines.push(format!("  ... {} more {label} equations", total - shown));
    }
}

fn truncate_debug<T: std::fmt::Debug>(value: &T, max_chars: usize) -> String {
    let rendered = format!("{value:?}");
    if rendered.chars().count() <= max_chars {
        return rendered;
    }
    let mut out = rendered.chars().take(max_chars).collect::<String>();
    out.push_str("...");
    out
}

pub(super) fn append_markdown_hover(existing: Option<Hover>, extra_markdown: &str) -> Hover {
    let mut merged = String::new();
    if let Some(hover) = existing {
        match hover.contents {
            HoverContents::Markup(markup) => merged.push_str(&markup.value),
            HoverContents::Scalar(marked) => merged.push_str(&marked_string_to_markdown(marked)),
            HoverContents::Array(items) => {
                let joined = items
                    .into_iter()
                    .map(marked_string_to_markdown)
                    .collect::<Vec<_>>()
                    .join("\n\n");
                merged.push_str(&joined);
            }
        }
    }
    if !merged.is_empty() {
        merged.push_str("\n\n");
    }
    merged.push_str(extra_markdown);
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: merged,
        }),
        range: None,
    }
}

pub(super) fn marked_string_to_markdown(marked: MarkedString) -> String {
    match marked {
        MarkedString::String(s) => s,
        MarkedString::LanguageString(ls) => format!("```{}\n{}\n```", ls.language, ls.value),
    }
}
