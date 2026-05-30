use super::*;
use crate::helpers::location_to_range;

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

pub(super) fn class_target_definition(
    target_uri: &str,
    declaration_location: &rumoca_compile::parsing::ir_core::Location,
    fallback_uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let target_uri = Url::from_file_path(target_uri)
        .ok()
        .unwrap_or_else(|| fallback_uri.clone());
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: target_uri,
        range: location_to_range(declaration_location),
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

pub(super) fn flattened_preview_for_model(
    session: &mut Session,
    model_name: &str,
) -> Option<String> {
    let mut report = session.compile_model_strict_reachable_uncached_with_recovery(model_name);
    if !report.requested_succeeded() {
        return None;
    }
    let Some(PhaseResult::Success(result)) = report.requested_result.take() else {
        return None;
    };

    let mut lines = Vec::new();
    lines.push(format!(
        "model={model_name} | f_x={} | f_z={} | f_m={} | m={} | balance={}",
        result.dae.continuous.equations.len(),
        result.dae.discrete.real_updates.len(),
        result.dae.discrete.valued_updates.len(),
        result.dae.variables.discrete_valued.len(),
        dae_balance(&result.dae)
    ));
    lines.push(format!("f_x ({}):", result.dae.continuous.equations.len()));
    for (idx, eq) in result.dae.continuous.equations.iter().take(6).enumerate() {
        let lhs = eq
            .lhs
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "0".to_string());
        let rhs = truncate_debug(&eq.rhs, 140);
        lines.push(format!("  {idx}: {lhs} = {rhs}"));
    }
    push_more_equations_line(&mut lines, result.dae.continuous.equations.len(), 6, "f_x");
    lines.push(format!("f_z ({}):", result.dae.discrete.real_updates.len()));
    for (idx, eq) in result.dae.discrete.real_updates.iter().take(4).enumerate() {
        let lhs = eq
            .lhs
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "0".to_string());
        let rhs = truncate_debug(&eq.rhs, 140);
        lines.push(format!("  {idx}: {lhs} = {rhs}"));
    }
    push_more_equations_line(&mut lines, result.dae.discrete.real_updates.len(), 4, "f_z");
    lines.push(format!(
        "f_m ({}):",
        result.dae.discrete.valued_updates.len()
    ));
    for (idx, eq) in result
        .dae
        .discrete
        .valued_updates
        .iter()
        .take(4)
        .enumerate()
    {
        let lhs = eq
            .lhs
            .as_ref()
            .map(ToString::to_string)
            .unwrap_or_else(|| "0".to_string());
        let rhs = truncate_debug(&eq.rhs, 140);
        lines.push(format!("  {idx}: {lhs} = {rhs}"));
    }
    push_more_equations_line(
        &mut lines,
        result.dae.discrete.valued_updates.len(),
        4,
        "f_m",
    );
    if !result.dae.variables.discrete_valued.is_empty() {
        lines.push("m (discrete-valued variables):".to_string());
        for (idx, (name, var)) in result
            .dae
            .variables
            .discrete_valued
            .iter()
            .take(6)
            .enumerate()
        {
            let start = var
                .start
                .as_ref()
                .map(|expr| truncate_debug(expr, 80))
                .unwrap_or_else(|| "<none>".to_string());
            lines.push(format!("{idx}: {name} start={start}"));
        }
        if result.dae.variables.discrete_valued.len() > 6 {
            lines.push(format!(
                "... {} more discrete-valued variables",
                result.dae.variables.discrete_valued.len() - 6
            ));
        }
    }

    Some(format!(
        "**Flattened DAE Preview**\n\n```text\n{}\n```",
        lines.join("\n")
    ))
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
