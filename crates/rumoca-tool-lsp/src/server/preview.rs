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
    let lines = result.dae.inspect(|view| {
        let discrete_values = view
            .variables()
            .filter(|(_, variable)| {
                variable.role() == rumoca_compile::compile::VariableRole::DiscreteValue
            })
            .collect::<Vec<_>>();
        let mut lines = vec![format!(
            "model={model_name} | f_x={} | f_z={} | f_m={} | m={} | balance={}",
            view.continuous_owner_count(),
            view.discrete_real_equation_count(),
            view.discrete_value_definition_count(),
            discrete_values.len(),
            result.balance_detail.balance()
        )];
        push_checked_residuals(
            &mut lines,
            "f_x",
            view.continuous_equation_count(),
            6,
            |index| view.continuous_equation(index),
            view,
        );
        push_checked_discrete_real_equations(&mut lines, 4, view);
        lines.push(format!(
            "f_m ({} definitions in {} owners):",
            view.discrete_value_definition_count(),
            view.discrete_value_owner_count()
        ));
        push_discrete_value_owners(&mut lines, view, 4);
        push_more_equations_line(
            &mut lines,
            view.discrete_value_owner_count(),
            4,
            "f_m owners",
        );
        if !discrete_values.is_empty() {
            lines.push("m (discrete-valued variables):".to_string());
            for (index, (_, variable)) in discrete_values.iter().take(6).enumerate() {
                let start = variable
                    .start()
                    .and_then(|expression| view.expression(expression))
                    .and_then(|expression| view.source_text(expression.provenance()))
                    .map_or("<none>".to_string(), |source| truncate_text(source, 80));
                lines.push(format!("{index}: {} start={start}", variable.name()));
            }
            if discrete_values.len() > 6 {
                lines.push(format!(
                    "... {} more discrete-valued variables",
                    discrete_values.len() - 6
                ));
            }
        }
        lines
    });

    format!(
        "**Flattened DAE Preview**\n\n```text\n{}\n```",
        lines.join("\n")
    )
}

fn push_discrete_value_owners(
    lines: &mut Vec<String>,
    view: rumoca_compile::compile::DaeView<'_>,
    limit: usize,
) {
    for index in 0..view.discrete_value_owner_count().min(limit) {
        let id = view
            .discrete_value_owner_id(index)
            .expect("finalized B.1c owner has an identity");
        let owner = view
            .discrete_value_owner(id)
            .expect("branded B.1c owner resolves");
        let targets = owner
            .targets()
            .iter()
            .map(|target| {
                view.variables()
                    .find(|(id, _)| id.index() == target.index())
                    .map_or_else(
                        || format!("<discrete:{}>", target.index()),
                        |(_, variable)| variable.name().to_string(),
                    )
            })
            .collect::<Vec<_>>();
        lines.push(format!(
            "  {index}: [{}] := {} ordered branch(es) | owner {} at `{}`",
            targets.join(", "),
            owner.branches().len(),
            owner.provenance().origin(),
            provenance_text(view, owner.provenance())
        ));
        for (branch_index, branch) in owner.branches().iter().enumerate() {
            let activation = match branch.activation() {
                rumoca_compile::compile::DiscreteBranchActivation::Always => "always".to_string(),
                rumoca_compile::compile::DiscreteBranchActivation::When { trigger, guard } => {
                    format!(
                        "when trigger=`{}` guard=`{}`",
                        condition_text(view, trigger),
                        condition_text(view, guard)
                    )
                }
            };
            lines.push(format!(
                "    branch {branch_index}: {activation} | {} at `{}`",
                branch.provenance().origin(),
                provenance_text(view, branch.provenance())
            ));
            for (target, (value, action)) in targets.iter().zip(branch.values().iter()) {
                let value_source = expression_provenance_text(view, value);
                lines.push(format!(
                    "      {target} := `{value_source}` | action {} at `{}`",
                    action.origin(),
                    provenance_text(view, action)
                ));
            }
        }
    }
}

fn expression_provenance_text<'dae>(
    view: rumoca_compile::compile::DaeView<'dae>,
    expression: rumoca_compile::compile::ExprId<'dae>,
) -> String {
    view.expression(expression)
        .map_or("<expression unavailable>".to_string(), |expression| {
            provenance_text(view, expression.provenance())
        })
}

fn condition_text<'dae>(
    view: rumoca_compile::compile::DaeView<'dae>,
    condition: rumoca_compile::compile::ConditionId<'dae>,
) -> String {
    view.condition(condition).map_or_else(
        || format!("<condition:{}>", condition.index()),
        |condition| provenance_text(view, condition.provenance()),
    )
}

fn provenance_text(
    view: rumoca_compile::compile::DaeView<'_>,
    provenance: rumoca_compile::compile::DaeProvenance,
) -> String {
    view.source_text(provenance)
        .map_or("<source unavailable>".to_string(), |source| {
            truncate_text(source, 100)
        })
}

fn push_checked_residuals<'dae>(
    lines: &mut Vec<String>,
    label: &str,
    total: usize,
    limit: usize,
    equation: impl Fn(usize) -> Option<rumoca_compile::compile::ResidualEquationView<'dae>>,
    view: rumoca_compile::compile::DaeView<'dae>,
) {
    lines.push(format!("{label} ({total}):"));
    for index in 0..total.min(limit) {
        let equation = equation(index).expect("finalized residual equation resolves");
        let source = view
            .expression(equation.residual())
            .and_then(|expression| view.source_text(expression.provenance()))
            .map_or("<generated residual>", |source| source);
        lines.push(format!("  {index}: {}", truncate_text(source, 140)));
    }
    push_more_equations_line(lines, total, limit, label);
}

fn push_checked_discrete_real_equations(
    lines: &mut Vec<String>,
    limit: usize,
    view: rumoca_compile::compile::DaeView<'_>,
) {
    let total = view.discrete_real_equation_count();
    lines.push(format!("f_z ({total}):"));
    for index in 0..total.min(limit) {
        let equation = view
            .discrete_real_equation(index)
            .expect("finalized discrete Real equation resolves");
        let source = view
            .expression(equation.residual())
            .and_then(|expression| view.source_text(expression.provenance()))
            .map_or("<generated residual>", |source| source);
        let activation = match equation.activation() {
            rumoca_compile::compile::DiscreteRealActivation::Always => "always".to_string(),
            rumoca_compile::compile::DiscreteRealActivation::When { trigger, guard } => format!(
                "when trigger=`{}` guard=`{}`",
                condition_text(view, trigger),
                condition_text(view, guard)
            ),
        };
        lines.push(format!(
            "  {index}: {} | {activation} | owner {} at `{}`",
            truncate_text(source, 140),
            equation.provenance().origin(),
            provenance_text(view, equation.provenance())
        ));
    }
    push_more_equations_line(lines, total, limit, "f_z");
}

fn push_more_equations_line(lines: &mut Vec<String>, total: usize, shown: usize, label: &str) {
    if total > shown {
        lines.push(format!("  ... {} more {label} equations", total - shown));
    }
}

fn truncate_text(value: &str, max_chars: usize) -> String {
    if value.chars().count() <= max_chars {
        return value.to_string();
    }
    let mut out = value.chars().take(max_chars).collect::<String>();
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
