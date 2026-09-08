//! Jacobian synthesis: forward-mode differentiation of Modelica functions.
//!
//! A `jacobian(f(a, b), a)` call is recognized after parsing, expanded into a
//! plain generated Modelica function, and replaced by a call to it. The
//! expansion happens before any flattening, so every later phase, every
//! target, and OpenModelica all see ordinary Modelica.
//!
//! [`expand_source`] is the only entry: the expansion is a source-to-source
//! rewrite, so the text the compiler stores and parses and the text
//! `--emit-standard-modelica` writes are the same bytes and cannot drift.
//!
//! Everything outside the stated rules refuses with a [`Refusal`] naming the
//! rule, rather than differentiating something it cannot state exactly.

use std::fmt;

use rumoca_core::Span;
use rumoca_ir_ast as ast;

pub mod admission;

mod actuals;
mod builtins;
mod emit;
mod engine;
mod model;
mod refusal;
mod scope;
mod sites;

#[cfg(test)]
mod tests;

pub use refusal::{Refusable, Refusal, Rule, Site};

use model::{FunctionModel, PortRole};
use scope::FunctionScope;

/// A function minted by expansion, and where its text goes.
#[derive(Debug, Clone)]
pub(crate) struct Minted {
    /// Declared name of the generated function.
    pub name: String,
    /// Class path that owns it: the differentiated function's own siblings.
    pub owner: Vec<String>,
    /// Complete generated Modelica text.
    pub text: String,
    /// End offset of the class this function is minted next to.
    pub anchor_end: u32,
}

/// The complete expansion of one parsed file.
#[derive(Debug, Clone, Default)]
pub(crate) struct Expansion {
    /// Generated functions, in mint order.
    pub minted: Vec<Minted>,
    /// Call rewrites, one per recognized call site.
    pub rewrites: Vec<Rewrite>,
}

/// One recognized call and the call text it expands to.
#[derive(Debug, Clone)]
pub(crate) struct Rewrite {
    /// Span of the whole `jacobian` call.
    pub span: Span,
    /// The complete replacement call text.
    pub text: String,
}

/// A cheap check that a source text can possibly need expansion.
///
/// The check is a scan, not a parse, so the overwhelming majority of sources
/// pay nothing. It looks for the surface name spelled as its own identifier
/// and followed by an argument list: `right_jacobian(` and `Lib.jacobian(` are
/// other names and do not wake the expander.
pub fn may_expand(source: &str) -> bool {
    source
        .match_indices(sites::SURFACE)
        .any(|(offset, _)| heads_a_call(source, offset))
}

/// Whether the surface name at `offset` stands alone and opens a call.
fn heads_a_call(source: &str, offset: usize) -> bool {
    let preceding = source[..offset].chars().next_back();
    if preceding.is_some_and(|character| {
        character.is_alphanumeric() || character == '_' || character == '.'
    }) {
        return false;
    }
    source
        .get(offset + sites::SURFACE.len()..)
        .and_then(|tail| tail.chars().find(|character| !character.is_whitespace()))
        == Some('(')
}

/// Plan the expansion of a parsed file without changing it.
pub(crate) fn plan(definition: &ast::StoredDefinition, file: &str) -> Refusable<Expansion> {
    let sites = sites::collect(definition, file)?;
    let mut expansion = Expansion::default();
    for site in &sites {
        let wrapper = synthesize(definition, file, site, &mut expansion)?;
        let (prefix, _) = site.callee.split_at(site.callee.len().saturating_sub(1));
        let qualified: Vec<&str> = prefix
            .iter()
            .map(String::as_str)
            .chain(std::iter::once(wrapper.as_str()))
            .collect();
        expansion.rewrites.push(Rewrite {
            span: site.span,
            text: format!("{}({})", qualified.join("."), site.arguments.join(", ")),
        });
    }
    Ok(expansion)
}

/// Expand a source text into portable standard Modelica.
pub fn expand_source(source: &str, file: &str) -> Result<String, ExpansionError> {
    let definition = rumoca_phase_parse::parse_to_ast(source, file)
        .map_err(|error| ExpansionError::Parse(error.to_string()))?;
    let expansion = plan(&definition, file)?;
    Ok(apply_to_text(source, &expansion))
}

/// A failure of [`expand_source`].
#[derive(Debug)]
pub enum ExpansionError {
    /// The source did not parse.
    Parse(String),
    /// A construct was refused.
    Refused(Refusal),
}

impl From<Refusal> for ExpansionError {
    fn from(refusal: Refusal) -> Self {
        Self::Refused(refusal)
    }
}

impl fmt::Display for ExpansionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parse(message) => write!(formatter, "{message}"),
            Self::Refused(refusal) => write!(formatter, "{refusal}"),
        }
    }
}

impl std::error::Error for ExpansionError {}

/// Apply an expansion to the source text it was planned from.
fn apply_to_text(source: &str, expansion: &Expansion) -> String {
    let mut edits: Vec<(usize, usize, String)> = Vec::new();
    for rewrite in &expansion.rewrites {
        let start = rewrite.span.start.0;
        let end = rewrite.span.end.0;
        if source.get(start..end).is_none() {
            continue;
        }
        edits.push((start, end, rewrite.text.clone()));
    }
    for minted in &expansion.minted {
        let at = insertion_point(source, minted.anchor_end as usize);
        edits.push((at, at, format!("\n\n{}", minted.text)));
    }
    edits.sort_by(|left, right| right.0.cmp(&left.0).then(right.1.cmp(&left.1)));
    let mut expanded = source.to_string();
    for (start, end, text) in edits {
        if start <= end && end <= expanded.len() {
            expanded.replace_range(start..end, &text);
        }
    }
    expanded
}

/// Where generated text goes after the class ending at `anchor_end`.
fn insertion_point(source: &str, anchor_end: usize) -> usize {
    let tail = source.get(anchor_end..).unwrap_or("");
    let mut offset = anchor_end;
    for character in tail.chars() {
        if character == ';' {
            return offset + character.len_utf8();
        }
        if !character.is_whitespace() {
            return anchor_end;
        }
        offset += character.len_utf8();
    }
    anchor_end
}

/// Synthesize everything one call site needs, returning the wrapper's name.
fn synthesize(
    definition: &ast::StoredDefinition,
    file: &str,
    site: &sites::CallSite,
    expansion: &mut Expansion,
) -> Refusable<String> {
    let at = Site::at(file, Some(&site.location));
    let scope = FunctionScope::reaching(definition, &site.owner);
    let Some((class, owner)) = scope.locate(&site.callee) else {
        return Err(Refusal::new(
            Rule::CalleeLookup,
            at,
            format!(
                "`{}` is not a function declared in this file",
                site.callee.join(".")
            ),
        ));
    };
    let model = FunctionModel::read(class, &scope, &owner, &at)?;
    let formal = differentiated_formal(&model, site, &at)?;
    // A shape the wrapper states has to be a shape the call really produces.
    // Everything below reads declarations, and a call whose actual is wider
    // than its formal is evaluated once per element instead (MLS 12.4.6).
    actuals::check_actual_ranks(definition, site, &model, &at)?;
    let provenance = provenance_text(site, &at);
    let wrapper = emit::jacobian_wrapper(&model, &formal, &at, &provenance)?;
    let name = wrapper.name.clone();
    record(expansion, &owner, wrapper, class.location.end);
    synthesize_tangents(definition, &model, &owner, &at, expansion)?;
    Ok(name)
}

/// The formal input the named argument selects.
fn differentiated_formal(
    model: &FunctionModel,
    site: &sites::CallSite,
    at: &Site,
) -> Refusable<model::Port> {
    let inputs: Vec<&model::Port> = model.ports_with(PortRole::Input).collect();
    if inputs.len() != site.arguments.len() {
        return Err(Refusal::new(
            Rule::CallForm,
            at.clone(),
            format!(
                "`{}` takes {} inputs but the call passes {}",
                model.name,
                inputs.len(),
                site.arguments.len()
            ),
        ));
    }
    let position = site
        .arguments
        .iter()
        .position(|text| *text == site.argument)
        .ok_or_else(|| {
            Refusal::new(
                Rule::CallForm,
                at.clone(),
                format!("`{}` is not an argument of `{}`", site.argument, model.name),
            )
        })?;
    let formal = inputs[position].clone();
    if formal.kind != model::ValueKind::Differentiable {
        return Err(Refusal::new(
            Rule::DifferentiableType,
            at.clone(),
            format!(
                "`{}` is declared {} and is not differentiable",
                formal.name,
                formal.type_text.trim()
            ),
        ));
    }
    Ok(formal)
}

/// Mint the tangent of `model` and, transitively, of everything it calls.
fn synthesize_tangents(
    definition: &ast::StoredDefinition,
    model: &FunctionModel,
    owner: &[String],
    at: &Site,
    expansion: &mut Expansion,
) -> Refusable<()> {
    let mut pending = vec![(model.clone(), owner.to_vec())];
    let mut calls: Vec<(String, Vec<String>)> = Vec::new();
    while let Some((model, owner)) = pending.pop() {
        let name = emit::tangent_name(&model.name);
        if expansion
            .minted
            .iter()
            .any(|minted| minted.name == name && minted.owner == owner)
        {
            continue;
        }
        let scope = FunctionScope::reaching(definition, &owner);
        let provenance = format!("forward tangent of {}, {}", model.name, at);
        let generated = emit::tangent_function(&model, &scope, at, &provenance)?;
        let anchor = scope
            .locate(std::slice::from_ref(&model.name))
            .map_or(0, |(class, _)| class.location.end);
        calls.push((model.name.clone(), generated.requested.clone()));
        for requested in &generated.requested {
            let Some((class, callee_owner)) = scope.locate(std::slice::from_ref(requested)) else {
                return Err(Refusal::new(
                    Rule::CalleeTangent,
                    at.clone(),
                    format!("`{requested}` is not a function declared in this file"),
                ));
            };
            pending.push((
                FunctionModel::read(class, &scope, &callee_owner, at)?,
                callee_owner,
            ));
        }
        record(expansion, &owner, generated, anchor);
    }
    if let Some(cycle) = recursive_function(&calls) {
        return Err(Refusal::new(
            Rule::CalleeTangent,
            at.clone(),
            format!("`{cycle}` is recursive, and a recursive tangent is not synthesized"),
        ));
    }
    Ok(())
}

/// A function that reaches itself through the recorded call edges.
fn recursive_function(calls: &[(String, Vec<String>)]) -> Option<String> {
    calls
        .iter()
        .find(|(start, _)| reaches(calls, start))
        .map(|(start, _)| start.clone())
}

/// Whether `start` reaches itself by following the recorded call edges.
fn reaches(calls: &[(String, Vec<String>)], start: &str) -> bool {
    let mut seen: Vec<&str> = Vec::new();
    let mut frontier: Vec<&str> = vec![start];
    while let Some(current) = frontier.pop() {
        for callee in called_by(calls, current) {
            if callee == start {
                return true;
            }
            if !seen.contains(&callee) {
                seen.push(callee);
                frontier.push(callee);
            }
        }
    }
    false
}

/// The functions one function's tangent calls.
fn called_by<'calls>(
    calls: &'calls [(String, Vec<String>)],
    caller: &str,
) -> impl Iterator<Item = &'calls str> {
    calls
        .iter()
        .filter(move |(recorded, _)| recorded == caller)
        .flat_map(|(_, called)| called.iter().map(String::as_str))
}

fn record(
    expansion: &mut Expansion,
    owner: &[String],
    generated: emit::Generated,
    anchor_end: u32,
) {
    if expansion
        .minted
        .iter()
        .any(|minted| minted.name == generated.name && minted.owner == owner)
    {
        return;
    }
    expansion.minted.push(Minted {
        name: generated.name,
        owner: owner.to_vec(),
        text: generated.text,
        anchor_end,
    });
}

fn provenance_text(site: &sites::CallSite, at: &Site) -> String {
    format!(
        "jacobian({}({}), {}) at {}",
        site.callee.join("."),
        site.arguments.join(", "),
        site.argument,
        at
    )
}
