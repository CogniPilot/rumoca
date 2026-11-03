//! Print-plan template gate: the schema-independent half of the print-plan lint
//! contract (Rule 25, "backends are dumb").
//!
//! A covered template must parse, with the renderer's own MiniJinja version and
//! default syntax, to the contract's restricted AST, across the closure of its
//! static includes, imports and extends. Its literal target text must not hold
//! an executable body. Whether each printed field is really issued by the checked
//! plan is the print-plan schema and conformance gate's job, not this one's;
//! neither check alone shows that a backend only prints.
//!
//! Templates are covered once their consumer migrates to the print plan. Legacy
//! templates stay outside this gate and are tracked by the migration ledger, so a
//! pass here says nothing about them.

mod ast_rules;
mod literal_c;
#[cfg(test)]
mod tests;

use anyhow::{Context, Result, bail};
use minijinja::machinery::ast::Stmt;
use minijinja::machinery::{WhitespaceConfig, parse};
use minijinja::syntax::SyntaxConfig;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

/// Artifact templates whose consumer prints only an issued print plan, relative
/// to the codegen template directory. A migration adds its template here.
const COVERED_TEMPLATES: &[&str] = &[];

/// Single macros checked as strict regions inside a template that is otherwise
/// legacy: (template, macro name, literal language). Only that macro's body
/// and its static closure are covered; the rest of the file stays outside the
/// gate and under the migration ledger's freeze.
const COVERED_MACROS: &[(&str, &str, Language)] = &[
    ("fmi3/typed_functions.jinja", "element", Language::C),
    ("fmi3/typed_functions.jinja", "storage_name", Language::C),
    ("fmi3/typed_functions.jinja", "operand", Language::C),
    ("fmi3/typed_functions.jinja", "integer_literal", Language::C),
    ("fmi3/typed_functions.jinja", "unary_spelling", Language::C),
    ("fmi3/typed_functions.jinja", "binary_spelling", Language::C),
    (
        "fmi3/typed_functions.jinja",
        "compare_spelling",
        Language::C,
    ),
    ("fmi3/typed_functions.jinja", "checked", Language::C),
    ("fmi3/typed_functions.jinja", "domain", Language::C),
    ("fmi3/typed_functions.jinja", "elementwise", Language::C),
    ("fmi3/typed_functions.jinja", "convert", Language::C),
    ("fmi3/typed_functions.jinja", "tensor_item", Language::C),
    ("fmi3/typed_functions.jinja", "slot_frame", Language::C),
    ("fmi3/typed_functions.jinja", "plan_frame", Language::C),
    ("fmi3/typed_functions.jinja", "plan_item", Language::C),
    ("fmi3/typed_functions.jinja", "plan_outputs", Language::C),
];

const TEMPLATE_DIR: &str = "crates/rumoca-phase-codegen/src/templates";

/// The decoded-region registry, relative to the codegen template directory:
/// macro regions whose whole production output a blocking print-plan
/// conformance decoder consumes, with the closed issued tags each may dispatch
/// on. The conformance tests read the same file and prove each row's region is
/// invoked and decoded; registration here only selects the checks.
const REGION_REGISTRY: &str = "fmi3/print_plan_regions.toml";

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Rule {
    Parse,
    Arithmetic,
    Assignment,
    RangeOrSubscript,
    Collection,
    ValueTest,
    Call,
    Macro,
    Fallback,
    LoopControl,
    LoopIntrinsic,
    Composition,
    UnknownNode,
    LiteralExecutable,
    UnsupportedLiteralLanguage,
    UndeclaredRoot,
    RegionSelection,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Violation {
    pub(crate) file: String,
    pub(crate) line: u16,
    pub(crate) column: u16,
    pub(crate) rule: Rule,
    pub(crate) detail: String,
}

impl fmt::Display for Violation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{}: {:?}: {}",
            self.file, self.line, self.column, self.rule, self.detail
        )
    }
}

/// A static include, import or extends edge found in a template.
#[derive(Clone, Debug)]
pub(crate) struct Composition {
    pub(crate) from: String,
    pub(crate) line: u16,
    pub(crate) column: u16,
    pub(crate) kind: &'static str,
    pub(crate) target: String,
}

/// The target language of a template's literal text.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Language {
    C,
    /// No literal-text check exists for this language: any literal text fails.
    Unsupported,
}

impl Language {
    /// From an artifact template's name: `model.c.jinja` prints C.
    pub(crate) fn of_template(name: &str) -> Self {
        let stem = name.strip_suffix(".jinja").unwrap_or(name);
        match Path::new(stem).extension().and_then(|ext| ext.to_str()) {
            Some("c" | "h") => Self::C,
            _ => Self::Unsupported,
        }
    }
}

/// Where templates come from: the repository, or a fixed map in tests.
pub(crate) trait TemplateSource {
    /// The file a shared template name resolves to, as the render environment
    /// would resolve it.
    fn resolve(&self, name: &str) -> Option<String>;
    fn read(&self, file: &str) -> Result<String>;
}

/// The codegen template directory, with names resolved through the shared
/// names that each `target.toml` publishes (`[[partials]].name` and
/// `[[files]].shared_as`): exactly what the render environment registers.
pub(crate) struct RepoTemplates {
    dir: PathBuf,
    shared: BTreeMap<String, String>,
    artifacts: Vec<String>,
}

impl RepoTemplates {
    pub(crate) fn load(repo_root: &Path) -> Result<Self> {
        let dir = repo_root.join(TEMPLATE_DIR);
        let mut shared = BTreeMap::new();
        let mut artifacts = Vec::new();
        for entry in fs::read_dir(&dir).with_context(|| format!("reading {}", dir.display()))? {
            let target = entry?.path();
            let manifest = target.join("target.toml");
            if !manifest.is_file() {
                continue;
            }
            let name = target
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or_default()
                .to_owned();
            let text = fs::read_to_string(&manifest)?;
            let table: toml::Table =
                toml::from_str(&text).with_context(|| format!("parsing {}", manifest.display()))?;
            for row in rows(&table, "partials") {
                if let (Some(n), Some(t)) = (str_field(row, "name"), str_field(row, "template")) {
                    shared.insert(n.to_owned(), format!("{name}/{t}"));
                }
            }
            for row in rows(&table, "files") {
                let Some(t) = str_field(row, "template") else {
                    continue;
                };
                artifacts.push(format!("{name}/{t}"));
                if let Some(n) = str_field(row, "shared_as") {
                    shared.insert(n.to_owned(), format!("{name}/{t}"));
                }
            }
        }
        artifacts.sort();
        Ok(Self {
            dir,
            shared,
            artifacts,
        })
    }

    pub(crate) fn artifacts(&self) -> &[String] {
        &self.artifacts
    }
}

/// One decoded region: a covered macro whose reviewed spelling table, in the
/// named consumer's conformance decoder, governs its literal text.
#[derive(Clone, Debug)]
pub(crate) struct DecodedRegion {
    pub(crate) file: String,
    pub(crate) name: String,
    pub(crate) consumer: String,
    pub(crate) decoder: String,
    /// The macro's arguments, in order, with the issued schema each claims.
    pub(crate) arguments: Vec<(String, String)>,
    pub(crate) dispatch: Vec<ast_rules::Dispatch>,
}

/// The registry's regions, and a violation for every malformed entry: a
/// wrong container or value type is reported, never filtered out. A missing
/// registry decodes nothing, so every literal check stays in force.
pub(crate) fn load_registry(source: &dyn TemplateSource) -> (Vec<DecodedRegion>, Vec<Violation>) {
    let Ok(text) = source.read(REGION_REGISTRY) else {
        return (Vec::new(), Vec::new());
    };
    let mut violations = Vec::new();
    let mut flag = |detail: String| {
        violations.push(Violation {
            file: REGION_REGISTRY.to_owned(),
            line: 0,
            column: 0,
            rule: Rule::RegionSelection,
            detail,
        });
    };
    let table: toml::Table = match toml::from_str(&text) {
        Ok(table) => table,
        Err(error) => {
            flag(format!("unparsable registry: {error}"));
            return (Vec::new(), violations);
        }
    };
    let mut regions: Vec<DecodedRegion> = Vec::new();
    for key in table.keys().filter(|k| *k != "region") {
        flag(format!("unknown registry key {key:?}"));
    }
    for row in strict_rows(&table, "region", "registry", &mut flag) {
        if let Some(region) = region_row(row, &mut flag) {
            if regions
                .iter()
                .any(|r| r.file == region.file && r.name == region.name)
            {
                flag(format!(
                    "{} macro {} is registered twice",
                    region.file, region.name
                ));
            } else {
                regions.push(region);
            }
        }
    }
    (regions, violations)
}

/// The tables of array `key` in `table`; a non-array value or a non-table
/// element is reported.
fn strict_rows<'t>(
    table: &'t toml::Table,
    key: &str,
    owner: &str,
    flag: &mut dyn FnMut(String),
) -> Vec<&'t toml::Table> {
    match table.get(key) {
        None => Vec::new(),
        Some(toml::Value::Array(items)) => items
            .iter()
            .filter_map(|item| {
                let row = item.as_table();
                if row.is_none() {
                    flag(format!("{owner}: a {key} entry is not a table"));
                }
                row
            })
            .collect(),
        Some(_) => {
            flag(format!("{owner}: {key} is not an array of tables"));
            Vec::new()
        }
    }
}

fn unknown_keys(row: &toml::Table, known: &[&str], owner: &str, flag: &mut dyn FnMut(String)) {
    for key in row.keys().filter(|k| !known.contains(&k.as_str())) {
        flag(format!("{owner}: unknown key {key:?}"));
    }
}

/// A non-empty string field, or a report.
fn strict_str(
    row: &toml::Table,
    key: &str,
    owner: &str,
    flag: &mut dyn FnMut(String),
) -> Option<String> {
    match row.get(key).and_then(toml::Value::as_str) {
        Some(value) if !value.trim().is_empty() => Some(value.to_owned()),
        _ => {
            flag(format!("{owner}: {key} must be a non-empty string"));
            None
        }
    }
}

fn region_row(row: &toml::Table, flag: &mut dyn FnMut(String)) -> Option<DecodedRegion> {
    let file = strict_str(row, "file", "a region", flag);
    let name = strict_str(row, "macro", "a region", flag);
    let consumer = strict_str(row, "consumer", "a region", flag);
    let decoder = strict_str(row, "decoder", "a region", flag);
    unknown_keys(
        row,
        &["file", "macro", "consumer", "decoder", "argument", "tag"],
        "a region",
        flag,
    );
    let (file, name, consumer, decoder) = (file?, name?, consumer?, decoder?);
    let owner = format!("{file} macro {name}");
    let mut arguments: Vec<(String, String)> = Vec::new();
    for argument in strict_rows(row, "argument", &owner, flag) {
        unknown_keys(argument, &["name", "schema"], &owner, flag);
        let (Some(arg), Some(schema)) = (
            strict_str(argument, "name", &owner, flag),
            strict_str(argument, "schema", &owner, flag),
        ) else {
            continue;
        };
        if !is_name(&arg) || arguments.iter().any(|(a, _)| *a == arg) {
            flag(format!("{owner}: argument {arg:?} is not a new name"));
        }
        arguments.push((arg, schema));
    }
    let mut dispatch: Vec<ast_rules::Dispatch> = Vec::new();
    for tag in strict_rows(row, "tag", &owner, flag) {
        unknown_keys(tag, &["path", "schema", "variants"], &owner, flag);
        if let Some(d) = tag_row(tag, &arguments, &dispatch, &owner, flag) {
            dispatch.push(d);
        }
    }
    Some(DecodedRegion {
        file,
        name,
        consumer,
        decoder,
        arguments,
        dispatch,
    })
}

/// A tag from a declared argument: a dotted path into it, or the argument
/// itself when the argument's own schema is the tag's.
fn tag_row(
    tag: &toml::Table,
    arguments: &[(String, String)],
    taken: &[ast_rules::Dispatch],
    owner: &str,
    flag: &mut dyn FnMut(String),
) -> Option<ast_rules::Dispatch> {
    let path = strict_str(tag, "path", owner, flag)?;
    let schema = strict_str(tag, "schema", owner, flag)?;
    let variants: Vec<String> = match tag.get("variants") {
        Some(toml::Value::Array(items))
            if !items.is_empty() && items.iter().all(toml::Value::is_str) =>
        {
            items
                .iter()
                .filter_map(|v| v.as_str().map(str::to_owned))
                .collect()
        }
        _ => {
            flag(format!(
                "{owner}: tag {path:?} needs a non-empty array of string variants"
            ));
            return None;
        }
    };
    let segments: Vec<&str> = path.split('.').collect();
    let root = arguments.iter().find(|(a, _)| a == segments[0]);
    let bound = match (root, segments.len()) {
        (Some(_), n) if n > 1 => segments.iter().all(|s| is_name(s)),
        (Some((_, argument_schema)), 1) => *argument_schema == schema,
        _ => false,
    };
    if !bound {
        flag(format!(
            "{owner}: tag {path:?} must be a dotted path from a declared argument, or a declared argument of schema {schema}"
        ));
        return None;
    }
    if variants.iter().collect::<BTreeSet<_>>().len() != variants.len()
        || taken.iter().any(|d| d.path == path)
    {
        flag(format!(
            "{owner}: tag {path:?} repeats a variant or an earlier tag"
        ));
        return None;
    }
    Some(ast_rules::Dispatch { path, variants })
}

fn is_name(part: &str) -> bool {
    part.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
        && part.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

fn rows<'t>(table: &'t toml::Table, key: &str) -> impl Iterator<Item = &'t toml::Table> {
    table
        .get(key)
        .and_then(toml::Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(toml::Value::as_table)
}

fn str_field<'t>(row: &'t toml::Table, key: &str) -> Option<&'t str> {
    row.get(key).and_then(toml::Value::as_str)
}

impl TemplateSource for RepoTemplates {
    fn resolve(&self, name: &str) -> Option<String> {
        self.shared.get(name).cloned()
    }

    fn read(&self, file: &str) -> Result<String> {
        let path = self.dir.join(file);
        fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))
    }
}

/// The result of checking one artifact template and its static closure.
#[derive(Debug, Default)]
pub(crate) struct Report {
    pub(crate) files: Vec<String>,
    pub(crate) violations: Vec<Violation>,
}

/// Checks `root` and every template it statically includes, imports or extends.
/// Literal text is checked as `language` throughout the closure, since a
/// fragment prints into its includer's artifact.
pub(crate) fn check(source: &dyn TemplateSource, root: &str, language: Language) -> Result<Report> {
    let mut walk = Walk::new(source, language, None);
    walk.visit(root)?;
    walk.report.violations.sort();
    Ok(walk.report)
}

/// Checks one named macro of `file` as a strict region: exactly one macro of
/// that name must exist, its arguments are the only path roots, and its body,
/// literal text and static closure follow the template rules. The rest of the
/// file is not checked and not covered by a pass.
///
/// A region the decoded-region registry names is checked as decoded: it may
/// also dispatch on its registered closed tags and call registered decoded
/// helpers of the same file (each checked the same way, never recursively),
/// and its literal text is governed by its consumer's conformance decoder
/// instead of the literal lexer. Everything else stays in force.
pub(crate) fn check_macro(
    source: &dyn TemplateSource,
    file: &str,
    name: &str,
    language: Language,
) -> Result<Report> {
    let (registry, problems) = load_registry(source);
    let mut report = Report {
        files: Vec::new(),
        violations: problems,
    };
    let mut path = Vec::new();
    let mut done = BTreeSet::new();
    check_region(
        source,
        &registry,
        (file, name),
        language,
        (&mut path, &mut done),
        &mut report,
    )?;
    report.violations.sort();
    Ok(report)
}

fn check_region(
    source: &dyn TemplateSource,
    registry: &[DecodedRegion],
    (file, name): (&str, &str),
    language: Language,
    (path, done): (&mut Vec<String>, &mut BTreeSet<String>),
    report: &mut Report,
) -> Result<()> {
    let text = source.read(file)?;
    let ast = match parse(
        &text,
        file,
        SyntaxConfig::default(),
        WhitespaceConfig::default(),
    ) {
        Ok(ast) => ast,
        Err(error) => {
            report.files.push(file.to_owned());
            report.violations.push(parse_violation(file, &error));
            return Ok(());
        }
    };
    let mut found = Vec::new();
    find_macros(std::slice::from_ref(&ast), name, &mut found);
    let [region] = found.as_slice() else {
        report.files.push(file.to_owned());
        report.violations.push(Violation {
            file: file.to_owned(),
            line: found.get(1).map_or(0, |m| m.span().start_line),
            column: 0,
            rule: Rule::RegionSelection,
            detail: format!(
                "{} macros named {name:?}; exactly one is required",
                found.len()
            ),
        });
        return Ok(());
    };
    let decoded = registry.iter().find(|r| r.file == file && r.name == name);
    let (roots, mut violations) = macro_roots(file, region);
    if let Some(row) = decoded {
        let declared: Vec<&String> = row.arguments.iter().map(|(a, _)| a).collect();
        if declared != roots.iter().collect::<Vec<_>>() {
            violations.push(Violation {
                file: REGION_REGISTRY.to_owned(),
                line: 0,
                column: 0,
                rule: Rule::RegionSelection,
                detail: format!(
                    "{file} macro {name}: registered arguments {declared:?} are not the macro's {roots:?}"
                ),
            });
        }
    }
    let mut walk = Walk::new(source, language, Some(roots));
    walk.report.violations.append(&mut violations);
    walk.stack.push(file.to_owned());
    walk.report.files.push(format!(
        "{file} (macro {name}{} only; the rest of the file is not covered)",
        decoded.map_or_else(String::new, |row| format!(
            ", decoded by {} for {},",
            row.decoder, row.consumer
        ))
    ));
    let mut edges = Vec::new();
    match decoded {
        Some(region_row) => {
            let helpers = registry
                .iter()
                .filter(|r| r.file == file)
                .map(|r| r.name.clone())
                .collect();
            ast_rules::AstChecker::new(file, &mut walk.report.violations, &mut edges)
                .with_roots(walk.roots.clone())
                .with_decoded(region_row.dispatch.clone(), helpers)
                .stmts(&region.body);
        }
        None => walk.check_body(file, &region.body, &mut edges),
    }
    let key = format!("{file}#{name}");
    path.push(key.clone());
    for edge in edges {
        if edge.kind != "helper" {
            walk.follow(&edge)?;
            continue;
        }
        let helper = format!("{file}#{}", edge.target);
        if path.contains(&helper) {
            walk.report.violations.push(Violation {
                file: edge.from.clone(),
                line: edge.line,
                column: edge.column,
                rule: Rule::Composition,
                detail: format!("helper recursion through {}", edge.target),
            });
        } else if done.insert(helper) {
            check_region(
                source,
                registry,
                (file, &edge.target),
                language,
                (path, done),
                report,
            )?;
        }
    }
    path.pop();
    done.insert(key);
    report.files.append(&mut walk.report.files);
    report.violations.append(&mut walk.report.violations);
    Ok(())
}

type MacroNode<'s, 'a> =
    &'s minijinja::machinery::ast::Spanned<minijinja::machinery::ast::Macro<'a>>;

fn find_macros<'s, 'a>(stmts: &'s [Stmt<'a>], name: &str, out: &mut Vec<MacroNode<'s, 'a>>) {
    for stmt in stmts {
        if let Stmt::Macro(m) = stmt
            && m.name == name
        {
            out.push(m);
        }
        for body in bodies(stmt) {
            find_macros(body, name, out);
        }
    }
}

fn bodies<'s, 'a>(stmt: &'s Stmt<'a>) -> Vec<&'s [Stmt<'a>]> {
    match stmt {
        Stmt::Template(t) => vec![&t.children],
        Stmt::ForLoop(f) => vec![&f.body, &f.else_body],
        Stmt::IfCond(i) => vec![&i.true_body, &i.false_body],
        Stmt::Block(b) => vec![&b.body],
        Stmt::WithBlock(w) => vec![&w.body],
        Stmt::SetBlock(s) => vec![&s.body],
        Stmt::FilterBlock(f) => vec![&f.body],
        Stmt::AutoEscape(a) => vec![&a.body],
        Stmt::Macro(m) => vec![&m.body],
        Stmt::CallBlock(c) => vec![&c.macro_decl.body],
        _ => Vec::new(),
    }
}

/// A region's roots are its plain argument names. Defaults would choose a
/// value when the plan omits one, so they are fallbacks.
fn macro_roots(file: &str, region: MacroNode<'_, '_>) -> (Vec<String>, Vec<Violation>) {
    let mut roots = Vec::new();
    let mut violations = Vec::new();
    let mut flag = |rule: Rule, detail: &str| {
        violations.push(Violation {
            file: file.to_owned(),
            line: region.span().start_line,
            column: region.span().start_col,
            rule,
            detail: detail.to_owned(),
        });
    };
    for arg in &region.args {
        match arg {
            minijinja::machinery::ast::Expr::Var(v) => roots.push(v.id.to_owned()),
            _ => flag(Rule::Assignment, "macro argument must be a plain name"),
        }
    }
    if !region.defaults.is_empty() {
        flag(Rule::Fallback, "macro argument default");
    }
    (roots, violations)
}

fn parse_violation(file: &str, error: &minijinja::Error) -> Violation {
    Violation {
        file: file.to_owned(),
        line: error
            .line()
            .and_then(|l| u16::try_from(l).ok())
            .unwrap_or(0),
        column: 0,
        rule: Rule::Parse,
        detail: error.to_string(),
    }
}

struct Walk<'s> {
    source: &'s dyn TemplateSource,
    language: Language,
    /// A macro region's closed roots, carried into the fragments it includes.
    roots: Option<Vec<String>>,
    stack: Vec<String>,
    done: BTreeSet<String>,
    report: Report,
}

impl<'s> Walk<'s> {
    fn new(source: &'s dyn TemplateSource, language: Language, roots: Option<Vec<String>>) -> Self {
        Self {
            source,
            language,
            roots,
            stack: Vec::new(),
            done: BTreeSet::new(),
            report: Report::default(),
        }
    }

    fn visit(&mut self, file: &str) -> Result<()> {
        self.stack.push(file.to_owned());
        self.report.files.push(file.to_owned());
        let text = self.source.read(file)?;
        let mut edges = Vec::new();
        match parse(
            &text,
            file,
            SyntaxConfig::default(),
            WhitespaceConfig::default(),
        ) {
            Ok(ast) => self.check_body(file, std::slice::from_ref(&ast), &mut edges),
            Err(error) => self.report.violations.push(parse_violation(file, &error)),
        }
        for edge in edges {
            self.follow(&edge)?;
        }
        self.stack.pop();
        self.done.insert(file.to_owned());
        Ok(())
    }

    fn check_body(&mut self, file: &str, body: &[Stmt<'_>], edges: &mut Vec<Composition>) {
        ast_rules::AstChecker::new(file, &mut self.report.violations, edges)
            .with_roots(self.roots.clone())
            .stmts(body);
        literal_c::check(file, body, self.language, &mut self.report.violations);
    }

    fn follow(&mut self, edge: &Composition) -> Result<()> {
        let flag = |detail: String| Violation {
            file: edge.from.clone(),
            line: edge.line,
            column: edge.column,
            rule: Rule::Composition,
            detail,
        };
        let Some(file) = self.source.resolve(&edge.target) else {
            self.report.violations.push(flag(format!(
                "{} of unregistered template {:?}",
                edge.kind, edge.target
            )));
            return Ok(());
        };
        if self.stack.contains(&file) {
            self.report
                .violations
                .push(flag(format!("{} cycle through {file}", edge.kind)));
            return Ok(());
        }
        if self.done.contains(&file) {
            return Ok(());
        }
        self.visit(&file)
    }
}

/// Artifact templates that pass the strict check without being registered as
/// covered. Unregistered, the gate would not keep checking them, so a template
/// that becomes print-only must be added to `COVERED_TEMPLATES`.
pub(crate) fn uncovered_passes(
    source: &dyn TemplateSource,
    artifacts: &[String],
    covered: &[&str],
) -> Result<Vec<String>> {
    let mut out = Vec::new();
    for root in artifacts {
        if covered.contains(&root.as_str()) {
            continue;
        }
        if check(source, root, Language::of_template(root))?
            .violations
            .is_empty()
        {
            out.push(root.clone());
        }
    }
    Ok(out)
}

/// The lint gate: every covered template and macro region passes, and no
/// artifact template passes without being registered.
pub(crate) fn run() -> Result<()> {
    let repo = RepoTemplates::load(&repo_root())?;
    gate(&repo, repo.artifacts(), COVERED_TEMPLATES, COVERED_MACROS)?;
    println!(
        "print-plan template check: {} covered template(s) and {} macro region(s) pass; legacy templates are tracked by the migration ledger, not by this gate",
        COVERED_TEMPLATES.len(),
        COVERED_MACROS.len()
    );
    Ok(())
}

/// The gate's decision for a set of covered templates and macro regions.
pub(crate) fn gate(
    source: &dyn TemplateSource,
    artifacts: &[String],
    templates: &[&str],
    macros: &[(&str, &str, Language)],
) -> Result<()> {
    let mut failed = Vec::new();
    for root in templates {
        failed.extend(check(source, root, Language::of_template(root))?.violations);
    }
    for (file, name, language) in macros {
        failed.extend(check_macro(source, file, name, *language)?.violations);
    }
    let (registry, problems) = load_registry(source);
    failed.extend(problems);
    for row in &registry {
        if !macros
            .iter()
            .any(|(f, n, _)| *f == row.file && *n == row.name)
        {
            failed.push(Violation {
                file: REGION_REGISTRY.to_owned(),
                line: 0,
                column: 0,
                rule: Rule::RegionSelection,
                detail: format!(
                    "{} macro {} is a decoded region but not in COVERED_MACROS; a region needs both registries",
                    row.file, row.name
                ),
            });
        }
    }
    let unregistered = uncovered_passes(source, artifacts, templates)?;
    if !unregistered.is_empty() {
        bail!(
            "print-plan template check: {} pass but are not in COVERED_TEMPLATES; register them so the gate keeps checking them",
            unregistered.join(", ")
        );
    }
    if !failed.is_empty() {
        for violation in &failed {
            eprintln!("{violation}");
        }
        bail!(
            "print-plan template check: {} violation(s) in covered templates",
            failed.len()
        );
    }
    Ok(())
}

/// `rumoca-print-plan-template-check [--report] [--c] [--macro NAME FILE] [TEMPLATE...]`: check
/// named templates (failing on any violation), or report every artifact
/// template without failing. `--c` checks the literal text of a fragment, whose
/// name carries no target extension, as C.
pub(crate) fn run_cli(args: &[String]) -> Result<()> {
    let repo = RepoTemplates::load(&repo_root())?;
    let force_c = args.iter().any(|a| a == "--c");
    let args: Vec<String> = args.iter().filter(|a| *a != "--c").cloned().collect();
    if let [flag, name, file] = args.as_slice()
        && flag == "--macro"
    {
        let language = if force_c {
            Language::C
        } else {
            Language::of_template(file)
        };
        let report = check_macro(&repo, file, name, language)?;
        for violation in &report.violations {
            println!("{violation}");
        }
        println!(
            "{}: {}",
            report.files.join(", "),
            summarize(&report.violations)
        );
        return if report.violations.is_empty() {
            Ok(())
        } else {
            bail!("{} violation(s)", report.violations.len())
        };
    }
    if args.first().map(String::as_str) == Some("--report") {
        for root in repo.artifacts() {
            let report = check(&repo, root, Language::of_template(root))?;
            println!("{root}: {}", summarize(&report.violations));
        }
        return Ok(());
    }
    let roots: Vec<&str> = if args.is_empty() {
        COVERED_TEMPLATES.to_vec()
    } else {
        args.iter().map(String::as_str).collect()
    };
    let mut count = 0;
    for root in roots {
        let language = if force_c {
            Language::C
        } else {
            Language::of_template(root)
        };
        for violation in check(&repo, root, language)?.violations {
            println!("{violation}");
            count += 1;
        }
    }
    if count > 0 {
        bail!("{count} violation(s)");
    }
    Ok(())
}

fn summarize(violations: &[Violation]) -> String {
    if violations.is_empty() {
        return "pass".to_owned();
    }
    let mut by_rule: BTreeMap<Rule, usize> = BTreeMap::new();
    for violation in violations {
        *by_rule.entry(violation.rule).or_default() += 1;
    }
    let parts: Vec<String> = by_rule
        .iter()
        .map(|(rule, n)| format!("{rule:?}={n}"))
        .collect();
    format!("{} violation(s): {}", violations.len(), parts.join(" "))
}

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}
