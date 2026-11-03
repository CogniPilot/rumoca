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
const COVERED_MACROS: &[(&str, &str, Language)] = &[];

const TEMPLATE_DIR: &str = "crates/rumoca-phase-codegen/src/templates";

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
pub(crate) fn check_macro(
    source: &dyn TemplateSource,
    file: &str,
    name: &str,
    language: Language,
) -> Result<Report> {
    let text = source.read(file)?;
    let ast = match parse(
        &text,
        file,
        SyntaxConfig::default(),
        WhitespaceConfig::default(),
    ) {
        Ok(ast) => ast,
        Err(error) => {
            return Ok(Report {
                files: vec![file.to_owned()],
                violations: vec![parse_violation(file, &error)],
            });
        }
    };
    let mut found = Vec::new();
    find_macros(std::slice::from_ref(&ast), name, &mut found);
    let [region] = found.as_slice() else {
        return Ok(Report {
            files: vec![file.to_owned()],
            violations: vec![Violation {
                file: file.to_owned(),
                line: found.get(1).map_or(0, |m| m.span().start_line),
                column: 0,
                rule: Rule::RegionSelection,
                detail: format!(
                    "{} macros named {name:?}; exactly one is required",
                    found.len()
                ),
            }],
        });
    };
    let (roots, mut violations) = macro_roots(file, region);
    let mut walk = Walk::new(source, language, Some(roots));
    walk.report.violations.append(&mut violations);
    walk.stack.push(file.to_owned());
    walk.report.files.push(format!(
        "{file} (macro {name} only; the rest of the file is not covered)"
    ));
    let mut edges = Vec::new();
    walk.check_body(file, &region.body, &mut edges);
    for edge in edges {
        walk.follow(&edge)?;
    }
    walk.report.violations.sort();
    Ok(walk.report)
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
