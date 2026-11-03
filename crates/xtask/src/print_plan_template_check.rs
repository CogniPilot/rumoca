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
    let mut walk = Walk {
        source,
        language,
        stack: Vec::new(),
        done: BTreeSet::new(),
        report: Report::default(),
    };
    walk.visit(root)?;
    walk.report.violations.sort();
    Ok(walk.report)
}

struct Walk<'s> {
    source: &'s dyn TemplateSource,
    language: Language,
    stack: Vec<String>,
    done: BTreeSet<String>,
    report: Report,
}

impl Walk<'_> {
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
            Ok(ast) => self.check_ast(file, &ast, &mut edges),
            Err(error) => self.report.violations.push(Violation {
                file: file.to_owned(),
                line: error
                    .line()
                    .and_then(|l| u16::try_from(l).ok())
                    .unwrap_or(0),
                column: 0,
                rule: Rule::Parse,
                detail: error.to_string(),
            }),
        }
        for edge in edges {
            self.follow(&edge)?;
        }
        self.stack.pop();
        self.done.insert(file.to_owned());
        Ok(())
    }

    fn check_ast(&mut self, file: &str, ast: &Stmt<'_>, edges: &mut Vec<Composition>) {
        ast_rules::AstChecker::new(file, &mut self.report.violations, edges)
            .stmts(std::slice::from_ref(ast));
        literal_c::check(file, ast, self.language, &mut self.report.violations);
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

/// The lint gate: every covered template passes.
pub(crate) fn run() -> Result<()> {
    let repo = RepoTemplates::load(&repo_root())?;
    let mut failed = Vec::new();
    for root in COVERED_TEMPLATES {
        failed.extend(check(&repo, root, Language::of_template(root))?.violations);
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
    println!(
        "print-plan template check: {} covered template(s) pass; legacy templates are tracked by the migration ledger, not by this gate",
        COVERED_TEMPLATES.len()
    );
    Ok(())
}

/// `rumoca-print-plan-template-check [--report] [--c] [TEMPLATE...]`: check
/// named templates (failing on any violation), or report every artifact
/// template without failing. `--c` checks the literal text of a fragment, whose
/// name carries no target extension, as C.
pub(crate) fn run_cli(args: &[String]) -> Result<()> {
    let repo = RepoTemplates::load(&repo_root())?;
    let force_c = args.iter().any(|a| a == "--c");
    let args: Vec<String> = args.iter().filter(|a| *a != "--c").cloned().collect();
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
