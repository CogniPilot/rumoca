//! The restricted template AST of the print-plan contract: every MiniJinja node
//! kind it does not explicitly accept is a violation.

use super::{Composition, Rule, Violation};
use minijinja::machinery::Span;
use minijinja::machinery::ast::{self, CallArg, Expr, Stmt};

/// Encoding-only filters: each encodes an already-issued scalar or token as the
/// same value in the target's spelling. Adding one needs the contract owner's
/// review; `tojson`, `sanitize` and value formatters are deliberately absent.
const ENCODING_FILTERS: &[&str] = &["xml_escape", "modelica_string"];

/// Filters that turn a template into a representation or admission classifier.
const COLLECTION_FILTERS: &[&str] = &[
    "selectattr",
    "rejectattr",
    "select",
    "reject",
    "map",
    "sort",
    "groupby",
    "unique",
    "list",
    "length",
    "count",
    "first",
    "last",
    "reverse",
    "join",
    "sum",
    "min",
    "max",
    "product",
    "batch",
    "slice",
    "dictsort",
    "items",
    "attr",
];

/// Filters, tests and calls that choose a different product instead of failing
/// at construction.
const FALLBACK_NAMES: &[&str] = &["default", "d", "defined", "undefined", "none", "fail"];

/// The only `loop` attributes a template may read, and only as a print branch
/// for punctuation.
const PUNCTUATION_LOOP_MARKERS: &[&str] = &["first", "last"];

pub(super) struct AstChecker<'v> {
    file: String,
    scopes: Vec<String>,
    /// In a macro region, the macro's arguments: the only names (besides loop
    /// targets in scope) a path may start from. `None` checks a whole template,
    /// whose free names are the render context.
    roots: Option<Vec<String>>,
    violations: &'v mut Vec<Violation>,
    composition: &'v mut Vec<Composition>,
}

impl<'v> AstChecker<'v> {
    pub(super) fn new(
        file: &str,
        violations: &'v mut Vec<Violation>,
        composition: &'v mut Vec<Composition>,
    ) -> Self {
        Self {
            file: file.to_owned(),
            scopes: Vec::new(),
            roots: None,
            violations,
            composition,
        }
    }

    pub(super) fn with_roots(mut self, roots: Option<Vec<String>>) -> Self {
        self.roots = roots;
        self
    }

    fn flag(&mut self, span: Span, rule: Rule, detail: impl Into<String>) {
        self.violations.push(Violation {
            file: self.file.clone(),
            line: span.start_line,
            column: span.start_col,
            rule,
            detail: detail.into(),
        });
    }

    pub(super) fn stmts(&mut self, stmts: &[Stmt<'_>]) {
        for stmt in stmts {
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &Stmt<'_>) {
        match stmt {
            Stmt::Template(t) => self.stmts(&t.children),
            Stmt::EmitRaw(_) => {}
            Stmt::EmitExpr(e) => self.output(&e.expr),
            Stmt::ForLoop(f) => self.for_loop(f.span(), f),
            Stmt::IfCond(i) => {
                self.condition(&i.expr);
                self.stmts(&i.true_body);
                self.stmts(&i.false_body);
            }
            Stmt::Block(b) => self.stmts(&b.body),
            Stmt::Include(i) => self.include(i.span(), &i.name, i.ignore_missing),
            Stmt::Import(i) => self.static_reference(i.span(), &i.expr, "import"),
            Stmt::FromImport(i) => self.static_reference(i.span(), &i.expr, "from-import"),
            Stmt::Extends(e) => self.static_reference(e.span(), &e.name, "extends"),
            other => self.forbidden_stmt(other),
        }
    }

    fn forbidden_stmt(&mut self, stmt: &Stmt<'_>) {
        let (span, rule, what) = match stmt {
            Stmt::Set(s) => (s.span(), Rule::Assignment, "set"),
            Stmt::SetBlock(s) => (s.span(), Rule::Assignment, "set block"),
            Stmt::WithBlock(s) => (s.span(), Rule::Assignment, "with block"),
            Stmt::Macro(s) => (s.span(), Rule::Macro, "macro definition"),
            Stmt::CallBlock(s) => (s.span(), Rule::Macro, "call block"),
            Stmt::Do(s) => (s.span(), Rule::Call, "do statement"),
            Stmt::FilterBlock(s) => (s.span(), Rule::Call, "filter block"),
            Stmt::AutoEscape(s) => (s.span(), Rule::Composition, "autoescape block"),
            _ => (
                Span::default(),
                Rule::UnknownNode,
                "statement kind outside the allowlist",
            ),
        };
        self.flag(span, rule, what);
        self.forbidden_stmt_contents(stmt);
    }

    /// Reports what a forbidden statement contains too, so an audit witness
    /// inside a macro or a `set` is named at its own line.
    fn forbidden_stmt_contents(&mut self, stmt: &Stmt<'_>) {
        let expr = |checker: &mut Self, e: &Expr<'_>| {
            if !matches!(e, Expr::Var(_) | Expr::Const(_) | Expr::GetAttr(_)) {
                checker.forbidden_expr(e);
            }
        };
        match stmt {
            Stmt::Set(s) => expr(self, &s.expr),
            Stmt::SetBlock(s) => self.stmts(&s.body),
            Stmt::WithBlock(s) => {
                for (_, value) in &s.assignments {
                    expr(self, value);
                }
                self.stmts(&s.body);
            }
            Stmt::Macro(s) => self.stmts(&s.body),
            Stmt::CallBlock(s) => self.stmts(&s.macro_decl.body),
            Stmt::Do(s) => expr(self, &s.call.expr),
            Stmt::FilterBlock(s) => self.stmts(&s.body),
            Stmt::AutoEscape(s) => self.stmts(&s.body),
            _ => {}
        }
    }

    fn for_loop(&mut self, span: Span, f: &ast::ForLoop<'_>) {
        if f.recursive {
            self.flag(span, Rule::LoopControl, "recursive loop");
        }
        if let Some(filter) = &f.filter_expr {
            self.flag(expr_span(filter), Rule::Collection, "filtered loop");
            self.forbidden_expr(filter);
        }
        if !f.else_body.is_empty() {
            self.flag(
                span,
                Rule::LoopControl,
                "for-else branches on collection emptiness",
            );
        }
        self.plan_path(&f.iter, "loop iterable");
        let target = match &f.target {
            Expr::Var(v) if v.id != "loop" => Some(v.id.to_owned()),
            other => {
                self.flag(
                    expr_span(other),
                    Rule::Assignment,
                    "loop target must be one plain name",
                );
                None
            }
        };
        self.scopes.extend(target.clone());
        self.stmts(&f.body);
        if target.is_some() {
            self.scopes.pop();
        }
    }

    fn include(&mut self, span: Span, name: &Expr<'_>, ignore_missing: bool) {
        if ignore_missing {
            self.flag(span, Rule::Composition, "include with ignore missing");
        }
        self.static_reference(span, name, "include");
    }

    fn static_reference(&mut self, span: Span, name: &Expr<'_>, kind: &'static str) {
        match const_str(name) {
            Some(target) => self.composition.push(Composition {
                from: self.file.clone(),
                line: span.start_line,
                column: span.start_col,
                kind,
                target,
            }),
            None => {
                self.flag(span, Rule::Composition, format!("dynamic {kind} name"));
                self.forbidden_expr(name);
            }
        }
    }

    /// `{{ ... }}`: a projection of an issued field, a string literal, or one
    /// encoding-only filter over such a projection.
    fn output(&mut self, expr: &Expr<'_>) {
        match expr {
            Expr::Const(c) if c.value.as_str().is_some() => {}
            Expr::Filter(f) if ENCODING_FILTERS.contains(&f.name) && f.args.is_empty() => {
                match &f.expr {
                    Some(inner) => self.plan_path(inner, "encoded output"),
                    None => self.flag(f.span(), Rule::Call, "filter without an operand"),
                }
            }
            other => self.plan_path(other, "output"),
        }
    }

    /// `{% if ... %}`: an issued Boolean, or a loop's first/last marker.
    fn condition(&mut self, expr: &Expr<'_>) {
        if let Some(marker) = loop_marker(expr) {
            if !PUNCTUATION_LOOP_MARKERS.contains(&marker) {
                self.flag(
                    expr_span(expr),
                    Rule::LoopIntrinsic,
                    format!("loop.{marker}"),
                );
            }
            return;
        }
        self.plan_path(expr, "condition");
    }

    /// A plain name followed by attribute steps: the only shape that can be a
    /// direct projection of an issued field.
    fn plan_path(&mut self, expr: &Expr<'_>, role: &str) {
        match expr {
            Expr::Var(v) if v.id == "loop" => {
                self.flag(
                    v.span(),
                    Rule::LoopIntrinsic,
                    format!("loop used as {role}"),
                );
            }
            Expr::Var(v) => self.root(v.span(), v.id),
            Expr::GetAttr(a) => {
                if let Some(marker) = loop_marker(expr) {
                    self.flag(
                        a.span(),
                        Rule::LoopIntrinsic,
                        format!("loop.{marker} as {role}"),
                    );
                } else {
                    self.plan_path(&a.expr, role);
                }
            }
            other => self.forbidden_expr(other),
        }
    }

    fn root(&mut self, span: Span, id: &str) {
        let declared = self
            .roots
            .as_ref()
            .is_none_or(|roots| roots.iter().any(|r| r == id));
        if !declared && !self.scopes.iter().any(|s| s == id) {
            self.flag(
                span,
                Rule::UndeclaredRoot,
                format!("{id} is neither an argument of the checked macro nor a loop target"),
            );
        }
    }

    /// Flags a forbidden expression and every forbidden node inside it, so a
    /// report names the innermost witness (a `range` inside a filter, say).
    fn forbidden_expr(&mut self, expr: &Expr<'_>) {
        match (expr, loop_marker(expr)) {
            (_, Some(marker)) => self.flag(
                expr_span(expr),
                Rule::LoopIntrinsic,
                format!("loop.{marker}"),
            ),
            (Expr::Var(v), None) if v.id == "loop" => {
                self.flag(v.span(), Rule::LoopIntrinsic, "loop")
            }
            // A name or attribute step is reported through the construct it reads.
            (Expr::Var(_) | Expr::GetAttr(_), None) => {}
            _ => {
                let (rule, detail) = classify(expr);
                self.flag(expr_span(expr), rule, detail);
            }
        }
        for child in children(expr) {
            if !matches!(child, Expr::Const(_)) && !is_plain_path(child) {
                self.forbidden_expr(child);
            }
        }
    }
}

fn classify(expr: &Expr<'_>) -> (Rule, String) {
    match expr {
        Expr::BinOp(b) => (
            Rule::Arithmetic,
            format!("binary operator {:?}", bin_name(b.op)),
        ),
        Expr::Compare(_) => (Rule::Arithmetic, "comparison".to_owned()),
        Expr::UnaryOp(_) => (Rule::Arithmetic, "unary operator".to_owned()),
        Expr::IfExpr(_) => (Rule::Arithmetic, "ternary selection".to_owned()),
        Expr::Slice(_) => (Rule::RangeOrSubscript, "slice".to_owned()),
        Expr::GetItem(_) => (Rule::RangeOrSubscript, "subscript".to_owned()),
        Expr::List(_) | Expr::Map(_) => (Rule::Collection, "collection literal".to_owned()),
        Expr::Filter(f) => (filter_rule(f.name), format!("filter {}", f.name)),
        Expr::Test(t) => (test_rule(t.name), format!("test {}", t.name)),
        Expr::Call(c) => (call_rule(&c.expr), format!("call {}", callee_name(&c.expr))),
        Expr::Const(_) => (
            Rule::Arithmetic,
            "constant where an issued field is required".to_owned(),
        ),
        Expr::Var(_) | Expr::GetAttr(_) => (Rule::UnknownNode, "path".to_owned()),
    }
}

fn filter_rule(name: &str) -> Rule {
    if FALLBACK_NAMES.contains(&name) {
        Rule::Fallback
    } else if COLLECTION_FILTERS.contains(&name) {
        Rule::Collection
    } else {
        Rule::Call
    }
}

fn test_rule(name: &str) -> Rule {
    if FALLBACK_NAMES.contains(&name) {
        Rule::Fallback
    } else {
        Rule::ValueTest
    }
}

fn call_rule(callee: &Expr<'_>) -> Rule {
    match callee_name(callee).as_str() {
        "range" => Rule::RangeOrSubscript,
        "fail" => Rule::Fallback,
        "namespace" => Rule::Assignment,
        _ => Rule::Call,
    }
}

fn callee_name(callee: &Expr<'_>) -> String {
    match callee {
        Expr::Var(v) => v.id.to_owned(),
        Expr::GetAttr(a) => format!("{}.{}", callee_name(&a.expr), a.name),
        _ => "<computed callee>".to_owned(),
    }
}

fn bin_name(op: ast::BinOpKind) -> &'static str {
    use ast::BinOpKind as B;
    match op {
        B::Eq | B::Ne | B::Lt | B::Lte | B::Gt | B::Gte | B::In => "comparison",
        B::ScAnd | B::ScOr => "boolean formula",
        B::Concat => "concatenation",
        B::Add | B::Sub | B::Mul | B::Div | B::FloorDiv | B::Rem | B::Pow => "arithmetic",
    }
}

fn arg_exprs<'e, 'a>(args: &'e [CallArg<'a>]) -> impl Iterator<Item = &'e Expr<'a>> {
    args.iter().map(|arg| match arg {
        CallArg::Pos(e) | CallArg::Kwarg(_, e) | CallArg::PosSplat(e) | CallArg::KwargSplat(e) => e,
    })
}

fn children<'e, 'a>(expr: &'e Expr<'a>) -> Vec<&'e Expr<'a>> {
    match expr {
        Expr::Var(_) | Expr::Const(_) => Vec::new(),
        Expr::Slice(s) => [
            Some(&s.expr),
            s.start.as_ref(),
            s.stop.as_ref(),
            s.step.as_ref(),
        ]
        .into_iter()
        .flatten()
        .collect(),
        Expr::UnaryOp(u) => vec![&u.expr],
        Expr::BinOp(b) => vec![&b.left, &b.right],
        Expr::Compare(c) => std::iter::once(&c.expr)
            .chain(c.ops.iter().map(|o| &o.expr))
            .collect(),
        Expr::IfExpr(i) => [
            Some(&i.test_expr),
            Some(&i.true_expr),
            i.false_expr.as_ref(),
        ]
        .into_iter()
        .flatten()
        .collect(),
        Expr::Filter(f) => f.expr.iter().chain(arg_exprs(&f.args)).collect(),
        Expr::Test(t) => std::iter::once(&t.expr).chain(arg_exprs(&t.args)).collect(),
        Expr::GetAttr(a) => vec![&a.expr],
        Expr::GetItem(g) => vec![&g.expr, &g.subscript_expr],
        Expr::Call(c) => std::iter::once(&c.expr).chain(arg_exprs(&c.args)).collect(),
        Expr::List(l) => l.items.iter().collect(),
        Expr::Map(m) => m.keys.iter().chain(m.values.iter()).collect(),
    }
}

/// A name and attribute steps that never touch `loop`: harmless as an operand
/// of a construct that is already reported.
fn is_plain_path(expr: &Expr<'_>) -> bool {
    match expr {
        Expr::Var(v) => v.id != "loop",
        Expr::GetAttr(a) => is_plain_path(&a.expr),
        _ => false,
    }
}

fn loop_marker<'e>(expr: &'e Expr<'_>) -> Option<&'e str> {
    match expr {
        Expr::GetAttr(a) => match &a.expr {
            Expr::Var(v) if v.id == "loop" => Some(a.name),
            _ => None,
        },
        _ => None,
    }
}

fn const_str(expr: &Expr<'_>) -> Option<String> {
    match expr {
        Expr::Const(c) => c.value.as_str().map(str::to_owned),
        _ => None,
    }
}

pub(super) fn expr_span(expr: &Expr<'_>) -> Span {
    match expr {
        Expr::Var(s) => s.span(),
        Expr::Const(s) => s.span(),
        Expr::Slice(s) => s.span(),
        Expr::UnaryOp(s) => s.span(),
        Expr::BinOp(s) => s.span(),
        Expr::Compare(s) => s.span(),
        Expr::IfExpr(s) => s.span(),
        Expr::Filter(s) => s.span(),
        Expr::Test(s) => s.span(),
        Expr::GetAttr(s) => s.span(),
        Expr::GetItem(s) => s.span(),
        Expr::Call(s) => s.span(),
        Expr::List(s) => s.span(),
        Expr::Map(s) => s.span(),
    }
}
