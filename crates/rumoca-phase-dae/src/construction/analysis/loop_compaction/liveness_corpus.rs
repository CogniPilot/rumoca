//! A bounded function-body corpus and an independent execution oracle for it.
//!
//! The corpus is enumerated, not sampled: every program the grammar below can
//! build within its depth and length bounds is visited exactly once, so a clean
//! run is a statement about the whole grammar rather than about a lucky draw.
//! Three value names (`a`, `b`, `c`) and two loop binders (`i`, `j`) are enough
//! to express every shape the store-deletion proofs distinguish: a definite
//! overwrite, a conditional overwrite, a read before a write, a read after a
//! loop, and the same read one loop deeper.
//!
//! [`observed_incoming_reads_before_exit`] answers the corpus's central question by
//! executing it rather than by analyzing it. It carries a set of *states*, each
//! recording which values a path has already overwritten, and forks that set at
//! every branch and every loop iteration count. A name is reported when some
//! reachable state reads it without having overwritten it first, which is the
//! definition of "the incoming value is observed" with no dataflow reasoning in
//! between. Loops are run to a fixed point over the state set, so all iteration
//! counts including zero are covered.
//!
//! The oracle shares only the read-extraction helpers with
//! [`super::liveness`]; every control-flow decision is made independently, by
//! forward simulation rather than backward transfer.

use super::liveness::{LiveSet, collect_expression_reads, collect_subscript_reads};
use super::*;
use rumoca_core::{ComponentReference, Literal, OpBinary};
use std::collections::BTreeSet;

/// Value names the differential asks about.
pub(super) const VALUE_NAMES: [&str; 3] = ["a", "b", "c"];

fn span() -> Span {
    Span::from_offsets(rumoca_core::SourceId::DUMMY, 0, 1)
}

fn component(name: &str) -> ComponentReference {
    ComponentReference::construct(
        false,
        span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(1),
        }],
    )
    .expect("corpus component reference has exact identity")
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span: span(),
    }
}

/// An expression that reads exactly `reads` and nothing else.
fn reading(reads: &[&str]) -> Expression {
    let mut expression = Expression::Literal {
        value: Literal::Integer(1),
        span: span(),
    };
    for name in reads {
        expression = Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(expression),
            rhs: Box::new(var(name)),
            span: span(),
        };
    }
    expression
}

pub(super) fn assign(target: &str, reads: &[&str]) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: component(target),
        value: reading(reads),
        span: span(),
    }
}

pub(super) fn for_loop(
    binder: &str,
    range_reads: &[&str],
    body: Vec<rumoca_core::Statement>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::For {
        indices: vec![rumoca_core::ForIndex {
            ident: binder.to_string(),
            range: Expression::Range {
                start: Box::new(Expression::Literal {
                    value: Literal::Integer(1),
                    span: span(),
                }),
                step: None,
                end: Box::new(reading(range_reads)),
                span: span(),
            },
        }],
        equations: body,
        span: span(),
    }
}

pub(super) fn while_loop(
    cond_reads: &[&str],
    body: Vec<rumoca_core::Statement>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::While {
        block: rumoca_core::StatementBlock {
            cond: reading(cond_reads),
            stmts: body,
        },
        span: span(),
    }
}

pub(super) fn branch(
    cond_reads: &[&str],
    taken: Vec<rumoca_core::Statement>,
    fallback: Option<Vec<rumoca_core::Statement>>,
) -> rumoca_core::Statement {
    rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: reading(cond_reads),
            stmts: taken,
        }],
        else_block: fallback,
        span: span(),
    }
}

/// A readable one-line rendering, so a reported disagreement names a program.
pub(super) fn render(statements: &[rumoca_core::Statement]) -> String {
    statements
        .iter()
        .map(render_statement)
        .collect::<Vec<_>>()
        .join(" ")
}

fn render_statement(statement: &rumoca_core::Statement) -> String {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => {
            format!(
                "{} := {};",
                comp.to_var_name().as_str(),
                render_reads(value)
            )
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => format!(
            "for {} in 1:{} loop {} end for;",
            indices[0].ident,
            render_reads(&indices[0].range),
            render(equations)
        ),
        rumoca_core::Statement::While { block, .. } => format!(
            "while {} loop {} end while;",
            render_reads(&block.cond),
            render(&block.stmts)
        ),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            let fallback = else_block
                .as_ref()
                .map(|statements| format!("else {} ", render(statements)))
                .unwrap_or_default();
            format!(
                "if {} then {} {}end if;",
                render_reads(&cond_blocks[0].cond),
                render(&cond_blocks[0].stmts),
                fallback
            )
        }
        other => format!("{other:?}"),
    }
}

fn render_reads(expression: &Expression) -> String {
    let mut reads = LiveSet::new();
    collect_expression_reads(expression, &mut reads);
    if reads.is_empty() {
        return "1".to_string();
    }
    reads
        .iter()
        .map(|name| name.as_str().to_string())
        .collect::<Vec<_>>()
        .join("+")
}

/// Overwrites a single execution path has already performed.
type Overwritten = BTreeSet<VarName>;

/// The set of paths reaching a program point, one entry per distinct history.
type Paths = BTreeSet<Overwritten>;

/// Names whose value on entry to `statements` is read by some execution path
/// before that path overwrites it.
pub(super) fn observed_incoming_reads(statements: &[rumoca_core::Statement]) -> LiveSet {
    observed_incoming_reads_before_exit(statements, &LiveSet::new())
}

/// The same question, with `live_on_exit` read once the statements finish.
///
/// A function's outputs are its result (MLS §12.4.1), so a caller reads them at
/// a point past the last statement of the body. That read observes the incoming
/// value on any path that reaches the end without overwriting it, which is a
/// property of the path set the simulation already carries.
pub(super) fn observed_incoming_reads_before_exit(
    statements: &[rumoca_core::Statement],
    live_on_exit: &LiveSet,
) -> LiveSet {
    let mut simulation = Simulation {
        observed: LiveSet::new(),
        shadowed: LiveSet::new(),
    };
    let entry: Paths = std::iter::once(Overwritten::new()).collect();
    let reached = simulation.sequence(statements, &entry);
    let mut observed = simulation.observed;
    for name in live_on_exit {
        if reached
            .iter()
            .any(|overwritten| !overwritten.contains(name))
        {
            observed.insert(name.clone());
        }
    }
    observed
}

struct Simulation {
    observed: LiveSet,
    /// Loop binders currently in scope. A read of one of these names reaches
    /// the binder, never the enclosing value of the same text (MLS §11.2.2).
    shadowed: LiveSet,
}

impl Simulation {
    fn sequence(&mut self, statements: &[rumoca_core::Statement], entry: &Paths) -> Paths {
        let mut paths = entry.clone();
        for statement in statements {
            paths = self.statement(statement, &paths);
        }
        paths
    }

    /// Record every read that some path performs before overwriting the value.
    fn observe(&mut self, reads: &LiveSet, paths: &Paths) {
        for name in reads {
            if self.shadowed.contains(name) {
                continue;
            }
            if paths.iter().any(|overwritten| !overwritten.contains(name)) {
                self.observed.insert(name.clone());
            }
        }
    }

    fn statement(&mut self, statement: &rumoca_core::Statement, paths: &Paths) -> Paths {
        match statement {
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                let mut reads = LiveSet::new();
                collect_expression_reads(value, &mut reads);
                collect_subscript_reads(comp, &mut reads);
                self.observe(&reads, paths);
                overwrite(paths, scalar_assignment_target(comp).as_ref())
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => self.branches(cond_blocks, else_block.as_deref(), paths),
            rumoca_core::Statement::When { blocks, .. } => self.branches(blocks, None, paths),
            rumoca_core::Statement::For {
                indices, equations, ..
            } => self.for_statement(indices, equations, paths),
            rumoca_core::Statement::While { block, .. } => self.while_statement(block, paths),
            other => unreachable!("the corpus grammar does not build {other:?}"),
        }
    }

    fn branches(
        &mut self,
        blocks: &[rumoca_core::StatementBlock],
        fallback: Option<&[rumoca_core::Statement]>,
        paths: &Paths,
    ) -> Paths {
        let mut reached = match fallback {
            Some(statements) => self.sequence(statements, paths),
            None => paths.clone(),
        };
        for block in blocks {
            let mut reads = LiveSet::new();
            collect_expression_reads(&block.cond, &mut reads);
            self.observe(&reads, paths);
            reached.extend(self.sequence(&block.stmts, paths));
        }
        reached
    }

    fn for_statement(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        equations: &[rumoca_core::Statement],
        paths: &Paths,
    ) -> Paths {
        let mut reads = LiveSet::new();
        for index in indices {
            collect_expression_reads(&index.range, &mut reads);
        }
        self.observe(&reads, paths);
        let entering = indices
            .iter()
            .map(|index| VarName::new(&index.ident))
            .filter(|binder| self.shadowed.insert(binder.clone()))
            .collect::<Vec<_>>();
        let reached = self.iterate(equations, paths);
        for binder in entering {
            self.shadowed.remove(&binder);
        }
        reached
    }

    fn iterate(&mut self, body: &[rumoca_core::Statement], entry: &Paths) -> Paths {
        let mut reached = entry.clone();
        loop {
            let produced = self.sequence(body, &reached);
            let mut merged = reached.clone();
            merged.extend(produced);
            if merged == reached {
                return reached;
            }
            reached = merged;
        }
    }

    fn while_statement(&mut self, block: &rumoca_core::StatementBlock, paths: &Paths) -> Paths {
        let mut reads = LiveSet::new();
        collect_expression_reads(&block.cond, &mut reads);
        let mut reached = paths.clone();
        loop {
            self.observe(&reads, &reached);
            let produced = self.sequence(&block.stmts, &reached);
            let mut merged = reached.clone();
            merged.extend(produced);
            if merged == reached {
                return reached;
            }
            reached = merged;
        }
    }
}

fn overwrite(paths: &Paths, target: Option<&VarName>) -> Paths {
    paths
        .iter()
        .map(|overwritten| {
            let mut next = overwritten.clone();
            if let Some(target) = target {
                next.insert(target.clone());
            }
            next
        })
        .collect()
}
