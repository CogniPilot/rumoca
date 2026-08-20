//! Proven lexical placement of generated-C automatic locals (GAL-034).

use std::collections::{HashMap, HashSet};

use rumoca_ir_galec::ast;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum ScopeStep {
    IfBranch { statement: usize, branch: usize },
    IfElse { statement: usize },
    ForBody { statement: usize },
}

pub(super) type ScopePath = Vec<ScopeStep>;

/// Where a reference stands, for the purpose of deciding whether the generated
/// C *reads* the object.
///
/// The distinction exists to answer one question: does this declaration still
/// need the `(void)&x;` marker that suppresses "set but not used"? A read is
/// therefore defined narrowly and conservatively — an expression operand or a
/// subscript, both of which every target emits unconditionally. An assignment
/// target is a write; and a `limit` target, or a signal-check closure, is
/// counted as neither, because a target may legitimately emit nothing for one
/// (an unbounded declared range clamps to no code at all), and a declaration
/// whose only "read" then vanished would lose its marker and break the build.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Position {
    Read,
    Write,
}

pub(super) struct LocalPlacements<'a> {
    by_scope: HashMap<ScopePath, Vec<&'a ast::VariableDeclaration>>,
    placed: HashSet<&'a str>,
    read: HashSet<&'a str>,
}

/// The subset of `names` the body mentions at all, honouring `for`-iterator
/// shadowing.
///
/// This is the whole test for an **input parameter**: checked construction
/// proves an input is never written, so any mention of one is a read, and a
/// parameter the body never mentions is exactly the parameter whose
/// `(void)x;` is load-bearing.
pub(super) fn mentioned<'a>(
    names: &HashSet<&'a str>,
    statements: &'a [ast::Spanned<ast::Statement>],
) -> HashSet<&'a str> {
    let mut uses = HashMap::<&str, ScopePath>::new();
    let mut read = HashSet::new();
    collect_statements(statements, &mut Vec::new(), names, &mut uses, &mut read);
    uses.into_keys().collect()
}

impl<'a> LocalPlacements<'a> {
    pub(super) fn derive(
        declarations: &'a [ast::VariableDeclaration],
        statements: &'a [ast::Spanned<ast::Statement>],
    ) -> Self {
        let names = declarations
            .iter()
            .map(|declaration| declaration.name.lexeme())
            .collect::<HashSet<_>>();
        let mut uses = HashMap::<&str, ScopePath>::new();
        let mut read = HashSet::new();
        collect_statements(statements, &mut Vec::new(), &names, &mut uses, &mut read);
        let mut by_scope = HashMap::<ScopePath, Vec<_>>::new();
        let mut placed = HashSet::new();
        for declaration in declarations {
            if let Some(path) = uses.remove(declaration.name.lexeme()) {
                placed.insert(declaration.name.lexeme());
                by_scope.entry(path).or_default().push(declaration);
            }
        }
        Self {
            by_scope,
            placed,
            read,
        }
    }

    pub(super) fn at(&self, path: &[ScopeStep]) -> Vec<&'a ast::VariableDeclaration> {
        self.by_scope.get(path).cloned().unwrap_or_default()
    }

    /// The placements at `path` that stay automatic storage in the generated
    /// target. Array-valued declarations are hoisted into the caller-provided
    /// block context instead (GAL-034 placement still decides *whether* a
    /// declaration is reachable at all; this decides *where* it lives), so
    /// only the scalars remain in the frame.
    pub(super) fn frame_at(&self, path: &[ScopeStep]) -> Vec<&'a ast::VariableDeclaration> {
        let mut locals = self.at(path);
        locals.retain(|declaration| declaration.dimensions.is_empty());
        locals
    }

    /// Whether the declaration is reachable from the body at all. Unreachable
    /// declarations get neither a frame slot nor a context slot.
    pub(super) fn is_placed(&self, name: &str) -> bool {
        self.placed.contains(name)
    }

    /// Whether the generated body READS the declaration — see [`Position`].
    ///
    /// A declaration that is read needs no unused-entity marker: the read is
    /// the use. One that is only ever assigned does need one, because C's
    /// "set but not used" diagnostic is about exactly that shape, and under
    /// `-Werror` a missing marker is a failed build.
    pub(super) fn is_read(&self, name: &str) -> bool {
        self.read.contains(name)
    }
}

fn collect_statements<'a>(
    statements: &'a [ast::Spanned<ast::Statement>],
    path: &mut ScopePath,
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
    read: &mut HashSet<&'a str>,
) {
    for (statement_index, statement) in statements.iter().enumerate() {
        collect_statement(&statement.node, statement_index, path, names, uses, read);
    }
}

fn collect_statement<'a>(
    statement: &'a ast::Statement,
    statement_index: usize,
    path: &mut ScopePath,
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
    read: &mut HashSet<&'a str>,
) {
    match statement {
        ast::Statement::Assignment { target, value } => {
            collect_reference(target, Position::Write, path, names, uses, read);
            collect_expression(value, path, names, uses, read);
        }
        ast::Statement::MultiAssignment { targets, call } => {
            for target in targets {
                collect_reference(target, Position::Write, path, names, uses, read);
            }
            collect_call(call, path, names, uses, read);
        }
        ast::Statement::Call(call) => collect_call(call, path, names, uses, read),
        ast::Statement::If(value) => {
            for (branch_index, branch) in value.branches.iter().enumerate() {
                collect_condition(&branch.condition, path, names, uses, read);
                path.push(ScopeStep::IfBranch {
                    statement: statement_index,
                    branch: branch_index,
                });
                collect_statements(&branch.body, path, names, uses, read);
                path.pop();
            }
            if let Some(body) = &value.else_body {
                path.push(ScopeStep::IfElse {
                    statement: statement_index,
                });
                collect_statements(body, path, names, uses, read);
                path.pop();
            }
        }
        ast::Statement::For(value) => {
            collect_expression(&value.start, path, names, uses, read);
            if let Some(step) = &value.step {
                collect_expression(step, path, names, uses, read);
            }
            collect_expression(&value.stop, path, names, uses, read);
            path.push(ScopeStep::ForBody {
                statement: statement_index,
            });
            let mut body_names = names.clone();
            if let Some(iterator) = &value.iterator {
                body_names.remove(iterator.lexeme());
            }
            collect_statements(&value.body, path, &body_names, uses, read);
            path.pop();
        }
        ast::Statement::Limit(targets) => {
            for target in targets {
                if let ast::LimitTarget::Reference(reference) = target {
                    // `limit x` both reads and writes `x` — but a declaration
                    // whose range is unbounded clamps to NO emitted code, so
                    // counting this as a read could take a marker away from a
                    // variable the C never reads. Not a read.
                    collect_reference(reference, Position::Write, path, names, uses, read);
                }
            }
        }
        ast::Statement::Signal(_) => {}
    }
}

fn collect_condition<'a>(
    condition: &'a ast::Condition,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
    read: &mut HashSet<&'a str>,
) {
    match condition {
        ast::Condition::Expression(expression) => {
            collect_expression(expression, path, names, uses, read)
        }
        ast::Condition::SignalCheck(check) => {
            if let Some(closure) = &check.closure
                && names.contains(closure.as_str())
            {
                record_use(uses, closure.as_str(), path);
            }
            if let Some(fallback) = &check.fallback {
                collect_expression(fallback, path, names, uses, read);
            }
        }
    }
}

fn collect_call<'a>(
    call: &'a ast::FunctionCall,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
    read: &mut HashSet<&'a str>,
) {
    for argument in &call.arguments {
        collect_expression(argument, path, names, uses, read);
    }
}

fn collect_expression<'a>(
    expression: &'a ast::Expression,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
    read: &mut HashSet<&'a str>,
) {
    match expression {
        ast::Expression::Bool(_) | ast::Expression::Integer(_) | ast::Expression::Real(_) => {}
        ast::Expression::Ref(reference) | ast::Expression::Neg(reference) => {
            collect_reference(reference, Position::Read, path, names, uses, read);
        }
        ast::Expression::Size { array, dimension } => {
            collect_reference(array, Position::Read, path, names, uses, read);
            collect_expression(dimension, path, names, uses, read);
        }
        ast::Expression::Call(call) => collect_call(call, path, names, uses, read),
        ast::Expression::Paren(value) | ast::Expression::Not(value) => {
            collect_expression(value, path, names, uses, read);
        }
        ast::Expression::If(value) => {
            for (condition, branch) in &value.branches {
                collect_expression(condition, path, names, uses, read);
                collect_expression(branch, path, names, uses, read);
            }
            collect_expression(&value.else_value, path, names, uses, read);
        }
        ast::Expression::Array(values) => {
            for value in values {
                collect_expression(value, path, names, uses, read);
            }
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_expression(lhs, path, names, uses, read);
            collect_expression(rhs, path, names, uses, read);
        }
    }
}

fn collect_reference<'a>(
    reference: &'a ast::Reference,
    position: Position,
    path: &[ScopeStep],
    names: &HashSet<&'a str>,
    uses: &mut HashMap<&'a str, ScopePath>,
    read: &mut HashSet<&'a str>,
) {
    let parts = match reference {
        ast::Reference::Local(part) => {
            if names.contains(part.name.lexeme()) {
                record_use(uses, part.name.lexeme(), path);
                if position == Position::Read {
                    read.insert(part.name.lexeme());
                }
            }
            std::slice::from_ref(part)
        }
        ast::Reference::State(parts) => parts.as_slice(),
    };
    // A subscript is read whichever side of the assignment the reference is on:
    // `x[i] := …` still evaluates `i`.
    for subscript in parts.iter().flat_map(|part| &part.subscripts) {
        collect_expression(subscript, path, names, uses, read);
    }
}

fn record_use<'a>(uses: &mut HashMap<&'a str, ScopePath>, name: &'a str, path: &[ScopeStep]) {
    uses.entry(name)
        .and_modify(|owner| {
            let common = owner
                .iter()
                .zip(path)
                .take_while(|(lhs, rhs)| lhs == rhs)
                .count();
            owner.truncate(common);
        })
        .or_insert_with(|| path.to_vec());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn declaration(name: &str) -> ast::VariableDeclaration {
        ast::VariableDeclaration::scalar(ast::ScalarType::Real, ast::Name::ident(name))
    }

    fn assignment(target: &str, source: &str) -> ast::Spanned<ast::Statement> {
        ast::Spanned::dummy(ast::Statement::Assignment {
            target: ast::Reference::local(ast::Name::ident(target)),
            value: ast::Expression::Ref(ast::Reference::local(ast::Name::ident(source))),
        })
    }

    fn names<'a>(values: &'a [&'a ast::VariableDeclaration]) -> Vec<&'a str> {
        values.iter().map(|value| value.name.lexeme()).collect()
    }

    #[test]
    fn branch_local_and_cross_scope_uses_have_distinct_owners() {
        let declarations = vec![
            declaration("branch_value"),
            declaration("shared_value"),
            declaration("sink"),
        ];
        let statements = vec![
            ast::Spanned::dummy(ast::Statement::If(ast::IfStatement {
                branches: vec![ast::IfBranch {
                    condition: ast::Condition::Expression(ast::Expression::Bool(true)),
                    body: vec![
                        assignment("branch_value", "branch_value"),
                        assignment("shared_value", "branch_value"),
                    ],
                    span: rumoca_core::Span::DUMMY,
                }],
                else_body: None,
            })),
            assignment("sink", "shared_value"),
        ];
        let placements = LocalPlacements::derive(&declarations, &statements);

        assert_eq!(names(&placements.at(&[])), ["shared_value", "sink"]);
        assert_eq!(
            names(&placements.at(&[ScopeStep::IfBranch {
                statement: 0,
                branch: 0,
            }])),
            ["branch_value"]
        );
    }

    #[test]
    fn loop_only_local_is_owned_by_the_loop_body() {
        let declarations = vec![declaration("loop_value")];
        let statements = vec![ast::Spanned::dummy(ast::Statement::For(ast::ForLoop {
            iterator: Some(ast::Name::ident("index")),
            start: ast::Expression::Integer(1),
            step: None,
            stop: ast::Expression::Integer(2),
            body: vec![assignment("loop_value", "loop_value")],
        }))];
        let placements = LocalPlacements::derive(&declarations, &statements);

        assert!(placements.at(&[]).is_empty());
        assert_eq!(
            names(&placements.at(&[ScopeStep::ForBody { statement: 0 }])),
            ["loop_value"]
        );
    }

    #[test]
    fn unreferenced_checked_local_needs_no_c_storage() {
        let declarations = vec![declaration("lowered_away")];
        let placements = LocalPlacements::derive(&declarations, &[]);

        assert!(placements.at(&[]).is_empty());
    }

    /// A declaration the body reads needs no unused-entity marker; one it only
    /// ever assigns does. This is the whole basis on which the C targets stop
    /// emitting `(void)&x;` unconditionally, so it is worth its own name.
    #[test]
    fn only_an_assigned_never_read_local_still_needs_a_marker() {
        let declarations = vec![declaration("source"), declaration("sink")];
        let statements = vec![assignment("sink", "source")];
        let placements = LocalPlacements::derive(&declarations, &statements);

        assert!(placements.is_read("source"), "an operand is a read");
        assert!(
            !placements.is_read("sink"),
            "an assignment target is a write, and C diagnoses set-but-unused"
        );
    }

    /// A subscript is read on either side of an assignment: `x[i] := …` still
    /// evaluates `i`, so `i` needs no marker even though `x` does.
    #[test]
    fn a_subscript_is_read_on_the_target_side_too() {
        let declarations = vec![declaration("row"), declaration("index")];
        let mut target = ast::RefPart::plain(ast::Name::ident("row"));
        target.subscripts = vec![ast::Expression::Ref(ast::Reference::local(
            ast::Name::ident("index"),
        ))];
        let statements = vec![ast::Spanned::dummy(ast::Statement::Assignment {
            target: ast::Reference::Local(target),
            value: ast::Expression::Real(1.0),
        })];
        let placements = LocalPlacements::derive(&declarations, &statements);

        assert!(placements.is_read("index"));
        assert!(!placements.is_read("row"));
    }

    /// `limit x` is NOT counted as a read, and that is deliberate. A target
    /// emits nothing at all for a declaration whose declared range is
    /// unbounded, so a variable whose only "read" was a `limit` would lose its
    /// marker and fail the build. Fail closed: keep the marker.
    #[test]
    fn a_limit_target_does_not_count_as_a_read() {
        let declarations = vec![declaration("saturated")];
        let statements = [
            assignment("saturated", "saturated"),
            ast::Spanned::dummy(ast::Statement::Limit(vec![ast::LimitTarget::Reference(
                ast::Reference::local(ast::Name::ident("saturated")),
            )])),
        ];
        // The self-assignment above reads it, so isolate the `limit`.
        let placements = LocalPlacements::derive(&declarations, &statements[1..]);
        assert!(placements.is_placed("saturated"), "it still needs storage");
        assert!(!placements.is_read("saturated"));
    }

    /// An input parameter is never written, so any mention of one is a read —
    /// and a `for` iterator that shadows the name is not a mention of it.
    #[test]
    fn mentioned_answers_the_input_parameter_question_and_honours_shadowing() {
        let names = HashSet::from(["used", "shadowed", "never"]);
        let statements = vec![
            assignment("sink", "used"),
            ast::Spanned::dummy(ast::Statement::For(ast::ForLoop {
                iterator: Some(ast::Name::ident("shadowed")),
                start: ast::Expression::Integer(1),
                step: None,
                stop: ast::Expression::Integer(2),
                body: vec![assignment("sink", "shadowed")],
            })),
        ];
        let found = mentioned(&names, &statements);

        assert!(found.contains("used"));
        assert!(
            !found.contains("shadowed"),
            "the loop iterator, not the parameter, is what the body names there"
        );
        assert!(!found.contains("never"));
    }
}
