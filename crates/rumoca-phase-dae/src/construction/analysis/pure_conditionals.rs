//! Expression form for a pure statement conditional.
//!
//! A conditional whose every branch only assigns the same whole variables is
//! the MLS §3.6.5 if-expression of each variable: no assertion, partial
//! definition, captured region or loop needs the ordered statement owner, so
//! each output is issued as one conditional expression. Call-site expansion
//! and differentiation consume that straight-line form.
use rumoca_core::{
    Expression, ExpressionRewriter, Reference, Span, Statement, StatementBlock, Subscript, VarName,
};

/// The function values whose definedness is not guaranteed on entry, and the
/// subset a statement sequence has already assigned on every path.
pub(super) struct Scope<'a> {
    /// Outputs and protected locals of the function.
    pub(super) owned: &'a [VarName],
    defined: Vec<VarName>,
}

impl<'a> Scope<'a> {
    pub(super) fn new(owned: &'a [VarName]) -> Self {
        Self {
            owned,
            defined: Vec::new(),
        }
    }

    fn may_be_undefined(&self, name: &VarName) -> bool {
        let root = name.root_split().map_or(name.as_str(), |(root, _)| root);
        self.owned.iter().any(|owned| owned.as_str() == root)
            && !self.defined.iter().any(|defined| defined.as_str() == root)
    }

    fn branch(&self) -> Scope<'a> {
        Scope {
            owned: self.owned,
            defined: self.defined.clone(),
        }
    }
}

/// Rewrite every pure conditional outside a loop into if-expression
/// assignments, innermost first. Every other statement is left as it is.
pub(super) fn express(statements: &[Statement], scope: &mut Scope<'_>) -> Vec<Statement> {
    let mut expressed = Vec::with_capacity(statements.len());
    for statement in statements {
        expressed.extend(express_statement(statement, scope));
    }
    expressed
}

fn express_statement(statement: &Statement, scope: &mut Scope<'_>) -> Vec<Statement> {
    match statement {
        Statement::Assignment { comp, .. } => {
            scope.defined.extend(whole_variable(comp));
            vec![statement.clone()]
        }
        Statement::FunctionCall { outputs, .. } => {
            for output in outputs.iter().flatten() {
                scope
                    .defined
                    .extend(output.parts().first().map(|part| VarName::new(&part.ident)));
            }
            vec![statement.clone()]
        }
        Statement::If {
            cond_blocks,
            else_block,
            span,
        } => express_conditional(cond_blocks, else_block.as_deref(), *span, scope),
        other => vec![other.clone()],
    }
}

fn express_conditional(
    cond_blocks: &[StatementBlock],
    else_block: Option<&[Statement]>,
    span: Span,
    scope: &mut Scope<'_>,
) -> Vec<Statement> {
    let blocks = cond_blocks
        .iter()
        .map(|block| StatementBlock {
            cond: block.cond.clone(),
            stmts: express(&block.stmts, &mut scope.branch()),
        })
        .collect::<Vec<_>>();
    let fallback = else_block.map(|statements| express(statements, &mut scope.branch()));
    match pure_assignments(&blocks, fallback.as_deref(), span, scope) {
        Some(assignments) => {
            let targets = assignments
                .iter()
                .filter_map(|assignment| match assignment {
                    Statement::Assignment { comp, .. } => whole_variable(comp),
                    _ => None,
                });
            scope.defined.extend(targets);
            assignments
        }
        None => vec![Statement::If {
            cond_blocks: blocks,
            else_block: fallback,
            span,
        }],
    }
}

/// One output of a branch: its variable, the reference assigned, and the value
/// the branch leaves in it.
struct Output {
    name: VarName,
    comp: rumoca_core::ComponentReference,
    value: Expression,
}

fn pure_assignments(
    blocks: &[StatementBlock],
    fallback: Option<&[Statement]>,
    span: Span,
    scope: &Scope<'_>,
) -> Option<Vec<Statement>> {
    let fallback = branch_outputs(fallback?, scope)?;
    let branches = blocks
        .iter()
        .map(|block| branch_outputs(&block.stmts, scope))
        .collect::<Option<Vec<_>>>()?;
    if fallback.is_empty() || branches.iter().any(|branch| branch.len() != fallback.len()) {
        return None;
    }
    let targets = fallback
        .iter()
        .map(|output| output.name.clone())
        .collect::<Vec<_>>();
    // A condition reads the value the conditional is about to replace, which
    // an if-expression assignment per output would no longer see.
    for block in blocks {
        let mut reads = Reads::new(&targets, scope);
        reads.rewrite_expression(&block.cond);
        if reads.failed {
            return None;
        }
    }
    fallback
        .into_iter()
        .map(|output| {
            let arms = blocks
                .iter()
                .zip(&branches)
                .map(|(block, branch)| {
                    let arm = branch.iter().find(|arm| arm.name == output.name)?;
                    Some((block.cond.clone(), arm.value.clone()))
                })
                .collect::<Option<Vec<_>>>()?;
            Some(Statement::Assignment {
                value: if_expression(arms, output.value, span),
                comp: output.comp,
                span,
            })
        })
        .collect()
}

/// An if-expression over `arms` and `fallback`, where a fallback that is itself
/// an if-expression contributes its arms in order (MLS §3.6.5 evaluates them
/// in the same sequence), so an `elseif` chain keeps its flat shape.
fn if_expression(
    mut arms: Vec<(Expression, Expression)>,
    fallback: Expression,
    span: Span,
) -> Expression {
    match fallback {
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            arms.extend(branches);
            Expression::If {
                branches: arms,
                else_branch,
                span,
            }
        }
        fallback => Expression::If {
            branches: arms,
            else_branch: Box::new(fallback),
            span,
        },
    }
}

/// The final value of every variable a branch assigns, in first-assignment
/// order. A later statement that reads an earlier one's variable sees that
/// value, so it is substituted; a read of a value the branch has not yet
/// assigned, or of part of one, needs the statement order and refuses. So does
/// a read of a function value that some path leaves undefined.
fn branch_outputs(statements: &[Statement], scope: &Scope<'_>) -> Option<Vec<Output>> {
    let mut targets = Vec::new();
    for statement in statements {
        let Statement::Assignment { comp, .. } = statement else {
            return None;
        };
        let name = whole_variable(comp)?;
        if !targets.contains(&name) {
            targets.push(name);
        }
    }
    let mut outputs: Vec<Output> = Vec::new();
    for statement in statements {
        let Statement::Assignment { comp, value, .. } = statement else {
            return None;
        };
        let name = whole_variable(comp)?;
        let mut reads = Reads::new(&targets, scope);
        reads.assigned = outputs
            .iter()
            .map(|output| (output.name.clone(), output.value.clone()))
            .collect();
        let value = reads.rewrite_expression(value);
        if reads.failed {
            return None;
        }
        match outputs.iter_mut().find(|output| output.name == name) {
            Some(output) => output.value = value,
            None => outputs.push(Output {
                name,
                comp: comp.clone(),
                value,
            }),
        }
    }
    Some(outputs)
}

fn whole_variable(comp: &rumoca_core::ComponentReference) -> Option<VarName> {
    match comp.parts() {
        [part] if part.subs.is_empty() => Some(VarName::new(&part.ident)),
        _ => None,
    }
}

/// Substitutes the values already assigned in a branch and records whether any
/// other read of an assigned variable, or of an undefined value, occurs.
struct Reads<'scope> {
    targets: &'scope [VarName],
    scope: &'scope Scope<'scope>,
    assigned: Vec<(VarName, Expression)>,
    failed: bool,
}

impl<'scope> Reads<'scope> {
    fn new(targets: &'scope [VarName], scope: &'scope Scope<'scope>) -> Self {
        Self {
            targets,
            scope,
            assigned: Vec::new(),
            failed: false,
        }
    }
}

impl ExpressionRewriter for Reads<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        let var = name.var_name();
        let reads_target = self.targets.iter().any(|target| {
            var == target
                || var
                    .as_str()
                    .strip_prefix(target.as_str())
                    .is_some_and(|rest| rest.starts_with('.'))
        });
        if reads_target {
            let value = subscripts
                .is_empty()
                .then(|| self.assigned.iter().find(|(target, _)| target == var))
                .flatten();
            if let Some((_, value)) = value {
                return value.clone();
            }
            self.failed = true;
        } else if self.scope.may_be_undefined(var) {
            self.failed = true;
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }

    /// A comprehension binds names of its own, which a substituted variable
    /// of the same name would capture.
    fn walk_array_comprehension_expression(
        &mut self,
        expr: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
        span: Span,
    ) -> Expression {
        self.failed |= !self.assigned.is_empty();
        Expression::ArrayComprehension {
            expr: Box::new(self.rewrite_expression(expr)),
            indices: self.rewrite_comprehension_indices(indices),
            filter: filter.map(|filter| Box::new(self.rewrite_expression(filter))),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ComponentReference, Literal, OpBinary};

    fn span() -> Span {
        Span::from_offsets(rumoca_core::SourceId::DUMMY, 0, 1)
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::new(name),
            subscripts: Vec::new(),
            span: span(),
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: span(),
        }
    }

    fn assign(target: &str, value: Expression) -> Statement {
        Statement::Assignment {
            comp: ComponentReference::construct(
                false,
                span(),
                vec![rumoca_core::ComponentRefPart {
                    ident: target.to_string(),
                    span: span(),
                    subs: Vec::new(),
                    def_id: rumoca_core::DefId::new(1),
                }],
            )
            .expect("test component reference has exact identity"),
            value,
            span: span(),
        }
    }

    fn above_zero() -> Expression {
        Expression::Binary {
            op: OpBinary::Gt,
            lhs: Box::new(var("u")),
            rhs: Box::new(real(0.0)),
            span: span(),
        }
    }

    fn conditional(then: Vec<Statement>, otherwise: Option<Vec<Statement>>) -> Statement {
        Statement::If {
            cond_blocks: vec![StatementBlock {
                cond: above_zero(),
                stmts: then,
            }],
            else_block: otherwise,
            span: span(),
        }
    }

    fn express_all(statements: &[Statement], owned: &[&str]) -> Vec<Statement> {
        let owned = owned
            .iter()
            .map(|name| VarName::new(*name))
            .collect::<Vec<_>>();
        express(statements, &mut Scope::new(&owned))
    }

    #[test]
    fn a_total_conditional_of_assignments_becomes_one_if_expression_per_output() {
        let source = [conditional(
            vec![assign("a", real(1.0)), assign("b", var("a"))],
            Some(vec![assign("a", real(2.0)), assign("b", real(3.0))]),
        )];
        let expressed = express_all(&source, &["a", "b"]);
        let [
            Statement::Assignment { value: first, .. },
            Statement::Assignment { value: second, .. },
        ] = &expressed[..]
        else {
            panic!("each output is one assignment: {expressed:?}");
        };
        let Expression::If {
            branches,
            else_branch,
            ..
        } = second
        else {
            panic!("the output is an if-expression");
        };
        assert_eq!(
            branches[0].1,
            real(1.0),
            "the in-branch read is substituted"
        );
        assert_eq!(**else_branch, real(3.0));
        assert!(matches!(first, Expression::If { .. }));
    }

    #[test]
    fn a_branch_assertion_keeps_the_ordered_statement() {
        let assertion = Statement::Assert {
            condition: above_zero(),
            message: Box::new(Expression::Literal {
                value: Literal::String("positive".into()),
                span: span(),
            }),
            level: None,
            span: span(),
        };
        let source = [conditional(
            vec![assertion, assign("a", real(1.0))],
            Some(vec![assign("a", real(2.0))]),
        )];
        assert_eq!(express_all(&source, &["a"]), source);
    }

    #[test]
    fn a_conditional_without_an_else_or_with_unequal_outputs_stays_ordered() {
        let partial = [conditional(vec![assign("a", real(1.0))], None)];
        assert_eq!(express_all(&partial, &["a"]), partial);
        let unequal = [conditional(
            vec![assign("a", real(1.0)), assign("b", real(1.0))],
            Some(vec![assign("a", real(2.0))]),
        )];
        assert_eq!(express_all(&unequal, &["a", "b"]), unequal);
    }

    #[test]
    fn a_read_of_a_value_some_path_leaves_undefined_stays_ordered() {
        let source = [conditional(
            vec![assign("a", var("scratch"))],
            Some(vec![assign("a", real(2.0))]),
        )];
        assert_eq!(express_all(&source, &["a", "scratch"]), source);
        let defined = [assign("scratch", real(0.5)), source[0].clone()];
        let expressed = express_all(&defined, &["a", "scratch"]);
        assert!(matches!(
            &expressed[1],
            Statement::Assignment {
                value: Expression::If { .. },
                ..
            }
        ));
    }

    #[test]
    fn a_read_of_the_replaced_value_stays_ordered() {
        let source = [
            assign("a", real(1.0)),
            conditional(
                vec![assign(
                    "a",
                    Expression::Binary {
                        op: OpBinary::Add,
                        lhs: Box::new(var("a")),
                        rhs: Box::new(real(1.0)),
                        span: span(),
                    },
                )],
                Some(vec![assign("a", real(2.0))]),
            ),
        ];
        assert_eq!(express_all(&source, &["a"]), source);
    }
}
