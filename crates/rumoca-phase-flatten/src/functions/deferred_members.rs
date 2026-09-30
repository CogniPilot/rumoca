//! Member identities Resolve deferred across a replaceable edge inside a
//! callable body.
//!
//! Resolve leaves the tail of `owner.member` without a declaration identity when
//! `owner` is a replaceable class, package, or component, because a
//! redeclaration can change the member set (MLS §7.3). Instantiation re-proves
//! those tails for the equations and algorithms of instantiated classes, but a
//! function body is never instantiated: it is converted straight from the class
//! tree. The declarations in effect for that conversion are the ones the class
//! tree records, so each member is proved here against the owning class and its
//! extends chain, exactly as a non-replaceable owner would have been proved at
//! Resolve.
//!
//! Nothing is invented: a member the owner does not declare keeps its absent
//! identity and is reported at the Flat boundary (EF024).

use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rumoca_ir_ast::visitor::ExpressionTransformer;

/// Guard against a cyclic extends chain in an unvalidated class tree.
const MAX_EXTENDS_DEPTH: usize = 32;

/// Prove deferred member identities in one callable's algorithm sections.
///
/// `components` are the callable's own effective components, which own the
/// declared type of a reference rooted in a formal parameter or local.
pub(super) fn prove_deferred_members_in_algorithms(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    exposed_function_name: &str,
    own_function_name: Option<&str>,
    components: &IndexMap<String, ast::Component>,
    algorithms: &mut [Vec<ast::Statement>],
) {
    let mut prover = DeferredMemberProver {
        tree,
        class_index,
        exposed_function_name,
        own_function_name,
        components,
    };
    for section in algorithms.iter_mut() {
        for statement in section.iter_mut() {
            let taken = std::mem::replace(statement, ast::Statement::Empty);
            *statement = prover.prove_statement(taken);
        }
    }
}

struct DeferredMemberProver<'a, 'tree> {
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'tree>,
    exposed_function_name: &'a str,
    own_function_name: Option<&'a str>,
    components: &'a IndexMap<String, ast::Component>,
}

impl ExpressionTransformer for DeferredMemberProver<'_, '_> {
    fn transform_component_ref_inner(
        &mut self,
        mut reference: ast::ComponentReference,
    ) -> ast::ComponentReference {
        for part in &mut reference.parts {
            if let Some(subscripts) = &mut part.subs {
                *subscripts = subscripts
                    .drain(..)
                    .map(|subscript| self.transform_subscript(subscript))
                    .collect();
            }
        }
        self.prove_reference_members(&mut reference);
        reference
    }
}

impl DeferredMemberProver<'_, '_> {
    /// Fill absent identities left to right, each proved as a member of the
    /// class the preceding segment resolves to.
    fn prove_reference_members(&mut self, reference: &mut ast::ComponentReference) {
        let Some(root) = reference.parts.first() else {
            return;
        };
        let Some(root_def_id) = root.def_id else {
            return;
        };
        // A class segment continues in itself; a component segment continues in
        // its declared type.
        // Callable components are the lexical declarations of a single-part
        // root and shadow class names. Their copied effective declarations may
        // not retain the occurrence DefId carried by the body reference, so
        // name ownership is the authoritative discriminator here.
        let component = self.components.get(root.ident.text.as_ref());
        let Some(component) = component else {
            let owner = self.class_index.get(root_def_id).map(|_| root_def_id);
            self.prove_member_tail(&mut reference.parts, owner, false);
            return;
        };
        let declared_owner = component.type_def_id;
        // The exposed scope may be a package alias that names only the
        // replaceable slot's constraining class, so the selected
        // implementation's own package scope is tried next: it owns the
        // redeclarations that give the formal's type its members (MLS 7.3).
        for scope in [Some(self.exposed_function_name), self.own_function_name]
            .into_iter()
            .flatten()
        {
            let owner = self.component_type_in_scope(component, scope);
            let mut scoped_parts = reference.parts.clone();
            if self.prove_member_tail(&mut scoped_parts, owner, owner != declared_owner) {
                reference.parts = scoped_parts;
                return;
            }
        }
        self.prove_member_tail(&mut reference.parts, declared_owner, false);
    }

    /// Prove the member of each segment after the root, left to right, each as
    /// a member of the class the preceding segment continues in. Returns
    /// whether every segment was proved.
    fn prove_member_tail(
        &self,
        parts: &mut [ast::ComponentRefPart],
        mut owner: Option<DefId>,
        reprove_members: bool,
    ) -> bool {
        for part in parts.iter_mut().skip(1) {
            let Some(owner_def_id) = owner else {
                return false;
            };
            let Some(owner_class) = self.class_index.get(owner_def_id) else {
                return false;
            };
            let Some(member) = member_of_class(self.class_index, owner_class, &part.ident.text)
            else {
                return false;
            };
            if reprove_members || part.def_id.is_none() {
                part.def_id = Some(member.declaration);
            }
            owner = member.continues_in;
        }
        true
    }

    /// Resolve a formal/local type in the exposed callable scope before using
    /// its members. The stored `type_def_id` belongs to the generic declaration
    /// and can cross a replaceable edge; the exposed scope owns the concrete
    /// redeclaration that proves the member identity.
    fn component_type_in_scope(&self, component: &ast::Component, scope: &str) -> Option<DefId> {
        super::resolve_function_class_with_scope(
            self.tree,
            self.class_index,
            &component.type_name.to_string(),
            Some(scope),
        )
        .and_then(|resolution| resolution.class_def.def_id)
        .or(component.type_def_id)
    }

    fn prove_statement(&mut self, statement: ast::Statement) -> ast::Statement {
        match statement {
            ast::Statement::Empty => ast::Statement::Empty,
            ast::Statement::Return { token } => ast::Statement::Return { token },
            ast::Statement::Break { token } => ast::Statement::Break { token },
            ast::Statement::Assignment { comp, value } => ast::Statement::Assignment {
                comp: self.transform_component_ref_inner(comp),
                value: self.transform_expression(value),
            },
            ast::Statement::For { indices, equations } => ast::Statement::For {
                indices: indices
                    .into_iter()
                    .map(|index| self.transform_for_index(index))
                    .collect(),
                equations: self.prove_statements(equations),
            },
            ast::Statement::While(block) => ast::Statement::While(self.prove_block(block)),
            ast::Statement::If {
                cond_blocks,
                else_block,
            } => ast::Statement::If {
                cond_blocks: cond_blocks
                    .into_iter()
                    .map(|block| self.prove_block(block))
                    .collect(),
                else_block: else_block.map(|statements| self.prove_statements(statements)),
            },
            ast::Statement::When(blocks) => ast::Statement::When(
                blocks
                    .into_iter()
                    .map(|block| self.prove_block(block))
                    .collect(),
            ),
            ast::Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => ast::Statement::FunctionCall {
                comp: self.transform_component_ref_inner(comp),
                args: self.prove_expressions(args),
                outputs: self.prove_expressions(outputs),
            },
            ast::Statement::Reinit { variable, value } => ast::Statement::Reinit {
                variable: self.transform_component_ref_inner(variable),
                value: self.transform_expression(value),
            },
            ast::Statement::Assert {
                condition,
                message,
                level,
            } => ast::Statement::Assert {
                condition: self.transform_expression(condition),
                message: self.transform_expression(message),
                level: level.map(|level| Box::new(self.transform_expression(*level))),
            },
        }
    }

    fn prove_statements(&mut self, statements: Vec<ast::Statement>) -> Vec<ast::Statement> {
        statements
            .into_iter()
            .map(|statement| self.prove_statement(statement))
            .collect()
    }

    fn prove_expressions(&mut self, expressions: Vec<ast::Expression>) -> Vec<ast::Expression> {
        expressions
            .into_iter()
            .map(|expression| self.transform_expression(expression))
            .collect()
    }

    fn prove_block(&mut self, block: ast::StatementBlock) -> ast::StatementBlock {
        ast::StatementBlock {
            cond: self.transform_expression(block.cond),
            stmts: self.prove_statements(block.stmts),
        }
    }
}

/// One member declaration of a class, plus the class a further segment
/// continues in.
struct ClassMember {
    declaration: DefId,
    continues_in: Option<DefId>,
}

/// Find `name` in `class_def` or in the classes it extends.
fn member_of_class(
    class_index: &ast::ClassDefIndex<'_>,
    class_def: &ast::ClassDef,
    name: &str,
) -> Option<ClassMember> {
    let mut frontier = vec![class_def];
    let mut visited = std::collections::HashSet::new();
    for _ in 0..MAX_EXTENDS_DEPTH {
        let mut next = Vec::new();
        for class_def in frontier.drain(..) {
            if let Some(member) = declared_member(class_def, name) {
                return member;
            }
            next.extend(unvisited_base_classes(class_index, class_def, &mut visited));
        }
        if next.is_empty() {
            return None;
        }
        frontier = next;
    }
    None
}

/// The member `class_def` declares itself, if any.
///
/// An outer `None` means the class declares no such member; an inner `None`
/// means it declares one without a proved identity, which is not a member
/// identity this pass may supply.
fn declared_member(class_def: &ast::ClassDef, name: &str) -> Option<Option<ClassMember>> {
    if let Some(component) = class_def.components.get(name) {
        return Some(component.def_id.map(|declaration| ClassMember {
            declaration,
            continues_in: component.type_def_id,
        }));
    }
    let nested = class_def.classes.get(name)?;
    Some(nested.def_id.map(|declaration| ClassMember {
        declaration,
        continues_in: Some(declaration),
    }))
}

fn unvisited_base_classes<'tree>(
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &ast::ClassDef,
    visited: &mut std::collections::HashSet<DefId>,
) -> Vec<&'tree ast::ClassDef> {
    class_def
        .extends
        .iter()
        .filter_map(|extend| extend.base_def_id.or(extend.base_name.def_id))
        .filter(|base_def_id| visited.insert(*base_def_id))
        .filter_map(|base_def_id| class_index.get(base_def_id))
        .collect()
}
