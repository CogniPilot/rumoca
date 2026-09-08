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
use rumoca_ir_ast::visitor::{
    CalleeSite, ComponentReferenceSite, ExpressionTransformer, SemanticReferenceEditor,
    transform_callee_in_place, transform_component_reference_in_place,
    transform_expression_in_place, transform_for_index_in_place,
};

/// Prove deferred member identities in one callable's algorithm sections.
///
/// `components` are the callable's own effective components, which own the
/// declared type of a reference rooted in a formal parameter or local.
pub(super) fn prove_deferred_members_in_algorithms(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    exposed_function_name: &str,
    components: &IndexMap<String, super::function_context::OriginComponent>,
    algorithms: &mut [super::function_context::OriginAlgorithmSection],
) {
    let mut prover = DeferredMemberProver {
        tree,
        class_index,
        exposed_function_name,
        components,
    };
    for section in algorithms.iter_mut() {
        for statement in &mut section.statements {
            prover.prove_statement(statement);
        }
    }
}

struct DeferredMemberProver<'a, 'tree> {
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'tree>,
    exposed_function_name: &'a str,
    components: &'a IndexMap<String, super::function_context::OriginComponent>,
}

impl ExpressionTransformer for DeferredMemberProver<'_, '_> {
    fn transform_component_reference(
        &mut self,
        reference: SemanticReferenceEditor<'_>,
        _site: ComponentReferenceSite,
    ) {
        self.prove_reference_members(reference);
    }

    fn transform_callee(&mut self, callee: SemanticReferenceEditor<'_>, _site: CalleeSite) {
        self.prove_reference_members(callee);
    }
}

impl DeferredMemberProver<'_, '_> {
    /// Fill absent identities left to right, each proved as a member of the
    /// class the preceding segment resolves to.
    fn prove_reference_members(&mut self, mut reference: SemanticReferenceEditor<'_>) {
        let Some((mut owner, reprove_members)) = self.root_owner(reference.view()) else {
            return;
        };
        for mut slot in reference.part_identity_slots().skip(1) {
            let Some(owner_def_id) = owner else {
                return;
            };
            let Some(owner_class) = self.class_index.get(owner_def_id) else {
                return;
            };
            let Some(member) = member_of_class(self.class_index, owner_class, slot.ident_text())
            else {
                return;
            };
            if reprove_members || slot.def_id().is_none() {
                slot.set_def_id(member.declaration);
            }
            owner = member.continues_in;
        }
    }

    /// The class the reference's root resolves to, with whether already-proved
    /// member identities must be re-proved against it.
    ///
    /// A class segment continues in itself; a component segment continues in
    /// its declared type. Callable components are the lexical declarations of
    /// a single-part root and shadow class names. Their copied effective
    /// declarations may not retain the occurrence DefId carried by the body
    /// reference, so name ownership is the authoritative discriminator here.
    fn root_owner(
        &self,
        reference: ast::ComponentReferenceView<'_>,
    ) -> Option<(Option<DefId>, bool)> {
        let root = reference.parts().next()?;
        let root_def_id = root.def_id()?;
        let component = self
            .components
            .get(root.ident_text())
            .map(|member| &member.component);
        Some(if let Some(component) = component {
            let contextual_owner = self.contextual_component_type(component);
            let declared_owner = component.type_def_id;
            (contextual_owner, contextual_owner != declared_owner)
        } else {
            (
                self.class_index.get(root_def_id).map(|_| root_def_id),
                false,
            )
        })
    }

    /// Resolve a formal/local type in the exposed callable scope before using
    /// its members. The stored `type_def_id` belongs to the generic declaration
    /// and can cross a replaceable edge; the exposed scope owns the concrete
    /// redeclaration that proves the member identity.
    fn contextual_component_type(&self, component: &ast::Component) -> Option<DefId> {
        super::resolve_function_class_with_scope(
            self.tree,
            self.class_index,
            &component.type_name.to_string(),
            Some(self.exposed_function_name),
        )
        .and_then(|resolution| resolution.class_def.def_id)
        .or(component.type_def_id)
    }

    fn prove_statement(&mut self, statement: &mut ast::Statement) {
        match statement {
            ast::Statement::Empty
            | ast::Statement::Return { .. }
            | ast::Statement::Break { .. } => {}
            ast::Statement::Assignment { comp, value } => {
                transform_component_reference_in_place(self, comp);
                transform_expression_in_place(self, value);
            }
            ast::Statement::For { indices, equations } => {
                for index in indices {
                    transform_for_index_in_place(self, index);
                }
                self.prove_statements(equations);
            }
            ast::Statement::While(block) => self.prove_block(block),
            ast::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    self.prove_block(block);
                }
                if let Some(statements) = else_block {
                    self.prove_statements(statements);
                }
            }
            ast::Statement::When(blocks) => {
                for block in blocks {
                    self.prove_block(block);
                }
            }
            ast::Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => {
                transform_callee_in_place(self, comp);
                self.prove_expressions(args);
                self.prove_expressions(outputs);
            }
            ast::Statement::Reinit { variable, value } => {
                transform_component_reference_in_place(self, variable);
                transform_expression_in_place(self, value);
            }
            ast::Statement::Assert {
                condition,
                message,
                level,
            } => {
                transform_expression_in_place(self, condition);
                transform_expression_in_place(self, message);
                if let Some(level) = level {
                    transform_expression_in_place(self, level.as_mut());
                }
            }
        }
    }

    fn prove_statements(&mut self, statements: &mut [ast::Statement]) {
        for statement in statements {
            self.prove_statement(statement);
        }
    }

    fn prove_expressions(&mut self, expressions: &mut [ast::Expression]) {
        for expression in expressions {
            transform_expression_in_place(self, expression);
        }
    }

    fn prove_block(&mut self, block: &mut ast::StatementBlock) {
        transform_expression_in_place(self, &mut block.cond);
        self.prove_statements(&mut block.stmts);
    }
}

/// One member declaration of a class, plus the class a further segment
/// continues in.
struct ClassMember {
    declaration: DefId,
    continues_in: Option<DefId>,
}

/// Find `name` in `class_def` or in the classes it extends.
fn member_of_class<'tree>(
    class_index: &ast::ClassDefIndex<'tree>,
    class_def: &'tree ast::ClassDef,
    name: &str,
) -> Option<ClassMember> {
    super::function_metadata::inherited_class_member(class_index, class_def, |class| {
        declared_member(class, name)
    })
    .flatten()
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
