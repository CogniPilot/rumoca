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
    class_index: &ast::ClassDefIndex<'_>,
    callable_def_id: Option<DefId>,
    components: &IndexMap<String, ast::Component>,
    algorithms: &mut [Vec<ast::Statement>],
) {
    let mut prover = DeferredMemberProver {
        class_index,
        callable_def_id,
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
    class_index: &'a ast::ClassDefIndex<'tree>,
    callable_def_id: Option<DefId>,
    components: &'a IndexMap<String, ast::Component>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NestedTypeSelection {
    None,
    Unique(DefId),
    Ambiguous,
    Rejected,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NestedTypeOverride {
    None,
    Selected(DefId),
    Rejected,
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
        // root and shadow class names. The map key locates a candidate, but
        // the occurrence DefId remains the authoritative owner discriminator;
        // copied declarations without that exact identity stay unresolved.
        let component = self
            .components
            .get(root.ident.text.as_ref())
            .filter(|component| component.def_id == Some(root_def_id));
        let (mut owner, reprove_members) = if let Some(component) = component {
            let contextual_owner = self.contextual_component_type(component);
            let declared_owner = component.type_def_id;
            (contextual_owner, contextual_owner != declared_owner)
        } else {
            (
                self.class_index.get(root_def_id).map(|_| root_def_id),
                false,
            )
        };
        for part in reference.parts.iter_mut().skip(1) {
            let Some(owner_def_id) = owner else {
                return;
            };
            let Some(owner_class) = self.class_index.get(owner_def_id) else {
                return;
            };
            let Some(member) = member_of_class(self.class_index, owner_class, &part.ident.text)
            else {
                return;
            };
            if reprove_members || part.def_id.is_none() {
                part.def_id = Some(member.declaration);
            }
            owner = member.continues_in;
        }
    }

    /// Resolve a formal/local type in the exposed callable scope before using
    /// its members. The stored `type_def_id` belongs to the generic declaration
    /// and can cross a replaceable edge; the exposed scope owns the concrete
    /// redeclaration that proves the member identity.
    fn contextual_component_type(&self, component: &ast::Component) -> Option<DefId> {
        let declared = component.type_def_id?;
        match self.selected_nested_type(declared) {
            NestedTypeSelection::None => Some(declared),
            NestedTypeSelection::Unique(selected) => Some(selected),
            NestedTypeSelection::Ambiguous | NestedTypeSelection::Rejected => None,
        }
    }

    /// Select a concrete nested class by the declaration identity that crossed
    /// the replaceable edge.  A callable body is lowered from its selected
    /// implementation class, so the implementation owner's direct redeclare
    /// carries the exact record identity needed for deferred field members.
    ///
    /// The match is entirely DefId based: a rendered type name is never used to
    /// choose a scope or to manufacture a member identity.  Multiple concrete
    /// candidates are rejected as unresolved so an ambiguous inherited record
    /// cannot become an insertion-order choice.
    fn selected_nested_type(&self, declared: DefId) -> NestedTypeSelection {
        if self.class_index.get(declared).is_none() {
            return NestedTypeSelection::None;
        }
        let Some(owner) = self
            .callable_def_id
            .and_then(|callable| self.class_index.parent_def_id(callable))
        else {
            return NestedTypeSelection::None;
        };
        let mut visited = std::collections::HashSet::new();
        let mut frontier = vec![owner];
        while !frontier.is_empty() {
            let (candidates, next, rejected) =
                self.scan_nested_type_frontier(&mut frontier, declared, &mut visited);
            if rejected {
                return NestedTypeSelection::Rejected;
            }
            if !candidates.is_empty() {
                return match candidates.len() {
                    1 => NestedTypeSelection::Unique(candidates.into_iter().next().unwrap()),
                    _ => NestedTypeSelection::Ambiguous,
                };
            }
            frontier = next;
        }
        NestedTypeSelection::None
    }

    fn scan_nested_type_frontier(
        &self,
        frontier: &mut Vec<DefId>,
        declared: DefId,
        visited: &mut std::collections::HashSet<DefId>,
    ) -> (std::collections::HashSet<DefId>, Vec<DefId>, bool) {
        let mut next = Vec::new();
        let mut candidates = std::collections::HashSet::new();
        for owner_def_id in frontier.drain(..) {
            let Some((owner_candidates, bases, rejected)) =
                self.scan_nested_type_owner(owner_def_id, declared, visited)
            else {
                continue;
            };
            if rejected {
                return (std::collections::HashSet::new(), Vec::new(), true);
            }
            candidates.extend(owner_candidates);
            next.extend(bases);
        }
        (candidates, next, false)
    }

    fn scan_nested_type_owner(
        &self,
        owner_def_id: DefId,
        declared: DefId,
        visited: &mut std::collections::HashSet<DefId>,
    ) -> Option<(Vec<DefId>, Vec<DefId>, bool)> {
        if !visited.insert(owner_def_id) {
            return None;
        }
        let owner_class = self.class_index.get(owner_def_id)?;
        let mut candidates: Vec<DefId> = owner_class
            .classes
            .values()
            .filter(|nested| {
                nested.def_id == Some(declared)
                    || (nested.is_redeclare
                        && nested
                            .redeclare_target_def_id
                            .is_some_and(|target| self.redeclare_slot_reaches(target, declared)))
            })
            .filter_map(|nested| nested.def_id)
            .collect();
        match self.extends_modifier_nested_type(owner_def_id, declared) {
            NestedTypeOverride::None => {}
            NestedTypeOverride::Selected(selected) => {
                candidates.push(selected);
            }
            NestedTypeOverride::Rejected => return Some((Vec::new(), Vec::new(), true)),
        }
        let bases = owner_class
            .extends
            .iter()
            .filter_map(|extend| extend.base_def_id)
            .filter(|base| !visited.contains(base))
            .collect();
        Some((candidates, bases, false))
    }

    /// Select a record named by an exact redeclare in an `extends` modifier.
    /// The modifier's target must reach the formal slot by its resolved
    /// redeclare chain, and its RHS must already carry an exact terminal
    /// DefId. A rendered RHS name cannot prove a selected type here.
    fn extends_modifier_nested_type(
        &self,
        owner_def_id: DefId,
        declared: DefId,
    ) -> NestedTypeOverride {
        let Some(owner_class) = self.class_index.get(owner_def_id) else {
            return NestedTypeOverride::None;
        };
        let Some(member) = self.class_index.local_name(declared) else {
            return NestedTypeOverride::None;
        };
        let matching_modifiers = owner_class
            .extends
            .iter()
            .flat_map(|extend| extend.modifications.iter())
            .filter(|modification| modification.redeclare)
            .filter(|modification| modification_member_name(&modification.expr) == Some(member))
            .collect::<Vec<_>>();
        let [modification] = matching_modifiers.as_slice() else {
            return if matching_modifiers.is_empty() {
                NestedTypeOverride::None
            } else {
                NestedTypeOverride::Rejected
            };
        };
        let Some(slot) = modification_target_def_id(&modification.expr) else {
            return NestedTypeOverride::Rejected;
        };
        if !self.redeclare_slot_reaches(slot, declared) {
            return NestedTypeOverride::Rejected;
        }
        let Some(target) = modification_value_target_def_id(&modification.expr)
            .filter(|target| self.class_index.get(*target).is_some())
        else {
            return NestedTypeOverride::Rejected;
        };
        NestedTypeOverride::Selected(target)
    }

    /// Follow only compiler-resolved redeclare-slot links. An implementation
    /// may redeclare an inherited redeclaration, so its target reaches the
    /// original formal slot through a finite DefId chain. An `extends` edge,
    /// spelling, or same-named class is not evidence of slot replacement.
    fn redeclare_slot_reaches(&self, start: DefId, declared: DefId) -> bool {
        let mut current = Some(start);
        let mut visited = std::collections::HashSet::new();
        for _ in 0..MAX_EXTENDS_DEPTH {
            let Some(def_id) = current else {
                return false;
            };
            if def_id == declared {
                return true;
            }
            if !visited.insert(def_id) {
                return false;
            }
            current = self
                .class_index
                .get(def_id)
                .and_then(|class| class.redeclare_target_def_id);
        }
        false
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

fn modification_target_def_id(expr: &ast::Expression) -> Option<DefId> {
    match expr {
        ast::Expression::Modification { target, .. }
        | ast::Expression::ClassModification { target, .. }
        | ast::Expression::ComponentReference(target) => target.target_def_id(),
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            ..
        } => match lhs.as_ref() {
            ast::Expression::ComponentReference(target)
            | ast::Expression::ClassModification { target, .. } => target.target_def_id(),
            _ => None,
        },
        _ => None,
    }
}

fn modification_value_target_def_id(expr: &ast::Expression) -> Option<DefId> {
    let value = match expr {
        ast::Expression::Modification { value, .. } => value,
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            rhs,
            ..
        } => rhs,
        _ => return None,
    };
    match value.as_ref() {
        ast::Expression::ComponentReference(reference)
        | ast::Expression::ClassModification {
            target: reference, ..
        }
        | ast::Expression::FunctionCall {
            comp: reference, ..
        } => reference.target_def_id(),
        _ => None,
    }
}

fn modification_member_name(expr: &ast::Expression) -> Option<&str> {
    let target = match expr {
        ast::Expression::Modification { target, .. }
        | ast::Expression::ClassModification { target, .. }
        | ast::Expression::ComponentReference(target) => target,
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            ..
        } => match lhs.as_ref() {
            ast::Expression::ComponentReference(target)
            | ast::Expression::ClassModification { target, .. } => target,
            _ => return None,
        },
        _ => return None,
    };
    (target.parts.len() == 1).then(|| target.parts[0].ident.text.as_ref())
}

/// One member declaration of a class, plus the class a further segment
/// continues in.
#[derive(Clone, Copy, PartialEq, Eq)]
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
        let mut inherited_member = None;
        let mut unresolved_member = false;
        for class_def in frontier.drain(..) {
            match declared_member(class_def, name) {
                Some(Some(member))
                    if inherited_member.is_none_or(|candidate| candidate == member) =>
                {
                    inherited_member = Some(member)
                }
                Some(Some(_)) => return None,
                Some(None) => unresolved_member = true,
                None => {}
            }
            next.extend(unvisited_base_classes(class_index, class_def, &mut visited));
        }
        if unresolved_member {
            return None;
        }
        if inherited_member.is_some() {
            return inherited_member;
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
        .filter_map(|extend| extend.base_def_id)
        .filter(|base_def_id| visited.insert(*base_def_id))
        .filter_map(|base_def_id| class_index.get(base_def_id))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{DefId, Span, Token};

    fn token(text: &str) -> Token {
        Token {
            text: text.into(),
            ..Token::default()
        }
    }

    fn reference(parts: &[(&str, Option<DefId>)]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: parts
                .iter()
                .map(|(name, def_id)| ast::ComponentRefPart {
                    ident: token(name),
                    subs: None,
                    def_id: *def_id,
                })
                .collect(),
            span: Span::DUMMY,
            qualified_display_name: None,
        }
    }

    fn record_with_field(class_id: DefId, name: &str, field_id: DefId) -> ast::ClassDef {
        ast::ClassDef {
            def_id: Some(class_id),
            name: token(name),
            components: IndexMap::from_iter([(
                "T".to_string(),
                ast::Component {
                    name: "T".to_string(),
                    def_id: Some(field_id),
                    ..ast::Component::empty_with_span(Span::DUMMY)
                },
            )]),
            ..ast::ClassDef::default()
        }
    }

    fn redeclare(
        slot_id: DefId,
        target_name: &str,
        target_id: Option<DefId>,
    ) -> ast::ExtendModification {
        ast::ExtendModification {
            expr: ast::Expression::Modification {
                target: reference(&[("State", Some(slot_id))]),
                value: std::sync::Arc::new(ast::Expression::ClassModification {
                    target: reference(&[(target_name, target_id)]),
                    modifications: Vec::new(),
                    each_flags: Vec::new(),
                    final_flags: Vec::new(),
                    redeclare_flags: Vec::new(),
                    span: Span::DUMMY,
                }),
                span: Span::DUMMY,
            },
            redeclare: true,
            each: false,
            final_: false,
        }
    }

    fn malformed_extends_fixture() -> (ast::ClassTree, DefId, DefId, DefId, DefId) {
        let base_state_id = DefId::new(50);
        let generic_field_id = DefId::new(51);
        let selected_state_id = DefId::new(60);
        let selected_field_id = DefId::new(61);
        let base_id = DefId::new(70);
        let concrete_id = DefId::new(80);
        let callable_id = DefId::new(81);
        let state_component_id = DefId::new(90);
        let state = record_with_field(base_state_id, "State", generic_field_id);
        let selected = record_with_field(selected_state_id, "SelectedState", selected_field_id);
        let base = ast::ClassDef {
            def_id: Some(base_id),
            name: token("Base"),
            classes: IndexMap::from_iter([
                ("State".to_string(), state),
                ("SelectedState".to_string(), selected),
            ]),
            ..ast::ClassDef::default()
        };
        let callable = ast::ClassDef {
            def_id: Some(callable_id),
            name: token("f"),
            components: IndexMap::from_iter([(
                "state".to_string(),
                ast::Component {
                    name: "state".to_string(),
                    def_id: Some(state_component_id),
                    type_def_id: Some(base_state_id),
                    ..ast::Component::empty_with_span(Span::DUMMY)
                },
            )]),
            ..ast::ClassDef::default()
        };
        let concrete = ast::ClassDef {
            def_id: Some(concrete_id),
            name: token("Concrete"),
            extends: vec![ast::Extend {
                base_name: ast::Name::from_string("Base"),
                base_def_id: Some(base_id),
                modifications: vec![redeclare(base_state_id, "SelectedState", None)],
                ..ast::Extend::default()
            }],
            classes: IndexMap::from_iter([("f".to_string(), callable)]),
            ..ast::ClassDef::default()
        };
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("Base".to_string(), base);
        tree.definitions
            .classes
            .insert("Concrete".to_string(), concrete);
        (
            tree,
            callable_id,
            generic_field_id,
            selected_field_id,
            state_component_id,
        )
    }

    fn duplicate_extends_fixture(
        include_second: bool,
    ) -> (ast::ClassTree, DefId, DefId, DefId, DefId, DefId) {
        let state_id = DefId::new(100);
        let base_id = DefId::new(101);
        let concrete_id = DefId::new(102);
        let selected_one_id = DefId::new(103);
        let selected_two_id = DefId::new(104);
        let mut modifications = vec![redeclare(state_id, "SelectedOne", Some(selected_one_id))];
        if include_second {
            modifications.push(redeclare(state_id, "SelectedTwo", Some(selected_two_id)));
        }
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert(
            "Base".to_string(),
            ast::ClassDef {
                def_id: Some(base_id),
                name: token("Base"),
                classes: IndexMap::from_iter([(
                    "State".to_string(),
                    ast::ClassDef {
                        def_id: Some(state_id),
                        name: token("State"),
                        ..ast::ClassDef::default()
                    },
                )]),
                ..ast::ClassDef::default()
            },
        );
        tree.definitions.classes.insert(
            "Concrete".to_string(),
            ast::ClassDef {
                def_id: Some(concrete_id),
                name: token("Concrete"),
                extends: vec![ast::Extend {
                    base_name: ast::Name::from_string("Base"),
                    base_def_id: Some(base_id),
                    modifications,
                    ..ast::Extend::default()
                }],
                ..ast::ClassDef::default()
            },
        );
        for (name, def_id) in [
            ("SelectedOne", selected_one_id),
            ("SelectedTwo", selected_two_id),
        ] {
            tree.definitions.classes.insert(
                name.to_string(),
                ast::ClassDef {
                    def_id: Some(def_id),
                    name: token(name),
                    ..ast::ClassDef::default()
                },
            );
        }
        (
            tree,
            state_id,
            base_id,
            concrete_id,
            selected_one_id,
            selected_two_id,
        )
    }

    #[test]
    fn root_component_name_cannot_replace_a_wrong_root_def_id() {
        let state_def_id = DefId::new(20);
        let state_field_def_id = DefId::new(30);
        let root_def_id = DefId::new(10);
        let wrong_component_def_id = DefId::new(11);
        let state = ast::ClassDef {
            def_id: Some(state_def_id),
            components: IndexMap::from_iter([(
                "T".to_string(),
                ast::Component {
                    def_id: Some(state_field_def_id),
                    name: "T".to_string(),
                    ..ast::Component::empty_with_span(Span::DUMMY)
                },
            )]),
            ..ast::ClassDef::default()
        };
        let mut tree = ast::ClassTree::new();
        tree.definitions.classes.insert("State".to_string(), state);
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let components = IndexMap::from_iter([(
            "state".to_string(),
            ast::Component {
                def_id: Some(wrong_component_def_id),
                type_def_id: Some(state_def_id),
                name: "state".to_string(),
                ..ast::Component::empty_with_span(Span::DUMMY)
            },
        )]);
        let mut algorithms = vec![vec![ast::Statement::Assignment {
            comp: reference(&[("y", Some(DefId::new(40)))]),
            value: ast::Expression::ComponentReference(reference(&[
                ("state", Some(root_def_id)),
                ("T", None),
            ])),
        }]];

        prove_deferred_members_in_algorithms(&class_index, None, &components, &mut algorithms);

        let ast::Statement::Assignment { value, .. } = &algorithms[0][0] else {
            panic!("expected assignment")
        };
        let ast::Expression::ComponentReference(reference) = value else {
            panic!("expected component reference")
        };
        assert_eq!(reference.parts[1].def_id, None);
    }

    #[test]
    fn malformed_extends_rhs_cannot_fall_through_to_generic_record_identity() {
        let (tree, callable_id, generic_field_id, selected_field_id, state_component_id) =
            malformed_extends_fixture();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let components = class_index
            .get(callable_id)
            .expect("callable")
            .components
            .clone();
        let mut algorithms = vec![vec![ast::Statement::Assignment {
            comp: reference(&[("y", Some(DefId::new(91)))]),
            value: ast::Expression::ComponentReference(reference(&[
                ("state", Some(state_component_id)),
                ("T", None),
            ])),
        }]];

        prove_deferred_members_in_algorithms(
            &class_index,
            Some(callable_id),
            &components,
            &mut algorithms,
        );

        let ast::Statement::Assignment { value, .. } = &algorithms[0][0] else {
            panic!("expected assignment")
        };
        let ast::Expression::ComponentReference(reference) = value else {
            panic!("expected component reference")
        };
        assert_eq!(reference.parts[1].def_id, None);
        assert_ne!(generic_field_id, selected_field_id);
    }

    #[test]
    fn duplicate_extends_redeclare_slot_is_rejected_before_rhs_selection() {
        let (mut tree, state_id, _base_id, concrete_id, selected_one_id, selected_two_id) =
            duplicate_extends_fixture(false);
        {
            let class_index = ast::ClassDefIndex::from_tree(&tree);
            let components = IndexMap::default();
            let prover = DeferredMemberProver {
                class_index: &class_index,
                callable_def_id: None,
                components: &components,
            };
            assert_eq!(
                prover.extends_modifier_nested_type(concrete_id, state_id),
                NestedTypeOverride::Selected(selected_one_id)
            );
        }
        let concrete = tree
            .definitions
            .classes
            .get_mut("Concrete")
            .expect("Concrete");
        concrete
            .extends
            .first_mut()
            .expect("Base extends")
            .modifications
            .push(redeclare(state_id, "SelectedTwo", Some(selected_two_id)));
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let components = IndexMap::default();
        let prover = DeferredMemberProver {
            class_index: &class_index,
            callable_def_id: None,
            components: &components,
        };
        assert_eq!(
            prover.extends_modifier_nested_type(concrete_id, state_id),
            NestedTypeOverride::Rejected
        );
    }

    #[test]
    fn derived_extends_redeclare_shadows_inherited_modifier() {
        let state_id = DefId::new(110);
        let parent_id = DefId::new(111);
        let base_id = DefId::new(112);
        let derived_id = DefId::new(113);
        let selected_a_id = DefId::new(114);
        let selected_b_id = DefId::new(115);
        let callable_id = DefId::new(116);

        let redeclare = |target_name: &str, target_id| ast::ExtendModification {
            expr: ast::Expression::Modification {
                target: reference(&[("State", Some(state_id))]),
                value: std::sync::Arc::new(ast::Expression::ClassModification {
                    target: reference(&[(target_name, Some(target_id))]),
                    modifications: Vec::new(),
                    each_flags: Vec::new(),
                    final_flags: Vec::new(),
                    redeclare_flags: Vec::new(),
                    span: Span::DUMMY,
                }),
                span: Span::DUMMY,
            },
            redeclare: true,
            each: false,
            final_: false,
        };
        let state = ast::ClassDef {
            def_id: Some(state_id),
            name: token("State"),
            ..ast::ClassDef::default()
        };
        let parent = ast::ClassDef {
            def_id: Some(parent_id),
            name: token("Parent"),
            classes: IndexMap::from_iter([("State".to_string(), state)]),
            ..ast::ClassDef::default()
        };
        let base = ast::ClassDef {
            def_id: Some(base_id),
            name: token("Base"),
            extends: vec![ast::Extend {
                base_name: ast::Name::from_string("Parent"),
                base_def_id: Some(parent_id),
                modifications: vec![redeclare("SelectedA", selected_a_id)],
                ..ast::Extend::default()
            }],
            ..ast::ClassDef::default()
        };
        let derived = ast::ClassDef {
            def_id: Some(derived_id),
            name: token("Derived"),
            extends: vec![ast::Extend {
                base_name: ast::Name::from_string("Base"),
                base_def_id: Some(base_id),
                modifications: vec![redeclare("SelectedB", selected_b_id)],
                ..ast::Extend::default()
            }],
            classes: IndexMap::from_iter([(
                "f".to_string(),
                ast::ClassDef {
                    def_id: Some(callable_id),
                    name: token("f"),
                    ..ast::ClassDef::default()
                },
            )]),
            ..ast::ClassDef::default()
        };
        let mut tree = ast::ClassTree::new();
        tree.definitions
            .classes
            .insert("Parent".to_string(), parent);
        tree.definitions.classes.insert("Base".to_string(), base);
        tree.definitions
            .classes
            .insert("Derived".to_string(), derived);
        tree.definitions.classes.insert(
            "SelectedA".to_string(),
            ast::ClassDef {
                def_id: Some(selected_a_id),
                name: token("SelectedA"),
                ..ast::ClassDef::default()
            },
        );
        tree.definitions.classes.insert(
            "SelectedB".to_string(),
            ast::ClassDef {
                def_id: Some(selected_b_id),
                name: token("SelectedB"),
                ..ast::ClassDef::default()
            },
        );
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let components = IndexMap::default();
        let prover = DeferredMemberProver {
            class_index: &class_index,
            callable_def_id: Some(callable_id),
            components: &components,
        };

        assert_eq!(
            prover.selected_nested_type(state_id),
            NestedTypeSelection::Unique(selected_b_id)
        );
    }
}
