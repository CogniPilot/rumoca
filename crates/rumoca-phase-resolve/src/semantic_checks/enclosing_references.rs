//! Scope-identity checks that require the fully resolved class tree.

use super::*;
use rumoca_core::ScopeId;
use rumoca_ir_ast::{walk_class_def_default, walk_component_reference_default};
use std::collections::HashSet;
use std::ops::ControlFlow;

const ER130_NON_CONSTANT_ENCLOSING_REFERENCE: &str = "ER130";

pub(super) fn run_enclosing_reference_checks(tree: &ast::ClassTree) -> Vec<Diagnostic> {
    let mut non_constant_components = HashSet::new();
    collect_non_constant_components(&tree.definitions, &mut non_constant_components);

    let mut visitor = EnclosingReferenceVisitor {
        tree,
        non_constant_components,
        current_scope: None,
        current_class_is_short: false,
        diagnostics: Vec::new(),
    };
    let _ = visitor.visit_stored_definition(&tree.definitions);
    visitor.diagnostics
}

fn collect_non_constant_components(
    definition: &StoredDefinition,
    non_constant_components: &mut HashSet<DefId>,
) {
    for class in definition.classes.values() {
        collect_class_non_constant_components(class, non_constant_components);
    }
}

fn collect_class_non_constant_components(
    class: &ClassDef,
    non_constant_components: &mut HashSet<DefId>,
) {
    for component in class.components.values() {
        if !matches!(component.variability, Variability::Constant(_))
            && let Some(def_id) = component.def_id
        {
            non_constant_components.insert(def_id);
        }
    }
    for nested in class.classes.values() {
        collect_class_non_constant_components(nested, non_constant_components);
    }
}

struct EnclosingReferenceVisitor<'a> {
    tree: &'a ast::ClassTree,
    non_constant_components: HashSet<DefId>,
    current_scope: Option<ScopeId>,
    current_class_is_short: bool,
    diagnostics: Vec<Diagnostic>,
}

impl EnclosingReferenceVisitor<'_> {
    fn is_declared_in_enclosing_class(&self, current_scope: ScopeId, target: DefId) -> bool {
        let mut enclosing = self.tree.scope_tree.parent(current_scope);
        while let Some(scope) = enclosing {
            if self.tree.scope_to_class.contains_key(&scope) {
                if self.tree.scope_tree.declares(scope, target)
                    || self.tree.scope_tree.inherits_unique(scope, target)
                {
                    return true;
                }
            }
            enclosing = self.tree.scope_tree.parent(scope);
        }
        false
    }
}

impl Visitor for EnclosingReferenceVisitor<'_> {
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        let previous_scope = self.current_scope.replace(
            class
                .scope_id
                .expect("resolved class definitions must carry a ScopeId"),
        );
        let previous_short = std::mem::replace(
            &mut self.current_class_is_short,
            class.end_name_token.is_none() && !class.encapsulated,
        );
        let result = walk_class_def_default(self, class);
        self.current_class_is_short = previous_short;
        self.current_scope = previous_scope;
        result
    }

    fn visit_expression_ctx(
        &mut self,
        expression: &Expression,
        context: rumoca_ir_ast::ExpressionContext,
    ) -> ControlFlow<()> {
        if context == rumoca_ir_ast::ExpressionContext::ExtendModification
            && self.current_class_is_short
        {
            // A short-class modification is written and evaluated in the
            // enclosing instance scope. For example, in
            // `function f = Base(g0 = g)`, `g` is a component beside `f`,
            // not an illegal capture from inside `f`.
            let previous_scope = self.current_scope;
            self.current_scope =
                previous_scope.and_then(|scope| self.tree.scope_tree.parent(scope));
            let result = self.visit_expression(expression);
            self.current_scope = previous_scope;
            return result;
        }
        self.visit_expression(expression)
    }

    fn visit_component_reference(&mut self, reference: &ComponentReference) -> ControlFlow<()> {
        let Some(target) = reference.def_id else {
            return walk_component_reference_default(self, reference);
        };
        if !self.non_constant_components.contains(&target) {
            return walk_component_reference_default(self, reference);
        }
        let Some(current_scope) = self.current_scope else {
            return walk_component_reference_default(self, reference);
        };
        let Some(first_part) = reference.parts.first() else {
            return walk_component_reference_default(self, reference);
        };
        if self.is_declared_in_enclosing_class(current_scope, target) {
            self.diagnostics.push(semantic_error(
                ER130_NON_CONSTANT_ENCLOSING_REFERENCE,
                format!(
                    "reference to non-constant variable '{}' from a lexically nested class is not allowed (MLS §5.3.1)",
                    first_part.ident.text
                ),
                label_from_token(
                    &first_part.ident,
                    "enclosing_references/non_constant",
                    "variables referenced through an enclosing class must be constant",
                ),
            ));
        }
        walk_component_reference_default(self, reference)
    }
}
