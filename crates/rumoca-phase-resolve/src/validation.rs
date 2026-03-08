//! Validation utilities for the resolution phase.
//!
//! Provides a visitor to find unresolved symbols after resolution.

use rumoca_ir_ast as ast;
use rumoca_ir_core::Location;
use std::ops::ControlFlow;

type ClassDef = ast::ClassDef;
type ClassTree = ast::ClassTree;
type Component = ast::Component;
type ComponentReference = ast::ComponentReference;
type Equation = ast::Equation;
type Expression = ast::Expression;
type Extend = ast::Extend;
type Statement = ast::Statement;
type StoredDefinition = ast::StoredDefinition;

/// An unresolved symbol found during validation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedSymbol {
    pub name: String,
    pub kind: UnresolvedKind,
    pub scope_path: String,
    pub source_location: Location,
}

/// The kind of unresolved symbol.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnresolvedKind {
    TypeReference,
    ComponentReference,
    ExtendsBase,
    FunctionCall,
}

impl std::fmt::Display for UnresolvedSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} '{}' at {}",
            self.kind, self.name, self.source_location
        )
    }
}

/// Result of validation containing all unresolved symbols.
#[derive(Debug, Default)]
pub struct ValidationResult {
    pub unresolved: Vec<UnresolvedSymbol>,
}

impl ValidationResult {
    pub fn is_fully_resolved(&self) -> bool {
        self.unresolved.is_empty()
    }

    pub fn unresolved_count(&self) -> usize {
        self.unresolved.len()
    }
}

/// Validate that a ClassTree has all symbols resolved.
pub fn validate_resolution(tree: &ClassTree) -> ValidationResult {
    let mut v = Validator::default();
    v.visit_stored_def(&tree.definitions);
    ValidationResult {
        unresolved: v.unresolved.into_iter().collect(),
    }
}

#[derive(Default)]
struct Validator {
    path: Vec<String>,
    unresolved: Vec<UnresolvedSymbol>,
}

/// Expression visitor adapter for validation.
struct ExprVisitor<'a>(&'a mut Validator);

impl ast::Visitor for ExprVisitor<'_> {
    fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        self.0.visit_comp_ref(cr);
        self.visit_each(&cr.parts, |this, part| {
            if let Some(subs) = &part.subs {
                this.visit_each(subs, Self::visit_subscript)
            } else {
                ControlFlow::Continue(())
            }
        })
    }

    fn visit_expr_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        // Function calls should have def_id set (builtins are registered)
        if comp.def_id.is_none() && !comp.parts.is_empty() {
            let source_location = comp.parts[0].ident.location.clone();
            self.0.add(
                comp.to_string(),
                UnresolvedKind::FunctionCall,
                source_location,
            );
        }
        // Visit subscript expressions inside the component path, but avoid
        // re-reporting the function as an unresolved component reference.
        self.visit_each(&comp.parts, |this, part| {
            if let Some(subs) = &part.subs {
                this.visit_each(subs, Self::visit_subscript)
            } else {
                ControlFlow::Continue(())
            }
        })?;
        self.visit_each(args, Self::visit_expression)
    }
}

impl Validator {
    fn location(&self) -> String {
        if self.path.is_empty() {
            "<root>".into()
        } else {
            self.path.join(".")
        }
    }

    fn add(&mut self, name: String, kind: UnresolvedKind, source_location: Location) {
        assert!(
            has_valid_location(&source_location),
            "invalid AST location for unresolved {:?} '{}': file='{}' start={} end={} start_line={} start_col={} end_line={} end_col={}",
            kind,
            name,
            source_location.file_name,
            source_location.start,
            source_location.end,
            source_location.start_line,
            source_location.start_column,
            source_location.end_line,
            source_location.end_column
        );
        self.unresolved.push(UnresolvedSymbol {
            name,
            kind,
            scope_path: self.location(),
            source_location,
        });
    }

    fn with_scope<F: FnOnce(&mut Self)>(&mut self, name: &str, f: F) {
        self.path.push(name.to_string());
        f(self);
        self.path.pop();
    }

    fn visit_stored_def(&mut self, def: &StoredDefinition) {
        for (name, class) in &def.classes {
            self.with_scope(name, |v| v.visit_class(class));
        }
    }

    fn visit_class(&mut self, class: &ClassDef) {
        for ext in &class.extends {
            self.visit_extend(ext);
        }
        for (name, comp) in &class.components {
            self.with_scope(name, |v| v.visit_component(comp));
        }
        class
            .equations
            .iter()
            .for_each(|eq| self.visit_equation(eq));
        class
            .initial_equations
            .iter()
            .for_each(|eq| self.visit_equation(eq));
        class
            .algorithms
            .iter()
            .flatten()
            .for_each(|s| self.visit_statement(s));
        class
            .initial_algorithms
            .iter()
            .flatten()
            .for_each(|s| self.visit_statement(s));
        for (name, nested) in &class.classes {
            self.with_scope(name, |v| v.visit_class(nested));
        }
    }

    fn visit_extend(&mut self, ext: &Extend) {
        // Extends should have base_def_id set (builtins are now registered with DefIds)
        if ext.base_def_id.is_none() {
            self.add(
                ext.base_name.to_string(),
                UnresolvedKind::ExtendsBase,
                ext.location.clone(),
            );
        }
    }

    fn visit_component(&mut self, comp: &Component) {
        // Component type should have def_id set (builtins are now registered with DefIds)
        if comp.type_def_id.is_none() && comp.type_name.def_id.is_none() {
            let source_location = comp
                .type_name
                .name
                .first()
                .map(|token| token.location.clone())
                .expect("component type name must include at least one token");
            self.add(
                comp.type_name.to_string(),
                UnresolvedKind::TypeReference,
                source_location,
            );
        }
        if !matches!(comp.start, Expression::Empty) {
            self.visit_expr(&comp.start);
        }
        if let Some(cond) = &comp.condition {
            self.visit_expr(cond);
        }
    }

    fn visit_equation(&mut self, eq: &Equation) {
        match eq {
            Equation::Empty => {}
            Equation::Simple { lhs, rhs } => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            Equation::Connect { lhs, rhs, .. } => {
                self.visit_comp_ref(lhs);
                self.visit_comp_ref(rhs);
            }
            Equation::For { indices, equations } => {
                indices.iter().for_each(|i| self.visit_expr(&i.range));
                equations.iter().for_each(|e| self.visit_equation(e));
            }
            Equation::If {
                cond_blocks,
                else_block,
            } => {
                for b in cond_blocks {
                    self.visit_expr(&b.cond);
                    b.eqs.iter().for_each(|e| self.visit_equation(e));
                }
                if let Some(eqs) = else_block {
                    eqs.iter().for_each(|e| self.visit_equation(e));
                }
            }
            Equation::When(blocks) => {
                for b in blocks {
                    self.visit_expr(&b.cond);
                    b.eqs.iter().for_each(|e| self.visit_equation(e));
                }
            }
            Equation::FunctionCall { comp, args } => {
                self.visit_comp_ref(comp);
                args.iter().for_each(|a| self.visit_expr(a));
            }
            Equation::Assert {
                condition, message, ..
            } => {
                self.visit_expr(condition);
                self.visit_expr(message);
            }
        }
    }

    fn visit_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Empty | Statement::Return { .. } | Statement::Break { .. } => {}
            Statement::Assignment { comp, value } => {
                self.visit_comp_ref(comp);
                self.visit_expr(value);
            }
            Statement::FunctionCall { comp, args, .. } => {
                self.visit_comp_ref(comp);
                args.iter().for_each(|a| self.visit_expr(a));
            }
            Statement::For { indices, equations } => {
                indices.iter().for_each(|i| self.visit_expr(&i.range));
                equations.iter().for_each(|s| self.visit_statement(s));
            }
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                for b in cond_blocks {
                    self.visit_expr(&b.cond);
                    b.stmts.iter().for_each(|s| self.visit_statement(s));
                }
                if let Some(stmts) = else_block {
                    stmts.iter().for_each(|s| self.visit_statement(s));
                }
            }
            Statement::While(b) => {
                self.visit_expr(&b.cond);
                b.stmts.iter().for_each(|s| self.visit_statement(s));
            }
            Statement::When(blocks) => {
                for b in blocks {
                    self.visit_expr(&b.cond);
                    b.stmts.iter().for_each(|s| self.visit_statement(s));
                }
            }
            Statement::Reinit { variable, value } => {
                self.visit_comp_ref(variable);
                self.visit_expr(value);
            }
            Statement::Assert {
                condition, message, ..
            } => {
                self.visit_expr(condition);
                self.visit_expr(message);
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expression) {
        let mut visitor = ExprVisitor(self);
        let _ = ast::Visitor::visit_expression(&mut visitor, expr);
    }

    fn visit_comp_ref(&mut self, cr: &ComponentReference) {
        // Component references should have def_id set (builtins are registered)
        if cr.def_id.is_none() && !cr.parts.is_empty() {
            let source_location = cr.parts[0].ident.location.clone();
            self.add(
                cr.parts[0].ident.text.to_string(),
                UnresolvedKind::ComponentReference,
                source_location,
            );
        }
    }
}

fn has_valid_location(location: &Location) -> bool {
    !location.file_name.is_empty()
        && location.end > location.start
        && location.start_line > 0
        && location.start_column > 0
        && location.end_line > 0
        && location.end_column > 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Resolver, resolve_parsed};

    /// Run resolution without validation (for testing the validator itself).
    fn resolve_for_validation(source: &str) -> ValidationResult {
        let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
        let mut tree = rumoca_ir_ast::ClassTree::from_parsed(def);
        let mut resolver = Resolver::new();
        resolver.resolve(&mut tree);
        validate_resolution(&tree)
    }

    #[test]
    fn test_fully_resolved() {
        let def = rumoca_phase_parse::parse_to_ast(
            "model T Real x; equation der(x) = -x; end T;",
            "t.mo",
        )
        .unwrap();
        let tree = resolve_parsed(def).unwrap();
        assert!(validate_resolution(&tree).is_fully_resolved());
    }

    #[test]
    fn test_unresolved_type() {
        let r = resolve_for_validation("model T UnknownType x; equation x = 0; end T;");
        assert!(r.unresolved.iter().any(|s| s.name == "UnknownType"));
    }

    #[test]
    fn test_unresolved_extends() {
        let r = resolve_for_validation("model T extends UnknownBase; Real x; end T;");
        assert!(r.unresolved.iter().any(|s| s.name == "UnknownBase"));
    }

    #[test]
    fn test_unresolved_function() {
        let r = resolve_for_validation("model T Real x; equation x = unknownFunc(1.0); end T;");
        assert!(r.unresolved.iter().any(|s| s.name == "unknownFunc"));
    }

    #[test]
    fn test_builtin_types_resolved() {
        // Builtin types should now be resolved with DefIds
        let def = rumoca_phase_parse::parse_to_ast(
            "model T Real x; Integer n; Boolean b; String s; end T;",
            "t.mo",
        )
        .unwrap();
        let tree = resolve_parsed(def).unwrap();
        let result = validate_resolution(&tree);
        assert!(
            result.is_fully_resolved(),
            "Builtins should be resolved: {:?}",
            result.unresolved
        );
    }

    #[test]
    fn test_builtin_functions_resolved() {
        // Builtin functions should now be resolved with DefIds
        let def = rumoca_phase_parse::parse_to_ast(
            "model T Real x; equation der(x) = sin(x) + cos(x); end T;",
            "t.mo",
        )
        .unwrap();
        let tree = resolve_parsed(def).unwrap();
        let result = validate_resolution(&tree);
        assert!(
            result.is_fully_resolved(),
            "Builtin functions should be resolved: {:?}",
            result.unresolved
        );
    }
}
