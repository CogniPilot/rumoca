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
type Expression = ast::Expression;
type Extend = ast::Extend;

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
    let _ = ast::Visitor::visit_stored_definition(&mut v, &tree.definitions);
    ValidationResult {
        unresolved: v.unresolved,
    }
}

#[derive(Default)]
struct Validator {
    path: Vec<String>,
    unresolved: Vec<UnresolvedSymbol>,
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

    fn add_unresolved_name(&mut self, name: &ast::Name, kind: UnresolvedKind, _context: &str) {
        if name.def_id.is_some() || name.name.is_empty() {
            return;
        }
        let Some(source_location) = name.name.first().map(|token| token.location.clone()) else {
            return;
        };
        self.add(name.to_string(), kind, source_location);
    }

    fn add_unresolved_component_reference(&mut self, cr: &ComponentReference) {
        if cr.def_id.is_some() || cr.parts.is_empty() {
            return;
        }
        let source_location = cr.parts[0].ident.location.clone();
        self.add(
            cr.parts[0].ident.text.to_string(),
            UnresolvedKind::ComponentReference,
            source_location,
        );
    }

    fn add_unresolved_function_call(&mut self, cr: &ComponentReference) {
        if cr.def_id.is_none() && !cr.parts.is_empty() {
            let source_location = cr.parts[0].ident.location.clone();
            self.add(
                cr.parts[0].ident.text.to_string(),
                UnresolvedKind::FunctionCall,
                source_location,
            );
        }
    }
}

impl ast::Visitor for Validator {
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        self.path.push(class.name.text.to_string());

        if let Some(constrainedby) = &class.constrainedby {
            self.visit_type_name(constrainedby, ast::TypeNameContext::ClassConstrainedBy)?;
        }

        for ext in &class.extends {
            self.visit_extend(ext)?;
        }

        self.visit_each(&class.array_subscripts, Self::visit_subscript)?;

        for component in class.components.values() {
            self.visit_component(component)?;
        }
        self.visit_each(&class.equations, Self::visit_equation)?;
        self.visit_each(&class.initial_equations, Self::visit_equation)?;
        for algorithm_section in &class.algorithms {
            self.visit_each(algorithm_section, Self::visit_statement)?;
        }
        for algorithm_section in &class.initial_algorithms {
            self.visit_each(algorithm_section, Self::visit_statement)?;
        }

        if let Some(external) = &class.external {
            if let Some(output) = &external.output {
                self.visit_component_reference_ctx(
                    output,
                    ast::ComponentReferenceContext::Expression,
                )?;
            }
            self.visit_each(&external.args, Self::visit_expression)?;
        }

        for nested in class.classes.values() {
            self.visit_class_def(nested)?;
        }

        self.path.pop();
        ControlFlow::Continue(())
    }

    fn visit_extend(&mut self, ext: &Extend) -> ControlFlow<()> {
        if ext.base_def_id.is_none() {
            self.add(
                ext.base_name.to_string(),
                UnresolvedKind::ExtendsBase,
                ext.location.clone(),
            );
        }
        for modification in &ext.modifications {
            self.visit_expression(&modification.expr)?;
        }
        ControlFlow::Continue(())
    }

    fn visit_component(&mut self, comp: &Component) -> ControlFlow<()> {
        self.path.push(comp.name.clone());

        self.visit_type_name(&comp.type_name, ast::TypeNameContext::ComponentType)?;
        if let Some(constrainedby) = &comp.constrainedby {
            self.visit_type_name(constrainedby, ast::TypeNameContext::ComponentConstrainedBy)?;
        }

        if !matches!(comp.start, Expression::Empty) {
            self.visit_expression(&comp.start)?;
        }
        if let Some(binding) = &comp.binding {
            self.visit_expression(binding)?;
        }
        for modification in comp.modifications.values() {
            self.visit_expression(modification)?;
        }
        self.visit_each(&comp.shape_expr, Self::visit_subscript)?;
        if let Some(condition) = &comp.condition {
            self.visit_expression(condition)?;
        }

        self.path.pop();
        ControlFlow::Continue(())
    }

    fn visit_type_name(&mut self, name: &ast::Name, ctx: ast::TypeNameContext) -> ControlFlow<()> {
        match ctx {
            ast::TypeNameContext::ExtendsBase => {}
            ast::TypeNameContext::ComponentType => {
                self.add_unresolved_name(
                    name,
                    UnresolvedKind::TypeReference,
                    "component type name",
                );
            }
            ast::TypeNameContext::ClassConstrainedBy => {
                self.add_unresolved_name(
                    name,
                    UnresolvedKind::TypeReference,
                    "class constrainedby",
                );
            }
            ast::TypeNameContext::ComponentConstrainedBy => {
                self.add_unresolved_name(
                    name,
                    UnresolvedKind::TypeReference,
                    "component constrainedby",
                );
            }
        }
        ControlFlow::Continue(())
    }

    fn visit_component_reference_ctx(
        &mut self,
        cr: &ComponentReference,
        ctx: ast::ComponentReferenceContext,
    ) -> ControlFlow<()> {
        match ctx {
            ast::ComponentReferenceContext::ExpressionFunctionCallTarget
            | ast::ComponentReferenceContext::EquationFunctionCallTarget
            | ast::ComponentReferenceContext::StatementFunctionCallTarget
            | ast::ComponentReferenceContext::ClassModificationTarget
            | ast::ComponentReferenceContext::ModificationTarget => {}
            _ => self.add_unresolved_component_reference(cr),
        }
        ast::Visitor::visit_component_reference(self, cr)
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        ctx: ast::FunctionCallContext,
    ) -> ControlFlow<()> {
        self.add_unresolved_function_call(comp);
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }

    fn visit_expression_ctx(
        &mut self,
        expr: &Expression,
        ctx: ast::ExpressionContext,
    ) -> ControlFlow<()> {
        if ctx == ast::ExpressionContext::ComponentAnnotation {
            return ControlFlow::Continue(());
        }
        self.visit_expression(expr)
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

    #[test]
    fn test_clock_builtin_functions_resolved() {
        let def = rumoca_phase_parse::parse_to_ast(
            r#"
model T
  Clock c1 = Clock(0.1);
  Clock c2 = subSample(c1, 2);
  Clock c3 = superSample(c2, 2);
  Clock c4 = shiftSample(c3, 1, 2);
  Clock c5 = backSample(c4, 1, 2);
  Real x;
  Real y = hold(x);
  Real z = previous(x);
  Boolean first = firstTick(c1);
  Real dt = interval(c1);
end T;
"#,
            "t.mo",
        )
        .unwrap();
        let tree = resolve_parsed(def).unwrap();
        let result = validate_resolution(&tree);
        assert!(
            result.is_fully_resolved(),
            "clock builtins should be resolved: {:?}",
            result.unresolved
        );
    }

    #[test]
    fn test_array_builtin_function_resolved() {
        let def = rumoca_phase_parse::parse_to_ast(
            r#"
model T
  Real x[2] = array(1.0, 2.0);
end T;
"#,
            "t.mo",
        )
        .unwrap();
        let tree = resolve_parsed(def).unwrap();
        let result = validate_resolution(&tree);
        assert!(
            result.is_fully_resolved(),
            "array() should be resolved as a builtin function: {:?}",
            result.unresolved
        );
    }

    #[test]
    fn test_unresolved_external_call_argument_in_nested_function() {
        let r = resolve_for_validation(
            r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbdose = true;
    output EOTest env;
    external "C" env = init(verbose);
  end constructor;
end EOTest;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "verbose"),
            "expected unresolved external-call argument in nested function, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_external_call_output_in_nested_function() {
        let r = resolve_for_validation(
            r#"
class EOTest
  extends ExternalObject;
  function constructor
    input Boolean verbose = true;
    output EOTest env;
    external "C" wrong = init(verbose);
  end constructor;
end EOTest;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "wrong"),
            "expected unresolved external-call output reference, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_component_in_assert_level_expression() {
        let r = resolve_for_validation(
            r#"
model T
equation
  assert(true, "ok", lvl);
end T;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "lvl"),
            "expected unresolved assert-level reference, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_component_in_statement_function_outputs() {
        let r = resolve_for_validation(
            r#"
model T
  Real x;
algorithm
  (x, y) := sin(1.0);
end T;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "y"),
            "expected unresolved output reference in statement function call, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_extends_modifier_expression_reference() {
        let r = resolve_for_validation(
            r#"
model Base
  parameter Real k = 0;
end Base;

model Derived
  extends Base(k = missing);
end Derived;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "missing"),
            "expected unresolved extends-modifier reference, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_class_constrainedby_type_reference() {
        let r = resolve_for_validation(
            r#"
package RealMedium
end RealMedium;

model UsesMedium
  replaceable package Medium = RealMedium constrainedby MissingMedium;
end UsesMedium;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::TypeReference && s.name == "MissingMedium"),
            "expected unresolved constrainedby type on class, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_component_constrainedby_type_reference() {
        let r = resolve_for_validation(
            r#"
model M
  replaceable Real x constrainedby Missing;
equation
  x = 0;
end M;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::TypeReference && s.name == "Missing"),
            "expected unresolved constrainedby type on component, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_subscript_index_in_assignment_target() {
        let r = resolve_for_validation(
            r#"
model M
  Real a[2];
algorithm
  a[i] := 1;
end M;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "i"),
            "expected unresolved subscript index in assignment target, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_equation_function_call_is_classified_as_function_call() {
        let r = resolve_for_validation(
            r#"
model M
equation
  unknown(1.0);
end M;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::FunctionCall && s.name == "unknown"),
            "expected unresolved equation function call classification, got: {:?}",
            r.unresolved
        );
        assert!(
            !r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "unknown"),
            "function call should not also be reported as component reference, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_unresolved_statement_function_call_is_classified_as_function_call() {
        let r = resolve_for_validation(
            r#"
model M
algorithm
  unknown(1.0);
end M;
"#,
        );
        assert!(
            r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::FunctionCall && s.name == "unknown"),
            "expected unresolved statement function call classification, got: {:?}",
            r.unresolved
        );
        assert!(
            !r.unresolved
                .iter()
                .any(|s| s.kind == UnresolvedKind::ComponentReference && s.name == "unknown"),
            "function call should not also be reported as component reference, got: {:?}",
            r.unresolved
        );
    }

    #[test]
    fn test_component_annotation_reference_is_excluded_from_resolution_validation() {
        let r = resolve_for_validation(
            r#"
model M
  Real x annotation(Dialog(group = missingAnnotationRef));
equation
  x = 1;
end M;
"#,
        );
        assert!(
            !r.unresolved
                .iter()
                .any(|s| s.name == "missingAnnotationRef"),
            "annotation references should be excluded from resolution validation, got: {:?}",
            r.unresolved
        );
    }
}
