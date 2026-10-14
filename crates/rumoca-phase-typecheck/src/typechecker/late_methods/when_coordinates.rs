//! Parameter-variability target coordinates in when-equations (MLS §8.3.5.3).

use super::*;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::Visitor;
use std::ops::ControlFlow;

impl TypeChecker {
    pub(super) fn check_when_target_variability(&mut self, target: &Expression) {
        let mut visitor = TargetSubscripts { checker: self };
        let _ = visitor.visit_expression(target);
    }

    fn check_when_index_variability(&mut self, expression: &Expression) {
        let mut visitor = ParameterCoordinate {
            checker: self,
            variable: false,
        };
        let _ = visitor.visit_expression(expression);
        if !visitor.variable {
            return;
        }
        let Some(location) = expression.get_location() else {
            return;
        };
        let Some(span) =
            self.diagnostic_location_span(location, "when target subscript variability")
        else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET003",
            "when-equation target subscripts must be parameter expressions (MLS §8.3.5.3)",
            "simulation-time target subscript",
            span,
        ));
    }

    fn coordinate_builtin(
        &self,
        comp: &ast::ComponentReference,
    ) -> Option<rumoca_core::BuiltinFunction> {
        if comp
            .root_def_id()
            .is_some_and(|id| self.type_ids_by_def_id.contains_key(&id))
        {
            return None;
        }
        let name = comp.parts.last()?.ident.text.as_ref();
        rumoca_core::BuiltinFunction::from_name(name)
    }

    fn coordinate_call_is_simulation_time(&self, comp: &ast::ComponentReference) -> bool {
        use rumoca_core::BuiltinFunction;
        if matches!(
            self.coordinate_builtin(comp),
            Some(
                BuiltinFunction::Initial
                    | BuiltinFunction::Terminal
                    | BuiltinFunction::Sample
                    | BuiltinFunction::FirstTick
                    | BuiltinFunction::Interval
                    | BuiltinFunction::Previous
            )
        ) {
            return true;
        }
        self.user_function_definition(comp, &Self::component_ref_name(comp))
            .is_some_and(|function| !function.pure)
    }
}

struct TargetSubscripts<'a> {
    checker: &'a mut TypeChecker,
}

impl Visitor for TargetSubscripts<'_> {
    fn visit_subscript(&mut self, subscript: &ast::Subscript) -> ControlFlow<()> {
        if let ast::Subscript::Expression(expression) = subscript {
            self.checker.check_when_index_variability(expression);
        }
        ControlFlow::Continue(())
    }
}

struct ParameterCoordinate<'a> {
    checker: &'a TypeChecker,
    variable: bool,
}

impl Visitor for ParameterCoordinate<'_> {
    fn visit_component_reference_ctx(
        &mut self,
        reference: &ast::ComponentReference,
        _: ast::ComponentReferenceContext,
    ) -> ControlFlow<()> {
        self.variable |= !self.checker.is_iterator_reference(reference)
            && self.checker.is_simulation_time_reference(reference);
        self.visit_component_reference(reference)
    }

    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ast::ComponentReference,
        args: &[Expression],
        context: ast::FunctionCallContext,
    ) -> ControlFlow<()> {
        if let Some(
            intrinsic @ (rumoca_core::BuiltinFunction::Size | rumoca_core::BuiltinFunction::Ndims),
        ) = self.checker.coordinate_builtin(comp)
        {
            if intrinsic == rumoca_core::BuiltinFunction::Size {
                return self.visit_each(args.get(1..).unwrap_or_default(), Self::visit_expression);
            }
            return ControlFlow::Continue(());
        }
        self.variable |= self.checker.coordinate_call_is_simulation_time(comp);
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, context)
    }
}
