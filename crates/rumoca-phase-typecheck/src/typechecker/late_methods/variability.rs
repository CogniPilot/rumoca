//! Variability analysis for the late typecheck pass (MLS §3.8 / §18.3):
//! structural-parameter marking, declared variability constraints, and the
//! variability level inferred for binding expressions.

use super::*;

impl TypeChecker {
    /// Mark structural parameters (MLS §18.3).
    ///
    /// Structural parameters are those that affect array sizes, for-loop ranges,
    /// or if-equation conditions. They must be evaluable at translation time.
    pub(crate) fn mark_structural_parameters(&mut self, class: &mut ClassDef) {
        // Collect all variable references from dimension expressions
        let mut structural_refs = std::collections::HashSet::new();
        for (_name, comp) in class.components.iter() {
            for sub in &comp.shape_expr {
                structural_refs.extend(rumoca_eval_ast::eval::collect_subscript_refs(sub));
            }
        }

        // Collect from for-loop ranges and if-equation conditions
        super::api::collect_structural_refs_from_equations(&class.equations, &mut structural_refs);
        super::api::collect_structural_refs_from_equations(
            &class.initial_equations,
            &mut structural_refs,
        );

        // Mark referenced parameters as structural
        for (name, comp) in class.components.iter_mut() {
            let is_param = matches!(comp.variability, rumoca_core::Variability::Parameter(_));
            if structural_refs.contains(name) && is_param {
                comp.is_structural = true;
            }
        }
    }

    /// Validate variability constraints (MLS §4.5).
    ///
    /// Ensures that bindings and start values respect variability ordering:
    /// constant < parameter < discrete < continuous
    pub(crate) fn validate_variability_constraints(&mut self, class: &ClassDef) {
        for (name, comp) in &class.components {
            let comp_level =
                rumoca_eval_ast::eval::VariabilityLevel::from_variability(&comp.variability);

            // Check binding expression
            if let Some(binding) = &comp.binding {
                self.check_binding_variability(name, binding, comp_level, class, &comp.location);
            }

            // Check start expression (if it's a modification, not binding)
            if comp.start_is_modification && !matches!(comp.start, Expression::Empty { .. }) {
                self.check_binding_variability(
                    name,
                    &comp.start,
                    comp_level,
                    class,
                    &comp.location,
                );
            }
        }
    }

    /// Check that a binding expression respects variability constraints.
    pub(crate) fn check_binding_variability(
        &mut self,
        comp_name: &str,
        expr: &Expression,
        comp_level: rumoca_eval_ast::eval::VariabilityLevel,
        class: &ClassDef,
        location: &rumoca_core::Location,
    ) {
        let expr_level = self.max_expression_variability(expr, class);

        if expr_level > comp_level {
            let Some(span) = self.diagnostic_location_span(location, "binding variability") else {
                return;
            };

            self.diagnostics.emit(
                CommonDiagnostic::warning(
                    "ET004",
                    format!(
                    "variability violation: {} has {} variability but binding references {} variables (MLS §4.5)",
                    comp_name,
                    comp_level.name(),
                    expr_level.name()
                ),
                    rumoca_core::PrimaryLabel::new(span).with_message("binding here"),
                ),
            );
        }
    }

    fn max_expression_variability(
        &self,
        expr: &Expression,
        class: &ClassDef,
    ) -> rumoca_eval_ast::eval::VariabilityLevel {
        use rumoca_eval_ast::eval::VariabilityLevel;
        let mut pending = vec![expr];
        let mut maximum = VariabilityLevel::Constant;
        while let Some(current) = pending.pop() {
            maximum = maximum.max(self.expression_node_variability(current, class, &mut pending));
        }
        maximum
    }

    /// Variability contributed by one expression node, pushing its operands onto
    /// `pending` for the caller's worklist.
    ///
    /// Split out of [`Self::max_expression_variability`] so the per-variant match
    /// is not nested inside the worklist loop (SPEC_0021 nesting budget).
    fn expression_node_variability<'expr>(
        &self,
        current: &'expr Expression,
        class: &ClassDef,
        pending: &mut Vec<&'expr Expression>,
    ) -> rumoca_eval_ast::eval::VariabilityLevel {
        use rumoca_eval_ast::eval::VariabilityLevel;
        match current {
            Expression::Terminal { .. }
            | Expression::Empty { .. }
            | Expression::ClassModification { .. } => VariabilityLevel::Constant,
            Expression::ComponentReference(reference) => {
                self.component_reference_variability(reference, class)
            }
            Expression::FunctionCall { comp, args, .. } => {
                Self::function_call_variability(comp, args, pending)
            }
            Expression::Unary { rhs, .. }
            | Expression::Parenthesized { inner: rhs, .. }
            | Expression::NamedArgument { value: rhs, .. }
            | Expression::Modification { value: rhs, .. }
            | Expression::ArrayIndex { base: rhs, .. }
            | Expression::FieldAccess { base: rhs, .. } => {
                pending.push(rhs);
                VariabilityLevel::Constant
            }
            Expression::Binary { lhs, rhs, .. } => {
                pending.push(lhs);
                pending.push(rhs);
                VariabilityLevel::Constant
            }
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                pending.extend(
                    branches
                        .iter()
                        .flat_map(|(condition, value)| [condition, value]),
                );
                pending.push(else_branch);
                VariabilityLevel::Constant
            }
            Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
                pending.extend(elements.iter());
                VariabilityLevel::Constant
            }
            Expression::Range {
                start, step, end, ..
            } => {
                pending.push(start);
                pending.extend(step.iter().map(std::ops::Deref::deref));
                pending.push(end);
                VariabilityLevel::Constant
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => {
                pending.push(expr);
                pending.extend(indices.iter().map(|index| &index.range));
                pending.extend(filter.iter().map(std::ops::Deref::deref));
                VariabilityLevel::Constant
            }
        }
    }

    /// MLS §3.8: `size`, `ndims` and `cardinality` are parameter-variability
    /// regardless of their arguments, so their operands are not traversed.
    fn function_call_variability<'expr>(
        comp: &rumoca_ir_ast::ComponentReference,
        args: &'expr [Expression],
        pending: &mut Vec<&'expr Expression>,
    ) -> rumoca_eval_ast::eval::VariabilityLevel {
        use rumoca_eval_ast::eval::VariabilityLevel;
        let name = match comp.parts.last() {
            Some(part) => part.ident.text.as_ref(),
            None => "",
        };
        if matches!(name, "size" | "ndims" | "cardinality") {
            return VariabilityLevel::Parameter;
        }
        pending.extend(args.iter());
        VariabilityLevel::Constant
    }

    fn component_reference_variability(
        &self,
        reference: &rumoca_ir_ast::ComponentReference,
        class: &ClassDef,
    ) -> rumoca_eval_ast::eval::VariabilityLevel {
        use rumoca_eval_ast::eval::VariabilityLevel;

        if reference.parts.len() == 1
            && reference.parts[0].ident.text.as_ref() == "time"
            && reference.parts[0].subs.iter().flatten().next().is_none()
        {
            return VariabilityLevel::Continuous;
        }
        match self.lookup_component_reference_variability(reference) {
            SemanticLookup::Found(variability) => variability,
            // Ambiguity must never make a binding look less variable. The
            // component-reference validation emits ET001 for the same node.
            SemanticLookup::Ambiguous => VariabilityLevel::Continuous,
            SemanticLookup::Missing => Self::declared_reference_variability(reference, class)
                .unwrap_or(VariabilityLevel::Constant),
        }
    }

    fn declared_reference_variability(
        reference: &rumoca_ir_ast::ComponentReference,
        class: &ClassDef,
    ) -> Option<rumoca_eval_ast::eval::VariabilityLevel> {
        reference
            .def_id
            .and_then(|def_id| {
                class
                    .components
                    .values()
                    .find(|component| component.def_id == Some(def_id))
            })
            .map(|component| {
                rumoca_eval_ast::eval::VariabilityLevel::from_variability(&component.variability)
            })
    }
}
