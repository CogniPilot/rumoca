//! Equation and reference type/shape compatibility checks (MLS §6.7, §10.5).

use super::*;

impl TypeChecker {
    /// Check assignment compatibility for a simple equation.
    ///
    /// This currently targets scalar/component identity checks needed for
    /// user-defined type mismatch diagnostics in equations.
    pub(crate) fn check_equation_type_compatibility(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        type_table: &TypeTable,
    ) {
        let lhs_ty = self.infer_expression_type(lhs, type_table);
        let rhs_ty = self.infer_expression_type(rhs, type_table);

        let (Some(lhs_ty), Some(rhs_ty)) = (lhs_ty, rhs_ty) else {
            return;
        };
        if lhs_ty.is_unknown() || rhs_ty.is_unknown() {
            return;
        }
        let lhs_root = self.resolve_type_root(type_table, lhs_ty);
        let rhs_root = self.resolve_type_root(type_table, rhs_ty);
        if lhs_root == rhs_root {
            self.check_equation_shape_compatibility(lhs, rhs, type_table);
            return;
        }
        if Self::is_unresolved_alias_root(type_table, lhs_root)
            || Self::is_unresolved_alias_root(type_table, rhs_root)
        {
            return;
        }
        let builtin_mismatch = Self::builtin_roots_incompatible(type_table, lhs_root, rhs_root);
        if !builtin_mismatch
            && (!Self::is_user_defined_compatibility_type(type_table, lhs_root)
                || !Self::is_user_defined_compatibility_type(type_table, rhs_root))
        {
            // Compatible builtin roots (e.g. Real = Integer expression) still
            // need matching array shapes.
            self.check_equation_shape_compatibility(lhs, rhs, type_table);
            return;
        }

        let Some(loc) = lhs.get_location().or_else(|| rhs.get_location()) else {
            return;
        };
        let span =
            self.source_map
                .location_to_span(&loc.file_name, loc.start as usize, loc.end as usize);
        let expected = Self::format_type_name(type_table, lhs_ty);
        let found = Self::format_type_name(type_table, rhs_ty);
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            format!("type mismatch: expected `{expected}`, found `{found}`"),
            "equation assignment here",
            span,
        ));
    }

    pub(in crate::typechecker) fn validate_component_reference(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) {
        match self.resolve_component_reference_type(comp, type_table) {
            Ok(_) => self.validate_component_subscripts(comp),
            Err(missing) => self.emit_unknown_component_member(missing, type_table),
        }
    }

    /// MLS §10.5.1: the number of subscripts must not exceed the declared
    /// dimensions, and literal index subscripts must stay within bounds.
    fn validate_component_subscripts(&mut self, comp: &rumoca_ir_ast::ComponentReference) {
        let name = Self::component_ref_name(comp);
        let Some(Some(declared)) = self.current_component_shapes.get(&name).cloned() else {
            return;
        };
        let subscripts: Vec<&rumoca_ir_ast::Subscript> = comp
            .parts
            .iter()
            .flat_map(|part| part.subs.iter().flatten())
            .collect();
        if subscripts.is_empty() {
            return;
        }
        let location = &comp.parts[0].ident.location;
        let span = self.source_map.location_to_span(
            &location.file_name,
            location.start as usize,
            location.end as usize,
        );
        if subscripts.len() > declared.len() {
            self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                "ET009",
                format!(
                    "`{name}` has {} dimension(s) but is subscripted with {} subscript(s) (MLS §10.5.1)",
                    declared.len(),
                    subscripts.len()
                ),
                "subscripted reference here",
                span,
            ));
            return;
        }
        for (subscript, dim) in subscripts.iter().zip(declared.iter()) {
            let rumoca_ir_ast::Subscript::Expression(Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token,
                ..
            }) = subscript
            else {
                continue;
            };
            let Ok(index) = token.text.parse::<usize>() else {
                continue;
            };
            if index < 1 || index > *dim {
                self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                    "ET009",
                    format!(
                        "subscript {index} for `{name}` is out of bounds for dimension of size {dim} (MLS §10.5.1)"
                    ),
                    "subscripted reference here",
                    span,
                ));
            }
        }
    }

    /// True when two builtin roots cannot appear on the two sides of an
    /// equation. MLS §6.7 permits only the Integer -> Real implicit
    /// conversion among the builtin types; Boolean and String never mix
    /// with the numeric types or each other.
    pub(crate) fn builtin_roots_incompatible(
        type_table: &TypeTable,
        lhs_root: TypeId,
        rhs_root: TypeId,
    ) -> bool {
        use rumoca_ir_ast::BuiltinType;
        let (Some(Type::Builtin(lhs)), Some(Type::Builtin(rhs))) =
            (type_table.get(lhs_root), type_table.get(rhs_root))
        else {
            return false;
        };
        let numeric = |ty: &BuiltinType| matches!(ty, BuiltinType::Real | BuiltinType::Integer);
        if lhs == rhs || (numeric(lhs) && numeric(rhs)) {
            return false;
        }
        // Clock and other special builtins stay out of this check.
        let checked = |ty: &BuiltinType| {
            matches!(
                ty,
                BuiltinType::Real
                    | BuiltinType::Integer
                    | BuiltinType::Boolean
                    | BuiltinType::String
            )
        };
        checked(lhs) && checked(rhs)
    }
}
