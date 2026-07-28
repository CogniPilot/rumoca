//! Equation and reference type/shape compatibility checks (MLS §6.7, §10.5).

use super::*;
use crate::typechecker::late_methods::ComponentReferenceTypeError;

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
        if Self::is_unresolved_alias_root(type_table, lhs_root)
            || Self::is_unresolved_alias_root(type_table, rhs_root)
        {
            return;
        }
        if self.equation_types_compatible(type_table, lhs_ty, rhs_ty) {
            self.check_equation_shape_compatibility(lhs, rhs, type_table);
            return;
        }

        let Some(location) = lhs.get_location().or_else(|| rhs.get_location()) else {
            return;
        };
        self.emit_assignment_type_mismatch(
            lhs_ty,
            rhs_ty,
            location,
            "equation type compatibility",
            "equation assignment here",
            type_table,
        );
    }

    pub(crate) fn check_assignment_type_compatibility(
        &mut self,
        target: &rumoca_ir_ast::ComponentReference,
        value: &Expression,
        type_table: &TypeTable,
    ) {
        let Some(expected) = self.infer_component_ref_type(target, type_table) else {
            return;
        };
        let Some(found) = self.infer_expression_type(value, type_table) else {
            return;
        };
        self.check_expected_expression_type(
            expected,
            found,
            target.get_location().or_else(|| value.get_location()),
            "algorithm assignment type compatibility",
            "algorithm assignment here",
            type_table,
        );
    }

    pub(crate) fn check_expected_expression_type(
        &mut self,
        expected: TypeId,
        found: TypeId,
        location: Option<&rumoca_core::Location>,
        context: &str,
        label: &str,
        type_table: &TypeTable,
    ) {
        if expected.is_unknown() || found.is_unknown() {
            return;
        }
        let expected_root = self.resolve_type_root(type_table, expected);
        let found_root = self.resolve_type_root(type_table, found);
        if Self::is_unresolved_alias_root(type_table, expected_root)
            || Self::is_unresolved_alias_root(type_table, found_root)
            || self.assignment_types_compatible(type_table, expected, found)
        {
            return;
        }
        let Some(location) = location else {
            return;
        };
        self.emit_assignment_type_mismatch(expected, found, location, context, label, type_table);
    }

    fn emit_assignment_type_mismatch(
        &mut self,
        expected: TypeId,
        found: TypeId,
        location: &rumoca_core::Location,
        context: &str,
        label: &str,
        type_table: &TypeTable,
    ) {
        let Some(span) = self.diagnostic_location_span(location, context) else {
            return;
        };
        let expected = Self::format_type_name(type_table, expected);
        let found = Self::format_type_name(type_table, found);
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET002",
            format!("type mismatch: expected `{expected}`, found `{found}`"),
            label,
            span,
        ));
    }

    fn assignment_types_compatible(
        &self,
        type_table: &TypeTable,
        lhs: TypeId,
        rhs: TypeId,
    ) -> bool {
        if lhs == rhs || self.nominal_class_types_compatible(type_table, lhs, rhs) {
            return true;
        }
        let lhs_root = self.resolve_type_root(type_table, lhs);
        let rhs_root = self.resolve_type_root(type_table, rhs);
        if lhs_root == rhs_root
            || self.nominal_class_types_compatible(type_table, lhs_root, rhs_root)
        {
            return true;
        }
        matches!(
            (type_table.get(lhs_root), type_table.get(rhs_root)),
            (Some(Type::Builtin(_)), Some(Type::Builtin(_)))
        ) && !Self::builtin_roots_incompatible(type_table, lhs_root, rhs_root)
    }

    pub(in crate::typechecker) fn equation_types_compatible(
        &self,
        type_table: &TypeTable,
        lhs: TypeId,
        rhs: TypeId,
    ) -> bool {
        if self.assignment_types_compatible(type_table, lhs, rhs) {
            return true;
        }
        let lhs_root = self.resolve_type_root(type_table, lhs);
        let rhs_root = self.resolve_type_root(type_table, rhs);
        matches!(
            (type_table.get(lhs_root), type_table.get(rhs_root)),
            (
                Some(Type::Builtin(
                    rumoca_ir_ast::BuiltinType::Real | rumoca_ir_ast::BuiltinType::Integer
                )),
                Some(Type::Builtin(
                    rumoca_ir_ast::BuiltinType::Real | rumoca_ir_ast::BuiltinType::Integer
                ))
            )
        )
    }

    fn nominal_class_types_compatible(
        &self,
        type_table: &TypeTable,
        expected: TypeId,
        found: TypeId,
    ) -> bool {
        if let (Some(Type::Class(expected)), Some(Type::Class(found))) =
            (type_table.get(expected), type_table.get(found))
        {
            self.class_extends(found.def_id, expected.def_id)
        } else {
            false
        }
    }

    fn class_extends(&self, found: rumoca_core::DefId, expected: rumoca_core::DefId) -> bool {
        let mut pending = vec![found];
        let mut visited = std::collections::HashSet::new();
        while let Some(current) = pending.pop() {
            if current == expected {
                return true;
            }
            if !visited.insert(current) {
                continue;
            }
            if let Some(bases) = self.class_base_def_ids.get(&current) {
                pending.extend(bases.iter().copied());
            }
        }
        false
    }

    pub(in crate::typechecker) fn validate_component_reference(
        &mut self,
        comp: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) {
        if self.is_integer_iterator_reference(comp) {
            return;
        }
        match self.resolve_component_reference_type(comp, type_table) {
            Ok(_) => self.validate_component_subscripts(comp),
            Err(ComponentReferenceTypeError::MissingMember(missing)) => {
                self.emit_unknown_component_member(missing, type_table);
            }
            Err(ComponentReferenceTypeError::MissingSourceContext(error)) => {
                self.emit_typecheck_error(error);
            }
            Err(ComponentReferenceTypeError::AmbiguousIdentity { reference, span }) => {
                self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                    "ET001",
                    format!(
                        "ambiguous component identity for `{reference}`; resolved instance candidates disagree"
                    ),
                    "ambiguous component reference",
                    span,
                ));
            }
        }
    }

    /// MLS §10.5.1: the number of subscripts must not exceed the declared
    /// dimensions, and literal index subscripts must stay within bounds.
    fn validate_component_subscripts(&mut self, comp: &rumoca_ir_ast::ComponentReference) {
        // Absence from the concrete instance-shape index is unknown, not a
        // scalar declaration. A known scalar is represented explicitly as
        // `Some(Vec::new())` by the shape index.
        let mut effective_shape: Option<Vec<usize>> = None;
        for (part_index, part) in comp.parts.iter().enumerate() {
            let prefix = ComponentPath::from_parts(
                comp.parts
                    .iter()
                    .take(part_index + 1)
                    .map(|part| part.ident.text.to_string()),
            );
            match self.lookup_component_reference_prefix_shape(comp, part_index + 1) {
                // MLS §10.4.1: the shape of `a.b` is `size(a)` ++ `size(b)`.
                // The instance shape index stores every path with its own
                // declared extents, so each part contributes all of them. This
                // is the same composition rule the equation-shape walk uses; a
                // member whose extents happen to repeat the owner's must not be
                // mistaken for an owner-inclusive row.
                SemanticLookup::Found(Some(local_shape)) => {
                    effective_shape
                        .get_or_insert_with(Vec::new)
                        .extend_from_slice(&local_shape);
                }
                SemanticLookup::Found(None) | SemanticLookup::Ambiguous => {
                    // The declaration has dimensions, but one or more extents
                    // are not evaluable in this instance. Unknown shape is not
                    // scalar: defer arity and bounds validation until a phase
                    // owns concrete extents.
                    effective_shape = None;
                }
                SemanticLookup::Missing => {}
            }
            let Some(subscripts) = part.subs.as_ref() else {
                continue;
            };
            let Some(known_shape) = effective_shape.as_mut() else {
                continue;
            };
            self.validate_subscript_group(&prefix, known_shape, subscripts, &part.ident.location);
        }
    }

    fn validate_subscript_group(
        &mut self,
        path: &ComponentPath,
        effective_shape: &mut Vec<usize>,
        subscripts: &[rumoca_ir_ast::Subscript],
        location: &rumoca_core::Location,
    ) {
        let Some(span) = self.diagnostic_location_span(location, "component subscript validation")
        else {
            return;
        };
        if subscripts.len() > effective_shape.len() {
            self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
                "ET009",
                format!(
                    "`{path}` has {} dimension(s) but is subscripted with {} subscript(s) (MLS §10.5.1)",
                    effective_shape.len(),
                    subscripts.len()
                ),
                "subscripted reference here",
                span,
            ));
            return;
        }
        for (subscript, dim) in subscripts.iter().zip(effective_shape.iter()) {
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
                        "subscript {index} for `{path}` is out of bounds for dimension of size {dim} (MLS §10.5.1)"
                    ),
                    "subscripted reference here",
                    span,
                ));
            }
        }
        effective_shape.drain(..subscripts.len());
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
        !matches!(
            (lhs, rhs),
            (BuiltinType::Real, BuiltinType::Real | BuiltinType::Integer)
                | (BuiltinType::Integer, BuiltinType::Integer)
                | (BuiltinType::Boolean, BuiltinType::Boolean)
                | (BuiltinType::String, BuiltinType::String)
                | (BuiltinType::Clock, BuiltinType::Clock)
        )
    }
}
