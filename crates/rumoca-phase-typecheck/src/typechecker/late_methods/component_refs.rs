//! Component-reference resolution for the late typecheck pass: the type,
//! shape, and variability of a reference in the current instance scope, member
//! lookup through the owning type, and the unknown-member diagnostic.

use super::*;

impl TypeChecker {
    pub(crate) fn component_ref_name(cr: &rumoca_ir_ast::ComponentReference) -> String {
        cr.parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".")
    }

    pub(crate) fn infer_component_ref_type(
        &self,
        cr: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        match self.resolve_component_reference_type(cr, type_table) {
            Ok(type_id) if !type_id.is_unknown() => {
                Self::filter_non_value_component_type(type_table, type_id)
            }
            _ => {
                // Enum literal: <EnumType>.<Literal> or <Pkg>.<EnumType>.<Literal>.
                let parts: Vec<&str> = cr.parts.iter().map(|p| p.ident.text.as_ref()).collect();
                (1..parts.len()).rev().find_map(|i| {
                    let candidate = parts[..i].join(".");
                    let type_id = type_table.lookup(&candidate)?;
                    matches!(type_table.get(type_id), Some(Type::Enumeration(_))).then_some(type_id)
                })
            }
        }
    }

    pub(super) fn infer_field_access_type(
        &self,
        base: &Expression,
        field: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        let base_type = self.infer_expression_type(base, type_table)?;
        self.lookup_component_member_type(base_type, field, type_table)
            .or_else(|| self.infer_named_function_output_type(base, field, type_table))
            .and_then(|ty| Self::filter_non_value_component_type(type_table, ty))
    }

    pub(super) fn validate_field_access(
        &mut self,
        base: &Expression,
        field: &str,
        type_table: &TypeTable,
    ) {
        let Some(base_type) = self.infer_expression_type(base, type_table) else {
            return;
        };
        if self
            .lookup_component_member_type(base_type, field, type_table)
            .is_some()
            || self
                .infer_named_function_output_type(base, field, type_table)
                .is_some()
        {
            return;
        }
        if !Self::is_strict_component_member_owner(type_table, base_type) {
            return;
        }
        let Some(location) = base.get_location() else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(location, "field access validation") else {
            return;
        };
        self.emit_unknown_component_member(
            MissingComponentMember {
                owner_type: base_type,
                member_name: field.to_string(),
                reference: format!("{base}.{field}"),
                span,
            },
            type_table,
        );
    }

    /// Modelica permits selecting a function's named output from a call
    /// expression (`f(...).result`). The call itself has the value type of its
    /// first scalar output, so ordinary record-member lookup cannot distinguish
    /// this projection from a member of the returned record.
    fn infer_named_function_output_type(
        &self,
        base: &Expression,
        field: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        let Expression::FunctionCall { comp, .. } = base else {
            return None;
        };
        if let Some(output) = comp
            .def_id
            .and_then(|def_id| self.function_signatures.get(&def_id))
            .and_then(|signature| {
                signature
                    .outputs
                    .iter()
                    .find(|(name, _)| name == field)
                    .map(|(_, output)| output)
            })
        {
            let type_id = self.resolve_function_signature_component_type(comp, output, type_table);
            return (!type_id.is_unknown()).then_some(type_id);
        }
        let dotted_name = Self::component_ref_name(comp);
        let function = self.user_function_definition(comp, &dotted_name)?;
        let output = function.components.get(field)?;
        if !matches!(output.causality, rumoca_core::Causality::Output(_)) {
            return None;
        }
        let type_id = self.resolve_type_name(
            &output.type_name.to_string(),
            output.type_def_id,
            type_table,
        );
        (!type_id.is_unknown()).then_some(type_id)
    }

    pub(in crate::typechecker) fn resolve_component_reference_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
        type_table: &TypeTable,
    ) -> Result<TypeId, ComponentReferenceTypeError> {
        let Some((mut current_type, prefix_len)) = self.find_component_ref_prefix_type(comp)?
        else {
            return Ok(TypeId::UNKNOWN);
        };
        if prefix_len == comp.parts.len() {
            return Ok(current_type);
        }

        for part in comp.parts.iter().skip(prefix_len) {
            let member_name = part.ident.text.to_string();
            match self.lookup_component_member_type(current_type, &member_name, type_table) {
                Some(next_type) => current_type = next_type,
                None if !Self::is_strict_component_member_owner(type_table, current_type) => {
                    return Ok(TypeId::UNKNOWN);
                }
                None => {
                    let span = self.component_reference_member_span(&part.ident.location)?;
                    return Err(ComponentReferenceTypeError::MissingMember(
                        MissingComponentMember {
                            owner_type: current_type,
                            member_name,
                            reference: comp.to_string(),
                            span,
                        },
                    ));
                }
            }
        }

        Ok(current_type)
    }

    fn component_reference_member_span(
        &self,
        location: &rumoca_core::Location,
    ) -> Result<Span, ComponentReferenceTypeError> {
        self.source_map
            .try_span(
                location.source,
                location.start as usize,
                location.end as usize,
            )
            .ok_or_else(|| {
                let file_name = self
                    .source_map
                    .name(location.source)
                    .unwrap_or(crate::UNKNOWN_SOURCE_DISPLAY_NAME);
                ComponentReferenceTypeError::MissingSourceContext(
                    TypeCheckError::missing_source_context(format!(
                        "source file `{file_name}` for component reference member was not found"
                    )),
                )
            })
    }

    fn find_component_ref_prefix_type(
        &self,
        comp: &rumoca_ir_ast::ComponentReference,
    ) -> Result<Option<(TypeId, usize)>, ComponentReferenceTypeError> {
        for prefix_len in (1..=comp.parts.len()).rev() {
            match self.lookup_instance_reference(comp, prefix_len) {
                SemanticLookup::Found(semantics) => {
                    return Ok(Some((semantics.type_id, prefix_len)));
                }
                SemanticLookup::Ambiguous => {
                    return Err(self.ambiguous_component_reference(comp));
                }
                SemanticLookup::Missing => {}
            }
        }
        if let Some(semantics) = comp
            .def_id
            .and_then(|def_id| self.current_declaration_semantics.get(&def_id))
        {
            return Ok(Some((semantics.type_id, 1)));
        }
        Ok(None)
    }

    pub(super) fn lookup_instance_expression(
        &self,
        expression: &Expression,
    ) -> SemanticLookup<ComponentSemantics> {
        self.current_instance_semantics.lookup_expression(
            expression,
            self.current_class_instance_id,
            self.current_instance_scope.as_ref(),
        )
    }

    pub(in crate::typechecker) fn lookup_component_reference_prefix_shape(
        &self,
        reference: &rumoca_ir_ast::ComponentReference,
        prefix_len: usize,
    ) -> SemanticLookup<Option<Vec<usize>>> {
        match self.current_instance_semantics.lookup_reference_shape(
            reference,
            prefix_len,
            self.current_class_instance_id,
            self.current_instance_scope.as_ref(),
        ) {
            SemanticLookup::Missing if prefix_len == 1 => reference
                .def_id
                .and_then(|def_id| self.current_declaration_semantics.get(&def_id))
                .map_or(SemanticLookup::Missing, |semantics| {
                    SemanticLookup::Found(semantics.shape.clone())
                }),
            result => result,
        }
    }

    pub(super) fn lookup_component_reference_variability(
        &self,
        reference: &rumoca_ir_ast::ComponentReference,
    ) -> SemanticLookup<rumoca_eval_ast::eval::VariabilityLevel> {
        match self.lookup_instance_reference(reference, reference.parts.len()) {
            SemanticLookup::Found(semantics) => SemanticLookup::Found(semantics.variability),
            SemanticLookup::Missing => reference
                .def_id
                .and_then(|def_id| self.current_declaration_semantics.get(&def_id))
                .map_or(SemanticLookup::Missing, |semantics| {
                    SemanticLookup::Found(semantics.variability)
                }),
            SemanticLookup::Ambiguous => SemanticLookup::Ambiguous,
        }
    }

    fn lookup_instance_reference(
        &self,
        reference: &rumoca_ir_ast::ComponentReference,
        prefix_len: usize,
    ) -> SemanticLookup<ComponentSemantics> {
        self.current_instance_semantics.lookup_reference(
            reference,
            prefix_len,
            self.current_class_instance_id,
            self.current_instance_scope.as_ref(),
        )
    }

    fn ambiguous_component_reference(
        &self,
        reference: &rumoca_ir_ast::ComponentReference,
    ) -> ComponentReferenceTypeError {
        let span = if reference.span.is_dummy() {
            reference.parts.first().and_then(|part| {
                self.source_map.try_span(
                    part.ident.location.source,
                    part.ident.location.start as usize,
                    part.ident.location.end as usize,
                )
            })
        } else {
            Some(reference.span)
        };
        match span {
            Some(span) => ComponentReferenceTypeError::AmbiguousIdentity {
                reference: reference.to_string(),
                span,
            },
            None => ComponentReferenceTypeError::MissingSourceContext(
                TypeCheckError::missing_source_context(format!(
                    "source span for ambiguous component reference `{reference}` was not found"
                )),
            ),
        }
    }

    fn lookup_component_member_type(
        &self,
        current_type: TypeId,
        member_name: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        let current_root = self.resolve_type_root(type_table, current_type);
        let Some(Type::Class(class_type)) = type_table.get(current_root) else {
            return None;
        };
        self.component_modifier_member_types
            .get(&class_type.def_id)
            .and_then(|members| members.get(member_name).copied())
    }

    fn component_member_names(&self, owner_type: TypeId, type_table: &TypeTable) -> Vec<String> {
        let owner_root = self.resolve_type_root(type_table, owner_type);
        let Some(Type::Class(class_type)) = type_table.get(owner_root) else {
            return Vec::new();
        };
        let Some(members) = self.component_modifier_member_types.get(&class_type.def_id) else {
            return Vec::new();
        };
        let mut names = members.keys().cloned().collect::<Vec<_>>();
        names.sort();
        names
    }

    fn is_strict_component_member_owner(type_table: &TypeTable, owner_type: TypeId) -> bool {
        matches!(
            type_table.get(Self::resolve_alias_root(type_table, owner_type)),
            Some(Type::Class(class_type))
                if matches!(
                    class_type.kind,
                    ClassKind::Class
                        | ClassKind::Model
                        | ClassKind::Block
                        | ClassKind::Record
                        | ClassKind::Connector
                        | ClassKind::Type
                        | ClassKind::Operator
                )
        )
    }

    pub(in crate::typechecker) fn emit_unknown_component_member(
        &mut self,
        missing: MissingComponentMember,
        type_table: &TypeTable,
    ) {
        let owner_type = Self::format_type_name(type_table, missing.owner_type);
        let mut diagnostic = TypeCheckError::phase_diagnostic(
            "ET001",
            format!(
                "unknown member `{}` on component reference `{}` of type `{}`",
                missing.member_name, missing.reference, owner_type
            ),
            "unknown member",
            missing.span,
        );
        let available_members = self.component_member_names(missing.owner_type, type_table);
        if !available_members.is_empty() {
            diagnostic = diagnostic.with_note(format!(
                "available members: {}",
                available_members.join(", ")
            ));
        }
        self.emit_typecheck_error(diagnostic);
    }
}
