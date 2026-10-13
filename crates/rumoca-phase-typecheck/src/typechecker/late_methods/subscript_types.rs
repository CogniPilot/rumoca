//! Source subscript types and declaration index domains (MLS §10.5.1).

use super::*;
use rumoca_ir_ast as ast;

impl TypeChecker {
    pub(crate) fn collect_component_index_domains(&mut self, tree: &ClassTree, types: &TypeTable) {
        self.component_index_domains.clear();
        self.component_member_ids.clear();
        let classes = rumoca_ir_ast::ClassDefIndex::from_tree(tree);
        for class_id in classes.def_ids() {
            let Some(class) = classes.get(class_id) else {
                continue;
            };
            self.collect_class_index_domains(class_id, class, &classes, types);
        }
    }

    fn collect_class_index_domains(
        &mut self,
        class_id: DefId,
        class: &ClassDef,
        classes: &ast::ClassDefIndex<'_>,
        types: &TypeTable,
    ) {
        for (name, component) in &class.components {
            let Some(id) = component.def_id else { continue };
            let domains = self.component_source_index_domains(component, classes, types);
            self.component_index_domains.insert(id, domains);
            self.component_member_ids
                .insert((class_id, name.clone()), id);
        }
    }

    fn component_source_index_domains(
        &self,
        component: &Component,
        classes: &ast::ClassDefIndex<'_>,
        types: &TypeTable,
    ) -> Vec<TypeId> {
        let mut domains = self.dimension_index_domains(&component.shape_expr, types);
        let mut pending = component.type_def_id;
        let mut visited = HashSet::new();
        while let Some(alias_id) = pending.filter(|id| visited.insert(*id)) {
            let Some(alias) = classes.get(alias_id) else {
                break;
            };
            domains.extend(self.dimension_index_domains(&alias.array_subscripts, types));
            pending = alias.extends.first().and_then(|base| base.base_def_id);
        }
        domains
    }

    fn dimension_index_domains(
        &self,
        dimensions: &[ast::Subscript],
        types: &TypeTable,
    ) -> Vec<TypeId> {
        dimensions
            .iter()
            .map(|dimension| {
                let ast::Subscript::Expression(Expression::ComponentReference(reference)) =
                    dimension
                else {
                    return types.integer();
                };
                let ty = self.resolve_type_name(
                    &Self::component_ref_name(reference),
                    reference.target_def_id(),
                    types,
                );
                let root = self.resolve_type_root(types, ty);
                match types.get(root) {
                    Some(Type::Builtin(ast::BuiltinType::Boolean) | Type::Enumeration(_)) => root,
                    _ => types.integer(),
                }
            })
            .collect()
    }

    fn reference_part_index_domains(
        &self,
        reference: &ast::ComponentReference,
        part: usize,
        types: &TypeTable,
    ) -> Vec<TypeId> {
        if let Some(domains) = reference.parts[part]
            .def_id
            .and_then(|id| self.component_index_domains.get(&id))
        {
            return domains.clone();
        }
        let id = if part == 0 {
            reference.root_def_id()
        } else {
            let mut parent = reference.clone();
            parent.parts.truncate(part);
            let owner = self
                .infer_component_ref_type(&parent, types)
                .map(|ty| Self::resolve_alias_root(types, ty));
            match owner.and_then(|ty| types.get(ty)) {
                Some(Type::Class(class)) => self.index_domain_member_id(
                    class.def_id,
                    reference.parts[part].ident.text.as_ref(),
                ),
                _ => None,
            }
        };
        id.and_then(|id| self.component_index_domains.get(&id))
            .cloned()
            .unwrap_or_default()
    }

    fn index_domain_member_id(&self, owner: DefId, member: &str) -> Option<DefId> {
        let mut pending = vec![owner];
        let mut visited = HashSet::new();
        while let Some(owner) = pending.pop() {
            if !visited.insert(owner) {
                continue;
            }
            if let Some(id) = self.component_member_ids.get(&(owner, member.to_owned())) {
                return Some(*id);
            }
            pending.extend(
                self.class_base_def_ids
                    .get(&owner)
                    .into_iter()
                    .flatten()
                    .rev()
                    .copied(),
            );
        }
        None
    }

    pub(super) fn check_component_subscript_types(
        &mut self,
        reference: &ast::ComponentReference,
        types: &TypeTable,
    ) {
        let mut domains = Vec::new();
        for (part, segment) in reference.parts.iter().enumerate() {
            domains.extend(self.reference_part_index_domains(reference, part, types));
            let Some(subscripts) = &segment.subs else {
                continue;
            };
            self.check_subscript_types(subscripts, &domains, types);
            domains.drain(..subscripts.len().min(domains.len()));
        }
    }

    pub(super) fn check_array_index_types(&mut self, expression: &Expression, types: &TypeTable) {
        let Expression::ArrayIndex {
            base, subscripts, ..
        } = expression
        else {
            return;
        };
        let domains = self.expression_index_domains(base, types);
        self.check_subscript_types(subscripts, &domains, types);
    }

    fn expression_index_domains(&self, expression: &Expression, types: &TypeTable) -> Vec<TypeId> {
        match expression {
            Expression::ComponentReference(reference) => {
                self.remaining_reference_index_domains(reference, types)
            }
            Expression::Parenthesized { inner, .. } => self.expression_index_domains(inner, types),
            Expression::FunctionCall { comp, .. } => self.function_output_index_domains(comp, None),
            Expression::FieldAccess { base, field, .. } => {
                self.field_index_domains(base, field, types)
            }
            Expression::ArrayIndex {
                base, subscripts, ..
            } => {
                let mut domains = self.expression_index_domains(base, types);
                domains.drain(..subscripts.len().min(domains.len()));
                domains
            }
            _ => Vec::new(),
        }
    }

    fn function_output_index_domains(
        &self,
        comp: &ast::ComponentReference,
        field: Option<&str>,
    ) -> Vec<TypeId> {
        let output = comp
            .root_def_id()
            .and_then(|id| self.function_signatures.get(&id))
            .and_then(|signature| match field {
                Some(field) => signature.outputs.iter().find(|(name, _)| name == field),
                None => signature.outputs.first(),
            })
            .and_then(|(_, component)| component.def_id);
        output
            .and_then(|id| self.component_index_domains.get(&id))
            .cloned()
            .unwrap_or_default()
    }

    fn field_index_domains(
        &self,
        base: &Expression,
        field: &str,
        types: &TypeTable,
    ) -> Vec<TypeId> {
        if let Expression::FunctionCall { comp, .. } = base {
            let output = self.function_output_index_domains(comp, Some(field));
            if !output.is_empty() {
                return output;
            }
        }
        let owner = self
            .infer_expression_type(base, types)
            .map(|ty| Self::resolve_alias_root(types, ty));
        let Some(Type::Class(class)) = owner.and_then(|ty| types.get(ty)) else {
            return Vec::new();
        };
        self.index_domain_member_id(class.def_id, field)
            .and_then(|id| self.component_index_domains.get(&id))
            .cloned()
            .unwrap_or_default()
    }

    fn remaining_reference_index_domains(
        &self,
        reference: &ast::ComponentReference,
        types: &TypeTable,
    ) -> Vec<TypeId> {
        let mut domains = Vec::new();
        for (part, segment) in reference.parts.iter().enumerate() {
            domains.extend(self.reference_part_index_domains(reference, part, types));
            if let Some(subscripts) = &segment.subs {
                domains.drain(..subscripts.len().min(domains.len()));
            }
        }
        domains
    }

    fn check_subscript_types(
        &mut self,
        subscripts: &[ast::Subscript],
        domains: &[TypeId],
        types: &TypeTable,
    ) {
        for (dimension, subscript) in subscripts.iter().enumerate() {
            let ast::Subscript::Expression(expression) = subscript else {
                continue;
            };
            let Some(actual) = self.infer_expression_type(expression, types) else {
                continue;
            };
            let actual = self.subscript_element_type(actual, types);
            let expected = domains.get(dimension).copied().unwrap_or(types.integer());
            if self
                .infer_expression_shape(expression, types)
                .is_some_and(|shape| shape.len() > 1)
            {
                self.emit_subscript_type_error(
                    expression,
                    "array subscript must be a scalar or vector (MLS §10.5.1)".to_owned(),
                );
                continue;
            }
            if actual == expected || actual.is_unknown() {
                continue;
            }
            self.emit_subscript_type_error(
                expression,
                format!(
                    "array subscript must have {} type, found {} (MLS §10.5.1)",
                    Self::format_type_name(types, expected),
                    Self::format_type_name(types, actual)
                ),
            );
        }
    }

    fn subscript_element_type(&self, ty: TypeId, types: &TypeTable) -> TypeId {
        let mut ty = self.resolve_type_root(types, ty);
        let mut visited = HashSet::new();
        while visited.insert(ty) {
            let Some(Type::Array(array)) = types.get(ty) else {
                return ty;
            };
            ty = self.resolve_type_root(types, array.element);
        }
        TypeId::UNKNOWN
    }

    fn emit_subscript_type_error(&mut self, expression: &Expression, message: String) {
        let Some(location) = expression.get_location() else {
            return;
        };
        let Some(span) = self.diagnostic_location_span(location, "subscript type") else {
            return;
        };
        self.emit_typecheck_error(TypeCheckError::phase_diagnostic(
            "ET009",
            message,
            "subscript type here",
            span,
        ));
    }
}
