//! Enumeration parameter evaluation for the flatten context: literal
//! extraction, reference chasing through parameters and record aliases, and
//! normalization to final literal values (MLS §4.9.5).

use super::param_binding::ParamBinding;
use super::*;

enum EnumDependencyStep {
    Follow(rumoca_eval_flat::constant::ResolvedOccurrenceKey),
    Resolved,
    DeclarationMissing,
    QualifiedReference,
    SubscriptedReference,
    NonReference,
    NonEnumeration,
}

impl Context {
    /// Extract enumeration parameter values (MLS §4.9.5).
    ///
    /// Enumeration values retain their resolved declaration and ordinal.
    /// This handles both direct enum literals and references to other enum parameters.
    /// MLS §4.9.5: Enumeration types have literals that are constant values.
    pub(super) fn eval_enum_param_bindings(
        &mut self,
        params: &[ParamBinding<'_>],
    ) -> Result<bool, FlattenError> {
        let mut progress = false;
        loop {
            let new_vals = self.collect_enum_values(params)?;
            if new_vals.is_empty() {
                break;
            }
            let pass_progress = self.insert_enum_values(new_vals)?;
            progress |= pass_progress;
            if !pass_progress {
                break;
            }
        }
        self.reject_unresolved_enum_identity_cycles(params)?;
        Ok(progress)
    }

    fn reject_unresolved_enum_identity_cycles(
        &self,
        params: &[ParamBinding<'_>],
    ) -> Result<(), FlattenError> {
        let enum_declarations = params
            .iter()
            .filter(|binding| binding.primitive.may_evaluate_as_enumeration())
            .map(|binding| (binding.identity, binding))
            .collect::<rustc_hash::FxHashMap<_, _>>();
        let declarations = params
            .iter()
            .map(|binding| (binding.identity, binding))
            .collect::<rustc_hash::FxHashMap<_, _>>();
        for start in enum_declarations.keys() {
            if self
                .parameter_values_by_identity
                .get(start)
                .is_some_and(|value| {
                    matches!(value, rumoca_eval_flat::constant::Value::ResolvedEnum(_))
                })
            {
                continue;
            }
            self.walk_unresolved_enum_dependency(*start, &declarations)?;
        }
        Ok(())
    }

    fn walk_unresolved_enum_dependency(
        &self,
        start: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
        declarations: &rustc_hash::FxHashMap<
            rumoca_eval_flat::constant::ResolvedOccurrenceKey,
            &ParamBinding<'_>,
        >,
    ) -> Result<(), FlattenError> {
        let mut seen = rustc_hash::FxHashSet::default();
        let mut current = start;
        loop {
            if !seen.insert(current) {
                return Err(FlattenError::internal(
                    "cyclic enumeration parameter dependency by exact occurrence identity",
                ));
            }
            match self.enum_dependency_step(current, declarations) {
                EnumDependencyStep::Follow(next) => current = next,
                EnumDependencyStep::Resolved
                | EnumDependencyStep::DeclarationMissing
                | EnumDependencyStep::QualifiedReference
                | EnumDependencyStep::SubscriptedReference
                | EnumDependencyStep::NonReference
                | EnumDependencyStep::NonEnumeration => return Ok(()),
            }
        }
    }

    fn enum_dependency_step(
        &self,
        current: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
        declarations: &rustc_hash::FxHashMap<
            rumoca_eval_flat::constant::ResolvedOccurrenceKey,
            &ParamBinding<'_>,
        >,
    ) -> EnumDependencyStep {
        let Some(binding) = declarations.get(&current) else {
            return EnumDependencyStep::DeclarationMissing;
        };
        if !binding.primitive.may_evaluate_as_enumeration() {
            return EnumDependencyStep::NonEnumeration;
        }
        let Expression::VarRef {
            name, subscripts, ..
        } = binding.binding
        else {
            return EnumDependencyStep::NonReference;
        };
        if !subscripts.is_empty() {
            return EnumDependencyStep::SubscriptedReference;
        }
        if name.parts().len() > 1 {
            return EnumDependencyStep::QualifiedReference;
        }
        let Some(next) =
            name.instance_id()
                .zip(name.root_def_id())
                .map(|(instance_id, root_def_id)| {
                    rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                        instance_id,
                        root_def_id,
                    }
                })
        else {
            return EnumDependencyStep::DeclarationMissing;
        };
        if self
            .parameter_values_by_identity
            .get(&next)
            .is_some_and(|value| {
                matches!(value, rumoca_eval_flat::constant::Value::ResolvedEnum(_))
            })
        {
            return EnumDependencyStep::Resolved;
        }
        match declarations.get(&next) {
            Some(binding) if binding.primitive.may_evaluate_as_enumeration() => {
                EnumDependencyStep::Follow(next)
            }
            Some(_) => EnumDependencyStep::NonEnumeration,
            None => EnumDependencyStep::DeclarationMissing,
        }
    }

    fn collect_enum_values(
        &self,
        params: &[ParamBinding<'_>],
    ) -> Result<
        Vec<(
            String,
            rumoca_eval_flat::constant::ResolvedOccurrenceKey,
            rumoca_eval_flat::constant::Value,
        )>,
        FlattenError,
    > {
        let mut values = Vec::new();
        for ParamBinding {
            name,
            identity,
            binding,
            primitive,
            ..
        } in params
        {
            if !primitive.may_evaluate_as_enumeration() {
                continue;
            }
            if let Some(enum_val) = self.try_eval_enum_binding(binding)? {
                values.push(((*name).to_string(), *identity, enum_val));
            }
        }
        Ok(values)
    }

    fn insert_enum_values(
        &mut self,
        new_vals: Vec<(
            String,
            rumoca_eval_flat::constant::ResolvedOccurrenceKey,
            rumoca_eval_flat::constant::Value,
        )>,
    ) -> Result<bool, FlattenError> {
        let mut progress = false;
        for (name, identity, val) in new_vals {
            let rumoca_eval_flat::constant::Value::ResolvedEnum(resolved) = val else {
                return Err(FlattenError::internal(format!(
                    "resolved enumeration parameter `{name}` produced a non-resolved value"
                )));
            };
            let should_insert = self
                .enum_parameter_values
                .get(&name)
                .is_none_or(|existing| {
                    existing.declaration() != resolved.declaration()
                        || existing.ordinal() != resolved.ordinal()
                });
            if should_insert {
                self.enum_parameter_values.insert(name, resolved.clone());
                self.parameter_values_by_identity.insert(
                    identity,
                    rumoca_eval_flat::constant::Value::ResolvedEnum(resolved),
                );
                progress = true;
            }
        }
        Ok(progress)
    }

    fn try_eval_enum_binding(
        &self,
        binding: &Expression,
    ) -> Result<Option<rumoca_eval_flat::constant::Value>, FlattenError> {
        let param_context = ParamEvalContext::new_resolved(
            &self.parameter_values,
            &self.real_parameter_values,
            &self.boolean_parameter_values,
            &self.array_dimensions,
            &self.functions,
            rumoca_eval_flat::phase_constant::ResolvedParamInventory::new(
                &self.parameter_values_by_identity,
                &self.array_dimensions_by_identity,
                &self.resolved_enum_catalog,
            ),
            None,
        );
        let evaluated = crate::constant_eval::map_optional_evaluation(
            rumoca_eval_flat::phase_constant::try_eval_flat_expr_enum_value_with_context(
                binding,
                &param_context,
            ),
            "evaluating an enumeration parameter binding",
            binding.span(),
        )?;
        Ok(evaluated)
    }
}
