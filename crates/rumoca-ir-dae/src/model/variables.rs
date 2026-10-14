use super::*;

pub struct Variables<'storage, 'dae> {
    pub(super) source_map: &'storage SourceMap,
    pub(super) storage: &'storage mut Storage,
    pub(super) marker: PhantomData<&'dae mut &'dae ()>,
}

macro_rules! variable_role_constructors {
    ($(
        $complete:ident / $reserve:ident
        ($($argument:ident: $argument_type:ty),*)
        => $id:ident, $role:path, $variability:expr
    );+ $(;)?) => {
        $(pub fn $complete(
            &mut self,
            name: VarName,
            value_type: ValueTypeId<'dae>,
            $($argument: $argument_type,)*
            declaration: DaeProvenance,
            attributes: VariableAttributes<'dae>,
        ) -> Result<$id<'dae>, DaeConstructionError> {
            self.add_complete(
                name,
                $role,
                $variability,
                value_type,
                declaration,
                attributes,
            )
            .map(|id| $id::from_raw(id.index()))
        }

        pub fn $reserve(
            &mut self,
            name: VarName,
            value_type: ValueTypeId<'dae>,
            $($argument: $argument_type,)*
            declaration: DaeProvenance,
        ) -> Result<($id<'dae>, VariableReservation<'dae>), DaeConstructionError> {
            let id = self.reserve_forward(
                name,
                $role,
                $variability,
                value_type,
                declaration,
            )?;
            Ok((
                $id::from_raw(id.index()),
                VariableReservation { variable: id },
            ))
        })+
    };
}

impl<'dae> Variables<'_, 'dae> {
    variable_role_constructors! {
        parameter / reserve_parameter ()
            => ParameterId, VariableRole::Parameter, ExpressionVariability::Parameter;
        constant / reserve_constant ()
            => ParameterId, VariableRole::Constant, ExpressionVariability::Constant;
        input / reserve_input (variability: InputVariability)
            => InputId, VariableRole::Input, variability.expression_variability();
        state / reserve_state ()
            => StateId, VariableRole::State, ExpressionVariability::Continuous;
        algebraic / reserve_algebraic ()
            => AlgebraicId, VariableRole::Algebraic, ExpressionVariability::Continuous;
        output / reserve_output ()
            => AlgebraicId, VariableRole::Output, ExpressionVariability::Continuous;
        discrete_real / reserve_discrete_real ()
            => DiscreteRealId, VariableRole::DiscreteReal, ExpressionVariability::Discrete;
        discrete_value / reserve_discrete_value ()
            => DiscreteValueId, VariableRole::DiscreteValue, ExpressionVariability::Discrete;
    }

    pub fn define(
        &mut self,
        reservation: VariableReservation<'dae>,
        attributes: VariableAttributes<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let variable = reservation.variable;
        self.validate_attributes(variable, &attributes, provenance)?;
        let role = self
            .storage
            .variables
            .get(variable.index() as usize)
            .map(|entry| entry.role)
            .ok_or_else(|| unknown("variable", variable.index(), provenance))?;
        let requires_discrete_value_owner =
            role == VariableRole::DiscreteValue && attributes.causality != VariableCausality::Input;
        if requires_discrete_value_owner {
            self.storage
                .expect_discrete_value_topology_open(provenance)?;
        }
        let Some(entry) = self.storage.variables.get_mut(variable.index() as usize) else {
            return Err(unknown("variable", variable.index(), provenance));
        };
        if entry.attributes.is_some() {
            return Err(duplicate("variable", variable.index(), provenance));
        }
        entry.attributes = Some(erase_variable_attributes(attributes));
        if requires_discrete_value_owner {
            self.storage
                .register_required_discrete_value(variable.index(), provenance);
        }
        self.storage.unfilled_variables -= 1;
        Ok(())
    }

    fn add_complete(
        &mut self,
        name: VarName,
        role: VariableRole,
        variability: ExpressionVariability,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
        let requires_discrete_value_owner =
            role == VariableRole::DiscreteValue && attributes.causality != VariableCausality::Input;
        if requires_discrete_value_owner {
            self.storage
                .expect_discrete_value_topology_open(declaration)?;
        }
        let id = self.reserve_forward(name, role, variability, value_type, declaration)?;
        self.validate_attributes(id, &attributes, declaration)?;
        self.storage.variables[id.index() as usize].attributes =
            Some(erase_variable_attributes(attributes));
        if requires_discrete_value_owner {
            self.storage
                .register_required_discrete_value(id.index(), declaration);
        }
        self.storage.unfilled_variables -= 1;
        Ok(id)
    }

    fn reserve_forward(
        &mut self,
        name: VarName,
        role: VariableRole,
        variability: ExpressionVariability,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, declaration)?;
        let capability = self.storage.variable_type_capability(
            &name,
            role,
            variability,
            value_type,
            declaration,
        )?;
        self.insert_reservation(name, capability, declaration)
    }

    fn insert_reservation(
        &mut self,
        name: VarName,
        capability: VariableTypeCapability<'dae>,
        declaration: DaeProvenance,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
        if self.storage.variable_by_name.contains_key(&name) {
            return Err(DaeConstructionError::DuplicateKey {
                kind: "variable",
                key: name.to_string(),
                span: declaration.span(),
            });
        }
        let raw = checked_u32(self.storage.variables.len(), "variable arena", declaration)?;
        self.storage.variable_by_name.insert(name.clone(), raw);
        self.storage.variables.push(VariableEntry {
            name,
            role: capability.role(),
            variability: capability.variability(),
            value_type: capability.value_type().index(),
            declaration,
            attributes: None,
        });
        self.storage.unfilled_variables += 1;
        Ok(VariableId::from_raw(raw))
    }

    fn validate_attributes(
        &self,
        variable: VariableId<'dae>,
        attributes: &VariableAttributes<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let expected = self
            .storage
            .variables
            .get(variable.index() as usize)
            .map(|entry| entry.value_type)
            .ok_or_else(|| unknown("variable", variable.index(), provenance))?;
        if attributes.is_held {
            let entry = &self.storage.variables[variable.index() as usize];
            if !matches!(
                entry.role,
                VariableRole::DiscreteReal | VariableRole::DiscreteValue
            ) || attributes.start.is_none()
            {
                return Err(DaeConstructionError::InvalidVariableRole {
                    name: entry.name.clone(),
                    span: provenance.span(),
                });
            }
        }
        self.validate_evaluable(variable, attributes, provenance)?;
        self.validate_declared_causality(variable, attributes, provenance)?;
        if let Some(binding) = attributes.binding {
            self.storage.expect_closed_expression(binding, provenance)?;
            let found = self
                .storage
                .expressions
                .value_types
                .get(binding.index() as usize)
                .copied()
                .ok_or_else(|| unknown("expression", binding.index(), provenance))?;
            self.storage
                .expect_value_type_compatible(expected, found, provenance)?;
        }
        for expression in [
            attributes.start,
            attributes.min,
            attributes.max,
            attributes.nominal,
        ]
        .into_iter()
        .flatten()
        {
            self.storage
                .expect_closed_expression(expression, provenance)?;
            let found = self
                .storage
                .expressions
                .value_types
                .get(expression.index() as usize)
                .copied()
                .ok_or_else(|| unknown("expression", expression.index(), provenance))?;
            self.storage
                .expect_attribute_type_compatible(expected, found, provenance)?;
        }
        Ok(())
    }

    /// An exported `Input` or `Output` causality exists only for a declaration
    /// carrying the same prefix (SPEC_0040 DAE-C24).
    fn validate_declared_causality(
        &self,
        variable: VariableId<'dae>,
        attributes: &VariableAttributes<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let required = match attributes.causality {
            VariableCausality::Input => DeclaredCausality::Input,
            VariableCausality::Output => DeclaredCausality::Output,
            _ => return Ok(()),
        };
        if attributes.declared_causality == required {
            return Ok(());
        }
        let entry = self.storage.variable(variable.index(), provenance)?;
        Err(DaeConstructionError::InvalidDeclaredCausality {
            name: entry.name.clone(),
            span: provenance.span(),
        })
    }
}

/// Reduce a scalarized `fixed` attribute to a single value when every element
/// agrees (MLS §4.8). Absent attributes and differing elements both yield
/// `None`, which a whole-declaration decision cannot represent.
pub(crate) fn uniform_fixed(values: Option<&[bool]>) -> Option<bool> {
    let values = values?;
    let first = *values.first()?;
    values.iter().all(|&value| value == first).then_some(first)
}

/// Select the `fixed` value for one scalar element, broadcasting a single
/// stored value over every element (MLS §4.8.6).
pub(crate) fn scalar_fixed(values: Option<&[bool]>, scalar: usize) -> Option<bool> {
    let values = values?;
    if values.len() == 1 {
        values.first().copied()
    } else {
        values.get(scalar).copied()
    }
}

fn erase_variable_attributes(attributes: VariableAttributes<'_>) -> VariableAttributesWire {
    VariableAttributesWire {
        component_ref: attributes.component_ref,
        binding: attributes.binding.map(ExprId::index),
        start: attributes.start.map(ExprId::index),
        fixed: attributes.fixed,
        min: attributes.min.map(ExprId::index),
        max: attributes.max.map(ExprId::index),
        nominal: attributes.nominal.map(ExprId::index),
        unit: attributes.unit,
        state_select: attributes.state_select,
        description: attributes.description,
        enumeration: attributes.enumeration,
        causality: attributes.causality,
        declared_causality: attributes.declared_causality,
        is_tunable: attributes.is_tunable,
        is_held: attributes.is_held,
        evaluable: attributes.evaluable,
        origin: attributes.origin,
    }
}
