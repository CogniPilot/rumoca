use super::*;

impl Storage {
    pub(super) fn freeze(self) -> FrozenStorage {
        FrozenStorage {
            value_types: self.value_types.into_boxed_slice(),
            flat_type_ids: self.flat_type_ids.into_boxed_slice(),
            value_type_provenance: self.value_type_provenance.into_boxed_slice(),
            variables: self.variables.into_boxed_slice(),
            functions: self.functions.into_boxed_slice(),
            function_folds: self.function_folds.into_boxed_slice(),
            domains: self.domains.into_boxed_slice(),
            expressions: self.expressions.freeze(),
            continuous_equations: self.continuous_equations.into_boxed_slice(),
            initialization_equations: self.initialization_equations.into_boxed_slice(),
            discrete_real_equations: self.discrete_real_equations.into_boxed_slice(),
            discrete_assignments: self.discrete_assignments.into_boxed_slice(),
            continuous_families: self.continuous_families.into_boxed_slice(),
            initialization_families: self.initialization_families.into_boxed_slice(),
            continuous_equation_owners: self.continuous_equation_owners.into_boxed_slice(),
            initialization_equation_owners: self.initialization_equation_owners.into_boxed_slice(),
            equation_family_bodies: self.equation_family_bodies.into_boxed_slice(),
            relations: self.relations.into_boxed_slice(),
            conditions: self.conditions.into_boxed_slice(),
            roots: self.roots.into_boxed_slice(),
            time_events: self.time_events.into_boxed_slice(),
            event_actions: self.event_actions.into_boxed_slice(),
            clocks: self.clocks.into_boxed_slice(),
            clock_ownerships: self.clock_ownerships.into_boxed_slice(),
            previous_values: self.previous_values.into_boxed_slice(),
            terminals: self.terminals.into_boxed_slice(),
            delays: self.delays.into_boxed_slice(),
        }
    }

    pub(crate) fn intern_flat_type<'dae>(
        &mut self,
        flat_type: TypeId,
        ty: ValueType,
        at: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_type_capacity(&ty, at)?;
        if flat_type.is_unknown() {
            return Err(DaeConstructionError::InvalidEffectiveTypeId {
                type_id: flat_type,
                span: at.span(),
            });
        }
        if let Some(index) = self
            .flat_type_ids
            .iter()
            .zip(&self.value_types)
            .position(|(candidate, value_type)| *candidate == Some(flat_type) && value_type == &ty)
        {
            return Ok(ValueTypeId::from_raw(index as u32));
        }
        let raw = checked_u32(self.value_types.len(), "value type arena", at)?;
        self.value_types.push(ty);
        self.flat_type_ids.push(Some(flat_type));
        self.value_type_provenance.push(at);
        Ok(ValueTypeId::from_raw(raw))
    }

    pub(crate) fn intern_type<'dae>(
        &mut self,
        ty: ValueType,
        at: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_type_capacity(&ty, at)?;
        if let Some(index) = self
            .value_types
            .iter()
            .position(|candidate| candidate == &ty)
        {
            return Ok(ValueTypeId::from_raw(index as u32));
        }
        let raw = checked_u32(self.value_types.len(), "value type arena", at)?;
        self.value_types.push(ty);
        self.flat_type_ids.push(None);
        self.value_type_provenance.push(at);
        Ok(ValueTypeId::from_raw(raw))
    }

    pub(crate) fn expr_type<'dae>(
        &self,
        expression: ExprId<'dae>,
        at: DaeProvenance,
    ) -> Result<&ValueType, DaeConstructionError> {
        let Some(&ty) = self
            .expressions
            .value_types
            .get(expression.index() as usize)
        else {
            return Err(unknown("expression", expression.index(), at));
        };
        self.value_types
            .get(ty as usize)
            .ok_or_else(|| unknown("value type", ty, at))
    }

    pub(crate) fn expr_variability(
        &self,
        expression: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<ExpressionVariability, DaeConstructionError> {
        self.expressions
            .variability
            .get(expression.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression.index(), at))
    }

    pub(crate) fn expect_value_type_compatible(
        &self,
        expected: u32,
        found: u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let expected = self.value_type_at(expected, at)?;
        let found = self.value_type_at(found, at)?;
        if expected == found
            || (expected.dimensions() == found.dimensions()
                && expected.scalar_type() == ScalarType::Real
                && found.scalar_type() == ScalarType::Integer)
        {
            return Ok(());
        }
        Err(DaeConstructionError::ShapeMismatch { span: at.span() })
    }

    pub(crate) fn expect_attribute_type_compatible(
        &self,
        expected: u32,
        found: u32,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let expected_type = self.value_type_at(expected, at)?;
        let found_type = self.value_type_at(found, at)?;
        if found_type.is_scalar()
            && (!expected_type.is_scalar())
            && (expected_type.scalar_type() == found_type.scalar_type()
                || (expected_type.scalar_type() == ScalarType::Real
                    && found_type.scalar_type() == ScalarType::Integer))
        {
            return Ok(());
        }
        self.expect_value_type_compatible(expected, found, at)
    }

    pub(crate) fn expr_binder_domain(
        &self,
        expression: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<Option<u32>, DaeConstructionError> {
        self.expressions
            .binder_domains
            .get(expression.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression.index(), at))
    }

    pub(crate) fn expr_function_scope(
        &self,
        expression: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<Option<u32>, DaeConstructionError> {
        self.expressions
            .function_scopes
            .get(expression.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", expression.index(), at))
    }

    pub(crate) fn static_integer(&self, expression: ExprId<'_>) -> Option<i64> {
        let mut current = expression.index();
        for _ in 0..=self.variables.len() {
            match self.expressions.nodes.get(current as usize)? {
                ExprNode::Literal(crate::DaeLiteral::Integer(value)) => return Some(*value),
                ExprNode::Coordinate(crate::expression::Coordinate::Parameter(variable)) => {
                    current = self
                        .variables
                        .get(*variable as usize)?
                        .attributes
                        .as_ref()?
                        .binding?;
                }
                _ => return None,
            }
        }
        None
    }

    pub(crate) fn expect_closed_expression(
        &self,
        expression: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        match self.expr_binder_domain(expression, at)? {
            None => {}
            Some(found_domain) => Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: None,
                found_domain,
                span: at.span(),
            })?,
        }
        match self.expr_function_scope(expression, at)? {
            None => Ok(()),
            Some(found_function) => Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: None,
                found_function,
                span: at.span(),
            }),
        }
    }

    pub(crate) fn expect_function_expression(
        &self,
        expression: ExprId<'_>,
        function: FunctionId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        if let Some(found_domain) = self.expr_binder_domain(expression, at)? {
            return Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: None,
                found_domain,
                span: at.span(),
            });
        }
        match self.expr_function_scope(expression, at)? {
            None => Ok(()),
            Some(found_function) if found_function == function.index() => Ok(()),
            Some(found_function) => Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(function.index()),
                found_function,
                span: at.span(),
            }),
        }
    }

    pub(crate) fn expect_domain_expression(
        &self,
        expression: ExprId<'_>,
        domain: DomainId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        match self.expr_binder_domain(expression, at)? {
            None => Ok(()),
            Some(found_domain)
                if self.domain_is_ancestor_or_same(found_domain, domain.index(), at)? =>
            {
                Ok(())
            }
            Some(found_domain) => Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: Some(domain.index()),
                found_domain,
                span: at.span(),
            }),
        }
    }

    pub(crate) fn domain_parent(
        &self,
        domain: DomainId<'_>,
        at: DaeProvenance,
    ) -> Result<Option<u32>, DaeConstructionError> {
        self.domains
            .get(domain.index() as usize)
            .map(|entry| entry.parent)
            .ok_or_else(|| unknown("domain", domain.index(), at))
    }

    pub(crate) fn domain_is_ancestor_or_same(
        &self,
        ancestor: u32,
        mut domain: u32,
        at: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        loop {
            if ancestor == domain {
                return Ok(true);
            }
            let entry = self
                .domains
                .get(domain as usize)
                .ok_or_else(|| unknown("domain", domain, at))?;
            let Some(parent) = entry.parent else {
                return Ok(false);
            };
            domain = parent;
        }
    }

    pub(crate) fn domain_binder(
        &self,
        domain: u32,
        ordinal: u32,
        at: DaeProvenance,
    ) -> Result<&rumoca_core::StructuredIndexBinder, DaeConstructionError> {
        self.domains
            .get(domain as usize)
            .ok_or_else(|| unknown("domain", domain, at))?
            .domain
            .binders
            .get(ordinal as usize)
            .ok_or_else(|| unknown("domain binder", ordinal, at))
    }

    pub(crate) fn coordinate_facts<'dae>(
        &mut self,
        coordinate: CoordinateInput<'dae>,
        at: DaeProvenance,
    ) -> Result<(ValueTypeId<'dae>, ExpressionVariability), DaeConstructionError> {
        let (raw, variability) = match coordinate {
            CoordinateInput::Parameter(id) => self.coordinate_variable_facts(
                id.index(),
                &[VariableRole::Parameter, VariableRole::Constant],
                at,
            )?,
            CoordinateInput::Input(id) => {
                self.coordinate_variable_facts(id.index(), &[VariableRole::Input], at)?
            }
            CoordinateInput::State(id) | CoordinateInput::Derivative(id) => {
                self.coordinate_variable_facts(id.index(), &[VariableRole::State], at)?
            }
            CoordinateInput::Algebraic(id) => self.coordinate_variable_facts(
                id.index(),
                &[VariableRole::Algebraic, VariableRole::Output],
                at,
            )?,
            CoordinateInput::DiscreteReal(id) | CoordinateInput::PreDiscreteReal(id) => {
                self.coordinate_variable_facts(id.index(), &[VariableRole::DiscreteReal], at)?
            }
            CoordinateInput::DiscreteValue(id) | CoordinateInput::PreDiscreteValue(id) => {
                self.coordinate_variable_facts(id.index(), &[VariableRole::DiscreteValue], at)?
            }
            CoordinateInput::Time => {
                let ty = self.intern_type(ValueType::scalar(ScalarType::Real), at)?;
                return Ok((ty, ExpressionVariability::Continuous));
            }
            CoordinateInput::Condition(id) => {
                if self.conditions.get(id.index() as usize).is_none() {
                    return Err(unknown("condition", id.index(), at));
                }
                let ty = self.intern_type(ValueType::scalar(ScalarType::Boolean), at)?;
                return Ok((ty, ExpressionVariability::Discrete));
            }
            CoordinateInput::Previous(id) => {
                let previous = self
                    .previous_values
                    .get(id.index() as usize)
                    .ok_or_else(|| unknown("previous value", id.index(), at))?;
                (previous.value_type, ExpressionVariability::Discrete)
            }
            CoordinateInput::Terminal(id) => {
                self.terminals
                    .get(id.index() as usize)
                    .ok_or_else(|| unknown("terminal coordinate", id.index(), at))?;
                let ty = self.intern_type(ValueType::scalar(ScalarType::Boolean), at)?;
                return Ok((ty, ExpressionVariability::Discrete));
            }
            CoordinateInput::FunctionParameter(id) => {
                return self.function_parameter_facts(id, at);
            }
        };
        Ok((ValueTypeId::from_raw(raw), variability))
    }

    pub(crate) fn function_parameter_facts<'dae>(
        &self,
        parameter: FunctionParameterId<'dae>,
        at: DaeProvenance,
    ) -> Result<(ValueTypeId<'dae>, ExpressionVariability), DaeConstructionError> {
        let function = self
            .functions
            .get(parameter.function().index() as usize)
            .ok_or_else(|| unknown("function", parameter.function().index(), at))?;
        let value_type = function
            .parameters
            .get(parameter.ordinal() as usize)
            .copied()
            .ok_or_else(|| unknown("function parameter", parameter.ordinal(), at))?;
        Ok((
            ValueTypeId::from_raw(value_type),
            ExpressionVariability::Continuous,
        ))
    }

    pub(crate) fn function_value_facts<'dae>(
        &self,
        value: FunctionValueId<'dae>,
        at: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        let value_type = self
            .functions
            .get(value.function().index() as usize)
            .and_then(|function| function.values.get(value.ordinal() as usize))
            .map(|value| value.value_type)
            .ok_or_else(|| unknown("function value", value.ordinal(), at))?;
        Ok(ValueTypeId::from_raw(value_type))
    }

    fn coordinate_variable_facts(
        &self,
        raw: u32,
        roles: &[VariableRole],
        at: DaeProvenance,
    ) -> Result<(u32, ExpressionVariability), DaeConstructionError> {
        let variable = self
            .variables
            .get(raw as usize)
            .ok_or_else(|| unknown("variable", raw, at))?;
        if !roles.contains(&variable.role) {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable.name.clone(),
                span: at.span(),
            });
        }
        Ok((variable.value_type, variable.variability))
    }

    pub(crate) fn function_signature<'dae>(
        &self,
        function: FunctionId<'dae>,
        at: DaeProvenance,
    ) -> Result<(&[u32], &[u32]), DaeConstructionError> {
        self.functions
            .get(function.index() as usize)
            .map(|function| (function.parameters.as_slice(), function.results.as_slice()))
            .ok_or_else(|| unknown("function", function.index(), at))
    }

    pub(crate) fn domain_extents<'dae>(
        &self,
        domain: DomainId<'dae>,
        at: DaeProvenance,
    ) -> Result<&[u32], DaeConstructionError> {
        self.domains
            .get(domain.index() as usize)
            .map(|domain| domain.extents.as_ref())
            .ok_or_else(|| unknown("domain", domain.index(), at))
    }

    pub(crate) fn domain_scalar_count(
        &self,
        domain: DomainId<'_>,
        at: DaeProvenance,
    ) -> Result<usize, DaeConstructionError> {
        self.domains
            .get(domain.index() as usize)
            .map(|domain| domain.scalar_count as usize)
            .ok_or_else(|| unknown("domain", domain.index(), at))
    }

    pub(crate) fn expect_real_residual(
        &self,
        residual: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.expect_closed_expression(residual, at)?;
        let ty = self.expr_type(residual, at)?;
        if ty.is_scalar() && ty.scalar_type() == ScalarType::Real {
            return Ok(());
        }
        Err(DaeConstructionError::TypeMismatch {
            expected: ScalarType::Real,
            found: ty.scalar_type(),
            span: at.span(),
        })
    }

    pub(crate) fn expect_primitive_relation(
        &self,
        relation: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.expect_closed_expression(relation, at)?;
        let ty = self.expr_type(relation, at)?;
        let node = self
            .expressions
            .nodes
            .get(relation.index() as usize)
            .ok_or_else(|| unknown("expression", relation.index(), at))?;
        if ty.is_scalar()
            && ty.scalar_type() == ScalarType::Boolean
            && matches!(
                node,
                ExprNode::Binary {
                    operator: BinaryOperator::Equal
                        | BinaryOperator::NotEqual
                        | BinaryOperator::Less
                        | BinaryOperator::LessEqual
                        | BinaryOperator::Greater
                        | BinaryOperator::GreaterEqual,
                    ..
                }
            )
        {
            return Ok(());
        }
        Err(DaeConstructionError::ExpectedPrimitiveRelation { span: at.span() })
    }

    pub(crate) fn expect_discrete_value_target(
        &self,
        target: DiscreteValueId<'_>,
        value: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.expect_closed_expression(value, at)?;
        let variable = self
            .variables
            .get(target.index() as usize)
            .ok_or_else(|| unknown("variable", target.index(), at))?;
        if variable.role != VariableRole::DiscreteValue {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable.name.clone(),
                span: at.span(),
            });
        }
        let found = self
            .expressions
            .value_types
            .get(value.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", value.index(), at))?;
        self.expect_value_type_compatible(variable.value_type, found, at)
    }

    pub(crate) fn expect_discrete_real_target(
        &self,
        target: DiscreteRealId<'_>,
        value: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.expect_closed_expression(value, at)?;
        let variable = self
            .variables
            .get(target.index() as usize)
            .ok_or_else(|| unknown("variable", target.index(), at))?;
        if variable.role != VariableRole::DiscreteReal {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable.name.clone(),
                span: at.span(),
            });
        }
        let found = self
            .expressions
            .value_types
            .get(value.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", value.index(), at))?;
        self.expect_value_type_compatible(variable.value_type, found, at)
    }

    pub(crate) fn expect_state_update(
        &self,
        state: StateId<'_>,
        value: ExprId<'_>,
        at: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.expect_closed_expression(value, at)?;
        let variable = self
            .variables
            .get(state.index() as usize)
            .ok_or_else(|| unknown("state", state.index(), at))?;
        if variable.role != VariableRole::State {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable.name.clone(),
                span: at.span(),
            });
        }
        let value_type = self
            .expressions
            .value_types
            .get(value.index() as usize)
            .copied()
            .ok_or_else(|| unknown("expression", value.index(), at))?;
        self.expect_value_type_compatible(variable.value_type, value_type, at)
    }

    pub(crate) fn variable_name(
        &self,
        raw: u32,
        at: DaeProvenance,
    ) -> Result<&VarName, DaeConstructionError> {
        self.variables
            .get(raw as usize)
            .map(|entry| &entry.name)
            .ok_or_else(|| unknown("variable", raw, at))
    }

    pub(crate) fn variable(
        &self,
        raw: u32,
        at: DaeProvenance,
    ) -> Result<&VariableEntry, DaeConstructionError> {
        self.variables
            .get(raw as usize)
            .ok_or_else(|| unknown("variable", raw, at))
    }

    pub(crate) fn total_residual_equation_count(&self) -> usize {
        self.continuous_equations.len()
            + self.initialization_equations.len()
            + self.discrete_real_equations.len()
    }

    pub(super) fn finish_construction(&self) -> Result<(), DaeConstructionError> {
        if self.unfilled_variables != 0 {
            return Err(self.incomplete_arena("variable", &self.variables));
        }
        if self.unfilled_functions != 0 {
            return Err(self.incomplete_arena("function", &self.functions));
        }
        if self.unfilled_function_folds != 0 {
            let (index, fold) = self
                .function_folds
                .iter()
                .enumerate()
                .find(|(_, entry)| entry.update_values.is_empty())
                .expect("private function-fold count matches its arena");
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "function loop transition",
                index: u32::try_from(index)
                    .expect("checked dense arena remains within u32 capacity"),
                span: fold.provenance.span(),
            });
        }
        if self.unfilled_conditions != 0 {
            return Err(self.incomplete_arena("condition", &self.conditions));
        }
        if self.unassigned_discrete_values != 0 {
            let Some((index, variable)) =
                self.variables.iter().enumerate().find(|(index, entry)| {
                    entry.role == VariableRole::DiscreteValue
                        && !self.has_discrete_value_definition(*index)
                })
            else {
                unreachable!("private B.1c definition count diverged from its arena");
            };
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "B.1c target",
                index: u32::try_from(index)
                    .expect("a dense DAE arena cannot exceed its checked u32 capacity"),
                span: variable.declaration.span(),
            });
        }
        self.validate_discrete_dependency_graphs()?;
        Ok(())
    }

    fn validate_discrete_dependency_graphs(&self) -> Result<(), DaeConstructionError> {
        self.validate_discrete_dependency_group(
            self.discrete_assignments
                .iter()
                .map(|entry| (entry.target, entry.value, entry.provenance))
                .collect(),
        )?;
        let mut groups = Vec::new();
        for action in &self.event_actions {
            if matches!(action.kind, EventActionKind::AssignDiscreteValue { .. })
                && !groups.contains(&(action.trigger, action.guard))
            {
                groups.push((action.trigger, action.guard));
            }
        }
        for (trigger, guard) in groups {
            self.validate_discrete_dependency_group(
                self.event_actions
                    .iter()
                    .filter(|entry| entry.trigger == trigger && entry.guard == guard)
                    .filter_map(|entry| match entry.kind {
                        EventActionKind::AssignDiscreteValue { target, value } => {
                            Some((target, value, entry.provenance))
                        }
                        _ => None,
                    })
                    .collect(),
            )?;
        }
        Ok(())
    }

    fn validate_discrete_dependency_group(
        &self,
        assignments: Vec<(u32, u32, DaeProvenance)>,
    ) -> Result<(), DaeConstructionError> {
        for &(target, value, provenance) in &assignments {
            self.validate_discrete_assignment_dependencies(
                &assignments,
                target,
                value,
                provenance,
            )?;
        }
        Ok(())
    }

    fn validate_discrete_assignment_dependencies(
        &self,
        assignments: &[(u32, u32, DaeProvenance)],
        target: u32,
        value: u32,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let mut pending = self.current_discrete_value_dependencies(value, provenance)?;
        let mut visited = vec![false; self.variables.len()];
        while let Some(dependency) = pending.pop() {
            if dependency == target {
                return Err(DaeConstructionError::InvalidDiscreteDependencyCycle {
                    target,
                    span: provenance.span(),
                });
            }
            let Some(seen) = visited.get_mut(dependency as usize) else {
                return Err(unknown("variable", dependency, provenance));
            };
            if std::mem::replace(seen, true) {
                continue;
            }
            let Some((_, dependency_value, _)) = assignments
                .iter()
                .find(|(candidate, _, _)| *candidate == dependency)
            else {
                continue;
            };
            pending
                .extend(self.current_discrete_value_dependencies(*dependency_value, provenance)?);
        }
        Ok(())
    }

    fn current_discrete_value_dependencies(
        &self,
        expression: u32,
        provenance: DaeProvenance,
    ) -> Result<Vec<u32>, DaeConstructionError> {
        let mut dependencies = Vec::new();
        let mut pending = vec![expression];
        let mut visited = vec![false; self.expressions.nodes.len()];
        while let Some(index) = pending.pop() {
            let Some(seen) = visited.get_mut(index as usize) else {
                return Err(unknown("expression", index, provenance));
            };
            if std::mem::replace(seen, true) {
                continue;
            }
            let node = self
                .expressions
                .nodes
                .get(index as usize)
                .ok_or_else(|| unknown("expression", index, provenance))?;
            match node {
                ExprNode::Coordinate(Coordinate::DiscreteValue(variable)) => {
                    dependencies.push(*variable);
                }
                ExprNode::Unary { operand, .. } => pending.push(*operand),
                ExprNode::Binary { lhs, rhs, .. } => pending.extend([*lhs, *rhs]),
                ExprNode::Field { base, .. } => pending.push(*base),
                ExprNode::Comprehension { body, .. } => pending.push(*body),
                ExprNode::Index { base, subscripts } => {
                    pending.push(*base);
                    self.push_subscript_expressions(*subscripts, provenance, &mut pending)?;
                }
                ExprNode::ArrayUpdate {
                    base,
                    value,
                    subscripts,
                } => {
                    pending.extend([*base, *value]);
                    self.push_subscript_expressions(*subscripts, provenance, &mut pending)?;
                }
                ExprNode::Conditional { operands }
                | ExprNode::Array { operands }
                | ExprNode::Record { operands }
                | ExprNode::Builtin { operands, .. }
                | ExprNode::Call { operands, .. } => {
                    pending.extend(
                        self.expressions
                            .operands
                            .get(operands.indices())
                            .ok_or_else(|| unknown("operand range", operands.start, provenance))?,
                    );
                }
                ExprNode::Literal(_)
                | ExprNode::Coordinate(_)
                | ExprNode::Range { .. }
                | ExprNode::FunctionValue { .. }
                | ExprNode::FunctionFoldParameter { .. }
                | ExprNode::FunctionFoldOutput { .. } => {}
            }
        }
        dependencies.sort_unstable();
        dependencies.dedup();
        Ok(dependencies)
    }

    fn push_subscript_expressions(
        &self,
        range: crate::expression::OperandRange,
        provenance: DaeProvenance,
        pending: &mut Vec<u32>,
    ) -> Result<(), DaeConstructionError> {
        for subscript in self
            .expressions
            .subscripts
            .get(range.indices())
            .ok_or_else(|| unknown("subscript range", range.start, provenance))?
        {
            match subscript.kind {
                PackedSubscriptKind::Index(expression) | PackedSubscriptKind::Slice(expression) => {
                    pending.push(expression)
                }
                PackedSubscriptKind::Whole => {}
            }
        }
        Ok(())
    }

    fn has_discrete_value_definition(&self, index: usize) -> bool {
        self.discrete_assignments
            .iter()
            .any(|assignment| assignment.target as usize == index)
            || self.event_actions.iter().any(|action| {
                matches!(
                    action.kind,
                    EventActionKind::AssignDiscreteValue { target, .. }
                        if target as usize == index
                )
            })
    }

    fn incomplete_arena<T: DeclaredEntry>(
        &self,
        kind: &'static str,
        entries: &[T],
    ) -> DaeConstructionError {
        let Some((index, entry)) = entries
            .iter()
            .enumerate()
            .find(|(_, entry)| !entry.is_complete())
        else {
            unreachable!("private forward-definition count diverged from its arena");
        };
        DaeConstructionError::IncompleteDefinition {
            kind,
            index: u32::try_from(index)
                .expect("a dense DAE arena cannot exceed its checked u32 capacity"),
            span: entry.declaration().span(),
        }
    }

    pub(crate) fn value_type_at(
        &self,
        raw: u32,
        at: DaeProvenance,
    ) -> Result<&ValueType, DaeConstructionError> {
        self.value_types
            .get(raw as usize)
            .ok_or_else(|| unknown("value type", raw, at))
    }

    pub(super) fn expression_at(
        &self,
        raw: u32,
        at: DaeProvenance,
    ) -> Result<&ExprNode, DaeConstructionError> {
        self.expressions
            .nodes
            .get(raw as usize)
            .ok_or_else(|| unknown("expression", raw, at))
    }
}
