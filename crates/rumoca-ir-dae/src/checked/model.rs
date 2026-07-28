use std::marker::PhantomData;

use rumoca_core::{ComponentReference, SourceMap, StateSelect, VarName};
use serde::{Deserialize, Serialize};

use super::expression::{
    Coordinate, CoordinateInput, ExprNode, ExpressionArenaStorage, Expressions, ValueType,
    source_text,
};
use super::{
    AlgebraicId, BinaryOperator, ConditionId, DaeConstructionError, DaeGeneration, DaeProvenance,
    DiscreteRealId, DiscreteValueId, DomainId, EquationId, ExprId, FunctionId, InputId,
    ParameterId, ScalarType, StateId, ValueTypeId, VariableId,
};

pub const CHECKED_DAE_SCHEMA_VERSION: u16 = 11;

mod wire;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct VariableEntry {
    name: VarName,
    role: VariableRole,
    value_type: u32,
    declaration: DaeProvenance,
    attributes: Option<VariableAttributesWire>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct VariableAttributesWire {
    component_ref: Option<ComponentReference>,
    start: Option<u32>,
    fixed: Option<bool>,
    min: Option<u32>,
    max: Option<u32>,
    nominal: Option<u32>,
    unit: Option<String>,
    state_select: StateSelect,
    description: Option<String>,
    causality: VariableCausality,
    is_tunable: bool,
    origin: VariableOrigin,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableRole {
    Parameter,
    Constant,
    Input,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableCausality {
    Input,
    Output,
    Parameter,
    CalculatedParameter,
    Independent,
    #[default]
    Local,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableOrigin {
    #[default]
    Source,
    Generated,
}

#[derive(Debug, Clone, Default)]
pub struct VariableAttributes<'dae> {
    pub component_ref: Option<ComponentReference>,
    pub start: Option<ExprId<'dae>>,
    pub fixed: Option<bool>,
    pub min: Option<ExprId<'dae>>,
    pub max: Option<ExprId<'dae>>,
    pub nominal: Option<ExprId<'dae>>,
    pub unit: Option<String>,
    pub state_select: StateSelect,
    pub description: Option<String>,
    pub causality: VariableCausality,
    pub is_tunable: bool,
    pub origin: VariableOrigin,
}

/// Linear authority to attach forward-referencing variable attributes.
///
/// The token is branded, non-cloneable, and consumed by [`Variables::define`].
pub struct VariableReservation<'dae> {
    variable: VariableId<'dae>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionEntry {
    name: VarName,
    parameters: Vec<u32>,
    results: Vec<u32>,
    declaration: DaeProvenance,
    definition: Option<FunctionDefinitionWire>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct FunctionDefinitionWire {
    results: Vec<u32>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct DomainEntry {
    extent: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct EquationEntry {
    residual: u32,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ConditionEntry {
    expression: Option<u32>,
    provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub(crate) struct Storage {
    pub(crate) value_types: Vec<ValueType>,
    value_type_provenance: Vec<DaeProvenance>,
    variables: Vec<VariableEntry>,
    functions: Vec<FunctionEntry>,
    domains: Vec<DomainEntry>,
    pub(crate) expressions: ExpressionArenaStorage,
    equations: Vec<EquationEntry>,
    conditions: Vec<ConditionEntry>,
    #[serde(skip)]
    unfilled_variables: usize,
    #[serde(skip)]
    unfilled_functions: usize,
    #[serde(skip)]
    unfilled_conditions: usize,
}

impl Storage {
    pub(crate) fn intern_type<'dae>(
        &mut self,
        ty: ValueType,
        at: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        if let Some(index) = self
            .value_types
            .iter()
            .position(|candidate| candidate == &ty)
        {
            return Ok(ValueTypeId::from_raw(index as u32));
        }
        let raw = checked_u32(self.value_types.len(), "value type arena", at)?;
        self.value_types.push(ty);
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

    pub(crate) fn coordinate_type<'dae>(
        &mut self,
        coordinate: CoordinateInput<'dae>,
        at: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        let raw = match coordinate {
            CoordinateInput::Parameter(id) => self.coordinate_variable_type(
                id.index(),
                &[VariableRole::Parameter, VariableRole::Constant],
                at,
            )?,
            CoordinateInput::Input(id) => {
                self.coordinate_variable_type(id.index(), &[VariableRole::Input], at)?
            }
            CoordinateInput::State(id) | CoordinateInput::Derivative(id) => {
                self.coordinate_variable_type(id.index(), &[VariableRole::State], at)?
            }
            CoordinateInput::Algebraic(id) => self.coordinate_variable_type(
                id.index(),
                &[VariableRole::Algebraic, VariableRole::Output],
                at,
            )?,
            CoordinateInput::DiscreteReal(id)
            | CoordinateInput::PreDiscreteReal(id)
            | CoordinateInput::Previous(id) => {
                self.coordinate_variable_type(id.index(), &[VariableRole::DiscreteReal], at)?
            }
            CoordinateInput::DiscreteValue(id) | CoordinateInput::PreDiscreteValue(id) => {
                self.coordinate_variable_type(id.index(), &[VariableRole::DiscreteValue], at)?
            }
            CoordinateInput::Time | CoordinateInput::Delay(_) => {
                return self.intern_type(ValueType::scalar(ScalarType::Real), at);
            }
            CoordinateInput::Condition(id) => {
                if self.conditions.get(id.index() as usize).is_none() {
                    return Err(unknown("condition", id.index(), at));
                }
                return self.intern_type(ValueType::scalar(ScalarType::Boolean), at);
            }
            CoordinateInput::Terminal => {
                return self.intern_type(ValueType::scalar(ScalarType::Boolean), at);
            }
        };
        Ok(ValueTypeId::from_raw(raw))
    }

    fn coordinate_variable_type(
        &self,
        raw: u32,
        roles: &[VariableRole],
        at: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
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
        Ok(variable.value_type)
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

    pub(crate) fn domain_extent<'dae>(
        &self,
        domain: DomainId<'dae>,
        at: DaeProvenance,
    ) -> Result<u32, DaeConstructionError> {
        self.domains
            .get(domain.index() as usize)
            .map(|domain| domain.extent)
            .ok_or_else(|| unknown("domain", domain.index(), at))
    }

    fn finish_construction(&self) -> Result<(), DaeConstructionError> {
        if self.unfilled_variables != 0 {
            return Err(self.incomplete_arena("variable", &self.variables));
        }
        if self.unfilled_functions != 0 {
            return Err(self.incomplete_arena("function", &self.functions));
        }
        if self.unfilled_conditions != 0 {
            return Err(self.incomplete_arena("condition", &self.conditions));
        }
        Ok(())
    }

    fn incomplete_arena<T: DeclaredEntry>(
        &self,
        kind: &'static str,
        entries: &[T],
    ) -> DaeConstructionError {
        let index = entries.len().saturating_sub(1);
        let span = entries
            .last()
            .map_or(rumoca_core::Span::DUMMY, |entry| entry.declaration().span());
        DaeConstructionError::IncompleteDefinition {
            kind,
            index: u32::try_from(index).unwrap_or(u32::MAX),
            span,
        }
    }

    fn validate_columns(&self) -> Result<(), DaeConstructionError> {
        let len = self.expressions.nodes.len();
        if self.expressions.provenance.len() != len
            || self.expressions.value_types.len() != len
            || self.value_type_provenance.len() != self.value_types.len()
        {
            let at = self
                .expressions
                .provenance
                .first()
                .copied()
                .or_else(|| self.variables.first().map(|entry| entry.declaration));
            return Err(DaeConstructionError::CapacityExceeded {
                arena: "misaligned expression columns",
                attempted_index: len,
                span: at.map_or(rumoca_core::Span::DUMMY, DaeProvenance::span),
            });
        }
        Ok(())
    }

    fn value_type_at(
        &self,
        raw: u32,
        at: DaeProvenance,
    ) -> Result<&ValueType, DaeConstructionError> {
        self.value_types
            .get(raw as usize)
            .ok_or_else(|| unknown("value type", raw, at))
    }

    fn expression_at(
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

/// Immutable, checked schema-v11 DAE prototype.
#[derive(Debug, Clone, Serialize)]
pub struct Dae {
    schema_version: u16,
    source_map: SourceMap,
    storage: Storage,
}

impl Dae {
    /// Construct a DAE through a fresh, generative ownership brand.
    ///
    /// The higher-ranked closure prevents any arena ID from escaping. Semantic
    /// owner closures borrow this one aggregate sequentially.
    pub fn construct<F>(source_map: SourceMap, build: F) -> Result<Self, DaeConstructionError>
    where
        F: for<'dae> FnOnce(&mut DaeConstruction<'dae>) -> Result<(), DaeConstructionError>,
    {
        let mut storage = Storage::default();
        {
            let mut construction = DaeConstruction {
                source_map: &source_map,
                storage: &mut storage,
                marker: PhantomData,
            };
            build(&mut construction)?;
        }
        storage.finish_construction()?;
        Ok(Self {
            schema_version: CHECKED_DAE_SCHEMA_VERSION,
            source_map,
            storage,
        })
    }

    pub const fn schema_version(&self) -> u16 {
        self.schema_version
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.source_map
    }

    pub fn source_text(&self, provenance: DaeProvenance) -> Option<&str> {
        source_text(&self.source_map, provenance)
    }

    /// Inspect the finalized DAE through a fresh brand.
    pub fn inspect<R>(&self, inspect: impl for<'dae> FnOnce(DaeView<'dae>) -> R) -> R {
        inspect(DaeView {
            dae: self,
            marker: PhantomData,
        })
    }
}

/// The single mutable aggregate lent to semantic owner closures.
pub struct DaeConstruction<'dae> {
    source_map: &'dae SourceMap,
    storage: &'dae mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DaeConstruction<'dae> {
    pub fn types<R>(
        &mut self,
        build: impl FnOnce(&mut ValueTypes<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut ValueTypes {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn variables<R>(
        &mut self,
        build: impl FnOnce(&mut Variables<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Variables {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn functions<R>(
        &mut self,
        build: impl FnOnce(&mut Functions<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Functions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn domains<R>(
        &mut self,
        build: impl FnOnce(&mut Domains<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Domains {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn expressions<R>(
        &mut self,
        build: impl FnOnce(&mut Expressions<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Expressions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }

    pub fn equation(
        &mut self,
        owner: DaeProvenance,
        build: impl FnOnce(&mut Equation<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<EquationId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, owner)?;
        let mut equation = Equation {
            source_map: self.source_map,
            storage: self.storage,
            owner,
            residual: None,
            marker: PhantomData,
        };
        build(&mut equation)?;
        let Some(residual) = equation.residual else {
            return Err(DaeConstructionError::IncompleteDefinition {
                kind: "equation residual",
                index: equation.storage.equations.len() as u32,
                span: owner.span(),
            });
        };
        let raw = checked_u32(equation.storage.equations.len(), "equation arena", owner)?;
        equation.storage.equations.push(EquationEntry {
            residual,
            provenance: owner,
        });
        Ok(EquationId::from_raw(raw))
    }

    pub fn conditions<R>(
        &mut self,
        build: impl FnOnce(&mut Conditions<'_, 'dae>) -> Result<R, DaeConstructionError>,
    ) -> Result<R, DaeConstructionError> {
        build(&mut Conditions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        })
    }
}

pub struct ValueTypes<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ValueTypes<'_, 'dae> {
    pub fn intern(
        &mut self,
        ty: ValueType,
        provenance: DaeProvenance,
    ) -> Result<ValueTypeId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        self.storage.intern_type(ty, provenance)
    }
}

pub struct Variables<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Variables<'_, 'dae> {
    pub fn parameter(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<ParameterId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Parameter,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| ParameterId::from_raw(id.index()))
    }

    pub fn constant(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<ParameterId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Constant,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| ParameterId::from_raw(id.index()))
    }

    pub fn input(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<InputId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Input,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| InputId::from_raw(id.index()))
    }

    pub fn state(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<StateId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::State,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| StateId::from_raw(id.index()))
    }

    pub fn algebraic(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<AlgebraicId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Algebraic,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| AlgebraicId::from_raw(id.index()))
    }

    pub fn output(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<AlgebraicId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::Output,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| AlgebraicId::from_raw(id.index()))
    }

    pub fn discrete_real(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<DiscreteRealId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::DiscreteReal,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| DiscreteRealId::from_raw(id.index()))
    }

    pub fn discrete_value(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<DiscreteValueId<'dae>, DaeConstructionError> {
        self.add_complete(
            name,
            VariableRole::DiscreteValue,
            value_type,
            declaration,
            attributes,
        )
        .map(|id| DiscreteValueId::from_raw(id.index()))
    }

    pub fn reserve_algebraic(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(AlgebraicId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(name, VariableRole::Algebraic, value_type, declaration)?;
        Ok((
            AlgebraicId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
    }

    pub fn reserve_parameter(
        &mut self,
        name: VarName,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<(ParameterId<'dae>, VariableReservation<'dae>), DaeConstructionError> {
        let id = self.reserve_forward(name, VariableRole::Parameter, value_type, declaration)?;
        Ok((
            ParameterId::from_raw(id.index()),
            VariableReservation { variable: id },
        ))
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
        let Some(entry) = self.storage.variables.get_mut(variable.index() as usize) else {
            return Err(unknown("variable", variable.index(), provenance));
        };
        if entry.attributes.is_some() {
            return Err(duplicate("variable", variable.index(), provenance));
        }
        entry.attributes = Some(erase_variable_attributes(attributes));
        self.storage.unfilled_variables -= 1;
        Ok(())
    }

    fn add_complete(
        &mut self,
        name: VarName,
        role: VariableRole,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
        attributes: VariableAttributes<'dae>,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
        let id = self.reserve_forward(name, role, value_type, declaration)?;
        self.validate_attributes(id, &attributes, declaration)?;
        self.storage.variables[id.index() as usize].attributes =
            Some(erase_variable_attributes(attributes));
        self.storage.unfilled_variables -= 1;
        Ok(id)
    }

    fn reserve_forward(
        &mut self,
        name: VarName,
        role: VariableRole,
        value_type: ValueTypeId<'dae>,
        declaration: DaeProvenance,
    ) -> Result<VariableId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, declaration)?;
        self.storage
            .value_type_at(value_type.index(), declaration)?;
        if self
            .storage
            .variables
            .iter()
            .any(|entry| entry.name == name)
        {
            return Err(DaeConstructionError::DuplicateKey {
                kind: "variable",
                key: name.to_string(),
                span: declaration.span(),
            });
        }
        let raw = checked_u32(self.storage.variables.len(), "variable arena", declaration)?;
        self.storage.variables.push(VariableEntry {
            name,
            role,
            value_type: value_type.index(),
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
        for expression in [
            attributes.start,
            attributes.min,
            attributes.max,
            attributes.nominal,
        ]
        .into_iter()
        .flatten()
        {
            let found = self
                .storage
                .expressions
                .value_types
                .get(expression.index() as usize)
                .copied()
                .ok_or_else(|| unknown("expression", expression.index(), provenance))?;
            if found != expected {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: provenance.span(),
                });
            }
        }
        Ok(())
    }
}

fn erase_variable_attributes(attributes: VariableAttributes<'_>) -> VariableAttributesWire {
    VariableAttributesWire {
        component_ref: attributes.component_ref,
        start: attributes.start.map(ExprId::index),
        fixed: attributes.fixed,
        min: attributes.min.map(ExprId::index),
        max: attributes.max.map(ExprId::index),
        nominal: attributes.nominal.map(ExprId::index),
        unit: attributes.unit,
        state_select: attributes.state_select,
        description: attributes.description,
        causality: attributes.causality,
        is_tunable: attributes.is_tunable,
        origin: attributes.origin,
    }
}

pub struct Functions<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Functions<'_, 'dae> {
    pub fn reserve_recursive(
        &mut self,
        name: VarName,
        parameters: impl IntoIterator<Item = ValueTypeId<'dae>>,
        results: impl IntoIterator<Item = ValueTypeId<'dae>>,
        declaration: DaeProvenance,
    ) -> Result<FunctionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, declaration)?;
        let parameters = parameters
            .into_iter()
            .map(ValueTypeId::index)
            .collect::<Vec<_>>();
        let results = results
            .into_iter()
            .map(ValueTypeId::index)
            .collect::<Vec<_>>();
        for &ty in parameters.iter().chain(&results) {
            self.storage.value_type_at(ty, declaration)?;
        }
        let raw = checked_u32(self.storage.functions.len(), "function arena", declaration)?;
        self.storage.functions.push(FunctionEntry {
            name,
            parameters,
            results,
            declaration,
            definition: None,
        });
        self.storage.unfilled_functions += 1;
        Ok(FunctionId::from_raw(raw))
    }

    pub fn define(
        &mut self,
        function: FunctionId<'dae>,
        results: impl IntoIterator<Item = ExprId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let results = results.into_iter().map(ExprId::index).collect::<Vec<_>>();
        let expected = self
            .storage
            .functions
            .get(function.index() as usize)
            .map(|entry| entry.results.clone())
            .ok_or_else(|| unknown("function", function.index(), provenance))?;
        validate_function_results(self.storage, &expected, &results, provenance)?;
        let Some(entry) = self.storage.functions.get_mut(function.index() as usize) else {
            return Err(unknown("function", function.index(), provenance));
        };
        if entry.definition.is_some() {
            return Err(duplicate("function", function.index(), provenance));
        }
        if entry.results.len() != results.len() {
            return Err(DaeConstructionError::InvalidArity {
                expected: entry.results.len(),
                found: results.len(),
                span: provenance.span(),
            });
        }
        entry.definition = Some(FunctionDefinitionWire { results });
        self.storage.unfilled_functions -= 1;
        Ok(())
    }
}

pub struct Domains<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Domains<'_, 'dae> {
    pub fn compact(
        &mut self,
        extent: u32,
        provenance: DaeProvenance,
    ) -> Result<DomainId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let raw = checked_u32(self.storage.domains.len(), "domain arena", provenance)?;
        self.storage
            .domains
            .push(DomainEntry { extent, provenance });
        Ok(DomainId::from_raw(raw))
    }
}

pub struct Equation<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    owner: DaeProvenance,
    residual: Option<u32>,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Equation<'_, 'dae> {
    pub fn expressions(&mut self) -> Expressions<'_, 'dae> {
        Expressions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        }
    }

    pub fn residual(&mut self, residual: ExprId<'dae>) -> Result<(), DaeConstructionError> {
        if self.residual.is_some() {
            return Err(duplicate(
                "equation residual",
                self.storage.equations.len() as u32,
                self.owner,
            ));
        }
        let ty = self.storage.expr_type(residual, self.owner)?;
        if !ty.is_scalar() || ty.scalar_type() != ScalarType::Real {
            return Err(DaeConstructionError::TypeMismatch {
                expected: ScalarType::Real,
                found: ty.scalar_type(),
                span: self.owner.span(),
            });
        }
        self.residual = Some(residual.index());
        Ok(())
    }

    pub fn equal(
        &mut self,
        lhs: ExprId<'dae>,
        rhs: ExprId<'dae>,
    ) -> Result<ExprId<'dae>, DaeConstructionError> {
        let provenance =
            DaeProvenance::generated(DaeGeneration::SyntheticResidual, self.owner.span())?;
        check_provenance(self.source_map, provenance)?;
        let residual =
            self.expressions()
                .at(provenance)
                .binary(BinaryOperator::Subtract, lhs, rhs)?;
        self.residual(residual)?;
        Ok(residual)
    }
}

pub struct Conditions<'storage, 'dae> {
    source_map: &'storage SourceMap,
    storage: &'storage mut Storage,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> Conditions<'_, 'dae> {
    pub fn reserve(
        &mut self,
        provenance: DaeProvenance,
    ) -> Result<ConditionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let raw = checked_u32(self.storage.conditions.len(), "condition arena", provenance)?;
        self.storage.conditions.push(ConditionEntry {
            expression: None,
            provenance,
        });
        self.storage.unfilled_conditions += 1;
        Ok(ConditionId::from_raw(raw))
    }

    pub fn define(
        &mut self,
        condition: ConditionId<'dae>,
        expression: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let ty = self.storage.expr_type(expression, provenance)?;
        if !ty.is_scalar() || ty.scalar_type() != ScalarType::Boolean {
            return Err(DaeConstructionError::TypeMismatch {
                expected: ScalarType::Boolean,
                found: ty.scalar_type(),
                span: provenance.span(),
            });
        }
        let Some(entry) = self.storage.conditions.get_mut(condition.index() as usize) else {
            return Err(unknown("condition", condition.index(), provenance));
        };
        if entry.expression.is_some() {
            return Err(duplicate("condition", condition.index(), provenance));
        }
        entry.expression = Some(expression.index());
        self.storage.unfilled_conditions -= 1;
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub struct DaeView<'dae> {
    dae: &'dae Dae,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DaeView<'dae> {
    pub fn expression_count(self) -> usize {
        self.dae.storage.expressions.nodes.len()
    }

    pub fn variable_count(self) -> usize {
        self.dae.storage.variables.len()
    }

    pub fn equation_count(self) -> usize {
        self.dae.storage.equations.len()
    }

    pub fn expression_id(self, index: usize) -> Option<ExprId<'dae>> {
        self.dae
            .storage
            .expressions
            .nodes
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(ExprId::from_raw)
    }

    pub fn variable_id(self, index: usize) -> Option<VariableId<'dae>> {
        self.dae
            .storage
            .variables
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(VariableId::from_raw)
    }

    pub fn variable_declaration(self, id: VariableId<'dae>) -> Option<DaeProvenance> {
        self.dae
            .storage
            .variables
            .get(id.index() as usize)
            .map(|variable| variable.declaration)
    }

    pub fn expression(self, id: ExprId<'dae>) -> Option<ExpressionView<'dae>> {
        let index = id.index() as usize;
        Some(ExpressionView {
            node: self.dae.storage.expressions.nodes.get(index)?,
            provenance: *self.dae.storage.expressions.provenance.get(index)?,
            value_type: self
                .dae
                .storage
                .value_types
                .get(*self.dae.storage.expressions.value_types.get(index)? as usize)?,
        })
    }

    pub fn equation(self, index: usize) -> Option<EquationView<'dae>> {
        let equation = self.dae.storage.equations.get(index)?;
        Some(EquationView {
            residual: ExprId::from_raw(equation.residual),
            provenance: equation.provenance,
        })
    }

    pub fn subscript_provenance(self, index: usize) -> Option<DaeProvenance> {
        self.dae
            .storage
            .expressions
            .subscripts
            .get(index)
            .map(|subscript| subscript.provenance)
    }

    pub fn source_text(self, provenance: DaeProvenance) -> Option<&'dae str> {
        source_text(&self.dae.source_map, provenance)
    }
}

#[derive(Clone, Copy)]
pub struct ExpressionView<'dae> {
    node: &'dae ExprNode,
    provenance: DaeProvenance,
    value_type: &'dae ValueType,
}

impl<'dae> ExpressionView<'dae> {
    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }

    pub const fn value_type(self) -> &'dae ValueType {
        self.value_type
    }

    pub fn kind(self) -> ExpressionKind {
        match self.node {
            ExprNode::Literal(_) => ExpressionKind::Literal,
            ExprNode::Coordinate(_) => ExpressionKind::Coordinate,
            ExprNode::Unary { .. } => ExpressionKind::Unary,
            ExprNode::Binary { .. } => ExpressionKind::Binary,
            ExprNode::Conditional { .. } => ExpressionKind::Conditional,
            ExprNode::Array { .. } => ExpressionKind::Array,
            ExprNode::Range { .. } => ExpressionKind::Range,
            ExprNode::Comprehension { .. } => ExpressionKind::Comprehension,
            ExprNode::Index { .. } => ExpressionKind::Index,
            ExprNode::Builtin { .. } => ExpressionKind::Builtin,
            ExprNode::Call { .. } => ExpressionKind::Call,
        }
    }

    pub fn variable_coordinate(self) -> Option<VariableId<'dae>> {
        match self.node {
            ExprNode::Coordinate(
                Coordinate::Parameter(variable)
                | Coordinate::Input(variable)
                | Coordinate::State(variable)
                | Coordinate::Derivative(variable)
                | Coordinate::Algebraic(variable)
                | Coordinate::DiscreteReal(variable)
                | Coordinate::DiscreteValue(variable)
                | Coordinate::PreDiscreteReal(variable)
                | Coordinate::PreDiscreteValue(variable)
                | Coordinate::Previous(variable),
            ) => Some(VariableId::from_raw(*variable)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExpressionKind {
    Literal,
    Coordinate,
    Unary,
    Binary,
    Conditional,
    Array,
    Range,
    Comprehension,
    Index,
    Builtin,
    Call,
}

#[derive(Clone, Copy)]
pub struct EquationView<'dae> {
    residual: ExprId<'dae>,
    provenance: DaeProvenance,
}

impl<'dae> EquationView<'dae> {
    pub const fn residual(self) -> ExprId<'dae> {
        self.residual
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

fn validate_function_results(
    storage: &Storage,
    expected: &[u32],
    results: &[u32],
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if expected.len() != results.len() {
        return Err(DaeConstructionError::InvalidArity {
            expected: expected.len(),
            found: results.len(),
            span: at.span(),
        });
    }
    for (&result, &expected_type) in results.iter().zip(expected) {
        storage.expression_at(result, at)?;
        if storage.expressions.value_types[result as usize] != expected_type {
            return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
        }
    }
    Ok(())
}

pub(crate) fn check_provenance(
    source_map: &SourceMap,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let span = provenance.span();
    let Some((_, source)) = source_map.get_source(span.source) else {
        return Err(DaeConstructionError::UnknownSource { span });
    };
    let range = span.start.0..span.end.0;
    if range.start > range.end
        || range.end > source.len()
        || !source.is_char_boundary(range.start)
        || !source.is_char_boundary(range.end)
    {
        return Err(DaeConstructionError::InvalidSourceRange {
            span,
            source_len: source.len(),
        });
    }
    Ok(())
}

fn checked_u32(
    value: usize,
    arena: &'static str,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    u32::try_from(value).map_err(|_| DaeConstructionError::CapacityExceeded {
        arena,
        attempted_index: value,
        span: at.span(),
    })
}

fn unknown(kind: &'static str, index: u32, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::UnknownId {
        kind,
        index,
        span: at.span(),
    }
}

fn duplicate(kind: &'static str, index: u32, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::DuplicateDefinition {
        kind,
        index,
        span: at.span(),
    }
}

fn incomplete(kind: &'static str, index: usize, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::IncompleteDefinition {
        kind,
        index: u32::try_from(index).unwrap_or(u32::MAX),
        span: at.span(),
    }
}

trait DeclaredEntry {
    fn declaration(&self) -> DaeProvenance;
}

impl DeclaredEntry for VariableEntry {
    fn declaration(&self) -> DaeProvenance {
        self.declaration
    }
}

impl DeclaredEntry for FunctionEntry {
    fn declaration(&self) -> DaeProvenance {
        self.declaration
    }
}

impl DeclaredEntry for ConditionEntry {
    fn declaration(&self) -> DaeProvenance {
        self.provenance
    }
}
