use std::marker::PhantomData;

use rumoca_core::{ComprehensionScalarView, StructuredIndexBinder, StructuredIndexDomain};

use crate::conditions::condition_owner_clock;
use crate::expression::{Coordinate, ExprNode};
use crate::model::{
    Storage, check_provenance, checked_u32, duplicate, insert_domain, invalid_arity,
};
use crate::{
    BinaryOperator, ConditionId, ContinuousEquationId, ContinuousFamilyId, DaeConstructionError,
    DaeGeneration, DaeProvenance, DiscreteRealEquationId, DiscreteRealId, DiscreteValueId,
    DomainId, ExprId, InitialDiscreteValueId, InitializationEquationId, InitializationFamilyId,
    ScalarType, VariableId, VariableRole,
};

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ResidualEquationEntry {
    pub(crate) residual: u32,
    pub(crate) provenance: DaeProvenance,
}

/// Which residual shapes one equation partition admits.
///
/// Continuous and initialization rows are scalar: an array equation reaches
/// those partitions only through [`build_value_equation`], which projects it
/// onto a structured family and keeps every row scalar. The discrete Real
/// partition owns no family arena because an MLS §8.5 event assignment defines
/// the whole array at one event instant, so its residual keeps the target's
/// shape and Solve scalarizes the row.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ResidualShape {
    Scalar,
    TargetShaped,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum DiscreteRealActivationEntry {
    Always,
    When { trigger: u32, guard: u32 },
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DiscreteRealEquationEntry {
    pub(crate) residual: u32,
    pub(crate) activation: DiscreteRealActivationEntry,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiscreteRealActivation<'dae> {
    Always,
    When {
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
    },
}

#[derive(Clone, Copy)]
pub struct DiscreteRealEquationView<'dae> {
    pub(crate) residual: ExprId<'dae>,
    pub(crate) activation: DiscreteRealActivation<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> DiscreteRealEquationView<'dae> {
    pub const fn residual(self) -> ExprId<'dae> {
        self.residual
    }

    pub const fn activation(self) -> DiscreteRealActivation<'dae> {
        self.activation
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FamilyBodyRange {
    pub(crate) start: u32,
    pub(crate) len: u32,
}

impl FamilyBodyRange {
    pub(crate) fn indices(self) -> std::ops::Range<usize> {
        let start = self.start as usize;
        start..start + self.len as usize
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct StructuredFamilyEntry {
    pub(crate) domain: u32,
    pub(crate) scalar_view: ComprehensionScalarView,
    pub(crate) bodies: FamilyBodyRange,
    #[serde(skip_serializing)]
    pub(crate) scalar_rows: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum EquationOwnerEntry {
    Residual(u32),
    Structured(u32),
}

trait ResidualPartition {
    fn insert(
        storage: &mut Storage,
        entry: ResidualEquationEntry,
        owner: DaeProvenance,
    ) -> Result<u32, DaeConstructionError>;
}

trait StructuredPartition: ResidualPartition {
    fn insert_family(
        storage: &mut Storage,
        entry: StructuredFamilyEntry,
        owner: DaeProvenance,
    ) -> Result<u32, DaeConstructionError>;
}

macro_rules! equation_partitions {
    (
        $(
            structured $partition:ident, $scope:ident {
                residual $equation:ident -> $equation_id:ident, $equations:ident;
                family $value_equation:ident, $family:ident
                    -> $family_id:ident, $families:ident, $owners:ident;
            }
        )*
    ) => {
        $(
            struct $partition;

            impl ResidualPartition for $partition {
                fn insert(
                    storage: &mut Storage,
                    entry: ResidualEquationEntry,
                    owner: DaeProvenance,
                ) -> Result<u32, DaeConstructionError> {
                    let raw = push_dense(&mut storage.$equations, entry, "equation arena", owner)?;
                    storage.$owners.push(EquationOwnerEntry::Residual(raw));
                    Ok(raw)
                }
            }

            impl StructuredPartition for $partition {
                fn insert_family(
                    storage: &mut Storage,
                    entry: StructuredFamilyEntry,
                    owner: DaeProvenance,
                ) -> Result<u32, DaeConstructionError> {
                    let raw = push_dense(
                        &mut storage.$families,
                        entry,
                        "structured equation family arena",
                        owner,
                    )?;
                    storage.$owners.push(EquationOwnerEntry::Structured(raw));
                    Ok(raw)
                }
            }

            pub struct $scope<'storage, 'dae> {
                pub(crate) source_map: &'storage rumoca_core::SourceMap,
                pub(crate) storage: &'storage mut Storage,
                pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
            }

            impl<'dae> $scope<'_, 'dae> {
                pub fn $value_equation(
                    &mut self,
                    owner: DaeProvenance,
                    residual: ExprId<'dae>,
                ) -> Result<(), DaeConstructionError> {
                    build_value_equation::<$partition>(
                        self.source_map,
                        self.storage,
                        owner,
                        residual,
                    )
                }

                pub fn $equation(
                    &mut self,
                    owner: DaeProvenance,
                    build: impl FnOnce(
                        &mut ResidualEquation<'_, 'dae>,
                    ) -> Result<(), DaeConstructionError>,
                ) -> Result<$equation_id<'dae>, DaeConstructionError> {
                    build_residual::<$partition>(self.source_map, self.storage, owner, build)
                        .map($equation_id::from_raw)
                }

                pub fn $family(
                    &mut self,
                    owner: DaeProvenance,
                    domain: DomainId<'dae>,
                    scalar_view: ComprehensionScalarView,
                    build: impl FnOnce(
                        &mut StructuredResiduals<'_, 'dae>,
                    ) -> Result<(), DaeConstructionError>,
                ) -> Result<$family_id<'dae>, DaeConstructionError> {
                    build_structured_family::<$partition>(
                        self.source_map,
                        self.storage,
                        owner,
                        domain,
                        scalar_view,
                        build,
                    )
                    .map($family_id::from_raw)
                }
            }
        )*
    };
}

equation_partitions! {
    structured ContinuousPartition, ContinuousEquations {
        residual equation -> ContinuousEquationId, continuous_equations;
        family value_equation, structured_family
            -> ContinuousFamilyId, continuous_families, continuous_equation_owners;
    }
    structured InitializationPartition, InitializationEquations {
        residual equation -> InitializationEquationId, initialization_equations;
        family value_equation, structured_family
            -> InitializationFamilyId, initialization_families, initialization_equation_owners;
    }
}

/// MLS §8.6 initial value of one discrete coordinate.
///
/// An `initial algorithm` assigns a discrete-time variable; the declarative
/// meaning of that assignment is the value the coordinate holds when
/// initialization finishes. That is a definition, not a residual: nothing
/// solves for it, and the equation-section owner that defines the coordinate
/// afterwards (an MLS §8.5 `when`) is a different partition. The initialization
/// system therefore owns it as a target/value pair.
#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct InitialDiscreteValueEntry {
    pub(crate) target: u32,
    pub(crate) value: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub struct InitialDiscreteValueView<'dae> {
    pub(crate) target: VariableId<'dae>,
    pub(crate) value: ExprId<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> InitialDiscreteValueView<'dae> {
    pub const fn target(self) -> VariableId<'dae> {
        self.target
    }

    pub const fn value(self) -> ExprId<'dae> {
        self.value
    }

    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> InitializationEquations<'_, 'dae> {
    /// Define the initialization-instant value of one discrete Real coordinate.
    pub fn discrete_real_initial_value(
        &mut self,
        target: DiscreteRealId<'dae>,
        value: ExprId<'dae>,
        owner: DaeProvenance,
    ) -> Result<InitialDiscreteValueId<'dae>, DaeConstructionError> {
        insert_initial_discrete_value(
            self.source_map,
            self.storage,
            target.index(),
            VariableRole::DiscreteReal,
            value,
            owner,
        )
    }

    /// Define the initialization-instant value of one discrete-valued coordinate.
    pub fn discrete_value_initial_value(
        &mut self,
        target: DiscreteValueId<'dae>,
        value: ExprId<'dae>,
        owner: DaeProvenance,
    ) -> Result<InitialDiscreteValueId<'dae>, DaeConstructionError> {
        insert_initial_discrete_value(
            self.source_map,
            self.storage,
            target.index(),
            VariableRole::DiscreteValue,
            value,
            owner,
        )
    }
}

/// Insert one checked discrete initial-value definition.
///
/// The local integrity this checks is what makes an unsettled read
/// unrepresentable rather than merely unlikely: the initialization update rows
/// are applied at the known initialization instant, before any trajectory
/// exists, so the defining value may read only coordinates that are already
/// settled there — parameters, constants, and `time`. A read of a state,
/// algebraic, output, input, `pre`, delay, or any other discrete coordinate has
/// no proven evaluation order at that instant, so it fails here instead of
/// producing a numerically plausible but unproven initial value.
fn insert_initial_discrete_value<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    target: u32,
    expected_role: VariableRole,
    value: ExprId<'dae>,
    owner: DaeProvenance,
) -> Result<InitialDiscreteValueId<'dae>, DaeConstructionError> {
    check_provenance(source_map, owner)?;
    let variable = storage.variable(target, owner)?;
    if variable.role != expected_role {
        return Err(DaeConstructionError::InvalidVariableRole {
            name: variable.name.clone(),
            span: owner.span(),
        });
    }
    let declared = storage
        .value_types
        .get(variable.value_type as usize)
        .ok_or_else(|| crate::model::unknown("value type", variable.value_type, owner))?
        .clone();
    storage.expect_closed_expression(value, owner)?;
    let found = storage.expr_type(value, owner)?;
    if found.scalar_type() != declared.scalar_type() {
        return Err(DaeConstructionError::TypeMismatch {
            expected: declared.scalar_type(),
            found: found.scalar_type(),
            span: owner.span(),
        });
    }
    if !declared.is_scalar() || !found.is_scalar() {
        return Err(DaeConstructionError::ExpectedScalar { span: owner.span() });
    }
    expect_initialization_settled_reads(storage, value, owner)?;
    if storage
        .initial_discrete_value_by_variable
        .contains_key(&target)
    {
        return Err(duplicate("discrete initial value", target, owner));
    }
    let raw = push_dense(
        &mut storage.initial_discrete_values,
        InitialDiscreteValueEntry {
            target,
            value: value.index(),
            provenance: owner,
        },
        "discrete initial-value arena",
        owner,
    )?;
    storage
        .initial_discrete_value_by_variable
        .insert(target, raw);
    Ok(InitialDiscreteValueId::from_raw(raw))
}

/// Prove that every coordinate the value reads is settled at the initialization
/// instant, and that evaluating the value twice there yields the same number.
///
/// The runtime applies initialization updates until they stop changing, so a
/// value whose result depends on when it ran — an MLS §12.3 `impure` external
/// call — has no fixed point to reach. Rejecting it here names the missing
/// owner instead of leaving a convergence failure for the runtime to hit.
fn expect_initialization_settled_reads(
    storage: &Storage,
    value: ExprId<'_>,
    owner: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let mut pending = vec![value.index()];
    let mut visited = std::collections::BTreeSet::new();
    while let Some(expression) = pending.pop() {
        if !visited.insert(expression) {
            continue;
        }
        let node = &storage.expressions.nodes[expression as usize];
        let settled = match node {
            ExprNode::Coordinate(coordinate) => matches!(
                coordinate,
                Coordinate::Parameter(_) | Coordinate::Time | Coordinate::ClockInterval(_)
            ),
            ExprNode::Call { function, .. } => storage.function_is_pure(*function, owner)?,
            _ => true,
        };
        if !settled {
            return Err(DaeConstructionError::InvalidExpressionForm { span: owner.span() });
        }
        node.for_each_child(&storage.expressions, |child| pending.push(child));
    }
    Ok(())
}

pub struct DiscreteEquations<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DiscreteEquations<'_, 'dae> {
    pub fn real_equation(
        &mut self,
        owner: DaeProvenance,
        build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<DiscreteRealEquationId<'dae>, DaeConstructionError> {
        self.insert_real_equation(DiscreteRealActivationEntry::Always, owner, build)
    }

    pub fn when_real_equation(
        &mut self,
        trigger: ConditionId<'dae>,
        guard: ConditionId<'dae>,
        owner: DaeProvenance,
        build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<DiscreteRealEquationId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, owner)?;
        expect_defined_condition(self.storage, trigger, owner)?;
        expect_defined_condition(self.storage, guard, owner)?;
        self.insert_real_equation(
            DiscreteRealActivationEntry::When {
                trigger: trigger.index(),
                guard: guard.index(),
            },
            owner,
            build,
        )
    }

    fn insert_real_equation(
        &mut self,
        activation: DiscreteRealActivationEntry,
        owner: DaeProvenance,
        build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<DiscreteRealEquationId<'dae>, DaeConstructionError> {
        let residual = build_residual_entry(
            self.source_map,
            self.storage,
            owner,
            ResidualShape::TargetShaped,
            build,
        )?;
        if let DiscreteRealActivationEntry::When { guard, .. } = activation {
            expect_clock_owned_discrete_reals(self.storage, guard, residual.residual, owner)?;
        }
        let entry = DiscreteRealEquationEntry {
            residual: residual.residual,
            activation,
            provenance: residual.provenance,
        };
        push_dense(
            &mut self.storage.discrete_real_equations,
            entry,
            "discrete Real equation arena",
            owner,
        )
        .map(DiscreteRealEquationId::from_raw)
    }
}

pub struct ResidualEquation<'storage, 'dae> {
    source_map: &'storage rumoca_core::SourceMap,
    storage: &'storage mut Storage,
    owner: DaeProvenance,
    shape: ResidualShape,
    residual: Option<u32>,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ResidualEquation<'_, 'dae> {
    pub fn expressions(&mut self) -> crate::Expressions<'_, 'dae> {
        crate::Expressions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        }
    }

    pub fn residual(&mut self, residual: ExprId<'dae>) -> Result<(), DaeConstructionError> {
        if self.residual.is_some() {
            return Err(duplicate(
                "equation residual",
                self.storage.total_residual_equation_count() as u32,
                self.owner,
            ));
        }
        self.storage
            .expect_real_residual(residual, self.shape, self.owner)?;
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
        let residual =
            self.expressions()
                .at(provenance)
                .binary(BinaryOperator::Subtract, lhs, rhs)?;
        self.residual(residual)?;
        Ok(residual)
    }
}

pub struct StructuredResiduals<'storage, 'dae> {
    source_map: &'storage rumoca_core::SourceMap,
    storage: &'storage mut Storage,
    owner: DaeProvenance,
    domain: DomainId<'dae>,
    scalar_view: ComprehensionScalarView,
    body_count: usize,
    marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> StructuredResiduals<'_, 'dae> {
    pub fn expressions(&mut self) -> crate::Expressions<'_, 'dae> {
        crate::Expressions {
            source_map: self.source_map,
            storage: self.storage,
            marker: PhantomData,
        }
    }

    pub fn body(&mut self, residual: ExprId<'dae>) -> Result<(), DaeConstructionError> {
        self.storage
            .expect_domain_expression(residual, self.domain, self.owner)?;
        let ty = self.storage.expr_type(residual, self.owner)?;
        if ty.scalar_type() != ScalarType::Real {
            return Err(DaeConstructionError::TypeMismatch {
                expected: ScalarType::Real,
                found: ty.scalar_type(),
                span: self.owner.span(),
            });
        }
        let extents = self.storage.domain_extents(self.domain, self.owner)?;
        match self.scalar_view {
            ComprehensionScalarView::BinderSubstitution if !ty.is_scalar() => {
                return Err(DaeConstructionError::ExpectedScalar {
                    span: self.owner.span(),
                });
            }
            // A row-major view selects by flat scalar ordinal. Singleton axes
            // remain part of the tensor type, but do not add domain points;
            // equal checked cardinality is therefore the exact ownership proof.
            ComprehensionScalarView::RowMajorProjection
                if ty.scalar_count()
                    != Some(self.storage.domain_scalar_count(self.domain, self.owner)?) =>
            {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: self.owner.span(),
                });
            }
            ComprehensionScalarView::BinderPrefixProjection { binder_count }
                if extents.get(usize::try_from(binder_count).unwrap_or(usize::MAX)..)
                    != Some(ty.dimensions()) =>
            {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: self.owner.span(),
                });
            }
            _ => {}
        }
        self.storage.equation_family_bodies.push(residual.index());
        self.body_count += 1;
        Ok(())
    }
}

fn build_value_equation<'dae, P: StructuredPartition>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    owner: DaeProvenance,
    residual: ExprId<'dae>,
) -> Result<(), DaeConstructionError> {
    check_provenance(source_map, owner)?;
    storage.expect_closed_expression(residual, owner)?;
    let ty = storage.expr_type(residual, owner)?.clone();
    if ty.scalar_type() != ScalarType::Real {
        return Err(DaeConstructionError::TypeMismatch {
            expected: ScalarType::Real,
            found: ty.scalar_type(),
            span: owner.span(),
        });
    }
    if ty.is_scalar() {
        build_residual::<P>(source_map, storage, owner, |equation| {
            equation.residual(residual)
        })?;
        return Ok(());
    }
    let domain_provenance =
        DaeProvenance::generated(DaeGeneration::ArrayEquationProjection, owner.span())?;
    let domain = insert_domain(
        source_map,
        storage,
        None,
        projection_domain(ty.dimensions()),
        domain_provenance,
    )?;
    build_structured_family::<P>(
        source_map,
        storage,
        owner,
        domain,
        ComprehensionScalarView::RowMajorProjection,
        |family| family.body(residual),
    )?;
    Ok(())
}

fn projection_domain(dimensions: &[u32]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: dimensions
            .iter()
            .enumerate()
            .map(|(axis, extent)| StructuredIndexBinder {
                id: axis,
                display_name: format!("$axis{}", axis + 1),
                lower: 1,
                upper: i64::from(*extent),
                step: 1,
            })
            .collect(),
    }
}

fn build_structured_family<'dae, P: StructuredPartition>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    owner: DaeProvenance,
    domain: DomainId<'dae>,
    scalar_view: ComprehensionScalarView,
    build: impl FnOnce(&mut StructuredResiduals<'_, 'dae>) -> Result<(), DaeConstructionError>,
) -> Result<u32, DaeConstructionError> {
    check_provenance(source_map, owner)?;
    let scalar_count = storage.domain_scalar_count(domain, owner)?;
    let start = storage.equation_family_bodies.len();
    let mut residuals = StructuredResiduals {
        source_map,
        storage,
        owner,
        domain,
        scalar_view,
        body_count: 0,
        marker: PhantomData,
    };
    build(&mut residuals)?;
    if residuals.body_count == 0 {
        return Err(invalid_arity(1, 0, owner));
    }
    let body_count = residuals.body_count;
    let scalar_rows =
        scalar_count
            .checked_mul(body_count)
            .ok_or(DaeConstructionError::CapacityExceeded {
                arena: "structured equation scalar rows",
                attempted_index: usize::MAX,
                span: owner.span(),
            })?;
    let entry = StructuredFamilyEntry {
        domain: domain.index(),
        scalar_view,
        bodies: FamilyBodyRange {
            start: checked_u32(start, "equation family body buffer", owner)?,
            len: checked_u32(body_count, "equation family body buffer", owner)?,
        },
        scalar_rows: checked_u32(scalar_rows, "structured equation scalar rows", owner)?,
        provenance: owner,
    };
    P::insert_family(residuals.storage, entry, owner)
}

fn build_residual<'dae, P: ResidualPartition>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    owner: DaeProvenance,
    build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
) -> Result<u32, DaeConstructionError> {
    let entry = build_residual_entry(source_map, storage, owner, ResidualShape::Scalar, build)?;
    P::insert(storage, entry, owner)
}

fn build_residual_entry<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    owner: DaeProvenance,
    shape: ResidualShape,
    build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
) -> Result<ResidualEquationEntry, DaeConstructionError> {
    check_provenance(source_map, owner)?;
    let mut equation = ResidualEquation {
        source_map,
        storage,
        owner,
        shape,
        residual: None,
        marker: PhantomData,
    };
    build(&mut equation)?;
    let Some(residual) = equation.residual else {
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "equation residual",
            index: equation.storage.total_residual_equation_count() as u32,
            span: owner.span(),
        });
    };
    let entry = ResidualEquationEntry {
        residual,
        provenance: owner,
    };
    Ok(entry)
}

fn expect_defined_condition(
    storage: &Storage,
    condition: ConditionId<'_>,
    owner: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let Some(entry) = storage.conditions.get(condition.index() as usize) else {
        return Err(crate::model::unknown("condition", condition.index(), owner));
    };
    if entry.node.is_none() {
        return Err(DaeConstructionError::IncompleteDefinition {
            kind: "discrete Real equation activation condition",
            index: condition.index(),
            span: owner.span(),
        });
    }
    Ok(())
}

fn expect_clock_owned_discrete_reals(
    storage: &Storage,
    guard: u32,
    residual: u32,
    owner: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let Some(clock) = condition_owner_clock(storage, guard, owner)? else {
        return Ok(());
    };
    let mut pending = vec![residual];
    let mut visited = std::collections::BTreeSet::new();
    while let Some(expression) = pending.pop() {
        if !visited.insert(expression) {
            continue;
        }
        let node = &storage.expressions.nodes[expression as usize];
        if let ExprNode::Coordinate(Coordinate::DiscreteReal(variable)) = node
            && storage
                .clock_ownership_by_variable
                .get(variable)
                .and_then(|&index| storage.clock_ownerships.get(index as usize))
                .is_none_or(|ownership| ownership.clock != clock)
        {
            return Err(DaeConstructionError::MissingClockOwnership {
                variable: *variable,
                clock,
                span: owner.span(),
            });
        }
        node.for_each_child(&storage.expressions, |child| pending.push(child));
    }
    Ok(())
}

fn push_dense<T>(
    arena: &mut Vec<T>,
    entry: T,
    name: &'static str,
    owner: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    let raw = checked_u32(arena.len(), name, owner)?;
    arena.push(entry);
    Ok(raw)
}
