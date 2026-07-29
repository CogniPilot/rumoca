use std::marker::PhantomData;

use rumoca_core::{ComprehensionScalarView, StructuredIndexBinder, StructuredIndexDomain};

use crate::events::EventActionKind;
use crate::model::{
    Storage, check_provenance, checked_u32, duplicate, insert_domain, invalid_arity,
};
use crate::{
    BinaryOperator, ContinuousEquationId, ContinuousFamilyId, DaeConstructionError, DaeGeneration,
    DaeProvenance, DiscreteAssignmentId, DiscreteRealEquationId, DiscreteValueId, DomainId, ExprId,
    InitializationEquationId, InitializationFamilyId, ScalarType,
};

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ResidualEquationEntry {
    pub(crate) residual: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DiscreteAssignmentEntry {
    pub(crate) target: u32,
    pub(crate) value: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy)]
pub struct DiscreteAssignmentView<'dae> {
    pub(crate) target: DiscreteValueId<'dae>,
    pub(crate) value: ExprId<'dae>,
    pub(crate) provenance: DaeProvenance,
}

impl<'dae> DiscreteAssignmentView<'dae> {
    pub const fn target(self) -> DiscreteValueId<'dae> {
        self.target
    }

    pub const fn value(self) -> ExprId<'dae> {
        self.value
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
    pub(crate) scalar_rows: u32,
    pub(crate) provenance: DaeProvenance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum EquationOwnerEntry {
    Residual(u32),
    Structured(u32),
}

pub struct ContinuousEquations<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> ContinuousEquations<'_, 'dae> {
    /// Attach one scalar or array-valued Real residual as one semantic owner.
    ///
    /// Array residuals become a compact row-major family whose domain is
    /// derived from the checked expression shape.
    pub fn value_equation(
        &mut self,
        owner: DaeProvenance,
        residual: ExprId<'dae>,
    ) -> Result<(), DaeConstructionError> {
        build_value_equation(
            self.source_map,
            self.storage,
            owner,
            residual,
            StructuredPartition::Continuous,
        )
    }

    pub fn equation(
        &mut self,
        owner: DaeProvenance,
        build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<ContinuousEquationId<'dae>, DaeConstructionError> {
        let raw = build_residual(
            self.source_map,
            self.storage,
            owner,
            build,
            ResidualPartition::Continuous,
        )?;
        Ok(ContinuousEquationId::from_raw(raw))
    }

    pub fn structured_family(
        &mut self,
        owner: DaeProvenance,
        domain: DomainId<'dae>,
        scalar_view: ComprehensionScalarView,
        build: impl FnOnce(&mut StructuredResiduals<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<ContinuousFamilyId<'dae>, DaeConstructionError> {
        build_structured_family(
            self.source_map,
            self.storage,
            owner,
            domain,
            scalar_view,
            build,
            StructuredPartition::Continuous,
        )
        .map(ContinuousFamilyId::from_raw)
    }
}

pub struct InitializationEquations<'storage, 'dae> {
    pub(crate) source_map: &'storage rumoca_core::SourceMap,
    pub(crate) storage: &'storage mut Storage,
    pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> InitializationEquations<'_, 'dae> {
    /// Attach one scalar or array-valued initialization residual.
    pub fn value_equation(
        &mut self,
        owner: DaeProvenance,
        residual: ExprId<'dae>,
    ) -> Result<(), DaeConstructionError> {
        build_value_equation(
            self.source_map,
            self.storage,
            owner,
            residual,
            StructuredPartition::Initialization,
        )
    }

    pub fn equation(
        &mut self,
        owner: DaeProvenance,
        build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<InitializationEquationId<'dae>, DaeConstructionError> {
        let raw = build_residual(
            self.source_map,
            self.storage,
            owner,
            build,
            ResidualPartition::Initialization,
        )?;
        Ok(InitializationEquationId::from_raw(raw))
    }

    pub fn structured_family(
        &mut self,
        owner: DaeProvenance,
        domain: DomainId<'dae>,
        scalar_view: ComprehensionScalarView,
        build: impl FnOnce(&mut StructuredResiduals<'_, 'dae>) -> Result<(), DaeConstructionError>,
    ) -> Result<InitializationFamilyId<'dae>, DaeConstructionError> {
        build_structured_family(
            self.source_map,
            self.storage,
            owner,
            domain,
            scalar_view,
            build,
            StructuredPartition::Initialization,
        )
        .map(InitializationFamilyId::from_raw)
    }
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
        let raw = build_residual(
            self.source_map,
            self.storage,
            owner,
            build,
            ResidualPartition::DiscreteReal,
        )?;
        Ok(DiscreteRealEquationId::from_raw(raw))
    }

    pub fn assignment(
        &mut self,
        owner: DaeProvenance,
        target: DiscreteValueId<'dae>,
        value: ExprId<'dae>,
    ) -> Result<DiscreteAssignmentId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, owner)?;
        self.storage
            .expect_discrete_value_target(target, value, owner)?;
        if self
            .storage
            .discrete_assignments
            .iter()
            .any(|entry| entry.target == target.index())
            || self.storage.event_actions.iter().any(|entry| {
                matches!(
                    entry.kind,
                    EventActionKind::AssignDiscreteValue {
                        target: found,
                        ..
                    } if found == target.index()
                )
            })
        {
            return Err(DaeConstructionError::DuplicateKey {
                kind: "B.1c target",
                key: self
                    .storage
                    .variable_name(target.index(), owner)?
                    .to_string(),
                span: owner.span(),
            });
        }
        let raw = checked_u32(
            self.storage.discrete_assignments.len(),
            "discrete assignment arena",
            owner,
        )?;
        self.storage
            .discrete_assignments
            .push(DiscreteAssignmentEntry {
                target: target.index(),
                value: value.index(),
                provenance: owner,
            });
        self.storage.unassigned_discrete_values -= 1;
        Ok(DiscreteAssignmentId::from_raw(raw))
    }
}

pub struct ResidualEquation<'storage, 'dae> {
    source_map: &'storage rumoca_core::SourceMap,
    storage: &'storage mut Storage,
    owner: DaeProvenance,
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
        self.storage.expect_real_residual(residual, self.owner)?;
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
            ComprehensionScalarView::RowMajorProjection if ty.dimensions() != extents => {
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

enum ResidualPartition {
    Continuous,
    Initialization,
    DiscreteReal,
}

enum StructuredPartition {
    Continuous,
    Initialization,
}

fn build_value_equation<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    owner: DaeProvenance,
    residual: ExprId<'dae>,
    partition: StructuredPartition,
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
        let residual_partition = match partition {
            StructuredPartition::Continuous => ResidualPartition::Continuous,
            StructuredPartition::Initialization => ResidualPartition::Initialization,
        };
        build_residual(
            source_map,
            storage,
            owner,
            |equation| equation.residual(residual),
            residual_partition,
        )?;
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
    build_structured_family(
        source_map,
        storage,
        owner,
        domain,
        ComprehensionScalarView::RowMajorProjection,
        |family| family.body(residual),
        partition,
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

fn build_structured_family<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    owner: DaeProvenance,
    domain: DomainId<'dae>,
    scalar_view: ComprehensionScalarView,
    build: impl FnOnce(&mut StructuredResiduals<'_, 'dae>) -> Result<(), DaeConstructionError>,
    partition: StructuredPartition,
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
    let raw = match partition {
        StructuredPartition::Continuous => {
            let raw = checked_u32(
                residuals.storage.continuous_families.len(),
                "structured equation family arena",
                owner,
            )?;
            residuals.storage.continuous_families.push(entry);
            residuals
                .storage
                .continuous_equation_owners
                .push(EquationOwnerEntry::Structured(raw));
            raw
        }
        StructuredPartition::Initialization => {
            let raw = checked_u32(
                residuals.storage.initialization_families.len(),
                "structured equation family arena",
                owner,
            )?;
            residuals.storage.initialization_families.push(entry);
            residuals
                .storage
                .initialization_equation_owners
                .push(EquationOwnerEntry::Structured(raw));
            raw
        }
    };
    Ok(raw)
}

fn build_residual<'dae>(
    source_map: &rumoca_core::SourceMap,
    storage: &mut Storage,
    owner: DaeProvenance,
    build: impl FnOnce(&mut ResidualEquation<'_, 'dae>) -> Result<(), DaeConstructionError>,
    partition: ResidualPartition,
) -> Result<u32, DaeConstructionError> {
    check_provenance(source_map, owner)?;
    let mut equation = ResidualEquation {
        source_map,
        storage,
        owner,
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
    let raw = match partition {
        ResidualPartition::Continuous => {
            let raw = checked_u32(
                equation.storage.continuous_equations.len(),
                "equation arena",
                owner,
            )?;
            equation.storage.continuous_equations.push(entry);
            equation
                .storage
                .continuous_equation_owners
                .push(EquationOwnerEntry::Residual(raw));
            raw
        }
        ResidualPartition::Initialization => {
            let raw = checked_u32(
                equation.storage.initialization_equations.len(),
                "equation arena",
                owner,
            )?;
            equation.storage.initialization_equations.push(entry);
            equation
                .storage
                .initialization_equation_owners
                .push(EquationOwnerEntry::Residual(raw));
            raw
        }
        ResidualPartition::DiscreteReal => {
            let raw = checked_u32(
                equation.storage.discrete_real_equations.len(),
                "equation arena",
                owner,
            )?;
            equation.storage.discrete_real_equations.push(entry);
            raw
        }
    };
    Ok(raw)
}
