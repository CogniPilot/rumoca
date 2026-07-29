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
        residual $discrete_partition:ident, $discrete_scope:ident {
            residual $real_equation:ident
                -> $discrete_equation_id:ident, $discrete_equations:ident;
        }
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

        struct $discrete_partition;

        impl ResidualPartition for $discrete_partition {
            fn insert(
                storage: &mut Storage,
                entry: ResidualEquationEntry,
                owner: DaeProvenance,
            ) -> Result<u32, DaeConstructionError> {
                push_dense(
                    &mut storage.$discrete_equations,
                    entry,
                    "equation arena",
                    owner,
                )
            }
        }

        pub struct $discrete_scope<'storage, 'dae> {
            pub(crate) source_map: &'storage rumoca_core::SourceMap,
            pub(crate) storage: &'storage mut Storage,
            pub(crate) marker: PhantomData<&'dae mut &'dae ()>,
        }

        impl<'dae> $discrete_scope<'_, 'dae> {
            pub fn $real_equation(
                &mut self,
                owner: DaeProvenance,
                build: impl FnOnce(
                    &mut ResidualEquation<'_, 'dae>,
                ) -> Result<(), DaeConstructionError>,
            ) -> Result<$discrete_equation_id<'dae>, DaeConstructionError> {
                build_residual::<$discrete_partition>(
                    self.source_map,
                    self.storage,
                    owner,
                    build,
                )
                .map($discrete_equation_id::from_raw)
            }
        }
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
    residual DiscreteRealPartition, DiscreteEquations {
        residual real_equation -> DiscreteRealEquationId, discrete_real_equations;
    }
}

impl<'dae> DiscreteEquations<'_, 'dae> {
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
    P::insert(equation.storage, entry, owner)
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
