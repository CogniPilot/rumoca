use std::ops::Range;

use rumoca_core::{
    ArrayAccess, ComprehensionScalarView, ComprehensionTemplate, Expression, RegularForFamily,
    StructuredIndexBinderId, StructuredIndexDomainError, ValidStructuredIndexDomain, VarName,
};

use crate::{Equation, Model, StructuredEquationFamily, Variable};

#[cfg(test)]
mod tests;

/// The two equation vectors whose structured owners are proved together.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EquationPartitionKind {
    Continuous,
    Initialization,
}

impl EquationPartitionKind {
    pub(crate) const fn wire_name(self) -> &'static str {
        match self {
            Self::Continuous => "regular",
            Self::Initialization => "initial",
        }
    }
}

/// Typed reason that a Flat equation-owner partition could not be constructed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StructuredEquationOwnerError {
    ZeroScalarCount {
        partition: EquationPartitionKind,
        row: usize,
    },
    InvalidDomain {
        partition: EquationPartitionKind,
        family: usize,
        source: StructuredIndexDomainError,
    },
    NonCanonicalBinder {
        partition: EquationPartitionKind,
        family: usize,
        binder: usize,
    },
    EmptyBinderName {
        partition: EquationPartitionKind,
        family: usize,
        binder: usize,
    },
    ZeroEquationsPerPoint {
        partition: EquationPartitionKind,
        family: usize,
    },
    MissingCanonicalTemplate {
        partition: EquationPartitionKind,
        family: usize,
    },
    TemplateBodyCountMismatch {
        partition: EquationPartitionKind,
        family: usize,
    },
    ProjectionOutsideDomain {
        partition: EquationPartitionKind,
        family: usize,
    },
    RepresentedRowCountOverflow {
        partition: EquationPartitionKind,
        family: usize,
    },
    RowRangeOutsidePartition {
        partition: EquationPartitionKind,
        family: usize,
    },
    RowOriginMismatch {
        partition: EquationPartitionKind,
        family: usize,
        row: usize,
    },
    RowScalarCountMismatch {
        partition: EquationPartitionKind,
        family: usize,
        row: usize,
    },
    OverlappingFamilies {
        partition: EquationPartitionKind,
        family: usize,
        prior_family: usize,
        row: usize,
    },
    NonMaterializedWithoutRegularProof {
        partition: EquationPartitionKind,
        family: usize,
    },
    RegularBinderMismatch {
        partition: EquationPartitionKind,
        family: usize,
    },
    RegularRequiresBinderSubstitution {
        partition: EquationPartitionKind,
        family: usize,
    },
    EmptyRegularAccess {
        partition: EquationPartitionKind,
        family: usize,
        access: usize,
    },
    RegularAccessCoefficientRankMismatch {
        partition: EquationPartitionKind,
        family: usize,
        access: usize,
    },
    RegularAccessMissingVariable {
        partition: EquationPartitionKind,
        family: usize,
        access: usize,
    },
    RegularAccessRankMismatch {
        partition: EquationPartitionKind,
        family: usize,
        access: usize,
    },
    RegularAccessOutOfBounds {
        partition: EquationPartitionKind,
        family: usize,
        access: usize,
        axis: usize,
    },
}

impl StructuredEquationOwnerError {
    pub const fn partition(&self) -> EquationPartitionKind {
        match self {
            Self::ZeroScalarCount { partition, .. }
            | Self::InvalidDomain { partition, .. }
            | Self::NonCanonicalBinder { partition, .. }
            | Self::EmptyBinderName { partition, .. }
            | Self::ZeroEquationsPerPoint { partition, .. }
            | Self::MissingCanonicalTemplate { partition, .. }
            | Self::TemplateBodyCountMismatch { partition, .. }
            | Self::ProjectionOutsideDomain { partition, .. }
            | Self::RepresentedRowCountOverflow { partition, .. }
            | Self::RowRangeOutsidePartition { partition, .. }
            | Self::RowOriginMismatch { partition, .. }
            | Self::RowScalarCountMismatch { partition, .. }
            | Self::OverlappingFamilies { partition, .. }
            | Self::NonMaterializedWithoutRegularProof { partition, .. }
            | Self::RegularBinderMismatch { partition, .. }
            | Self::RegularRequiresBinderSubstitution { partition, .. }
            | Self::EmptyRegularAccess { partition, .. }
            | Self::RegularAccessCoefficientRankMismatch { partition, .. }
            | Self::RegularAccessMissingVariable { partition, .. }
            | Self::RegularAccessRankMismatch { partition, .. }
            | Self::RegularAccessOutOfBounds { partition, .. } => *partition,
        }
    }

    pub const fn location(&self) -> StructuredOwnerErrorLocation {
        match self {
            Self::ZeroScalarCount { row, .. } => StructuredOwnerErrorLocation::Row(*row),
            Self::InvalidDomain { family, .. }
            | Self::NonCanonicalBinder { family, .. }
            | Self::EmptyBinderName { family, .. }
            | Self::ZeroEquationsPerPoint { family, .. }
            | Self::MissingCanonicalTemplate { family, .. }
            | Self::TemplateBodyCountMismatch { family, .. }
            | Self::ProjectionOutsideDomain { family, .. }
            | Self::RepresentedRowCountOverflow { family, .. }
            | Self::RowRangeOutsidePartition { family, .. }
            | Self::RowOriginMismatch { family, .. }
            | Self::RowScalarCountMismatch { family, .. }
            | Self::OverlappingFamilies { family, .. }
            | Self::NonMaterializedWithoutRegularProof { family, .. }
            | Self::RegularBinderMismatch { family, .. }
            | Self::RegularRequiresBinderSubstitution { family, .. }
            | Self::EmptyRegularAccess { family, .. }
            | Self::RegularAccessCoefficientRankMismatch { family, .. }
            | Self::RegularAccessMissingVariable { family, .. }
            | Self::RegularAccessRankMismatch { family, .. }
            | Self::RegularAccessOutOfBounds { family, .. } => {
                StructuredOwnerErrorLocation::Family(*family)
            }
        }
    }

    pub(crate) const fn wire_reason(&self) -> &'static str {
        match self {
            Self::ZeroScalarCount { .. } => "scalar_count must be positive",
            Self::InvalidDomain { .. } => "the compact domain is invalid or overflows",
            Self::NonCanonicalBinder { .. } => {
                "structured binder identities must equal their domain ordinals"
            }
            Self::EmptyBinderName { .. } => "structured binders require nonempty display names",
            Self::ZeroEquationsPerPoint { .. } => "equations_per_point must be positive",
            Self::MissingCanonicalTemplate { .. } => {
                "a structured family requires its canonical compact template"
            }
            Self::TemplateBodyCountMismatch { .. } | Self::RepresentedRowCountOverflow { .. } => {
                "the represented row count is inconsistent or overflows"
            }
            Self::ProjectionOutsideDomain { .. } => {
                "the projected scalar row shape is outside its exact domain"
            }
            Self::RowRangeOutsidePartition { .. } => {
                "the represented row range is outside its equation partition"
            }
            Self::RowOriginMismatch { .. } => {
                "a represented row's typed origin contradicts its structured owner"
            }
            Self::RowScalarCountMismatch { .. } => {
                "a represented row's scalar shape contradicts its structured projection"
            }
            Self::OverlappingFamilies { .. } => "two structured owners overlap one equation row",
            Self::NonMaterializedWithoutRegularProof { .. } => {
                "a non-materialized family requires its checked regular-family proof"
            }
            Self::RegularBinderMismatch { .. } => {
                "the regular-family binder inventory contradicts its exact domain"
            }
            Self::RegularRequiresBinderSubstitution { .. } => {
                "a regular-family proof requires its binder-substitution template"
            }
            Self::EmptyRegularAccess { .. } | Self::RegularAccessCoefficientRankMismatch { .. } => {
                "a regular-family access has incomplete affine-domain evidence"
            }
            Self::RegularAccessMissingVariable { .. } => {
                "a regular-family access is absent from the exact Flat variable catalog"
            }
            Self::RegularAccessRankMismatch { .. } => {
                "a regular-family access rank contradicts its exact Flat variable"
            }
            Self::RegularAccessOutOfBounds { .. } => {
                "a regular-family affine access exceeds exact one-based bounds"
            }
        }
    }
}

impl std::fmt::Display for StructuredEquationOwnerError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let partition = self.partition().wire_name();
        match self.location() {
            StructuredOwnerErrorLocation::Family(family) => write!(
                formatter,
                "Flat {partition} structured family {family} is invalid: {}",
                self.wire_reason()
            ),
            StructuredOwnerErrorLocation::Row(row) => write!(
                formatter,
                "Flat {partition} equation {row} is invalid: {}",
                self.wire_reason()
            ),
        }
    }
}

impl std::error::Error for StructuredEquationOwnerError {}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StructuredOwnerErrorLocation {
    Row(usize),
    Family(usize),
}

/// One regular-family descriptor whose binder, access, rank, and bounds facts
/// have been checked against its exact Flat owner.
pub struct CheckedRegularFamily<'model> {
    descriptor: &'model RegularForFamily,
    accesses: Box<[CheckedRegularAccess<'model>]>,
}

impl<'model> CheckedRegularFamily<'model> {
    pub fn descriptor(&self) -> &'model RegularForFamily {
        self.descriptor
    }

    pub fn accesses(&self) -> &[CheckedRegularAccess<'model>] {
        &self.accesses
    }
}

/// One affine access paired with the exact Flat variable whose rank and bounds
/// admitted it.
pub struct CheckedRegularAccess<'model> {
    descriptor: &'model ArrayAccess,
    variable: &'model Variable,
}

impl<'model> CheckedRegularAccess<'model> {
    pub fn descriptor(&self) -> &'model ArrayAccess {
        self.descriptor
    }

    pub fn variable(&self) -> &'model Variable {
        self.variable
    }
}

/// Whether a template family's row vector is complete or only retains the
/// checked regular-family construction sample.
pub enum CheckedTemplateRows<'model> {
    Complete {
        regular: Option<CheckedRegularFamily<'model>>,
    },
    RegularConstructionSample {
        regular: CheckedRegularFamily<'model>,
    },
}

impl<'model> CheckedTemplateRows<'model> {
    pub fn regular(&self) -> Option<&CheckedRegularFamily<'model>> {
        match self {
            Self::Complete { regular } => regular.as_ref(),
            Self::RegularConstructionSample { regular } => Some(regular),
        }
    }
}

/// One equation row that has no structured owner.
pub struct CheckedStandaloneRow<'model> {
    row: usize,
    equation: &'model Equation,
}

impl<'model> CheckedStandaloneRow<'model> {
    pub fn row(&self) -> usize {
        self.row
    }

    pub fn equation(&self) -> &'model Equation {
        self.equation
    }
}

/// One authoritative compact template and its proven scalar-view row range.
pub struct CheckedTemplateFamily<'model> {
    family_index: usize,
    family: &'model StructuredEquationFamily,
    template: &'model ComprehensionTemplate,
    domain: ValidStructuredIndexDomain<'model>,
    rows: Range<usize>,
    equations: &'model [Equation],
    row_representation: CheckedTemplateRows<'model>,
}

impl<'model> CheckedTemplateFamily<'model> {
    pub fn family_index(&self) -> usize {
        self.family_index
    }

    pub fn family(&self) -> &'model StructuredEquationFamily {
        self.family
    }

    pub fn template(&self) -> &'model ComprehensionTemplate {
        self.template
    }

    pub fn domain(&self) -> &ValidStructuredIndexDomain<'model> {
        &self.domain
    }

    pub fn rows(&self) -> Range<usize> {
        self.rows.clone()
    }

    pub fn equations(&self) -> &'model [Equation] {
        self.equations
    }

    pub fn row_representation(&self) -> &CheckedTemplateRows<'model> {
        &self.row_representation
    }
}

/// One source-ordered semantic equation owner.
pub enum CheckedEquationOwner<'model> {
    Standalone(CheckedStandaloneRow<'model>),
    Template(CheckedTemplateFamily<'model>),
}

impl CheckedEquationOwner<'_> {
    fn first_row(&self) -> usize {
        match self {
            Self::Standalone(owner) => owner.row,
            Self::Template(owner) => owner.rows.start,
        }
    }

    fn stable_ordinal(&self) -> usize {
        match self {
            Self::Standalone(_) => usize::MAX,
            Self::Template(owner) => owner.family_index,
        }
    }
}

/// One proved equation partition in source-row order.
pub struct CheckedEquationPartition<'model> {
    kind: EquationPartitionKind,
    owners: Box<[CheckedEquationOwner<'model>]>,
    semantic_expression_roots: Box<[&'model Expression]>,
}

impl<'model> CheckedEquationPartition<'model> {
    pub fn kind(&self) -> EquationPartitionKind {
        self.kind
    }

    pub fn owners(&self) -> &[CheckedEquationOwner<'model>] {
        &self.owners
    }

    pub fn standalone_rows(&self) -> impl Iterator<Item = &CheckedStandaloneRow<'model>> {
        self.owners.iter().filter_map(|owner| match owner {
            CheckedEquationOwner::Standalone(row) => Some(row),
            CheckedEquationOwner::Template(_) => None,
        })
    }

    pub fn structured_row_indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.owners.iter().flat_map(|owner| match owner {
            CheckedEquationOwner::Standalone(_) => 0..0,
            CheckedEquationOwner::Template(family) => family.rows(),
        })
    }

    pub fn semantic_expression_roots(&self) -> impl Iterator<Item = &'model Expression> + '_ {
        self.semantic_expression_roots.iter().copied()
    }
}

/// The single correlated ownership proof for both Flat equation partitions.
pub struct CheckedStructuredEquationOwners<'model> {
    model: &'model Model,
    continuous: CheckedEquationPartition<'model>,
    initialization: CheckedEquationPartition<'model>,
}

impl<'model> CheckedStructuredEquationOwners<'model> {
    pub fn model(&self) -> &'model Model {
        self.model
    }

    pub fn continuous(&self) -> &CheckedEquationPartition<'model> {
        &self.continuous
    }

    pub fn initialization(&self) -> &CheckedEquationPartition<'model> {
        &self.initialization
    }

    pub fn semantic_expression_roots(&self) -> impl Iterator<Item = &'model Expression> + '_ {
        self.continuous
            .semantic_expression_roots()
            .chain(self.initialization.semantic_expression_roots())
    }
}

impl Model {
    /// Check the two exact Flat equation vectors once and return their complete,
    /// ordered semantic-owner partition.
    pub fn structured_equation_owners(
        &self,
    ) -> Result<CheckedStructuredEquationOwners<'_>, StructuredEquationOwnerError> {
        Ok(CheckedStructuredEquationOwners {
            model: self,
            continuous: CheckedEquationPartition::construct(
                EquationPartitionKind::Continuous,
                &self.equations,
                &self.structured_equations,
                &self.variables,
            )?,
            initialization: CheckedEquationPartition::construct(
                EquationPartitionKind::Initialization,
                &self.initial_equations,
                &self.initial_structured_equations,
                &self.variables,
            )?,
        })
    }
}

impl<'model> CheckedEquationPartition<'model> {
    fn construct(
        kind: EquationPartitionKind,
        equations: &'model [Equation],
        families: &'model [StructuredEquationFamily],
        variables: &'model crate::VarNameIndexMap<Variable>,
    ) -> Result<Self, StructuredEquationOwnerError> {
        validate_equation_rows(kind, equations)?;
        let mut structured_owners = Vec::with_capacity(families.len());
        let mut claimed_rows = vec![None; equations.len()];
        for (family_index, family) in families.iter().enumerate() {
            let owner = check_family(kind, family_index, family, equations, variables)?;
            claim_family_rows(kind, &mut claimed_rows, &owner)?;
            structured_owners.push(owner);
        }
        structured_owners.sort_by_key(|owner| (owner.first_row(), owner.stable_ordinal()));
        let owners = merge_standalone_rows(equations, structured_owners);
        let semantic_expression_roots = collect_semantic_expression_roots(&owners);
        Ok(Self {
            kind,
            owners: owners.into_boxed_slice(),
            semantic_expression_roots: semantic_expression_roots.into_boxed_slice(),
        })
    }
}

fn validate_equation_rows(
    partition: EquationPartitionKind,
    equations: &[Equation],
) -> Result<(), StructuredEquationOwnerError> {
    for (row, equation) in equations.iter().enumerate() {
        if equation.scalar_count == 0 {
            return Err(StructuredEquationOwnerError::ZeroScalarCount { partition, row });
        }
    }
    Ok(())
}

struct CheckedProjection {
    represented_rows: usize,
    scalar_count_per_row: usize,
}

fn check_family<'model>(
    partition: EquationPartitionKind,
    family_index: usize,
    family: &'model StructuredEquationFamily,
    equations: &'model [Equation],
    variables: &'model crate::VarNameIndexMap<Variable>,
) -> Result<CheckedEquationOwner<'model>, StructuredEquationOwnerError> {
    let domain = family.domain.validated().map_err(|source| {
        StructuredEquationOwnerError::InvalidDomain {
            partition,
            family: family_index,
            source,
        }
    })?;
    validate_canonical_binders(partition, family_index, &domain)?;
    if family.equations_per_point == 0 {
        return Err(StructuredEquationOwnerError::ZeroEquationsPerPoint {
            partition,
            family: family_index,
        });
    }
    let projection = check_projection(partition, family_index, family, &domain)?;
    let rows = checked_row_range(
        partition,
        family_index,
        family.first_equation_index,
        projection.represented_rows,
        equations.len(),
    )?;
    validate_row_correlation(
        partition,
        family_index,
        family,
        equations,
        &rows,
        projection.scalar_count_per_row,
    )?;
    let regular = check_regular_family(partition, family_index, family, variables, &domain)?;
    build_family_owner(
        partition,
        family_index,
        family,
        equations,
        rows,
        domain,
        regular,
    )
}

fn validate_canonical_binders(
    partition: EquationPartitionKind,
    family_index: usize,
    domain: &ValidStructuredIndexDomain<'_>,
) -> Result<(), StructuredEquationOwnerError> {
    for (binder, descriptor) in domain.binders().iter().enumerate() {
        if Some(descriptor.id) != StructuredIndexBinderId::from_ordinal(binder) {
            return Err(StructuredEquationOwnerError::NonCanonicalBinder {
                partition,
                family: family_index,
                binder,
            });
        }
        if descriptor.display_name.is_empty() {
            return Err(StructuredEquationOwnerError::EmptyBinderName {
                partition,
                family: family_index,
                binder,
            });
        }
    }
    Ok(())
}

fn check_projection(
    partition: EquationPartitionKind,
    family_index: usize,
    family: &StructuredEquationFamily,
    domain: &ValidStructuredIndexDomain<'_>,
) -> Result<CheckedProjection, StructuredEquationOwnerError> {
    let Some(template) = &family.template else {
        return Err(StructuredEquationOwnerError::MissingCanonicalTemplate {
            partition,
            family: family_index,
        });
    };
    if template.body.len() != family.equations_per_point {
        return Err(StructuredEquationOwnerError::TemplateBodyCountMismatch {
            partition,
            family: family_index,
        });
    }
    match template.scalar_view {
        ComprehensionScalarView::BinderSubstitution => checked_projection_products(
            partition,
            family_index,
            domain.scalar_count(),
            family.equations_per_point,
            1,
        ),
        ComprehensionScalarView::RowMajorProjection => Ok(CheckedProjection {
            represented_rows: family.equations_per_point,
            scalar_count_per_row: domain.scalar_count(),
        }),
        ComprehensionScalarView::BinderPrefixProjection { binder_count } => {
            let binder_count = usize::try_from(binder_count).map_err(|_| {
                StructuredEquationOwnerError::ProjectionOutsideDomain {
                    partition,
                    family: family_index,
                }
            })?;
            let Some((prefix, suffix)) = domain.extents().split_at_checked(binder_count) else {
                return Err(StructuredEquationOwnerError::ProjectionOutsideDomain {
                    partition,
                    family: family_index,
                });
            };
            let represented_points = checked_extent_product(partition, family_index, prefix)?;
            let scalar_count_per_row = checked_extent_product(partition, family_index, suffix)?;
            checked_projection_products(
                partition,
                family_index,
                represented_points,
                family.equations_per_point,
                scalar_count_per_row,
            )
        }
    }
}

fn checked_projection_products(
    partition: EquationPartitionKind,
    family_index: usize,
    represented_points: usize,
    equations_per_point: usize,
    scalar_count_per_row: usize,
) -> Result<CheckedProjection, StructuredEquationOwnerError> {
    let represented_rows = represented_points.checked_mul(equations_per_point).ok_or(
        StructuredEquationOwnerError::RepresentedRowCountOverflow {
            partition,
            family: family_index,
        },
    )?;
    Ok(CheckedProjection {
        represented_rows,
        scalar_count_per_row,
    })
}

fn checked_extent_product(
    partition: EquationPartitionKind,
    family_index: usize,
    extents: &[usize],
) -> Result<usize, StructuredEquationOwnerError> {
    extents.iter().try_fold(1usize, |product, extent| {
        product.checked_mul(*extent).ok_or(
            StructuredEquationOwnerError::RepresentedRowCountOverflow {
                partition,
                family: family_index,
            },
        )
    })
}

fn checked_row_range(
    partition: EquationPartitionKind,
    family_index: usize,
    start: usize,
    represented_rows: usize,
    equation_count: usize,
) -> Result<Range<usize>, StructuredEquationOwnerError> {
    let end = start
        .checked_add(represented_rows)
        .filter(|end| *end <= equation_count)
        .ok_or(StructuredEquationOwnerError::RowRangeOutsidePartition {
            partition,
            family: family_index,
        })?;
    Ok(start..end)
}

fn validate_row_correlation(
    partition: EquationPartitionKind,
    family_index: usize,
    family: &StructuredEquationFamily,
    equations: &[Equation],
    rows: &Range<usize>,
    scalar_count_per_row: usize,
) -> Result<(), StructuredEquationOwnerError> {
    for row in rows.clone() {
        if equations[row].origin != family.origin {
            return Err(StructuredEquationOwnerError::RowOriginMismatch {
                partition,
                family: family_index,
                row,
            });
        }
        if equations[row].scalar_count != scalar_count_per_row {
            return Err(StructuredEquationOwnerError::RowScalarCountMismatch {
                partition,
                family: family_index,
                row,
            });
        }
    }
    Ok(())
}

fn check_regular_family<'model>(
    partition: EquationPartitionKind,
    family_index: usize,
    family: &'model StructuredEquationFamily,
    variables: &'model crate::VarNameIndexMap<Variable>,
    domain: &ValidStructuredIndexDomain<'_>,
) -> Result<Option<CheckedRegularFamily<'model>>, StructuredEquationOwnerError> {
    let Some(regular) = &family.regular else {
        if !family.interiors_materialized {
            return Err(
                StructuredEquationOwnerError::NonMaterializedWithoutRegularProof {
                    partition,
                    family: family_index,
                },
            );
        }
        return Ok(None);
    };
    let binders_match = regular.binders.len() == domain.rank()
        && regular
            .binders
            .iter()
            .zip(domain.binders())
            .all(|(actual, expected)| actual == &expected.display_name);
    if !binders_match {
        return Err(StructuredEquationOwnerError::RegularBinderMismatch {
            partition,
            family: family_index,
        });
    }
    if !matches!(
        family
            .template
            .as_ref()
            .map(|template| template.scalar_view),
        Some(ComprehensionScalarView::BinderSubstitution)
    ) {
        return Err(
            StructuredEquationOwnerError::RegularRequiresBinderSubstitution {
                partition,
                family: family_index,
            },
        );
    }
    let accesses = validate_regular_accesses(partition, family_index, regular, variables, domain)?;
    Ok(Some(CheckedRegularFamily {
        descriptor: regular,
        accesses: accesses.into_boxed_slice(),
    }))
}

fn validate_regular_accesses<'model>(
    partition: EquationPartitionKind,
    family_index: usize,
    regular: &'model RegularForFamily,
    variables: &'model crate::VarNameIndexMap<Variable>,
    domain: &ValidStructuredIndexDomain<'_>,
) -> Result<Vec<CheckedRegularAccess<'model>>, StructuredEquationOwnerError> {
    let mut checked = Vec::with_capacity(regular.accesses.len());
    for (access_index, access) in regular.accesses.iter().enumerate() {
        if access.var.is_empty() || access.subscripts.is_empty() {
            return Err(StructuredEquationOwnerError::EmptyRegularAccess {
                partition,
                family: family_index,
                access: access_index,
            });
        }
        if access
            .subscripts
            .iter()
            .any(|subscript| subscript.coeffs.len() != domain.rank())
        {
            return Err(
                StructuredEquationOwnerError::RegularAccessCoefficientRankMismatch {
                    partition,
                    family: family_index,
                    access: access_index,
                },
            );
        }
        let variable = variables.get(&VarName::new(&access.var)).ok_or(
            StructuredEquationOwnerError::RegularAccessMissingVariable {
                partition,
                family: family_index,
                access: access_index,
            },
        )?;
        if access.subscripts.len() != variable.dims.len() {
            return Err(StructuredEquationOwnerError::RegularAccessRankMismatch {
                partition,
                family: family_index,
                access: access_index,
            });
        }
        for (axis, (affine, extent)) in access.subscripts.iter().zip(&variable.dims).enumerate() {
            if !affine_form_fits_domain(affine, domain, *extent) {
                return Err(StructuredEquationOwnerError::RegularAccessOutOfBounds {
                    partition,
                    family: family_index,
                    access: access_index,
                    axis,
                });
            }
        }
        checked.push(CheckedRegularAccess {
            descriptor: access,
            variable,
        });
    }
    Ok(checked)
}

fn affine_form_fits_domain(
    affine: &rumoca_core::AffineForm,
    domain: &ValidStructuredIndexDomain<'_>,
    extent: i64,
) -> bool {
    let Some((minimum, maximum)) = affine.coeffs.iter().zip(domain.binders()).try_fold(
        (i128::from(affine.constant), i128::from(affine.constant)),
        |(minimum, maximum), (coefficient, binder)| {
            let first = i128::from(*coefficient).checked_mul(i128::from(binder.lower))?;
            let last = i128::from(*coefficient).checked_mul(i128::from(binder.upper))?;
            Some((
                minimum.checked_add(first.min(last))?,
                maximum.checked_add(first.max(last))?,
            ))
        },
    ) else {
        return false;
    };
    minimum >= 1 && maximum <= i128::from(extent)
}

fn build_family_owner<'model>(
    partition: EquationPartitionKind,
    family_index: usize,
    family: &'model StructuredEquationFamily,
    equations: &'model [Equation],
    rows: Range<usize>,
    domain: ValidStructuredIndexDomain<'model>,
    regular: Option<CheckedRegularFamily<'model>>,
) -> Result<CheckedEquationOwner<'model>, StructuredEquationOwnerError> {
    let Some(template) = &family.template else {
        return Err(StructuredEquationOwnerError::MissingCanonicalTemplate {
            partition,
            family: family_index,
        });
    };
    let row_representation = if family.interiors_materialized {
        CheckedTemplateRows::Complete { regular }
    } else {
        let regular = regular.ok_or(
            StructuredEquationOwnerError::NonMaterializedWithoutRegularProof {
                partition,
                family: family_index,
            },
        )?;
        CheckedTemplateRows::RegularConstructionSample { regular }
    };
    Ok(CheckedEquationOwner::Template(CheckedTemplateFamily {
        family_index,
        family,
        template,
        domain,
        equations: &equations[rows.clone()],
        rows,
        row_representation,
    }))
}

fn claim_family_rows(
    partition: EquationPartitionKind,
    claimed: &mut [Option<usize>],
    owner: &CheckedEquationOwner<'_>,
) -> Result<(), StructuredEquationOwnerError> {
    let (family, rows) = match owner {
        CheckedEquationOwner::Standalone(_) => return Ok(()),
        CheckedEquationOwner::Template(owner) => (owner.family_index, owner.rows()),
    };
    for row in rows {
        if let Some(prior_family) = claimed[row].replace(family) {
            return Err(StructuredEquationOwnerError::OverlappingFamilies {
                partition,
                family,
                prior_family,
                row,
            });
        }
    }
    Ok(())
}

fn merge_standalone_rows<'model>(
    equations: &'model [Equation],
    structured: Vec<CheckedEquationOwner<'model>>,
) -> Vec<CheckedEquationOwner<'model>> {
    let mut owners = Vec::with_capacity(equations.len() + structured.len());
    let mut row = 0;
    for owner in structured {
        let start = owner.first_row();
        while row < start {
            owners.push(CheckedEquationOwner::Standalone(CheckedStandaloneRow {
                row,
                equation: &equations[row],
            }));
            row += 1;
        }
        row = match &owner {
            CheckedEquationOwner::Template(family) => row.max(family.rows.end),
            CheckedEquationOwner::Standalone(_) => row,
        };
        owners.push(owner);
    }
    while row < equations.len() {
        owners.push(CheckedEquationOwner::Standalone(CheckedStandaloneRow {
            row,
            equation: &equations[row],
        }));
        row += 1;
    }
    owners
}

fn collect_semantic_expression_roots<'model>(
    owners: &[CheckedEquationOwner<'model>],
) -> Vec<&'model Expression> {
    let mut roots = Vec::new();
    for owner in owners {
        match owner {
            CheckedEquationOwner::Standalone(row) => roots.push(&row.equation.residual),
            CheckedEquationOwner::Template(family) => roots.extend(&family.template.body),
        }
    }
    roots
}
