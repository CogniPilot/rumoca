// SPEC_0021 file-size exception - split plan: split the derivation walk by construction owner (scalar-JVP derivation into structural_pattern/scalar_jvp.rs, seed/output dependency derivation into structural_pattern/dependency.rs, wire records into structural_pattern/wire.rs), leaving construction + provenance here; tracked as the pattern-authority follow-up slice (SPEC_0021 follow-up).
use std::cmp::Reverse;
use std::collections::BTreeSet;

use rumoca_core::{Span, StructuredIndexDomain};
use serde::{Deserialize, Deserializer, Serialize};

use crate::{
    AffineStencilLoadStride, BinaryOp, LinearOp, Reg, ScalarProgramBlock, TensorOutputMap,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PatternDerivation {
    ConservativeFull,
    DependencyPropagation,
    TensorOperand,
    AffineDomain,
    ComplexLaneExpansion,
}

/// Source-backed evidence for one derived pattern.
///
/// SPEC_0039 / SOLVE-C17 authority closure. A provenance value is only
/// evidence *about* a pattern; the soundness of a pattern is decided by which
/// constructor issued its rows, not by the label carried here. This type
/// therefore derives neither `Default` nor `Deserialize`, so a dummy span can
/// never be laundered in through a decoder (wire data reaches it through
/// `PatternProvenanceWire`), and [`PatternProvenance::derived`] stays public
/// only because every publicly reachable constructor that accepts an arbitrary
/// provenance produces a *conservative* pattern: [`StructuralPattern::full`]
/// over-approximates every relation and cannot omit a true edge whatever label
/// a caller attaches.
///
/// Every pattern that an *in-process* caller can obtain and that is allowed to
/// omit edges is issued by this module from a checked semantic owner:
/// [`StructuralPattern::derive_from_scalar_jvp`] walks the checked
/// [`ScalarProgramBlock`] itself, and [`StructuralPattern::project`] restricts
/// an already-certified pattern. No caller can hand either of them a dependency
/// row.
///
/// EXACT REMAINING SCOPE (SOLVE-C17, not closed here): decoding is a second
/// entry point, and this module cannot certify it alone. `Deserialize` for
/// [`StructuralPattern`] replays a caller-supplied representation through the
/// checked local constructors, so a forged wire may still assert a *sparse*
/// relation that omits a true edge; local replay proves shape and CSR
/// integrity only. The semantic owner of those patterns is the containing
/// [`crate::ComputeNode`] operand program (`lhs_ops`/`rhs_ops`/`setup_ops`),
/// which is not visible from here, so the durable fix belongs to that
/// container: drop the pattern fields from the node wire and rederive them
/// from the decoded operand owner. Until that lands, treat a decoded pattern
/// as untrusted evidence, not as a certificate.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct PatternProvenance {
    derivation: PatternDerivation,
    span: Span,
}

impl PatternProvenance {
    pub fn derived(
        derivation: PatternDerivation,
        span: Span,
    ) -> Result<Self, StructuralPatternError> {
        if span.is_dummy() {
            return Err(StructuralPatternError::MissingProvenance);
        }
        Ok(Self { derivation, span })
    }

    pub const fn derivation(self) -> PatternDerivation {
        self.derivation
    }

    pub const fn span(self) -> Span {
        self.span
    }
}

/// A structural relation certified by whichever constructor issued it.
///
/// SPEC_0039 / SOLVE-C17 authority closure, pinned by compile-fail evidence
/// rather than by comment. A dependent crate compiled as production sees no
/// route to a sparse relation of its own choosing.
///
/// `from_row_dependencies` exists only under the `pattern-fixtures` feature,
/// which production dependency graphs do not enable. The manifest and
/// architecture gates enforce that feature boundary. Independently of the
/// active feature set, a caller cannot reach the edge-omitting storage
/// constructors directly; they are private so that only this module's
/// exhaustive derivations can select them:
///
/// ```compile_fail
/// use rumoca_ir_solve::{PatternDerivation, PatternProvenance, StructuralPattern};
/// use rumoca_core::{SourceId, Span};
///
/// let provenance = PatternProvenance::derived(
///     PatternDerivation::DependencyPropagation,
///     Span::from_offsets(SourceId::from_source_name("forgery.mo"), 0, 1),
/// )
/// .unwrap();
/// let forged = StructuralPattern::csr(1, 1, [0, 0], [], provenance).unwrap();
/// assert!(!forged.contains(0, 0));
/// ```
///
/// What remains publicly constructible is [`StructuralPattern::full`], which
/// over-approximates every relation and is therefore always sound. This
/// companion example uses the same imports and provenance as the two blocks
/// above, so their failures are attributable to the missing constructors and
/// not to an unrelated compile error:
///
/// ```
/// use rumoca_ir_solve::{PatternDerivation, PatternProvenance, StructuralPattern};
/// use rumoca_core::{SourceId, Span};
///
/// let provenance = PatternProvenance::derived(
///     PatternDerivation::ConservativeFull,
///     Span::from_offsets(SourceId::from_source_name("forgery.mo"), 0, 1),
/// )
/// .unwrap();
/// let conservative = StructuralPattern::full(1, 1, provenance).unwrap();
/// assert!(conservative.contains(0, 0));
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct StructuralPattern {
    rows: u32,
    columns: u32,
    representation: PatternRepresentation,
    provenance: PatternProvenance,
}

/// SPEC_0039: wire data decodes through [`PatternRepresentationWire`] via the
/// checked pattern constructors, which validate shape and local integrity.
/// For every pattern reachable by an in-process caller, semantic truth of a
/// *sparse* relation is established at construction by the exhaustive
/// derivations in this module, never by a caller-supplied row set. A decoded
/// representation is not covered by that guarantee — see the remaining-scope
/// paragraph on [`PatternProvenance`].
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
enum PatternRepresentation {
    Empty,
    Full,
    Diagonal,
    Banded {
        lower_bandwidth: u32,
        upper_bandwidth: u32,
    },
    Csr {
        row_offsets: Box<[u32]>,
        column_indices: Box<[u32]>,
    },
    Affine {
        domain: StructuredIndexDomain,
        row_start: u32,
        column_maps: Box<[AffineColumnMap]>,
    },
}

/// One seed coordinate propagated affinely across a compact tensor domain.
/// Strides are expressed in domain ordinals, not Modelica binder values.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct AffineColumnMap {
    start: u32,
    strides: Box<[isize]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StructuralPatternView<'pattern> {
    Empty,
    Full,
    Diagonal,
    Banded {
        lower_bandwidth: u32,
        upper_bandwidth: u32,
    },
    Csr {
        row_offsets: &'pattern [u32],
        column_indices: &'pattern [u32],
    },
    Affine {
        domain_rank: usize,
        access_count: usize,
    },
}

/// A deterministic coloring certified against one structural pattern.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnColoring {
    column_count: u32,
    groups: Box<[Box<[u32]>]>,
}

impl ColumnColoring {
    pub const fn column_count(&self) -> u32 {
        self.column_count
    }

    pub fn groups(&self) -> &[Box<[u32]>] {
        &self.groups
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StructuralPatternError {
    MissingProvenance,
    DimensionOverflow,
    DependencyRowCount {
        expected: usize,
        found: usize,
    },
    InvalidRowOffsetCount {
        expected: usize,
        found: usize,
    },
    FirstRowOffsetNotZero {
        found: u32,
    },
    NonMonotoneRowOffsets {
        row: usize,
    },
    FinalRowOffsetMismatch {
        expected: usize,
        found: u32,
    },
    ColumnOutOfBounds {
        row: usize,
        column: u32,
        columns: u32,
    },
    ColumnsNotStrictlyIncreasing {
        row: usize,
    },
    /// A checked owner failed its own derivation contract while this module
    /// was deriving the relation from it.
    DependencyContract {
        message: String,
        span: Option<Span>,
    },
    /// The derivation reached a register the owner never defined, so no
    /// dependency fact exists for it.
    UninitializedRegister {
        register: Reg,
        span: Option<Span>,
    },
}

impl std::fmt::Display for StructuralPatternError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingProvenance => {
                formatter.write_str("structural pattern requires source-backed provenance")
            }
            Self::DimensionOverflow => {
                formatter.write_str("structural pattern dimensions exceed u32")
            }
            Self::DependencyRowCount { expected, found } => write!(
                formatter,
                "structural dependency row count is {found}, expected {expected}"
            ),
            Self::InvalidRowOffsetCount { expected, found } => write!(
                formatter,
                "CSR row-offset count is {found}, expected {expected}"
            ),
            Self::FirstRowOffsetNotZero { found } => {
                write!(formatter, "CSR first row offset is {found}, expected zero")
            }
            Self::NonMonotoneRowOffsets { row } => {
                write!(formatter, "CSR row offsets decrease at row {row}")
            }
            Self::FinalRowOffsetMismatch { expected, found } => write!(
                formatter,
                "CSR final row offset is {found}, expected {expected}"
            ),
            Self::ColumnOutOfBounds {
                row,
                column,
                columns,
            } => write!(
                formatter,
                "CSR row {row} contains column {column} outside 0..{columns}"
            ),
            Self::ColumnsNotStrictlyIncreasing { row } => {
                write!(
                    formatter,
                    "CSR columns are not strictly increasing in row {row}"
                )
            }
            Self::DependencyContract { message, .. } => formatter.write_str(message),
            Self::UninitializedRegister { register, .. } => write!(
                formatter,
                "structural dependency derivation read uninitialized register {register}"
            ),
        }
    }
}

impl std::error::Error for StructuralPatternError {}

fn validate_affine_jvp_domain(
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
    rows: usize,
    owner_span: Span,
) -> Result<(), StructuralPatternError> {
    if owner_span.is_dummy() {
        return Err(dependency_error(
            "affine Jacobian sparsity requires source-backed owner provenance",
            None,
        ));
    }
    let dense_output =
        TensorOutputMap::dense_contiguous(output_map.start, domain).map_err(|error| {
            dependency_error(
                format!("invalid affine output domain: {error:?}"),
                Some(owner_span),
            )
        })?;
    if output_map != &dense_output {
        return Err(dependency_error(
            "affine Jacobian output map is not dense-contiguous",
            Some(owner_span),
        ));
    }
    let expected_rows = output_map.output_count(domain).map_err(|error| {
        dependency_error(
            format!("invalid affine output map: {error:?}"),
            Some(owner_span),
        )
    })?;
    if expected_rows != rows {
        return Err(dependency_error(
            format!(
                "affine Jacobian row extent {rows} does not match output extent {expected_rows}"
            ),
            Some(owner_span),
        ));
    }
    Ok(())
}

fn derive_affine_column_maps(
    dependencies: &BTreeSet<usize>,
    seed_positions: &[usize],
    base_ops: &[LinearOp],
    load_strides: &[AffineStencilLoadStride],
    dimension_count: usize,
    owner_span: Span,
) -> Result<Vec<AffineColumnMap>, StructuralPatternError> {
    dependencies
        .iter()
        .map(|&position| {
            if !seed_positions.contains(&position) {
                return Err(dependency_error(
                    format!("affine dependency {position} is not a seed-load owner"),
                    Some(owner_span),
                ));
            }
            let Some(LinearOp::LoadSeed { index: start, .. }) = base_ops.get(position) else {
                return Err(dependency_error(
                    format!("affine dependency operation {position} is not LoadSeed"),
                    Some(owner_span),
                ));
            };
            Ok(AffineColumnMap {
                start: checked_dimension(*start)?,
                strides: derive_affine_strides(
                    position,
                    load_strides,
                    dimension_count,
                    owner_span,
                )?
                .into_boxed_slice(),
            })
        })
        .collect()
}

fn derive_affine_strides(
    position: usize,
    load_strides: &[AffineStencilLoadStride],
    dimension_count: usize,
    owner_span: Span,
) -> Result<Vec<isize>, StructuralPatternError> {
    let mut strides = vec![0isize; dimension_count];
    for descriptor in load_strides
        .iter()
        .filter(|descriptor| descriptor.op_position == position)
    {
        for term in &descriptor.terms {
            let target = strides.get_mut(term.dimension).ok_or_else(|| {
                dependency_error(
                    format!(
                        "affine seed stride dimension {} is outside 0..{dimension_count}",
                        term.dimension
                    ),
                    Some(owner_span),
                )
            })?;
            *target = target.checked_add(term.stride).ok_or_else(|| {
                dependency_error(
                    "affine seed stride accumulation overflowed",
                    Some(owner_span),
                )
            })?;
        }
    }
    Ok(strides)
}

impl StructuralPattern {
    fn empty(
        rows: usize,
        columns: usize,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(rows, columns, PatternRepresentation::Empty, provenance)
    }

    pub fn full(
        rows: usize,
        columns: usize,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(rows, columns, PatternRepresentation::Full, provenance)
    }

    fn diagonal(
        rows: usize,
        columns: usize,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(rows, columns, PatternRepresentation::Diagonal, provenance)
    }

    fn banded(
        rows: usize,
        columns: usize,
        lower_bandwidth: u32,
        upper_bandwidth: u32,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::new(
            rows,
            columns,
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            },
            provenance,
        )
    }

    fn csr(
        rows: usize,
        columns: usize,
        row_offsets: impl Into<Box<[u32]>>,
        column_indices: impl Into<Box<[u32]>>,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        let rows = checked_dimension(rows)?;
        let columns = checked_dimension(columns)?;
        let row_offsets = row_offsets.into();
        let column_indices = column_indices.into();
        validate_csr(rows, columns, &row_offsets, &column_indices)?;
        Ok(Self {
            rows,
            columns,
            representation: PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            },
            provenance,
        })
    }

    fn new(
        rows: usize,
        columns: usize,
        representation: PatternRepresentation,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Ok(Self {
            rows: checked_dimension(rows)?,
            columns: checked_dimension(columns)?,
            representation,
            provenance,
        })
    }

    /// Derive the exact compact dependency relation of one checked affine JVP
    /// tensor owner without materializing any domain point or scalar row.
    pub fn derive_from_affine_jvp(
        domain: &StructuredIndexDomain,
        output_map: &TensorOutputMap,
        base_ops: &[LinearOp],
        load_strides: &[AffineStencilLoadStride],
        rows: usize,
        columns: usize,
        owner_span: Span,
    ) -> Result<Self, StructuralPatternError> {
        validate_affine_jvp_domain(domain, output_map, rows, owner_span)?;

        let mut symbolic_ops = base_ops.to_vec();
        let mut seed_positions = Vec::new();
        for (position, operation) in symbolic_ops.iter_mut().enumerate() {
            if let LinearOp::LoadSeed { index, .. } = operation {
                *index = position;
                seed_positions.push(position);
            }
        }
        let outputs = program_output_dependencies(&symbolic_ops, Some(owner_span))?;
        let [dependencies] = outputs.as_slice() else {
            return Err(dependency_error(
                format!(
                    "affine Jacobian base program must produce exactly one output, found {}",
                    outputs.len()
                ),
                Some(owner_span),
            ));
        };
        let DependencyState::Known(dependencies) = dependencies;
        let mut column_maps = derive_affine_column_maps(
            dependencies,
            &seed_positions,
            base_ops,
            load_strides,
            domain.binders.len(),
            owner_span,
        )?;
        column_maps.sort_by(|lhs, rhs| {
            (lhs.start, lhs.strides.as_ref()).cmp(&(rhs.start, rhs.strides.as_ref()))
        });
        column_maps.dedup();
        let provenance = PatternProvenance::derived(PatternDerivation::AffineDomain, owner_span)?;
        Self::checked_affine(
            rows,
            columns,
            domain.clone(),
            checked_dimension(output_map.start)?,
            column_maps.into_boxed_slice(),
            provenance,
        )
    }

    fn checked_affine(
        rows: usize,
        columns: usize,
        domain: StructuredIndexDomain,
        row_start: u32,
        column_maps: Box<[AffineColumnMap]>,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        let checked_rows = checked_dimension(rows)?;
        let checked_columns = checked_dimension(columns)?;
        validate_affine_pattern(
            &domain,
            row_start,
            &column_maps,
            checked_rows,
            checked_columns,
            Some(provenance.span()),
        )?;
        if domain.scalar_count().map_err(|error| {
            dependency_error(
                format!("invalid affine domain: {error}"),
                Some(provenance.span()),
            )
        })? == 0
            || column_maps.is_empty()
        {
            return Self::empty(rows, columns, provenance);
        }
        Ok(Self {
            rows: checked_rows,
            columns: checked_columns,
            representation: PatternRepresentation::Affine {
                domain,
                row_start,
                column_maps,
            },
            provenance,
        })
    }

    pub const fn rows(&self) -> u32 {
        self.rows
    }

    pub const fn columns(&self) -> u32 {
        self.columns
    }

    pub const fn provenance(&self) -> PatternProvenance {
        self.provenance
    }

    pub fn view(&self) -> StructuralPatternView<'_> {
        match &self.representation {
            PatternRepresentation::Empty => StructuralPatternView::Empty,
            PatternRepresentation::Full => StructuralPatternView::Full,
            PatternRepresentation::Diagonal => StructuralPatternView::Diagonal,
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => StructuralPatternView::Banded {
                lower_bandwidth: *lower_bandwidth,
                upper_bandwidth: *upper_bandwidth,
            },
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => StructuralPatternView::Csr {
                row_offsets,
                column_indices,
            },
            PatternRepresentation::Affine {
                domain,
                column_maps,
                ..
            } => StructuralPatternView::Affine {
                domain_rank: domain.binders.len(),
                access_count: column_maps.len(),
            },
        }
    }

    pub fn contains(&self, row: u32, column: u32) -> bool {
        if row >= self.rows || column >= self.columns {
            return false;
        }
        match &self.representation {
            PatternRepresentation::Empty => false,
            PatternRepresentation::Full => true,
            PatternRepresentation::Diagonal => row == column,
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => {
                column.saturating_add(*lower_bandwidth) >= row
                    && row.saturating_add(*upper_bandwidth) >= column
            }
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => {
                let start = row_offsets[row as usize] as usize;
                let end = row_offsets[row as usize + 1] as usize;
                let row_columns = &column_indices[start..end];
                if row_columns.len() <= 8 {
                    row_columns.contains(&column)
                } else {
                    row_columns.binary_search(&column).is_ok()
                }
            }
            PatternRepresentation::Affine {
                domain,
                row_start,
                column_maps,
            } => affine_columns_for_row(domain, *row_start, column_maps, row as usize)
                .is_some_and(|columns| columns.binary_search(&(column as usize)).is_ok()),
        }
    }

    pub fn nonzero_upper_bound(&self) -> Option<usize> {
        let rows = self.rows as usize;
        let columns = self.columns as usize;
        match &self.representation {
            PatternRepresentation::Empty => Some(0),
            PatternRepresentation::Full => rows.checked_mul(columns),
            PatternRepresentation::Diagonal => Some(rows.min(columns)),
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => banded_nonzero_count(
                rows,
                columns,
                *lower_bandwidth as usize,
                *upper_bandwidth as usize,
            ),
            PatternRepresentation::Csr { column_indices, .. } => Some(column_indices.len()),
            PatternRepresentation::Affine {
                domain,
                column_maps,
                ..
            } => domain.scalar_count().ok()?.checked_mul(column_maps.len()),
        }
    }

    /// Materialize the certified relation in deterministic row-major order.
    /// Runtime and backend storage policies may consume this view without
    /// rediscovering dependencies from programs or numerical values.
    pub fn nonzero_coordinates(&self) -> Vec<(usize, usize)> {
        let capacity = self.nonzero_upper_bound().unwrap_or(0);
        let mut coordinates = Vec::with_capacity(capacity);
        for row in 0..self.rows as usize {
            self.visit_row_columns(row, |column| coordinates.push((row, column)));
        }
        coordinates
    }

    /// Visit the certified columns in one row without materializing the
    /// complete sparse relation. Columns are yielded in ascending order.
    #[inline]
    pub fn visit_row_columns(&self, row: usize, mut visitor: impl FnMut(usize)) {
        debug_assert!(row < self.rows as usize);
        match &self.representation {
            PatternRepresentation::Empty => {}
            PatternRepresentation::Full => {
                (0..self.columns as usize).for_each(&mut visitor);
            }
            PatternRepresentation::Diagonal => {
                if row < self.columns as usize {
                    visitor(row);
                }
            }
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => {
                let start = row.saturating_sub(*lower_bandwidth as usize);
                let end = row
                    .saturating_add(*upper_bandwidth as usize)
                    .saturating_add(1)
                    .min(self.columns as usize);
                (start..end).for_each(&mut visitor);
            }
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => {
                let start = row_offsets[row] as usize;
                let end = row_offsets[row + 1] as usize;
                column_indices[start..end]
                    .iter()
                    .for_each(|column| visitor(*column as usize));
            }
            PatternRepresentation::Affine {
                domain,
                row_start,
                column_maps,
            } => {
                if let Some(columns) = affine_columns_for_row(domain, *row_start, column_maps, row)
                {
                    columns.into_iter().for_each(&mut visitor);
                }
            }
        }
    }

    /// Derive the exact structural Jacobian relation of a checked scalar JVP
    /// block by walking the block itself.
    ///
    /// SPEC_0039 / SOLVE-C17: this is the production authority for a sparse
    /// pattern. The semantic owner is the [`ScalarProgramBlock`] — already
    /// checked when it was built — and the exhaustive register-dependency
    /// derivation runs *here*, so no caller can substitute its own row set,
    /// widen a row, or drop an edge. The issued provenance is
    /// [`PatternDerivation::DependencyPropagation`] anchored at the owner span.
    pub fn derive_from_scalar_jvp(
        block: &ScalarProgramBlock,
        rows: usize,
        columns: usize,
        owner_span: Span,
    ) -> Result<Self, StructuralPatternError> {
        let row_dependencies =
            derive_scalar_jvp_row_dependencies(block, rows, columns, owner_span)?;
        let provenance =
            PatternProvenance::derived(PatternDerivation::DependencyPropagation, owner_span)?;
        Self::from_checked_row_dependencies(rows, columns, &row_dependencies, provenance)
    }

    /// Restrict this certified relation to a sub-block of rows and columns.
    ///
    /// SPEC_0039 / SOLVE-C17: the sub-relation is read out of `self`, which is
    /// already certified, so the projected pattern inherits that certification
    /// instead of re-asserting one. Callers choose *which* rows and columns the
    /// sub-block covers; they cannot choose what depends on what.
    pub fn project(
        &self,
        rows: &[usize],
        columns: &[usize],
    ) -> Result<Self, StructuralPatternError> {
        let span = Some(self.provenance.span());
        let source_rows = checked_projection_axis(rows, self.rows, "row", span)?;
        let source_columns = checked_projection_axis(columns, self.columns, "column", span)?;
        let dependencies = source_rows
            .iter()
            .map(|&row| {
                source_columns
                    .iter()
                    .enumerate()
                    .filter_map(|(local_column, &source_column)| {
                        self.contains(row, source_column).then_some(local_column)
                    })
                    .collect()
            })
            .collect::<Vec<Vec<usize>>>();
        Self::from_checked_row_dependencies(
            rows.len(),
            columns.len(),
            &dependencies,
            self.provenance,
        )
    }

    /// Exact seed dependencies of the sole output of one checked scalar row.
    pub fn derive_row_seed_dependencies(
        program: &[LinearOp],
    ) -> Result<Vec<usize>, StructuralPatternError> {
        scalar_row_seed_dependencies(program)
    }

    /// Exact solver-`Y` dependencies of every output of one checked scalar
    /// program, in output order.
    pub fn derive_output_y_dependencies(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Vec<BTreeSet<usize>>, StructuralPatternError> {
        program_output_y_dependencies(program, span)
    }

    /// Exact solver-`P` dependencies of every output of one checked scalar
    /// program, in output order.
    ///
    /// Runtime storage follows immutable model parameters in the same `P`
    /// vector. Consumers that prove a program parameter-static must therefore
    /// compare this construction-derived set with the immutable prefix rather
    /// than treating every `P` load as a constant.
    pub fn derive_output_p_dependencies(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Vec<BTreeSet<usize>>, StructuralPatternError> {
        program_output_dependencies_with_fold(
            program,
            span,
            None,
            None,
            None,
            DependencySource::SolverP,
        )?
        .into_iter()
        .map(|dependencies| {
            let DependencyState::Known(indices) = dependencies;
            Ok(indices)
        })
        .collect()
    }

    /// Whether each output of one checked scalar program depends on an AD
    /// seed input.
    pub fn derive_output_seed_dependencies(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Vec<bool>, StructuralPatternError> {
        derive_output_dependency_presence(program, span, DependencySource::Seed)
    }

    /// Whether each output of one checked scalar program contains an opaque
    /// runtime operation such as table access or random-state evaluation.
    ///
    /// The dependency walk deliberately treats these operations as an effect
    /// even when their explicit register inputs are constant. This lets cache
    /// certificates fail closed without making consumers rediscover nested
    /// function/fold control flow.
    pub fn derive_output_effect_dependencies(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Vec<bool>, StructuralPatternError> {
        derive_output_dependency_presence(program, span, DependencySource::Effect)
    }

    /// Whether each output of one checked scalar program depends on time.
    ///
    /// This uses the same exhaustive compact-operation walk as solver-`Y`
    /// dependency derivation, so tensor and typed-call inputs remain one
    /// construction-owned proof instead of being rediscovered by consumers.
    pub fn derive_output_time_dependencies(
        program: &[LinearOp],
        span: Option<Span>,
    ) -> Result<Vec<bool>, StructuralPatternError> {
        derive_output_dependency_presence(program, span, DependencySource::Time)
    }

    /// Non-production fixture constructor.
    ///
    /// SPEC_0039 / SOLVE-C17: tests and backend benchmarks need a pattern with
    /// a chosen shape and no semantic owner to derive it from. This surface
    /// exists only under `cfg(test)` inside this crate and under the
    /// `pattern-fixtures` feature, which the workspace enables solely through
    /// `[dev-dependencies]`; a production build of any dependent crate cannot
    /// reach it. `caller_supplied_rows_stay_behind_the_fixture_gate` pins the
    /// gate on this declaration and `pattern_fixture_feature_is_dev_only` pins
    /// the workspace wiring that keeps the gate off in production.
    #[cfg(any(test, feature = "pattern-fixtures"))]
    pub fn from_row_dependencies(
        rows: usize,
        columns: usize,
        row_dependencies: &[Vec<usize>],
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        Self::from_checked_row_dependencies(rows, columns, row_dependencies, provenance)
    }

    /// Canonicalize dependency facts this module derived itself.
    ///
    /// Private on purpose: the facts must come from an exhaustive derivation
    /// over a checked owner, never from a caller.
    fn from_checked_row_dependencies(
        rows: usize,
        columns: usize,
        row_dependencies: &[Vec<usize>],
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        if row_dependencies.len() != rows {
            return Err(StructuralPatternError::DependencyRowCount {
                expected: rows,
                found: row_dependencies.len(),
            });
        }
        let checked_rows = checked_dimension(rows)?;
        let checked_columns = checked_dimension(columns)?;
        let mut row_offsets = Vec::with_capacity(rows.saturating_add(1));
        let mut column_indices = Vec::new();
        row_offsets.push(0);
        let mut diagonal = true;
        let mut full = true;
        for (row, dependencies) in row_dependencies.iter().enumerate() {
            let mut canonical = dependencies.clone();
            canonical.sort_unstable();
            canonical.dedup();
            diagonal &= canonical
                .iter()
                .all(|column| *column == row && row < columns);
            full &= canonical.len() == columns && canonical.iter().copied().eq(0..columns);
            append_checked_columns(row, canonical, checked_columns, &mut column_indices)?;
            row_offsets.push(
                u32::try_from(column_indices.len())
                    .map_err(|_| StructuralPatternError::DimensionOverflow)?,
            );
        }
        if column_indices.is_empty() {
            return Self::empty(rows, columns, provenance);
        }
        if diagonal {
            return Self::diagonal(rows, columns, provenance);
        }
        if full {
            return Self::full(rows, columns, provenance);
        }
        validate_csr(checked_rows, checked_columns, &row_offsets, &column_indices)?;
        Ok(Self {
            rows: checked_rows,
            columns: checked_columns,
            representation: PatternRepresentation::Csr {
                row_offsets: row_offsets.into_boxed_slice(),
                column_indices: column_indices.into_boxed_slice(),
            },
            provenance,
        })
    }

    pub fn column_rows(&self) -> Vec<Vec<usize>> {
        let mut columns = vec![Vec::new(); self.columns as usize];
        match &self.representation {
            PatternRepresentation::Empty => {}
            PatternRepresentation::Full => {
                for column in &mut columns {
                    column.extend(0..self.rows as usize);
                }
            }
            PatternRepresentation::Diagonal => {
                for index in 0..(self.rows.min(self.columns) as usize) {
                    columns[index].push(index);
                }
            }
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => append_banded_column_rows(
                &mut columns,
                self.rows as usize,
                *lower_bandwidth as usize,
                *upper_bandwidth as usize,
            ),
            PatternRepresentation::Csr {
                row_offsets,
                column_indices,
            } => append_csr_column_rows(&mut columns, row_offsets, column_indices),
            PatternRepresentation::Affine { .. } => {
                for row in 0..self.rows as usize {
                    self.visit_row_columns(row, |column| columns[column].push(row));
                }
            }
        }
        columns
    }

    pub fn column_coloring(&self) -> ColumnColoring {
        match &self.representation {
            PatternRepresentation::Empty | PatternRepresentation::Diagonal => {
                return coloring_for_nonconflicting_columns(self.columns);
            }
            PatternRepresentation::Full if self.rows == 0 => {
                return coloring_for_nonconflicting_columns(self.columns);
            }
            PatternRepresentation::Full => return coloring_for_full_columns(self.columns),
            PatternRepresentation::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => {
                return coloring_for_banded_columns(
                    self.columns,
                    *lower_bandwidth,
                    *upper_bandwidth,
                );
            }
            PatternRepresentation::Csr { .. } | PatternRepresentation::Affine { .. } => {}
        }
        let column_rows = self.column_rows();
        let mut order: Vec<usize> = (0..column_rows.len()).collect();
        order.sort_by_key(|column| (Reverse(column_rows[*column].len()), *column));

        let mut groups: Vec<Vec<u32>> = Vec::new();
        let mut occupied_rows: Vec<BTreeSet<usize>> = Vec::new();
        for column in order {
            let rows = &column_rows[column];
            if let Some((group_index, occupied)) = occupied_rows
                .iter_mut()
                .enumerate()
                .find(|(_, occupied)| rows.iter().all(|row| !occupied.contains(row)))
            {
                groups[group_index].push(column as u32);
                occupied.extend(rows);
            } else {
                groups.push(vec![column as u32]);
                occupied_rows.push(rows.iter().copied().collect());
            }
        }
        for group in &mut groups {
            group.sort_unstable();
        }
        groups.sort_by_key(|group| group.first().copied().unwrap_or(u32::MAX));
        ColumnColoring {
            column_count: self.columns,
            groups: groups
                .into_iter()
                .map(Vec::into_boxed_slice)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        }
    }
}

fn coloring_for_nonconflicting_columns(columns: u32) -> ColumnColoring {
    let groups = if columns == 0 {
        Vec::new()
    } else {
        vec![(0..columns).collect::<Vec<_>>().into_boxed_slice()]
    };
    ColumnColoring {
        column_count: columns,
        groups: groups.into_boxed_slice(),
    }
}

fn coloring_for_full_columns(columns: u32) -> ColumnColoring {
    ColumnColoring {
        column_count: columns,
        groups: (0..columns)
            .map(|column| Box::from([column]))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

fn coloring_for_banded_columns(columns: u32, lower: u32, upper: u32) -> ColumnColoring {
    let color_count = u64::from(lower)
        .saturating_add(u64::from(upper))
        .saturating_add(1)
        .min(u64::from(columns)) as usize;
    let groups = (0..color_count)
        .map(|color| {
            (color..columns as usize)
                .step_by(color_count)
                .map(|column| column as u32)
                .collect::<Vec<_>>()
                .into_boxed_slice()
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    ColumnColoring {
        column_count: columns,
        groups,
    }
}

fn validate_affine_pattern(
    domain: &StructuredIndexDomain,
    row_start: u32,
    column_maps: &[AffineColumnMap],
    rows: u32,
    columns: u32,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    let point_count = domain
        .scalar_count()
        .map_err(|error| dependency_error(format!("invalid affine domain: {error}"), span))?;
    let row_end = (row_start as usize)
        .checked_add(point_count)
        .ok_or_else(|| dependency_error("affine row extent overflows", span))?;
    if row_end > rows as usize {
        return Err(dependency_error(
            format!("affine rows {row_start}..{row_end} exceed 0..{rows}"),
            span,
        ));
    }
    let extents = domain
        .extents()
        .map_err(|error| dependency_error(format!("invalid affine domain: {error}"), span))?;
    for map in column_maps {
        if map.strides.len() != extents.len() {
            return Err(dependency_error(
                format!(
                    "affine column map rank {} does not match domain rank {}",
                    map.strides.len(),
                    extents.len()
                ),
                span,
            ));
        }
        let mut minimum = i128::from(map.start);
        let mut maximum = minimum;
        for (&extent, &stride) in extents.iter().zip(map.strides.iter()) {
            let last = i128::try_from(extent.saturating_sub(1))
                .map_err(|_| dependency_error("affine extent exceeds arithmetic range", span))?;
            let offset = last
                .checked_mul(stride as i128)
                .ok_or_else(|| dependency_error("affine column bound overflowed", span))?;
            if offset < 0 {
                minimum = minimum
                    .checked_add(offset)
                    .ok_or_else(|| dependency_error("affine column bound overflowed", span))?;
            } else {
                maximum = maximum
                    .checked_add(offset)
                    .ok_or_else(|| dependency_error("affine column bound overflowed", span))?;
            }
        }
        if minimum < 0 || maximum >= i128::from(columns) {
            return Err(dependency_error(
                format!("affine column range {minimum}..={maximum} exceeds 0..{columns}"),
                span,
            ));
        }
    }
    Ok(())
}

fn affine_columns_for_row(
    domain: &StructuredIndexDomain,
    row_start: u32,
    column_maps: &[AffineColumnMap],
    row: usize,
) -> Option<Vec<usize>> {
    let ordinal = row.checked_sub(row_start as usize)?;
    let extents = domain.extents().ok()?;
    let point_count = extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent))?;
    if ordinal >= point_count {
        return None;
    }
    let mut remainder = ordinal;
    let mut positions = vec![0usize; extents.len()];
    for (position, extent) in positions.iter_mut().zip(extents.iter()).rev() {
        if *extent == 0 {
            return None;
        }
        *position = remainder % *extent;
        remainder /= *extent;
    }
    let mut columns = Vec::with_capacity(column_maps.len());
    for map in column_maps {
        let mut column = i128::from(map.start);
        for (&position, &stride) in positions.iter().zip(map.strides.iter()) {
            column = column.checked_add((position as i128).checked_mul(stride as i128)?)?;
        }
        columns.push(usize::try_from(column).ok()?);
    }
    columns.sort_unstable();
    columns.dedup();
    Some(columns)
}

/// Exhaustive structural-Jacobian derivation over the checked semantic owner.
///
/// Every row of the result is produced by walking the owner's own programs;
/// rows the checked sparse output map identifies as holes have no producing
/// operation and are therefore structurally empty.
fn derive_scalar_jvp_row_dependencies(
    block: &ScalarProgramBlock,
    rows: usize,
    columns: usize,
    owner_span: Span,
) -> Result<Vec<Vec<usize>>, StructuralPatternError> {
    if owner_span.is_dummy() {
        return Err(dependency_error(
            "Jacobian sparsity requires source-backed owner provenance",
            None,
        ));
    }
    if block.output_count() != rows {
        return Err(dependency_error(
            format!(
                "Jacobian row extent {rows} does not match checked sparse output extent {}",
                block.output_count()
            ),
            Some(owner_span),
        ));
    }

    let mut row_dependencies = vec![None; rows];
    let mut output_ordinal = 0usize;
    for (program_index, program) in block.programs().iter().enumerate() {
        let span = block.program_span(program_index).or(Some(owner_span));
        for dependencies in program_output_dependencies(program, span)? {
            let output_index = *block.output_indices().get(output_ordinal).ok_or_else(|| {
                dependency_error(
                    format!(
                        "Jacobian sparsity output {output_ordinal} has no checked output identity"
                    ),
                    span,
                )
            })?;
            let slot = row_dependencies.get_mut(output_index).ok_or_else(|| {
                dependency_error(
                    format!("Jacobian sparsity output index {output_index} is outside 0..{rows}"),
                    span,
                )
            })?;
            if slot.is_some() {
                return Err(dependency_error(
                    format!("Jacobian sparsity output index {output_index} is produced twice"),
                    span,
                ));
            }
            let dependencies = dependencies.into_set();
            if let Some(index) = dependencies.iter().find(|index| **index >= columns) {
                return Err(dependency_error(
                    format!("Jacobian seed index {index} is outside 0..{columns}"),
                    span,
                ));
            }
            *slot = Some(dependencies.into_iter().collect());
            output_ordinal = output_ordinal.checked_add(1).ok_or_else(|| {
                dependency_error("Jacobian output ordinal overflows host index range", span)
            })?;
        }
    }
    if output_ordinal != block.output_indices().len() {
        return Err(dependency_error(
            format!(
                "Jacobian emitted {output_ordinal} outputs but carries {} output identities",
                block.output_indices().len()
            ),
            Some(owner_span),
        ));
    }
    Ok(row_dependencies
        .into_iter()
        // An interior hole is explicitly identified by the checked sparse
        // output map and therefore has no producing operation or edge.
        .map(Option::unwrap_or_default)
        .collect())
}

fn append_checked_columns(
    row: usize,
    canonical: Vec<usize>,
    columns: u32,
    output: &mut Vec<u32>,
) -> Result<(), StructuralPatternError> {
    for column in canonical {
        let column = checked_dimension(column)?;
        if column >= columns {
            return Err(StructuralPatternError::ColumnOutOfBounds {
                row,
                column,
                columns,
            });
        }
        output.push(column);
    }
    Ok(())
}

/// Check one projection axis against the source extent before it is used.
///
/// SPEC_0039 / SOLVE-C17: a projection selects a sub-block; it never widens the
/// relation. Validating the whole axis up front keeps that check in one place
/// and out of the membership walk.
fn checked_projection_axis(
    indices: &[usize],
    extent: u32,
    axis: &str,
    span: Option<Span>,
) -> Result<Vec<u32>, StructuralPatternError> {
    indices
        .iter()
        .map(|&index| {
            let index = u32::try_from(index)
                .map_err(|_| dependency_error(format!("projection {axis} exceeds u32"), span))?;
            if index >= extent {
                return Err(dependency_error(
                    format!("projection {axis} {index} is outside 0..{extent}"),
                    span,
                ));
            }
            Ok(index)
        })
        .collect()
}

fn append_banded_column_rows(
    columns: &mut [Vec<usize>],
    rows: usize,
    lower_bandwidth: usize,
    upper_bandwidth: usize,
) {
    for row in 0..rows {
        let start = row.saturating_sub(lower_bandwidth);
        let end = row
            .saturating_add(upper_bandwidth)
            .saturating_add(1)
            .min(columns.len());
        columns[start..end]
            .iter_mut()
            .for_each(|column_rows| column_rows.push(row));
    }
}

fn append_csr_column_rows(columns: &mut [Vec<usize>], row_offsets: &[u32], column_indices: &[u32]) {
    for row in 0..row_offsets.len().saturating_sub(1) {
        let start = row_offsets[row] as usize;
        let end = row_offsets[row + 1] as usize;
        column_indices[start..end]
            .iter()
            .for_each(|column| columns[*column as usize].push(row));
    }
}

fn checked_dimension(value: usize) -> Result<u32, StructuralPatternError> {
    u32::try_from(value).map_err(|_| StructuralPatternError::DimensionOverflow)
}

fn validate_csr(
    rows: u32,
    columns: u32,
    row_offsets: &[u32],
    column_indices: &[u32],
) -> Result<(), StructuralPatternError> {
    let expected = rows as usize + 1;
    if row_offsets.len() != expected {
        return Err(StructuralPatternError::InvalidRowOffsetCount {
            expected,
            found: row_offsets.len(),
        });
    }
    if row_offsets.first().copied() != Some(0) {
        return Err(StructuralPatternError::FirstRowOffsetNotZero {
            found: row_offsets.first().copied().unwrap_or(u32::MAX),
        });
    }
    for row in 0..rows as usize {
        let start = row_offsets[row];
        let end = row_offsets[row + 1];
        if start > end {
            return Err(StructuralPatternError::NonMonotoneRowOffsets { row });
        }
        let entries = column_indices.get(start as usize..end as usize).ok_or(
            StructuralPatternError::FinalRowOffsetMismatch {
                expected: column_indices.len(),
                found: end,
            },
        )?;
        let mut previous = None;
        for &column in entries {
            if column >= columns {
                return Err(StructuralPatternError::ColumnOutOfBounds {
                    row,
                    column,
                    columns,
                });
            }
            if previous.is_some_and(|previous| previous >= column) {
                return Err(StructuralPatternError::ColumnsNotStrictlyIncreasing { row });
            }
            previous = Some(column);
        }
    }
    let found = row_offsets.last().copied().unwrap_or(u32::MAX);
    if found as usize != column_indices.len() {
        return Err(StructuralPatternError::FinalRowOffsetMismatch {
            expected: column_indices.len(),
            found,
        });
    }
    Ok(())
}

fn banded_nonzero_count(
    rows: usize,
    columns: usize,
    lower_bandwidth: usize,
    upper_bandwidth: usize,
) -> Option<usize> {
    (0..rows).try_fold(0usize, |count, row| {
        let start = row.saturating_sub(lower_bandwidth);
        let end = row
            .checked_add(upper_bandwidth)?
            .checked_add(1)?
            .min(columns);
        count.checked_add(end.saturating_sub(start))
    })
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum DependencyState {
    Known(BTreeSet<usize>),
}

#[derive(Clone, Copy)]
enum DependencySource {
    Effect,
    Seed,
    SolverP,
    SolverY,
    Time,
}

fn derive_output_dependency_presence(
    program: &[LinearOp],
    span: Option<Span>,
    source: DependencySource,
) -> Result<Vec<bool>, StructuralPatternError> {
    program_output_dependencies_with_fold(program, span, None, None, None, source)?
        .into_iter()
        .map(|dependencies| {
            let DependencyState::Known(indices) = dependencies;
            Ok(!indices.is_empty())
        })
        .collect()
}

impl DependencyState {
    fn empty() -> Self {
        Self::Known(BTreeSet::new())
    }

    fn singleton(index: usize) -> Self {
        Self::Known(BTreeSet::from([index]))
    }

    fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Known(mut lhs), Self::Known(rhs)) => {
                lhs.extend(rhs);
                Self::Known(lhs)
            }
        }
    }

    fn into_set(self) -> BTreeSet<usize> {
        let Self::Known(indices) = self;
        indices
    }
}

/// Dependencies of the sole output of a scalar row.
fn scalar_row_seed_dependencies(
    program: &[LinearOp],
) -> Result<Vec<usize>, StructuralPatternError> {
    let outputs = program_output_dependencies(program, None)?;
    let [dependencies] = outputs.as_slice() else {
        return Err(dependency_error(
            format!(
                "scalar row must produce exactly one output for dependency analysis, found {}",
                outputs.len()
            ),
            None,
        ));
    };
    let DependencyState::Known(indices) = dependencies;
    Ok(indices.iter().copied().collect())
}

fn program_output_dependencies(
    program: &[LinearOp],
    span: Option<Span>,
) -> Result<Vec<DependencyState>, StructuralPatternError> {
    program_output_dependencies_with_fold(program, span, None, None, None, DependencySource::Seed)
}

fn program_output_y_dependencies(
    program: &[LinearOp],
    span: Option<Span>,
) -> Result<Vec<BTreeSet<usize>>, StructuralPatternError> {
    program_output_dependencies_with_fold(
        program,
        span,
        None,
        None,
        None,
        DependencySource::SolverY,
    )?
    .into_iter()
    .map(|dependencies| {
        let DependencyState::Known(indices) = dependencies;
        Ok(indices)
    })
    .collect()
}

/// Exact solver-Y dependencies of every initialized register in one checked
/// scalar program. `None` denotes an uninitialized or opaque register and is
/// consumed fail-closed by refresh construction.
pub(crate) fn program_register_y_dependencies(
    program: &[LinearOp],
) -> Result<Vec<Option<BTreeSet<usize>>>, StructuralPatternError> {
    let mut walk = DependencyWalk {
        registers: Vec::new(),
        outputs: Vec::new(),
        span: None,
        fold_carried: None,
        fold_captures: None,
        conditional_captures: None,
        source: DependencySource::SolverY,
    };
    for operation in program {
        apply_dependency_op(&mut walk, operation)?;
    }
    Ok(walk
        .registers
        .into_iter()
        .map(|dependencies| match dependencies {
            Some(DependencyState::Known(indices)) => Some(indices),
            None => None,
        })
        .collect())
}

fn program_output_dependencies_with_fold(
    program: &[LinearOp],
    span: Option<Span>,
    fold_carried: Option<&[DependencyState]>,
    fold_captures: Option<&[DependencyState]>,
    conditional_captures: Option<&[DependencyState]>,
    source: DependencySource,
) -> Result<Vec<DependencyState>, StructuralPatternError> {
    let mut walk = DependencyWalk {
        registers: Vec::new(),
        outputs: Vec::new(),
        span,
        fold_carried,
        fold_captures,
        conditional_captures,
        source,
    };
    for op in program {
        apply_dependency_op(&mut walk, op)?;
    }
    Ok(walk.outputs)
}

/// Register and output state of one structural dependency walk.
///
/// The walk holds no evaluated value: every method reads `LinearOp` register
/// flow only, which is what makes the derived pattern a structural proof
/// rather than a sampled observation.
struct DependencyWalk<'a> {
    registers: Vec<Option<DependencyState>>,
    outputs: Vec<DependencyState>,
    span: Option<Span>,
    fold_carried: Option<&'a [DependencyState]>,
    fold_captures: Option<&'a [DependencyState]>,
    conditional_captures: Option<&'a [DependencyState]>,
    source: DependencySource,
}

/// Structural classification of one checked [`LinearOp`].
///
/// This is the sole dispatch table of the derivation: every arm names its
/// variant and delegates to one [`DependencyWalk`] method, so the table
/// carries the operation vocabulary and no analysis logic. The match stays
/// exhaustive with no catch-all arm, so a new `LinearOp` variant cannot reach
/// the derivation until it is classified here.
///
/// The table is laid out by hand (`rustfmt::skip`) so one arm reads as one
/// entry. Automatic formatting expands every multi-field pattern to one field
/// per line, which triples the table without telling a reader anything the
/// single-line form does not.
#[rustfmt::skip]
fn apply_dependency_op(
    walk: &mut DependencyWalk<'_>,
    op: &LinearOp,
) -> Result<(), StructuralPatternError> {
    match op {
        LinearOp::Const { dst, .. } => walk.set_empty(*dst),
        LinearOp::LoadP { dst, index } => walk.load_p(*dst, *index),
        LinearOp::LoadTime { dst } => walk.load_time(*dst),
        LinearOp::LoadY { dst, index } => walk.load_y(*dst, *index),
        LinearOp::LoadSeed { dst, index } => walk.load_seed(*dst, *index),
        LinearOp::LoadFoldCarried { dst, index } => walk.load_fold_carried(*dst, *index)?,
        LinearOp::LoadFoldIndex { dst, .. } => walk.set_empty(*dst),
        LinearOp::LoadFoldCapture { dst, index } => walk.load_fold_capture(*dst, *index)?,
        LinearOp::LoadFunctionConditionalCapture { dst, index } =>
            walk.load_conditional_capture(*dst, *index)?,
        LinearOp::LoadFunctionConditionalCaptureRange { dst_start, index_start, count } =>
            walk.load_conditional_capture_range(*dst_start, *index_start, *count)?,
        LinearOp::LoadIndexedP { dst, base, count, index } =>
            walk.load_indexed_p(*dst, *base, *count, *index)?,
        LinearOp::LoadIndexedRegister { dst, base, stride, dimensions, indices } =>
            walk.load_indexed_register(*dst, *base, *stride, dimensions, indices)?,
        LinearOp::LoadIndexedFoldCarried { dst, base, stride, dimensions, indices } =>
            walk.load_indexed_fold_carried(*dst, *base, *stride, dimensions, indices)?,
        LinearOp::LoadIndexedFoldCapture { dst, base, stride, dimensions, indices } =>
            walk.load_indexed_fold_capture(*dst, *base, *stride, dimensions, indices)?,
        LinearOp::LoadIndexedSeed { dst, base, count, index } =>
            walk.load_indexed_seed(*dst, *base, *count, *index)?,
        LinearOp::Move { dst, src } | LinearOp::Unary { dst, arg: src, .. } => walk.copy(*dst, *src)?,
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } =>
            walk.union(*dst, [*lhs, *rhs])?,
        LinearOp::Select { dst, cond, if_true, if_false } =>
            walk.union(*dst, [*cond, *if_true, *if_false])?,
        LinearOp::LinearSolveComponent { dst, matrix_start, rhs_start, n, .. } =>
            walk.linear_solve(*dst, *matrix_start, *rhs_start, *n)?,
        LinearOp::DotProduct { dst, lhs_start, rhs_start, count, lhs_stride, rhs_stride } =>
            walk.dot_product(*dst, *lhs_start, *rhs_start, *count, *lhs_stride, *rhs_stride)?,
        LinearOp::MatrixMultiply { dst_start, lhs_start, rhs_start, rows, inner, columns, lanes } =>
            walk.matrix_multiply(&MatrixMultiplyShape {
                dst_start: *dst_start, lhs_start: *lhs_start, rhs_start: *rhs_start,
                rows: *rows, inner: *inner, columns: *columns, lanes: *lanes,
            })?,
        LinearOp::TensorBinary {
            dst_start, op, lhs_start, rhs_start, count, lhs_stride, rhs_stride, lanes,
        } => walk.tensor_binary(&TensorBinaryShape {
            dst_start: *dst_start, op: *op, lhs_start: *lhs_start, rhs_start: *rhs_start,
            count: *count, lhs_stride: *lhs_stride, rhs_stride: *rhs_stride, lanes: *lanes,
        })?,
        LinearOp::TensorCross { dst_start, lhs_start, rhs_start, lanes } =>
            walk.tensor_cross(*dst_start, *lhs_start, *rhs_start, *lanes)?,
        LinearOp::TensorTranspose { dst_start, src_start, rows, columns, element_width, lanes } =>
            walk.tensor_transpose(*dst_start, *src_start, *rows, *columns, *element_width, *lanes)?,
        LinearOp::TensorConcatenate { dst_start, sources, dimensions, axis, lanes } =>
            walk.tensor_concatenate(*dst_start, sources, dimensions, *axis, *lanes)?,
        LinearOp::TensorUpdate {
            dst_start, base_start, value_start, dimensions, subscripts, lanes,
        } => walk.tensor_update(
            *dst_start, *base_start, *value_start, dimensions, subscripts, *lanes,
        )?,
        LinearOp::TensorFill { dst_start, value_start, count, lanes } =>
            walk.tensor_fill(*dst_start, *value_start, *count, *lanes)?,
        LinearOp::TensorIdentity { dst_start, size, lanes } =>
            walk.tensor_identity(*dst_start, *size, *lanes),
        LinearOp::TensorLoad { dst_start, input, input_start, count, seed_start, lanes } =>
            walk.tensor_load(*dst_start, *input, *input_start, *count, *seed_start, *lanes),
        op @ (LinearOp::TableBounds { .. } | LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. } | LinearOp::TableNextEvent { .. }
        | LinearOp::RandomInitialState { .. } | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. } | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. } | LinearOp::ImpureRandomInteger { .. }) =>
            walk.runtime(op)?,
        LinearOp::FunctionFold { dst_start, initial_start, capture_start, program } =>
            walk.function_fold(*dst_start, *initial_start, *capture_start, program)?,
        LinearOp::GuardedFunctionFold {
            dst_start, initial_start, capture_start, activation, program,
        } => walk.guarded_fold(*dst_start, *initial_start, *capture_start, *activation, program)?,
        LinearOp::FunctionConditional { dst_start, capture_start, program } =>
            walk.function_conditional(*dst_start, *capture_start, program)?,
        LinearOp::PureCall { dst_start, input_starts, site } => walk.pure_call(
            *dst_start, input_starts, site.inputs(), site.output_scalar_count(),
            "pure-call output width overflows",
        )?,
        LinearOp::PureCallDirectional { dst_start, input_starts, site } => walk.pure_call(
            *dst_start, input_starts, site.inputs(), site.output_scalar_count(),
            "directional pure-call output width overflows",
        )?,
        LinearOp::StoreOutputFoldTensorUpdate {
            source_base, source_stride, dimensions, updates, nodes, lanes, ..
        } => walk.store_fold_tensor_update(
            *source_base, *source_stride, dimensions, updates, nodes, *lanes,
        )?,
        LinearOp::StoreOutputFunctionFold {
            initial, capture_start, program, result_base, count, condition, ..
        } => walk.nested_fold(initial, *capture_start, program, *result_base, *count, *condition)?,
        LinearOp::StoreOutputRange { start, count, stride } =>
            walk.store_output_range(*start, *count, *stride)?,
        LinearOp::StoreOutput { src } => walk.store_output(*src)?,
    }
    Ok(())
}

/// Dense matrix product shape, kept as one owner so the walk method stays
/// inside the argument budget without splitting a single operation in two.
struct MatrixMultiplyShape {
    dst_start: Reg,
    lhs_start: Reg,
    rhs_start: Reg,
    rows: usize,
    inner: usize,
    columns: usize,
    lanes: usize,
}

/// Elementwise tensor-operator shape, kept as one owner for the same reason as
/// [`MatrixMultiplyShape`].
struct TensorBinaryShape {
    dst_start: Reg,
    op: BinaryOp,
    lhs_start: Reg,
    rhs_start: Reg,
    count: usize,
    lhs_stride: usize,
    rhs_stride: usize,
    lanes: usize,
}

/// The value tuple and the three diagnostics that distinguish an indexed
/// function-fold *carried* projection from an indexed *capture* projection.
/// Everything else about the two operations is identical, so they share one
/// implementation parameterized by this record.
struct IndexedFoldContext<'a> {
    values: Option<&'a [DependencyState]>,
    missing: &'static str,
    overflow: &'static str,
    invalid_range: &'static str,
}

impl DependencyWalk<'_> {
    fn get(&self, register_id: Reg) -> Result<DependencyState, StructuralPatternError> {
        register(&self.registers, register_id, self.span)
    }

    fn range(&self, start: Reg, len: usize) -> Result<DependencyState, StructuralPatternError> {
        register_range(&self.registers, start, len, self.span)
    }

    fn register_tuple(
        &self,
        start: Reg,
        count: usize,
    ) -> Result<Vec<DependencyState>, StructuralPatternError> {
        (0..count)
            .map(|offset| self.get(start + offset as Reg))
            .collect()
    }

    fn set(&mut self, dst: Reg, dependencies: DependencyState) {
        set_register(&mut self.registers, dst, dependencies);
    }

    fn set_empty(&mut self, dst: Reg) {
        set_empty_dependency(&mut self.registers, dst);
    }

    fn set_seed(&mut self, dst: Reg, index: usize) {
        set_seed_dependency(&mut self.registers, dst, index);
    }

    fn copy(&mut self, dst: Reg, src: Reg) -> Result<(), StructuralPatternError> {
        copy_dependency(&mut self.registers, dst, src, self.span)
    }

    fn union<const N: usize>(
        &mut self,
        dst: Reg,
        sources: [Reg; N],
    ) -> Result<(), StructuralPatternError> {
        set_union_dependency(&mut self.registers, dst, sources, self.span)
    }

    /// Solver-`Y` loads seed the solver-`Y` walk and the AD-seed walk sees them
    /// as constants; [`DependencyWalk::load_seed`] is the mirror image.
    fn load_y(&mut self, dst: Reg, index: usize) {
        match self.source {
            DependencySource::Effect
            | DependencySource::Seed
            | DependencySource::SolverP
            | DependencySource::Time => self.set_empty(dst),
            DependencySource::SolverY => self.set_seed(dst, index),
        }
    }

    fn load_p(&mut self, dst: Reg, index: usize) {
        match self.source {
            DependencySource::SolverP => self.set_seed(dst, index),
            DependencySource::Effect
            | DependencySource::Seed
            | DependencySource::SolverY
            | DependencySource::Time => self.set_empty(dst),
        }
    }

    fn load_time(&mut self, dst: Reg) {
        match self.source {
            DependencySource::Time => self.set_seed(dst, 0),
            DependencySource::Effect
            | DependencySource::Seed
            | DependencySource::SolverP
            | DependencySource::SolverY => self.set_empty(dst),
        }
    }

    fn load_seed(&mut self, dst: Reg, index: usize) {
        match self.source {
            DependencySource::Seed => self.set_seed(dst, index),
            DependencySource::Effect
            | DependencySource::SolverP
            | DependencySource::SolverY
            | DependencySource::Time => self.set_empty(dst),
        }
    }

    fn load_indexed_p(
        &mut self,
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<(), StructuralPatternError> {
        let mut dependencies = self.get(index)?;
        if matches!(self.source, DependencySource::SolverP) {
            let end = checked_indexed_seed_end(base, count, self.span)?;
            dependencies = dependencies.union(DependencyState::Known((base..end).collect()));
        }
        self.set(dst, dependencies);
        Ok(())
    }

    fn load_context_value(
        &mut self,
        dst: Reg,
        values: Option<&[DependencyState]>,
        index: usize,
        message: &'static str,
    ) -> Result<(), StructuralPatternError> {
        let dependency = values
            .and_then(|values| values.get(index))
            .cloned()
            .ok_or_else(|| dependency_error(message, self.span))?;
        self.set(dst, dependency);
        Ok(())
    }

    fn load_fold_carried(&mut self, dst: Reg, index: usize) -> Result<(), StructuralPatternError> {
        let values = self.fold_carried;
        self.load_context_value(dst, values, index, "invalid function-fold carried load")
    }

    fn load_fold_capture(&mut self, dst: Reg, index: usize) -> Result<(), StructuralPatternError> {
        let values = self.fold_captures;
        self.load_context_value(dst, values, index, "invalid function-fold capture load")
    }

    fn load_conditional_capture(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), StructuralPatternError> {
        let values = self.conditional_captures;
        self.load_context_value(
            dst,
            values,
            index,
            "invalid function-conditional capture load",
        )
    }

    fn load_conditional_capture_range(
        &mut self,
        dst_start: Reg,
        index_start: usize,
        count: usize,
    ) -> Result<(), StructuralPatternError> {
        const MESSAGE: &str = "invalid function-conditional capture range load";
        let captures = self
            .conditional_captures
            .ok_or_else(|| dependency_error(MESSAGE, self.span))?;
        for offset in 0..count {
            let dependency = captures
                .get(index_start + offset)
                .cloned()
                .ok_or_else(|| dependency_error(MESSAGE, self.span))?;
            self.set(dst_start + offset as Reg, dependency);
        }
        Ok(())
    }

    /// Runtime subscript registers of one compact tensor projection.
    fn union_runtime_indices(
        &self,
        mut dependencies: DependencyState,
        indices: &[crate::TensorIndex],
    ) -> Result<DependencyState, StructuralPatternError> {
        for index in indices {
            if let crate::TensorIndex::Runtime(register_id) = index {
                dependencies = dependencies.union(self.get(*register_id)?);
            }
        }
        Ok(dependencies)
    }

    fn load_indexed_register(
        &mut self,
        dst: Reg,
        base: Reg,
        stride: usize,
        dimensions: &[u32],
        indices: &[crate::TensorIndex],
    ) -> Result<(), StructuralPatternError> {
        let count = checked_tensor_extent(dimensions).ok_or_else(|| {
            dependency_error("runtime tensor projection extent overflow", self.span)
        })?;
        let mut dependencies = DependencyState::empty();
        for offset in 0..count {
            dependencies = dependencies.union(self.get(base + (offset * stride) as Reg)?);
        }
        let dependencies = self.union_runtime_indices(dependencies, indices)?;
        self.set(dst, dependencies);
        Ok(())
    }

    fn load_indexed_fold_carried(
        &mut self,
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: &[u32],
        indices: &[crate::TensorIndex],
    ) -> Result<(), StructuralPatternError> {
        let context = IndexedFoldContext {
            values: self.fold_carried,
            missing: "invalid indexed function-fold carried load",
            overflow: "indexed function-fold carried extent overflow",
            invalid_range: "indexed function-fold carried range is invalid",
        };
        self.load_indexed_fold(dst, base, stride, dimensions, indices, context)
    }

    fn load_indexed_fold_capture(
        &mut self,
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: &[u32],
        indices: &[crate::TensorIndex],
    ) -> Result<(), StructuralPatternError> {
        let context = IndexedFoldContext {
            values: self.fold_captures,
            missing: "invalid indexed function-fold capture load",
            overflow: "indexed function-fold capture extent overflow",
            invalid_range: "indexed function-fold capture range is invalid",
        };
        self.load_indexed_fold(dst, base, stride, dimensions, indices, context)
    }

    fn load_indexed_fold(
        &mut self,
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: &[u32],
        indices: &[crate::TensorIndex],
        context: IndexedFoldContext<'_>,
    ) -> Result<(), StructuralPatternError> {
        let values = context
            .values
            .ok_or_else(|| dependency_error(context.missing, self.span))?;
        let count = checked_tensor_extent(dimensions)
            .ok_or_else(|| dependency_error(context.overflow, self.span))?;
        let mut dependencies = DependencyState::empty();
        for offset in 0..count {
            let dependency = values
                .get(base + offset * stride)
                .ok_or_else(|| dependency_error(context.invalid_range, self.span))?;
            dependencies = dependencies.union(dependency.clone());
        }
        let dependencies = self.union_runtime_indices(dependencies, indices)?;
        self.set(dst, dependencies);
        Ok(())
    }

    fn load_indexed_seed(
        &mut self,
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<(), StructuralPatternError> {
        let mut dependencies = self.get(index)?;
        if matches!(self.source, DependencySource::Seed) {
            let end = checked_indexed_seed_end(base, count, self.span)?;
            dependencies = dependencies.union(DependencyState::Known((base..end).collect()));
        }
        self.set(dst, dependencies);
        Ok(())
    }

    fn linear_solve(
        &mut self,
        dst: Reg,
        matrix_start: Reg,
        rhs_start: Reg,
        n: usize,
    ) -> Result<(), StructuralPatternError> {
        set_linear_solve_dependency(
            &mut self.registers,
            LinearSolveDependency {
                dst,
                matrix_start,
                rhs_start,
                n,
            },
            self.span,
        )
    }

    fn dot_product(
        &mut self,
        dst: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        count: usize,
        lhs_stride: usize,
        rhs_stride: usize,
    ) -> Result<(), StructuralPatternError> {
        let mut sources = Vec::with_capacity(count.saturating_mul(2));
        for term in 0..count {
            sources.push(lhs_start + (term * lhs_stride) as Reg);
            sources.push(rhs_start + (term * rhs_stride) as Reg);
        }
        let mut dependencies = DependencyState::empty();
        for source in sources {
            dependencies = dependencies.union(self.get(source)?);
        }
        self.set(dst, dependencies);
        Ok(())
    }

    fn matrix_multiply(
        &mut self,
        shape: &MatrixMultiplyShape,
    ) -> Result<(), StructuralPatternError> {
        for row in 0..shape.rows {
            for column in 0..shape.columns {
                self.matrix_multiply_element(shape, row, column)?;
            }
        }
        Ok(())
    }

    /// One row/column element of a dense product, over every AD lane.
    fn matrix_multiply_element(
        &mut self,
        shape: &MatrixMultiplyShape,
        row: usize,
        column: usize,
    ) -> Result<(), StructuralPatternError> {
        let output = (row * shape.columns + column) * shape.lanes;
        for lane in 0..shape.lanes {
            let dependencies = self.matrix_multiply_lane(shape, row, column, lane)?;
            self.set(shape.dst_start + (output + lane) as Reg, dependencies);
        }
        Ok(())
    }

    /// One lane of one dense product element, accumulated over the inner
    /// extent. The second lane of a dual product also depends on both primal
    /// operands, which is why it unions the lane-zero registers as well.
    fn matrix_multiply_lane(
        &self,
        shape: &MatrixMultiplyShape,
        row: usize,
        column: usize,
        lane: usize,
    ) -> Result<DependencyState, StructuralPatternError> {
        let mut dependencies = DependencyState::empty();
        for term in 0..shape.inner {
            let lhs = (row * shape.inner + term) * shape.lanes;
            let rhs = (term * shape.columns + column) * shape.lanes;
            dependencies = dependencies.union(self.get(shape.lhs_start + (lhs + lane) as Reg)?);
            dependencies = dependencies.union(self.get(shape.rhs_start + (rhs + lane) as Reg)?);
            if shape.lanes == 2 && lane == 1 {
                dependencies = dependencies.union(self.get(shape.lhs_start + lhs as Reg)?);
                dependencies = dependencies.union(self.get(shape.rhs_start + rhs as Reg)?);
            }
        }
        Ok(dependencies)
    }

    fn tensor_binary(&mut self, shape: &TensorBinaryShape) -> Result<(), StructuralPatternError> {
        for element in 0..shape.count {
            let lhs = shape.lhs_start + (element * shape.lhs_stride * shape.lanes) as Reg;
            let rhs = shape.rhs_start + (element * shape.rhs_stride * shape.lanes) as Reg;
            let output = shape.dst_start + (element * shape.lanes) as Reg;
            let primal = self.get(lhs)?.union(self.get(rhs)?);
            self.set(output, primal.clone());
            if shape.lanes == 2 {
                let tangent = self.tensor_binary_tangent(shape, lhs, rhs, primal)?;
                self.set(output + 1, tangent);
            }
        }
        Ok(())
    }

    /// Tangent lane of one elementwise tensor operator. A product or quotient
    /// tangent also depends on both primal operands.
    fn tensor_binary_tangent(
        &self,
        shape: &TensorBinaryShape,
        lhs: Reg,
        rhs: Reg,
        primal: DependencyState,
    ) -> Result<DependencyState, StructuralPatternError> {
        let mut tangent = self.get(lhs + 1)?.union(self.get(rhs + 1)?);
        if matches!(shape.op, BinaryOp::Mul | BinaryOp::Div) {
            tangent = tangent.union(primal);
        }
        Ok(tangent)
    }

    fn tensor_cross(
        &mut self,
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        lanes: usize,
    ) -> Result<(), StructuralPatternError> {
        for (component, (first, second)) in
            [(1usize, 2usize), (2, 0), (0, 1)].into_iter().enumerate()
        {
            let lhs_first = lhs_start + (first * lanes) as Reg;
            let lhs_second = lhs_start + (second * lanes) as Reg;
            let rhs_first = rhs_start + (first * lanes) as Reg;
            let rhs_second = rhs_start + (second * lanes) as Reg;
            let primal = self
                .get(lhs_first)?
                .union(self.get(lhs_second)?)
                .union(self.get(rhs_first)?)
                .union(self.get(rhs_second)?);
            let output = dst_start + (component * lanes) as Reg;
            self.set(output, primal.clone());
            if lanes == 2 {
                let tangent = primal
                    .union(self.get(lhs_first + 1)?)
                    .union(self.get(lhs_second + 1)?)
                    .union(self.get(rhs_first + 1)?)
                    .union(self.get(rhs_second + 1)?);
                self.set(output + 1, tangent);
            }
        }
        Ok(())
    }

    fn tensor_transpose(
        &mut self,
        dst_start: Reg,
        src_start: Reg,
        rows: usize,
        columns: usize,
        element_width: usize,
        lanes: usize,
    ) -> Result<(), StructuralPatternError> {
        let value_width = element_width * lanes;
        for row in 0..rows {
            for column in 0..columns {
                let dst = (row * columns + column) * value_width;
                let src = (column * rows + row) * value_width;
                self.transpose_element(dst_start, src_start, dst, src, value_width)?;
            }
        }
        Ok(())
    }

    /// Every untouched trailing value of one transposed element, copied from
    /// its source coordinate to its destination coordinate.
    fn transpose_element(
        &mut self,
        dst_start: Reg,
        src_start: Reg,
        dst_base: usize,
        src_base: usize,
        value_width: usize,
    ) -> Result<(), StructuralPatternError> {
        for value in 0..value_width {
            let dst = dst_base + value;
            let src = src_base + value;
            let dependencies = self.get(src_start + src as Reg)?;
            self.set(dst_start + dst as Reg, dependencies);
        }
        Ok(())
    }

    fn tensor_concatenate(
        &mut self,
        dst_start: Reg,
        sources: &[crate::TensorConcatenateSource],
        dimensions: &[u32],
        axis: usize,
        lanes: usize,
    ) -> Result<(), StructuralPatternError> {
        let span = self.span;
        let registers = &mut self.registers;
        visit_tensor_concatenate(sources, dimensions, axis, lanes, |source, destination| {
            let dependencies = register(registers, source, span)?;
            set_register(registers, dst_start + destination as Reg, dependencies);
            Ok::<(), StructuralPatternError>(())
        })
    }

    fn tensor_update(
        &mut self,
        dst_start: Reg,
        base_start: Reg,
        value_start: Reg,
        dimensions: &[u32],
        subscripts: &[crate::TensorUpdateSubscript],
        lanes: usize,
    ) -> Result<(), StructuralPatternError> {
        let count = saturating_tensor_extent(dimensions);
        let (value_count, selector) = self.tensor_update_patch(dimensions, subscripts, lanes)?;
        let patch = self.range(value_start, value_count)?.union(selector);
        for element in 0..count {
            for lane in 0..lanes {
                let offset = element * lanes + lane;
                let dependencies = self.get(base_start + offset as Reg)?.union(patch.clone());
                self.set(dst_start + offset as Reg, dependencies);
            }
        }
        Ok(())
    }

    /// Width of the patch value range and the dependencies of the runtime
    /// coordinates that select where the patch lands.
    fn tensor_update_patch(
        &self,
        dimensions: &[u32],
        subscripts: &[crate::TensorUpdateSubscript],
        lanes: usize,
    ) -> Result<(usize, DependencyState), StructuralPatternError> {
        let mut value_count = lanes;
        let mut selector = DependencyState::empty();
        for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
            match subscript {
                crate::TensorUpdateSubscript::Whole => {
                    value_count = value_count.saturating_mul(extent as usize);
                }
                crate::TensorUpdateSubscript::Index(crate::TensorIndex::Runtime(register_id)) => {
                    selector = selector.union(self.get(*register_id)?);
                }
                crate::TensorUpdateSubscript::Index(crate::TensorIndex::Constant(_)) => {}
                crate::TensorUpdateSubscript::Slice { start, dimensions } => {
                    let slice_count = saturating_tensor_extent(dimensions);
                    selector = selector.union(self.range(*start, slice_count)?);
                    value_count = value_count.saturating_mul(slice_count);
                }
            }
        }
        Ok((value_count, selector))
    }

    fn tensor_fill(
        &mut self,
        dst_start: Reg,
        value_start: Reg,
        count: usize,
        lanes: usize,
    ) -> Result<(), StructuralPatternError> {
        for element in 0..count {
            for lane in 0..lanes {
                let dependencies = self.get(value_start + lane as Reg)?;
                self.set(dst_start + (element * lanes + lane) as Reg, dependencies);
            }
        }
        Ok(())
    }

    fn tensor_identity(&mut self, dst_start: Reg, size: usize, lanes: usize) {
        for offset in 0..size * size * lanes {
            self.set_empty(dst_start + offset as Reg);
        }
    }

    fn tensor_load(
        &mut self,
        dst_start: Reg,
        input: crate::TensorInputKind,
        input_start: usize,
        count: usize,
        seed_start: Option<usize>,
        lanes: usize,
    ) {
        for element in 0..count {
            let primal = match (self.source, input) {
                (DependencySource::SolverY, crate::TensorInputKind::Y) => {
                    DependencyState::singleton(input_start + element)
                }
                (DependencySource::SolverP, crate::TensorInputKind::P) => {
                    DependencyState::singleton(input_start + element)
                }
                (DependencySource::Seed, _)
                | (DependencySource::Effect, _)
                | (DependencySource::Time, _)
                | (DependencySource::SolverP, crate::TensorInputKind::Y)
                | (DependencySource::SolverY, crate::TensorInputKind::P) => {
                    DependencyState::empty()
                }
            };
            self.set(dst_start + (element * lanes) as Reg, primal);
            if lanes == 2 {
                let dependency = match self.source {
                    DependencySource::Seed => tensor_load_seed(seed_start, element),
                    DependencySource::Effect
                    | DependencySource::SolverP
                    | DependencySource::SolverY
                    | DependencySource::Time => DependencyState::empty(),
                };
                self.set(dst_start + (element * lanes + 1) as Reg, dependency);
            }
        }
    }

    fn runtime(&mut self, op: &LinearOp) -> Result<(), StructuralPatternError> {
        if matches!(self.source, DependencySource::Effect) {
            let dst = op.dst_register().ok_or_else(|| {
                dependency_error(
                    "runtime dependency operation has no output register",
                    self.span,
                )
            })?;
            self.set_seed(dst, 0);
            return Ok(());
        }
        apply_runtime_dependency(&mut self.registers, op, self.span)
    }

    fn function_fold(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        program: &crate::FunctionFoldProgram,
    ) -> Result<(), StructuralPatternError> {
        let carried = self.register_tuple(initial_start, program.carried_count)?;
        let captures = self.register_tuple(capture_start, program.capture_count)?;
        let carried =
            function_fold_dependencies(program, carried, &captures, self.span, self.source)?;
        for (offset, dependency) in carried.into_iter().enumerate() {
            self.set(dst_start + offset as Reg, dependency);
        }
        Ok(())
    }

    fn guarded_fold(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        activation: Reg,
        program: &crate::FunctionFoldProgram,
    ) -> Result<(), StructuralPatternError> {
        let activation = self.get(activation)?;
        let carried = self.register_tuple(initial_start, program.carried_count)?;
        let captures = self.register_tuple(capture_start, program.capture_count)?;
        let carried =
            function_fold_dependencies(program, carried, &captures, self.span, self.source)?;
        for (offset, dependency) in carried.into_iter().enumerate() {
            self.set(
                dst_start + offset as Reg,
                dependency.union(activation.clone()),
            );
        }
        Ok(())
    }

    /// One nested program of the enclosing walk, evaluated under the same fold
    /// context but with its own function-conditional capture tuple.
    fn nested_program(
        &self,
        program: &[LinearOp],
        captures: &[DependencyState],
    ) -> Result<Vec<DependencyState>, StructuralPatternError> {
        program_output_dependencies_with_fold(
            program,
            self.span,
            self.fold_carried,
            self.fold_captures,
            Some(captures),
            self.source,
        )
    }

    fn function_conditional(
        &mut self,
        dst_start: Reg,
        capture_start: Reg,
        program: &crate::FunctionConditionalProgram,
    ) -> Result<(), StructuralPatternError> {
        let captures = self.register_tuple(capture_start, program.capture_count)?;
        let mut condition_dependency = DependencyState::empty();
        let mut result = vec![DependencyState::empty(); program.result_count];
        for arm in &program.arms {
            let condition = self.nested_program(&arm.condition, &captures)?;
            let condition = condition
                .first()
                .cloned()
                .ok_or_else(|| dependency_error("missing conditional condition", self.span))?;
            condition_dependency = condition_dependency.union(condition);
            let branch = self.nested_program(&arm.result, &captures)?;
            union_conditional_results(&mut result, branch, self.span)?;
        }
        let fallback = self.nested_program(&program.fallback, &captures)?;
        union_conditional_results(&mut result, fallback, self.span)?;
        for (offset, dependency) in result.into_iter().enumerate() {
            let dependency = dependency.union(condition_dependency.clone());
            self.set(dst_start + offset as Reg, dependency);
        }
        Ok(())
    }

    fn union_scalar_range(
        &self,
        mut dependency: DependencyState,
        start: Reg,
        count: usize,
    ) -> Result<DependencyState, StructuralPatternError> {
        for offset in 0..count {
            dependency = dependency.union(self.get(start + offset as Reg)?);
        }
        Ok(dependency)
    }

    /// Every ordered result of a typed pure call depends on every typed input:
    /// the call owner is opaque to this structural walk, so the conservative
    /// closure is the only sound one. The plain and directional sites differ
    /// only in the diagnostic their output-width overflow reports.
    fn pure_call(
        &mut self,
        dst_start: Reg,
        input_starts: &[Reg],
        inputs: &[crate::SolveValueType],
        output_scalar_count: Option<usize>,
        message: &'static str,
    ) -> Result<(), StructuralPatternError> {
        let mut dependency = DependencyState::empty();
        for (start, value_type) in input_starts.iter().zip(inputs) {
            let count = value_type.scalar_count() as usize;
            dependency = self.union_scalar_range(dependency, *start, count)?;
        }
        let output_count =
            output_scalar_count.ok_or_else(|| dependency_error(message, self.span))?;
        for offset in 0..output_count {
            self.set(dst_start + offset as Reg, dependency.clone());
        }
        Ok(())
    }

    fn store_fold_tensor_update(
        &mut self,
        source_base: usize,
        source_stride: usize,
        dimensions: &[u32],
        updates: &[crate::FoldTensorUpdate],
        nodes: &[crate::FoldTensorNode],
        lanes: usize,
    ) -> Result<(), StructuralPatternError> {
        let carried = self.fold_carried.ok_or_else(|| {
            dependency_error(
                "aggregate output escaped its function-fold update body",
                self.span,
            )
        })?;
        let count = checked_tensor_extent(dimensions)
            .ok_or_else(|| dependency_error("tensor update extent overflow", self.span))?;
        let mut update_dependency = DependencyState::empty();
        for update in updates {
            update_dependency =
                self.union_fold_tensor_update(update_dependency, dimensions, update, lanes)?;
        }
        for node in nodes {
            if let crate::FoldTensorNode::Select { condition, .. } = *node {
                update_dependency = update_dependency.union(self.get(condition)?);
            }
        }
        for element in 0..count {
            let source = source_base + element * source_stride;
            self.push_fold_tensor_element(carried, source, lanes, &update_dependency)?;
        }
        Ok(())
    }

    /// One aggregate element of a carried tensor update, over every AD lane.
    /// The unchanged carried value stays in the dependency set because a patch
    /// that misses this coordinate leaves it in place.
    fn push_fold_tensor_element(
        &mut self,
        carried: &[DependencyState],
        source: usize,
        lanes: usize,
        update_dependency: &DependencyState,
    ) -> Result<(), StructuralPatternError> {
        for lane in 0..lanes {
            let unchanged = carried.get(source + lane).cloned().ok_or_else(|| {
                dependency_error("tensor update carried source is invalid", self.span)
            })?;
            self.outputs
                .push(unchanged.union(update_dependency.clone()));
        }
        Ok(())
    }

    /// Condition, patch values and runtime coordinates of one aggregate patch.
    fn union_fold_tensor_update(
        &self,
        mut update_dependency: DependencyState,
        dimensions: &[u32],
        update: &crate::FoldTensorUpdate,
        lanes: usize,
    ) -> Result<DependencyState, StructuralPatternError> {
        let value_count = dimensions
            .iter()
            .zip(update.subscripts.iter())
            .try_fold(1usize, |count, (&extent, subscript)| {
                if matches!(subscript, crate::TensorSubscript::Whole) {
                    count.checked_mul(extent as usize)
                } else {
                    Some(count)
                }
            })
            .ok_or_else(|| dependency_error("tensor update value extent overflow", self.span))?;
        if let Some(condition) = update.condition {
            update_dependency = update_dependency.union(self.get(condition)?);
        }
        for element in 0..value_count {
            for lane in 0..lanes {
                let offset = (element * update.value_stride + lane) as Reg;
                update_dependency = update_dependency.union(self.get(update.value_start + offset)?);
            }
        }
        for subscript in &update.subscripts {
            if let crate::TensorSubscript::Index(crate::TensorIndex::Runtime(register_id)) =
                subscript
            {
                update_dependency = update_dependency.union(self.get(*register_id)?);
            }
        }
        Ok(update_dependency)
    }

    fn nested_fold(
        &mut self,
        initial: &[crate::FoldInitialSource],
        capture_start: Reg,
        program: &crate::FunctionFoldProgram,
        result_base: usize,
        count: usize,
        condition: Option<Reg>,
    ) -> Result<(), StructuralPatternError> {
        let parent = self.fold_carried.ok_or_else(|| {
            dependency_error(
                "nested aggregate fold escaped its parent update body",
                self.span,
            )
        })?;
        let mut carried = Vec::with_capacity(program.carried_count);
        for source in initial {
            self.push_nested_fold_initial(&mut carried, parent, source)?;
        }
        let captures = self.register_tuple(capture_start, program.capture_count)?;
        let carried =
            function_fold_dependencies(program, carried, &captures, self.span, self.source)?;
        let end = result_base
            .checked_add(count)
            .ok_or_else(|| dependency_error("nested fold result range overflow", self.span))?;
        let result = carried
            .get(result_base..end)
            .ok_or_else(|| dependency_error("nested fold result range is invalid", self.span))?;
        let Some(condition) = condition else {
            self.outputs.extend_from_slice(result);
            return Ok(());
        };
        let condition = self.get(condition)?;
        let output_base = self.outputs.len();
        for (offset, nested) in result.iter().cloned().enumerate() {
            let unchanged = parent.get(output_base + offset).cloned().ok_or_else(|| {
                dependency_error("conditional nested fold parent range is invalid", self.span)
            })?;
            self.outputs
                .push(nested.union(unchanged).union(condition.clone()));
        }
        Ok(())
    }

    fn push_nested_fold_initial(
        &self,
        carried: &mut Vec<DependencyState>,
        parent: &[DependencyState],
        source: &crate::FoldInitialSource,
    ) -> Result<(), StructuralPatternError> {
        match *source {
            crate::FoldInitialSource::Registers { start, count } => {
                for offset in 0..count {
                    carried.push(self.get(start + offset as Reg)?);
                }
            }
            crate::FoldInitialSource::ParentCarried { base, count } => {
                let end = base.checked_add(count).ok_or_else(|| {
                    dependency_error("nested fold carried range overflow", self.span)
                })?;
                let values = parent.get(base..end).ok_or_else(|| {
                    dependency_error("nested fold carried range is invalid", self.span)
                })?;
                carried.extend_from_slice(values);
            }
        }
        Ok(())
    }

    fn store_output_range(
        &mut self,
        start: Reg,
        count: usize,
        stride: usize,
    ) -> Result<(), StructuralPatternError> {
        for ordinal in 0..count {
            let offset = ordinal.checked_mul(stride).ok_or_else(|| {
                dependency_error("conditional output range offset overflows", self.span)
            })?;
            let offset = Reg::try_from(offset).map_err(|_| {
                dependency_error("conditional output range exceeds registers", self.span)
            })?;
            let source = start.checked_add(offset).ok_or_else(|| {
                dependency_error("conditional output register overflows", self.span)
            })?;
            let dependency = self.get(source)?;
            self.outputs.push(dependency);
        }
        Ok(())
    }

    fn store_output(&mut self, src: Reg) -> Result<(), StructuralPatternError> {
        let dependency = self.get(src)?;
        self.outputs.push(dependency);
        Ok(())
    }
}

/// Row-major element count of a compact tensor shape, or `None` when the shape
/// product leaves the register range.
fn checked_tensor_extent(dimensions: &[u32]) -> Option<usize> {
    dimensions
        .iter()
        .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
}

/// Row-major element count of a shape-preserving array patch. This walk
/// saturates rather than reporting overflow because the patch is a subrange of
/// an aggregate whose own extent was already checked by its owner.
fn saturating_tensor_extent(dimensions: &[u32]) -> usize {
    dimensions.iter().fold(1usize, |count, extent| {
        count.saturating_mul(*extent as usize)
    })
}

/// AD seed dependency of one scalar of a runtime tensor load, empty when the
/// load carries no seed region.
fn tensor_load_seed(seed_start: Option<usize>, element: usize) -> DependencyState {
    seed_start.map_or_else(DependencyState::empty, |seed_start| {
        DependencyState::singleton(seed_start + element)
    })
}

fn union_conditional_results(
    accumulated: &mut [DependencyState],
    branch: Vec<DependencyState>,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    if accumulated.len() != branch.len() {
        return Err(dependency_error(
            "function-conditional result dependency count mismatch",
            span,
        ));
    }
    for (accumulated, branch) in accumulated.iter_mut().zip(branch) {
        *accumulated = accumulated.clone().union(branch);
    }
    Ok(())
}

fn function_fold_dependencies(
    program: &crate::FunctionFoldProgram,
    mut carried: Vec<DependencyState>,
    captures: &[DependencyState],
    span: Option<Span>,
    source: DependencySource,
) -> Result<Vec<DependencyState>, StructuralPatternError> {
    if carried.len() != program.carried_count {
        return Err(dependency_error(
            "function-fold initial dependency count mismatch",
            span,
        ));
    }
    if program
        .domain
        .scalar_count()
        .map_err(|error| dependency_error(format!("invalid function-fold domain: {error}"), span))?
        == 0
    {
        return Ok(carried);
    }
    loop {
        let updates = program_output_dependencies_with_fold(
            &program.update,
            span,
            Some(&carried),
            Some(captures),
            None,
            source,
        )?;
        if updates.len() != carried.len() {
            return Err(dependency_error(
                "function-fold update output count mismatch",
                span,
            ));
        }
        let next = carried
            .iter()
            .cloned()
            .zip(updates)
            .map(|(old, new)| old.union(new))
            .collect::<Vec<_>>();
        if next == carried {
            return Ok(carried);
        }
        carried = next;
    }
}

struct LinearSolveDependency {
    dst: Reg,
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
}

fn apply_runtime_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    operation: &LinearOp,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    match operation {
        LinearOp::TableBounds { dst, table_id, .. } => {
            copy_dependency(registers, *dst, *table_id, span)
        }
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        }
        | LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => set_union_dependency(registers, *dst, [*table_id, *column, *input], span),
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => set_union_dependency(registers, *dst, [*table_id, *time], span),
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => set_union_dependency(registers, *dst, [*local_seed, *global_seed], span),
        LinearOp::RandomResult {
            dst,
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            dst,
            state_start,
            state_len,
            ..
        } => set_range_dependency(registers, *dst, *state_start, *state_len, span),
        LinearOp::ImpureRandomInit { dst, seed } => copy_dependency(registers, *dst, *seed, span),
        LinearOp::ImpureRandom { dst, id, .. } => copy_dependency(registers, *dst, *id, span),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => set_union_dependency(registers, *dst, [*id, *imin, *imax], span),
        _ => unreachable!("runtime dependency operation is classified by the exhaustive caller"),
    }
}

fn set_empty_dependency(registers: &mut Vec<Option<DependencyState>>, dst: Reg) {
    set_register(registers, dst, DependencyState::empty());
}

fn set_seed_dependency(registers: &mut Vec<Option<DependencyState>>, dst: Reg, index: usize) {
    set_register(registers, dst, DependencyState::singleton(index));
}

fn copy_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dst: Reg,
    src: Reg,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    let dependencies = register(registers, src, span)?;
    set_register(registers, dst, dependencies);
    Ok(())
}

fn set_union_dependency<const N: usize>(
    registers: &mut Vec<Option<DependencyState>>,
    dst: Reg,
    sources: [Reg; N],
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    let dependencies = union_registers(registers, sources, span)?;
    set_register(registers, dst, dependencies);
    Ok(())
}

fn set_range_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dst: Reg,
    start: Reg,
    len: usize,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    let dependencies = register_range(registers, start, len, span)?;
    set_register(registers, dst, dependencies);
    Ok(())
}

fn set_linear_solve_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dependency: LinearSolveDependency,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    let matrix_len = rumoca_core::checked_product(dependency.n, dependency.n).ok_or_else(|| {
        dependency_error(
            format!(
                "linear solve matrix shape product {} * {} overflows register range",
                dependency.n, dependency.n
            ),
            span,
        )
    })?;
    let dependencies = register_range(registers, dependency.matrix_start, matrix_len, span)?.union(
        register_range(registers, dependency.rhs_start, dependency.n, span)?,
    );
    set_register(registers, dependency.dst, dependencies);
    Ok(())
}

fn set_register(
    registers: &mut Vec<Option<DependencyState>>,
    register: Reg,
    dependencies: DependencyState,
) {
    let index = register as usize;
    if registers.len() <= index {
        registers.resize_with(index + 1, || None);
    }
    registers[index] = Some(dependencies);
}

fn register(
    registers: &[Option<DependencyState>],
    register: Reg,
    span: Option<Span>,
) -> Result<DependencyState, StructuralPatternError> {
    registers
        .get(register as usize)
        .and_then(Clone::clone)
        .ok_or(StructuralPatternError::UninitializedRegister { register, span })
}

fn register_range(
    registers: &[Option<DependencyState>],
    start: Reg,
    len: usize,
    span: Option<Span>,
) -> Result<DependencyState, StructuralPatternError> {
    let mut dependencies = DependencyState::empty();
    for offset in 0..len {
        dependencies = dependencies.union(register(
            registers,
            checked_reg_offset(start, offset, span)?,
            span,
        )?);
    }
    Ok(dependencies)
}

fn union_registers<const N: usize>(
    registers: &[Option<DependencyState>],
    operands: [Reg; N],
    span: Option<Span>,
) -> Result<DependencyState, StructuralPatternError> {
    operands
        .into_iter()
        .try_fold(DependencyState::empty(), |dependencies, register_id| {
            Ok(dependencies.union(register(registers, register_id, span)?))
        })
}

fn checked_indexed_seed_end(
    base: usize,
    count: usize,
    span: Option<Span>,
) -> Result<usize, StructuralPatternError> {
    let width = count.max(1);
    base.checked_add(width).ok_or_else(|| {
        dependency_error(
            format!("indexed seed range base {base} plus count {count} overflows"),
            span,
        )
    })
}

fn checked_reg_offset(
    start: Reg,
    offset: usize,
    span: Option<Span>,
) -> Result<Reg, StructuralPatternError> {
    let offset = u32::try_from(offset)
        .map_err(|_| dependency_error(format!("register offset {offset} exceeds u32"), span))?;
    start.checked_add(offset).ok_or_else(|| {
        dependency_error(
            format!("register range start {start} plus offset {offset} overflows"),
            span,
        )
    })
}

fn dependency_error(message: impl Into<String>, span: Option<Span>) -> StructuralPatternError {
    StructuralPatternError::DependencyContract {
        message: message.into(),
        span,
    }
}

/// Destination mapping of a tensor concatenation, shared by the structural
/// derivation and any consumer that must agree with it element for element.
fn visit_tensor_concatenate<E>(
    sources: &[crate::TensorConcatenateSource],
    dimensions: &[u32],
    axis: usize,
    lanes: usize,
    mut visit: impl FnMut(Reg, usize) -> Result<(), E>,
) -> Result<(), E> {
    let inner = dimensions[axis + 1..]
        .iter()
        .fold(1usize, |count, extent| count * *extent as usize);
    let result_axis = dimensions[axis] as usize;
    let mut axis_offset = 0usize;
    for source in sources {
        let source_axis = source.dimensions[axis] as usize;
        let source_count = source
            .dimensions
            .iter()
            .fold(1usize, |count, extent| count * *extent as usize);
        let source_block = source_axis * inner;
        for element in 0..source_count {
            let outer = element / source_block;
            let within = element % source_block;
            let destination = outer * result_axis * inner + axis_offset * inner + within;
            for lane in 0..lanes {
                visit(
                    source.start + (element * lanes + lane) as Reg,
                    destination * lanes + lane,
                )?;
            }
        }
        axis_offset += source_axis;
    }
    Ok(())
}

/// Current-wire mirror of [`PatternProvenance`]. Decoding a value of this type
/// proves nothing on its own; [`PatternProvenanceWire::checked`] is the single
/// bridge back to a certified provenance.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PatternProvenanceWire {
    derivation: PatternDerivation,
    span: Span,
}

impl PatternProvenanceWire {
    fn checked(self) -> Result<PatternProvenance, StructuralPatternError> {
        PatternProvenance::derived(self.derivation, self.span)
    }
}

/// Current-wire mirror of [`PatternRepresentation`]. Its serialized form is
/// identical, but a decoded value is only a representation *claim* until one of
/// the checked [`StructuralPattern`] constructors accepts it.
#[derive(Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
enum PatternRepresentationWire {
    Empty,
    Full,
    Diagonal,
    Banded {
        lower_bandwidth: u32,
        upper_bandwidth: u32,
    },
    Csr {
        row_offsets: Box<[u32]>,
        column_indices: Box<[u32]>,
    },
    Affine {
        domain: StructuredIndexDomain,
        row_start: u32,
        column_maps: Box<[AffineColumnMap]>,
    },
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct StructuralPatternWire {
    rows: u32,
    columns: u32,
    representation: PatternRepresentationWire,
    provenance: PatternProvenanceWire,
}

impl StructuralPatternWire {
    /// Rebuild a pattern from wire claims through the checked local
    /// constructors.
    ///
    /// SOLVE-C17 scope: this proves shape, bandwidth, and CSR integrity, and it
    /// refuses a dummy-span provenance. It does *not* prove that a sparse claim
    /// matches the relation its owner computes — no owner is reachable from
    /// here. See the remaining-scope paragraph on [`PatternProvenance`].
    fn replay(self) -> Result<StructuralPattern, StructuralPatternError> {
        let provenance = self.provenance.checked()?;
        let rows = self.rows as usize;
        let columns = self.columns as usize;
        match self.representation {
            PatternRepresentationWire::Empty => StructuralPattern::empty(rows, columns, provenance),
            PatternRepresentationWire::Full => StructuralPattern::full(rows, columns, provenance),
            PatternRepresentationWire::Diagonal => {
                StructuralPattern::diagonal(rows, columns, provenance)
            }
            PatternRepresentationWire::Banded {
                lower_bandwidth,
                upper_bandwidth,
            } => StructuralPattern::banded(
                rows,
                columns,
                lower_bandwidth,
                upper_bandwidth,
                provenance,
            ),
            PatternRepresentationWire::Csr {
                row_offsets,
                column_indices,
            } => StructuralPattern::csr(rows, columns, row_offsets, column_indices, provenance),
            PatternRepresentationWire::Affine {
                domain,
                row_start,
                column_maps,
            } => StructuralPattern::checked_affine(
                rows,
                columns,
                domain,
                row_start,
                column_maps,
                provenance,
            ),
        }
    }
}

impl<'de> Deserialize<'de> for StructuralPattern {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        StructuralPatternWire::deserialize(deserializer)?
            .replay()
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn provenance() -> PatternProvenance {
        PatternProvenance::derived(
            PatternDerivation::DependencyPropagation,
            Span::from_offsets(
                rumoca_core::SourceId::from_source_name("structural_pattern.mo"),
                0,
                1,
            ),
        )
        .expect("fixture provenance")
    }

    #[test]
    fn csr_construction_rejects_unsorted_and_out_of_bounds_columns() {
        assert!(matches!(
            StructuralPattern::csr(2, 3, [0, 2, 2], [2, 1], provenance()),
            Err(StructuralPatternError::ColumnsNotStrictlyIncreasing { row: 0 })
        ));
        assert!(matches!(
            StructuralPattern::csr(1, 3, [0, 1], [3], provenance()),
            Err(StructuralPatternError::ColumnOutOfBounds { row: 0, .. })
        ));
    }

    #[test]
    fn checked_pattern_round_trip_preserves_membership() {
        let pattern = StructuralPattern::csr(3, 4, [0, 2, 3, 3], [0, 3, 2], provenance()).unwrap();
        let json = serde_json::to_string(&pattern).unwrap();
        let decoded: StructuralPattern = serde_json::from_str(&json).unwrap();
        assert_eq!(decoded, pattern);
        assert!(decoded.contains(0, 3));
        assert!(!decoded.contains(2, 0));
    }

    #[test]
    fn compact_patterns_color_without_dense_column_rows() {
        let full = StructuralPattern::full(4, 5, provenance()).expect("full pattern");
        assert_eq!(
            full.column_coloring().groups(),
            &[
                Box::from([0]),
                Box::from([1]),
                Box::from([2]),
                Box::from([3]),
                Box::from([4]),
            ]
        );

        let diagonal = StructuralPattern::diagonal(4, 5, provenance()).expect("diagonal pattern");
        assert_eq!(
            diagonal.column_coloring().groups(),
            &[Box::from([0, 1, 2, 3, 4])]
        );

        let banded = StructuralPattern::banded(7, 7, 1, 1, provenance()).expect("banded pattern");
        assert_eq!(
            banded.column_coloring().groups(),
            &[Box::from([0, 3, 6]), Box::from([1, 4]), Box::from([2, 5]),]
        );
    }

    #[test]
    fn dummy_provenance_is_rejected() {
        assert_eq!(
            PatternProvenance::derived(PatternDerivation::ConservativeFull, Span::DUMMY),
            Err(StructuralPatternError::MissingProvenance)
        );
    }

    /// Compile-time probe: the inherent constant is selected only when the
    /// parameter implements `Deserialize`; otherwise the blanket trait constant
    /// answers. SPEC_0039 forbids the pattern fields from deriving it.
    struct DeserializeProbe<T>(std::marker::PhantomData<T>);

    trait DeserializeProbeFallback {
        const DESERIALIZES: bool = false;
    }

    impl<T> DeserializeProbeFallback for DeserializeProbe<T> {}

    impl<T: for<'de> Deserialize<'de>> DeserializeProbe<T> {
        const DESERIALIZES: bool = true;
    }

    #[test]
    fn only_the_checked_pattern_type_implements_deserialize() {
        let decodable: Vec<&str> = [
            (
                "PatternProvenance",
                DeserializeProbe::<PatternProvenance>::DESERIALIZES,
            ),
            (
                "PatternRepresentation",
                DeserializeProbe::<PatternRepresentation>::DESERIALIZES,
            ),
            (
                "StructuralPattern",
                DeserializeProbe::<StructuralPattern>::DESERIALIZES,
            ),
        ]
        .into_iter()
        .filter_map(|(name, deserializes)| deserializes.then_some(name))
        .collect();
        assert_eq!(
            decodable,
            ["StructuralPattern"],
            "SPEC_0039: pattern fields decode only through the checked wire replay"
        );
    }

    #[test]
    fn dummy_provenance_cannot_be_minted_by_deserialization() {
        let forged = serde_json::json!({
            "rows": 2,
            "columns": 2,
            "representation": "full",
            "provenance": {
                "derivation": "conservative_full",
                "span": Span::DUMMY,
            },
        });
        let error = serde_json::from_value::<StructuralPattern>(forged)
            .expect_err("dummy-span provenance must not decode");
        assert!(
            error
                .to_string()
                .contains(&StructuralPatternError::MissingProvenance.to_string()),
            "unexpected decode error: {error}"
        );
    }

    fn owner_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("pattern_authority.mo"),
            0,
            1,
        )
    }

    /// `y0 = seed1`, `y1 = 0`: the owner proves an edge at (0, 1) and no edge
    /// anywhere in row 1.
    fn owner_block() -> ScalarProgramBlock {
        ScalarProgramBlock::with_program_spans(
            vec![vec![
                LinearOp::LoadSeed { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::StoreOutput { src: 1 },
            ]],
            vec![owner_span()],
        )
        .expect("owner block is computable")
    }

    #[test]
    fn derived_rows_come_from_the_owner_and_not_from_any_caller() {
        let pattern = StructuralPattern::derive_from_scalar_jvp(&owner_block(), 2, 3, owner_span())
            .expect("owner is derivable");
        assert!(pattern.contains(0, 1));
        assert!(!pattern.contains(0, 0));
        assert!(!pattern.contains(1, 1));
        assert_eq!(
            pattern.provenance().derivation(),
            PatternDerivation::DependencyPropagation
        );
        assert_eq!(pattern.provenance().span(), owner_span());
    }

    #[test]
    fn a_derived_pattern_cannot_be_asked_for_a_dropped_edge() {
        // SPEC_0039 / SOLVE-C17 forgery regression. The whole public surface
        // that yields a pattern able to omit an edge is enumerated here, and
        // none of it accepts a dependency row: the caller supplies the checked
        // owner and the extents only, so the (0, 1) edge the owner proves is
        // present in every reachable result.
        let block = owner_block();
        let derived = StructuralPattern::derive_from_scalar_jvp(&block, 2, 3, owner_span())
            .expect("owner is derivable");
        let projected = derived
            .project(&[0, 1], &[0, 1, 2])
            .expect("projection of a certified pattern");
        let conservative = StructuralPattern::full(
            2,
            3,
            PatternProvenance::derived(PatternDerivation::ConservativeFull, owner_span())
                .expect("conservative provenance"),
        )
        .expect("conservative pattern");
        for pattern in [&derived, &projected, &conservative] {
            assert!(
                pattern.contains(0, 1),
                "a reachable constructor dropped an edge the owner proves"
            );
        }
    }

    #[test]
    fn projection_cannot_invent_an_edge_the_source_does_not_certify() {
        let derived = StructuralPattern::derive_from_scalar_jvp(&owner_block(), 2, 3, owner_span())
            .expect("owner is derivable");
        let projected = derived.project(&[1], &[0, 1, 2]).expect("empty sub-block");
        assert!(!projected.contains(0, 0));
        assert!(!projected.contains(0, 1));
        assert!(!projected.contains(0, 2));
        assert_eq!(projected.provenance(), derived.provenance());
        assert!(matches!(
            derived.project(&[2], &[0]),
            Err(StructuralPatternError::DependencyContract { .. })
        ));
        assert!(matches!(
            derived.project(&[0], &[3]),
            Err(StructuralPatternError::DependencyContract { .. })
        ));
    }

    #[test]
    fn a_dummy_owner_span_cannot_anchor_a_derivation() {
        assert!(matches!(
            StructuralPattern::derive_from_scalar_jvp(&owner_block(), 2, 3, Span::DUMMY),
            Err(StructuralPatternError::DependencyContract { .. })
        ));
    }

    /// SPEC_0039 / SOLVE-C17: the only constructor that accepts caller-supplied
    /// dependency rows must stay behind the fixture gate. This reads the module
    /// source so the gate cannot be removed without failing here.
    #[test]
    fn caller_supplied_rows_stay_behind_the_fixture_gate() {
        let source = include_str!("structural_pattern.rs");
        // Assembled at runtime so this test's own text is not a match.
        let declaration = ["pub fn from_row_", "dependencies("].concat();
        let declarations = source.match_indices(declaration.as_str()).count();
        assert_eq!(
            declarations, 1,
            "exactly one caller-row constructor may exist"
        );
        let declaration_prefix = source
            .split(declaration.as_str())
            .next()
            .expect("the declaration is preceded by its attribute");
        let gated = declaration_prefix
            .strip_suffix("    ")
            .and_then(|prefix| prefix.lines().next_back())
            == Some("    #[cfg(any(test, feature = \"pattern-fixtures\"))]");
        assert!(
            gated,
            "from_row_dependencies must be declared directly under the pattern-fixtures gate"
        );
    }

    /// SPEC_0039 / SOLVE-C17: production cannot mint a fixture pattern because
    /// no crate turns the gate on outside `[dev-dependencies]`.
    #[test]
    fn pattern_fixture_feature_is_dev_only() {
        let workspace = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(std::path::Path::parent)
            .expect("workspace root")
            .join("crates");
        let mut offenders = Vec::new();
        for entry in std::fs::read_dir(&workspace).expect("crates directory is readable") {
            let manifest = entry
                .expect("crate directory entry")
                .path()
                .join("Cargo.toml");
            let Ok(text) = std::fs::read_to_string(&manifest) else {
                continue;
            };
            collect_fixture_offenders(&manifest, &text, &mut offenders);
        }
        assert!(
            offenders.is_empty(),
            "pattern-fixtures may only be enabled from [dev-dependencies]: {offenders:?}"
        );
    }

    /// Record every line of one manifest that turns the fixture gate on from a
    /// section other than `[dev-dependencies]`.
    fn collect_fixture_offenders(
        manifest: &std::path::Path,
        text: &str,
        offenders: &mut Vec<String>,
    ) {
        let mut in_dev_section = false;
        for line in text.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with('[') {
                in_dev_section = trimmed.contains("dev-dependencies");
            }
            if trimmed.contains("pattern-fixtures")
                && !trimmed.starts_with('#')
                && !trimmed.starts_with("pattern-fixtures =")
                && !in_dev_section
            {
                offenders.push(format!("{}: {trimmed}", manifest.display()));
            }
        }
    }

    #[test]
    fn wire_replay_rechecks_representation_claims() {
        let forged = serde_json::json!({
            "rows": 1,
            "columns": 2,
            "representation": {
                "csr": { "row_offsets": [0, 3], "column_indices": [0, 1] },
            },
            "provenance": {
                "derivation": "dependency_propagation",
                "span": provenance().span(),
            },
        });
        assert!(
            serde_json::from_value::<StructuralPattern>(forged).is_err(),
            "an inconsistent CSR claim must not decode"
        );
    }

    /// SPEC_0039: every wire record here is exactly its constructor inputs, so
    /// an extra field is a smuggling attempt and must fail closed rather than
    /// be dropped silently.
    #[test]
    fn wire_records_reject_smuggled_fields() {
        let span = serde_json::to_value(provenance().span()).expect("span serializes");
        let root_extra = serde_json::json!({
            "rows": 1,
            "columns": 1,
            "representation": "full",
            "provenance": { "derivation": "conservative_full", "span": span },
            "certified": true,
        });
        assert!(
            serde_json::from_value::<StructuralPattern>(root_extra).is_err(),
            "an unknown pattern field must not decode"
        );

        let provenance_extra = serde_json::json!({
            "rows": 1,
            "columns": 1,
            "representation": "full",
            "provenance": {
                "derivation": "conservative_full",
                "span": span,
                "owner": "forged",
            },
        });
        assert!(
            serde_json::from_value::<StructuralPattern>(provenance_extra).is_err(),
            "an unknown provenance field must not decode"
        );

        let representation_extra = serde_json::json!({
            "rows": 2,
            "columns": 2,
            "representation": {
                "banded": { "lower_bandwidth": 0, "upper_bandwidth": 0, "exact": true },
            },
            "provenance": { "derivation": "conservative_full", "span": span },
        });
        assert!(
            serde_json::from_value::<StructuralPattern>(representation_extra).is_err(),
            "an unknown representation field must not decode"
        );
    }
}
