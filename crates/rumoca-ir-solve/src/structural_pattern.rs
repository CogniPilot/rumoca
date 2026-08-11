// SPEC_0021 file-size exception - split plan: split the derivation walk by construction owner (scalar-JVP derivation into structural_pattern/scalar_jvp.rs, seed/output dependency derivation into structural_pattern/dependency.rs, wire records into structural_pattern/wire.rs), leaving construction + provenance here; tracked as the pattern-authority follow-up slice (dev/2026-08-11 remediation note).
use std::cmp::Reverse;
use std::collections::BTreeSet;

use rumoca_core::Span;
use serde::{Deserialize, Deserializer, Serialize};

use crate::{BinaryOp, LinearOp, Reg, ScalarProgramBlock};

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
/// [`PatternProvenanceWire`]), and [`PatternProvenance::derived`] stays public
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
/// A caller cannot name its own dependency rows — `from_row_dependencies`
/// exists only under the `pattern-fixtures` feature, which the workspace
/// enables from `[dev-dependencies]` only, so it is absent from this crate's
/// own default build:
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
/// // The owner proves an edge at (0, 0); this row set drops it.
/// let forged =
///     StructuralPattern::from_row_dependencies(1, 1, &[Vec::new()], provenance).unwrap();
/// assert!(!forged.contains(0, 0));
/// ```
///
/// Nor can it reach the edge-omitting storage constructors directly; they are
/// private so that only this module's exhaustive derivations can select them:
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

    pub fn compressed_seed_count(&self) -> usize {
        self.groups.len()
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
        }
        columns
    }

    pub fn column_coloring(&self) -> ColumnColoring {
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
            let dependencies = dependencies.into_conservative_set(columns);
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

// ---------------------------------------------------------------------------
// SPEC_0039 / SOLVE-C17: exhaustive structural dependency derivation.
//
// This analysis is the proof that backs every sparse pattern this module
// issues, so it lives with the pattern authority rather than with any
// consumer. It is a pure structural walk over the checked Solve IR: it reads
// `LinearOp` register flow only and never evaluates a value.
// ---------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
enum DependencyState {
    Known(BTreeSet<usize>),
    Unknown,
}

#[derive(Clone, Copy)]
enum DependencySource {
    Seed,
    SolverY,
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
            (Self::Known(_), Self::Unknown)
            | (Self::Unknown, Self::Known(_))
            | (Self::Unknown, Self::Unknown) => Self::Unknown,
        }
    }

    fn into_conservative_set(self, columns: usize) -> BTreeSet<usize> {
        match self {
            Self::Known(indices) => indices,
            Self::Unknown => (0..columns).collect(),
        }
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
    match dependencies {
        DependencyState::Known(indices) => Ok(indices.iter().copied().collect()),
        DependencyState::Unknown => Err(dependency_error(
            "scalar row has an opaque dependency without a known column bound",
            None,
        )),
    }
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
    .map(|dependencies| match dependencies {
        DependencyState::Known(indices) => Ok(indices),
        DependencyState::Unknown => Err(dependency_error(
            "scalar output has an opaque solver-Y dependency",
            span,
        )),
    })
    .collect()
}

fn program_output_dependencies_with_fold(
    program: &[LinearOp],
    span: Option<Span>,
    fold_carried: Option<&[DependencyState]>,
    fold_captures: Option<&[DependencyState]>,
    conditional_captures: Option<&[DependencyState]>,
    source: DependencySource,
) -> Result<Vec<DependencyState>, StructuralPatternError> {
    let mut registers: Vec<Option<DependencyState>> = Vec::new();
    let mut outputs = Vec::new();
    for op in program.iter().cloned() {
        match op {
            LinearOp::Const { dst, .. }
            | LinearOp::LoadTime { dst }
            | LinearOp::LoadP { dst, .. } => set_empty_dependency(&mut registers, dst),
            LinearOp::LoadY { dst, index } => match source {
                DependencySource::Seed => set_empty_dependency(&mut registers, dst),
                DependencySource::SolverY => set_seed_dependency(&mut registers, dst, index),
            },
            LinearOp::LoadSeed { dst, index } => match source {
                DependencySource::Seed => set_seed_dependency(&mut registers, dst, index),
                DependencySource::SolverY => set_empty_dependency(&mut registers, dst),
            },
            LinearOp::LoadFoldCarried { dst, index } => {
                let dependency = fold_carried
                    .and_then(|values| values.get(index))
                    .cloned()
                    .ok_or_else(|| dependency_error("invalid function-fold carried load", span))?;
                set_register(&mut registers, dst, dependency);
            }
            LinearOp::LoadFoldIndex { dst, .. } => {
                set_empty_dependency(&mut registers, dst);
            }
            LinearOp::LoadFoldCapture { dst, index } => {
                let dependency = fold_captures
                    .and_then(|values| values.get(index))
                    .cloned()
                    .ok_or_else(|| dependency_error("invalid function-fold capture load", span))?;
                set_register(&mut registers, dst, dependency);
            }
            LinearOp::LoadFunctionConditionalCapture { dst, index } => {
                let dependency = conditional_captures
                    .and_then(|values| values.get(index))
                    .cloned()
                    .ok_or_else(|| {
                        dependency_error("invalid function-conditional capture load", span)
                    })?;
                set_register(&mut registers, dst, dependency);
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => {
                let captures = conditional_captures.ok_or_else(|| {
                    dependency_error("invalid function-conditional capture range load", span)
                })?;
                for offset in 0..count {
                    let dependency =
                        captures.get(index_start + offset).cloned().ok_or_else(|| {
                            dependency_error(
                                "invalid function-conditional capture range load",
                                span,
                            )
                        })?;
                    set_register(&mut registers, dst_start + offset as Reg, dependency);
                }
            }
            LinearOp::LoadIndexedP { dst, index, .. } => {
                copy_dependency(&mut registers, dst, index, span)?;
            }
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| {
                        dependency_error("runtime tensor projection extent overflow", span)
                    })?;
                let mut dependencies = DependencyState::empty();
                for offset in 0..count {
                    dependencies = dependencies.union(register(
                        &registers,
                        base + (offset * stride) as Reg,
                        span,
                    )?);
                }
                for index in indices {
                    if let crate::TensorIndex::Runtime(register_id) = index {
                        dependencies = dependencies.union(register(&registers, register_id, span)?);
                    }
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let carried = fold_carried.ok_or_else(|| {
                    dependency_error("invalid indexed function-fold carried load", span)
                })?;
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| {
                        dependency_error("indexed function-fold carried extent overflow", span)
                    })?;
                let mut dependencies = DependencyState::empty();
                for offset in 0..count {
                    let dependency = carried.get(base + offset * stride).ok_or_else(|| {
                        dependency_error("indexed function-fold carried range is invalid", span)
                    })?;
                    dependencies = dependencies.union(dependency.clone());
                }
                for index in indices {
                    if let crate::TensorIndex::Runtime(register_id) = index {
                        dependencies = dependencies.union(register(&registers, register_id, span)?);
                    }
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let captures = fold_captures.ok_or_else(|| {
                    dependency_error("invalid indexed function-fold capture load", span)
                })?;
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| {
                        dependency_error("indexed function-fold capture extent overflow", span)
                    })?;
                let mut dependencies = DependencyState::empty();
                for offset in 0..count {
                    let dependency = captures.get(base + offset * stride).ok_or_else(|| {
                        dependency_error("indexed function-fold capture range is invalid", span)
                    })?;
                    dependencies = dependencies.union(dependency.clone());
                }
                for index in indices {
                    if let crate::TensorIndex::Runtime(register_id) = index {
                        dependencies = dependencies.union(register(&registers, register_id, span)?);
                    }
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::LoadIndexedSeed {
                dst,
                base,
                count,
                index,
            } => set_indexed_seed_dependency(
                &mut registers,
                IndexedSeedDependency {
                    dst,
                    base,
                    count,
                    index,
                },
                span,
            )?,
            LinearOp::Move { dst, src } | LinearOp::Unary { dst, arg: src, .. } => {
                copy_dependency(&mut registers, dst, src, span)?;
            }
            LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
                set_union_dependency(&mut registers, dst, [lhs, rhs], span)?;
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                set_union_dependency(&mut registers, dst, [cond, if_true, if_false], span)?;
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                ..
            } => set_linear_solve_dependency(
                &mut registers,
                LinearSolveDependency {
                    dst,
                    matrix_start,
                    rhs_start,
                    n,
                },
                span,
            )?,
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => {
                let mut sources = Vec::with_capacity(count.saturating_mul(2));
                for term in 0..count {
                    sources.push(lhs_start + (term * lhs_stride) as Reg);
                    sources.push(rhs_start + (term * rhs_stride) as Reg);
                }
                let mut dependencies = DependencyState::empty();
                for source in sources {
                    dependencies = dependencies.union(register(&registers, source, span)?);
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            } => {
                for row in 0..rows {
                    for column in 0..columns {
                        let output = (row * columns + column) * lanes;
                        for lane in 0..lanes {
                            let mut dependencies = DependencyState::empty();
                            for term in 0..inner {
                                let lhs = (row * inner + term) * lanes;
                                let rhs = (term * columns + column) * lanes;
                                dependencies = dependencies.union(register(
                                    &registers,
                                    lhs_start + (lhs + lane) as Reg,
                                    span,
                                )?);
                                dependencies = dependencies.union(register(
                                    &registers,
                                    rhs_start + (rhs + lane) as Reg,
                                    span,
                                )?);
                                if lanes == 2 && lane == 1 {
                                    dependencies = dependencies.union(register(
                                        &registers,
                                        lhs_start + lhs as Reg,
                                        span,
                                    )?);
                                    dependencies = dependencies.union(register(
                                        &registers,
                                        rhs_start + rhs as Reg,
                                        span,
                                    )?);
                                }
                            }
                            set_register(
                                &mut registers,
                                dst_start + (output + lane) as Reg,
                                dependencies,
                            );
                        }
                    }
                }
            }
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
            } => {
                for element in 0..count {
                    let lhs = lhs_start + (element * lhs_stride * lanes) as Reg;
                    let rhs = rhs_start + (element * rhs_stride * lanes) as Reg;
                    let output = dst_start + (element * lanes) as Reg;
                    let primal =
                        register(&registers, lhs, span)?.union(register(&registers, rhs, span)?);
                    set_register(&mut registers, output, primal.clone());
                    if lanes == 2 {
                        let mut tangent = register(&registers, lhs + 1, span)?.union(register(
                            &registers,
                            rhs + 1,
                            span,
                        )?);
                        if matches!(op, BinaryOp::Mul | BinaryOp::Div) {
                            tangent = tangent.union(primal);
                        }
                        set_register(&mut registers, output + 1, tangent);
                    }
                }
            }
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes,
            } => {
                for (component, (first, second)) in
                    [(1usize, 2usize), (2, 0), (0, 1)].into_iter().enumerate()
                {
                    let lhs_first = lhs_start + (first * lanes) as Reg;
                    let lhs_second = lhs_start + (second * lanes) as Reg;
                    let rhs_first = rhs_start + (first * lanes) as Reg;
                    let rhs_second = rhs_start + (second * lanes) as Reg;
                    let primal = register(&registers, lhs_first, span)?
                        .union(register(&registers, lhs_second, span)?)
                        .union(register(&registers, rhs_first, span)?)
                        .union(register(&registers, rhs_second, span)?);
                    let output = dst_start + (component * lanes) as Reg;
                    set_register(&mut registers, output, primal.clone());
                    if lanes == 2 {
                        let tangent = primal
                            .union(register(&registers, lhs_first + 1, span)?)
                            .union(register(&registers, lhs_second + 1, span)?)
                            .union(register(&registers, rhs_first + 1, span)?)
                            .union(register(&registers, rhs_second + 1, span)?);
                        set_register(&mut registers, output + 1, tangent);
                    }
                }
            }
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            } => {
                let value_width = element_width * lanes;
                for row in 0..rows {
                    for column in 0..columns {
                        for value in 0..value_width {
                            let dst = (row * columns + column) * value_width + value;
                            let src = (column * rows + row) * value_width + value;
                            let dependencies = register(&registers, src_start + src as Reg, span)?;
                            set_register(&mut registers, dst_start + dst as Reg, dependencies);
                        }
                    }
                }
            }
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes,
            } => {
                visit_tensor_concatenate(
                    &sources,
                    &dimensions,
                    axis,
                    lanes,
                    |source, destination| {
                        let dependencies = register(&registers, source, span)?;
                        set_register(&mut registers, dst_start + destination as Reg, dependencies);
                        Ok::<(), StructuralPatternError>(())
                    },
                )?;
            }
            LinearOp::TensorUpdate {
                dst_start,
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
            } => {
                let count = dimensions.iter().fold(1usize, |count, extent| {
                    count.saturating_mul(*extent as usize)
                });
                let mut value_count = lanes;
                let mut selector = DependencyState::empty();
                for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
                    match subscript {
                        crate::TensorUpdateSubscript::Whole => {
                            value_count = value_count.saturating_mul(extent as usize);
                        }
                        crate::TensorUpdateSubscript::Index(crate::TensorIndex::Runtime(
                            register_id,
                        )) => {
                            selector = selector.union(register(&registers, *register_id, span)?);
                        }
                        crate::TensorUpdateSubscript::Index(crate::TensorIndex::Constant(_)) => {}
                        crate::TensorUpdateSubscript::Slice { start, dimensions } => {
                            let slice_count = dimensions.iter().fold(1usize, |count, extent| {
                                count.saturating_mul(*extent as usize)
                            });
                            selector = selector.union(register_range(
                                &registers,
                                *start,
                                slice_count,
                                span,
                            )?);
                            value_count = value_count.saturating_mul(slice_count);
                        }
                    }
                }
                let patch =
                    register_range(&registers, value_start, value_count, span)?.union(selector);
                for element in 0..count {
                    for lane in 0..lanes {
                        let offset = element * lanes + lane;
                        let dependencies = register(&registers, base_start + offset as Reg, span)?
                            .union(patch.clone());
                        set_register(&mut registers, dst_start + offset as Reg, dependencies);
                    }
                }
            }
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                lanes,
            } => {
                for element in 0..count {
                    for lane in 0..lanes {
                        let dependencies = register(&registers, value_start + lane as Reg, span)?;
                        set_register(
                            &mut registers,
                            dst_start + (element * lanes + lane) as Reg,
                            dependencies,
                        );
                    }
                }
            }
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes,
            } => {
                for offset in 0..size * size * lanes {
                    set_empty_dependency(&mut registers, dst_start + offset as Reg);
                }
            }
            LinearOp::TensorLoad {
                dst_start,
                count,
                seed_start,
                lanes,
                ..
            } => {
                for element in 0..count {
                    set_empty_dependency(&mut registers, dst_start + (element * lanes) as Reg);
                    if lanes == 2 {
                        let dependency = seed_start
                            .map_or_else(DependencyState::empty, |seed_start| {
                                DependencyState::singleton(seed_start + element)
                            });
                        set_register(
                            &mut registers,
                            dst_start + (element * lanes + 1) as Reg,
                            dependency,
                        );
                    }
                }
            }
            op @ (LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }) => {
                apply_runtime_dependency(&mut registers, op, span)?;
            }
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => {
                let carried = (0..program.carried_count)
                    .map(|offset| register(&registers, initial_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let carried =
                    function_fold_dependencies(&program, carried, &captures, span, source)?;
                for (offset, dependency) in carried.into_iter().enumerate() {
                    set_register(&mut registers, dst_start + offset as Reg, dependency);
                }
            }
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => {
                let activation = register(&registers, activation, span)?;
                let carried = (0..program.carried_count)
                    .map(|offset| register(&registers, initial_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let carried =
                    function_fold_dependencies(&program, carried, &captures, span, source)?;
                for (offset, dependency) in carried.into_iter().enumerate() {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.union(activation.clone()),
                    );
                }
            }
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => {
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut condition_dependency = DependencyState::empty();
                let mut result = vec![DependencyState::empty(); program.result_count];
                for arm in &program.arms {
                    let condition = program_output_dependencies_with_fold(
                        &arm.condition,
                        span,
                        fold_carried,
                        fold_captures,
                        Some(&captures),
                        source,
                    )?;
                    condition_dependency =
                        condition_dependency.union(condition.first().cloned().ok_or_else(
                            || dependency_error("missing conditional condition", span),
                        )?);
                    let branch = program_output_dependencies_with_fold(
                        &arm.result,
                        span,
                        fold_carried,
                        fold_captures,
                        Some(&captures),
                        source,
                    )?;
                    union_conditional_results(&mut result, branch, span)?;
                }
                let fallback = program_output_dependencies_with_fold(
                    &program.fallback,
                    span,
                    fold_carried,
                    fold_captures,
                    Some(&captures),
                    source,
                )?;
                union_conditional_results(&mut result, fallback, span)?;
                for (offset, dependency) in result.into_iter().enumerate() {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.union(condition_dependency.clone()),
                    );
                }
            }
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => {
                let mut dependency = DependencyState::empty();
                for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                    for offset in 0..value_type.scalar_count() as usize {
                        dependency =
                            dependency.union(register(&registers, start + offset as Reg, span)?);
                    }
                }
                let output_count = site
                    .output_scalar_count()
                    .ok_or_else(|| dependency_error("pure-call output width overflows", span))?;
                for offset in 0..output_count {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.clone(),
                    );
                }
            }
            LinearOp::PureCallDirectional {
                dst_start,
                input_starts,
                site,
            } => {
                let mut dependency = DependencyState::empty();
                for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                    for offset in 0..value_type.scalar_count() as usize {
                        dependency =
                            dependency.union(register(&registers, start + offset as Reg, span)?);
                    }
                }
                let output_count = site.output_scalar_count().ok_or_else(|| {
                    dependency_error("directional pure-call output width overflows", span)
                })?;
                for offset in 0..output_count {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.clone(),
                    );
                }
            }
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base,
                source_stride,
                dimensions,
                updates,
                nodes,
                lanes,
                ..
            } => {
                let carried = fold_carried.ok_or_else(|| {
                    dependency_error(
                        "aggregate output escaped its function-fold update body",
                        span,
                    )
                })?;
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| dependency_error("tensor update extent overflow", span))?;
                let mut update_dependency = DependencyState::empty();
                for update in &updates {
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
                        .ok_or_else(|| {
                            dependency_error("tensor update value extent overflow", span)
                        })?;
                    if let Some(condition) = update.condition {
                        update_dependency =
                            update_dependency.union(register(&registers, condition, span)?);
                    }
                    for element in 0..value_count {
                        for lane in 0..lanes {
                            update_dependency = update_dependency.union(register(
                                &registers,
                                update.value_start + (element * update.value_stride + lane) as Reg,
                                span,
                            )?);
                        }
                    }
                    for subscript in &update.subscripts {
                        if let crate::TensorSubscript::Index(crate::TensorIndex::Runtime(
                            register_id,
                        )) = subscript
                        {
                            update_dependency =
                                update_dependency.union(register(&registers, *register_id, span)?);
                        }
                    }
                }
                for node in &nodes {
                    if let crate::FoldTensorNode::Select { condition, .. } = *node {
                        update_dependency =
                            update_dependency.union(register(&registers, condition, span)?);
                    }
                }
                for element in 0..count {
                    for lane in 0..lanes {
                        let unchanged = carried
                            .get(source_base + element * source_stride + lane)
                            .cloned()
                            .ok_or_else(|| {
                                dependency_error("tensor update carried source is invalid", span)
                            })?;
                        outputs.push(unchanged.union(update_dependency.clone()));
                    }
                }
            }
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                result_base,
                count,
                condition,
                ..
            } => {
                let parent = fold_carried.ok_or_else(|| {
                    dependency_error("nested aggregate fold escaped its parent update body", span)
                })?;
                let mut carried = Vec::with_capacity(program.carried_count);
                for source in initial.iter() {
                    match *source {
                        crate::FoldInitialSource::Registers { start, count } => {
                            for offset in 0..count {
                                carried.push(register(&registers, start + offset as Reg, span)?);
                            }
                        }
                        crate::FoldInitialSource::ParentCarried { base, count } => {
                            let end = base.checked_add(count).ok_or_else(|| {
                                dependency_error("nested fold carried range overflow", span)
                            })?;
                            let values = parent.get(base..end).ok_or_else(|| {
                                dependency_error("nested fold carried range is invalid", span)
                            })?;
                            carried.extend_from_slice(values);
                        }
                    }
                }
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let carried =
                    function_fold_dependencies(&program, carried, &captures, span, source)?;
                let end = result_base
                    .checked_add(count)
                    .ok_or_else(|| dependency_error("nested fold result range overflow", span))?;
                let result = carried
                    .get(result_base..end)
                    .ok_or_else(|| dependency_error("nested fold result range is invalid", span))?;
                if let Some(condition) = condition {
                    let condition = register(&registers, condition, span)?;
                    let output_base = outputs.len();
                    for (offset, nested) in result.iter().cloned().enumerate() {
                        let unchanged =
                            parent.get(output_base + offset).cloned().ok_or_else(|| {
                                dependency_error(
                                    "conditional nested fold parent range is invalid",
                                    span,
                                )
                            })?;
                        outputs.push(nested.union(unchanged).union(condition.clone()));
                    }
                } else {
                    outputs.extend_from_slice(result);
                }
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for ordinal in 0..count {
                    let offset = ordinal.checked_mul(stride).ok_or_else(|| {
                        dependency_error("conditional output range offset overflows", span)
                    })?;
                    let offset = Reg::try_from(offset).map_err(|_| {
                        dependency_error("conditional output range exceeds registers", span)
                    })?;
                    let source = start.checked_add(offset).ok_or_else(|| {
                        dependency_error("conditional output register overflows", span)
                    })?;
                    outputs.push(register(&registers, source, span)?);
                }
            }
            LinearOp::StoreOutput { src } => outputs.push(register(&registers, src, span)?),
        }
    }
    Ok(outputs)
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

struct IndexedSeedDependency {
    dst: Reg,
    base: usize,
    count: usize,
    index: Reg,
}

struct LinearSolveDependency {
    dst: Reg,
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
}

fn apply_runtime_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    operation: LinearOp,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    match operation {
        LinearOp::TableBounds { dst, table_id, .. } => {
            copy_dependency(registers, dst, table_id, span)
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
        } => set_union_dependency(registers, dst, [table_id, column, input], span),
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => set_union_dependency(registers, dst, [table_id, time], span),
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => set_union_dependency(registers, dst, [local_seed, global_seed], span),
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
        } => set_range_dependency(registers, dst, state_start, state_len, span),
        LinearOp::ImpureRandomInit { dst, seed } => copy_dependency(registers, dst, seed, span),
        LinearOp::ImpureRandom { dst, id, .. } => copy_dependency(registers, dst, id, span),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => set_union_dependency(registers, dst, [id, imin, imax], span),
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

fn set_indexed_seed_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dependency: IndexedSeedDependency,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    let mut dependencies = register(registers, dependency.index, span)?;
    let end = checked_indexed_seed_end(dependency.base, dependency.count, span)?;
    dependencies = dependencies.union(DependencyState::Known((dependency.base..end).collect()));
    set_register(registers, dependency.dst, dependencies);
    Ok(())
}

fn set_linear_solve_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dependency: LinearSolveDependency,
    span: Option<Span>,
) -> Result<(), StructuralPatternError> {
    let matrix_len = checked_product(dependency.n, dependency.n, "linear solve matrix", span)?;
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

fn checked_product(
    lhs: usize,
    rhs: usize,
    operation: &'static str,
    span: Option<Span>,
) -> Result<usize, StructuralPatternError> {
    lhs.checked_mul(rhs).ok_or_else(|| {
        dependency_error(
            format!("{operation} shape product {lhs} * {rhs} overflows register range"),
            span,
        )
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
        let gated = source
            .split(declaration.as_str())
            .next()
            .expect("the declaration is preceded by its attribute")
            .ends_with("#[cfg(any(test, feature = \"pattern-fixtures\"))]\n    ");
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
        assert!(
            offenders.is_empty(),
            "pattern-fixtures may only be enabled from [dev-dependencies]: {offenders:?}"
        );
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
