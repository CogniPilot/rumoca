//! Numeric storage and preparation-bound reads; no derivative or solver policy.
use super::*;
use std::sync::Arc;

#[cfg(test)]
thread_local! {
    static READ_PREPARATIONS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[cfg(test)]
pub(super) fn read_preparation_count() -> usize {
    READ_PREPARATIONS.get()
}

pub(crate) trait JacobianMatrix {
    fn shape(&self) -> (usize, usize);
    fn as_slice(&self) -> &[f64];
    fn value_layout(&self) -> Option<&Arc<solve::JacobianValueLayout>> {
        None
    }
    fn application_identity(&self) -> Option<&Arc<()>> {
        None
    }
    fn nrows(&self) -> usize {
        self.shape().0
    }
    fn ncols(&self) -> usize {
        self.shape().1
    }
    fn to_dense(&self) -> Option<DMatrix<f64>> {
        if let Some(layout) = self.value_layout() {
            if layout.shape() != self.shape() || layout.len() != self.as_slice().len() {
                return None;
            }
            let mut matrix = allocate_projection_jacobian(self.nrows(), self.ncols());
            for (slot, &value) in self.as_slice().iter().enumerate() {
                matrix[layout.coordinate(slot)?] = value;
            }
            Some(matrix)
        } else {
            (self.nrows().checked_mul(self.ncols()) == Some(self.as_slice().len()))
                .then(|| DMatrix::from_column_slice(self.nrows(), self.ncols(), self.as_slice()))
        }
    }
}
impl JacobianMatrix for DMatrix<f64> {
    fn shape(&self) -> (usize, usize) {
        self.shape()
    }
    fn as_slice(&self) -> &[f64] {
        self.as_slice()
    }
}

#[derive(Clone)]
pub(crate) enum JacobianStorage {
    Dense(DMatrix<f64>),
    Pattern {
        application: solve::ProjectionJacobianApplication,
        values: Vec<f64>,
    },
}
impl JacobianStorage {
    pub(crate) fn new(structure: &solve::JacobianStructure, rows: usize, columns: usize) -> Self {
        match structure.jacobian_application() {
            Some(application) => Self::Pattern {
                application: application.clone(),
                values: vec![0.0; application.output_len()],
            },
            None => Self::Dense(allocate_projection_jacobian(rows, columns)),
        }
    }
    pub(crate) fn as_mut_slice(&mut self) -> &mut [f64] {
        match self {
            Self::Dense(m) => m.as_mut_slice(),
            Self::Pattern { values, .. } => values,
        }
    }
    pub(crate) fn owns(&self, application: &solve::ProjectionJacobianApplication) -> bool {
        match self {
            Self::Dense(_) => false,
            Self::Pattern {
                application: bound,
                values,
            } => {
                Arc::ptr_eq(bound.identity(), application.identity())
                    && Arc::ptr_eq(bound.value_layout(), application.value_layout())
                    && bound.block_index() == application.block_index()
                    && bound.rows() == application.rows()
                    && bound.y_indices() == application.y_indices()
                    && bound.source().shares_program_owner(application.source())
                    && values.len() == application.output_len()
            }
        }
    }
}
impl JacobianMatrix for JacobianStorage {
    fn shape(&self) -> (usize, usize) {
        match self {
            Self::Dense(m) => m.shape(),
            Self::Pattern { application, .. } => application.value_layout().shape(),
        }
    }
    fn as_slice(&self) -> &[f64] {
        match self {
            Self::Dense(m) => m.as_slice(),
            Self::Pattern { values, .. } => values,
        }
    }
    fn value_layout(&self) -> Option<&Arc<solve::JacobianValueLayout>> {
        match self {
            Self::Dense(_) => None,
            Self::Pattern { application, .. } => Some(application.value_layout()),
        }
    }
    fn application_identity(&self) -> Option<&Arc<()>> {
        match self {
            Self::Dense(_) => None,
            Self::Pattern { application, .. } => Some(application.identity()),
        }
    }
}

/// Gather derived once for an exact consumer ordering and numeric-storage owner.
#[derive(Clone)]
pub(super) struct JacobianReadMap {
    shape: (usize, usize),
    layout: Option<Arc<solve::JacobianValueLayout>>,
    application_identity: Option<Arc<()>>,
    offsets: Box<[Option<usize>]>,
}
impl JacobianReadMap {
    pub(super) fn prepare(
        source: &dyn JacobianMatrix,
        coordinates: impl IntoIterator<Item = (usize, usize)>,
    ) -> Option<Self> {
        #[cfg(test)]
        READ_PREPARATIONS.set(READ_PREPARATIONS.get() + 1);
        let shape = source.shape();
        let offsets = coordinates
            .into_iter()
            .map(|(row, column)| {
                if row >= shape.0 || column >= shape.1 {
                    return None;
                }
                if let Some(layout) = source.value_layout() {
                    layout.locate(row, column)
                } else {
                    Some(Some(column.checked_mul(shape.0)?.checked_add(row)?))
                }
            })
            .collect::<Option<Box<[_]>>>()?;
        Some(Self {
            shape,
            layout: source.value_layout().cloned(),
            application_identity: source.application_identity().cloned(),
            offsets,
        })
    }
    pub(super) fn matches(&self, source: &dyn JacobianMatrix) -> bool {
        self.shape == source.shape()
            && match (&self.application_identity, source.application_identity()) {
                (None, None) => true,
                (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                _ => false,
            }
            && match (&self.layout, source.value_layout()) {
                (None, None) => {
                    self.shape.0.checked_mul(self.shape.1) == Some(source.as_slice().len())
                }
                (Some(a), Some(b)) => Arc::ptr_eq(a, b) && source.as_slice().len() == a.len(),
                _ => false,
            }
    }
    pub(super) fn bind<'a>(&'a self, source: &'a dyn JacobianMatrix) -> Option<JacobianReads<'a>> {
        self.matches(source).then_some(JacobianReads {
            offsets: &self.offsets,
            values: source.as_slice(),
        })
    }
}
#[derive(Clone, Copy)]
pub(super) struct JacobianReads<'a> {
    offsets: &'a [Option<usize>],
    values: &'a [f64],
}
impl JacobianReads<'_> {
    pub(super) fn at(&self, ordinal: usize) -> f64 {
        self.offsets[ordinal].map_or(0.0, |offset| self.values[offset])
    }
}

#[cfg(test)]
pub(super) struct CompactFixture {
    layout: Arc<solve::JacobianValueLayout>,
    identity: Arc<()>,
    values: Vec<f64>,
}

#[cfg(test)]
impl CompactFixture {
    pub(super) fn from_dense(pattern: solve::StructuralPattern, dense: &DMatrix<f64>) -> Self {
        let structure = solve::JacobianStructure::derived(pattern);
        let layout = Arc::clone(structure.value_layout().unwrap());
        assert_eq!(layout.shape(), dense.shape());
        let values = (0..layout.len())
            .map(|slot| dense[layout.coordinate(slot).unwrap()])
            .collect();
        Self {
            layout,
            identity: Arc::new(()),
            values,
        }
    }
    pub(super) fn stored_len(&self) -> usize {
        self.values.len()
    }
}

#[cfg(test)]
impl JacobianMatrix for CompactFixture {
    fn shape(&self) -> (usize, usize) {
        self.layout.shape()
    }
    fn as_slice(&self) -> &[f64] {
        &self.values
    }
    fn value_layout(&self) -> Option<&Arc<solve::JacobianValueLayout>> {
        Some(&self.layout)
    }
    fn application_identity(&self) -> Option<&Arc<()>> {
        Some(&self.identity)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{SourceId, Span};

    fn pattern() -> solve::StructuralPattern {
        solve::StructuralPattern::from_row_dependencies(
            2,
            3,
            &[vec![0, 2], vec![1]],
            solve::PatternProvenance::derived(
                solve::PatternDerivation::DependencyPropagation,
                Span::from_offsets(SourceId::from_source_name("compact_reads.mo"), 0, 1),
            )
            .unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn prepared_reads_bind_exact_layout_and_preserve_structural_positive_zero() {
        let dense = DMatrix::from_row_slice(2, 3, &[1.0, 0.0, -0.0, 0.0, f64::NAN, 0.0]);
        let compact = CompactFixture::from_dense(pattern(), &dense);
        let before = super::super::jacobian_allocation_count();
        let map = JacobianReadMap::prepare(&compact, [(0, 0), (0, 1), (0, 2), (1, 1)]).unwrap();
        let reads = map.bind(&compact).unwrap();
        assert_eq!(reads.at(0), 1.0);
        assert_eq!(reads.at(1).to_bits(), 0.0_f64.to_bits());
        assert_eq!(reads.at(2).to_bits(), (-0.0_f64).to_bits());
        assert!(reads.at(3).is_nan());
        assert_eq!(super::super::jacobian_allocation_count(), before);
        let foreign = CompactFixture::from_dense(pattern(), &dense);
        assert!(map.bind(&foreign).is_none());
        let shared_layout_foreign_application = CompactFixture {
            layout: Arc::clone(&compact.layout),
            identity: Arc::new(()),
            values: compact.values.clone(),
        };
        assert!(map.bind(&shared_layout_foreign_application).is_none());
        assert!(JacobianReadMap::prepare(&compact, [(2, 0)]).is_none());
        assert!(JacobianReadMap::prepare(&compact, [(0, 3)]).is_none());
    }

    #[test]
    fn dense_materialization_is_explicit_and_independent() {
        let dense = DMatrix::from_row_slice(2, 3, &[1.0, 0.0, 2.0, 0.0, -3.0, 0.0]);
        let compact = CompactFixture::from_dense(pattern(), &dense);
        let before = super::super::jacobian_allocation_count();
        let mut materialized = compact.to_dense().unwrap();
        assert_eq!(super::super::jacobian_allocation_count(), before + 1);
        assert_eq!(materialized, dense);
        materialized[(0, 0)] = 42.0;
        assert_eq!(compact.values[0], 1.0);
    }
}
