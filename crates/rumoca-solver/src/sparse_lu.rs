//! Pattern-keyed sparse LU shared by the projection Newton and the
//! linearly implicit integrators.
//!
//! The compressed-column structure is fixed at construction, so the symbolic
//! factorization is computed once; a numeric factorization is redone only when
//! a stored value changed bitwise since the last one. Callers write values in
//! storage order (`coordinates`) and keep any scaling of their own.

use faer::{
    Conj, MatMut, Par,
    dyn_stack::{MemBuffer, MemStack, StackReq},
    sparse::{
        SparseColMat, Triplet,
        linalg::lu::{LuRef, NumericLu, SymbolicLu, factorize_symbolic_lu},
    },
};

/// A square sparse matrix of fixed structure and its LU factors.
#[derive(Clone)]
pub struct SparseLuSystem {
    matrix: SparseColMat<usize, f64>,
    coordinates: Box<[(usize, usize)]>,
    symbolic: SymbolicLu<usize>,
    factorization: NumericFactor,
    factor_values: Box<[u64]>,
    workspace: SparseWorkspace,
}

#[derive(Clone, Default)]
enum NumericFactor {
    #[default]
    Unfactored,
    Ready(NumericLu<usize, f64>),
    Rejected(NumericLu<usize, f64>),
}

impl NumericFactor {
    fn into_storage(self) -> NumericLu<usize, f64> {
        match self {
            Self::Unfactored => NumericLu::new(),
            Self::Ready(numeric) | Self::Rejected(numeric) => numeric,
        }
    }
}

impl SparseLuSystem {
    /// Prepare a `dimension` x `dimension` system whose structural nonzeros
    /// are `coordinates` (row, column); repeated coordinates are stored once.
    /// `None` when faer rejects the structure or its symbolic factorization.
    #[must_use]
    pub fn new(
        dimension: usize,
        coordinates: impl IntoIterator<Item = (usize, usize)>,
    ) -> Option<Self> {
        let structure = coordinates
            .into_iter()
            .map(|(row, column)| (column, row))
            .collect::<std::collections::BTreeSet<_>>();
        let triplets = structure
            .into_iter()
            .map(|(column, row)| Triplet::new(row, column, 0.0))
            .collect::<Vec<_>>();
        let matrix = SparseColMat::try_new_from_triplets(dimension, dimension, &triplets).ok()?;
        let symbolic = factorize_symbolic_lu(matrix.symbolic(), Default::default()).ok()?;
        let workspace = SparseWorkspace::new(&symbolic, faer::get_global_parallelism())?;
        let coordinates = matrix
            .as_ref()
            .triplet_iter()
            .map(|entry| (entry.row, entry.col))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let factor_values = vec![0; coordinates.len()].into_boxed_slice();
        Some(Self {
            matrix,
            coordinates,
            symbolic,
            factorization: NumericFactor::Unfactored,
            factor_values,
            workspace,
        })
    }

    /// (row, column) of each stored value, in storage (column-major) order.
    #[must_use]
    pub fn coordinates(&self) -> &[(usize, usize)] {
        &self.coordinates
    }

    /// Storage index of `(row, column)`, if it is structural.
    #[must_use]
    pub fn position(&self, row: usize, column: usize) -> Option<usize> {
        self.coordinates
            .binary_search_by(|&(r, c)| c.cmp(&column).then(r.cmp(&row)))
            .ok()
    }

    /// Set every stored value from its storage index and (row, column).
    pub fn fill(&mut self, mut value: impl FnMut(usize, usize, usize) -> f64) {
        let stored = self.matrix.val_mut().iter_mut();
        for (index, (stored, &(row, column))) in stored.zip(&self.coordinates).enumerate() {
            *stored = value(index, row, column);
        }
    }

    /// Factor the current values unless they are bitwise those of the last
    /// factorization. Returns whether usable factors are held.
    pub fn refactor_if_changed(&mut self) -> bool {
        let parallelism = faer::get_global_parallelism();
        if parallelism != self.workspace.parallelism {
            let Some(workspace) = SparseWorkspace::new(&self.symbolic, parallelism) else {
                return false;
            };
            self.workspace = workspace;
        }
        if !matches!(self.factorization, NumericFactor::Unfactored)
            && self
                .matrix
                .val()
                .iter()
                .zip(&self.factor_values)
                .all(|(value, cached)| value.to_bits() == *cached)
        {
            #[cfg(rumoca_qualification)]
            crate::qualification::factor(
                true,
                matches!(self.factorization, NumericFactor::Ready(_)),
            );
            return matches!(self.factorization, NumericFactor::Ready(_));
        }
        let mut numeric = std::mem::take(&mut self.factorization).into_storage();
        let succeeded = self
            .symbolic
            .factorize_numeric_lu(
                &mut numeric,
                self.matrix.as_ref(),
                parallelism,
                MemStack::new(&mut self.workspace.storage),
                Default::default(),
            )
            .is_ok();
        self.factorization = if succeeded {
            NumericFactor::Ready(numeric)
        } else {
            NumericFactor::Rejected(numeric)
        };
        #[cfg(rumoca_qualification)]
        crate::qualification::factor(false, succeeded);
        for (cached, value) in self.factor_values.iter_mut().zip(self.matrix.val()) {
            *cached = value.to_bits();
        }
        succeeded
    }

    /// Address of the solve workspace, so tests can tell a reused workspace
    /// from a fresh one (a clone allocates its own).
    #[cfg(test)]
    pub(crate) fn workspace_address(&self) -> usize {
        self.workspace.storage.as_ptr() as usize
    }

    /// Overwrite `rhs` with the solution of the last successful factorization.
    /// Returns `false`, leaving `rhs` unchanged, when none is held.
    pub fn solve_in_place(&mut self, rhs: &mut [f64]) -> bool {
        let NumericFactor::Ready(numeric) = &self.factorization else {
            return false;
        };
        let rows = rhs.len();
        // Only checked factorization of this owner's immutable CSC structure
        // constructs Ready; cloning preserves that pair, and refactoring takes
        // it out of Ready before mutating numeric storage.
        let factor = LuRef::new_unchecked(&self.symbolic, numeric);
        factor.solve_in_place_with_conj(
            Conj::No,
            MatMut::from_column_major_slice_mut(rhs, rows, 1),
            self.workspace.parallelism,
            MemStack::new(&mut self.workspace.storage),
        );
        true
    }
}

struct SparseWorkspace {
    parallelism: Par,
    requirements: StackReq,
    storage: MemBuffer,
}

impl SparseWorkspace {
    fn new(symbolic: &SymbolicLu<usize>, parallelism: Par) -> Option<Self> {
        let requirements = StackReq::any_of(&[
            symbolic.factorize_numeric_lu_scratch::<f64>(parallelism, Default::default()),
            symbolic.solve_in_place_scratch::<f64>(1, parallelism),
        ]);
        Some(Self {
            parallelism,
            requirements,
            storage: MemBuffer::try_new(requirements).ok()?,
        })
    }
}

impl Clone for SparseWorkspace {
    fn clone(&self) -> Self {
        Self {
            parallelism: self.parallelism,
            requirements: self.requirements,
            storage: MemBuffer::new(self.requirements),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn repeated_coordinates_are_stored_once_in_column_order() {
        let system = SparseLuSystem::new(2, [(1, 0), (0, 0), (1, 0), (1, 1)]).expect("structure");
        assert_eq!(system.coordinates(), &[(0, 0), (1, 0), (1, 1)]);
        assert_eq!(system.position(1, 0), Some(1));
        assert_eq!(system.position(0, 1), None);
    }

    #[test]
    fn solves_and_skips_an_unchanged_refactor() {
        // [[2, 0], [1, 4]] x = [2, 9] has x = [1, 2].
        let mut system = SparseLuSystem::new(2, [(0, 0), (1, 0), (1, 1)]).expect("structure");
        system.fill(|_, row, column| [[2.0, 0.0], [1.0, 4.0]][row][column]);
        assert!(system.refactor_if_changed());
        assert!(system.refactor_if_changed());
        let mut rhs = [2.0, 9.0];
        assert!(system.solve_in_place(&mut rhs));
        assert!((rhs[0] - 1.0).abs() < 1.0e-15 && (rhs[1] - 2.0).abs() < 1.0e-15);
    }

    #[test]
    fn an_unfactored_system_does_not_solve() {
        let mut system = SparseLuSystem::new(1, [(0, 0)]).expect("structure");
        let mut rhs = [3.0];
        assert!(!system.solve_in_place(&mut rhs));
        assert_eq!(rhs, [3.0]);
    }
}
