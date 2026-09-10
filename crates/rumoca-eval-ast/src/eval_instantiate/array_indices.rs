//! Row-major enumeration of array element indices (MLS §10.1).

/// Lazy row-major enumeration of 1-based array index tuples (MLS §10.1).
///
/// Yields the identical sequence as [`generate_array_indices`] without
/// materializing the whole domain up front, so callers that only need to walk
/// the domain stay O(rank) in resident memory instead of O(cardinality).
pub struct ArrayIndexTuples {
    dims: Vec<i64>,
    current: Vec<i64>,
    done: bool,
}

impl Iterator for ArrayIndexTuples {
    type Item = Vec<i64>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        let item = self.current.clone();
        self.advance();
        Some(item)
    }
}

impl ArrayIndexTuples {
    /// Increment the innermost-fastest counter, marking exhaustion on carry-out.
    fn advance(&mut self) {
        let mut position = self.dims.len();
        while position > 0 {
            position -= 1;
            self.current[position] += 1;
            if self.current[position] <= self.dims[position] {
                return;
            }
            if position == 0 {
                self.done = true;
                return;
            }
            self.current[position] = 1;
        }
        self.done = true;
    }
}

/// Enumerate all array indices for multi-dimensional arrays lazily.
/// For dims = `[2, 3]`, yields: `[1,1], [1,2], [1,3], [2,1], [2,2], [2,3]`.
/// Uses 1-based indexing per Modelica semantics (MLS §10.1).
pub fn array_index_tuples(dims: &[i64]) -> ArrayIndexTuples {
    // A rank-0 (scalar) shape has no indices. Degenerate (<= 0) extents keep the
    // historical counter behavior of `generate_array_indices`; callers guard
    // zero-sized arrays before reaching here (see `component_loop`).
    let done = dims.is_empty();
    ArrayIndexTuples {
        dims: dims.to_vec(),
        current: vec![1i64; dims.len()],
        done,
    }
}

/// Generate all array indices for multi-dimensional arrays.
/// For dims = `[2, 3]`, generates: `[[1,1], [1,2], [1,3], [2,1], [2,2], [2,3]]`.
/// Uses 1-based indexing per Modelica semantics (MLS §10.1).
pub fn generate_array_indices(dims: &[i64]) -> Vec<Vec<i64>> {
    array_index_tuples(dims).collect()
}

#[cfg(test)]
mod tests {
    use super::{array_index_tuples, generate_array_indices};

    /// Every enumeration this module promises, written out literally.
    ///
    /// `generate_array_indices` is a thin wrapper over `array_index_tuples`, so
    /// comparing the two would pass no matter what they both produced. These
    /// expectations are independent of the implementation: instantiate overlay
    /// order — and therefore flat variable order — is exactly this sequence.
    fn expected_enumerations() -> Vec<(Vec<i64>, Vec<Vec<i64>>)> {
        vec![
            // A rank-0 (scalar) shape has no index tuples at all.
            (Vec::new(), Vec::new()),
            (vec![1], vec![vec![1]]),
            (vec![3], vec![vec![1], vec![2], vec![3]]),
            // SPEC_0032 §2: row-major, innermost dimension varies fastest.
            (
                vec![2, 3],
                vec![
                    vec![1, 1],
                    vec![1, 2],
                    vec![1, 3],
                    vec![2, 1],
                    vec![2, 2],
                    vec![2, 3],
                ],
            ),
            (
                vec![2, 1, 4],
                vec![
                    vec![1, 1, 1],
                    vec![1, 1, 2],
                    vec![1, 1, 3],
                    vec![1, 1, 4],
                    vec![2, 1, 1],
                    vec![2, 1, 2],
                    vec![2, 1, 3],
                    vec![2, 1, 4],
                ],
            ),
            // Degenerate extents keep the historical counter behavior this
            // module doc commits to: one tuple is still emitted, and callers
            // (`component_loop`, `expand_array_component`) guard zero-sized
            // arrays before reaching here.
            (vec![0], vec![vec![1]]),
            (vec![2, 0], vec![vec![1, 1], vec![2, 1]]),
        ]
    }

    #[test]
    fn array_index_tuples_enumerates_the_documented_sequence() {
        for (dims, expected) in expected_enumerations() {
            assert_eq!(
                array_index_tuples(&dims).collect::<Vec<_>>(),
                expected,
                "dims {dims:?}"
            );
        }
    }

    #[test]
    fn generate_array_indices_enumerates_the_documented_sequence() {
        for (dims, expected) in expected_enumerations() {
            assert_eq!(generate_array_indices(&dims), expected, "dims {dims:?}");
        }
    }
}
