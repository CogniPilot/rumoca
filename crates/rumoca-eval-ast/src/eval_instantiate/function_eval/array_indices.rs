/// Generate all array indices for multi-dimensional arrays.
/// For dims = `[2, 3]`, generates: `[[1,1], [1,2], [1,3], [2,1], [2,2], [2,3]]`.
/// Uses 1-based indexing per Modelica semantics.
pub fn generate_array_indices(dims: &[i64]) -> Vec<Vec<i64>> {
    if dims.is_empty() {
        return vec![];
    }

    let total: usize = dims.iter().map(|&d| d as usize).product();
    let mut result = Vec::with_capacity(total);
    let mut indices = vec![1i64; dims.len()];
    loop {
        result.push(indices.clone());

        let mut i = dims.len();
        while i > 0 {
            i -= 1;
            indices[i] += 1;
            if indices[i] <= dims[i] {
                break;
            }
            if i == 0 {
                return result;
            }
            indices[i] = 1;
        }
    }
}
