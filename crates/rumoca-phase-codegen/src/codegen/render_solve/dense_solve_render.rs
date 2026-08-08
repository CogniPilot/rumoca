use super::*;

#[derive(Clone, Copy)]
pub(in crate::codegen) struct MatMulRenderShape {
    pub(in crate::codegen) lhs_start: usize,
    pub(in crate::codegen) rhs_start: usize,
    pub(in crate::codegen) m: usize,
    pub(in crate::codegen) k: usize,
    pub(in crate::codegen) n: usize,
    pub(in crate::codegen) offset: usize,
}

const MATMUL_RENDER_ENUMERATION_LIMIT: usize = 1_000_000;

impl MatMulRenderShape {
    pub(in crate::codegen) fn output_count(self) -> Result<usize, minijinja::Error> {
        let count = checked_matmul_product(self.m, self.n, "MatMul output count")?;
        checked_matmul_render_count(count, "MatMul output count")
    }

    fn dense_lhs_count(self) -> Result<usize, minijinja::Error> {
        let count = checked_matmul_product(self.m, self.k, "MatMul lhs element count")?;
        checked_matmul_render_count(count, "MatMul lhs element count")
    }

    fn dense_rhs_count(self) -> Result<usize, minijinja::Error> {
        let count = checked_matmul_product(self.k, self.n, "MatMul rhs element count")?;
        checked_matmul_render_count(count, "MatMul rhs element count")
    }

    fn diagonal_output_count(self) -> Result<usize, minijinja::Error> {
        checked_matmul_render_count(self.m, "MatMul diagonal output count")
    }

    pub(in crate::codegen) fn end_offset(self) -> Result<usize, minijinja::Error> {
        checked_matmul_sum(
            self.offset,
            self.output_count()?,
            "MatMul output end offset",
        )
    }

    pub(in crate::codegen) fn output_index(self, slot: usize) -> Result<usize, minijinja::Error> {
        checked_matmul_sum(self.offset, slot, "MatMul output index")
    }

    fn lhs_matrix_reg(self, row: usize, col: usize) -> Result<usize, minijinja::Error> {
        let row_offset = checked_matmul_product(row, self.k, "MatMul lhs row offset")?;
        checked_matmul_sum(
            self.lhs_start,
            checked_matmul_sum(row_offset, col, "MatMul lhs cell offset")?,
            "MatMul lhs register index",
        )
    }

    fn rhs_matrix_reg(self, row: usize, col: usize) -> Result<usize, minijinja::Error> {
        let row_offset = checked_matmul_product(row, self.n, "MatMul rhs row offset")?;
        checked_matmul_sum(
            self.rhs_start,
            checked_matmul_sum(row_offset, col, "MatMul rhs cell offset")?,
            "MatMul rhs register index",
        )
    }

    fn diagonal_lhs_reg(self, index: usize) -> Result<usize, minijinja::Error> {
        let stride = checked_matmul_sum(self.m, 1, "MatMul diagonal stride")?;
        let offset = checked_matmul_product(index, stride, "MatMul diagonal lhs offset")?;
        checked_matmul_sum(self.lhs_start, offset, "MatMul diagonal lhs register index")
    }

    fn rhs_vector_reg(self, index: usize) -> Result<usize, minijinja::Error> {
        checked_matmul_sum(self.rhs_start, index, "MatMul rhs vector register index")
    }
}

fn validate_matmul_render_shape(
    shape: MatMulRenderShape,
    is_diagonal_matvec: bool,
    is_explicit_sparse: bool,
) -> Result<usize, minijinja::Error> {
    let end_offset = shape.end_offset()?;
    if is_diagonal_matvec {
        shape.diagonal_output_count()?;
    } else if is_explicit_sparse {
        shape.output_count()?;
    } else {
        shape.dense_lhs_count()?;
        shape.dense_rhs_count()?;
        shape.output_count()?;
    }
    Ok(end_offset)
}

fn checked_matmul_product(
    lhs: usize,
    rhs: usize,
    context: &'static str,
) -> Result<usize, minijinja::Error> {
    lhs.checked_mul(rhs)
        .ok_or_else(|| render_err(format!("{context} overflows host index range")))
}

fn checked_matmul_sum(
    lhs: usize,
    rhs: usize,
    context: &'static str,
) -> Result<usize, minijinja::Error> {
    lhs.checked_add(rhs)
        .ok_or_else(|| render_err(format!("{context} overflows host index range")))
}

fn checked_matmul_render_count(
    count: usize,
    context: &'static str,
) -> Result<usize, minijinja::Error> {
    if count > MATMUL_RENDER_ENUMERATION_LIMIT {
        return Err(render_err(format!(
            "{context} ({count}) exceeds render enumeration limit {MATMUL_RENDER_ENUMERATION_LIMIT}"
        )));
    }
    Ok(count)
}

// ─── MatMul MLIR emitter ─────────────────────────────────────────────────────

/// Render a `ComputeNode::MatMul` inner value as MLIR textual IR.
///
/// Three dispatch paths:
/// - `Diagonal` lhs (n=1, m=k): element-wise scalar multiplies (no GEMM)
/// - `Explicit { nnz }` lhs: scalar FMA over each nonzero position
/// - `Dense` (default): `linalg.matmul` with heap-allocated temporaries
///
/// Called from the MLIR template as `render_matmul_mlir(node.MatMul, node_id, output_offset)`.
///
/// Pass pipeline required (in addition to the standard scalar passes):
/// `--linalg-generalize-named-ops --convert-linalg-to-loops --convert-scf-to-cf --convert-cf-to-llvm`
pub(in crate::codegen) fn render_matmul_mlir_function(
    node: Value,
    node_id: Value,
    output_offset: Value,
) -> Result<String, minijinja::Error> {
    let id = required_usize_arg(&node_id, "MatMul node_id")?;
    let offset = required_usize_arg(&output_offset, "MatMul output_offset")?;

    let lhs_ops = get_field(&node, "lhs_ops")?;
    let lhs_start = solve_field_usize(&node, "lhs_start")?;
    let rhs_ops = get_field(&node, "rhs_ops")?;
    let rhs_start = solve_field_usize(&node, "rhs_start")?;
    let m = solve_field_usize(&node, "m")?;
    let k = solve_field_usize(&node, "k")?;
    let n = solve_field_usize(&node, "n")?;

    let pattern_kind = get_field(&node, "lhs_pattern_kind")
        .map_err(|err| render_err(format!("MatMul missing lhs_pattern_kind: {err}")))?
        .as_str()
        .ok_or_else(|| render_err("MatMul lhs_pattern_kind must be a string"))?
        .to_owned();
    let is_diagonal_matvec = pattern_kind == "diagonal" && n == 1 && m == k;
    let explicit_nnz = if !is_diagonal_matvec && pattern_kind == "csr" {
        Some(extract_pattern_nonzeros(&node)?)
    } else {
        None
    };
    let shape = MatMulRenderShape {
        lhs_start,
        rhs_start,
        m,
        k,
        n,
        offset,
    };
    let end_offset =
        validate_matmul_render_shape(shape, is_diagonal_matvec, explicit_nnz.is_some())?;

    let pfx = format!("mm{id}");
    let mut out = format!(
        "    // MatMul {}x{}x{} → out[{}..{}]\n",
        m, k, n, offset, end_offset
    );

    // Emit scalar ops that build the A and B register values.
    emit_linear_ops_mlir(&lhs_ops, &pfx, &mut out)?;
    emit_linear_ops_mlir(&rhs_ops, &pfx, &mut out)?;

    if is_diagonal_matvec {
        // Diagonal A (m×m) * x (m×1): out[offset+i] = A[i,i] * x[i]
        for i in 0..m {
            let a_reg = shape.diagonal_lhs_reg(i)?;
            let b_reg = shape.rhs_vector_reg(i)?;
            out.push_str(&format!(
                "    %{pfx}_diag{i} = arith.mulf %{pfx}_r{a_reg}, %{pfx}_r{b_reg} : f64\n"
            ));
            out.push_str(&format!(
                "    %{pfx}_douti{i} = arith.constant {} : index\n",
                shape.output_index(i)?
            ));
            out.push_str(&format!(
                "    memref.store %{pfx}_diag{i}, %out[%{pfx}_douti{i}] : memref<?xf64>\n"
            ));
        }
        return Ok(out);
    }

    if let Some(nnz) = explicit_nnz {
        render_explicit_sparse_matmul_mlir(&mut out, &pfx, &nnz, shape)?;
        return Ok(out);
    }

    render_dense_matmul_mlir(&mut out, &pfx, shape)?;
    Ok(out)
}

fn render_dense_matmul_mlir(
    out: &mut String,
    pfx: &str,
    shape: MatMulRenderShape,
) -> Result<(), minijinja::Error> {
    let MatMulRenderShape { m, k, n, .. } = shape;
    out.push_str(&format!(
        "    %{pfx}_A = memref.alloc() : memref<{m}x{k}xf64>\n"
    ));
    for i in 0..m {
        for j in 0..k {
            let reg = shape.lhs_matrix_reg(i, j)?;
            out.push_str(&format!(
                "    %{pfx}_Ai{i}_{j} = arith.constant {i} : index\n\
                 \t%{pfx}_Aj{i}_{j} = arith.constant {j} : index\n\
                 \tmemref.store %{pfx}_r{reg}, %{pfx}_A[%{pfx}_Ai{i}_{j}, %{pfx}_Aj{i}_{j}] : memref<{m}x{k}xf64>\n"
            ));
        }
    }

    out.push_str(&format!(
        "    %{pfx}_B = memref.alloc() : memref<{k}x{n}xf64>\n"
    ));
    for i in 0..k {
        for j in 0..n {
            let reg = shape.rhs_matrix_reg(i, j)?;
            out.push_str(&format!(
                "    %{pfx}_Bi{i}_{j} = arith.constant {i} : index\n\
                 \t%{pfx}_Bj{i}_{j} = arith.constant {j} : index\n\
                 \tmemref.store %{pfx}_r{reg}, %{pfx}_B[%{pfx}_Bi{i}_{j}, %{pfx}_Bj{i}_{j}] : memref<{k}x{n}xf64>\n"
            ));
        }
    }

    out.push_str(&format!(
        "    %{pfx}_zero = arith.constant 0.0 : f64\n\
         \t%{pfx}_C = memref.alloc() : memref<{m}x{n}xf64>\n\
         \tlinalg.fill ins(%{pfx}_zero : f64) outs(%{pfx}_C : memref<{m}x{n}xf64>)\n\
         \tlinalg.matmul ins(%{pfx}_A, %{pfx}_B : memref<{m}x{k}xf64>, memref<{k}x{n}xf64>) \
                        outs(%{pfx}_C : memref<{m}x{n}xf64>)\n"
    ));

    // Load C results into output memref.
    for i in 0..m {
        for j in 0..n {
            let slot = checked_matmul_sum(
                checked_matmul_product(i, n, "MatMul output row offset")?,
                j,
                "MatMul output cell offset",
            )?;
            let output_idx = shape.output_index(slot)?;
            out.push_str(&format!(
                "    %{pfx}_Ci{i}_{j} = arith.constant {i} : index\n\
                 \t%{pfx}_Cj{i}_{j} = arith.constant {j} : index\n\
                 \t%{pfx}_Cv{i}_{j} = memref.load %{pfx}_C[%{pfx}_Ci{i}_{j}, %{pfx}_Cj{i}_{j}] : memref<{m}x{n}xf64>\n\
                 \t%{pfx}_oi{i}_{j} = arith.constant {output_idx} : index\n\
                 \tmemref.store %{pfx}_Cv{i}_{j}, %out[%{pfx}_oi{i}_{j}] : memref<?xf64>\n"
            ));
        }
    }
    out.push_str(&format!(
        "    memref.dealloc %{pfx}_C : memref<{m}x{n}xf64>\n\
         \tmemref.dealloc %{pfx}_B : memref<{k}x{n}xf64>\n\
         \tmemref.dealloc %{pfx}_A : memref<{m}x{k}xf64>\n"
    ));
    Ok(())
}

fn render_explicit_sparse_matmul_mlir(
    out: &mut String,
    pfx: &str,
    nnz: &[(usize, usize)],
    shape: MatMulRenderShape,
) -> Result<(), minijinja::Error> {
    out.push_str(&format!("    // Explicit sparse: {} nnz\n", nnz.len()));
    for slot in 0..shape.output_count()? {
        let out_row = slot / shape.n;
        let out_col = slot % shape.n;
        let output_idx = shape.output_index(slot)?;
        let row_nzs = matmul_nnz_for_row(nnz, out_row)?;
        render_sparse_matmul_cell_mlir(out, pfx, shape, out_row, out_col, output_idx, &row_nzs)?;
    }
    Ok(())
}

fn render_sparse_matmul_cell_mlir(
    out: &mut String,
    pfx: &str,
    shape: MatMulRenderShape,
    out_row: usize,
    out_col: usize,
    output_idx: usize,
    nzs: &[(usize, usize)],
) -> Result<(), minijinja::Error> {
    if nzs.is_empty() {
        out.push_str(&format!(
            "    %{pfx}_ez{out_row}_{out_col} = arith.constant 0.0 : f64\n\
             \t%{pfx}_eoi{out_row}_{out_col} = arith.constant {output_idx} : index\n\
             \tmemref.store %{pfx}_ez{out_row}_{out_col}, %out[%{pfx}_eoi{out_row}_{out_col}] : memref<?xf64>\n"
        ));
        return Ok(());
    }

    let (_, k0) = nzs[0];
    let a0 = shape.lhs_matrix_reg(out_row, k0)?;
    let b0 = shape.rhs_matrix_reg(k0, out_col)?;
    out.push_str(&format!(
        "    %{pfx}_eacc{out_row}_{out_col}_0 = arith.mulf %{pfx}_r{a0}, %{pfx}_r{b0} : f64\n"
    ));
    for (nz_idx, (_, ki)) in nzs.iter().enumerate().skip(1) {
        let a_reg = shape.lhs_matrix_reg(out_row, *ki)?;
        let b_reg = shape.rhs_matrix_reg(*ki, out_col)?;
        let prev = nz_idx - 1;
        let curr = nz_idx;
        out.push_str(&format!(
            "    %{pfx}_eprod{out_row}_{out_col}_{curr} = arith.mulf %{pfx}_r{a_reg}, %{pfx}_r{b_reg} : f64\n\
             \t%{pfx}_eacc{out_row}_{out_col}_{curr} = arith.addf %{pfx}_eacc{out_row}_{out_col}_{prev}, %{pfx}_eprod{out_row}_{out_col}_{curr} : f64\n"
        ));
    }
    let last = nzs.len() - 1;
    out.push_str(&format!(
        "    %{pfx}_eoi{out_row}_{out_col} = arith.constant {output_idx} : index\n\
         \tmemref.store %{pfx}_eacc{out_row}_{out_col}_{last}, %out[%{pfx}_eoi{out_row}_{out_col}] : memref<?xf64>\n"
    ));
    Ok(())
}

#[derive(Clone, Copy)]
pub(in crate::codegen) struct LinSolveRenderShape {
    pub(in crate::codegen) matrix_start: usize,
    pub(in crate::codegen) rhs_start: usize,
    pub(in crate::codegen) n: usize,
    pub(in crate::codegen) output_offset: usize,
}

const LIN_SOLVE_RENDER_ENUMERATION_LIMIT: usize = 1_000_000;

impl LinSolveRenderShape {
    pub(in crate::codegen) fn matrix_count(self) -> Result<usize, minijinja::Error> {
        let count = checked_linsolve_product(self.n, self.n, "LinSolve matrix element count")?;
        checked_linsolve_render_count(count, "LinSolve matrix element count")
    }

    pub(in crate::codegen) fn rhs_count(self) -> Result<usize, minijinja::Error> {
        checked_linsolve_render_count(self.n, "LinSolve RHS element count")
    }

    pub(in crate::codegen) fn output_count(self) -> Result<usize, minijinja::Error> {
        checked_linsolve_render_count(self.n, "LinSolve output count")
    }

    pub(in crate::codegen) fn end_offset(self) -> Result<usize, minijinja::Error> {
        checked_linsolve_sum(
            self.output_offset,
            self.output_count()?,
            "LinSolve output end offset",
        )
    }

    pub(in crate::codegen) fn output_index(
        self,
        component: usize,
    ) -> Result<usize, minijinja::Error> {
        checked_linsolve_sum(self.output_offset, component, "LinSolve output index")
    }

    pub(in crate::codegen) fn matrix_reg(self, offset: usize) -> Result<usize, minijinja::Error> {
        checked_linsolve_sum(self.matrix_start, offset, "LinSolve matrix register index")
    }

    pub(in crate::codegen) fn rhs_reg(self, offset: usize) -> Result<usize, minijinja::Error> {
        checked_linsolve_sum(self.rhs_start, offset, "LinSolve RHS register index")
    }
}

pub(in crate::codegen) fn validate_linsolve_render_shape(
    shape: LinSolveRenderShape,
) -> Result<(usize, usize, usize), minijinja::Error> {
    let matrix_count = shape.matrix_count()?;
    let rhs_count = shape.rhs_count()?;
    let end_offset = shape.end_offset()?;
    Ok((matrix_count, rhs_count, end_offset))
}

pub(in crate::codegen) fn checked_linsolve_product(
    lhs: usize,
    rhs: usize,
    context: &'static str,
) -> Result<usize, minijinja::Error> {
    lhs.checked_mul(rhs)
        .ok_or_else(|| render_err(format!("{context} overflows host index range")))
}

pub(in crate::codegen) fn checked_linsolve_sum(
    lhs: usize,
    rhs: usize,
    context: &'static str,
) -> Result<usize, minijinja::Error> {
    lhs.checked_add(rhs)
        .ok_or_else(|| render_err(format!("{context} overflows host index range")))
}

pub(in crate::codegen) fn checked_linsolve_render_count(
    count: usize,
    context: &'static str,
) -> Result<usize, minijinja::Error> {
    if count > LIN_SOLVE_RENDER_ENUMERATION_LIMIT {
        return Err(render_err(format!(
            "{context} ({count}) exceeds render enumeration limit {LIN_SOLVE_RENDER_ENUMERATION_LIMIT}"
        )));
    }
    Ok(count)
}

/// Emit MLIR textual IR for a flat `Vec<LinearOp>` into `out`.
///
/// Uses `%{pfx}_r{dst}` as the SSA name for register `dst`.
/// `StoreOutput` is skipped — in MatMul context the register file holds
/// the matrix operand values; no output memref store is emitted here.
/// Render a `ComputeNode::LinSolve` inner value as MLIR textual IR.
///
/// Emits `setup_ops`, heap-allocates flat A (n×n), b (n), and x (n) memrefs, fills
/// them from computed registers, then calls `@rumoca_solve_linear` once for
/// the complete output vector.
///
/// Pointers are passed as `i64` to avoid the MLIR memref-descriptor ABI:
///   `memref.extract_aligned_pointer_as_index` → `arith.index_cast` → `i64`
///
/// Called from the MLIR template as `render_linsolve_mlir(node.LinSolve, node_id, output_offset)`.
pub(in crate::codegen) fn render_linsolve_mlir_function(
    node: Value,
    node_id: Value,
    output_offset: Value,
) -> Result<String, minijinja::Error> {
    let id = required_usize_arg(&node_id, "LinSolve node_id")?;
    let offset = required_usize_arg(&output_offset, "LinSolve output_offset")?;

    let setup_ops = get_field(&node, "setup_ops")?;
    let matrix_start = solve_field_usize(&node, "matrix_start")?;
    let rhs_start = solve_field_usize(&node, "rhs_start")?;
    let n = solve_field_usize(&node, "n")?;
    let shape = LinSolveRenderShape {
        matrix_start,
        rhs_start,
        n,
        output_offset: offset,
    };
    let (matrix_count, rhs_count, end_offset) = validate_linsolve_render_shape(shape)?;

    let pfx = format!("ls{id}");
    let mut out = format!("    // LinSolve {n}×{n} → out[{offset}..{end_offset}]\n");

    // Evaluate setup_ops → fills registers matrix_start..+n*n and rhs_start..+n
    emit_linear_ops_mlir(&setup_ops, &pfx, &mut out)?;

    // Keep model-sized dense solve workspaces off the thread stack. These
    // buffers are scoped to one evaluation and released after the result is
    // copied to the caller's output memref.
    out.push_str(&format!(
        "    %{pfx}_A = memref.alloc() : memref<{matrix_count}xf64>\n"
    ));
    for i in 0..matrix_count {
        let reg = shape.matrix_reg(i)?;
        out.push_str(&format!(
            "    %{pfx}_Ai{i} = arith.constant {i} : index\n\
             \tmemref.store %{pfx}_r{reg}, %{pfx}_A[%{pfx}_Ai{i}] : memref<{matrix_count}xf64>\n"
        ));
    }
    out.push_str(&format!(
        "    %{pfx}_b = memref.alloc() : memref<{rhs_count}xf64>\n"
    ));
    for i in 0..rhs_count {
        let reg = shape.rhs_reg(i)?;
        out.push_str(&format!(
            "    %{pfx}_bi{i} = arith.constant {i} : index\n\
             \tmemref.store %{pfx}_r{reg}, %{pfx}_b[%{pfx}_bi{i}] : memref<{rhs_count}xf64>\n"
        ));
    }

    // Extract aligned pointers as i64 (avoids memref-descriptor ABI complexity)
    // and solve the complete system once. The scalar compatibility helper
    // returns one component per call; using it here would refactorize A `n`
    // times and turn a native O(n^3) node into O(n^4).
    out.push_str(&format!(
        "    %{pfx}_Aidx = memref.extract_aligned_pointer_as_index %{pfx}_A : memref<{matrix_count}xf64> -> index\n\
         \t%{pfx}_Ai64 = arith.index_cast %{pfx}_Aidx : index to i64\n\
         \t%{pfx}_bidx = memref.extract_aligned_pointer_as_index %{pfx}_b : memref<{rhs_count}xf64> -> index\n\
         \t%{pfx}_bi64 = arith.index_cast %{pfx}_bidx : index to i64\n\
         \t%{pfx}_x = memref.alloc() : memref<{rhs_count}xf64>\n\
         \t%{pfx}_xidx = memref.extract_aligned_pointer_as_index %{pfx}_x : memref<{rhs_count}xf64> -> index\n\
         \t%{pfx}_xi64 = arith.index_cast %{pfx}_xidx : index to i64\n\
         \t%{pfx}_nn = arith.constant {n} : i64\n\
         \tfunc.call @rumoca_solve_linear(%{pfx}_Ai64, %{pfx}_bi64, %{pfx}_nn, %{pfx}_xi64) : (i64, i64, i64, i64) -> ()\n"
    ));

    // Load each component from the one solved vector and store it to output.
    for comp in 0..shape.output_count()? {
        let output_idx = shape.output_index(comp)?;
        out.push_str(&format!(
            "    %{pfx}_xi{comp} = arith.constant {comp} : index\n\
             \t%{pfx}_x{comp} = memref.load %{pfx}_x[%{pfx}_xi{comp}] : memref<{rhs_count}xf64>\n\
             \t%{pfx}_oi{comp} = arith.constant {output_idx} : index\n\
             \tmemref.store %{pfx}_x{comp}, %out[%{pfx}_oi{comp}] : memref<?xf64>\n"
        ));
    }
    out.push_str(&format!(
        "    memref.dealloc %{pfx}_x : memref<{rhs_count}xf64>\n\
         \tmemref.dealloc %{pfx}_b : memref<{rhs_count}xf64>\n\
         \tmemref.dealloc %{pfx}_A : memref<{matrix_count}xf64>\n"
    ));

    Ok(out)
}

fn required_usize_arg(value: &Value, context: &'static str) -> Result<usize, minijinja::Error> {
    value
        .as_usize()
        .ok_or_else(|| render_err(format!("{context} must be a non-negative integer")))
}

pub(in crate::codegen) fn required_usize_field(
    value: &Value,
    field: &'static str,
) -> Result<usize, minijinja::Error> {
    let field_value =
        get_field(value, field).map_err(|err| render_err(format!("missing `{field}`: {err}")))?;
    required_usize_arg(&field_value, field)
}

pub(in crate::codegen) fn required_bool_field(
    value: &Value,
    field: &'static str,
) -> Result<bool, minijinja::Error> {
    let field_value =
        get_field(value, field).map_err(|err| render_err(format!("missing `{field}`: {err}")))?;
    bool::try_from(field_value).map_err(|_| render_err(format!("`{field}` must be a boolean")))
}

pub(in crate::codegen) fn required_string_field(
    value: &Value,
    field: &'static str,
) -> Result<String, minijinja::Error> {
    let field_value =
        get_field(value, field).map_err(|err| render_err(format!("missing `{field}`: {err}")))?;
    field_value
        .as_str()
        .map(str::to_string)
        .ok_or_else(|| render_err(format!("`{field}` must be a string")))
}

fn emit_linear_ops_mlir(ops: &Value, pfx: &str, out: &mut String) -> Result<(), minijinja::Error> {
    for op in ops
        .try_iter()
        .map_err(|_| render_err("MatMul lhs_ops/rhs_ops must be iterable"))?
    {
        emit_one_linear_op_mlir(&op, pfx, out)?;
    }
    Ok(())
}

fn emit_one_linear_op_mlir(
    op: &Value,
    pfx: &str,
    out: &mut String,
) -> Result<(), minijinja::Error> {
    if let Ok(v) = get_field(op, "Const") {
        let dst = solve_field_usize(&v, "dst")?;
        let val = solve_const_value_string(&v, "INFINITY")?;
        out.push_str(&format!("    %{pfx}_r{dst} = arith.constant {val} : f64\n"));
    } else if let Ok(v) = get_field(op, "LoadY") {
        let dst = solve_field_usize(&v, "dst")?;
        let idx = solve_field_usize(&v, "index")?;
        out.push_str(&format!(
            "    %{pfx}_iy{dst} = arith.constant {idx} : index\n\
             \t%{pfx}_r{dst} = memref.load %y[%{pfx}_iy{dst}] : memref<?xf64>\n"
        ));
    } else if let Ok(v) = get_field(op, "LoadP") {
        let dst = solve_field_usize(&v, "dst")?;
        let idx = solve_field_usize(&v, "index")?;
        out.push_str(&format!(
            "    %{pfx}_ip{dst} = arith.constant {idx} : index\n\
             \t%{pfx}_r{dst} = memref.load %p[%{pfx}_ip{dst}] : memref<?xf64>\n"
        ));
    } else if let Ok(v) = get_field(op, "LoadIndexedP") {
        let dst = solve_field_usize(&v, "dst")?;
        let base = solve_field_usize(&v, "base")?;
        let count = solve_field_usize(&v, "count")?;
        let index = solve_field_usize(&v, "index")?;
        let last = if count == 0 { 0 } else { count - 1 };
        // round + clamp the runtime index in f64, convert to an index, add base,
        // then load — matching `resolve_indexed_slot`.
        out.push_str(&format!(
            "    %{pfx}_rnd{dst} = math.round %{pfx}_r{index} : f64\n\
             \t%{pfx}_zr{dst} = arith.constant 0.0 : f64\n\
             \t%{pfx}_lo{dst} = arith.maxnumf %{pfx}_rnd{dst}, %{pfx}_zr{dst} : f64\n\
             \t%{pfx}_hi{dst} = arith.constant {last}.0 : f64\n\
             \t%{pfx}_cl{dst} = arith.minnumf %{pfx}_lo{dst}, %{pfx}_hi{dst} : f64\n\
             \t%{pfx}_si{dst} = arith.fptosi %{pfx}_cl{dst} : f64 to i64\n\
             \t%{pfx}_ic{dst} = arith.index_cast %{pfx}_si{dst} : i64 to index\n\
             \t%{pfx}_bs{dst} = arith.constant {base} : index\n\
             \t%{pfx}_ix{dst} = arith.addi %{pfx}_ic{dst}, %{pfx}_bs{dst} : index\n\
             \t%{pfx}_r{dst} = memref.load %p[%{pfx}_ix{dst}] : memref<?xf64>\n"
        ));
    } else if let Ok(v) = get_field(op, "Move") {
        let dst = solve_field_usize(&v, "dst")?;
        let src = solve_field_usize(&v, "src")?;
        out.push_str(&format!(
            "    %{pfx}_mo{dst} = arith.constant 1.0 : f64\n\
             \t%{pfx}_r{dst} = arith.mulf %{pfx}_r{src}, %{pfx}_mo{dst} : f64\n"
        ));
    } else if let Ok(v) = get_field(op, "Unary") {
        let dst = solve_field_usize(&v, "dst")?;
        let arg = solve_field_usize(&v, "arg")?;
        let op_name = solve_variant_name(&get_field(&v, "op")?)?;
        let mop = unary_to_mlir_op(&op_name)
            .ok_or_else(|| render_err(format!("unsupported unary in MatMul ops: {op_name}")))?;
        out.push_str(&format!("    %{pfx}_r{dst} = {mop} %{pfx}_r{arg} : f64\n"));
    } else if let Ok(v) = get_field(op, "Binary") {
        let dst = solve_field_usize(&v, "dst")?;
        let lhs = solve_field_usize(&v, "lhs")?;
        let rhs = solve_field_usize(&v, "rhs")?;
        let op_name = solve_variant_name(&get_field(&v, "op")?)?;
        let mop = binary_to_mlir_op(&op_name)
            .ok_or_else(|| render_err(format!("unsupported binary in MatMul ops: {op_name}")))?;
        out.push_str(&format!(
            "    %{pfx}_r{dst} = {mop} %{pfx}_r{lhs}, %{pfx}_r{rhs} : f64\n"
        ));
    } else if get_field(op, "StoreOutput").is_ok() {
        // In MatMul context the register file holds the matrix values — no output store here.
    } else {
        return Err(render_err(format!(
            "unsupported LinearOp in MatMul lhs/rhs: {op}"
        )));
    }
    Ok(())
}

fn unary_to_mlir_op(op: &str) -> Option<&'static str> {
    match op {
        "Neg" => Some("arith.negf"),
        "Abs" => Some("math.absf"),
        "Sqrt" => Some("math.sqrt"),
        "Sin" => Some("math.sin"),
        "Cos" => Some("math.cos"),
        "Tan" => Some("math.tan"),
        "Exp" => Some("math.exp"),
        "Log" => Some("math.log"),
        "Floor" => Some("math.floor"),
        "Ceil" => Some("math.ceil"),
        "Trunc" => Some("math.trunc"),
        _ => None,
    }
}

fn binary_to_mlir_op(op: &str) -> Option<&'static str> {
    match op {
        "Add" => Some("arith.addf"),
        "Sub" => Some("arith.subf"),
        "Mul" => Some("arith.mulf"),
        "Div" => Some("arith.divf"),
        "Pow" => Some("math.powf"),
        "Min" => Some("arith.minnumf"),
        "Max" => Some("arith.maxnumf"),
        _ => None,
    }
}

/// Whether MLIR's native dense-node emitter can lower every setup operation.
///
/// Unsupported operations must retain the shared scalarized program. Marking
/// only part of a tensor setup stream as native would filter out that fallback
/// and silently omit the node's outputs from generated MLIR.
pub(in crate::codegen) fn mlir_native_dense_node_supported(node: &solve::ComputeNode) -> bool {
    match node {
        solve::ComputeNode::MatMul {
            lhs_ops, rhs_ops, ..
        } => lhs_ops
            .iter()
            .chain(rhs_ops)
            .all(mlir_native_linear_op_supported),
        solve::ComputeNode::LinSolve { setup_ops, .. } => {
            setup_ops.iter().all(mlir_native_linear_op_supported)
        }
        solve::ComputeNode::ScalarPrograms(_)
        | solve::ComputeNode::Map { .. }
        | solve::ComputeNode::AffineStencil { .. } => false,
    }
}

fn mlir_native_linear_op_supported(op: &solve::LinearOp) -> bool {
    match op {
        solve::LinearOp::Const { .. }
        | solve::LinearOp::LoadY { .. }
        | solve::LinearOp::LoadP { .. }
        | solve::LinearOp::LoadIndexedP { .. }
        | solve::LinearOp::Move { .. }
        | solve::LinearOp::StoreOutput { .. } => true,
        solve::LinearOp::Unary { op, .. } => matches!(
            op,
            solve::UnaryOp::Neg
                | solve::UnaryOp::Abs
                | solve::UnaryOp::Sqrt
                | solve::UnaryOp::Floor
                | solve::UnaryOp::Ceil
                | solve::UnaryOp::Trunc
                | solve::UnaryOp::Sin
                | solve::UnaryOp::Cos
                | solve::UnaryOp::Tan
                | solve::UnaryOp::Exp
                | solve::UnaryOp::Log
        ),
        solve::LinearOp::Binary { op, .. } => matches!(
            op,
            solve::BinaryOp::Add
                | solve::BinaryOp::Sub
                | solve::BinaryOp::Mul
                | solve::BinaryOp::Div
                | solve::BinaryOp::Pow
                | solve::BinaryOp::Min
                | solve::BinaryOp::Max
        ),
        solve::LinearOp::LoadTime { .. }
        | solve::LinearOp::LoadSeed { .. }
        | solve::LinearOp::LoadIndexedSeed { .. }
        | solve::LinearOp::LinearSolveComponent { .. }
        | solve::LinearOp::TableBounds { .. }
        | solve::LinearOp::TableLookup { .. }
        | solve::LinearOp::TableLookupSlope { .. }
        | solve::LinearOp::TableNextEvent { .. }
        | solve::LinearOp::RandomInitialState { .. }
        | solve::LinearOp::RandomResult { .. }
        | solve::LinearOp::RandomState { .. }
        | solve::LinearOp::ImpureRandomInit { .. }
        | solve::LinearOp::ImpureRandom { .. }
        | solve::LinearOp::ImpureRandomInteger { .. }
        | solve::LinearOp::Compare { .. }
        | solve::LinearOp::Select { .. } => false,
    }
}

fn extract_pattern_nonzeros(node: &Value) -> Result<Vec<(usize, usize)>, minijinja::Error> {
    let nonzeros = get_field(node, "lhs_pattern_nonzeros")
        .map_err(|err| render_err(format!("MatMul missing lhs_pattern_nonzeros: {err}")))?;
    let count = nonzeros
        .len()
        .ok_or_else(|| render_err("MatMul lhs_pattern_nonzeros must be an array"))?;
    let mut entries = render_vec_with_capacity(count, "MatMul structural nonzero count")?;
    for position in 0..count {
        let pair = nonzeros
            .get_item(&Value::from(position))
            .map_err(|err| render_err(format!("MatMul nonzero[{position}]: {err}")))?;
        let row = pair
            .get_item(&Value::from(0))
            .map_err(|err| render_err(format!("MatMul nonzero[{position}] row: {err}")))?
            .as_usize()
            .ok_or_else(|| render_err("MatMul nonzero row must be an integer"))?;
        let column = pair
            .get_item(&Value::from(1))
            .map_err(|err| render_err(format!("MatMul nonzero[{position}] column: {err}")))?
            .as_usize()
            .ok_or_else(|| render_err("MatMul nonzero column must be an integer"))?;
        entries.push((row, column));
    }
    Ok(entries)
}

fn matmul_nnz_for_row(
    nnz: &[(usize, usize)],
    row: usize,
) -> Result<Vec<(usize, usize)>, minijinja::Error> {
    let mut row_nzs = render_vec_with_capacity(nnz.len(), "MatMul row nonzero count")?;
    for pair in nnz.iter().filter(|(candidate, _)| *candidate == row) {
        row_nzs.push(*pair);
    }
    Ok(row_nzs)
}

pub(in crate::codegen) fn render_solve_row_wgsl_function(
    row: Value,
    config: Value,
) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_row_for(&row, &cfg, SolveRowDialect::Wgsl)
}

pub(in crate::codegen) fn render_solve_row_output_wgsl_function(
    row: Value,
    output_ordinal: Value,
    config: Value,
) -> RenderResult {
    let output_ordinal = output_ordinal
        .as_usize()
        .ok_or_else(|| render_err("solve row output ordinal must be a non-negative integer"))?;
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_row_output_for(&row, output_ordinal, &cfg, SolveRowDialect::Wgsl)
}
