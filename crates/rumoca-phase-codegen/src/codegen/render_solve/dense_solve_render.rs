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
/// - `Dense` (default): `linalg.matmul` with alloca'd temporaries
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

    let lhs_sparsity_val = get_field(&node, "lhs_sparsity")
        .map_err(|err| render_err(format!("MatMul missing lhs_sparsity: {err}")))?;
    let lhs_sparsity_str = value_to_string(&lhs_sparsity_val);
    let is_diagonal_matvec = lhs_sparsity_str.contains("Diagonal") && n == 1 && m == k;
    let explicit_nnz = if !is_diagonal_matvec && lhs_sparsity_str.contains("Explicit") {
        Some(extract_explicit_nnz(&lhs_sparsity_val)?)
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
        "    %{pfx}_A = memref.alloca() : memref<{m}x{k}xf64>\n"
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
        "    %{pfx}_B = memref.alloca() : memref<{k}x{n}xf64>\n"
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
         \t%{pfx}_C = memref.alloca() : memref<{m}x{n}xf64>\n\
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
/// Emits `setup_ops`, allocas flat A (n×n) and b (n) memrefs, fills them
/// from computed registers, then calls `@rumoca_solve_linear_component` (the
/// Gaussian-elimination runtime) once per output component.
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

    // Alloca flat A (n×n) and b (n) buffers on the stack
    out.push_str(&format!(
        "    %{pfx}_A = memref.alloca() : memref<{matrix_count}xf64>\n"
    ));
    for i in 0..matrix_count {
        let reg = shape.matrix_reg(i)?;
        out.push_str(&format!(
            "    %{pfx}_Ai{i} = arith.constant {i} : index\n\
             \tmemref.store %{pfx}_r{reg}, %{pfx}_A[%{pfx}_Ai{i}] : memref<{matrix_count}xf64>\n"
        ));
    }
    out.push_str(&format!(
        "    %{pfx}_b = memref.alloca() : memref<{rhs_count}xf64>\n"
    ));
    for i in 0..rhs_count {
        let reg = shape.rhs_reg(i)?;
        out.push_str(&format!(
            "    %{pfx}_bi{i} = arith.constant {i} : index\n\
             \tmemref.store %{pfx}_r{reg}, %{pfx}_b[%{pfx}_bi{i}] : memref<{rhs_count}xf64>\n"
        ));
    }

    // Extract aligned pointers as i64 (avoids memref-descriptor ABI complexity)
    out.push_str(&format!(
        "    %{pfx}_Aidx = memref.extract_aligned_pointer_as_index %{pfx}_A : memref<{matrix_count}xf64> -> index\n\
         \t%{pfx}_Ai64 = arith.index_cast %{pfx}_Aidx : index to i64\n\
         \t%{pfx}_bidx = memref.extract_aligned_pointer_as_index %{pfx}_b : memref<{rhs_count}xf64> -> index\n\
         \t%{pfx}_bi64 = arith.index_cast %{pfx}_bidx : index to i64\n\
         \t%{pfx}_nn = arith.constant {n} : i64\n"
    ));

    // Call runtime once per component, store each result to output
    for comp in 0..shape.output_count()? {
        let output_idx = shape.output_index(comp)?;
        out.push_str(&format!(
            "    %{pfx}_comp{comp} = arith.constant {comp} : i64\n\
             \t%{pfx}_x{comp} = func.call @rumoca_solve_linear_component(%{pfx}_Ai64, %{pfx}_bi64, %{pfx}_nn, %{pfx}_comp{comp}) : (i64, i64, i64, i64) -> f64\n\
             \t%{pfx}_oi{comp} = arith.constant {output_idx} : index\n\
             \tmemref.store %{pfx}_x{comp}, %out[%{pfx}_oi{comp}] : memref<?xf64>\n"
        ));
    }

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
             \t%{pfx}_lo{dst} = arith.maximumf %{pfx}_rnd{dst}, %{pfx}_zr{dst} : f64\n\
             \t%{pfx}_hi{dst} = arith.constant {last}.0 : f64\n\
             \t%{pfx}_cl{dst} = arith.minimumf %{pfx}_lo{dst}, %{pfx}_hi{dst} : f64\n\
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
            "    %{pfx}_mz{dst} = arith.constant 0.0 : f64\n\
             \t%{pfx}_r{dst} = arith.addf %{pfx}_r{src}, %{pfx}_mz{dst} : f64\n"
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
        "Min" => Some("arith.minimumf"),
        "Max" => Some("arith.maximumf"),
        _ => None,
    }
}

/// Extract `(row, col)` nonzero pairs from a serialized `SparsityPattern::Explicit`.
///
/// Expects the minijinja `Value` to represent `{"Explicit": {"nnz": [[r,c], ...]}}`.
fn extract_explicit_nnz(sparsity: &Value) -> Result<Vec<(usize, usize)>, minijinja::Error> {
    let explicit = get_field(sparsity, "Explicit")
        .map_err(|err| render_err(format!("MatMul Explicit sparsity missing variant: {err}")))?;
    let nnz_val = get_field(&explicit, "nnz")
        .map_err(|err| render_err(format!("MatMul Explicit sparsity missing nnz: {err}")))?;
    let len = nnz_val
        .len()
        .ok_or_else(|| render_err("MatMul Explicit sparsity nnz must be an array"))?;
    let mut nnz = render_vec_with_capacity(len, "MatMul Explicit sparsity nnz count")?;
    for i in 0..len {
        let pair = nnz_val
            .get_item(&minijinja::Value::from(i))
            .map_err(|err| render_err(format!("MatMul Explicit sparsity nnz[{i}]: {err}")))?;
        let row = pair
            .get_item(&minijinja::Value::from(0))
            .map_err(|err| render_err(format!("MatMul Explicit sparsity nnz[{i}] row: {err}")))?
            .as_usize()
            .ok_or_else(|| {
                render_err(format!(
                    "MatMul Explicit sparsity nnz[{i}] row must be a non-negative integer"
                ))
            })?;
        let col = pair
            .get_item(&minijinja::Value::from(1))
            .map_err(|err| render_err(format!("MatMul Explicit sparsity nnz[{i}] col: {err}")))?
            .as_usize()
            .ok_or_else(|| {
                render_err(format!(
                    "MatMul Explicit sparsity nnz[{i}] col must be a non-negative integer"
                ))
            })?;
        nnz.push((row, col));
    }
    Ok(nnz)
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

pub(in crate::codegen) fn render_solve_row_c_function(row: Value, config: Value) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_row_c(&row, &cfg)
}

pub(in crate::codegen) fn render_solve_target_assignment_c_function(
    row: Value,
    target_y_index: Value,
    config: Value,
) -> RenderResult {
    let target_y_index = target_y_index
        .as_usize()
        .ok_or_else(|| render_err("target-assignment Y index must be a non-negative integer"))?;
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_target_assignment_c(&row, target_y_index, &cfg)
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

pub(in crate::codegen) fn render_solve_row_rust_function(
    row: Value,
    config: Value,
) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_row_for(&row, &cfg, SolveRowDialect::Rust)
}

/// Render a whole list of scalar programs as a C statement block, handling
/// multi-output programs and computing shared registers once. `out_set` is the
/// assignment pattern (two `{}`: index, value), e.g. `"out[{}] = {}"`.
pub(in crate::codegen) fn render_solve_block_c_function(
    programs: Value,
    config: Value,
    out_set: Value,
    output_target: Option<Value>,
) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_block_for(
        &programs,
        &cfg,
        SolveRowDialect::C,
        &value_to_string(&out_set),
        solve_output_targets(output_target)?,
    )
}

/// Rust counterpart of [`render_solve_block_c_function`].
pub(in crate::codegen) fn render_solve_block_rust_function(
    programs: Value,
    config: Value,
    out_set: Value,
    output_target: Option<Value>,
) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_block_for(
        &programs,
        &cfg,
        SolveRowDialect::Rust,
        &value_to_string(&out_set),
        solve_output_targets(output_target)?,
    )
}

/// Python (CasADi/JAX) counterpart of [`render_solve_block_c_function`].
///
/// Emits one-tab-indented Python statements (`__rN = …`, `out[i] = …;`) that
/// build a symbolic expression graph from the solve bytecode. Function names are
/// bare (`sin`, `fabs`, `if_else`, …) and must be bound by the consuming
/// template to the chosen array namespace. The `cfg` patterns are typically
/// `{"y": "x[{}]", "p": "P[{}]", "time": "0.0"}` and `out_set` `"out[{}] = {}"`.
pub(in crate::codegen) fn render_solve_block_py_function(
    programs: Value,
    config: Value,
    out_set: Value,
    output_target: Option<Value>,
) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_block_for(
        &programs,
        &cfg,
        SolveRowDialect::Python,
        &value_to_string(&out_set),
        solve_output_targets(output_target)?,
    )
}

pub(in crate::codegen) enum SolveOutputTargets {
    DenseOffset(usize),
    Explicit(Vec<usize>),
}

impl SolveOutputTargets {
    pub(in crate::codegen) fn target_for(&self, ordinal: usize) -> Result<usize, minijinja::Error> {
        match self {
            Self::DenseOffset(offset) => offset
                .checked_add(ordinal)
                .ok_or_else(|| render_err("solve output index overflow")),
            Self::Explicit(indices) => indices.get(ordinal).copied().ok_or_else(|| {
                render_err(format!(
                    "solve output_indices missing entry for StoreOutput #{ordinal}"
                ))
            }),
        }
    }
}

pub(in crate::codegen) fn solve_output_targets(
    value: Option<Value>,
) -> Result<SolveOutputTargets, minijinja::Error> {
    let Some(value) = value else {
        return Ok(SolveOutputTargets::DenseOffset(0));
    };
    if let Some(offset) = value.as_usize() {
        return Ok(SolveOutputTargets::DenseOffset(offset));
    }
    let mut indices = Vec::new();
    for value in value
        .try_iter()
        .map_err(|_| render_err("solve output target must be an offset or output_indices array"))?
    {
        let Some(index) = value.as_usize() else {
            return Err(render_err(
                "solve output_indices entries must be non-negative integers",
            ));
        };
        reserve_render_capacity(&mut indices, 1, "solve output_indices count")?;
        indices.push(index);
    }
    Ok(SolveOutputTargets::Explicit(indices))
}

/// Total number of outputs (`StoreOutput` ops) across a list of scalar programs.
/// Used by templates to size output buffers and advance running output offsets
/// when a program may emit more than one output.
pub(in crate::codegen) fn solve_block_output_count_function(
    programs: Value,
) -> Result<usize, minijinja::Error> {
    let mut count = 0usize;
    for program in programs
        .try_iter()
        .map_err(|_| render_err("solve programs must be an array"))?
    {
        for op in program
            .try_iter()
            .map_err(|_| render_err("solve program must be an array of LinearOp values"))?
        {
            if get_field(&op, "StoreOutput").is_ok() {
                count = count
                    .checked_add(1)
                    .ok_or_else(|| render_err("solve StoreOutput count overflows host range"))?;
            }
        }
    }
    Ok(count)
}

pub(in crate::codegen) fn render_solve_slot_assign_c_function(
    slot: Value,
    value: Value,
    config: Value,
) -> RenderResult {
    render_solve_slot_assign_c_required(&slot, &value, &config)
}

pub(in crate::codegen) fn render_solve_pre_param_binding_c_function(
    binding: Value,
    access_config: Value,
    assign_config: Value,
) -> RenderResult {
    let access_cfg = SolveRowCConfig::from_value(&access_config);
    let assign_cfg = SolveSlotAssignCConfig::from_value(&assign_config);
    let dest = solve_field_usize(&binding, "dest_p_index")?;
    let source = get_field(&binding, "source")?;
    let value = if let Ok(slot) = get_field(&source, "Y") {
        access_cfg.y_access(solve_field_usize(&slot, "index")?)
    } else if let Ok(slot) = get_field(&source, "P") {
        access_cfg.p_access(solve_field_usize(&slot, "index")?)
    } else {
        return Err(render_err(format!(
            "unsupported pre-parameter source slot: {source}"
        )));
    };

    Ok(format!(
        "{};",
        format_solve_set(&assign_cfg.p_set_pattern, dest, &value)
    ))
}

pub(in crate::codegen) fn render_optional_solve_slot_assign_c_function(
    slot: Value,
    value: Value,
    config: Value,
) -> RenderResult {
    if slot.is_none() || slot.is_undefined() {
        return Ok(String::new());
    }
    render_solve_slot_assign_c_required(&slot, &value, &config)
}

fn render_solve_slot_assign_c_required(
    slot: &Value,
    value: &Value,
    config: &Value,
) -> RenderResult {
    let cfg = SolveSlotAssignCConfig::from_value(config);
    let value = value_to_string(value);
    if let Ok(slot) = get_field(slot, "Y") {
        let index = solve_field_usize(&slot, "index")?;
        return Ok(format!(
            "{};",
            format_solve_set(&cfg.y_set_pattern, index, &value)
        ));
    }
    if let Ok(slot) = get_field(slot, "P") {
        let index = solve_field_usize(&slot, "index")?;
        return Ok(format!(
            "{};",
            format_solve_set(&cfg.p_set_pattern, index, &value)
        ));
    }
    Err(render_err(format!(
        "unsupported solve assignment target slot: {slot}"
    )))
}

/// Render a `ComputeNode::MatMul` inner value as a C block.
///
/// Three dispatch paths:
/// - `Diagonal` lhs (n=1, m=k): element-wise scalar multiplies (no GEMM call)
/// - `Explicit { nnz }` lhs: scalar accumulate over each nonzero position
/// - `Dense` (default): `__rumoca_dgemm` call
///
/// `node` is the inner struct of the `MatMul` variant.  `output_offset` is the first
/// output array index this node writes to.
pub(in crate::codegen) fn render_matmul_c_function(
    node: Value,
    output_offset: Value,
    config: Value,
) -> Result<String, minijinja::Error> {
    let cfg = SolveRowCConfig::from_value(&config);
    let offset: usize = output_offset
        .as_usize()
        .ok_or_else(|| render_err("output_offset must be a non-negative integer"))?;

    let lhs_ops = get_field(&node, "lhs_ops")?;
    let lhs_start = solve_field_usize(&node, "lhs_start")?;
    let rhs_ops = get_field(&node, "rhs_ops")?;
    let rhs_start = solve_field_usize(&node, "rhs_start")?;
    let m = solve_field_usize(&node, "m")?;
    let k = solve_field_usize(&node, "k")?;
    let n = solve_field_usize(&node, "n")?;

    let lhs_sparsity_val = get_field(&node, "lhs_sparsity")
        .map_err(|err| render_err(format!("MatMul missing lhs_sparsity: {err}")))?;
    let lhs_sparsity_str = value_to_string(&lhs_sparsity_val);
    let is_diagonal_matvec = lhs_sparsity_str.contains("Diagonal") && n == 1 && m == k;
    let explicit_nnz = if !is_diagonal_matvec && lhs_sparsity_str.contains("Explicit") {
        Some(extract_explicit_nnz(&lhs_sparsity_val)?)
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

    // Evaluate lhs_ops into a register file.
    let mut regs = Vec::<String>::new();
    for op in lhs_ops
        .try_iter()
        .map_err(|_| render_err("MatMul lhs_ops must be iterable"))?
    {
        render_solve_op_c(&op, &cfg, &mut regs, None)?;
    }

    // Evaluate rhs_ops into the same register file.
    for op in rhs_ops
        .try_iter()
        .map_err(|_| render_err("MatMul rhs_ops must be iterable"))?
    {
        render_solve_op_c(&op, &cfg, &mut regs, None)?;
    }

    if is_diagonal_matvec {
        // A (m×m diagonal) * x (m×1): output[i] = A[i,i] * x[i].
        // Only diagonal elements of lhs are needed: positions i*k+i = i*(m+1).
        let mut lines = format!("    /* DiagonalMul {m}x{m}: output[{offset}..{end_offset}] */\n");
        for i in 0..m {
            let diag_reg = solve_reg(&regs, shape.diagonal_lhs_reg(i)?)?;
            let rhs_reg = solve_reg(&regs, shape.rhs_vector_reg(i)?)?;
            lines.push_str(&format!(
                "\tm->xdot[{out}] = ({diag_reg}) * ({rhs_reg});\n",
                out = shape.output_index(i)?,
            ));
        }
        Ok(lines.trim_end().to_string())
    } else if let Some(nnz) = explicit_nnz {
        let lines = render_explicit_sparse_matmul_c(&regs, &nnz, shape, end_offset)?;
        Ok(lines.trim_end().to_string())
    } else {
        let lhs_count = shape.dense_lhs_count()?;
        let rhs_count = shape.dense_rhs_count()?;
        let lhs_array =
            render_matmul_register_array(&regs, lhs_start, lhs_count, "MatMul lhs operand")?
                .join(", ");
        let rhs_array =
            render_matmul_register_array(&regs, rhs_start, rhs_count, "MatMul rhs operand")?
                .join(", ");
        Ok(format!(
            "    /* MatMul {m}x{k}x{n}: output[{offset}..{end_offset}] */\n\
             \t{{\n\
             \t\tdouble __lhs[{lhs_count}] = {{{lhs_array}}};\n\
             \t\tdouble __rhs[{rhs_count}] = {{{rhs_array}}};\n\
             \t\t__rumoca_dgemm({m}, {n}, {k}, __lhs, __rhs, &m->xdot[{offset}]);\n\
             \t}}"
        ))
    }
}

fn render_explicit_sparse_matmul_c(
    regs: &[String],
    nnz: &[(usize, usize)],
    shape: MatMulRenderShape,
    end_offset: usize,
) -> Result<String, minijinja::Error> {
    let mut lines = format!(
        "    /* SparseMul {}x{}x{} ({} nnz): output[{}..{}] */\n",
        shape.m,
        shape.k,
        shape.n,
        nnz.len(),
        shape.offset,
        end_offset
    );
    for slot in 0..shape.output_count()? {
        let out_row = slot / shape.n;
        let out_col = slot % shape.n;
        let output_idx = shape.output_index(slot)?;
        let row_nzs = matmul_nnz_for_row(nnz, out_row)?;
        render_sparse_matmul_cell_c(
            &mut lines, regs, shape, out_row, out_col, output_idx, &row_nzs,
        )?;
    }
    Ok(lines)
}

fn render_sparse_matmul_cell_c(
    lines: &mut String,
    regs: &[String],
    shape: MatMulRenderShape,
    out_row: usize,
    out_col: usize,
    output_idx: usize,
    nzs: &[(usize, usize)],
) -> Result<(), minijinja::Error> {
    if nzs.is_empty() {
        lines.push_str(&format!("\tm->xdot[{output_idx}] = 0.0;\n"));
        return Ok(());
    }

    let mut terms = render_vec_with_capacity(nzs.len(), "MatMul sparse term count")?;
    for (_, ki) in nzs {
        let a = solve_reg(regs, shape.lhs_matrix_reg(out_row, *ki)?)?;
        let b = solve_reg(regs, shape.rhs_matrix_reg(*ki, out_col)?)?;
        terms.push(format!("({a}) * ({b})"));
    }
    lines.push_str(&format!(
        "\tm->xdot[{output_idx}] = {};\n",
        terms.join(" + ")
    ));
    Ok(())
}

fn render_matmul_register_array(
    regs: &[String],
    start: usize,
    count: usize,
    context: &'static str,
) -> Result<Vec<String>, minijinja::Error> {
    let mut values = render_vec_with_capacity(count, context)?;
    for offset in 0..count {
        let reg = checked_matmul_sum(start, offset, "MatMul dense register index")?;
        values.push(solve_reg(regs, reg)?);
    }
    Ok(values)
}
