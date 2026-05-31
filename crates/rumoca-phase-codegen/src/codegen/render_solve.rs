//! C and MLIR rendering for solver-facing row IR.

use crate::errors::render_err;
use minijinja::Value;

use super::render_expr::get_field;
use super::{RenderResult, value_to_string};

#[derive(Clone, Copy)]
struct MatMulRenderShape {
    lhs_start: usize,
    rhs_start: usize,
    m: usize,
    k: usize,
    n: usize,
    offset: usize,
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
pub(super) fn render_matmul_mlir_function(
    node: Value,
    node_id: Value,
    output_offset: Value,
) -> Result<String, minijinja::Error> {
    let id = node_id.as_usize().unwrap_or(0);
    let offset = output_offset.as_usize().unwrap_or(0);

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
        extract_explicit_nnz(&lhs_sparsity_val)
    } else {
        None
    };

    let pfx = format!("mm{id}");
    let mut out = format!(
        "    // MatMul {}x{}x{} → out[{}..{}]\n",
        m,
        k,
        n,
        offset,
        offset + m * n
    );

    // Emit scalar ops that build the A and B register values.
    emit_linear_ops_mlir(&lhs_ops, &pfx, &mut out)?;
    emit_linear_ops_mlir(&rhs_ops, &pfx, &mut out)?;

    if is_diagonal_matvec {
        // Diagonal A (m×m) * x (m×1): out[offset+i] = A[i,i] * x[i]
        for i in 0..m {
            let a_reg = lhs_start + i * (m + 1); // diagonal element index
            let b_reg = rhs_start + i;
            out.push_str(&format!(
                "    %{pfx}_diag{i} = arith.mulf %{pfx}_r{a_reg}, %{pfx}_r{b_reg} : f64\n"
            ));
            out.push_str(&format!(
                "    %{pfx}_douti{i} = arith.constant {} : index\n",
                offset + i
            ));
            out.push_str(&format!(
                "    memref.store %{pfx}_diag{i}, %out[%{pfx}_douti{i}] : memref<?xf64>\n"
            ));
        }
        return Ok(out);
    }

    if let Some(nnz) = explicit_nnz {
        render_explicit_sparse_matmul_mlir(
            &mut out,
            &pfx,
            &nnz,
            MatMulRenderShape {
                lhs_start,
                rhs_start,
                m,
                k,
                n,
                offset,
            },
        );
        return Ok(out);
    }

    render_dense_matmul_mlir(
        &mut out,
        &pfx,
        MatMulRenderShape {
            lhs_start,
            rhs_start,
            m,
            k,
            n,
            offset,
        },
    );
    Ok(out)
}

fn render_dense_matmul_mlir(out: &mut String, pfx: &str, shape: MatMulRenderShape) {
    let MatMulRenderShape {
        lhs_start,
        rhs_start,
        m,
        k,
        n,
        offset,
    } = shape;
    out.push_str(&format!(
        "    %{pfx}_A = memref.alloca() : memref<{m}x{k}xf64>\n"
    ));
    for i in 0..m {
        for j in 0..k {
            let reg = lhs_start + i * k + j;
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
            let reg = rhs_start + i * n + j;
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
            let output_idx = offset + i * n + j;
            out.push_str(&format!(
                "    %{pfx}_Ci{i}_{j} = arith.constant {i} : index\n\
                 \t%{pfx}_Cj{i}_{j} = arith.constant {j} : index\n\
                 \t%{pfx}_Cv{i}_{j} = memref.load %{pfx}_C[%{pfx}_Ci{i}_{j}, %{pfx}_Cj{i}_{j}] : memref<{m}x{n}xf64>\n\
                 \t%{pfx}_oi{i}_{j} = arith.constant {output_idx} : index\n\
                 \tmemref.store %{pfx}_Cv{i}_{j}, %out[%{pfx}_oi{i}_{j}] : memref<?xf64>\n"
            ));
        }
    }
}

fn render_explicit_sparse_matmul_mlir(
    out: &mut String,
    pfx: &str,
    nnz: &[(usize, usize)],
    shape: MatMulRenderShape,
) {
    out.push_str(&format!("    // Explicit sparse: {} nnz\n", nnz.len()));
    for slot in 0..shape.m * shape.n {
        let out_row = slot / shape.n;
        let out_col = slot % shape.n;
        let output_idx = shape.offset + slot;
        let row_nzs: Vec<(usize, usize)> =
            nnz.iter().filter(|(r, _)| *r == out_row).copied().collect();
        render_sparse_matmul_cell_mlir(out, pfx, shape, out_row, out_col, output_idx, &row_nzs);
    }
}

fn render_sparse_matmul_cell_mlir(
    out: &mut String,
    pfx: &str,
    shape: MatMulRenderShape,
    out_row: usize,
    out_col: usize,
    output_idx: usize,
    nzs: &[(usize, usize)],
) {
    if nzs.is_empty() {
        out.push_str(&format!(
            "    %{pfx}_ez{out_row}_{out_col} = arith.constant 0.0 : f64\n\
             \t%{pfx}_eoi{out_row}_{out_col} = arith.constant {output_idx} : index\n\
             \tmemref.store %{pfx}_ez{out_row}_{out_col}, %out[%{pfx}_eoi{out_row}_{out_col}] : memref<?xf64>\n"
        ));
        return;
    }

    let (_, k0) = nzs[0];
    let a0 = shape.lhs_start + out_row * shape.k + k0;
    let b0 = shape.rhs_start + k0 * shape.n + out_col;
    out.push_str(&format!(
        "    %{pfx}_eacc{out_row}_{out_col}_0 = arith.mulf %{pfx}_r{a0}, %{pfx}_r{b0} : f64\n"
    ));
    for (nz_idx, (_, ki)) in nzs.iter().enumerate().skip(1) {
        let a_reg = shape.lhs_start + out_row * shape.k + ki;
        let b_reg = shape.rhs_start + ki * shape.n + out_col;
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
pub(super) fn render_linsolve_mlir_function(
    node: Value,
    node_id: Value,
    output_offset: Value,
) -> Result<String, minijinja::Error> {
    let id = node_id.as_usize().unwrap_or(0);
    let offset = output_offset.as_usize().unwrap_or(0);

    let setup_ops = get_field(&node, "setup_ops")?;
    let matrix_start = solve_field_usize(&node, "matrix_start")?;
    let rhs_start = solve_field_usize(&node, "rhs_start")?;
    let n = solve_field_usize(&node, "n")?;
    let n2 = n * n;

    let pfx = format!("ls{id}");
    let mut out = format!("    // LinSolve {n}×{n} → out[{offset}..{}]\n", offset + n);

    // Evaluate setup_ops → fills registers matrix_start..+n*n and rhs_start..+n
    emit_linear_ops_mlir(&setup_ops, &pfx, &mut out)?;

    // Alloca flat A (n×n) and b (n) buffers on the stack
    out.push_str(&format!(
        "    %{pfx}_A = memref.alloca() : memref<{n2}xf64>\n"
    ));
    for i in 0..n2 {
        let reg = matrix_start + i;
        out.push_str(&format!(
            "    %{pfx}_Ai{i} = arith.constant {i} : index\n\
             \tmemref.store %{pfx}_r{reg}, %{pfx}_A[%{pfx}_Ai{i}] : memref<{n2}xf64>\n"
        ));
    }
    out.push_str(&format!(
        "    %{pfx}_b = memref.alloca() : memref<{n}xf64>\n"
    ));
    for i in 0..n {
        let reg = rhs_start + i;
        out.push_str(&format!(
            "    %{pfx}_bi{i} = arith.constant {i} : index\n\
             \tmemref.store %{pfx}_r{reg}, %{pfx}_b[%{pfx}_bi{i}] : memref<{n}xf64>\n"
        ));
    }

    // Extract aligned pointers as i64 (avoids memref-descriptor ABI complexity)
    out.push_str(&format!(
        "    %{pfx}_Aidx = memref.extract_aligned_pointer_as_index %{pfx}_A : memref<{n2}xf64> -> index\n\
         \t%{pfx}_Ai64 = arith.index_cast %{pfx}_Aidx : index to i64\n\
         \t%{pfx}_bidx = memref.extract_aligned_pointer_as_index %{pfx}_b : memref<{n}xf64> -> index\n\
         \t%{pfx}_bi64 = arith.index_cast %{pfx}_bidx : index to i64\n\
         \t%{pfx}_nn = arith.constant {n} : i64\n"
    ));

    // Call runtime once per component, store each result to output
    for comp in 0..n {
        let output_idx = offset + comp;
        out.push_str(&format!(
            "    %{pfx}_comp{comp} = arith.constant {comp} : i64\n\
             \t%{pfx}_x{comp} = func.call @rumoca_solve_linear_component(%{pfx}_Ai64, %{pfx}_bi64, %{pfx}_nn, %{pfx}_comp{comp}) : (i64, i64, i64, i64) -> f64\n\
             \t%{pfx}_oi{comp} = arith.constant {output_idx} : index\n\
             \tmemref.store %{pfx}_x{comp}, %out[%{pfx}_oi{comp}] : memref<?xf64>\n"
        ));
    }

    Ok(out)
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
/// Returns `None` if the value is not an `Explicit` variant or cannot be parsed.
fn extract_explicit_nnz(sparsity: &Value) -> Option<Vec<(usize, usize)>> {
    let explicit = get_field(sparsity, "Explicit").ok()?;
    let nnz_val = get_field(&explicit, "nnz").ok()?;
    let len = nnz_val.len()?;
    let mut nnz = Vec::with_capacity(len);
    for i in 0..len {
        let pair = nnz_val.get_item(&minijinja::Value::from(i)).ok()?;
        let row = pair.get_item(&minijinja::Value::from(0)).ok()?.as_usize()?;
        let col = pair.get_item(&minijinja::Value::from(1)).ok()?.as_usize()?;
        nnz.push((row, col));
    }
    Some(nnz)
}

pub(super) fn render_solve_row_c_function(row: Value, config: Value) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_row_c(&row, &cfg)
}

pub(super) fn render_solve_row_rust_function(row: Value, config: Value) -> RenderResult {
    let cfg = SolveRowCConfig::from_value(&config);
    render_solve_row_for(&row, &cfg, SolveRowDialect::Rust)
}

pub(super) fn render_solve_slot_assign_c_function(
    slot: Value,
    value: Value,
    config: Value,
) -> RenderResult {
    render_solve_slot_assign_c_required(&slot, &value, &config)
}

pub(super) fn render_solve_pre_param_binding_c_function(
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

pub(super) fn render_optional_solve_slot_assign_c_function(
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
pub(super) fn render_matmul_c_function(
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
        extract_explicit_nnz(&lhs_sparsity_val)
    } else {
        None
    };

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

    let end_offset = offset + m * n;

    if is_diagonal_matvec {
        // A (m×m diagonal) * x (m×1): output[i] = A[i,i] * x[i].
        // Only diagonal elements of lhs are needed: positions i*k+i = i*(m+1).
        let mut lines = format!("    /* DiagonalMul {m}x{m}: output[{offset}..{end_offset}] */\n");
        for i in 0..m {
            let diag_reg = solve_reg(&regs, lhs_start + i * (m + 1))?;
            let rhs_reg = solve_reg(&regs, rhs_start + i)?;
            lines.push_str(&format!(
                "\tm->xdot[{out}] = ({diag_reg}) * ({rhs_reg});\n",
                out = offset + i,
            ));
        }
        Ok(lines.trim_end().to_string())
    } else if let Some(nnz) = explicit_nnz {
        let lines = render_explicit_sparse_matmul_c(
            &regs,
            &nnz,
            MatMulRenderShape {
                lhs_start,
                rhs_start,
                m,
                k,
                n,
                offset,
            },
            end_offset,
        )?;
        Ok(lines.trim_end().to_string())
    } else {
        let lhs_elems: Result<Vec<_>, _> = (0..m * k)
            .map(|i| solve_reg(&regs, lhs_start + i))
            .collect();
        let rhs_elems: Result<Vec<_>, _> = (0..k * n)
            .map(|i| solve_reg(&regs, rhs_start + i))
            .collect();
        let lhs_array = lhs_elems?.join(", ");
        let rhs_array = rhs_elems?.join(", ");
        let mk = m * k;
        let kn = k * n;
        Ok(format!(
            "    /* MatMul {m}x{k}x{n}: output[{offset}..{end_offset}] */\n\
             \t{{\n\
             \t\tdouble __lhs[{mk}] = {{{lhs_array}}};\n\
             \t\tdouble __rhs[{kn}] = {{{rhs_array}}};\n\
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
    for slot in 0..shape.m * shape.n {
        let out_row = slot / shape.n;
        let out_col = slot % shape.n;
        let output_idx = shape.offset + slot;
        let row_nzs: Vec<(usize, usize)> =
            nnz.iter().filter(|(r, _)| *r == out_row).copied().collect();
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

    let terms: Result<Vec<String>, minijinja::Error> = nzs
        .iter()
        .map(|(_, ki)| {
            let a = solve_reg(regs, shape.lhs_start + out_row * shape.k + ki)?;
            let b = solve_reg(regs, shape.rhs_start + ki * shape.n + out_col)?;
            Ok(format!("({a}) * ({b})"))
        })
        .collect();
    lines.push_str(&format!(
        "\tm->xdot[{output_idx}] = {};\n",
        terms?.join(" + ")
    ));
    Ok(())
}

fn render_solve_row_c(row: &Value, cfg: &SolveRowCConfig) -> RenderResult {
    render_solve_row_for(row, cfg, SolveRowDialect::C)
}

fn render_solve_row_for(
    row: &Value,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
) -> RenderResult {
    let mut regs = Vec::<String>::new();
    let mut output = None;
    let iter = row
        .try_iter()
        .map_err(|_| render_err("solve row must be an array of LinearOp values"))?;
    for op in iter {
        output = render_solve_op_for(&op, cfg, dialect, &mut regs, output)?;
    }
    output.ok_or_else(|| render_err("solve row did not contain StoreOutput"))
}

fn render_solve_op_c(
    op: &Value,
    cfg: &SolveRowCConfig,
    regs: &mut Vec<String>,
    output: Option<String>,
) -> Result<Option<String>, minijinja::Error> {
    render_solve_op_for(op, cfg, SolveRowDialect::C, regs, output)
}

fn render_solve_op_for(
    op: &Value,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
    regs: &mut Vec<String>,
    output: Option<String>,
) -> Result<Option<String>, minijinja::Error> {
    if let Ok(value) = get_field(op, "Const") {
        let dst = solve_field_usize(&value, "dst")?;
        let value = solve_const_value_string(&value, dialect.infinity())?;
        store_solve_reg(regs, dst, value);
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "LoadTime") {
        let dst = solve_field_usize(&value, "dst")?;
        store_solve_reg(regs, dst, cfg.time.clone());
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "LoadY") {
        let dst = solve_field_usize(&value, "dst")?;
        let index = solve_field_usize(&value, "index")?;
        store_solve_reg(regs, dst, cfg.y_access(index));
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "LoadP") {
        let dst = solve_field_usize(&value, "dst")?;
        let index = solve_field_usize(&value, "index")?;
        store_solve_reg(regs, dst, cfg.p_access(index));
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "LoadSeed") {
        let dst = solve_field_usize(&value, "dst")?;
        let index = solve_field_usize(&value, "index")?;
        let Some(seed) = cfg.seed_access(index) else {
            return Err(render_err(
                "LoadSeed requires a `seed` access pattern in solve-row C output",
            ));
        };
        store_solve_reg(regs, dst, seed);
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "Move") {
        let dst = solve_field_usize(&value, "dst")?;
        let src = solve_reg(regs, solve_field_usize(&value, "src")?)?;
        store_solve_reg(regs, dst, src);
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "LinearSolveComponent") {
        let dst = solve_field_usize(&value, "dst")?;
        let matrix_start = solve_field_usize(&value, "matrix_start")?;
        let rhs_start = solve_field_usize(&value, "rhs_start")?;
        let n = solve_field_usize(&value, "n")?;
        let component = solve_field_usize(&value, "component")?;
        let expr =
            dialect.render_linear_solve_component(regs, matrix_start, rhs_start, n, component)?;
        store_solve_reg(regs, dst, expr);
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "Unary") {
        let dst = solve_field_usize(&value, "dst")?;
        let op = solve_variant_name(&get_field(&value, "op")?)?;
        let arg = solve_reg(regs, solve_field_usize(&value, "arg")?)?;
        store_solve_reg(regs, dst, dialect.render_unary(&op, arg)?);
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "Binary") {
        let dst = solve_field_usize(&value, "dst")?;
        let op = solve_variant_name(&get_field(&value, "op")?)?;
        let lhs = solve_reg(regs, solve_field_usize(&value, "lhs")?)?;
        let rhs = solve_reg(regs, solve_field_usize(&value, "rhs")?)?;
        store_solve_reg(regs, dst, dialect.render_binary(&op, lhs, rhs)?);
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "Compare") {
        let dst = solve_field_usize(&value, "dst")?;
        let op = solve_variant_name(&get_field(&value, "op")?)?;
        let lhs = solve_reg(regs, solve_field_usize(&value, "lhs")?)?;
        let rhs = solve_reg(regs, solve_field_usize(&value, "rhs")?)?;
        store_solve_reg(regs, dst, render_solve_compare(&op, lhs, rhs)?);
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "Select") {
        let dst = solve_field_usize(&value, "dst")?;
        let cond = solve_reg(regs, solve_field_usize(&value, "cond")?)?;
        let if_true = solve_reg(regs, solve_field_usize(&value, "if_true")?)?;
        let if_false = solve_reg(regs, solve_field_usize(&value, "if_false")?)?;
        store_solve_reg(
            regs,
            dst,
            format!("(({cond}) != 0.0 ? ({if_true}) : ({if_false}))"),
        );
        return Ok(output);
    }
    if let Ok(value) = get_field(op, "StoreOutput") {
        let src = solve_field_usize(&value, "src")?;
        return Ok(Some(solve_reg(regs, src)?));
    }
    Err(render_err(format!("unsupported solve LinearOp: {op}")))
}

#[derive(Clone, Copy)]
enum SolveRowDialect {
    C,
    Rust,
}

impl SolveRowDialect {
    fn infinity(self) -> &'static str {
        match self {
            Self::C => "INFINITY",
            Self::Rust => "f64::INFINITY",
        }
    }

    fn render_linear_solve_component(
        self,
        regs: &[String],
        matrix_start: usize,
        rhs_start: usize,
        n: usize,
        component: usize,
    ) -> RenderResult {
        if component >= n {
            return Err(render_err(
                "LinearSolveComponent component is out of bounds",
            ));
        }
        let matrix = (0..n * n)
            .map(|offset| solve_reg(regs, matrix_start + offset))
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");
        let rhs = (0..n)
            .map(|offset| solve_reg(regs, rhs_start + offset))
            .collect::<Result<Vec<_>, _>>()?
            .join(", ");
        match self {
            Self::C => Ok(format!(
                "__rumoca_solve_linear_component((double[]){{{matrix}}}, (double[]){{{rhs}}}, {n}, {component})"
            )),
            Self::Rust => Ok(format!(
                "rumoca_solve_linear_component(&[{matrix}], &[{rhs}], {n}, {component})"
            )),
        }
    }

    fn render_unary(self, op: &str, arg: String) -> RenderResult {
        match self {
            Self::C => render_solve_unary_c(op, arg),
            Self::Rust => render_solve_unary_rust(op, arg),
        }
    }

    fn render_binary(self, op: &str, lhs: String, rhs: String) -> RenderResult {
        match self {
            Self::C => render_solve_binary_c(op, lhs, rhs),
            Self::Rust => render_solve_binary_rust(op, lhs, rhs),
        }
    }
}

fn render_solve_unary_c(op: &str, arg: String) -> RenderResult {
    match op {
        "Neg" => Ok(format!("(-({arg}))")),
        "Not" => Ok(format!("(({arg}) == 0.0 ? 1.0 : 0.0)")),
        "Abs" => Ok(format!("fabs({arg})")),
        "Sign" => Ok(format!(
            "(({arg}) > 0.0 ? 1.0 : (({arg}) < 0.0 ? -1.0 : 0.0))"
        )),
        "Sqrt" => Ok(format!("sqrt({arg})")),
        "Floor" => Ok(format!("floor({arg})")),
        "Ceil" => Ok(format!("ceil({arg})")),
        "Trunc" => Ok(format!("trunc({arg})")),
        "Sin" => Ok(format!("sin({arg})")),
        "Cos" => Ok(format!("cos({arg})")),
        "Tan" => Ok(format!("tan({arg})")),
        "Asin" => Ok(format!("asin({arg})")),
        "Acos" => Ok(format!("acos({arg})")),
        "Atan" => Ok(format!("atan({arg})")),
        "Sinh" => Ok(format!("sinh({arg})")),
        "Cosh" => Ok(format!("cosh({arg})")),
        "Tanh" => Ok(format!("tanh({arg})")),
        "Exp" => Ok(format!("exp({arg})")),
        "Log" => Ok(format!("log({arg})")),
        "Log10" => Ok(format!("log10({arg})")),
        _ => Err(render_err(format!("unsupported solve unary op: {op}"))),
    }
}

fn render_solve_unary_rust(op: &str, arg: String) -> RenderResult {
    match op {
        "Neg" => Ok(format!("(-({arg}))")),
        "Not" => Ok(format!("if ({arg}) == 0.0 {{ 1.0 }} else {{ 0.0 }}")),
        "Abs" => Ok(format!("({arg}).abs()")),
        "Sign" => Ok(format!(
            "if ({arg}) > 0.0 {{ 1.0 }} else if ({arg}) < 0.0 {{ -1.0 }} else {{ 0.0 }}"
        )),
        "Sqrt" => Ok(format!("({arg}).sqrt()")),
        "Floor" => Ok(format!("({arg}).floor()")),
        "Ceil" => Ok(format!("({arg}).ceil()")),
        "Trunc" => Ok(format!("({arg}).trunc()")),
        "Sin" => Ok(format!("({arg}).sin()")),
        "Cos" => Ok(format!("({arg}).cos()")),
        "Tan" => Ok(format!("({arg}).tan()")),
        "Asin" => Ok(format!("({arg}).asin()")),
        "Acos" => Ok(format!("({arg}).acos()")),
        "Atan" => Ok(format!("({arg}).atan()")),
        "Sinh" => Ok(format!("({arg}).sinh()")),
        "Cosh" => Ok(format!("({arg}).cosh()")),
        "Tanh" => Ok(format!("({arg}).tanh()")),
        "Exp" => Ok(format!("({arg}).exp()")),
        "Log" => Ok(format!("({arg}).ln()")),
        "Log10" => Ok(format!("({arg}).log10()")),
        _ => Err(render_err(format!("unsupported solve unary op: {op}"))),
    }
}

fn render_solve_binary_c(op: &str, lhs: String, rhs: String) -> RenderResult {
    match op {
        "Add" => Ok(format!("(({lhs}) + ({rhs}))")),
        "Sub" => Ok(format!("(({lhs}) - ({rhs}))")),
        "Mul" => Ok(format!("(({lhs}) * ({rhs}))")),
        "Div" => Ok(format!("(({lhs}) / ({rhs}))")),
        "Pow" => Ok(format!("pow({lhs}, {rhs})")),
        "And" => Ok(format!("((({lhs}) != 0.0 && ({rhs}) != 0.0) ? 1.0 : 0.0)")),
        "Or" => Ok(format!("((({lhs}) != 0.0 || ({rhs}) != 0.0) ? 1.0 : 0.0)")),
        "Atan2" => Ok(format!("atan2({lhs}, {rhs})")),
        "Min" => Ok(format!("fmin({lhs}, {rhs})")),
        "Max" => Ok(format!("fmax({lhs}, {rhs})")),
        _ => Err(render_err(format!("unsupported solve binary op: {op}"))),
    }
}

fn render_solve_binary_rust(op: &str, lhs: String, rhs: String) -> RenderResult {
    match op {
        "Add" => Ok(format!("(({lhs}) + ({rhs}))")),
        "Sub" => Ok(format!("(({lhs}) - ({rhs}))")),
        "Mul" => Ok(format!("(({lhs}) * ({rhs}))")),
        "Div" => Ok(format!("(({lhs}) / ({rhs}))")),
        "Pow" => Ok(format!("({lhs}).powf({rhs})")),
        "And" => Ok(format!(
            "if ({lhs}) != 0.0 && ({rhs}) != 0.0 {{ 1.0 }} else {{ 0.0 }}"
        )),
        "Or" => Ok(format!(
            "if ({lhs}) != 0.0 || ({rhs}) != 0.0 {{ 1.0 }} else {{ 0.0 }}"
        )),
        "Atan2" => Ok(format!("({lhs}).atan2({rhs})")),
        "Min" => Ok(format!("({lhs}).min({rhs})")),
        "Max" => Ok(format!("({lhs}).max({rhs})")),
        _ => Err(render_err(format!("unsupported solve binary op: {op}"))),
    }
}

fn render_solve_compare(op: &str, lhs: String, rhs: String) -> RenderResult {
    let op = match op {
        "Lt" => "<",
        "Le" => "<=",
        "Gt" => ">",
        "Ge" => ">=",
        "Eq" => "==",
        "Ne" => "!=",
        _ => return Err(render_err(format!("unsupported solve compare op: {op}"))),
    };
    Ok(format!("((({lhs}) {op} ({rhs})) ? 1.0 : 0.0)"))
}

fn store_solve_reg(regs: &mut Vec<String>, dst: usize, expr: String) {
    if regs.len() <= dst {
        regs.resize(dst + 1, String::new());
    }
    regs[dst] = expr;
}

fn solve_reg(regs: &[String], reg: usize) -> RenderResult {
    regs.get(reg)
        .filter(|expr| !expr.is_empty())
        .cloned()
        .ok_or_else(|| render_err(format!("solve row references unset register {reg}")))
}

fn solve_field_usize(value: &Value, field: &str) -> Result<usize, minijinja::Error> {
    solve_field_string(value, field)?
        .parse::<usize>()
        .map_err(|_| render_err(format!("solve field `{field}` is not a usize")))
}

fn solve_field_string(value: &Value, field: &str) -> Result<String, minijinja::Error> {
    Ok(value_to_string(&get_field(value, field)?))
}

fn solve_const_value_string(value: &Value, infinity: &str) -> Result<String, minijinja::Error> {
    let field = value
        .get_attr("value")
        .or_else(|_| value.get_item(&Value::from("value")))?;
    if field.is_none() {
        return Ok(infinity.to_string());
    }
    if field.is_undefined() {
        return Err(render_err("solve Const op is missing `value`"));
    }
    Ok(value_to_string(&field))
}

fn solve_variant_name(value: &Value) -> RenderResult {
    let raw = value_to_string(value);
    if !raw.is_empty() && raw != "{}" {
        return Ok(raw);
    }
    for name in [
        "Neg", "Not", "Abs", "Sign", "Sqrt", "Floor", "Ceil", "Trunc", "Sin", "Cos", "Tan", "Asin",
        "Acos", "Atan", "Sinh", "Cosh", "Tanh", "Exp", "Log", "Log10", "Add", "Sub", "Mul", "Div",
        "Pow", "And", "Or", "Atan2", "Min", "Max", "Lt", "Le", "Gt", "Ge", "Eq", "Ne",
    ] {
        if get_field(value, name).is_ok() {
            return Ok(name.to_string());
        }
    }
    Err(render_err(format!("unsupported solve op variant: {value}")))
}

struct SolveRowCConfig {
    time: String,
    y_pattern: String,
    p_pattern: String,
    seed_pattern: Option<String>,
}

impl SolveRowCConfig {
    fn from_value(value: &Value) -> Self {
        Self {
            time: config_string(value, "time").unwrap_or_else(|| "m->time".to_string()),
            y_pattern: config_string(value, "y")
                .unwrap_or_else(|| "__rumoca_solve_y(m, {})".to_string()),
            p_pattern: config_string(value, "p")
                .unwrap_or_else(|| "__rumoca_solve_p(m, {})".to_string()),
            seed_pattern: config_string(value, "seed"),
        }
    }

    fn y_access(&self, index: usize) -> String {
        format_solve_access(&self.y_pattern, index)
    }

    fn p_access(&self, index: usize) -> String {
        format_solve_access(&self.p_pattern, index)
    }

    fn seed_access(&self, index: usize) -> Option<String> {
        self.seed_pattern
            .as_ref()
            .map(|pattern| format_solve_access(pattern, index))
    }
}

struct SolveSlotAssignCConfig {
    y_set_pattern: String,
    p_set_pattern: String,
}

impl SolveSlotAssignCConfig {
    fn from_value(value: &Value) -> Self {
        Self {
            y_set_pattern: config_string(value, "y_set")
                .unwrap_or_else(|| "__rumoca_solve_set_y(m, {}, {})".to_string()),
            p_set_pattern: config_string(value, "p_set")
                .unwrap_or_else(|| "__rumoca_solve_set_p(m, {}, {})".to_string()),
        }
    }
}

fn format_solve_access(pattern: &str, index: usize) -> String {
    if pattern.contains("{}") {
        return pattern.replacen("{}", &index.to_string(), 1);
    }
    format!("{pattern}[{index}]")
}

fn format_solve_set(pattern: &str, index: usize, value: &str) -> String {
    if pattern.contains("{}") {
        return pattern
            .replacen("{}", &index.to_string(), 1)
            .replacen("{}", value, 1);
    }
    format!("{pattern}[{index}] = {value}")
}

fn config_string(value: &Value, field: &str) -> Option<String> {
    get_field(value, field)
        .ok()
        .map(|field| value_to_string(&field))
        .filter(|field| !field.is_empty())
}
