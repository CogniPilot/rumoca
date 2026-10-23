//! Host kernel for dense `Real` (binary64) matrix products called from JIT code.
//!
//! Generated scalar code forms each output as `(a[i,0]·b[0,j] + a[i,1]·b[1,j]) + …`
//! with separate multiplies and adds: the first product is its own sum, so an
//! all `-0.0` dot keeps its sign, and an empty dot is `+0.0`. This kernel keeps
//! exactly that sequence per output (`k` ascending, no fused multiply-add), so
//! its results are bit-identical to the generated loop. Only the loop order
//! changes: `i`, then `k`, then a contiguous sweep over `j`, which reads both
//! operands along rows and lets the compiler vectorize the inner sweep.

use std::ptr;

/// `dst[rows×columns] = lhs[rows×inner] · rhs[inner×columns]`, all dense
/// row-major binary64. A vector operand is the one-row or one-column case.
///
/// # Safety
///
/// Each pointer must address the stated number of readable (and, for `dst`,
/// writable) `f64` cells. `dst` may overlap an operand: the product is then
/// formed in a temporary before it is written.
pub(super) unsafe extern "C" fn rumoca_host_matmul_f64(
    dst: *mut f64,
    lhs: *const f64,
    rhs: *const f64,
    rows: u64,
    inner: u64,
    columns: u64,
) {
    #[cfg(test)]
    MATMUL_CALLS.with(|calls| calls.set(calls.get() + 1));
    let shape = (rows as usize, inner as usize, columns as usize);
    // SAFETY: forwarded from this function's contract.
    unsafe { multiply_cells(dst, lhs, rhs, shape) };
}

/// The body of [`rumoca_host_matmul_f64`], shared with the transposed entry.
///
/// # Safety
///
/// As [`rumoca_host_matmul_f64`].
unsafe fn multiply_cells(
    dst: *mut f64,
    lhs: *const f64,
    rhs: *const f64,
    (rows, inner, columns): (usize, usize, usize),
) {
    let (lhs_len, rhs_len, dst_len) = (rows * inner, inner * columns, rows * columns);
    let overlaps = |start: *const f64, len: usize| {
        let (a, b) = (dst as usize, start as usize);
        let bytes = std::mem::size_of::<f64>();
        a < b + len * bytes && b < a + dst_len * bytes
    };
    // SAFETY: the caller guarantees both operand ranges are readable.
    let (a, b) = unsafe {
        (
            std::slice::from_raw_parts(lhs, lhs_len),
            std::slice::from_raw_parts(rhs, rhs_len),
        )
    };
    if overlaps(lhs, lhs_len) || overlaps(rhs, rhs_len) {
        let mut product = vec![0.0; dst_len];
        multiply(&mut product, a, b, (rows, inner, columns));
        // SAFETY: `dst` addresses `dst_len` writable cells; `product` is a
        // separate allocation, and a copy tolerates the operands' overlap.
        unsafe { ptr::copy(product.as_ptr(), dst, dst_len) };
        return;
    }
    // SAFETY: `dst` addresses `dst_len` writable cells disjoint from `a`, `b`.
    let c = unsafe { std::slice::from_raw_parts_mut(dst, dst_len) };
    multiply(c, a, b, (rows, inner, columns));
}

/// `dst` = the transpose of `src`, a `rows×columns` matrix of `trailing`-cell
/// elements, both dense row-major 64-bit cells; any cell type moves unchanged.
///
/// # Safety
///
/// Each pointer must address `rows·columns·trailing` cells, readable for
/// `src` and writable for `dst`. The ranges may overlap.
pub(super) unsafe extern "C" fn rumoca_host_transpose_cells(
    dst: *mut u64,
    src: *const u64,
    rows: u64,
    columns: u64,
    trailing: u64,
) {
    let (rows, columns, trailing) = (rows as usize, columns as usize, trailing as usize);
    let len = rows * columns * trailing;
    let bytes = len * std::mem::size_of::<u64>();
    let (d, s) = (dst as usize, src as usize);
    let copied;
    let source = if d < s + bytes && s < d + bytes {
        // SAFETY: the caller guarantees `len` readable cells at `src`.
        copied = unsafe { std::slice::from_raw_parts(src, len) }.to_vec();
        copied.as_ptr()
    } else {
        src
    };
    // SAFETY: `len` cells are readable at `source` and writable at `dst`, and
    // the two ranges are disjoint (an overlapping source was copied out).
    unsafe { transpose_blocked(dst, source, rows, columns, trailing) };
}

/// Transpose in square tiles so both the reads and the strided writes of a
/// tile stay within a few cache lines.
unsafe fn transpose_blocked(
    dst: *mut u64,
    src: *const u64,
    rows: usize,
    columns: usize,
    trailing: usize,
) {
    const TILE: usize = 16;
    for row_start in (0..rows).step_by(TILE) {
        for column_start in (0..columns).step_by(TILE) {
            let tile_rows = row_start..(row_start + TILE).min(rows);
            let tile_columns = column_start..(column_start + TILE).min(columns);
            // SAFETY: the tile lies inside both `rows×columns` matrices.
            unsafe { transpose_tile(dst, src, (rows, columns, trailing), tile_rows, tile_columns) };
        }
    }
}

/// Move one element of `trailing` cells; a one-cell element moves directly.
///
/// # Safety
///
/// Both pointers address `trailing` cells, disjoint from each other.
#[inline]
unsafe fn copy_element(src: *const u64, dst: *mut u64, trailing: usize) {
    if trailing == 1 {
        // SAFETY: per the caller, one cell at each pointer.
        unsafe { *dst = *src };
    } else {
        // SAFETY: per the caller, `trailing` disjoint cells at each pointer.
        unsafe { ptr::copy_nonoverlapping(src, dst, trailing) };
    }
}

/// One tile of [`transpose_blocked`]: each element's `trailing` cells move
/// together.
///
/// # Safety
///
/// As [`transpose_blocked`], with the tile inside the matrix.
unsafe fn transpose_tile(
    dst: *mut u64,
    src: *const u64,
    (rows, columns, trailing): (usize, usize, usize),
    tile_rows: std::ops::Range<usize>,
    tile_columns: std::ops::Range<usize>,
) {
    for row in tile_rows {
        for column in tile_columns.clone() {
            let from = (row * columns + column) * trailing;
            let to = (column * rows + row) * trailing;
            // SAFETY: both element ranges are below `rows·columns·trailing`,
            // and the caller keeps `dst` and `src` disjoint.
            unsafe { copy_element(src.add(from), dst.add(to), trailing) };
        }
    }
}

/// [`rumoca_host_matmul_f64`] where bit 0 of `transposed` means `lhs` holds
/// the left operand's transpose (`inner×rows`) and bit 1 that `rhs` holds the
/// right operand's (`columns×inner`). The product, and each output's term
/// sequence, is that of the untransposed operands.
///
/// # Safety
///
/// As [`rumoca_host_matmul_f64`], with the stored extents above.
pub(super) unsafe extern "C" fn rumoca_host_matmul_t_f64(
    dst: *mut f64,
    lhs: *const f64,
    rhs: *const f64,
    rows: u64,
    inner: u64,
    columns: u64,
    transposed: u64,
) {
    #[cfg(test)]
    MATMUL_CALLS.with(|calls| calls.set(calls.get() + 1));
    let (r, i, c) = (rows as usize, inner as usize, columns as usize);
    #[cfg(target_arch = "x86_64")]
    {
        let bytes = std::mem::size_of::<f64>();
        let (d, l, q) = (dst as usize, lhs as usize, rhs as usize);
        let disjoint = (d + r * c * bytes <= l || l + r * i * bytes <= d)
            && (d + r * c * bytes <= q || q + i * c * bytes <= d);
        if disjoint && transposed == 1 && c < 8 && std::arch::is_x86_feature_detected!("avx2") {
            // SAFETY: AVX2 was detected; the operands hold their stored
            // extents and `dst` is disjoint from both.
            unsafe {
                avx2::transposed_left_columns(
                    std::slice::from_raw_parts_mut(dst, r * c),
                    std::slice::from_raw_parts(lhs, r * i),
                    std::slice::from_raw_parts(rhs, i * c),
                    (r, i, c),
                )
            };
            return;
        }
        if disjoint
            && r % 4 == 0
            && c % 8 == 0
            && c > 0
            && std::arch::is_x86_feature_detected!("avx2")
        {
            // SAFETY: AVX2 was detected; the operands hold their stored
            // extents and `dst` is disjoint from both.
            unsafe {
                avx2::multiply_packed(
                    std::slice::from_raw_parts_mut(dst, r * c),
                    std::slice::from_raw_parts(lhs, r * i),
                    std::slice::from_raw_parts(rhs, i * c),
                    (r, i, c),
                    (transposed & 1 != 0, transposed & 2 != 0),
                )
            };
            return;
        }
    }
    let materialize = |source: *const f64, stored_rows: usize, stored_columns: usize| {
        let mut cells = vec![0.0f64; stored_rows * stored_columns];
        // SAFETY: the caller guarantees the stored extents are readable, and
        // `cells` is a separate allocation of the same size.
        unsafe {
            transpose_blocked(
                cells.as_mut_ptr().cast::<u64>(),
                source.cast::<u64>(),
                stored_rows,
                stored_columns,
                1,
            )
        };
        cells
    };
    let lhs_cells;
    let lhs = if transposed & 1 != 0 {
        lhs_cells = materialize(lhs, i, r);
        lhs_cells.as_ptr()
    } else {
        lhs
    };
    let rhs_cells;
    let rhs = if transposed & 2 != 0 {
        rhs_cells = materialize(rhs, c, i);
        rhs_cells.as_ptr()
    } else {
        rhs
    };
    // SAFETY: both operands now address their untransposed extents.
    unsafe { multiply_cells(dst, lhs, rhs, (r, i, c)) };
}

#[cfg(test)]
thread_local! {
    static MATMUL_CALLS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Calls of either kernel entry on this thread since the last take: an invocation
/// witness the scalar loop cannot produce.
#[cfg(test)]
pub(in crate::emit) fn take_matmul_calls() -> usize {
    MATMUL_CALLS.with(|calls| calls.replace(0))
}

fn multiply(c: &mut [f64], a: &[f64], b: &[f64], shape: (usize, usize, usize)) {
    let (rows, _inner, columns) = shape;
    if rows == 0 || columns == 0 {
        return;
    }
    #[cfg(target_arch = "x86_64")]
    {
        if std::arch::is_x86_feature_detected!("avx2") {
            // SAFETY: AVX2 was detected on this CPU, and the slices hold the
            // stated extents (checked by the public entry point).
            unsafe { avx2::multiply(c, a, b, shape) };
            return;
        }
    }
    multiply_portable(c, a, b, shape);
}

/// The portable form of the kernel: `i`, then `k`, then a contiguous sweep
/// over `j`, which the compiler vectorizes with the baseline instruction set.
fn multiply_portable(
    c: &mut [f64],
    a: &[f64],
    b: &[f64],
    (rows, inner, columns): (usize, usize, usize),
) {
    for (i, out) in c.chunks_exact_mut(columns).enumerate().take(rows) {
        out.fill(0.0);
        for k in 0..inner {
            let aik = a[i * inner + k];
            let b_row = &b[k * columns..(k + 1) * columns];
            for (cij, bkj) in out.iter_mut().zip(b_row) {
                *cij = accumulate(*cij, aik * *bkj, k == 0);
            }
        }
    }
}

/// `sum + product`, except that the first term is its own sum.
#[inline(always)]
fn accumulate(sum: f64, product: f64, first: bool) -> f64 {
    if first { product } else { sum + product }
}

/// Register-tiled AVX2 form. Each output still starts from its first product
/// and adds its later `k` terms in ascending order with separate multiplies
/// and adds, so the tiling changes which outputs are formed together, never
/// the per-output sequence: the results are bit-identical to
/// [`multiply_portable`].
#[cfg(target_arch = "x86_64")]
mod avx2 {
    use super::accumulate;
    use std::arch::x86_64::*;

    /// Rows formed together in a wide tile.
    const MR: usize = 4;
    /// Columns formed together in a wide tile: two 4-lane vectors.
    const NR: usize = 8;

    /// Lane-wise [`accumulate`]: `sum + product`, or `product` for the first
    /// term. The callers' `k == 0` tests peel out of their loops.
    #[target_feature(enable = "avx2")]
    #[inline]
    fn accumulate_lanes(sum: __m256d, product: __m256d, first: bool) -> __m256d {
        if first {
            product
        } else {
            _mm256_add_pd(sum, product)
        }
    }

    /// # Safety
    ///
    /// Requires AVX2, and `a`, `b`, `c` holding `rows×inner`, `inner×columns`
    /// and `rows×columns` cells.
    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn multiply(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        (rows, inner, columns): (usize, usize, usize),
    ) {
        debug_assert!(a.len() >= rows * inner && b.len() >= inner * columns);
        debug_assert!(c.len() >= rows * columns);
        if rows < MR && columns >= 4 {
            // Too few rows for a tile: sweep each output row over `k`.
            // SAFETY: extents hold per the caller's contract.
            unsafe { row_sweep(c, a, b, rows, inner, columns) };
            return;
        }
        if columns < 4 {
            // SAFETY: extents hold per the caller's contract.
            unsafe { narrow(c, a, b, rows, inner, columns) };
            return;
        }
        let wide_columns = columns - columns % NR;
        let full_rows = rows - rows % MR;
        // Column panels outermost, each packed contiguously, and row tiles of
        // `a` packed `k`-major: the common extents are powers of two, whose
        // row strides map a panel onto a few L1 sets. Packing changes only
        // where operands are read from, never an output's term sequence.
        let (mut panel, mut packed) = take_buffers(inner * NR, full_rows * inner);
        pack_row_tiles(&mut packed[..full_rows * inner], inner, |row, k| {
            a[row * inner + k]
        });
        let mut j = 0;
        while j < wide_columns {
            for k in 0..inner {
                panel[k * NR..(k + 1) * NR]
                    .copy_from_slice(&b[k * columns + j..k * columns + j + NR]);
            }
            let mut i = 0;
            while i < full_rows {
                let tile = &packed[(i / MR) * inner * MR..(i / MR + 1) * inner * MR];
                // SAFETY: tile [i, i+MR) × [j, j+NR) lies inside `c`.
                unsafe { packed_tile(c, tile, &panel, inner, columns, i, j) };
                i += MR;
            }
            while i < rows {
                // SAFETY: row i, columns [j, j+NR) lie inside `c`.
                unsafe { wide_tile::<1>(c, a, b, inner, columns, i, j) };
                i += 1;
            }
            j += NR;
        }
        give_buffers(panel, packed);
        let mut j = wide_columns;
        if columns - j >= 4 {
            let mut i = 0;
            while i < full_rows {
                // SAFETY: rows [i, i+MR), columns [j, j+4) lie inside `c`.
                unsafe { quad_tile(c, a, b, inner, columns, i, j) };
                i += MR;
            }
            // SAFETY: the remaining rows, columns [j, columns).
            unsafe { scalar_block(c, a, b, (inner, columns), full_rows..rows, j) };
            j += 4;
            // SAFETY: the tiled rows, columns past the four.
            unsafe { scalar_block(c, a, b, (inner, columns), 0..full_rows, j) };
        } else {
            // SAFETY: every row, columns [wide_columns, columns).
            unsafe { scalar_block(c, a, b, (inner, columns), 0..rows, j) };
        }
    }

    /// Rows a multiple of `MR` and columns of `NR`, packed straight from
    /// operands that may be stored transposed.
    ///
    /// # Safety
    ///
    /// Requires AVX2 and the stated extents; `c` disjoint from `a` and `b`.
    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn multiply_packed(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        (rows, inner, columns): (usize, usize, usize),
        (lhs_transposed, rhs_transposed): (bool, bool),
    ) {
        let lhs = |row: usize, k: usize| {
            if lhs_transposed {
                a[k * rows + row]
            } else {
                a[row * inner + k]
            }
        };
        let rhs = |k: usize, column: usize| {
            if rhs_transposed {
                b[column * inner + k]
            } else {
                b[k * columns + column]
            }
        };
        let (mut panel, mut packed) = take_buffers(inner * NR, rows * inner);
        pack_row_tiles(&mut packed[..rows * inner], inner, lhs);
        let mut j = 0;
        while j < columns {
            pack_panel(&mut panel[..inner * NR], j, rhs);
            let mut i = 0;
            while i < rows {
                let tile = &packed[(i / MR) * inner * MR..(i / MR + 1) * inner * MR];
                // SAFETY: tile [i, i+MR) × [j, j+NR) lies inside `c`.
                unsafe { packed_tile(c, tile, &panel, inner, columns, i, j) };
                i += MR;
            }
            j += NR;
        }
        give_buffers(panel, packed);
    }

    /// Row tiles of `MR` rows, `k`-major: cell `(tile·inner + k)·MR + r`
    /// holds the left operand at row `tile·MR + r`, column `k`.
    fn pack_row_tiles(packed: &mut [f64], inner: usize, lhs: impl Fn(usize, usize) -> f64) {
        if inner == 0 {
            return;
        }
        for (tile, cells) in packed.chunks_exact_mut(inner * MR).enumerate() {
            for (k, lanes) in cells.chunks_exact_mut(MR).enumerate() {
                let row = tile * MR;
                lanes
                    .iter_mut()
                    .enumerate()
                    .for_each(|(r, cell)| *cell = lhs(row + r, k));
            }
        }
    }

    /// The `inner × NR` column panel of the right operand starting at
    /// column `j`, row-major.
    fn pack_panel(panel: &mut [f64], j: usize, rhs: impl Fn(usize, usize) -> f64) {
        for (k, row) in panel.chunks_exact_mut(NR).enumerate() {
            row.iter_mut()
                .enumerate()
                .for_each(|(column, cell)| *cell = rhs(k, j + column));
        }
    }

    /// `MR` rows by `NR` columns from a packed row tile and column panel.
    ///
    /// # Safety
    ///
    /// Requires AVX2; `tile` holds `inner × MR` and `panel` `inner × NR`
    /// cells, and the output tile lies inside `c`.
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn packed_tile(
        c: &mut [f64],
        tile: &[f64],
        panel: &[f64],
        inner: usize,
        columns: usize,
        i: usize,
        j: usize,
    ) {
        // SAFETY: all offsets are inside the operands per the tile contract.
        unsafe {
            let mut lo = [_mm256_setzero_pd(); MR];
            let mut hi = [_mm256_setzero_pd(); MR];
            let t_ptr = tile.as_ptr();
            let p_ptr = panel.as_ptr();
            for k in 0..inner {
                let b_row = p_ptr.add(k * NR);
                let b_lo = _mm256_loadu_pd(b_row);
                let b_hi = _mm256_loadu_pd(b_row.add(4));
                for r in 0..MR {
                    let aik = _mm256_set1_pd(*t_ptr.add(k * MR + r));
                    lo[r] = accumulate_lanes(lo[r], _mm256_mul_pd(aik, b_lo), k == 0);
                    hi[r] = accumulate_lanes(hi[r], _mm256_mul_pd(aik, b_hi), k == 0);
                }
            }
            let c_ptr = c.as_mut_ptr();
            for r in 0..MR {
                let c_row = c_ptr.add((i + r) * columns + j);
                _mm256_storeu_pd(c_row, lo[r]);
                _mm256_storeu_pd(c_row.add(4), hi[r]);
            }
        }
    }

    thread_local! {
        static BUFFERS: std::cell::RefCell<(Vec<f64>, Vec<f64>)> =
            const { std::cell::RefCell::new((Vec::new(), Vec::new())) };
    }

    /// This thread's packing buffers, at least `panel` and `packed` cells long.
    /// Every cell the kernel reads is written by packing first, so the reused
    /// contents never reach a result.
    fn take_buffers(panel: usize, packed: usize) -> (Vec<f64>, Vec<f64>) {
        let (mut a, mut b) = BUFFERS.with(|buffers| std::mem::take(&mut *buffers.borrow_mut()));
        if a.len() < panel {
            a.resize(panel, 0.0);
        }
        if b.len() < packed {
            b.resize(packed, 0.0);
        }
        (a, b)
    }

    fn give_buffers(panel: Vec<f64>, packed: Vec<f64>) {
        BUFFERS.with(|buffers| *buffers.borrow_mut() = (panel, packed));
    }

    /// Each output row of a product with few rows, as `i`, `k`, then a
    /// vector sweep over `j`: every output still starts from its first
    /// product and adds the later terms in ascending `k` with separate
    /// multiplies and adds; an empty dot is `+0.0`.
    ///
    /// # Safety
    ///
    /// Requires AVX2 and the stated extents.
    #[target_feature(enable = "avx2")]
    unsafe fn row_sweep(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        rows: usize,
        inner: usize,
        columns: usize,
    ) {
        let vectors = columns / 4;
        for i in 0..rows {
            // SAFETY: row i of `c` and `a`, and every row of `b`, lie inside
            // the operands per the caller's contract.
            unsafe {
                let out = c.as_mut_ptr().add(i * columns);
                for j in 0..columns {
                    *out.add(j) = 0.0;
                }
                for k in 0..inner {
                    let aik = *a.as_ptr().add(i * inner + k);
                    let lanes = _mm256_set1_pd(aik);
                    let row = b.as_ptr().add(k * columns);
                    for v in 0..vectors {
                        let sum = accumulate_lanes(
                            _mm256_loadu_pd(out.add(4 * v)),
                            _mm256_mul_pd(lanes, _mm256_loadu_pd(row.add(4 * v))),
                            k == 0,
                        );
                        _mm256_storeu_pd(out.add(4 * v), sum);
                    }
                    for j in 4 * vectors..columns {
                        *out.add(j) = accumulate(*out.add(j), aik * *row.add(j), k == 0);
                    }
                }
            }
        }
    }

    /// A product whose left operand is stored transposed (`inner × rows`),
    /// with few columns: for each `k`, the stored row is a contiguous column
    /// of the left operand, scaled into each output column held contiguously.
    /// Each output starts from its first product and adds the later terms in
    /// ascending `k`.
    ///
    /// # Safety
    ///
    /// Requires AVX2; `a` holds `inner × rows`, `b` `inner × columns` and `c`
    /// `rows × columns` cells, `c` disjoint from both.
    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn transposed_left_columns(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        (rows, inner, columns): (usize, usize, usize),
    ) {
        let mut accumulators = vec![0.0f64; rows * columns];
        let vectors = rows / 4;
        for k in 0..inner {
            // SAFETY: row k of the stored left operand and of `b`, and every
            // accumulator column, lie inside their buffers.
            unsafe {
                let column = a.as_ptr().add(k * rows);
                for j in 0..columns {
                    let bkj = *b.as_ptr().add(k * columns + j);
                    let lanes = _mm256_set1_pd(bkj);
                    let out = accumulators.as_mut_ptr().add(j * rows);
                    for v in 0..vectors {
                        let sum = accumulate_lanes(
                            _mm256_loadu_pd(out.add(4 * v)),
                            _mm256_mul_pd(_mm256_loadu_pd(column.add(4 * v)), lanes),
                            k == 0,
                        );
                        _mm256_storeu_pd(out.add(4 * v), sum);
                    }
                    for i in 4 * vectors..rows {
                        *out.add(i) = accumulate(*out.add(i), *column.add(i) * bkj, k == 0);
                    }
                }
            }
        }
        for i in 0..rows {
            for j in 0..columns {
                c[i * columns + j] = accumulators[j * rows + i];
            }
        }
    }

    /// `MR` rows by four columns, one vector per row.
    ///
    /// # Safety
    ///
    /// Requires AVX2 and the tile to lie inside every operand.
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn quad_tile(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        inner: usize,
        columns: usize,
        i: usize,
        j: usize,
    ) {
        // SAFETY: all offsets are inside the operands per the tile contract.
        unsafe {
            let mut acc = [_mm256_setzero_pd(); MR];
            let a_ptr = a.as_ptr();
            let b_ptr = b.as_ptr();
            for k in 0..inner {
                let b_row = _mm256_loadu_pd(b_ptr.add(k * columns + j));
                for (r, sum) in acc.iter_mut().enumerate() {
                    let aik = _mm256_set1_pd(*a_ptr.add((i + r) * inner + k));
                    *sum = accumulate_lanes(*sum, _mm256_mul_pd(aik, b_row), k == 0);
                }
            }
            let c_ptr = c.as_mut_ptr();
            for (r, sum) in acc.iter().enumerate() {
                _mm256_storeu_pd(c_ptr.add((i + r) * columns + j), *sum);
            }
        }
    }

    /// `R` rows by `NR` columns of the product, accumulated in registers.
    ///
    /// # Safety
    ///
    /// Requires AVX2 and the tile to lie inside every operand.
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn wide_tile<const R: usize>(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        inner: usize,
        columns: usize,
        i: usize,
        j: usize,
    ) {
        // SAFETY: all offsets are inside the operands per the tile contract.
        unsafe {
            let mut lo = [_mm256_setzero_pd(); R];
            let mut hi = [_mm256_setzero_pd(); R];
            let a_ptr = a.as_ptr();
            let b_ptr = b.as_ptr();
            for k in 0..inner {
                let b_row = b_ptr.add(k * columns + j);
                let b_lo = _mm256_loadu_pd(b_row);
                let b_hi = _mm256_loadu_pd(b_row.add(4));
                for r in 0..R {
                    let aik = _mm256_set1_pd(*a_ptr.add((i + r) * inner + k));
                    lo[r] = accumulate_lanes(lo[r], _mm256_mul_pd(aik, b_lo), k == 0);
                    hi[r] = accumulate_lanes(hi[r], _mm256_mul_pd(aik, b_hi), k == 0);
                }
            }
            let c_ptr = c.as_mut_ptr();
            for r in 0..R {
                let c_row = c_ptr.add((i + r) * columns + j);
                _mm256_storeu_pd(c_row, lo[r]);
                _mm256_storeu_pd(c_row.add(4), hi[r]);
            }
        }
    }

    /// Fewer than four columns: four rows are formed together instead, with
    /// the operand column gathered from the rows and the `b` cell broadcast.
    ///
    /// # Safety
    ///
    /// Requires AVX2 and the stated extents.
    #[target_feature(enable = "avx2")]
    unsafe fn narrow(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        rows: usize,
        inner: usize,
        columns: usize,
    ) {
        let mut i = 0;
        while i + 4 <= rows {
            for j in 0..columns {
                // SAFETY: rows [i, i+4) and column j lie inside the operands.
                unsafe {
                    let a_ptr = a.as_ptr();
                    let mut acc = _mm256_setzero_pd();
                    for k in 0..inner {
                        let col = _mm256_set_pd(
                            *a_ptr.add((i + 3) * inner + k),
                            *a_ptr.add((i + 2) * inner + k),
                            *a_ptr.add((i + 1) * inner + k),
                            *a_ptr.add(i * inner + k),
                        );
                        let bkj = _mm256_set1_pd(*b.as_ptr().add(k * columns + j));
                        acc = accumulate_lanes(acc, _mm256_mul_pd(col, bkj), k == 0);
                    }
                    let mut lanes = [0.0f64; 4];
                    _mm256_storeu_pd(lanes.as_mut_ptr(), acc);
                    for (r, lane) in lanes.iter().enumerate() {
                        *c.as_mut_ptr().add((i + r) * columns + j) = *lane;
                    }
                }
            }
            i += 4;
        }
        // SAFETY: the remaining rows and all columns lie inside the operands.
        unsafe { scalar_block(c, a, b, (inner, columns), i..rows, 0) };
    }

    /// Rows `rows`, columns `[j0, columns)`, one output at a time
    /// in the reference order.
    ///
    /// # Safety
    ///
    /// The block must lie inside every operand.
    #[target_feature(enable = "avx2")]
    #[inline]
    unsafe fn scalar_block(
        c: &mut [f64],
        a: &[f64],
        b: &[f64],
        (inner, columns): (usize, usize),
        rows: std::ops::Range<usize>,
        j0: usize,
    ) {
        for r in rows {
            for j in j0..columns {
                // SAFETY: inside the block per the contract.
                let term = |k: usize| unsafe {
                    *a.get_unchecked(r * inner + k) * *b.get_unchecked(k * columns + j)
                };
                let sum = (0..inner).fold(0.0, |sum, k| accumulate(sum, term(k), k == 0));
                // SAFETY: inside the block per the contract.
                unsafe { *c.get_unchecked_mut(r * columns + j) = sum };
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The generated loop's order: each output from its first product, `k`
    /// ascending; an empty dot is `+0.0`.
    fn reference(a: &[f64], b: &[f64], (rows, inner, columns): (usize, usize, usize)) -> Vec<f64> {
        (0..rows * columns)
            .map(|output| {
                let (i, j) = (output / columns, output % columns);
                (0..inner)
                    .map(|k| a[i * inner + k] * b[k * columns + j])
                    .reduce(|sum, term| sum + term)
                    .unwrap_or(0.0)
            })
            .collect()
    }

    fn values(n: usize, seed: f64) -> Vec<f64> {
        (0..n)
            .map(|i| {
                ((i as f64 + seed) * 0.618_033_988_7).sin()
                    * 1e3_f64.powf((i % 7) as f64 / 3.0 - 1.0)
            })
            .collect()
    }

    #[test]
    fn the_kernel_is_bit_identical_to_the_generated_summation_order() {
        for shape in [
            (1, 7, 1),
            (3, 5, 1),
            (1, 4, 6),
            (17, 33, 9),
            (64, 128, 128),
            (64, 128, 2),
            (128, 64, 1),
            (5, 9, 2),
            (6, 7, 3),
            (9, 5, 13),
            (7, 3, 12),
            (2, 128, 128),
        ] {
            let a = values(shape.0 * shape.1, 0.3);
            let b = values(shape.1 * shape.2, 1.7);
            let mut c = vec![f64::NAN; shape.0 * shape.2];
            // SAFETY: each buffer holds exactly the stated extents.
            unsafe {
                rumoca_host_matmul_f64(
                    c.as_mut_ptr(),
                    a.as_ptr(),
                    b.as_ptr(),
                    shape.0 as u64,
                    shape.1 as u64,
                    shape.2 as u64,
                );
            }
            let want = reference(&a, &b, shape);
            assert!(
                c.iter().zip(&want).all(|(x, y)| x.to_bits() == y.to_bits()),
                "{shape:?}"
            );
        }
    }

    /// The transpose of a `rows × columns` matrix, as `columns × rows`.
    fn transposed(cells: &[f64], rows: usize, columns: usize) -> Vec<f64> {
        (0..rows * columns)
            .map(|output| cells[(output % rows) * columns + output / rows])
            .collect()
    }

    /// The product through the transposed-operand entry, `flags` saying which
    /// operands are stored transposed.
    fn stored_product(
        lhs: &[f64],
        rhs: &[f64],
        (rows, inner, columns): (usize, usize, usize),
        flags: u64,
    ) -> Vec<f64> {
        let mut c = vec![f64::NAN; rows * columns];
        // SAFETY: each buffer holds its stored extents.
        unsafe {
            rumoca_host_matmul_t_f64(
                c.as_mut_ptr(),
                lhs.as_ptr(),
                rhs.as_ptr(),
                rows as u64,
                inner as u64,
                columns as u64,
                flags,
            );
        }
        c
    }

    /// The transpose of a `rows × columns` matrix of `trailing`-cell elements.
    fn transposed_cells(
        source: &[u64],
        (rows, columns, trailing): (usize, usize, usize),
    ) -> Vec<u64> {
        (0..source.len())
            .map(|cell| {
                let (element, lane) = (cell / trailing, cell % trailing);
                let (column, row) = (element / rows, element % rows);
                source[(row * columns + column) * trailing + lane]
            })
            .collect()
    }

    fn bits_equal(got: &[f64], want: &[f64]) -> bool {
        got.len() == want.len()
            && got
                .iter()
                .zip(want)
                .all(|(x, y)| x.to_bits() == y.to_bits())
    }

    #[test]
    fn every_tile_path_and_remainder_keeps_the_summation_order() {
        // Packed tiles, the quad tile, the few-row sweep, column and row
        // remainders, odd and zero extents.
        for shape in [
            (4, 64, 8),
            (8, 16, 16),
            (12, 7, 24),
            (4, 3, 4),
            (128, 64, 4),
            (6, 9, 12),
            (3, 64, 128),
            (1, 64, 128),
            (2, 5, 7),
            (5, 0, 9),
            (0, 4, 4),
            (4, 4, 0),
        ] {
            let a = values(shape.0 * shape.1, 0.7);
            let b = values(shape.1 * shape.2, 2.3);
            let mut c = vec![f64::NAN; shape.0 * shape.2];
            // SAFETY: each buffer holds exactly the stated extents.
            unsafe {
                rumoca_host_matmul_f64(
                    c.as_mut_ptr(),
                    a.as_ptr(),
                    b.as_ptr(),
                    shape.0 as u64,
                    shape.1 as u64,
                    shape.2 as u64,
                );
            }
            assert!(bits_equal(&c, &reference(&a, &b, shape)), "{shape:?}");
        }
    }

    #[test]
    fn transposed_operands_are_read_in_place_with_the_same_order() {
        for shape in [
            (64, 128, 128),
            (128, 64, 128),
            (64, 4, 128),
            (128, 64, 4),
            (2, 64, 128),
            (64, 128, 2),
            (5, 7, 3),
            (8, 9, 10),
        ] {
            let (rows, inner, columns) = shape;
            let a = values(rows * inner, 1.1);
            let b = values(inner * columns, 3.9);
            let want = reference(&a, &b, shape);
            // Stored transposes: `inner × rows` and `columns × inner`.
            let a_stored = transposed(&a, rows, inner);
            let b_stored = transposed(&b, inner, columns);
            for (flags, lhs, rhs) in [
                (1, &a_stored, &b),
                (2, &a, &b_stored),
                (3, &a_stored, &b_stored),
            ] {
                let c = stored_product(lhs, rhs, shape, flags);
                assert!(bits_equal(&c, &want), "{shape:?} flags {flags}");
            }
        }
    }

    #[test]
    fn a_transposed_operand_that_aliases_the_output_is_read_first() {
        // `x := transpose(M) * x`, with x in the destination cells.
        let m = values(16, 0.4);
        let mut x = values(4, 5.5);
        let want = reference(&transposed(&m, 4, 4), &x, (4, 4, 1));
        // SAFETY: `x` holds 4 cells for both roles; `m` holds 16.
        unsafe { rumoca_host_matmul_t_f64(x.as_mut_ptr(), m.as_ptr(), x.as_ptr(), 4, 4, 1, 1) };
        assert!(bits_equal(&x, &want));
    }

    #[test]
    fn the_host_transpose_moves_whole_elements_and_tolerates_overlap() {
        for (rows, columns, trailing) in [(3, 5, 1), (16, 17, 1), (4, 6, 3), (1, 9, 2)] {
            let source: Vec<u64> = (0..rows * columns * trailing)
                .map(|cell| cell as u64 * 7 + 1)
                .collect();
            let mut target = vec![0u64; source.len()];
            // SAFETY: both buffers hold rows·columns·trailing cells.
            unsafe {
                rumoca_host_transpose_cells(
                    target.as_mut_ptr(),
                    source.as_ptr(),
                    rows as u64,
                    columns as u64,
                    trailing as u64,
                )
            };
            assert_eq!(target, transposed_cells(&source, (rows, columns, trailing)));
            // In place: the source is read out before any cell is written.
            let mut cells = source.clone();
            // SAFETY: one buffer of rows·columns·trailing cells in both roles.
            unsafe {
                rumoca_host_transpose_cells(
                    cells.as_mut_ptr(),
                    cells.as_ptr(),
                    rows as u64,
                    columns as u64,
                    trailing as u64,
                )
            };
            assert_eq!(cells, target);
        }
    }

    #[test]
    fn an_output_that_overlaps_an_operand_is_formed_before_it_is_written() {
        // `x := A * x` with x stored in the destination cells.
        let a = values(16, 0.9);
        let mut x = values(4, 2.1);
        let want = reference(&a, &x, (4, 4, 1));
        // SAFETY: `x` holds 4 cells for both roles; `a` holds 16.
        unsafe { rumoca_host_matmul_f64(x.as_mut_ptr(), a.as_ptr(), x.as_ptr(), 4, 4, 1) };
        assert!(x.iter().zip(&want).all(|(p, q)| p.to_bits() == q.to_bits()));
    }

    /// Ordinary values with every IEEE edge mixed in at fixed positions.
    fn edge_values(n: usize, seed: f64) -> Vec<f64> {
        let edges = [
            0.0,
            -0.0,
            f64::INFINITY,
            f64::NEG_INFINITY,
            f64::NAN,
            f64::MIN_POSITIVE / 8.0,
            f64::MAX,
        ];
        let mut cells = values(n, seed);
        for (index, cell) in cells.iter_mut().enumerate().filter(|(i, _)| i % 5 == 3) {
            *cell = edges[index % edges.len()];
        }
        cells
    }

    /// Bit equality, or for `exact == false` also NaN against NaN.
    fn same_bits(got: &[f64], want: &[f64], exact: bool) -> bool {
        let same =
            |x: &f64, y: &f64| x.to_bits() == y.to_bits() || (!exact && x.is_nan() && y.is_nan());
        got.len() == want.len() && got.iter().zip(want).all(|(x, y)| same(x, y))
    }

    #[test]
    fn the_portable_kernel_keeps_the_generated_summation_order() {
        // The fallback for hosts without AVX2, called directly: on an AVX2
        // host the dispatcher never selects it.
        for shape in [
            (1, 1, 1),
            (3, 5, 2),
            (4, 0, 3),
            (0, 3, 4),
            (7, 9, 13),
            (17, 33, 9),
            (64, 128, 2),
        ] {
            let (rows, inner, columns) = shape;
            // Ordinary values compare bit for bit; with IEEE edges mixed in,
            // a NaN result compares as NaN (`inf * 0` makes the default NaN,
            // whose payload is not the summation order under test).
            for (a, b, exact) in [
                (
                    values(rows * inner, 0.3),
                    values(inner * columns, 1.7),
                    true,
                ),
                (
                    edge_values(rows * inner, 0.9),
                    edge_values(inner * columns, 2.2),
                    false,
                ),
            ] {
                let mut portable = vec![f64::NAN; rows * columns];
                multiply_portable(&mut portable, &a, &b, shape);
                let want = reference(&a, &b, shape);
                assert!(same_bits(&portable, &want, exact), "{shape:?}");
                let mut dispatched = vec![f64::NAN; rows * columns];
                multiply(&mut dispatched, &a, &b, shape);
                assert!(
                    same_bits(&portable, &dispatched, exact),
                    "{shape:?} vs dispatch"
                );
            }
        }
    }

    /// Zeros of both signs and nonzero terms: rows `i % 3 == 0` of `a` are
    /// all `-1.0`, and columns `j % 3 == 0` of `b` all `+0.0`, so those
    /// outputs are all `-0.0` dots; the rest mix signed zeros and values.
    fn signed_zero_operands(rows: usize, inner: usize, columns: usize) -> (Vec<f64>, Vec<f64>) {
        let a = (0..rows * inner)
            .map(|cell| match ((cell / inner.max(1)) % 3, cell % 2) {
                (0, _) => -1.0,
                (1, 0) => -0.0,
                (1, _) => 0.0,
                _ => (cell as f64 * 0.37).sin(),
            })
            .collect();
        let b = (0..inner * columns)
            .map(|cell| match ((cell % columns.max(1)) % 3, cell % 2) {
                (0, _) => 0.0,
                (1, _) => -0.0,
                (_, 0) => -2.5,
                _ => 0.0,
            })
            .collect();
        (a, b)
    }

    #[test]
    fn every_path_starts_each_output_from_its_first_product() {
        // Every tile path, remainder, transposed entry and overlap, with
        // `-0.0` dots, mixed signed zeros, and empty dots that stay `+0.0`.
        for shape in [
            (4, 64, 8),
            (8, 16, 16),
            (12, 7, 24),
            (4, 3, 4),
            (128, 64, 4),
            (6, 9, 12),
            (3, 64, 128),
            (1, 64, 128),
            (2, 5, 7),
            (64, 128, 2),
            (9, 5, 13),
            (5, 7, 3),
            (8, 9, 10),
            (1, 1, 1),
            (5, 0, 9),
            (8, 0, 16),
        ] {
            let (rows, inner, columns) = shape;
            let (a, b) = signed_zero_operands(rows, inner, columns);
            let want = reference(&a, &b, shape);
            if inner == 0 {
                assert!(want.iter().all(|x| x.to_bits() == 0), "{shape:?}");
            } else {
                assert_eq!(want[0].to_bits(), (-0.0f64).to_bits(), "{shape:?}");
            }
            let mut c = vec![f64::NAN; rows * columns];
            // SAFETY: each buffer holds exactly the stated extents.
            unsafe {
                rumoca_host_matmul_f64(
                    c.as_mut_ptr(),
                    a.as_ptr(),
                    b.as_ptr(),
                    rows as u64,
                    inner as u64,
                    columns as u64,
                );
            }
            assert!(bits_equal(&c, &want), "{shape:?}");
            let a_stored = transposed(&a, rows, inner);
            let b_stored = transposed(&b, inner, columns);
            for (flags, lhs, rhs) in [
                (0, &a, &b),
                (1, &a_stored, &b),
                (2, &a, &b_stored),
                (3, &a_stored, &b_stored),
            ] {
                let c = stored_product(lhs, rhs, shape, flags);
                assert!(bits_equal(&c, &want), "{shape:?} flags {flags}");
            }
        }
        // `x := A * x` and `x := transpose(M) * x`, through the copy-out path.
        let (a, x) = signed_zero_operands(4, 4, 1);
        for flags in [0, 1] {
            let lhs = if flags == 1 {
                transposed(&a, 4, 4)
            } else {
                a.clone()
            };
            let want = reference(&a, &x, (4, 4, 1));
            let mut cells = x.clone();
            // SAFETY: `cells` holds 4 cells for both roles; `lhs` holds 16.
            unsafe {
                rumoca_host_matmul_t_f64(
                    cells.as_mut_ptr(),
                    lhs.as_ptr(),
                    cells.as_ptr(),
                    4,
                    4,
                    1,
                    flags,
                )
            };
            assert!(bits_equal(&cells, &want), "flags {flags}");
        }
    }
}
