//! Host kernel for dense `Real` (binary64) matrix products called from JIT code.
//!
//! Generated scalar code forms each output as `((0 + a[i,0]·b[0,j]) + a[i,1]·b[1,j]) + …`
//! with separate multiplies and adds. This kernel keeps exactly that sequence
//! per output (initialized to `+0.0`, `k` ascending, no fused multiply-add), so
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
    let (rows, inner, columns) = (rows as usize, inner as usize, columns as usize);
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

#[cfg(test)]
thread_local! {
    static MATMUL_CALLS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Calls of the kernel on this thread since the last take: an invocation
/// witness the scalar loop cannot produce.
#[cfg(test)]
pub(in crate::emit) fn take_matmul_calls() -> usize {
    MATMUL_CALLS.with(|calls| calls.replace(0))
}

fn multiply(c: &mut [f64], a: &[f64], b: &[f64], (rows, inner, columns): (usize, usize, usize)) {
    if columns == 0 {
        return;
    }
    for (i, out) in c.chunks_exact_mut(columns).enumerate().take(rows) {
        out.fill(0.0);
        for k in 0..inner {
            let aik = a[i * inner + k];
            let b_row = &b[k * columns..(k + 1) * columns];
            for (cij, bkj) in out.iter_mut().zip(b_row) {
                *cij += aik * *bkj;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The generated loop's order: each output from `+0.0`, `k` ascending.
    fn reference(a: &[f64], b: &[f64], (rows, inner, columns): (usize, usize, usize)) -> Vec<f64> {
        (0..rows * columns)
            .map(|output| {
                let (i, j) = (output / columns, output % columns);
                (0..inner).fold(0.0, |sum, k| sum + a[i * inner + k] * b[k * columns + j])
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
        for shape in [(1, 7, 1), (3, 5, 1), (1, 4, 6), (17, 33, 9), (64, 128, 128)] {
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
}
