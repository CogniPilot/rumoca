#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

static int rumoca_solve_linear_impl(
        const double *Ain, const double *bin, long long n, double *x) {
    size_t un;
    size_t matrix_count;
    double *A;
    size_t element;
    long long i, j, row, col;

    if (n <= 0) return 0;
    if ((uint64_t)n > SIZE_MAX) return 0;
    un = (size_t)n;
    if (un > SIZE_MAX / un || un * un > SIZE_MAX / sizeof(double)) return 0;
    matrix_count = un * un;
    A = (double *)malloc(matrix_count * sizeof(double));
    if (A == NULL) return 0;

    for (element = 0; element < matrix_count; element++) A[element] = Ain[element];
    for (i = 0; i < n; i++) x[i] = bin[i];

    /* Forward elimination with partial pivoting. */
    for (col = 0; col < n; col++) {
        long long pivot = col;
        double max_val = fabs(A[col * n + col]);
        for (row = col + 1; row < n; row++) {
            double value = fabs(A[row * n + col]);
            if (value > max_val) {
                max_val = value;
                pivot = row;
            }
        }
        if (max_val == 0.0 || !isfinite(max_val)) {
            free(A);
            return 0;
        }
        for (j = 0; j < n; j++) {
            double value = A[col * n + j];
            A[col * n + j] = A[pivot * n + j];
            A[pivot * n + j] = value;
        }
        {
            double value = x[col];
            x[col] = x[pivot];
            x[pivot] = value;
        }
        for (row = col + 1; row < n; row++) {
            double factor = A[row * n + col] / A[col * n + col];
            for (j = col; j < n; j++) A[row * n + j] -= factor * A[col * n + j];
            x[row] -= factor * x[col];
        }
    }

    /* Back substitution. */
    for (i = n - 1; i >= 0; i--) {
        x[i] /= A[i * n + i];
        for (j = i - 1; j >= 0; j--) x[j] -= A[j * n + i] * x[i];
    }
    free(A);
    return 1;
}

/* A_ptr, b_ptr, x_ptr are row-major arrays passed as pointer-integers.
   Computes the complete solution once for a native LinSolve node. */
void rumoca_solve_linear(
        long long A_ptr, long long b_ptr, long long n, long long x_ptr) {
    const double *Ain = (const double *)(size_t)A_ptr;
    const double *bin = (const double *)(size_t)b_ptr;
    double *x = (double *)(size_t)x_ptr;
    long long i;
    if (!rumoca_solve_linear_impl(Ain, bin, n, x)) {
        for (i = 0; i < n; i++) x[i] = NAN;
    }
}

/* Scalar compatibility ABI used by LinearSolveComponent rows. */
double rumoca_solve_linear_component(
        long long A_ptr, long long b_ptr, long long n, long long comp) {
    const double *Ain = (const double *)(size_t)A_ptr;
    const double *bin = (const double *)(size_t)b_ptr;
    double *x;
    double result;
    if (n <= 0 || comp < 0 || comp >= n || (size_t)n > SIZE_MAX / sizeof(double)) {
        return NAN;
    }
    x = (double *)malloc((size_t)n * sizeof(double));
    if (x == NULL) return NAN;
    if (!rumoca_solve_linear_impl(Ain, bin, n, x)) {
        free(x);
        return NAN;
    }
    result = x[comp];
    free(x);
    return result;
}
