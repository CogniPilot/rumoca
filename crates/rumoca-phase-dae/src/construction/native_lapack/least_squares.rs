//! LAPACK `dgelsy` with one right-hand side as straight-line arithmetic over
//! translation-time extents (SPEC_0040 DAE-C29).
//!
//! The steps are those of the reference driver:
//!
//! 1. `dgeqp3` (unblocked `dlaqp2`): QR factorization with column pivoting.
//!    Step `i` moves the first column of largest partial norm to position
//!    `i`, reduces it with a Householder reflector (`dlarfg`), applies the
//!    reflector to the later columns, and downdates their partial norms,
//!    recomputing a norm whose downdate lost more than `sqrt(eps)`.
//! 2. The effective rank: the leading `k` columns of `R` stay while the
//!    incremental condition estimates (`dlaic1`) of the largest and smallest
//!    singular values satisfy `smax * rcond <= smin`; a zero `R(1,1)` is
//!    rank 0 and a zero solution.
//! 3. `Q**T * b`, the complete orthogonal factorization (`dtzrzf`) of the
//!    leading `rank` rows when `rank < n`, the triangular solve, the zero
//!    tail, `Z**T`, and the inverse column permutation.
//!
//! The rank decides how many rows step 3 uses, so step 3 is built for every
//! rank and the computed rank selects one. Reads past the selected rank and
//! divisions on branches the driver does not take are kept finite by
//! guarding their divisors; they never reach a selected value. The driver's
//! scaling of matrices whose largest entry is outside `[smlnum, bignum]`
//! changes only rounding and is not reproduced.

use super::*;

/// `dlamch('Epsilon')`: the relative machine precision of rounding.
const EPSILON: f64 = f64::EPSILON * 0.5;

/// A value of the incremental condition estimator: the new estimate and the
/// sine and cosine that extend its vector.
type Estimate<'dae> = (dae::ExprId<'dae>, dae::ExprId<'dae>, dae::ExprId<'dae>);

/// The pivoted QR factorization of the matrix argument.
struct PivotedQr<'dae> {
    /// Column `j` of the factored matrix: `R` on and above the diagonal,
    /// reflector tails below it.
    columns: Vec<Vec<dae::ExprId<'dae>>>,
    /// The 1-based original column at each position.
    permutation: Vec<dae::ExprId<'dae>>,
    /// The reflector scalar of each step.
    taus: Vec<dae::ExprId<'dae>>,
}

impl<'dae> LapackBuilder<'_, '_, 'dae> {
    /// The `dgelsy` solution vector (of the storage extent of `rhs`) and
    /// effective rank of `matrix`.
    pub(super) fn least_squares(
        &mut self,
        matrix: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        rcond: dae::ExprId<'dae>,
    ) -> Result<(dae::ExprId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError> {
        let dimensions = self
            .expressions
            .value_type(matrix, self.provenance)?
            .dimensions()
            .to_vec();
        let [rows, columns] = [dimensions[0] as usize, dimensions[1] as usize];
        let storage = self.extent(rhs)?;
        let smallest = rows.min(columns);
        let extent = rows.max(columns);
        let mut matrix_columns = Vec::with_capacity(columns);
        for column in 1..=columns {
            let mut entries = Vec::with_capacity(rows);
            for row in 1..=rows {
                let entry = self.at_index(matrix, row)?;
                entries.push(self.at_index(entry, column)?);
            }
            matrix_columns.push(entries);
        }
        let initial = (1..=storage)
            .map(|index| self.at_index(rhs, index))
            .collect::<Result<Vec<_>, _>>()?;
        let qr = self.pivoted_qr(matrix_columns, smallest)?;
        let rank = self.effective_rank(&qr.columns, smallest, rcond)?;
        let transformed = self.apply_qt(&qr, &initial[..rows])?;
        let mut right = transformed[..smallest].to_vec();
        right.extend_from_slice(&initial[smallest..columns]);
        let mut candidates = Vec::with_capacity(smallest);
        let mut rank_flags = Vec::with_capacity(smallest);
        for candidate in 1..=smallest {
            rank_flags.push(self.is_integer(rank, candidate as i64)?);
            let solution = self.rank_solution(&qr.columns, candidate, &right)?;
            candidates.push(self.unpermute(&qr.permutation, &solution)?);
        }
        let zero = self.real(0.0)?;
        let mut entries = Vec::with_capacity(storage);
        for index in 0..storage {
            let selected = if index < columns {
                let branches = rank_flags
                    .iter()
                    .zip(&candidates)
                    .map(|(flag, candidate)| (*flag, candidate[index]))
                    .collect::<Vec<_>>();
                self.expressions
                    .at(self.provenance)
                    .conditional(branches, zero)?
            } else if index < extent {
                // Rows past `n` keep `Q**T * b` unless the rank is zero.
                let deficient = self.is_integer(rank, 0)?;
                self.conditional(deficient, zero, transformed[index])?
            } else {
                initial[index]
            };
            entries.push(selected);
        }
        let solution = self.expressions.at(self.provenance).array(entries)?;
        Ok((solution, rank))
    }

    /// `dlaqp2` over all columns, every column free.
    fn pivoted_qr(
        &mut self,
        mut columns: Vec<Vec<dae::ExprId<'dae>>>,
        steps: usize,
    ) -> Result<PivotedQr<'dae>, dae::DaeConstructionError> {
        let rows = columns[0].len();
        let mut permutation = (1..=columns.len())
            .map(|column| self.integer(column))
            .collect::<Result<Vec<_>, _>>()?;
        let mut norms = columns
            .iter()
            .map(|column| self.norm(column))
            .collect::<Result<Vec<_>, _>>()?;
        let mut reference = norms.clone();
        let mut taus = Vec::with_capacity(steps);
        let zero = self.real(0.0)?;
        let tolerance = self.real(EPSILON.sqrt())?;
        for step in 0..steps {
            let largest = self.maximum(&norms[step..])?;
            let taken = self.first_largest(&norms[step..], largest)?;
            self.swap_into(&mut columns[step..], &taken)?;
            self.swap_into(&mut permutation[step..], &taken)?;
            // The departing partial norms move to the vacated position.
            for (offset, flag) in taken.iter().enumerate().skip(1) {
                let index = step + offset;
                norms[index] = self.conditional(*flag, norms[step], norms[index])?;
                reference[index] = self.conditional(*flag, reference[step], reference[index])?;
            }
            let tau = self.reflect(&mut columns[step][step..])?;
            taus.push(tau);
            let (pivot, later) = columns.split_at_mut(step + 1);
            let reflector = &pivot[step][step..];
            for column in later.iter_mut() {
                self.apply_reflector(tau, reflector, &mut column[step..])?;
            }
            for (offset, column) in later.iter().enumerate() {
                let index = step + 1 + offset;
                let (norm, base) = self.downdate(
                    (norms[index], reference[index]),
                    column[step],
                    &column[step + 1..rows],
                    (zero, tolerance),
                )?;
                norms[index] = norm;
                reference[index] = base;
            }
        }
        Ok(PivotedQr {
            columns,
            permutation,
            taus,
        })
    }

    /// Move the taken entry of `values` to its front, the front entry to the
    /// taken position (LAPACK column swap).
    fn swap_into<T: SwapValue<'dae>>(
        &mut self,
        values: &mut [T],
        taken: &[dae::ExprId<'dae>],
    ) -> Result<(), dae::DaeConstructionError> {
        let front = values[0].clone();
        let mut chosen = front.clone();
        for (offset, flag) in taken.iter().enumerate().skip(1) {
            chosen = T::select(self, *flag, &values[offset], &chosen)?;
            values[offset] = T::select(self, *flag, &front, &values[offset])?;
        }
        values[0] = chosen;
        Ok(())
    }

    /// One partial-norm update of `dlaqp2` after a step leaves `leading` in
    /// the reduced row and `tail` below it.
    fn downdate(
        &mut self,
        (norm, reference): (dae::ExprId<'dae>, dae::ExprId<'dae>),
        leading: dae::ExprId<'dae>,
        tail: &[dae::ExprId<'dae>],
        (zero, tolerance): (dae::ExprId<'dae>, dae::ExprId<'dae>),
    ) -> Result<(dae::ExprId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError> {
        let one = self.real(1.0)?;
        let magnitude = self.abs(leading)?;
        let safe_norm = self.nonzero(norm)?;
        let ratio = self.op(dae::BinaryOperator::Divide, magnitude, safe_norm)?;
        let squared = self.op(dae::BinaryOperator::Multiply, ratio, ratio)?;
        let remaining = self.op(dae::BinaryOperator::Subtract, one, squared)?;
        let remaining = self.max(remaining, zero)?;
        let safe_reference = self.nonzero(reference)?;
        let drift = self.op(dae::BinaryOperator::Divide, norm, safe_reference)?;
        let drift = self.op(dae::BinaryOperator::Multiply, drift, drift)?;
        let drift = self.op(dae::BinaryOperator::Multiply, remaining, drift)?;
        let recompute = self.op(dae::BinaryOperator::LessEqual, drift, tolerance)?;
        let fresh = if tail.is_empty() {
            zero
        } else {
            self.norm(tail)?
        };
        let root = self.sqrt(remaining)?;
        let scaled = self.op(dae::BinaryOperator::Multiply, norm, root)?;
        let updated = self.conditional(recompute, fresh, scaled)?;
        let base = self.conditional(recompute, fresh, reference)?;
        let present = self.op(dae::BinaryOperator::NotEqual, norm, zero)?;
        Ok((
            self.conditional(present, updated, norm)?,
            self.conditional(present, base, reference)?,
        ))
    }

    /// `dlarfg` on `values = [alpha, x]`: overwrite it with `[beta, v]` and
    /// return `tau`, so `(I - tau [1; v] [1; v]**T) [alpha; x] = [beta; 0]`.
    fn reflect(
        &mut self,
        values: &mut [dae::ExprId<'dae>],
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        let Some((&alpha, tail)) = values.split_first() else {
            return Ok(zero);
        };
        if tail.is_empty() {
            return Ok(zero);
        }
        let tail_norm = self.norm(tail)?;
        let trivial = self.op(dae::BinaryOperator::Equal, tail_norm, zero)?;
        let alpha_squared = self.op(dae::BinaryOperator::Multiply, alpha, alpha)?;
        let tail_squared = self.op(dae::BinaryOperator::Multiply, tail_norm, tail_norm)?;
        let total = self.op(dae::BinaryOperator::Add, alpha_squared, tail_squared)?;
        let length = self.sqrt(total)?;
        let sign = self.sign(alpha)?;
        let signed = self.op(dae::BinaryOperator::Multiply, sign, length)?;
        let beta = self
            .expressions
            .at(self.provenance)
            .unary(dae::UnaryOperator::Negate, signed)?;
        let safe_beta = self.nonzero(beta)?;
        let difference = self.op(dae::BinaryOperator::Subtract, beta, alpha)?;
        let ratio = self.op(dae::BinaryOperator::Divide, difference, safe_beta)?;
        let tau = self.conditional(trivial, zero, ratio)?;
        let denominator = self.op(dae::BinaryOperator::Subtract, alpha, beta)?;
        let denominator = self.nonzero(denominator)?;
        for value in values[1..].iter_mut() {
            let scaled = self.op(dae::BinaryOperator::Divide, *value, denominator)?;
            *value = self.conditional(trivial, *value, scaled)?;
        }
        values[0] = self.conditional(trivial, alpha, beta)?;
        Ok(tau)
    }

    /// `target - tau * (v**T target) * v` for `v = [1; reflector[1..]]`.
    fn apply_reflector(
        &mut self,
        tau: dae::ExprId<'dae>,
        reflector: &[dae::ExprId<'dae>],
        target: &mut [dae::ExprId<'dae>],
    ) -> Result<(), dae::DaeConstructionError> {
        let mut projection = target[0];
        for (component, value) in reflector[1..].iter().zip(&target[1..]) {
            let product = self.op(dae::BinaryOperator::Multiply, *component, *value)?;
            projection = self.op(dae::BinaryOperator::Add, projection, product)?;
        }
        let weight = self.op(dae::BinaryOperator::Multiply, tau, projection)?;
        target[0] = self.op(dae::BinaryOperator::Subtract, target[0], weight)?;
        for (component, value) in reflector[1..].iter().zip(target[1..].iter_mut()) {
            let product = self.op(dae::BinaryOperator::Multiply, weight, *component)?;
            *value = self.op(dae::BinaryOperator::Subtract, *value, product)?;
        }
        Ok(())
    }

    /// The `dgelsy` effective rank: 0 for a zero `R(1,1)`, else the count of
    /// leading columns the incremental condition estimate keeps.
    fn effective_rank(
        &mut self,
        factored: &[Vec<dae::ExprId<'dae>>],
        smallest: usize,
        rcond: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        let one = self.real(1.0)?;
        let mut largest = self.abs(factored[0][0])?;
        let mut least = largest;
        let mut continuing = self.op(dae::BinaryOperator::NotEqual, largest, zero)?;
        let none = self.integer(0)?;
        let first = self.integer(1)?;
        let mut rank = self.conditional(continuing, first, none)?;
        let mut least_vector = vec![one];
        let mut largest_vector = vec![one];
        for kept in 1..smallest {
            let column = &factored[kept][..kept];
            let gamma = factored[kept][kept];
            let alpha_least = self.dot(&least_vector, column)?;
            let alpha_largest = self.dot(&largest_vector, column)?;
            let (least_next, sine_least, cosine_least) =
                self.estimate_least(alpha_least, least, gamma)?;
            let (largest_next, sine_largest, cosine_largest) =
                self.estimate_largest(alpha_largest, largest, gamma)?;
            let scaled = self.op(dae::BinaryOperator::Multiply, largest_next, rcond)?;
            let accepted = self.op(dae::BinaryOperator::LessEqual, scaled, least_next)?;
            continuing = self.op(dae::BinaryOperator::And, continuing, accepted)?;
            let count = self.integer(kept + 1)?;
            rank = self.conditional(continuing, count, rank)?;
            // Past the first rejection the estimates are no longer read.
            for value in &mut least_vector {
                *value = self.op(dae::BinaryOperator::Multiply, *value, sine_least)?;
            }
            for value in &mut largest_vector {
                *value = self.op(dae::BinaryOperator::Multiply, *value, sine_largest)?;
            }
            least_vector.push(cosine_least);
            largest_vector.push(cosine_largest);
            least = least_next;
            largest = largest_next;
        }
        Ok(rank)
    }

    /// `dlaic1` with `JOB = 1`: extend an estimate `sest` of the largest
    /// singular value by a column with projection `alpha` and diagonal
    /// `gamma`.
    fn estimate_largest(
        &mut self,
        alpha: dae::ExprId<'dae>,
        sest: dae::ExprId<'dae>,
        gamma: dae::ExprId<'dae>,
    ) -> Result<Estimate<'dae>, dae::DaeConstructionError> {
        let cases = self.estimate_cases(alpha, sest, gamma)?;
        let zero = self.real(0.0)?;
        let one = self.real(1.0)?;
        let half = self.real(0.5)?;
        let EstimateCases {
            absalp,
            absgam,
            absest,
        } = cases;
        // sest == 0
        let s1 = self.max(absgam, absalp)?;
        let vanishing = self.op(dae::BinaryOperator::Equal, s1, zero)?;
        let safe_s1 = self.nonzero(s1)?;
        let s = self.op(dae::BinaryOperator::Divide, alpha, safe_s1)?;
        let c = self.op(dae::BinaryOperator::Divide, gamma, safe_s1)?;
        let (s, c, tmp) = self.normalize(s, c)?;
        let estimate = self.op(dae::BinaryOperator::Multiply, s1, tmp)?;
        let zero_case = (
            self.conditional(vanishing, zero, estimate)?,
            self.conditional(vanishing, zero, s)?,
            self.conditional(vanishing, one, c)?,
        );
        // absgam <= eps * absest
        let tmp = self.max(absest, absalp)?;
        let safe_tmp = self.nonzero(tmp)?;
        let r1 = self.op(dae::BinaryOperator::Divide, absest, safe_tmp)?;
        let r2 = self.op(dae::BinaryOperator::Divide, absalp, safe_tmp)?;
        let length = self.hypot(r1, r2)?;
        let gamma_case = (
            self.op(dae::BinaryOperator::Multiply, tmp, length)?,
            one,
            zero,
        );
        // absalp <= eps * absest
        let gamma_small = self.op(dae::BinaryOperator::LessEqual, absgam, absest)?;
        let alpha_case =
            self.select_estimate(gamma_small, (absest, one, zero), (absgam, zero, one))?;
        // absest <= eps * max(absalp, absgam)
        let below = self.op(dae::BinaryOperator::LessEqual, absgam, absalp)?;
        let safe_alp = self.nonzero(absalp)?;
        let safe_gam = self.nonzero(absgam)?;
        let ta = self.op(dae::BinaryOperator::Divide, absgam, safe_alp)?;
        let sa = self.hypot(one, ta)?;
        let gamma_over = self.op(dae::BinaryOperator::Divide, gamma, safe_alp)?;
        let alpha_sign = self.sign(alpha)?;
        let first = (
            self.op(dae::BinaryOperator::Multiply, absalp, sa)?,
            self.op(dae::BinaryOperator::Divide, alpha_sign, sa)?,
            self.op(dae::BinaryOperator::Divide, gamma_over, sa)?,
        );
        let tb = self.op(dae::BinaryOperator::Divide, absalp, safe_gam)?;
        let cb = self.hypot(one, tb)?;
        let alpha_over = self.op(dae::BinaryOperator::Divide, alpha, safe_gam)?;
        let gamma_sign = self.sign(gamma)?;
        let second = (
            self.op(dae::BinaryOperator::Multiply, absgam, cb)?,
            self.op(dae::BinaryOperator::Divide, alpha_over, cb)?,
            self.op(dae::BinaryOperator::Divide, gamma_sign, cb)?,
        );
        let estimate_case = self.select_estimate(below, first, second)?;
        // general case
        let safe_est = self.nonzero(absest)?;
        let zeta1 = self.op(dae::BinaryOperator::Divide, alpha, safe_est)?;
        let zeta2 = self.op(dae::BinaryOperator::Divide, gamma, safe_est)?;
        let z1 = self.op(dae::BinaryOperator::Multiply, zeta1, zeta1)?;
        let z2 = self.op(dae::BinaryOperator::Multiply, zeta2, zeta2)?;
        let b = self.op(dae::BinaryOperator::Subtract, one, z1)?;
        let b = self.op(dae::BinaryOperator::Subtract, b, z2)?;
        let b = self.op(dae::BinaryOperator::Multiply, b, half)?;
        let bb = self.op(dae::BinaryOperator::Multiply, b, b)?;
        let radicand = self.op(dae::BinaryOperator::Add, bb, z1)?;
        let root = self.sqrt(radicand)?;
        let positive = self.op(dae::BinaryOperator::Greater, b, zero)?;
        let denominator = self.op(dae::BinaryOperator::Add, b, root)?;
        let denominator = self.nonzero(denominator)?;
        let t_positive = self.op(dae::BinaryOperator::Divide, z1, denominator)?;
        let t_other = self.op(dae::BinaryOperator::Subtract, root, b)?;
        let t = self.conditional(positive, t_positive, t_other)?;
        let safe_t = self.nonzero(t)?;
        let sine = self.op(dae::BinaryOperator::Divide, zeta1, safe_t)?;
        let sine = self.negate(sine)?;
        let one_plus_t = self.op(dae::BinaryOperator::Add, one, t)?;
        let safe_one_plus_t = self.nonzero(one_plus_t)?;
        let cosine = self.op(dae::BinaryOperator::Divide, zeta2, safe_one_plus_t)?;
        let cosine = self.negate(cosine)?;
        let (s, c, _) = self.normalize(sine, cosine)?;
        let growth = self.max(one_plus_t, zero)?;
        let growth = self.sqrt(growth)?;
        let general = (
            self.op(dae::BinaryOperator::Multiply, growth, absest)?,
            s,
            c,
        );
        self.dispatch_estimate(
            (sest, absalp, absgam, absest),
            [zero_case, gamma_case, alpha_case, estimate_case, general],
        )
    }

    /// `dlaic1` with `JOB = 2`: extend an estimate `sest` of the smallest
    /// singular value.
    fn estimate_least(
        &mut self,
        alpha: dae::ExprId<'dae>,
        sest: dae::ExprId<'dae>,
        gamma: dae::ExprId<'dae>,
    ) -> Result<Estimate<'dae>, dae::DaeConstructionError> {
        let EstimateCases {
            absalp,
            absgam,
            absest,
        } = self.estimate_cases(alpha, sest, gamma)?;
        let zero = self.real(0.0)?;
        let one = self.real(1.0)?;
        // sest == 0
        let s1 = self.max(absgam, absalp)?;
        let vanishing = self.op(dae::BinaryOperator::Equal, s1, zero)?;
        let minus_gamma = self.negate(gamma)?;
        let sine = self.conditional(vanishing, one, minus_gamma)?;
        let cosine = self.conditional(vanishing, zero, alpha)?;
        let sine_magnitude = self.abs(sine)?;
        let cosine_magnitude = self.abs(cosine)?;
        let scale = self.max(sine_magnitude, cosine_magnitude)?;
        let scale = self.nonzero(scale)?;
        let s = self.op(dae::BinaryOperator::Divide, sine, scale)?;
        let c = self.op(dae::BinaryOperator::Divide, cosine, scale)?;
        let (s, c, _) = self.normalize(s, c)?;
        let zero_case = (zero, s, c);
        // absgam <= eps * absest
        let gamma_case = (absgam, zero, one);
        // absalp <= eps * absest
        let gamma_small = self.op(dae::BinaryOperator::LessEqual, absgam, absest)?;
        let alpha_case =
            self.select_estimate(gamma_small, (absgam, zero, one), (absest, one, zero))?;
        // absest <= eps * max(absalp, absgam)
        let below = self.op(dae::BinaryOperator::LessEqual, absgam, absalp)?;
        let safe_alp = self.nonzero(absalp)?;
        let safe_gam = self.nonzero(absgam)?;
        let ta = self.op(dae::BinaryOperator::Divide, absgam, safe_alp)?;
        let ca = self.hypot(one, ta)?;
        let ratio = self.op(dae::BinaryOperator::Divide, ta, ca)?;
        let gamma_over = self.op(dae::BinaryOperator::Divide, gamma, safe_alp)?;
        let gamma_over = self.op(dae::BinaryOperator::Divide, gamma_over, ca)?;
        let alpha_sign = self.sign(alpha)?;
        let first = (
            self.op(dae::BinaryOperator::Multiply, absest, ratio)?,
            self.negate(gamma_over)?,
            self.op(dae::BinaryOperator::Divide, alpha_sign, ca)?,
        );
        let tb = self.op(dae::BinaryOperator::Divide, absalp, safe_gam)?;
        let sb = self.hypot(one, tb)?;
        let gamma_sign = self.sign(gamma)?;
        let gamma_sign = self.op(dae::BinaryOperator::Divide, gamma_sign, sb)?;
        let alpha_over = self.op(dae::BinaryOperator::Divide, alpha, safe_gam)?;
        let second = (
            self.op(dae::BinaryOperator::Divide, absest, sb)?,
            self.negate(gamma_sign)?,
            self.op(dae::BinaryOperator::Divide, alpha_over, sb)?,
        );
        let estimate_case = self.select_estimate(below, first, second)?;
        let general = self.least_general(alpha, gamma, absest)?;
        self.dispatch_estimate(
            (sest, absalp, absgam, absest),
            [zero_case, gamma_case, alpha_case, estimate_case, general],
        )
    }

    /// The general case of `dlaic1` with `JOB = 2`: the smaller root of the
    /// secular equation of the extended triangle.
    fn least_general(
        &mut self,
        alpha: dae::ExprId<'dae>,
        gamma: dae::ExprId<'dae>,
        absest: dae::ExprId<'dae>,
    ) -> Result<Estimate<'dae>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        let one = self.real(1.0)?;
        let half = self.real(0.5)?;
        let safe_est = self.nonzero(absest)?;
        let zeta1 = self.op(dae::BinaryOperator::Divide, alpha, safe_est)?;
        let zeta2 = self.op(dae::BinaryOperator::Divide, gamma, safe_est)?;
        let z1 = self.op(dae::BinaryOperator::Multiply, zeta1, zeta1)?;
        let z2 = self.op(dae::BinaryOperator::Multiply, zeta2, zeta2)?;
        let cross = self.op(dae::BinaryOperator::Multiply, zeta1, zeta2)?;
        let cross = self.abs(cross)?;
        let norm_first = self.op(dae::BinaryOperator::Add, one, z1)?;
        let norm_first = self.op(dae::BinaryOperator::Add, norm_first, cross)?;
        let norm_second = self.op(dae::BinaryOperator::Add, cross, z2)?;
        let norma = self.max(norm_first, norm_second)?;
        let two = self.real(2.0)?;
        let difference = self.op(dae::BinaryOperator::Subtract, zeta1, zeta2)?;
        let sum = self.op(dae::BinaryOperator::Add, zeta1, zeta2)?;
        let test = self.op(dae::BinaryOperator::Multiply, two, difference)?;
        let test = self.op(dae::BinaryOperator::Multiply, test, sum)?;
        let test = self.op(dae::BinaryOperator::Add, one, test)?;
        let floor = self.real(4.0 * EPSILON * EPSILON)?;
        let floor = self.op(dae::BinaryOperator::Multiply, floor, norma)?;
        // test >= 0: root near sest
        let squares = self.op(dae::BinaryOperator::Add, z1, z2)?;
        let ba = self.op(dae::BinaryOperator::Add, squares, one)?;
        let ba = self.op(dae::BinaryOperator::Multiply, ba, half)?;
        let baba = self.op(dae::BinaryOperator::Multiply, ba, ba)?;
        let gap = self.op(dae::BinaryOperator::Subtract, baba, z2)?;
        let gap = self.abs(gap)?;
        let gap = self.sqrt(gap)?;
        let denominator = self.op(dae::BinaryOperator::Add, ba, gap)?;
        let denominator = self.nonzero(denominator)?;
        let ta = self.op(dae::BinaryOperator::Divide, z2, denominator)?;
        let one_minus = self.op(dae::BinaryOperator::Subtract, one, ta)?;
        let one_minus = self.nonzero(one_minus)?;
        let sine_a = self.op(dae::BinaryOperator::Divide, zeta1, one_minus)?;
        let safe_ta = self.nonzero(ta)?;
        let cosine_a = self.op(dae::BinaryOperator::Divide, zeta2, safe_ta)?;
        let cosine_a = self.negate(cosine_a)?;
        let shift_a = self.op(dae::BinaryOperator::Add, ta, floor)?;
        // test < 0: root near gamma
        let bb = self.op(dae::BinaryOperator::Subtract, squares, one)?;
        let bb = self.op(dae::BinaryOperator::Multiply, bb, half)?;
        let bbbb = self.op(dae::BinaryOperator::Multiply, bb, bb)?;
        let radicand = self.op(dae::BinaryOperator::Add, bbbb, z1)?;
        let root = self.sqrt(radicand)?;
        let nonnegative = self.op(dae::BinaryOperator::GreaterEqual, bb, zero)?;
        let denominator = self.op(dae::BinaryOperator::Add, bb, root)?;
        let denominator = self.nonzero(denominator)?;
        let tb_positive = self.op(dae::BinaryOperator::Divide, z1, denominator)?;
        let tb_positive = self.negate(tb_positive)?;
        let tb_other = self.op(dae::BinaryOperator::Subtract, bb, root)?;
        let tb = self.conditional(nonnegative, tb_positive, tb_other)?;
        let safe_tb = self.nonzero(tb)?;
        let sine_b = self.op(dae::BinaryOperator::Divide, zeta1, safe_tb)?;
        let sine_b = self.negate(sine_b)?;
        let one_plus = self.op(dae::BinaryOperator::Add, one, tb)?;
        let safe_one_plus = self.nonzero(one_plus)?;
        let cosine_b = self.op(dae::BinaryOperator::Divide, zeta2, safe_one_plus)?;
        let cosine_b = self.negate(cosine_b)?;
        let shift_b = self.op(dae::BinaryOperator::Add, one_plus, floor)?;
        let near_estimate = self.op(dae::BinaryOperator::GreaterEqual, test, zero)?;
        let sine = self.conditional(near_estimate, sine_a, sine_b)?;
        let cosine = self.conditional(near_estimate, cosine_a, cosine_b)?;
        let shift = self.conditional(near_estimate, shift_a, shift_b)?;
        let shift = self.max(shift, zero)?;
        let shift = self.sqrt(shift)?;
        let (s, c, _) = self.normalize(sine, cosine)?;
        Ok((self.op(dae::BinaryOperator::Multiply, shift, absest)?, s, c))
    }

    fn estimate_cases(
        &mut self,
        alpha: dae::ExprId<'dae>,
        sest: dae::ExprId<'dae>,
        gamma: dae::ExprId<'dae>,
    ) -> Result<EstimateCases<'dae>, dae::DaeConstructionError> {
        Ok(EstimateCases {
            absalp: self.abs(alpha)?,
            absgam: self.abs(gamma)?,
            absest: self.abs(sest)?,
        })
    }

    /// The `dlaic1` case order: `sest = 0`, a negligible `gamma`, a
    /// negligible `alpha`, a negligible `sest`, and the general case.
    fn dispatch_estimate(
        &mut self,
        (sest, absalp, absgam, absest): (
            dae::ExprId<'dae>,
            dae::ExprId<'dae>,
            dae::ExprId<'dae>,
            dae::ExprId<'dae>,
        ),
        [zero_case, gamma_case, alpha_case, estimate_case, general]: [Estimate<'dae>; 5],
    ) -> Result<Estimate<'dae>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        let epsilon = self.real(EPSILON)?;
        let vanishing = self.op(dae::BinaryOperator::Equal, sest, zero)?;
        let scaled_est = self.op(dae::BinaryOperator::Multiply, epsilon, absest)?;
        let gamma_small = self.op(dae::BinaryOperator::LessEqual, absgam, scaled_est)?;
        let alpha_small = self.op(dae::BinaryOperator::LessEqual, absalp, scaled_est)?;
        let scaled_alp = self.op(dae::BinaryOperator::Multiply, epsilon, absalp)?;
        let scaled_gam = self.op(dae::BinaryOperator::Multiply, epsilon, absgam)?;
        let below_alp = self.op(dae::BinaryOperator::LessEqual, absest, scaled_alp)?;
        let below_gam = self.op(dae::BinaryOperator::LessEqual, absest, scaled_gam)?;
        let estimate_small = self.op(dae::BinaryOperator::Or, below_alp, below_gam)?;
        let conditions = [vanishing, gamma_small, alpha_small, estimate_small];
        let cases = [zero_case, gamma_case, alpha_case, estimate_case];
        let mut component = |pick: fn(&Estimate<'dae>) -> dae::ExprId<'dae>| {
            let branches = conditions
                .iter()
                .zip(&cases)
                .map(|(condition, case)| (*condition, pick(case)))
                .collect::<Vec<_>>();
            self.expressions
                .at(self.provenance)
                .conditional(branches, pick(&general))
        };
        Ok((
            component(|case| case.0)?,
            component(|case| case.1)?,
            component(|case| case.2)?,
        ))
    }

    fn select_estimate(
        &mut self,
        condition: dae::ExprId<'dae>,
        then: Estimate<'dae>,
        otherwise: Estimate<'dae>,
    ) -> Result<Estimate<'dae>, dae::DaeConstructionError> {
        Ok((
            self.conditional(condition, then.0, otherwise.0)?,
            self.conditional(condition, then.1, otherwise.1)?,
            self.conditional(condition, then.2, otherwise.2)?,
        ))
    }

    /// `(s, c) / |(s, c)|` and `|(s, c)|`.
    fn normalize(
        &mut self,
        s: dae::ExprId<'dae>,
        c: dae::ExprId<'dae>,
    ) -> Result<Estimate<'dae>, dae::DaeConstructionError> {
        let length = self.hypot(s, c)?;
        let safe = self.nonzero(length)?;
        Ok((
            self.op(dae::BinaryOperator::Divide, s, safe)?,
            self.op(dae::BinaryOperator::Divide, c, safe)?,
            length,
        ))
    }

    /// `Q**T * b` over the leading `rows` entries, applying the reflectors
    /// in factorization order.
    fn apply_qt(
        &mut self,
        qr: &PivotedQr<'dae>,
        rhs: &[dae::ExprId<'dae>],
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let mut values = rhs.to_vec();
        for (step, tau) in qr.taus.iter().enumerate() {
            let reflector = &qr.columns[step][step..];
            self.apply_reflector(*tau, reflector, &mut values[step..])?;
        }
        Ok(values)
    }

    /// The pivoted-coordinate solution for one effective rank: the complete
    /// orthogonal factorization of the leading `rank` rows of `R` when
    /// `rank < n` (`dtzrzf`), the triangular solve, the zero tail, and
    /// `Z**T` (`dormrz`).
    fn rank_solution(
        &mut self,
        factored: &[Vec<dae::ExprId<'dae>>],
        rank: usize,
        right: &[dae::ExprId<'dae>],
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let columns = factored.len();
        let zero = self.real(0.0)?;
        let mut rows = leading_trapezoid(factored, rank, zero);
        let taus = if rank < columns {
            self.complete_orthogonal(&mut rows, rank)?
        } else {
            Vec::new()
        };
        let mut solution = right[..columns].to_vec();
        self.back_substitute(&rows, rank, &mut solution)?;
        for value in &mut solution[rank..] {
            *value = zero;
        }
        for (row, tau) in taus.iter().enumerate() {
            let reflector = std::iter::once(rows[row][row])
                .chain(rows[row][rank..].iter().copied())
                .collect::<Vec<_>>();
            self.apply_to_trailing(*tau, &reflector, &mut solution, row, rank)?;
        }
        Ok(solution)
    }

    /// `dlatrz`: reduce the leading `rank` rows `[T11 T12]` to `[T 0]` from
    /// the right, last row first, returning each row's reflector scalar; row
    /// `i` keeps its reflector in its trailing block.
    fn complete_orthogonal(
        &mut self,
        rows: &mut [Vec<dae::ExprId<'dae>>],
        rank: usize,
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        let mut taus = vec![zero; rank];
        for row in (0..rank).rev() {
            let mut reduced = std::iter::once(rows[row][row])
                .chain(rows[row][rank..].iter().copied())
                .collect::<Vec<_>>();
            let tau = self.reflect(&mut reduced)?;
            rows[row][row] = reduced[0];
            rows[row][rank..].copy_from_slice(&reduced[1..]);
            taus[row] = tau;
            for above in rows[..row].iter_mut() {
                self.apply_to_trailing(tau, &reduced, above, row, rank)?;
            }
        }
        Ok(taus)
    }

    /// Apply a reflector that acts on entry `lead` and the entries from
    /// `tail` on (`dlarz`).
    fn apply_to_trailing(
        &mut self,
        tau: dae::ExprId<'dae>,
        reflector: &[dae::ExprId<'dae>],
        values: &mut [dae::ExprId<'dae>],
        lead: usize,
        tail: usize,
    ) -> Result<(), dae::DaeConstructionError> {
        let mut target = std::iter::once(values[lead])
            .chain(values[tail..].iter().copied())
            .collect::<Vec<_>>();
        self.apply_reflector(tau, reflector, &mut target)?;
        values[lead] = target[0];
        values[tail..].copy_from_slice(&target[1..]);
        Ok(())
    }

    /// `dtrsm`: solve the leading upper-triangular `rank` block in place.
    fn back_substitute(
        &mut self,
        rows: &[Vec<dae::ExprId<'dae>>],
        rank: usize,
        solution: &mut [dae::ExprId<'dae>],
    ) -> Result<(), dae::DaeConstructionError> {
        for pivot in (0..rank).rev() {
            let diagonal = self.nonzero(rows[pivot][pivot])?;
            solution[pivot] = self.op(dae::BinaryOperator::Divide, solution[pivot], diagonal)?;
            for (row, coefficients) in rows[..pivot].iter().enumerate() {
                let product = self.op(
                    dae::BinaryOperator::Multiply,
                    solution[pivot],
                    coefficients[pivot],
                )?;
                solution[row] = self.op(dae::BinaryOperator::Subtract, solution[row], product)?;
            }
        }
        Ok(())
    }

    /// `x(jpvt(i)) = y(i)`: entry `j` of the result is the pivoted entry
    /// whose original column is `j`.
    fn unpermute(
        &mut self,
        permutation: &[dae::ExprId<'dae>],
        pivoted: &[dae::ExprId<'dae>],
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        (1..=pivoted.len())
            .map(|column| {
                let branches = permutation
                    .iter()
                    .zip(pivoted)
                    .map(|(origin, value)| Ok((self.is_integer(*origin, column as i64)?, *value)))
                    .collect::<Result<Vec<_>, dae::DaeConstructionError>>()?;
                self.expressions
                    .at(self.provenance)
                    .conditional(branches, zero)
            })
            .collect()
    }

    fn dot(
        &mut self,
        lhs: &[dae::ExprId<'dae>],
        rhs: &[dae::ExprId<'dae>],
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let mut sum = self.real(0.0)?;
        for (left, right) in lhs.iter().zip(rhs) {
            let product = self.op(dae::BinaryOperator::Multiply, *left, *right)?;
            sum = self.op(dae::BinaryOperator::Add, sum, product)?;
        }
        Ok(sum)
    }

    /// The Euclidean norm of `values` (`dnrm2`).
    fn norm(
        &mut self,
        values: &[dae::ExprId<'dae>],
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let squares = self.dot(values, values)?;
        self.sqrt(squares)
    }

    /// `sqrt(a**2 + b**2)` (`dlapy2`).
    fn hypot(
        &mut self,
        a: dae::ExprId<'dae>,
        b: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.norm(&[a, b])
    }

    fn maximum(
        &mut self,
        values: &[dae::ExprId<'dae>],
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let mut largest = values[0];
        for value in &values[1..] {
            largest = self.max(largest, *value)?;
        }
        Ok(largest)
    }

    /// FORTRAN `SIGN(1, x)`: `1` for `x >= 0`, else `-1`.
    fn sign(
        &mut self,
        value: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        let one = self.real(1.0)?;
        let minus_one = self.real(-1.0)?;
        let nonnegative = self.op(dae::BinaryOperator::GreaterEqual, value, zero)?;
        self.conditional(nonnegative, one, minus_one)
    }

    /// `value`, or 1 where it is zero: a divisor on a branch the driver takes
    /// only when it is nonzero.
    fn nonzero(
        &mut self,
        value: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let zero = self.real(0.0)?;
        let one = self.real(1.0)?;
        let vanishing = self.op(dae::BinaryOperator::Equal, value, zero)?;
        self.conditional(vanishing, one, value)
    }

    fn real(&mut self, value: f64) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.literal(dae::DaeLiteral::Real(value))
    }

    fn negate(
        &mut self,
        value: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions
            .at(self.provenance)
            .unary(dae::UnaryOperator::Negate, value)
    }

    fn builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: &[dae::ExprId<'dae>],
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.expressions
            .at(self.provenance)
            .builtin(builtin, arguments.iter().copied())
    }

    fn abs(
        &mut self,
        value: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.builtin(dae::PureBuiltin::Abs, &[value])
    }

    fn sqrt(
        &mut self,
        value: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.builtin(dae::PureBuiltin::Sqrt, &[value])
    }

    fn max(
        &mut self,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        self.builtin(dae::PureBuiltin::Max, &[lhs, rhs])
    }
}

struct EstimateCases<'dae> {
    absalp: dae::ExprId<'dae>,
    absgam: dae::ExprId<'dae>,
    absest: dae::ExprId<'dae>,
}

/// A value the column swap of a pivot step moves: a column of entries or one
/// entry (an original column index).
trait SwapValue<'dae>: Clone {
    fn select(
        builder: &mut LapackBuilder<'_, '_, 'dae>,
        condition: dae::ExprId<'dae>,
        then: &Self,
        otherwise: &Self,
    ) -> Result<Self, dae::DaeConstructionError>;
}

impl<'dae> SwapValue<'dae> for dae::ExprId<'dae> {
    fn select(
        builder: &mut LapackBuilder<'_, '_, 'dae>,
        condition: dae::ExprId<'dae>,
        then: &Self,
        otherwise: &Self,
    ) -> Result<Self, dae::DaeConstructionError> {
        builder.conditional(condition, *then, *otherwise)
    }
}

impl<'dae> SwapValue<'dae> for Vec<dae::ExprId<'dae>> {
    fn select(
        builder: &mut LapackBuilder<'_, '_, 'dae>,
        condition: dae::ExprId<'dae>,
        then: &Self,
        otherwise: &Self,
    ) -> Result<Self, dae::DaeConstructionError> {
        then.iter()
            .zip(otherwise)
            .map(|(then, otherwise)| builder.conditional(condition, *then, *otherwise))
            .collect()
    }
}

/// Row `i` of the leading `rank` rows of `R`: zero left of the diagonal.
fn leading_trapezoid<'dae>(
    factored: &[Vec<dae::ExprId<'dae>>],
    rank: usize,
    zero: dae::ExprId<'dae>,
) -> Vec<Vec<dae::ExprId<'dae>>> {
    let mut rows = vec![vec![zero; factored.len()]; rank];
    for (row, entries) in rows.iter_mut().enumerate() {
        for (column, values) in factored.iter().enumerate().skip(row) {
            entries[column] = values[row];
        }
    }
    rows
}
