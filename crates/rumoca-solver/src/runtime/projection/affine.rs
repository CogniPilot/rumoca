//! Evaluate a certified affine block independently of its incoming guess.

use super::*;

pub(super) fn project_affine_block<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    block: &solve::AlgebraicProjectionBlock,
    block_index: usize,
    tol: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    // The solve runs in `y` itself, which it changes only in the block
    // unknowns; they are restored when the block does not settle.
    let incoming = super::block_values(y, &block.y_indices);
    let settled = settle_affine_block(model, y, (p, t), block, block_index, tol);
    if !matches!(settled, Ok(true)) {
        super::restore_block_values(y, &block.y_indices, &incoming);
    }
    let settled = settled?;
    let changed = settled && super::block_values_changed(y, &block.y_indices, &incoming);
    Ok(ProjectionBlockUpdate { changed, settled })
}

/// Solve the block's unknowns in `candidate` from the arithmetic origin.
/// Construction proves F(x) = A*x + b for this block. Evaluating b at x=0 and
/// solving A*x=-b computes the coordinate itself. Accepting an incoming Newton
/// iterate merely because its correction is below tol can retain the wrong
/// side of a relation after a discrete branch change.
fn settle_affine_block<M: ImplicitProjectionModel>(
    model: &M,
    candidate: &mut [f64],
    (p, t): (&[f64], f64),
    block: &solve::AlgebraicProjectionBlock,
    block_index: usize,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    for &index in &block.y_indices {
        candidate[index] = 0.0;
    }
    let structure = model.algebraic_projection_block_structure(block_index);
    let retained = match structure {
        Some(_) => model
            .affine_jacobian_cache(block_index)
            .and_then(|cache| cache.borrow_mut().take_affine_linearization()),
        None => None,
    };
    let point = OriginPoint {
        candidate,
        p,
        t,
        block,
        structure,
    };
    let linearization = AffineLinearization::at_origin(model, point, retained)?;
    let mut system = AffineBlockSystem {
        model,
        parameters: p,
        time: t,
        block,
        block_index,
        certificate_scales: linearization.certificate_scales(model, candidate, block),
        linearization,
        structure,
        tolerance: tol,
        prefer_torn: true,
        used_torn: std::cell::Cell::new(false),
    };
    let mut settled = system.project(candidate)?;
    if !settled && system.used_torn.get() {
        system.prefer_torn = false;
        for &index in &block.y_indices {
            candidate[index] = 0.0;
        }
        settled = system.project(candidate)?;
    }
    if let (Some(_), Some(cache)) = (structure, model.affine_jacobian_cache(block_index)) {
        cache
            .borrow_mut()
            .retain_affine_linearization(system.linearization);
    }
    Ok(settled)
}

/// Everything an affine projection forms at the arithmetic origin before its
/// first residual: the block Jacobian, its row and variable scales, its row
/// magnitudes, and which row scales derive from a nonzero contribution.
///
/// A row carrying the parameter-static gradient certificate (SPEC_0043) has a
/// Jacobian row that is a function of its certified parameter snapshot alone,
/// and so are that row's scale, magnitude, and derived flag, except a scale
/// that falls back to a coordinate outside the block. A retained
/// linearization formed row by row from reverse gradients, whose certified
/// rows' parameter snapshot is bitwise identical, is therefore exactly the
/// formation at the current point once its uncertified rows are refilled from
/// their reverse gradients and the outside fallback scales are formed again,
/// and it is reused instead of formed.
#[derive(Clone)]
pub(crate) struct AffineLinearization {
    jacobian: BlockJacobian,
    row_scales: Vec<f64>,
    variable_scales: Vec<f64>,
    /// [`jacobian_row_magnitudes`] of `jacobian`.
    row_magnitudes: Vec<f64>,
    /// [`jacobian_row_derived`] at the origin variable scales.
    row_derived: Vec<bool>,
    /// Each row's fallback coordinate: its implicit target in solver Y.
    fallback_targets: Vec<Option<usize>>,
    /// What a reuse checks and refreshes, when the Jacobian was formed row by
    /// row from reverse gradients.
    reuse: Option<ReuseKey>,
    /// Names `jacobian`, `row_scales`, and `variable_scales` together (see
    /// [`ScaledNewtonSystem::revision`]): issued fresh whenever one changes.
    revision: u64,
}

/// The certified rows' parameter snapshot and the rows a reuse refills.
#[derive(Clone)]
struct ReuseKey {
    /// The union of the certified rows' parameter slots.
    slots: Box<[usize]>,
    /// Their bit patterns when the linearization was formed.
    bits: Box<[u64]>,
    /// Block-local rows without the certificate.
    uncertified: Box<[usize]>,
}

impl ReuseKey {
    /// The block's certified parameter slots and uncertified rows.
    fn layout<M: ImplicitProjectionModel>(
        model: &M,
        block: &solve::AlgebraicProjectionBlock,
    ) -> (Box<[usize]>, Box<[usize]>) {
        let mut slots = Vec::new();
        let mut uncertified = Vec::new();
        for (local, &row) in block.rows.iter().enumerate() {
            match model.implicit_row_static_gradient_parameters(row) {
                Some(parameters) => slots.extend_from_slice(parameters),
                None => uncertified.push(local),
            }
        }
        slots.sort_unstable();
        slots.dedup();
        (slots.into_boxed_slice(), uncertified.into_boxed_slice())
    }

    fn at((slots, uncertified): (Box<[usize]>, Box<[usize]>), p: &[f64]) -> Option<Self> {
        let bits = slots
            .iter()
            .map(|&slot| p.get(slot).map(|value| value.to_bits()))
            .collect::<Option<Box<[u64]>>>()?;
        Some(Self {
            slots,
            bits,
            uncertified,
        })
    }

    fn matches(&self, p: &[f64]) -> bool {
        self.slots
            .iter()
            .zip(&self.bits)
            .all(|(&slot, &bits)| p.get(slot).is_some_and(|value| value.to_bits() == bits))
    }
}

/// The block, its structure, and the point a linearization is formed at.
#[derive(Clone, Copy)]
struct OriginPoint<'a> {
    candidate: &'a [f64],
    p: &'a [f64],
    t: f64,
    block: &'a solve::AlgebraicProjectionBlock,
    structure: Option<&'a solve::JacobianStructure>,
}

impl AffineLinearization {
    /// The block's linearization at `point`, the arithmetic origin: `retained`
    /// refreshed when its certified parameter snapshot still holds, else
    /// formed afresh in the retained Jacobian's storage.
    fn at_origin<M: ImplicitProjectionModel>(
        model: &M,
        point: OriginPoint<'_>,
        retained: Option<Self>,
    ) -> Result<Self, RuntimeSolveError> {
        let Some(mut retained) = retained else {
            return Self::form(model, point, None);
        };
        if let Some(key) = retained.reuse.take()
            && key.matches(point.p)
            && retained.refresh(model, point, &key.uncertified)?
        {
            retained.reuse = Some(key);
            #[cfg(debug_assertions)]
            retained.assert_formed_again(model, point)?;
            return Ok(retained);
        }
        Self::form(model, point, Some(retained.jacobian))
    }

    fn form<M: ImplicitProjectionModel>(
        model: &M,
        point: OriginPoint<'_>,
        storage: Option<BlockJacobian>,
    ) -> Result<Self, RuntimeSolveError> {
        let OriginPoint {
            candidate,
            p,
            t,
            block,
            structure,
        } = point;
        let (jacobian, by_rows) =
            affine_block_jacobian(model, candidate, (p, t), block, structure, storage)?;
        let pattern = structure.map(solve::JacobianStructure::pattern);
        let (row_scales, variable_scales) =
            algebraic_block_scales(model, candidate, block, &jacobian, pattern);
        let row_magnitudes = jacobian_row_magnitudes(&jacobian, pattern);
        let row_derived = jacobian_row_derived(&jacobian, &variable_scales, pattern);
        let reuse = (by_rows && structure.is_some())
            .then(|| ReuseKey::at(ReuseKey::layout(model, block), p))
            .flatten();
        Ok(Self {
            jacobian,
            row_scales,
            variable_scales,
            row_magnitudes,
            row_derived,
            fallback_targets: fallback_targets(model, block),
            reuse,
            revision: next_revision(),
        })
    }

    /// Refill the `uncertified` rows from their reverse gradients at `point`
    /// with their scales, magnitudes, and derived flags, and form again every
    /// fallback row scale that reads a coordinate outside the block. `false`
    /// when an uncertified row has no reverse gradient here.
    fn refresh<M: ImplicitProjectionModel>(
        &mut self,
        model: &M,
        point: OriginPoint<'_>,
        uncertified: &[usize],
    ) -> Result<bool, RuntimeSolveError> {
        let Some(structure) = point.structure else {
            return Ok(false);
        };
        let pattern = structure.pattern();
        if !super::initial::refill_reverse_rows(
            model,
            (point.candidate, point.p, point.t),
            (&point.block.rows, &point.block.y_indices),
            pattern,
            uncertified,
            &mut self.jacobian,
        )? {
            return Ok(false);
        }
        // The torn factorization reads the Jacobian and both scale vectors;
        // refilled rows may change the Jacobian, the fallbacks only row scales.
        let mut changed = !uncertified.is_empty();
        for &row in uncertified {
            let fallback = self.fallback_scale(model, point.candidate, row);
            let (jacobian, scales) = (&self.jacobian, &self.variable_scales);
            self.row_scales[row] =
                super::scaling::jacobian_row_scale(jacobian, row, scales, fallback, Some(pattern));
            self.row_derived[row] =
                super::scaling::jacobian_row_scale(jacobian, row, scales, 0.0, Some(pattern)) > 0.0;
            self.row_magnitudes[row] =
                super::scaling::jacobian_row_magnitude(jacobian, row, Some(pattern));
        }
        for row in 0..self.row_scales.len() {
            if let Some(index) = self.fallback_targets[row]
                && !self.row_derived[row]
                && !point.block.y_indices.contains(&index)
            {
                let scale = model_variable_scale(model, index, point.candidate[index]);
                changed |= scale.to_bits() != self.row_scales[row].to_bits();
                self.row_scales[row] = scale;
            }
        }
        if changed {
            self.revision = next_revision();
        }
        Ok(true)
    }

    /// Row `row`'s fallback scale at `candidate`, as [`algebraic_block_scales`]
    /// forms it: its target's scale, or the unknown at the same offset.
    fn fallback_scale<M: ImplicitProjectionModel>(
        &self,
        model: &M,
        candidate: &[f64],
        row: usize,
    ) -> f64 {
        self.fallback_targets[row].map_or_else(
            || self.variable_scales.get(row).copied().unwrap_or(1.0),
            |index| model_variable_scale(model, index, candidate[index]),
        )
    }

    /// Debug builds form a reused linearization afresh and require the reuse
    /// to be bitwise the formation it stands for.
    #[cfg(debug_assertions)]
    fn assert_formed_again<M: ImplicitProjectionModel>(
        &self,
        model: &M,
        point: OriginPoint<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let fresh = Self::form(model, point, None)?;
        let bits = |values: &[f64]| {
            values
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>()
        };
        assert_eq!(
            bits(fresh.jacobian.as_dense().as_slice()),
            bits(self.jacobian.as_dense().as_slice()),
            "a reused affine Jacobian is the one formed at its parameter snapshot"
        );
        assert_eq!(bits(&fresh.row_scales), bits(&self.row_scales));
        assert_eq!(bits(&fresh.variable_scales), bits(&self.variable_scales));
        assert_eq!(bits(&fresh.row_magnitudes), bits(&self.row_magnitudes));
        assert_eq!(fresh.row_derived, self.row_derived);
        Ok(())
    }

    /// The model scales the refinement's certificate reads at `candidate`.
    /// Every block unknown is zero there, so its origin scale is its declared
    /// scale, and a fallback coordinate outside the block keeps its value for
    /// the whole call, so its origin scale serves every pass.
    fn certificate_scales<M: ImplicitProjectionModel>(
        &self,
        model: &M,
        candidate: &[f64],
        block: &solve::AlgebraicProjectionBlock,
    ) -> CertificateScales {
        CertificateScales {
            unknowns: block
                .y_indices
                .iter()
                .copied()
                .zip(self.variable_scales.iter().copied())
                .collect(),
            fallbacks: self
                .fallback_targets
                .iter()
                .map(|target| {
                    target
                        .map(|index| (index, model_variable_scale(model, index, candidate[index])))
                })
                .collect(),
        }
    }
}

/// The block Jacobian at `candidate`, refilled into `storage`, the compact
/// matrix the previous solve of this block retained, when it is stored in the
/// block's structural pattern, so a projection allocates nothing proportional
/// to the block; and whether every row was filled from its reverse gradient.
fn affine_block_jacobian<M: ImplicitProjectionModel>(
    model: &M,
    candidate: &[f64],
    (p, t): (&[f64], f64),
    block: &solve::AlgebraicProjectionBlock,
    structure: Option<&solve::JacobianStructure>,
    storage: Option<BlockJacobian>,
) -> Result<(BlockJacobian, bool), RuntimeSolveError> {
    let storage = match (storage, structure) {
        (Some(mut retained), Some(structure))
            if retained.is_stored_in(structure.compact_layout()) =>
        {
            retained.clear();
            retained
        }
        (_, Some(structure)) => BlockJacobian::compact(structure.compact_layout()),
        (_, None) => BlockJacobian::zeros(block.rows.len(), block.y_indices.len()),
    };
    super::initial::algebraic_block_jacobian_by_rows_in(
        model,
        (candidate, p, t),
        (&block.rows, &block.y_indices),
        structure,
        storage,
    )
}

struct AffineBlockSystem<'a, M> {
    model: &'a M,
    parameters: &'a [f64],
    time: f64,
    block: &'a solve::AlgebraicProjectionBlock,
    block_index: usize,
    linearization: AffineLinearization,
    /// The model scales the refinement's certificate reads.
    certificate_scales: CertificateScales,
    structure: Option<&'a solve::JacobianStructure>,
    tolerance: f64,
    prefer_torn: bool,
    used_torn: std::cell::Cell<bool>,
}

impl<M: ImplicitProjectionModel> AffineBlockSystem<'_, M> {
    fn project(&self, y: &mut [f64]) -> Result<bool, RuntimeSolveError> {
        let residual = self.residual(y)?;
        let Some(solution) = self.solve(&residual) else {
            return Ok(false);
        };
        for (&index, &value) in self.block.y_indices.iter().zip(solution.iter()) {
            y[index] = value;
        }
        self.refine(y)
    }

    fn residual(&self, y: &[f64]) -> Result<Vec<f64>, RuntimeSolveError> {
        if let Some(selection) = self
            .structure
            .and_then(solve::JacobianStructure::residual_output_evaluation)
        {
            let mut residual = vec![0.0; self.block.rows.len()];
            if self.model.eval_implicit_residual_outputs(
                selection,
                y,
                self.parameters,
                self.time,
                &mut residual,
            )? {
                return Ok(residual);
            }
        }
        implicit_selected_residuals(
            self.model,
            y,
            self.parameters,
            self.time,
            &self.block.rows,
            "affine block residual",
        )
    }

    fn solve(&self, residual: &[f64]) -> Option<DVector<f64>> {
        // Conditioning belongs to this fixed matrix. Candidate-dependent
        // scales still certify the fresh source residual in `refine`.
        let system = ScaledNewtonSystem {
            jacobian: &self.linearization.jacobian,
            residual,
            row_scales: &self.linearization.row_scales,
            variable_scales: &self.linearization.variable_scales,
            structure: self.structure.map(solve::JacobianStructure::pattern),
            tolerance: self.tolerance,
            revision: Some(self.linearization.revision),
        };
        let finite = |v: &DVector<f64>| {
            v.len() == self.block.y_indices.len() && v.iter().all(|x| x.is_finite())
        };
        if self.prefer_torn
            && let Some(delta) = self
                .model
                .solve_affine_torn_delta(self.block_index, system)
                .filter(finite)
        {
            self.used_torn.set(true);
            return Some(delta);
        }
        let delta = self
            .model
            .solve_algebraic_newton_delta(self.block_index, system)
            .filter(finite);
        if delta.is_none() {
            super::note_block_fallback(
                self.model.projection_site(self.block_index),
                ProjectionFallback::JacobianDeclined,
            );
        }
        delta
    }

    fn refine(&self, y: &mut [f64]) -> Result<bool, RuntimeSolveError> {
        for iteration in 0..ALGEBRAIC_PROJECTION_MAX_ITERS {
            let residual = self.residual(y)?;
            // Zero was only the arithmetic origin used to extract b. The
            // residual certificate uses this candidate's coordinate scales.
            let origin = OriginRowScales {
                jacobian: &self.linearization.jacobian,
                structure: self.structure.map(solve::JacobianStructure::pattern),
                scales: &self.linearization.row_scales,
                magnitudes: &self.linearization.row_magnitudes,
                derived: &self.linearization.row_derived,
            };
            let converged = origin_bounded_residual_converged(
                y,
                &self.certificate_scales,
                &origin,
                &residual,
                self.tolerance,
            );
            // One correction recovers small coordinates lost while solving
            // beside large offsets, even when the residual already fits tol.
            // Exact zero needs no correction; subsequent passes certify the
            // corrected coordinate under the unchanged convergence policy.
            if converged && (iteration > 0 || residual.iter().all(|&value| value == 0.0)) {
                return Ok(true);
            }
            // Factorization roundoff can leave small coordinates inaccurate
            // in a block containing much larger currents or forces. Refine
            // against the original residual with the same certified matrix.
            let Some(delta) = self.solve(&residual) else {
                return Ok(false);
            };
            let Some(changed) = self.apply_correction(y, delta.as_slice()) else {
                return Ok(false);
            };
            if !changed {
                return Ok(converged);
            }
        }
        Ok(false)
    }

    fn apply_correction(&self, y: &mut [f64], delta: &[f64]) -> Option<bool> {
        let mut changed = false;
        for (&index, &correction) in self.block.y_indices.iter().zip(delta) {
            let value = y[index] + correction;
            if !value.is_finite() {
                return None;
            }
            changed |= value != y[index];
            y[index] = value;
        }
        Some(changed)
    }
}

/// A revision no linearization has held before.
fn next_revision() -> u64 {
    static REVISIONS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    REVISIONS.fetch_add(1, std::sync::atomic::Ordering::Relaxed) + 1
}
