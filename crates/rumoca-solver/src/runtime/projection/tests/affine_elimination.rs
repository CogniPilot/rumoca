mod admission;
mod controls;

use super::*;

const DIMENSION: usize = 24;
type ResidualOverride = fn(&[f64], &mut [f64]);

struct CyclicAffine {
    matrix: DMatrix<f64>,
    rhs: DVector<f64>,
    plan: solve::AlgebraicProjectionPlan,
    structures: solve::ContinuousStructuralArtifacts,
    full_solves: Cell<usize>,
    cache: std::cell::RefCell<SparseNewtonCache>,
    residual_override: Option<ResidualOverride>,
    invalid_torn_correction: bool,
    invalid_guarded_correction: bool,
    candidate_calls: std::cell::RefCell<Vec<(solve::TearingCandidate, Vec<f64>)>>,
    origin_evaluations: Cell<usize>,
    certificate_evaluations: Cell<usize>,
    residual_error_at_origin: Option<bool>,
    storage: Option<std::cell::RefCell<JacobianStorage>>,
    prepared: bool,
}

impl CyclicAffine {
    fn new(zero_pivots: &[usize], expected: &DVector<f64>) -> Self {
        let dimension = expected.len();
        let mut matrix = DMatrix::zeros(dimension, dimension);
        for row in 0..dimension - 1 {
            matrix[(row, row)] = if zero_pivots.contains(&row) { 0.0 } else { 1.0 };
            matrix[(row, row + 1)] = -1.0;
        }
        matrix[(dimension - 1, 0)] = 1.0;
        matrix[(dimension - 1, dimension - 1)] = 1.0;
        let rhs = &matrix * expected;
        let plan = solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: (0..dimension).collect(),
                y_indices: (0..dimension).collect(),
                tearing: Some(solve::BlockTearing {
                    tear_y_indices: vec![dimension - 1],
                    residual_rows: vec![dimension - 1],
                    causal_steps: (0..dimension - 1)
                        .rev()
                        .map(|row| solve::CausalStep { row, y_index: row })
                        .collect(),
                }),
                guarded_tearing: None,
                alternate_charts: Vec::new(),
            }],
        };
        let dependencies = (0..dimension)
            .map(|row| vec![row, (row + 1) % dimension])
            .collect::<Vec<_>>();
        Self::from_system(matrix, rhs, plan, dependencies)
    }

    fn from_system(
        matrix: DMatrix<f64>,
        rhs: DVector<f64>,
        plan: solve::AlgebraicProjectionPlan,
        dependencies: Vec<Vec<usize>>,
    ) -> Self {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("TornAffine.mo"),
            0,
            1,
        );
        let pattern = solve::StructuralPattern::from_row_dependencies(
            matrix.nrows(),
            matrix.ncols(),
            &dependencies,
            solve::PatternProvenance::derived(
                solve::PatternDerivation::DependencyPropagation,
                span,
            )
            .unwrap(),
        )
        .unwrap();
        let primal = source_programs(&matrix, &rhs, false, span);
        let directional = source_programs(&matrix, &rhs, true, span);
        let structures = solve::ContinuousStructuralArtifacts::derived(
            None,
            vec![pattern],
            vec![false],
            None,
            vec![],
            None,
        )
        .with_algebraic_output_evaluations(&plan, &primal, &directional, &directional);
        Self {
            matrix,
            rhs,
            plan,
            structures,
            full_solves: Cell::new(0),
            cache: Default::default(),
            residual_override: None,
            invalid_torn_correction: false,
            invalid_guarded_correction: false,
            candidate_calls: Default::default(),
            origin_evaluations: Cell::new(0),
            certificate_evaluations: Cell::new(0),
            residual_error_at_origin: None,
            storage: None,
            prepared: false,
        }
    }
}

pub(super) fn source_programs(
    matrix: &DMatrix<f64>,
    rhs: &DVector<f64>,
    directional: bool,
    span: rumoca_core::Span,
) -> solve::ScalarProgramBlock {
    let rows = (0..matrix.nrows())
        .map(|row| {
            let mut ops = vec![solve::LinearOp::Const {
                dst: 0,
                value: if directional { 0.0 } else { -rhs[row] },
            }];
            let mut sum = 0;
            for column in (0..matrix.ncols()).filter(|&column| matrix[(row, column)] != 0.0) {
                let base = ops.len() as u32;
                ops.push(if directional {
                    solve::LinearOp::LoadSeed {
                        dst: base,
                        index: column,
                    }
                } else {
                    solve::LinearOp::LoadY {
                        dst: base,
                        index: column,
                    }
                });
                ops.push(solve::LinearOp::Const {
                    dst: base + 1,
                    value: matrix[(row, column)],
                });
                ops.push(solve::LinearOp::Binary {
                    dst: base + 2,
                    op: solve::BinaryOp::Mul,
                    lhs: base,
                    rhs: base + 1,
                });
                ops.push(solve::LinearOp::Binary {
                    dst: base + 3,
                    op: solve::BinaryOp::Add,
                    lhs: sum,
                    rhs: base + 2,
                });
                sum = base + 3;
            }
            ops.push(solve::LinearOp::StoreOutput { src: sum });
            ops
        })
        .collect();
    solve::ScalarProgramBlock::with_program_spans(rows, vec![span; matrix.nrows()]).unwrap()
}

impl ImplicitProjectionModel for CyclicAffine {
    fn lease_affine_jacobian(
        &self,
        _: &solve::JacobianStructure,
        _: (&[usize], &[usize]),
        _: usize,
    ) -> Result<Option<std::cell::RefMut<'_, JacobianStorage>>, RuntimeSolveError> {
        Ok(self.storage.as_ref().map(|storage| storage.borrow_mut()))
    }

    fn eval_prepared_implicit_jacobian(
        &self,
        structure: &solve::JacobianStructure,
        _: (&[usize], &[usize]),
        _: &[f64],
        _: &[f64],
        _: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        if self.prepared {
            let layout = structure.jacobian_application().unwrap().value_layout();
            assert_eq!(out.len(), layout.len());
            for (slot, value) in out.iter_mut().enumerate() {
                *value = self.matrix[layout.coordinate(slot).unwrap()];
            }
            return Ok(true);
        }
        // A declined partial fill must not survive sparse interpreted assembly.
        out.fill(f64::NAN);
        Ok(false)
    }

    fn eval_residual(
        &self,
        y: &[f64],
        _: &[f64],
        _: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let at_origin = y.iter().all(|&value| value == 0.0);
        if at_origin {
            self.origin_evaluations
                .set(self.origin_evaluations.get() + 1);
        } else {
            self.certificate_evaluations
                .set(self.certificate_evaluations.get() + 1);
        }
        if self.residual_error_at_origin == Some(at_origin) {
            return Err(RuntimeSolveError::solve_ir("affine residual witness"));
        }
        if let Some(storage) = &self.storage {
            assert!(
                storage.try_borrow_mut().is_err(),
                "lease spans residual certification"
            );
        }
        let residual = &self.matrix * DVector::from_column_slice(y) - &self.rhs;
        out.copy_from_slice(residual.as_slice());
        if let Some(evaluate) = self.residual_override {
            evaluate(y, out);
        }
        Ok(())
    }
    fn eval_jacobian_v(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out.copy_from_slice((&self.matrix * DVector::from_column_slice(v)).as_slice());
        Ok(())
    }
    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row))
    }
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
    fn target_name_for_row(&self, _: usize) -> Option<&str> {
        None
    }
    fn algebraic_projection_block_is_affine(&self, _: usize) -> bool {
        true
    }
    fn algebraic_projection_block_structure(&self, _: usize) -> Option<&solve::JacobianStructure> {
        self.structures.algebraic_projection().first()
    }
    fn solve_algebraic_newton_delta(
        &self,
        _: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        if let Some(storage) = &self.storage {
            assert!(storage.try_borrow_mut().is_err(), "lease spans full solve");
        }
        self.full_solves.set(self.full_solves.get() + 1);
        scaled_newton_delta(system)
    }

    fn solve_affine_torn_candidate(
        &self,
        block: usize,
        candidate: solve::TearingCandidate,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        self.candidate_calls
            .borrow_mut()
            .push((candidate, system.residual.to_vec()));
        if candidate == solve::TearingCandidate::Primary {
            return self.solve_affine_torn_delta(block, system);
        }
        if self.invalid_guarded_correction {
            return Some(DVector::zeros(self.matrix.ncols()));
        }
        scaled_newton_delta_with_tearing(
            system,
            &mut self.cache.borrow_mut(),
            candidate,
            self.structures.algebraic_projection()[0].affine_elimination_candidate(candidate)?,
        )
    }

    fn solve_affine_torn_delta(
        &self,
        _: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        if self.invalid_torn_correction {
            return Some(DVector::zeros(self.matrix.ncols()));
        }
        scaled_newton_delta_with_tearing(
            system,
            &mut self.cache.borrow_mut(),
            solve::TearingCandidate::Primary,
            self.structures.algebraic_projection()[0].affine_elimination()?,
        )
    }
}

fn project_cycle(zero_pivots: &[usize]) -> CyclicAffine {
    let expected = DVector::from_fn(DIMENSION, |row, _| 1.0 + row as f64 / 8.0);
    let model = CyclicAffine::new(zero_pivots, &expected);
    let mut y = vec![-10.0; DIMENSION];
    project_algebraics_with_plan_certified(
        &model,
        &model.plan,
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-10,
        },
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .unwrap();
    for (actual, expected) in y.iter().zip(expected.iter()) {
        assert!((actual - expected).abs() < 1e-9);
    }
    model
}

#[test]
fn affine_cycle_eliminates_causal_coordinates_before_factorization() {
    assert_eq!(project_cycle(&[]).full_solves.get(), 0);
}

#[test]
fn zero_causal_pivot_preserves_the_full_implicit_solve() {
    assert!(project_cycle(&[7]).full_solves.get() > 0);
}

#[test]
fn fully_unsuitable_causal_partition_preserves_the_full_system_solve() {
    assert!(
        project_cycle(&(0..DIMENSION - 1).collect::<Vec<_>>())
            .full_solves
            .get()
            > 0
    );
}
