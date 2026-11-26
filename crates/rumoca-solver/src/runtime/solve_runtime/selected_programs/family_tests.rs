//! A compact owner whose whole object binds no program takes its selected
//! entries from one selectable family of its scalar view, lowered once at
//! construction, never from one lowering per program.

use super::*;
use std::cell::Cell;

const ROWS: usize = 3;
const Y: [f64; ROWS] = [10.0, 20.0, 30.0];
const SEED: [f64; ROWS] = [1.0, -2.0, 0.5];

fn row_span(row: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("compact_family.mo"),
        10 * (row + 1),
        10 * (row + 1) + 1,
    )
}

/// Row `i` stores `y[i]`; its directional form stores `seed[i]`.
fn family(load: impl Fn(usize) -> solve::LinearOp) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_program_spans(
        (0..ROWS)
            .map(|row| vec![load(row), solve::LinearOp::StoreOutput { src: 0 }])
            .collect(),
        (0..ROWS).map(row_span).collect(),
    )
    .unwrap()
}

fn primal() -> solve::ScalarProgramBlock {
    family(|row| solve::LinearOp::LoadY { dst: 0, index: row })
}

fn directional() -> solve::ScalarProgramBlock {
    family(|row| solve::LinearOp::LoadSeed { dst: 0, index: row })
}

fn lone(source: &solve::ScalarProgramBlock, program: usize) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_program_spans(
        vec![source.program(program).unwrap().to_vec()],
        vec![source.program_span(program).unwrap()],
    )
    .unwrap()
}

/// A compiled scalar family: one whole object and one bound entry per program.
struct Entries<C: ?Sized> {
    whole: Rc<C>,
    programs: Vec<Rc<C>>,
}

impl<C: ?Sized> Entries<C> {
    fn new(
        source: &solve::ScalarProgramBlock,
        kernel: impl Fn(&solve::ScalarProgramBlock) -> Result<Rc<C>, String>,
    ) -> Result<Self, String> {
        Ok(Self {
            whole: kernel(source)?,
            programs: (0..source.row_count())
                .map(|program| kernel(&lone(source, program)))
                .collect::<Result<_, _>>()?,
        })
    }
}

impl CompiledSolveExpression for Entries<dyn CompiledSolveExpression> {
    fn bind_program(
        &self,
        _source: &solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        Ok(self.programs.get(program).cloned())
    }
    // Required by the trait; the tests call the bound per-program entries.
    #[cfg_attr(coverage_nightly, coverage(off))]
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.whole.call(y, p, t, tables, out)
    }
}

impl CompiledSolveJacobianExpression for Entries<dyn CompiledSolveJacobianExpression> {
    // Required by the trait; the tests never ask for committing programs.
    #[cfg_attr(coverage_nightly, coverage(off))]
    fn programs_committing(self: Rc<Self>) -> Option<Rc<dyn crate::CompiledProgramsCommitting>> {
        None
    }
    fn bind_program(
        &self,
        _source: &solve::ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        Ok(self.programs.get(program).cloned())
    }
    // Required by the trait; the tests call the bound per-program entries.
    #[cfg_attr(coverage_nightly, coverage(off))]
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.whole.call(y, p, t, seed, tables, out)
    }
}

/// A backend with a compact whole object that binds no program, as the
/// compute-block adapters of the native backend. Its scalar family binds
/// every program unless `unbound`, and refuses a family holding `refused`,
/// as it refuses that program alone.
#[derive(Default)]
struct Compact {
    compact: Cell<usize>,
    families: Cell<usize>,
    alone: Cell<usize>,
    unbound: Cell<bool>,
    refused: Cell<Option<rumoca_core::Span>>,
}

impl Compact {
    /// `(compact whole objects, scalar families, programs lowered alone)`.
    fn lowered(&self) -> (usize, usize, usize) {
        (self.compact.get(), self.families.get(), self.alone.get())
    }

    fn admit(&self, source: &solve::ScalarProgramBlock) -> Result<bool, crate::CompileRefusal> {
        let whole_family = source.row_count() > 1;
        let count = if whole_family {
            &self.families
        } else {
            &self.alone
        };
        count.set(count.get() + 1);
        if let Some(refused) = self.refused.get()
            && source.program_spans().contains(&refused)
        {
            return Err("row unavailable".into());
        }
        Ok(whole_family && !self.unbound.get())
    }
}

impl SolveExecutionBackend for Compact {
    fn compile_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, crate::CompileRefusal> {
        if self.admit(source)? {
            return Ok(Rc::new(Entries::new(
                source,
                super::super::test_kernels::expression,
            )?));
        }
        super::super::test_kernels::expression(source).map_err(Into::into)
    }
    fn compile_jacobian_expression(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, crate::CompileRefusal> {
        if self.admit(source)? {
            return Ok(Rc::new(Entries::new(
                source,
                super::super::test_kernels::jacobian,
            )?));
        }
        super::super::test_kernels::jacobian(source).map_err(Into::into)
    }
    fn compile_compute_expression(
        &self,
        source: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveExpression>>, String> {
        self.compact.set(self.compact.get() + 1);
        let scalar = to_scalar_program_block(source).map_err(|error| error.to_string())?;
        super::super::test_kernels::expression(&scalar).map(Some)
    }
    fn compile_compute_jacobian_expression(
        &self,
        source: &solve::ComputeBlock,
    ) -> Result<Option<Rc<dyn CompiledSolveJacobianExpression>>, String> {
        self.compact.set(self.compact.get() + 1);
        let scalar = to_scalar_program_block(source).map_err(|error| error.to_string())?;
        super::super::test_kernels::jacobian(&scalar).map(Some)
    }
}

fn expression(backend: &Compact) -> Result<Rc<CompleteExpression>, EvalSolveError> {
    let source = primal();
    let compute = solve::ComputeBlock::from_scalar_program_block(source.clone());
    compute_expression(backend, "family", &compute, &source)
}

fn selected(code: &CompleteExpression) -> [f64; ROWS] {
    let mut values = Vec::new();
    std::array::from_fn(|program| {
        code.call_program_outputs(program, &Y, &[], 0.0, &[], &mut values)
            .unwrap();
        assert_eq!(values.len(), 1);
        values[0]
    })
}

#[test]
fn a_compact_expression_owner_takes_its_selected_entries_from_one_family() {
    let backend = Compact::default();
    let code = expression(&backend).unwrap();
    assert_eq!(
        backend.lowered(),
        (1, 1, 0),
        "one compact object, one scalar family, and none of the {ROWS} programs alone"
    );

    assert_eq!(selected(&code), Y);
    assert_eq!(
        code.call_program_output((2, 0), &Y, &[], 0.0, &[]).unwrap(),
        30.0
    );
    let mut bulk = [f64::NAN; ROWS];
    code.call_program_outputs_at(&[(0, 0), (2, 0), (1, 0)], (&Y, &[], 0.0), &[], &mut bulk)
        .unwrap();
    assert_eq!(bulk, [10.0, 30.0, 20.0]);
    let mut whole = [f64::NAN; ROWS];
    code.call(&Y, &[], 0.0, &[], &mut whole).unwrap();
    assert_eq!(whole, Y);
    assert_eq!(backend.lowered(), (1, 1, 0), "execution lowers nothing");
}

#[test]
fn a_compact_directional_owner_takes_its_selected_entries_from_one_family() {
    let backend = Compact::default();
    let source = directional();
    let compute = solve::ComputeBlock::from_scalar_program_block(source.clone());
    let code = compute_jacobian(&backend, "family_v", &compute, &source).unwrap();
    assert_eq!(
        backend.lowered(),
        (1, 1, 0),
        "one compact object, one scalar family, and none of the {ROWS} programs alone"
    );

    let mut values = Vec::new();
    for (program, expected) in SEED.into_iter().enumerate() {
        let inputs = solve_eval::JacobianEvalInputs {
            y: &Y,
            p: &[],
            t: 0.0,
            seed: &SEED,
        };
        code.call_program_outputs(program, inputs, &[], &mut values)
            .unwrap();
        assert_eq!(values, [expected]);
    }
    assert_eq!(
        code.call_program_output((1, 0), &Y, &[], 0.0, &SEED, &[])
            .unwrap(),
        -2.0
    );
    let mut whole = [f64::NAN; ROWS];
    code.call(&Y, &[], 0.0, &SEED, &[], &mut whole).unwrap();
    assert_eq!(whole, SEED);
    assert_eq!(backend.lowered(), (1, 1, 0), "execution lowers nothing");
}

#[test]
fn a_program_its_family_does_not_bind_is_still_lowered_alone() {
    let backend = Compact::default();
    backend.unbound.set(true);
    let code = expression(&backend).unwrap();
    assert_eq!(backend.alone.get(), ROWS);
    assert_eq!(selected(&code), Y);
}

#[test]
fn a_refused_family_leaves_the_refusal_to_the_program_that_cannot_be_lowered() {
    let backend = Compact::default();
    backend.refused.set(Some(row_span(1)));
    let Err(error) = expression(&backend) else {
        panic!("a program that cannot be lowered must refuse construction");
    };
    assert!(
        matches!(
            &error,
            EvalSolveError::CompiledExecution { message, span }
                if message == "required selected program `family[1]` could not be lowered: row unavailable"
                    && *span == Some(row_span(1))
        ),
        "{error:?}"
    );
    assert_eq!(
        backend.alone.get(),
        2,
        "construction stops at the refused program"
    );
}
