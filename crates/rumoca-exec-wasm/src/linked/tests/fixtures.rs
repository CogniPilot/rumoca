//! One small checked source per kind of module a saved scalar product holds.
use rumoca_ir_solve::{
    self as solve, BinaryOp, IssuedAssignmentRows, LinearOp, ScalarProgramBlock,
    SolvePureCallTable, UnaryOp, VarLayout, fmi::FmiComponent,
};

/// Static, dynamic and value helpers behind the complete entry.
pub(super) const CAUSAL: (&str, &str) = (
    "CausalEntry",
    "model CausalEntry parameter Real gain=2; Real x[2](each start=1,each fixed=true); Real a; \
     Real b; equation a=3*gain; b=a*x[1]; der(x[1])=b; der(x[2])=b+x[2]; end CausalEntry;",
);

/// A proper kinematic partition: the control module has both entries.
pub(super) const MIXED: (&str, &str) = (
    "Mixed",
    "model Mixed parameter Real k=2; Real q(start=1,fixed=true); \
     Real w(start=0.5,fixed=true); Real s(start=0,fixed=true); Real a; equation a=k*q; \
     der(q)=w; der(w)=-a; der(s)=a; end Mixed;",
);

fn span(at: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("linked-product.mo"),
        at,
        at + 1,
    )
}

pub(super) fn component((name, source): (&str, &str)) -> FmiComponent {
    let mut session = rumoca_compile::Session::default();
    session.add_document(&format!("{name}.mo"), source).unwrap();
    let compiled = session.compile_model(name).unwrap();
    let lowered = rumoca_phase_solve::lower_solve_model_for_profile(
        &compiled.dae,
        &Default::default(),
        Default::default(),
        solve::RefreshProfile::Scalar,
        |_| {},
    )
    .unwrap();
    rumoca_phase_solve::fmi::finish_fmi_component(lowered).unwrap()
}

/// A model whose continuous refresh issues an exact assignment schedule.
pub(super) fn exact_model() -> solve::SolveModel {
    let mut session = rumoca_compile::Session::default();
    session
        .add_document(
            "exact-wire.mo",
            "model ExactWire parameter Real gain=2; Real x(start=1,fixed=true); Real y; \
             equation der(x)=-gain*x; y=x+1; end ExactWire;",
        )
        .unwrap();
    let compiled = session.compile_model("ExactWire").unwrap();
    rumoca_phase_solve::lower_solve_model_for_profile(
        &compiled.dae,
        &Default::default(),
        Default::default(),
        solve::RefreshProfile::Scalar,
        |_| {},
    )
    .unwrap()
    .into_model()
}

pub(super) fn exact_schedule(model: &solve::SolveModel) -> &solve::ExactRefreshAssignmentSchedule {
    model
        .problem
        .continuous
        .refresh_owners
        .exact_assignment_schedules()
        .iter()
        .find(|schedule| !schedule.program_ids().is_empty())
        .unwrap()
}

pub(super) fn empty_calls() -> SolvePureCallTable {
    SolvePureCallTable::builder(solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    ))
    .finish()
}

/// Two ordered rows writing one slot, then a second slot.
pub(super) fn issued_rows() -> IssuedAssignmentRows {
    IssuedAssignmentRows::new(
        vec![
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Unary {
                    dst: 1,
                    op: UnaryOp::Sin,
                    arg: 0,
                },
                LinearOp::StoreOutput { src: 1 },
            ],
        ],
        vec![span(1), span(2)],
        vec![0, 1],
        2,
    )
    .unwrap()
}

pub(super) fn issued_layout() -> VarLayout {
    VarLayout::from_parts(Default::default(), 2, 1)
}

/// Three private programs over one layout sharing one module; the third is
/// the first again, so its issued key equals the first's.
pub(super) fn private_blocks() -> Vec<ScalarProgramBlock> {
    let first = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Sin,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    let second = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    [(first.clone(), 10), (second, 20), (first, 10)]
        .into_iter()
        .map(|(program, at)| {
            ScalarProgramBlock::with_program_spans(vec![program], vec![span(at)]).unwrap()
        })
        .collect()
}

pub(super) fn private_layout() -> VarLayout {
    VarLayout::from_parts(Default::default(), 2, 1)
}

/// The retained event whose integer quotient faults on a zero denominator.
pub(super) fn event_program() -> solve::RetainedEventProgram {
    crate::typed_call::retained_event::tests::source()
        .event_program(solve::RetainedAssertionSource::ActionConditions, 0)
        .unwrap()
}
