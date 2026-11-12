//! Each persisted field must match nonactivated authoritative emission.
use super::*;

// The baseline replay substitutes only these three API adapters. The mutation
// generator, positive controls and refusal assertions stay byte-identical.
fn bind_ordinary(
    wire: &PrecompiledExactAssignments,
    source: &ComputeBlock,
    owners: &ContinuousRefreshOwners,
    schedule: &ExactRefreshAssignmentSchedule,
    layout: &VarLayout,
    calls: &SolvePureCallTable,
) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
    wire.bind(source, owners, schedule, layout, calls)
}

fn bind_rows(
    wire: &PrecompiledExactAssignments,
    rows: &IssuedAssignmentRows,
    layout: &VarLayout,
    calls: &SolvePureCallTable,
) -> Result<CompiledExactAssignmentWasm, WasmCompileError> {
    wire.bind_issued(rows, layout, calls)
}

fn schedule_key(
    source: &ComputeBlock,
    owners: &ContinuousRefreshOwners,
    schedule: &ExactRefreshAssignmentSchedule,
    layout: &VarLayout,
    calls: &SolvePureCallTable,
) -> Result<String, WasmCompileError> {
    rumoca_ir_solve::PreparedKernelSource::new(layout, calls)
        .and_then(|key| key.exact_assignment_schedule(source, owners, schedule))
        .map_err(WasmCompileError::Input)
}

fn fault_entries() -> [serde_json::Value; 3] {
    let typed = crate::TypedCallFault {
        status: 1000,
        owner: rumoca_ir_solve::SolvePureCallOwnerId::LIFECYCLE_METHOD,
        operation: Some(0),
        region_path: Vec::new(),
        opcode: "forged".into(),
        kind: crate::TypedCallFaultKind::IndexBounds,
        provenance: rumoca_core::Span::DUMMY,
    };
    let gather = crate::NativeGatherFault {
        status: 1000,
        kind: crate::TypedCallFaultKind::IndexBounds,
        kernel: 0,
        program: 0,
        operation: 0,
        region_path: Vec::new(),
        provenance: rumoca_core::Span::DUMMY,
    };
    let linear = crate::NativeLinearSolveFault {
        status: 1000,
        size: 1,
        component: 0,
        kernel: 0,
        program: 0,
        operation: 0,
        region_path: Vec::new(),
        provenance: rumoca_core::Span::DUMMY,
    };
    [
        serde_json::to_value(typed).unwrap(),
        serde_json::to_value(gather).unwrap(),
        serde_json::to_value(linear).unwrap(),
    ]
}

fn mutations(original: &serde_json::Value) -> Vec<(&'static str, serde_json::Value)> {
    let mut module = original.clone();
    let last = module["payload"]["bytes"]
        .as_array_mut()
        .unwrap()
        .last_mut()
        .unwrap();
    *last = (last.as_u64().unwrap() ^ 1).into();
    let mut scratch = original.clone();
    let size = scratch["payload"]["scratch_bytes"].as_u64().unwrap();
    scratch["payload"]["scratch_bytes"] = (size + 8).into();
    let mut cases = vec![
        ("payload.bytes", module),
        ("payload.scratch_bytes", scratch),
    ];
    for ((name, label), entry) in [
        ("faults", "payload.faults"),
        ("gather", "payload.gather"),
        ("linear", "payload.linear"),
    ]
    .into_iter()
    .zip(fault_entries())
    {
        let mut changed = original.clone();
        changed["payload"][name].as_array_mut().unwrap().push(entry);
        cases.push((label, changed));
    }
    for ((name, label), entry) in [("gather", "outer.gather"), ("linear", "outer.linear")]
        .into_iter()
        .zip(fault_entries().into_iter().skip(1))
    {
        let mut changed = original.clone();
        changed[name].as_array_mut().unwrap().push(entry);
        cases.push((label, changed));
    }
    assert_eq!(cases.len(), 7);
    cases
}

fn check(
    code: &CompiledExactAssignmentWasm,
    bind: impl Fn(&PrecompiledExactAssignments) -> Result<CompiledExactAssignmentWasm, WasmCompileError>,
    selected: Option<&str>,
) {
    let wire = PrecompiledExactAssignments::from_compiled(code);
    let value = serde_json::to_value(&wire).unwrap();
    let replay: PrecompiledExactAssignments = serde_json::from_value(value.clone()).unwrap();
    let loaded = bind(&replay).unwrap();
    assert_eq!(loaded.module_bytes(), code.module_bytes());
    assert_eq!(loaded.scratch_bytes(), code.scratch_bytes());
    let cases = mutations(&value);
    if let Some(wanted) = selected {
        assert_eq!(cases.iter().filter(|(name, _)| *name == wanted).count(), 1);
    }
    for (field, changed) in cases
        .into_iter()
        .filter(|(name, _)| selected.is_none_or(|wanted| *name == wanted))
    {
        let changed: PrecompiledExactAssignments = serde_json::from_value(changed).unwrap();
        let Err(error) = bind(&changed) else {
            panic!("modified {field} must refuse before runtime activation");
        };
        assert!(
            error.to_string().contains("authoritative issued emission"),
            "{field}: {error}"
        );
    }
    for field in ["version", "source_key", "y", "p"] {
        let mut changed = value.clone();
        changed[field] = if field == "source_key" {
            "foreign-source-key".into()
        } else {
            (changed[field].as_u64().unwrap() + 1).into()
        };
        let changed: PrecompiledExactAssignments = serde_json::from_value(changed).unwrap();
        assert!(bind(&changed).is_err(), "foreign header {field}");
    }
}

fn make_model() -> rumoca_ir_solve::SolveModel {
    let mut session = rumoca_compile::Session::default();
    session.add_document("exact-bind.mo", "model ExactBind parameter Real p=2; Real x(start=1,fixed=true); Real a; Real b; equation a=3*p; b=a*x; der(x)=b; end ExactBind;").unwrap();
    let compiled = session.compile_model("ExactBind").unwrap();
    rumoca_phase_solve::lower_solve_model_for_profile(
        &compiled.dae,
        &Default::default(),
        Default::default(),
        rumoca_ir_solve::RefreshProfile::Scalar,
        |_| {},
    )
    .unwrap()
    .into_model()
}

#[test]
fn ordinary_artifact_rederives_payload_and_both_outer_fault_tables() {
    let model = make_model();
    let layout = &model.problem.layout;
    let source = &model.problem.continuous.implicit_rhs;
    let owners = &model.problem.continuous.refresh_owners;
    let schedule = owners
        .exact_assignment_schedules()
        .iter()
        .find(|schedule| !schedule.program_ids().is_empty())
        .unwrap();
    let code =
        compile_exact_assignment_schedule_wasm(source, owners, schedule, layout, &model.pure_calls)
            .unwrap();
    check(
        &code,
        |wire| bind_ordinary(wire, source, owners, schedule, layout, &model.pure_calls),
        None,
    );
    let wire = PrecompiledExactAssignments::from_compiled(&code);
    let fresh = make_model();
    let fresh_owners = &fresh.problem.continuous.refresh_owners;
    let fresh_schedule = fresh_owners
        .exact_assignment_schedule(schedule.sequence_id())
        .unwrap();
    assert!(
        bind_ordinary(
            &wire,
            &fresh.problem.continuous.implicit_rhs,
            fresh_owners,
            fresh_schedule,
            &fresh.problem.layout,
            &fresh.pure_calls,
        )
        .is_ok()
    );
    let other = owners
        .exact_assignment_schedules()
        .iter()
        .find(|other| {
            !other.program_ids().is_empty() && other.sequence_id() != schedule.sequence_id()
        })
        .expect("fixture needs two separately issued schedule selections");
    assert!(bind_ordinary(&wire, source, owners, other, layout, &model.pure_calls).is_err());
    assert!(
        bind_ordinary(
            &wire,
            &ComputeBlock::default(),
            owners,
            schedule,
            layout,
            &model.pure_calls,
        )
        .is_err()
    );
    let foreign_layout = VarLayout::from_parts(
        Default::default(),
        layout.y_scalars() + 1,
        layout.p_scalars(),
    );
    assert!(
        bind_ordinary(
            &wire,
            source,
            owners,
            schedule,
            &foreign_layout,
            &model.pure_calls
        )
        .is_err()
    );
    assert!(bind_ordinary(&wire, source, owners, schedule, layout, &foreign_calls()).is_err());
}

fn foreign_calls() -> SolvePureCallTable {
    SolvePureCallTable::builder(rumoca_ir_solve::SolveArithmeticProfile::construct(
        rumoca_ir_solve::SolveRealFormat::Binary32,
        rumoca_ir_solve::SolveIntegerDomain::FULL,
    ))
    .finish()
}

#[test]
fn ordinary_key_names_its_domain_selected_schedule_and_context() {
    let model = make_model();
    let source = &model.problem.continuous.implicit_rhs;
    let owners = &model.problem.continuous.refresh_owners;
    let layout = &model.problem.layout;
    let mut schedules = owners
        .exact_assignment_schedules()
        .iter()
        .filter(|schedule| !schedule.program_ids().is_empty());
    let first = schedules.next().expect("one nonempty issued schedule");
    let other = schedules.next().expect("a different issued schedule");
    let key = schedule_key(source, owners, first, layout, &model.pure_calls).unwrap();
    let legacy = rumoca_ir_solve::PreparedKernelSource::new(layout, &model.pure_calls)
        .unwrap()
        .exact_assignments(source)
        .unwrap();
    assert_ne!(key, legacy, "executable and bare-source domains differ");
    assert_ne!(
        key,
        schedule_key(source, owners, other, layout, &model.pure_calls).unwrap(),
        "one original source can select different issued schedules"
    );
    let fresh = make_model();
    let fresh_owners = &fresh.problem.continuous.refresh_owners;
    assert_eq!(
        key,
        schedule_key(
            &fresh.problem.continuous.implicit_rhs,
            fresh_owners,
            fresh_owners
                .exact_assignment_schedule(first.sequence_id())
                .unwrap(),
            &fresh.problem.layout,
            &fresh.pure_calls,
        )
        .unwrap()
    );
    assert_ne!(
        key,
        schedule_key(source, owners, first, layout, &foreign_calls()).unwrap()
    );
    let other_layout = VarLayout::from_parts(
        Default::default(),
        layout.y_scalars() + 1,
        layout.p_scalars(),
    );
    assert_ne!(
        key,
        schedule_key(source, owners, first, &other_layout, &model.pure_calls).unwrap()
    );
}

/// Issued rows carry the provenance of a source program.
fn row_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("rows.mo"), 1, 2)
}

fn rows(target: usize, slots: usize) -> IssuedAssignmentRows {
    IssuedAssignmentRows::new(
        vec![vec![
            rumoca_ir_solve::LinearOp::Const { dst: 0, value: 3.0 },
            rumoca_ir_solve::LinearOp::StoreOutput { src: 0 },
        ]],
        vec![row_span()],
        vec![target],
        slots,
    )
    .unwrap()
}

fn ordered_rows(reverse: bool) -> IssuedAssignmentRows {
    let values = if reverse { [2.0, 1.0] } else { [1.0, 2.0] };
    let programs = values
        .into_iter()
        .map(|value| {
            vec![
                rumoca_ir_solve::LinearOp::Const { dst: 0, value },
                rumoca_ir_solve::LinearOp::StoreOutput { src: 0 },
            ]
        })
        .collect();
    IssuedAssignmentRows::new(programs, vec![row_span(); 2], vec![0, 0], 1).unwrap()
}

#[test]
fn issued_artifact_rederives_payload_and_both_outer_fault_tables() {
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let calls = SolvePureCallTable::default();
    let source = rows(0, 1);
    let code = compile_issued_assignment_rows_wasm(&source, &layout, &calls).unwrap();
    check(
        &code,
        |wire| bind_rows(wire, &source, &layout, &calls),
        None,
    );
    let wire = PrecompiledExactAssignments::from_compiled(&code);
    assert!(bind_rows(&wire, &rows(0, 1), &layout, &calls).is_ok());
    assert!(bind_rows(&wire, &source, &layout, &foreign_calls()).is_err());
    let seeded = source.clone().with_seed(0..1).unwrap();
    assert!(bind_rows(&wire, &seeded, &layout, &calls).is_err());
    let different_slots = rows(0, 2);
    assert!(bind_rows(&wire, &different_slots, &layout, &calls).is_err());
    let different_program = ordered_rows(false);
    assert!(bind_rows(&wire, &different_program, &layout, &calls).is_err());
    let different_target = rows(1, 2);
    let other_layout = VarLayout::from_parts(Default::default(), 2, 0);
    assert!(bind_rows(&wire, &different_target, &other_layout, &calls).is_err());
    let ordered = ordered_rows(false);
    let code = compile_issued_assignment_rows_wasm(&ordered, &layout, &calls).unwrap();
    let wire = PrecompiledExactAssignments::from_compiled(&code);
    assert!(bind_rows(&wire, &ordered_rows(true), &layout, &calls).is_err());
}

fn ordinary_field(field: &str) {
    let model = make_model();
    let source = &model.problem.continuous.implicit_rhs;
    let owners = &model.problem.continuous.refresh_owners;
    let layout = &model.problem.layout;
    let schedule = owners
        .exact_assignment_schedules()
        .iter()
        .find(|schedule| !schedule.program_ids().is_empty())
        .unwrap();
    let code =
        compile_exact_assignment_schedule_wasm(source, owners, schedule, layout, &model.pure_calls)
            .unwrap();
    check(
        &code,
        |wire| bind_ordinary(wire, source, owners, schedule, layout, &model.pure_calls),
        Some(field),
    );
}

fn issued_field(field: &str) {
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let calls = SolvePureCallTable::default();
    let source = rows(0, 1);
    let code = compile_issued_assignment_rows_wasm(&source, &layout, &calls).unwrap();
    check(
        &code,
        |wire| bind_rows(wire, &source, &layout, &calls),
        Some(field),
    );
}

// Each field has its own test-body RED on the baseline. One early panic must
// not prevent the other independently forged fields from being exercised.
macro_rules! mutation_controls {
    ($($name:ident, $run:ident, $field:literal;)*) => {
        $(#[test] fn $name() { $run($field); })*
    };
}

mutation_controls! {
    ordinary_module_refuses, ordinary_field, "payload.bytes";
    ordinary_scratch_refuses, ordinary_field, "payload.scratch_bytes";
    ordinary_typed_faults_refuse, ordinary_field, "payload.faults";
    ordinary_payload_gather_refuses, ordinary_field, "payload.gather";
    ordinary_payload_linear_refuses, ordinary_field, "payload.linear";
    ordinary_outer_gather_refuses, ordinary_field, "outer.gather";
    ordinary_outer_linear_refuses, ordinary_field, "outer.linear";
    issued_module_refuses, issued_field, "payload.bytes";
    issued_scratch_refuses, issued_field, "payload.scratch_bytes";
    issued_typed_faults_refuse, issued_field, "payload.faults";
    issued_payload_gather_refuses, issued_field, "payload.gather";
    issued_payload_linear_refuses, issued_field, "payload.linear";
    issued_outer_gather_refuses, issued_field, "outer.gather";
    issued_outer_linear_refuses, issued_field, "outer.linear";
}

#[test]
fn issued_key_and_bind_refuse_a_target_only_change() {
    let layout = VarLayout::from_parts(Default::default(), 2, 0);
    let calls = SolvePureCallTable::default();
    let first = rows(0, 2);
    let other = rows(1, 2);
    let context = rumoca_ir_solve::PreparedKernelSource::new(&layout, &calls).unwrap();
    assert_eq!(
        serde_json::to_value(first.rows()).unwrap(),
        serde_json::to_value(other.rows()).unwrap()
    );
    assert_eq!(first.slots(), other.slots());
    assert_eq!(first.seed(), other.seed());
    assert_eq!(
        first.chunks().collect::<Vec<_>>(),
        other.chunks().collect::<Vec<_>>()
    );
    assert_ne!(
        context.issued_assignment_rows(&first).unwrap(),
        context.issued_assignment_rows(&other).unwrap()
    );
    let code = compile_issued_assignment_rows_wasm(&first, &layout, &calls).unwrap();
    let wire = PrecompiledExactAssignments::from_compiled(&code);
    assert!(bind_rows(&wire, &first, &layout, &calls).is_ok());
    assert!(bind_rows(&wire, &other, &layout, &calls).is_err());
}

#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
#[test]
fn unadmitted_engine_refuses_without_mutating_inputs() {
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let calls = SolvePureCallTable::default();
    let source = rows(0, 1);
    let original = compile_issued_assignment_rows_wasm(&source, &layout, &calls).unwrap();
    let wire = PrecompiledExactAssignments::from_compiled(&original);
    let loaded = bind_rows(&wire, &source, &layout, &calls).unwrap();
    let mut y = [f64::from_bits(0x3ff0_0000_0000_0001)];
    let before = y.map(f64::to_bits);
    let mut scratch = vec![0.0; (loaded.scratch_bytes() as usize).div_ceil(8)];
    let error = loaded.call(&mut y, &[], 0.0, &mut scratch).unwrap_err();
    assert!(error.to_string().contains("no admitted engine"));
    assert_eq!(y.map(f64::to_bits), before);
}
