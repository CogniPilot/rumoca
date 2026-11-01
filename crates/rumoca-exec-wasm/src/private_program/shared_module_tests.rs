//! Entries of one shared module return the bits of standalone modules.
use super::*;
use crate::host_runtime::HostKernelRuntime;
use rumoca_ir_solve::{self as solve, BinaryOp, LinearOp, TensorInputKind, UnaryOp};
use std::num::NonZeroU64;

const Y: usize = 8;
const SEED: usize = 8;

fn span(offset: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("shared_private_module.mo"),
        offset,
        offset + 1,
    )
}

fn table() -> (SolvePureCallTable, solve::SolvePureCallSite) {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut builder = SolvePureCallTable::builder(profile);
    let owner = builder
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span(1),
            |body, inputs, outputs| {
                let input = body.load(inputs[0], span(2))?;
                let integer = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    input,
                    span(3),
                )?;
                let value = body.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    integer,
                    span(4),
                )?;
                body.store(outputs[0], value, span(5))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

/// A `1 x inner` by `inner x 1` product of Y through the register arena.
fn dot(inner: usize, lanes: usize) -> Vec<LinearOp> {
    let width = (2 * inner * lanes) as solve::Reg;
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2 * inner,
            seed_start: (lanes == 2).then_some(0),
            lanes,
        },
        LinearOp::MatrixMultiply {
            dst_start: width,
            lhs_start: 0,
            rhs_start: (inner * lanes) as solve::Reg,
            rows: 1,
            inner,
            columns: 1,
            lanes,
        },
        LinearOp::StoreOutputRange {
            start: width,
            count: lanes,
            stride: 1,
        },
    ]
}

fn sources(site: &solve::SolvePureCallSite) -> Vec<(ScalarProgramBlock, Option<usize>)> {
    let call = |dst: solve::Reg, input: solve::Reg| LinearOp::PureCall {
        dst_start: dst,
        input_starts: vec![input].into(),
        site: site.clone(),
    };
    let programs = vec![
        (
            vec![
                LinearOp::LoadY { dst: 0, index: 2 },
                LinearOp::Unary {
                    dst: 1,
                    op: UnaryOp::Sin,
                    arg: 0,
                },
                LinearOp::LoadP { dst: 2, index: 0 },
                LinearOp::Binary {
                    dst: 3,
                    op: BinaryOp::Pow,
                    lhs: 1,
                    rhs: 2,
                },
                LinearOp::StoreOutput { src: 3 },
                LinearOp::StoreOutput { src: 1 },
            ],
            None,
        ),
        (
            vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                call(1, 0),
                LinearOp::StoreOutput { src: 1 },
            ],
            None,
        ),
        (dot(4, 1), None),
        (dot(2, 2), Some(SEED)),
        (
            vec![
                LinearOp::LoadY { dst: 0, index: 5 },
                LinearOp::LoadSeed { dst: 1, index: 3 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::Unary {
                    dst: 3,
                    op: UnaryOp::Cos,
                    arg: 0,
                },
                LinearOp::StoreOutput { src: 2 },
                LinearOp::StoreOutput { src: 3 },
            ],
            Some(SEED - 2),
        ),
        (
            vec![
                LinearOp::LoadY { dst: 0, index: 7 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                call(3, 2),
                LinearOp::StoreOutput { src: 3 },
                LinearOp::StoreOutput { src: 2 },
            ],
            None,
        ),
    ];
    programs
        .into_iter()
        .enumerate()
        .map(|(index, (ops, seed))| {
            let block =
                ScalarProgramBlock::with_program_spans(vec![ops], vec![span(100 + index)]).unwrap();
            (block, seed)
        })
        .collect()
}

/// The previous standalone module of one program, run on its own instance.
struct Reference {
    runtime: HostKernelRuntime,
    artifact: CompiledNativeCallProgramWasm,
    seed_offset: Option<usize>,
}

impl Reference {
    fn new(
        block: &ScalarProgramBlock,
        seed: Option<usize>,
        layout: &VarLayout,
        table: &SolvePureCallTable,
    ) -> Self {
        let (artifact, seed_offset) = match seed {
            Some(count) => {
                let (artifact, offset) =
                    crate::emit::emit_private_directional_module(block, layout, table, count)
                        .unwrap();
                (artifact, Some(offset as usize / 8))
            }
            None => (
                crate::emit::emit_private_program_module(block, layout, table).unwrap(),
                None,
            ),
        };
        let runtime =
            HostKernelRuntime::new_named(artifact.module_bytes(), "eval_private").unwrap();
        Self {
            runtime,
            artifact,
            seed_offset,
        }
    }

    fn call(&self, y: &[f64], p: &[f64], seed: &[f64], count: Option<usize>) -> Outcome {
        let mut cells = vec![19f64.to_bits(); self.artifact.scratch_bytes() as usize / 8];
        if let (Some(offset), Some(count)) = (self.seed_offset, count) {
            for (cell, value) in cells[offset..offset + count].iter_mut().zip(seed) {
                *cell = value.to_bits();
            }
        }
        let status = self
            .runtime
            .call_native(&mut y.to_vec(), p, 0.25, &mut cells, &mut [])
            .unwrap();
        if status != 0 {
            return Outcome::Fault(fault(self.artifact.status_error(status)));
        }
        Outcome::Values(cells)
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Outcome {
    Values(Vec<u64>),
    Fault(String),
}

fn fault(error: WasmCompileError) -> String {
    match error {
        WasmCompileError::TypedSource(fault) => format!("{:?} {:?}", fault.kind, fault.provenance),
        other => format!("{other}"),
    }
}

fn shared(compiled: &CompiledPrivateProgramWasm, y: &[f64], p: &[f64], seed: &[f64]) -> Outcome {
    // Stale values from other entries stay in the shared buffer.
    let mut cells = vec![19.0; compiled.scratch_bytes() as usize / 8];
    let result = match compiled.seed {
        Some(_) => compiled.call_directional(y, p, 0.25, seed, &mut cells),
        None => compiled.call(y, p, 0.25, &mut cells),
    };
    match result {
        Ok(()) => Outcome::Values(cells.iter().map(|value| value.to_bits()).collect()),
        Err(error) => Outcome::Fault(fault(error)),
    }
}

fn tuple(outcome: Outcome, count: usize) -> Outcome {
    match outcome {
        Outcome::Values(cells) => Outcome::Values(cells[..count].to_vec()),
        fault => fault,
    }
}

#[test]
fn every_entry_of_one_shared_module_returns_its_standalone_module_bits() {
    let (table, site) = table();
    let sources = sources(&site);
    let layout = VarLayout::from_parts(Default::default(), Y, 1);
    let entries = sources
        .iter()
        .map(|(block, seed)| (block, *seed))
        .collect::<Vec<_>>();
    let compiled = compile_private_programs_wasm(&entries, &layout, &table).unwrap();
    assert_eq!(compiled.len(), sources.len());
    let bytes = compiled[0].module_bytes();
    assert!(
        compiled
            .iter()
            .all(|entry| std::ptr::eq(entry.module_bytes(), bytes))
    );
    assert!(
        !bytes
            .windows(b"rumoca_private_arena".len())
            .any(|name| name == b"rumoca_private_arena"),
        "a shared module defines its register memory instead of leasing it"
    );
    let references = sources
        .iter()
        .map(|(block, seed)| Reference::new(block, *seed, &layout, &table))
        .collect::<Vec<_>>();
    let inputs = [
        (
            [0.5, -2.75, 0.3, -0.0, 1e16, 0.7, 3.0, -1.5],
            [2.5],
            [0.25, -1.0, 2.0, -0.0, 0.5, 1.5, -3.0, 4.0],
        ),
        (
            [-1.0, f64::INFINITY, -0.0, 0.0, -1e16, f64::NAN, 1.0, 9.5],
            [-0.5],
            [1.0, 0.0, -0.0, 1e-300, -2.0, 0.125, 7.0, -0.5],
        ),
    ];
    let mut mismatches = Vec::new();
    let mut faults = 0;
    for (y, p, seed) in &inputs {
        // Forward, reverse and repeated orders leave other entries' state behind.
        let order = (0..sources.len())
            .chain((0..sources.len()).rev())
            .chain([3, 3, 2, 2]);
        for entry in order {
            let count = compiled[entry].output_count();
            let expected = tuple(references[entry].call(y, p, seed, sources[entry].1), count);
            let actual = tuple(shared(&compiled[entry], y, p, seed), count);
            faults += usize::from(matches!(actual, Outcome::Fault(_)));
            if actual != expected {
                mismatches.push(format!("entry {entry}: {actual:?} vs {expected:?}"));
            }
        }
    }
    assert!(mismatches.is_empty(), "{mismatches:#?}");
    assert!(
        faults > 0,
        "the infinite call input must reach a checked fault"
    );
}

#[test]
fn shared_storage_is_sized_for_its_widest_entry() {
    let (table, site) = table();
    let sources = sources(&site);
    let layout = VarLayout::from_parts(Default::default(), Y, 1);
    let entries = sources
        .iter()
        .map(|(block, seed)| (block, *seed))
        .collect::<Vec<_>>();
    let compiled = compile_private_programs_wasm(&entries, &layout, &table).unwrap();
    let widest = sources
        .iter()
        .map(|(block, seed)| {
            Reference::new(block, *seed, &layout, &table)
                .artifact
                .scratch_bytes()
        })
        .max()
        .unwrap();
    assert!(compiled[0].scratch_bytes() >= widest);
}

#[test]
fn an_empty_or_unchecked_entry_list_is_refused() {
    let (table, site) = table();
    let layout = VarLayout::from_parts(Default::default(), Y, 1);
    assert!(compile_private_programs_wasm(&[], &layout, &table).is_err());
    let sources = sources(&site);
    // A directional source admitted as primal is refused for the whole module.
    let entries = [(&sources[0].0, None), (&sources[4].0, None)];
    assert!(compile_private_programs_wasm(&entries, &layout, &table).is_err());
}

/// A call owner the module would refuse only at linkage: a recursive group.
fn recursive_call() -> (SolvePureCallTable, solve::SolvePureCallSite) {
    let profile = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut builder = SolvePureCallTable::builder(profile);
    let members = (0..2)
        .map(|i| {
            solve::SolveRecursiveMember::new(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(10 + i).unwrap()),
                vec![real.clone()],
                vec![solve::SolvePureCallOutput::result(real.clone())],
                span(10 + i as usize),
            )
        })
        .collect();
    let group = builder
        .add_recursive_group(members, |ordinal, ids, body, inputs, outputs| {
            let value = body.load(inputs[0], span(20))?;
            let result = body.call(ids[(ordinal + 1) % ids.len()], &[value], span(21))?;
            body.store(outputs[0], result[0], span(22))
        })
        .unwrap();
    let site = builder.call_site(group[0]).unwrap();
    (builder.finish(), site)
}

#[test]
fn admission_raises_a_linkage_refusal_before_the_shared_module_is_emitted() {
    let (table, site) = recursive_call();
    let layout = VarLayout::from_parts(Default::default(), Y, 1);
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::PureCall {
                dst_start: 1,
                input_starts: vec![0].into(),
                site,
            },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![span(200)],
    )
    .unwrap();
    let ordinary = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![span(201)],
    )
    .unwrap();
    let table = Rc::new(table);
    let admission = PrivateProgramAdmission::new(layout.clone(), Rc::clone(&table));
    assert!(admission.admit(&ordinary, None).is_ok());
    let refused = admission.admit(&block, None).unwrap_err().to_string();
    // The same refusal, raised again from the cached linkage, and by the module.
    assert_eq!(
        admission.admit(&block, None).unwrap_err().to_string(),
        refused
    );
    assert!(compile_private_programs_wasm(&[(&block, None)], &layout, &table).is_err());
    assert!(admission.admit(&ordinary, None).is_ok());
}

#[test]
fn every_admitted_entry_of_the_fixture_is_emitted_by_its_shared_module() {
    let (table, site) = table();
    let sources = sources(&site);
    let layout = VarLayout::from_parts(Default::default(), Y, 1);
    let table = Rc::new(table);
    let admission = PrivateProgramAdmission::new(layout.clone(), Rc::clone(&table));
    for (block, seed) in &sources {
        admission.admit(block, *seed).unwrap();
    }
    let entries = sources
        .iter()
        .map(|(block, seed)| (block, *seed))
        .collect::<Vec<_>>();
    assert_eq!(
        compile_private_programs_wasm(&entries, &layout, &table)
            .unwrap()
            .len(),
        sources.len()
    );
}
