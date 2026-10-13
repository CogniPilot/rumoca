//! Original-source native step timing: compile, bind, then execute the emitted
//! module in-process on wasmi, reporting wall time per call.
use std::{env, error::Error, fs, path::Path, time::Instant};

use rumoca_ir_solve::{NativeOutputLane, scalar_slot_p, scalar_slot_y};
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store};

fn stage<T>(
    name: &str,
    action: impl FnOnce() -> Result<T, Box<dyn Error>>,
) -> Result<(T, f64), Box<dyn Error>> {
    let started = Instant::now();
    eprintln!("NATIVE_STEP stage={name} event=begin");
    match action() {
        Ok(value) => {
            let seconds = started.elapsed().as_secs_f64();
            eprintln!("NATIVE_STEP stage={name} event=end seconds={seconds}");
            Ok((value, seconds))
        }
        Err(error) => {
            eprintln!(
                "NATIVE_STEP stage={name} event=refused seconds={} error={error:?}",
                started.elapsed().as_secs_f64()
            );
            Err(error)
        }
    }
}

fn peak_rss_kib() -> u64 {
    fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|status| {
            status
                .lines()
                .find_map(|line| line.strip_prefix("VmHWM:"))
                .and_then(|rest| rest.trim().trim_end_matches("kB").trim().parse().ok())
        })
        .unwrap_or(0)
}

fn words(bytes: &[u64]) -> Vec<u8> {
    bytes.iter().flat_map(|word| word.to_le_bytes()).collect()
}

fn unary(linker: &mut Linker<()>, name: &str, f: fn(f64) -> f64) -> Result<(), Box<dyn Error>> {
    linker.func_wrap("env", name, f)?;
    Ok(())
}

fn define_math(linker: &mut Linker<()>) -> Result<(), Box<dyn Error>> {
    unary(linker, "abs", f64::abs)?;
    unary(linker, "sin", f64::sin)?;
    unary(linker, "cos", f64::cos)?;
    unary(linker, "tan", f64::tan)?;
    unary(linker, "asin", f64::asin)?;
    unary(linker, "acos", f64::acos)?;
    unary(linker, "atan", f64::atan)?;
    unary(linker, "sinh", f64::sinh)?;
    unary(linker, "cosh", f64::cosh)?;
    unary(linker, "tanh", f64::tanh)?;
    unary(linker, "exp", f64::exp)?;
    unary(linker, "log", f64::ln)?;
    unary(linker, "log10", f64::log10)?;
    linker.func_wrap("env", "pow", |b: f64, e: f64| b.powf(e))?;
    linker.func_wrap("env", "atan2", |y: f64, x: f64| y.atan2(x))?;
    Ok(())
}

fn median(sorted: &[f64]) -> f64 {
    sorted[sorted.len() / 2]
}

type StageSeconds = Vec<(&'static str, f64)>;

/// Linear-memory layout the entry receives: Y, P, scratch, then typed lanes.
struct Abi {
    y_bytes: usize,
    p_offset: usize,
    scratch_offset: usize,
    typed_offset: usize,
    typed_bytes: usize,
    input_lane_bytes: usize,
    pages: usize,
}

/// Everything compiled and bound before any engine runs.
struct Product<'a> {
    host: &'a rumoca_exec_wasm::RetainedNativeWasmHost,
    names: StateNames,
    abi: Abi,
    scratch_bytes: usize,
    unsourced: Vec<serde_json::Value>,
    seconds: StageSeconds,
}

fn le_bytes(values: &[f64]) -> Vec<u8> {
    values.iter().flat_map(|v| v.to_le_bytes()).collect()
}

fn parse_policy(
    capacity: &[String],
) -> Result<rumoca_exec_wasm::CheckedNativeWasmCapacityPolicy, Box<dyn Error>> {
    match capacity {
        [] => Ok(rumoca_exec_wasm::CheckedNativeWasmCapacityPolicy::default()),
        [private, public] | [private, public, _] => {
            Ok(rumoca_exec_wasm::CheckedNativeWasmCapacityPolicy::new(
                rumoca_exec_wasm::NativeWasmCapacityRequest {
                    private_arena_bytes: private.parse()?,
                    public_scratch_bytes: public.parse()?,
                },
            )?)
        }
        _ => Err("capacity takes PRIVATE_BYTES and PUBLIC_BYTES together".into()),
    }
}

/// The `(output, input)` variable pairs a host declares by naming convention:
/// every Y output declared `<output_prefix><rest>` whose `<input_prefix><rest>`
/// input the model admits as its carried pair, and the number of outputs
/// left unpaired. The convention is the driver's own host contract; the
/// compiler receives only the declared pairs.
fn declared_pairs(
    problem: &rumoca_ir_solve::SolveProblem,
    schedule: &rumoca_ir_solve::NativeRefreshAssignmentSchedule,
    names: &StateNames,
) -> (Vec<(String, String)>, usize) {
    let mut pairs = Vec::new();
    let mut unpaired = 0usize;
    for (name, slot) in problem.layout.bindings() {
        let (
            rumoca_ir_solve::ScalarSlot::Y { .. } | rumoca_ir_solve::ScalarSlot::P { .. },
            Some(rest),
        ) = (*slot, name.as_str().strip_prefix(names.output.as_str()))
        else {
            continue;
        };
        let pair = (name.as_str().to_owned(), format!("{}{rest}", names.input));
        let admitted = rumoca_ir_solve::NativeCarriedState::declared(
            problem,
            schedule,
            &[(pair.0.as_str(), pair.1.as_str())],
        )
        .is_ok();
        if admitted {
            pairs.push(pair);
        } else {
            unpaired += 1;
        }
    }
    (pairs, unpaired)
}

fn compile_host(
    source: &str,
    source_path: &str,
    model: &str,
    (policy, carried): (
        rumoca_exec_wasm::CheckedNativeWasmCapacityPolicy,
        Option<&StateNames>,
    ),
) -> Result<(rumoca_exec_wasm::RetainedNativeWasmHost, StageSeconds), Box<dyn Error>> {
    let (compilation, frontend) = stage("frontend", || {
        rumoca::Compiler::new()
            .model(model)
            .compile_str(source, source_path)
            .map_err(|error| error.to_string().into())
    })?;
    let (component, checked) = stage("checked_component", || {
        Ok(rumoca_sim::lower_fmi_component(&compilation.dae)?)
    })?;
    let original = component.runtime_view().retain_assertion_root();
    let (preparation, _) = stage("native_preparation", || Ok(original.native_preparation()?))?;
    let (code, emission) = stage("native_emission", || {
        let state = if let Some(prefixes) = carried {
            let problem = &preparation.root().model().problem;
            let (pairs, unpaired) = declared_pairs(problem, preparation.schedule(), prefixes);
            let names = pairs
                .iter()
                .map(|(output, input)| (output.as_str(), input.as_str()))
                .collect::<Vec<_>>();
            let state = rumoca_ir_solve::NativeCarriedState::declared(
                problem,
                preparation.schedule(),
                &names,
            )?;
            eprintln!(
                "NATIVE_STEP carried_state runs={} unpaired={unpaired}",
                state.runs().len()
            );
            state
        } else {
            Default::default()
        };
        Ok(
            rumoca_exec_wasm::compile_retained_native_preparation_wasm_carrying(
                preparation,
                policy,
                &state,
            )?,
        )
    })?;
    let (host, bind) = stage("bind", || {
        let root = original.resolved_periodic_schedules_at(0.0)?;
        Ok(code.bind_resolved(root)?.into_host())
    })?;
    let seconds = vec![
        ("frontend", frontend),
        ("checked_component", checked),
        ("native_emission", emission),
        ("bind", bind),
    ];
    Ok((host, seconds))
}

fn abi_of(host: &rumoca_exec_wasm::RetainedNativeWasmHost) -> (Abi, usize) {
    let product = host.product();
    let layout = &product.inputs().root().model().problem.layout;
    let schedule = product.inputs().schedule();
    let scratch_bytes = product.compiled().scratch_bytes() as usize;
    let y_bytes = layout.y_scalars() * 8;
    let scratch_offset = y_bytes + layout.p_scalars() * 8;
    let typed_offset = (scratch_offset + scratch_bytes).next_multiple_of(8);
    let input_lane_bytes = schedule.input_lane_bytes();
    let typed_bytes = input_lane_bytes + schedule.lane_bytes();
    let abi = Abi {
        y_bytes,
        p_offset: y_bytes,
        scratch_offset,
        typed_offset,
        typed_bytes,
        input_lane_bytes,
        pages: (typed_offset + typed_bytes).div_ceil(65536).max(1),
    };
    (abi, scratch_bytes)
}

/// Costs the wasm32 host path pays per call or per transport, timed in isolation.
fn host_copy_costs(
    lease: &rumoca_exec_wasm::NativeInvocationLease<'_>,
    scratch_bytes: usize,
) -> Result<(f64, f64), Box<dyn Error>> {
    let clone_started = Instant::now();
    let work_y = lease.y().to_vec();
    let work_lanes = lease.typed_lanes().to_vec();
    let clone = clone_started.elapsed().as_secs_f64();
    std::hint::black_box((&work_y, &work_lanes));
    let fill_started = Instant::now();
    let mut scratch: Vec<u64> = Vec::new();
    scratch.try_reserve_exact(scratch_bytes.div_ceil(8))?;
    scratch.resize(scratch_bytes.div_ceil(8), 0);
    let fill = fill_started.elapsed().as_secs_f64();
    std::hint::black_box(&scratch);
    Ok((clone, fill))
}

fn output_descriptors(
    host: &rumoca_exec_wasm::RetainedNativeWasmHost,
    abi: &Abi,
) -> Vec<serde_json::Value> {
    let product = host.product();
    let layout = &product.inputs().root().model().problem.layout;
    product
        .inputs()
        .schedule()
        .derived_outputs()
        .iter()
        .map(|o| {
            serde_json::json!({"p_index": o.p_index(), "lane": o.lane().as_str(),
                "offset": abi.input_lane_bytes + o.lane_offset(),
                "name": layout.scalar_name_for_slot(scalar_slot_p(o.p_index()))})
        })
        .collect()
}

/// The browser engine, not wasmi, is the product target: wasmi refuses the full
/// module (too many function-local constants), so hand the exact initial memory
/// image to an engine harness.
fn export_image(
    directory: &Path,
    source: &str,
    steps: usize,
    product: &Product<'_>,
    lease: &rumoca_exec_wasm::NativeInvocationLease<'_>,
    copies: (f64, f64),
    (frames, slots, plan): (Option<&FrameSource>, &FrameSlots, &CarryPlan),
) -> Result<(), Box<dyn Error>> {
    let compiled = product.host.product().compiled();
    let model = product.host.product().inputs().root().model();
    let layout = &model.problem.layout;
    let abi = &product.abi;
    fs::create_dir_all(directory)?;
    fs::write(directory.join("module.wasm"), compiled.module_bytes())?;
    fs::write(directory.join("y.bin"), le_bytes(lease.y()))?;
    fs::write(directory.join("p.bin"), le_bytes(lease.p()))?;
    fs::write(directory.join("lanes.bin"), words(lease.typed_lanes()))?;
    let y_names: Vec<_> = (0..layout.y_scalars().min(16))
        .map(|i| layout.scalar_name_for_slot(scalar_slot_y(i)))
        .collect();
    let mut description = serde_json::json!({
        "pages": abi.pages, "y_bytes": abi.y_bytes, "p_offset": abi.p_offset,
        "scratch_offset": abi.scratch_offset, "typed_offset": abi.typed_offset,
        "typed_bytes": abi.typed_bytes, "y_count": layout.y_scalars(), "p_count": layout.p_scalars(),
        "outputs": output_descriptors(product.host, abi), "y_names": y_names, "steps": steps,
        "module_bytes": compiled.module_bytes().len(),
        "unsourced_inputs_zero_filled": product.unsourced,
        "frames": frames.map(|s| serde_json::json!({"dir": s.dir, "count": s.count, "interval_time": s.interval_time, "interval_step": s.interval_step,
            "first_epoch": s.first_epoch, "first_frame": s.first_frame, "epoch_every": s.epoch_every, "carry_state": s.carry_state, "slots": slots.json()})),
        "carry": plan.runs.iter().flat_map(|r| [r.from, r.to, r.bytes]).collect::<Vec<_>>(),
        "carried": compiled.carried_state().runs().iter().flat_map(|r| [8 * r.y_range().start, abi.p_offset + 8 * r.p_range().start, 8 * r.scalar_count()]).collect::<Vec<_>>(),
        "carried_lanes": compiled.carried_state().lanes().iter().flat_map(|l| [abi.typed_offset + abi.input_lane_bytes + l.output_range().start, abi.typed_offset + l.input_range().start, l.bytes()]).collect::<Vec<_>>(),
        "settings": plan.settings,
        "state": {"count": plan.previous.len(), "previous_hash": names_hash(plan.previous.keys()), "next_hash": names_hash(plan.outputs.keys())},
        "host_work_buffer_clone_seconds": copies.0, "scratch_zero_fill_seconds": copies.1,
        "peak_rss_kib_before_run": peak_rss_kib(),
        "compile_seconds": product.seconds.iter().cloned().collect::<std::collections::BTreeMap<_, _>>(),
    });
    if let (serde_json::Value::Object(map), serde_json::Value::Object(tables)) = (
        &mut description,
        model_tables(product.host, (abi, &product.names)),
    ) {
        map.extend(tables);
    }
    write_state_files(directory, plan)?;
    fs::write(
        directory.join("layout.json"),
        serde_json::to_vec(&description)?,
    )?;
    let owners: Vec<_> = model
        .pure_calls
        .owners()
        .iter()
        .enumerate()
        .map(|(i, owner)| {
            let span = owner.provenance();
            let (start, end) = (span.start.0, span.end.0);
            serde_json::json!({"owner": i, "start": start, "end": end,
                "source": source.get(start..end.min(start + 160))})
        })
        .collect();
    fs::write(directory.join("owners.json"), serde_json::to_vec(&owners)?)?;
    let names: Vec<(usize, String)> = owners
        .iter()
        .filter_map(|owner| {
            Some((
                owner["owner"].as_u64()? as usize,
                owner["source"].as_str()?.chars().take(80).collect(),
            ))
        })
        .collect();
    fs::write(
        directory.join("function_owners.json"),
        serde_json::to_vec(&function_owners(
            compiled.module_bytes(),
            compiled.scratch_report(),
            &names,
        )?)?,
    )?;
    Ok(())
}

struct Instance {
    store: Store<()>,
    memory: Memory,
    entry: wasmi::TypedFunc<(i32, i32, f64, i32, i32), i32>,
}

fn instantiate(
    module_bytes: &[u8],
    abi: &Abi,
    lease: &rumoca_exec_wasm::NativeInvocationLease<'_>,
) -> Result<Instance, Box<dyn Error>> {
    let engine = Engine::default();
    let mut store = Store::new(&engine, ());
    let module = Module::new(&engine, module_bytes)?;
    let memory = Memory::new(&mut store, MemoryType::new(abi.pages as u32, None))?;
    let mut linker = Linker::new(&engine);
    linker.define("env", "memory", memory)?;
    define_math(&mut linker)?;
    let entry = linker
        .instantiate(&mut store, &module)?
        .start(&mut store)?
        .get_typed_func::<(i32, i32, f64, i32, i32), i32>(&store, "eval_assignments")?;
    memory.write(&mut store, 0, &le_bytes(lease.y()))?;
    memory.write(&mut store, abi.p_offset, &le_bytes(lease.p()))?;
    memory.write(&mut store, abi.typed_offset, &words(lease.typed_lanes()))?;
    Ok(Instance {
        store,
        memory,
        entry,
    })
}

/// Run `steps` calls; stops at the first non-zero status, which is returned.
fn run_steps(
    instance: &mut Instance,
    abi: &Abi,
    steps: usize,
    mut before_step: impl FnMut(&mut Instance, usize) -> Result<(), Box<dyn Error>>,
) -> Result<(Vec<f64>, i32), Box<dyn Error>> {
    let typed_pointer = if abi.typed_bytes == 0 {
        0
    } else {
        abi.typed_offset as i32
    };
    let mut times = Vec::with_capacity(steps);
    let mut status = 0;
    for step in 0..steps {
        before_step(instance, step)?;
        let started = Instant::now();
        status = instance.entry.call(
            &mut instance.store,
            (
                0,
                abi.p_offset as i32,
                step as f64 / 30.0,
                abi.scratch_offset as i32,
                typed_pointer,
            ),
        )?;
        times.push(started.elapsed().as_secs_f64());
        eprintln!(
            "NATIVE_STEP step={step} status={status} seconds={}",
            times[step]
        );
        if status != 0 {
            break;
        }
    }
    Ok((times, status))
}

fn read_values(
    instance: &Instance,
    product: &Product<'_>,
) -> Result<(Vec<serde_json::Value>, Vec<serde_json::Value>), Box<dyn Error>> {
    let abi = &product.abi;
    let layout = &product
        .host
        .product()
        .inputs()
        .root()
        .model()
        .problem
        .layout;
    let mut y = vec![0u8; abi.y_bytes];
    instance.memory.read(&instance.store, 0, &mut y)?;
    let y_values: Vec<f64> = y
        .chunks_exact(8)
        .map(|c| f64::from_le_bytes(c.try_into().expect("8 bytes")))
        .collect();
    let tail_from = y_values.len().saturating_sub(8);
    let y_head = y_values
        .iter()
        .enumerate()
        .filter(|(index, _)| *index < 8 || *index >= tail_from)
        .map(|(index, value)| {
            serde_json::json!({"y_index": index,
                "name": layout.scalar_name_for_slot(scalar_slot_y(index)), "value": value})
        })
        .collect();
    let mut lanes = vec![0u8; abi.typed_bytes];
    instance
        .memory
        .read(&instance.store, abi.typed_offset, &mut lanes)?;
    let outputs = product
        .host
        .product()
        .inputs()
        .schedule()
        .derived_outputs()
        .iter()
        .take(8)
        .map(|output| {
            let at = abi.input_lane_bytes + output.lane_offset();
            let word: [u8; 8] = std::array::from_fn(|i| lanes.get(at + i).copied().unwrap_or(0));
            let value = match output.lane() {
                NativeOutputLane::Real => serde_json::json!(f64::from_le_bytes(word)),
                NativeOutputLane::Integer => serde_json::json!(i64::from_le_bytes(word)),
                NativeOutputLane::Boolean => serde_json::json!(word[0] != 0),
            };
            serde_json::json!({"p_index": output.p_index(),
                "name": layout.scalar_name_for_slot(scalar_slot_p(output.p_index())),
                "lane": output.lane().as_str(), "value": value})
        })
        .collect();
    Ok((y_head, outputs))
}

fn main() -> Result<(), Box<dyn Error>> {
    let (arguments, flags) = split_flags(env::args().collect())?;
    let frames = FrameSource::from_flags(&flags)?;
    let [_, source_path, model, ..] = arguments.as_slice() else {
        return Err(
            "usage: native_step SOURCE MODEL [STEPS [PRIVATE_BYTES PUBLIC_BYTES [EXPORT_DIR]]] [--frames DIR [--interval-time T] [--interval-step DT] [--image-epoch N] [--carry-state] [--carried-state] [--state-prefixes OUTPUT_PREFIX INPUT_PREFIX] [--first-frame K] [--epoch-every N] [--set NAME=V] [--state-csv FILE]]"
                .into(),
        );
    };
    let steps: usize = arguments.get(3).map_or(Ok(10), |n| n.parse())?;
    let names = flags.state_names();
    let policy = parse_policy(&arguments[4.min(arguments.len())..])?;
    let source = fs::read_to_string(source_path)?;
    let (host, mut seconds) = compile_host(
        &source,
        source_path,
        model,
        (policy, flags.carried_state.then_some(&names)),
    )?;
    let resolved = host.product().inputs().root().model();
    let (mut transport, transport_s) = stage("transport_from_source", || {
        Ok(host.transport_from_source(&resolved.initial_y, &resolved.parameters)?)
    })?;
    seconds.push(("transport_from_source", transport_s));
    let (abi, scratch_bytes) = abi_of(&host);
    let mut slots = FrameSlots::default();
    let unsourced = supply_unsourced_inputs(
        &mut transport,
        &resolved.problem.layout,
        &resolved.problem.solve_layout,
        (&host, &abi, frames.as_ref(), &mut slots),
    )?;
    let product = Product {
        host: &host,
        names: names.clone(),
        abi,
        scratch_bytes,
        unsourced,
        seconds,
    };
    let lease = transport.enter()?;
    let mut plan = carry_runs(&host, &product.abi, (&flags.settings, &names))?;
    if let Some(csv) = &flags.state_csv {
        (plan.initial, plan.initial_values) = initial_state(csv, &plan.previous, &names)?;
        eprintln!("NATIVE_STEP initial_state scalars={}", plan.initial.len());
    }
    let (carry, carry_matched, carry_unmatched) = (&plan.runs, plan.matched, plan.unmatched);
    if let Some(s) = frames.as_ref().filter(|s| s.carry_state) {
        eprintln!(
            "NATIVE_STEP carry_state matched={carry_matched} unmatched={carry_unmatched} runs={} frames={}",
            carry.len(),
            s.count
        );
    }
    let copies = host_copy_costs(&lease, product.scratch_bytes)?;
    if let Some(directory) = arguments.get(6) {
        export_image(
            Path::new(directory),
            &source,
            steps,
            &product,
            &lease,
            copies,
            (frames.as_ref(), &slots, &plan),
        )?;
        let module_bytes = host.product().compiled().module_bytes().len();
        println!(
            "{}",
            serde_json::json!({"exported": directory, "module_bytes": module_bytes})
        );
        return Ok(());
    }
    run_in_process(&InProcess {
        model,
        steps,
        product: &product,
        lease: &lease,
        plan: &plan,
        frames: frames.as_ref(),
        slots: &slots,
        copies,
    })
}

/// Inputs with no declared start have no source value; each such run is set
/// explicitly to its type's zero and reported, never silently defaulted.
fn supply_unsourced_inputs(
    transport: &mut rumoca_exec_wasm::NativeTransport,
    layout: &rumoca_ir_solve::VarLayout,
    solve_layout: &rumoca_ir_solve::SolveLayout,
    (host, abi, source, slots): (
        &rumoca_exec_wasm::RetainedNativeWasmHost,
        &Abi,
        Option<&FrameSource>,
        &mut FrameSlots,
    ),
) -> Result<Vec<serde_json::Value>, Box<dyn Error>> {
    let first_frame = source
        .map(|s| s.load(s.first_frame % s.count))
        .transpose()?;
    use rumoca_exec_wasm::{NativeTransportError, NativeTypedInputBatch};
    use rumoca_ir_solve::{ScalarSlot, SolveVariableStorageRole, SolveVariableValueKind};
    let mut filled = Vec::new();
    loop {
        let p_index = match transport.enter() {
            Ok(_) => return Ok(filled),
            Err(NativeTransportError::MissingInput { p_index }) => p_index,
            Err(error) => return Err(error.into()),
        };
        let run = solve_layout
            .variable_storage_runs
            .iter()
            .find(|run| {
                run.role == SolveVariableStorageRole::ExternalInput
                    && matches!(run.base, ScalarSlot::P { index, .. }
                        if (index..index + run.scalar_count).contains(&p_index))
            })
            .ok_or("missing input outside every external input run")?;
        let ScalarSlot::P { index, .. } = run.base else {
            return Err("external input run is not in P".into());
        };
        let count = run.scalar_count;
        let name = layout.scalar_name_for_slot(scalar_slot_p(index));
        let fed = frame_value(source, first_frame.as_ref(), name.as_deref(), count);
        if fed.is_some() {
            let lane = host.product().inputs().schedule().input_lane(index);
            let real_at = abi.p_offset + 8 * index;
            let int_at = lane.as_ref().map(|l| abi.typed_offset + l.lane_offset());
            slots.record(name.as_deref(), count, real_at, int_at);
        }
        match run.value_kind {
            SolveVariableValueKind::Real => {
                let zeros;
                let values: &[f64] = match &fed {
                    Some(FrameValue::Real(values)) => values,
                    _ => {
                        zeros = vec![0.0; count];
                        &zeros
                    }
                };
                transport.set_float64_input_batches(&[(index, values)])?
            }
            SolveVariableValueKind::Integer => transport.set_typed_inputs(
                index,
                NativeTypedInputBatch::IntegerRepeat {
                    value: match fed {
                        Some(FrameValue::Integer(value)) => value,
                        _ => 0,
                    },
                    count,
                },
            )?,
            SolveVariableValueKind::Boolean => transport.set_typed_inputs(
                index,
                NativeTypedInputBatch::BooleanRepeat {
                    value: false,
                    count,
                },
            )?,
            other => return Err(format!("unsupported input kind {other:?}").into()),
        }
        eprintln!(
            "NATIVE_STEP unsourced_input name={name:?} p_index={index} count={count} kind={:?} fed_from_frames={}",
            run.value_kind,
            fed.is_some()
        );
        filled.push(
            serde_json::json!({"name": name, "p_index": index, "count": count,
            "kind": format!("{:?}", run.value_kind), "fed_from_frames": fed.is_some()}),
        );
    }
}

/// Captured camera frames fed to the step in place of zero-filled images.
/// Files `frame-K.rgb8` (height x width x 3 bytes, top-down, RGB) and
/// `frame-K.z16-le` (height x width little-endian u16 raw depth units) hold
/// frame K; the byte values are written unchanged as Real samples.
struct FrameSource {
    dir: std::path::PathBuf,
    count: usize,
    interval_time: f64,
    interval_step: f64,
    first_epoch: i64,
    first_frame: usize,
    epoch_every: usize,
    carry_state: bool,
}

struct Frame {
    rgb: Vec<f64>,
    depth: Vec<f64>,
}

impl FrameSource {
    fn from_flags(flags: &Flags) -> Result<Option<Self>, Box<dyn Error>> {
        let Some(dir) = &flags.frames else {
            if flags.interval_time.is_some()
                || flags.interval_step.is_some()
                || flags.image_epoch.is_some()
                || flags.carry_state
                || flags.first_frame.is_some()
                || flags.epoch_every.is_some()
            {
                return Err(
                    "--interval-time, --image-epoch and --carry-state need --frames".into(),
                );
            }
            return Ok(None);
        };
        let count = (0..)
            .take_while(|k| dir.join(format!("frame-{k}.rgb8")).is_file())
            .count();
        if count == 0 {
            return Err(format!("no frame-0.rgb8 under {}", dir.display()).into());
        }
        Ok(Some(Self {
            dir: dir.into(),
            count,
            interval_time: flags.interval_time.unwrap_or(0.0),
            interval_step: flags.interval_step.unwrap_or(0.0),
            first_epoch: flags.image_epoch.unwrap_or(0),
            first_frame: flags.first_frame.unwrap_or(0),
            epoch_every: flags.epoch_every.unwrap_or(1).max(1),
            carry_state: flags.carry_state,
        }))
    }

    fn load(&self, k: usize) -> Result<Frame, Box<dyn Error>> {
        let rgb = fs::read(self.dir.join(format!("frame-{k}.rgb8")))?;
        let depth = fs::read(self.dir.join(format!("frame-{k}.z16-le")))?;
        Ok(Frame {
            rgb: rgb.iter().map(|&b| f64::from(b)).collect(),
            depth: depth
                .chunks_exact(2)
                .map(|c| f64::from(u16::from_le_bytes([c[0], c[1]])))
                .collect(),
        })
    }
}

/// The name prefixes of the output and input variables of the state a host
/// feeds from one step to the next: the driver's own host contract, declared
/// to the compiler as explicit variable pairs.
#[derive(Clone)]
struct StateNames {
    output: String,
    input: String,
}

#[derive(Default)]
struct Flags {
    frames: Option<std::path::PathBuf>,
    interval_time: Option<f64>,
    interval_step: Option<f64>,
    image_epoch: Option<i64>,
    first_frame: Option<usize>,
    epoch_every: Option<usize>,
    settings: Vec<(String, f64)>,
    state_csv: Option<std::path::PathBuf>,
    carry_state: bool,
    carried_state: bool,
    /// Name prefixes of the output and input variables of the state the driver
    /// feeds from one step to the next.
    state_prefixes: Option<(String, String)>,
}

impl Flags {
    fn state_names(&self) -> StateNames {
        let (output, input) = self
            .state_prefixes
            .clone()
            .unwrap_or_else(|| ("next.".into(), "previous.".into()));
        StateNames { output, input }
    }
}

/// Splits `--flag` options from the positional arguments.
fn split_flags(arguments: Vec<String>) -> Result<(Vec<String>, Flags), Box<dyn Error>> {
    let mut positional = Vec::new();
    let mut flags = Flags::default();
    let mut iter = arguments.into_iter();
    while let Some(argument) = iter.next() {
        let mut value = |name: &str| iter.next().ok_or_else(|| format!("{name} needs a value"));
        match argument.as_str() {
            "--frames" => flags.frames = Some(value("--frames")?.into()),
            "--interval-time" => flags.interval_time = Some(value("--interval-time")?.parse()?),
            "--interval-step" => flags.interval_step = Some(value("--interval-step")?.parse()?),
            "--image-epoch" => flags.image_epoch = Some(value("--image-epoch")?.parse()?),
            "--epoch-every" => flags.epoch_every = Some(value("--epoch-every")?.parse()?),
            "--first-frame" => flags.first_frame = Some(value("--first-frame")?.parse()?),
            "--set" => {
                let pair = value("--set")?;
                let (name, number) = pair.split_once('=').ok_or("--set takes NAME=VALUE")?;
                flags.settings.push((name.to_string(), number.parse()?));
            }
            "--state-csv" => flags.state_csv = Some(value("--state-csv")?.into()),
            "--carry-state" => flags.carry_state = true,
            "--carried-state" => flags.carried_state = true,
            "--state-prefixes" => {
                flags.state_prefixes =
                    Some((value("--state-prefixes")?, value("--state-prefixes")?));
            }
            _ => positional.push(argument),
        }
    }
    Ok((positional, flags))
}

/// Where each frame-fed input lives in linear memory.
#[derive(Default)]
struct FrameSlots {
    rgb: Option<(usize, usize)>,
    depth: Option<(usize, usize)>,
    interval_time: Option<usize>,
    image_epoch: Option<usize>,
}

impl FrameSlots {
    fn json(&self) -> serde_json::Value {
        serde_json::json!({"rgb": self.rgb, "depth": self.depth,
            "interval_time": self.interval_time, "image_epoch": self.image_epoch})
    }
}

/// Moves `previous.*` inputs from the matching `next.*` outputs between steps.
/// Each run copies `bytes` from `from` to `to` (absolute linear-memory offsets).
struct CarryRun {
    from: usize,
    to: usize,
    bytes: usize,
}

struct CarryPlan {
    runs: Vec<CarryRun>,
    matched: usize,
    unmatched: usize,
    /// `previous.*` input name (without the prefix) and its lane and offset.
    previous: std::collections::BTreeMap<String, (NativeOutputLane, usize)>,
    /// Offsets of the Real inputs named by `--set`.
    settings: Vec<(String, usize, f64)>,
    /// `next.*` outputs by name without the prefix: lane and absolute offset.
    outputs: std::collections::BTreeMap<String, (NativeOutputLane, usize)>,
    /// Values of `previous.*` inputs read from a state CSV.
    initial: Vec<(NativeOutputLane, usize, f64)>,
    /// The same values in canonical (sorted name) order.
    initial_values: Vec<f64>,
}

fn carry_runs(
    host: &rumoca_exec_wasm::RetainedNativeWasmHost,
    abi: &Abi,
    (settings, names): (&[(String, f64)], &StateNames),
) -> Result<CarryPlan, Box<dyn Error>> {
    use rumoca_ir_solve::{ScalarSlot, SolveVariableStorageRole};
    let product = host.product();
    let model = product.inputs().root().model();
    let layout = &model.problem.layout;
    let schedule = product.inputs().schedule();
    let mut inputs = std::collections::BTreeMap::new();
    let mut found = Vec::new();
    let external = model
        .problem
        .solve_layout
        .variable_storage_runs
        .iter()
        .filter(|run| run.role == SolveVariableStorageRole::ExternalInput)
        .filter_map(|run| match run.base {
            ScalarSlot::P { index, .. } => Some(index..index + run.scalar_count),
            _ => None,
        })
        .flatten();
    for p in external {
        let Some(name) = layout.scalar_name_for_slot(scalar_slot_p(p)) else {
            continue;
        };
        if let Some((_, value)) = settings.iter().find(|(n, _)| *n == name) {
            schedule
                .input_lane(p)
                .map_or(Ok(()), |_| Err(format!("--set {name} is not a Real input")))?;
            found.push((name, abi.p_offset + 8 * p, *value));
            continue;
        }
        let Some(rest) = name.strip_prefix(names.input.as_str()) else {
            continue;
        };
        let target = match schedule.input_lane(p) {
            Some(lane) => (lane.lane(), abi.typed_offset + lane.lane_offset()),
            None => (NativeOutputLane::Real, abi.p_offset + 8 * p),
        };
        inputs.insert(rest.to_string(), target);
    }
    let carried = host.product().compiled().carried_state();
    let (mut runs, mut matched, mut unmatched): (Vec<CarryRun>, usize, usize) = (vec![], 0, 0);
    let mut outputs = std::collections::BTreeMap::new();
    for output in schedule.derived_outputs() {
        let Some(name) = layout.scalar_name_for_slot(scalar_slot_p(output.p_index())) else {
            continue;
        };
        let Some(rest) = name.strip_prefix(names.output.as_str()) else {
            continue;
        };
        match inputs.get(rest) {
            Some(&(lane, to)) if lane == output.lane() => {
                matched += 1;
                if carried.lane_of_output(output.lane_offset()).is_some() {
                    // The module publishes this cell into the input lane itself.
                    outputs.insert(rest.to_string(), (lane, to));
                } else {
                    let from = abi.typed_offset + abi.input_lane_bytes + output.lane_offset();
                    outputs.insert(rest.to_string(), (lane, from));
                    push_run(&mut runs, from, to, lane.width());
                }
            }
            _ => unmatched += 1,
        }
    }
    for index in 0..layout.y_scalars() {
        let Some(name) = layout.scalar_name_for_slot(scalar_slot_y(index)) else {
            continue;
        };
        let Some(rest) = name.strip_prefix(names.output.as_str()) else {
            continue;
        };
        match inputs.get(rest) {
            Some(&(NativeOutputLane::Real, to)) => {
                matched += 1;
                if carried.run_of_y(index).is_some() {
                    // The module publishes this cell into the input itself.
                    outputs.insert(rest.to_string(), (NativeOutputLane::Real, to));
                } else {
                    outputs.insert(rest.to_string(), (NativeOutputLane::Real, 8 * index));
                    push_run(&mut runs, 8 * index, to, 8);
                }
            }
            _ => unmatched += 1,
        }
    }
    if let Some((name, _)) = settings
        .iter()
        .find(|(n, _)| !found.iter().any(|(f, _, _)| f == n))
    {
        return Err(format!("--set {name}: no such external input").into());
    }
    Ok(CarryPlan {
        runs,
        matched,
        unmatched,
        previous: inputs,
        settings: found,
        outputs,
        initial: Vec::new(),
        initial_values: Vec::new(),
    })
}

fn write_memory(
    instance: &mut Instance,
    offset: usize,
    bytes: &[u8],
) -> Result<(), Box<dyn Error>> {
    instance.memory.write(&mut instance.store, offset, bytes)?;
    Ok(())
}

/// Feeds frame `k` and, from the second step, the carried state.
fn feed_step(
    instance: &mut Instance,
    source: &FrameSource,
    slots: &FrameSlots,
    carry: &[CarryRun],
    step: usize,
) -> Result<(), Box<dyn Error>> {
    if step > 0 && source.carry_state {
        for run in carry {
            let mut bytes = vec![0u8; run.bytes];
            instance
                .memory
                .read(&instance.store, run.from, &mut bytes)?;
            write_memory(instance, run.to, &bytes)?;
        }
    }
    let image = step / source.epoch_every;
    if step.is_multiple_of(source.epoch_every) {
        let frame = source.load((source.first_frame + image) % source.count)?;
        if let Some((offset, _)) = slots.rgb {
            write_memory(instance, offset, &le_bytes(&frame.rgb))?;
        }
        if let Some((offset, _)) = slots.depth {
            write_memory(instance, offset, &le_bytes(&frame.depth))?;
        }
    }
    if let Some(offset) = slots.interval_time {
        let time = source.interval_time + source.interval_step * step as f64;
        write_memory(instance, offset, &time.to_le_bytes())?;
    }
    if let Some(offset) = slots.image_epoch {
        let epoch = source.first_epoch + image as i64;
        write_memory(instance, offset, &epoch.to_le_bytes())?;
    }
    Ok(())
}

/// Scalar outputs that are not state, plus true counts of Boolean arrays.
fn output_summary(
    host: &rumoca_exec_wasm::RetainedNativeWasmHost,
    (abi, names): (&Abi, &StateNames),
    lanes: &[u8],
) -> serde_json::Value {
    let product = host.product();
    let layout = &product.inputs().root().model().problem.layout;
    let mut scalars = serde_json::Map::new();
    let mut true_counts: std::collections::BTreeMap<String, (usize, usize)> = Default::default();
    for output in product.inputs().schedule().derived_outputs() {
        let Some(name) = layout.scalar_name_for_slot(scalar_slot_p(output.p_index())) else {
            continue;
        };
        if name.starts_with(names.output.as_str()) {
            continue;
        }
        let at = abi.input_lane_bytes + output.lane_offset();
        let word: [u8; 8] = std::array::from_fn(|i| lanes.get(at + i).copied().unwrap_or(0));
        let value = match output.lane() {
            NativeOutputLane::Real => serde_json::json!(f64::from_le_bytes(word)),
            NativeOutputLane::Integer => serde_json::json!(i64::from_le_bytes(word)),
            NativeOutputLane::Boolean => serde_json::json!(word[0] != 0),
        };
        match name.split_once('[') {
            None => {
                scalars.insert(name, value);
            }
            Some((base, _)) if output.lane() == NativeOutputLane::Boolean => {
                let entry = true_counts.entry(base.to_string()).or_default();
                entry.0 += usize::from(word[0] != 0);
                entry.1 += 1;
            }
            Some(_) => {}
        }
    }
    serde_json::json!({"scalars": scalars, "boolean_arrays_true_of_total": true_counts})
}

enum FrameValue {
    Real(Vec<f64>),
    Integer(i64),
}

/// The frame-file value for an unsourced input, when frames are supplied and
/// the input is one of the four the application feeds each step.
fn frame_value(
    source: Option<&FrameSource>,
    frame: Option<&Frame>,
    name: Option<&str>,
    count: usize,
) -> Option<FrameValue> {
    let (source, frame, name) = (source?, frame?, name?);
    match name {
        n if n.starts_with("rgb[") && count == frame.rgb.len() => {
            Some(FrameValue::Real(frame.rgb.clone()))
        }
        n if n.starts_with("depth[") && count == frame.depth.len() => {
            Some(FrameValue::Real(frame.depth.clone()))
        }
        "intervalTime" if count == 1 => Some(FrameValue::Real(vec![source.interval_time])),
        "imageEpoch" if count == 1 => Some(FrameValue::Integer(source.first_epoch)),
        _ => None,
    }
}

impl FrameSlots {
    fn record(&mut self, name: Option<&str>, count: usize, real_at: usize, int_at: Option<usize>) {
        match name {
            Some(n) if n.starts_with("rgb[") => self.rgb = Some((real_at, count)),
            Some(n) if n.starts_with("depth[") => self.depth = Some((real_at, count)),
            Some("intervalTime") => self.interval_time = Some(real_at),
            Some("imageEpoch") => self.image_epoch = int_at,
            _ => {}
        }
    }
}

fn lane_summary(
    instance: &Instance,
    product: &Product<'_>,
) -> Result<serde_json::Value, Box<dyn Error>> {
    let mut lanes = vec![0u8; product.abi.typed_bytes];
    instance
        .memory
        .read(&instance.store, product.abi.typed_offset, &mut lanes)?;
    Ok(output_summary(
        product.host,
        (&product.abi, &product.names),
        &lanes,
    ))
}

/// Splits one CSV record, honouring double-quoted fields that contain commas.
fn csv_fields(line: &str) -> Vec<&str> {
    let (mut fields, mut start, mut quoted) = (Vec::new(), 0, false);
    for (i, c) in line.char_indices() {
        match c {
            '"' => quoted = !quoted,
            ',' if !quoted => {
                fields.push(line[start..i].trim_matches('"'));
                start = i + 1;
            }
            _ => {}
        }
    }
    fields.push(line[start..].trim_matches('"'));
    fields
}

/// Reads the last row of a simulation CSV of a state constructor: column
/// `next.NAME` supplies input `previous.NAME` of the stepped model.
/// `(lane, offset, value)` per state scalar, then the values in sorted-name order.
type InitialState = (Vec<(NativeOutputLane, usize, f64)>, Vec<f64>);

fn initial_state(
    path: &Path,
    previous: &std::collections::BTreeMap<String, (NativeOutputLane, usize)>,
    names: &StateNames,
) -> Result<InitialState, Box<dyn Error>> {
    let text = fs::read_to_string(path)?;
    let mut lines = text.lines().filter(|l| !l.is_empty());
    let header = csv_fields(lines.next().ok_or("empty state CSV")?);
    let row = csv_fields(lines.next_back().ok_or("state CSV has no data row")?);
    if header.len() != row.len() {
        return Err("state CSV header and row differ in length".into());
    }
    let mut by_name = std::collections::BTreeMap::new();
    for (name, value) in header.iter().zip(&row) {
        if let Some(rest) = name
            .strip_prefix(names.output.as_str())
            .filter(|r| previous.contains_key(*r))
        {
            by_name.insert(rest, value.parse::<f64>()?);
        }
    }
    let mut triples = Vec::new();
    let mut canonical = Vec::new();
    for (name, (lane, offset)) in previous {
        let value = *by_name
            .get(name.as_str())
            .ok_or_else(|| format!("state CSV has no column {}{name}", names.output))?;
        triples.push((*lane, *offset, value));
        canonical.push(value);
    }
    Ok((triples, canonical))
}

/// Appends a copy, extending the previous run when both ends are adjacent.
fn push_run(runs: &mut Vec<CarryRun>, from: usize, to: usize, bytes: usize) {
    match runs.last_mut() {
        Some(last) if last.from + last.bytes == from && last.to + last.bytes == to => {
            last.bytes += bytes
        }
        _ => runs.push(CarryRun { from, to, bytes }),
    }
}

fn names_hash<'a>(names: impl Iterator<Item = &'a String>) -> String {
    let mut hash: u64 = 0xcbf29ce484222325;
    for name in names {
        for byte in name.bytes().chain(std::iter::once(b'\n')) {
            hash = (hash ^ u64::from(byte)).wrapping_mul(0x100000001b3);
        }
    }
    format!("{hash:016x}")
}

/// Binary state maps for cross-module transfer, one entry per state scalar in
/// sorted-name order: `state_in.bin` holds the offset of each `previous.*`
/// input, `state_out.bin` the offset of each `next.*` output, `state_lane.bin`
/// the lane (0 Real, 1 Integer, 2 Boolean) and `state_init.bin` the CSV values.
fn write_state_files(directory: &Path, plan: &CarryPlan) -> Result<(), Box<dyn Error>> {
    let offsets = |map: &std::collections::BTreeMap<String, (NativeOutputLane, usize)>| {
        map.values()
            .flat_map(|(_, at)| (*at as u32).to_le_bytes())
            .collect::<Vec<u8>>()
    };
    let lanes: Vec<u8> = plan
        .previous
        .values()
        .map(|(lane, _)| match lane {
            NativeOutputLane::Real => 0,
            NativeOutputLane::Integer => 1,
            NativeOutputLane::Boolean => 2,
        })
        .collect();
    fs::write(directory.join("state_in.bin"), offsets(&plan.previous))?;
    fs::write(directory.join("state_out.bin"), offsets(&plan.outputs))?;
    fs::write(directory.join("state_lane.bin"), lanes)?;
    fs::write(
        directory.join("state_init.bin"),
        le_bytes(&plan.initial_values),
    )?;
    Ok(())
}

/// A carried-state scalar run: variable `name` (without `previous.`/`next.`)
/// whose elements from `first` live at `previous` and `next` byte offsets.
struct StateSlot {
    slot: rumoca_ir_solve::ScalarSlot,
    count: usize,
}

/// Layout facts shared by the declared-variable tables.
struct Tables<'a> {
    abi: &'a Abi,
    schedule: &'a rumoca_ir_solve::NativeRefreshAssignmentSchedule,
    /// `p_index -> lane offset` of every derived output.
    output_lane: std::collections::HashMap<usize, usize>,
}

impl Tables<'_> {
    /// Byte offset and lane through which the host writes P scalar `p`.
    fn input_at(&self, p: usize) -> (NativeOutputLane, usize) {
        match self.schedule.input_lane(p) {
            Some(lane) => (lane.lane(), self.abi.typed_offset + lane.lane_offset()),
            None => (NativeOutputLane::Real, self.abi.p_offset + 8 * p),
        }
    }

    /// Byte offset of scalar `ordinal` of the `next.*` variable at `slot`.
    fn next_at(&self, slot: rumoca_ir_solve::ScalarSlot, ordinal: usize) -> Option<usize> {
        use rumoca_ir_solve::ScalarSlot;
        match slot {
            ScalarSlot::Y { index, .. } => Some(8 * (index + ordinal)),
            ScalarSlot::P { index, .. } => self
                .output_lane
                .get(&(index + ordinal))
                .map(|offset| self.abi.typed_offset + self.abi.input_lane_bytes + offset),
            _ => None,
        }
    }

    /// Runs of consecutive state scalars that are adjacent at both ends.
    fn state_runs(
        &self,
        previous: &std::collections::BTreeMap<String, StateSlot>,
        next: &std::collections::BTreeMap<String, StateSlot>,
    ) -> Vec<serde_json::Value> {
        previous
            .iter()
            .filter_map(|(name, from)| Some((name, from, next.get(name)?)))
            .flat_map(|(name, from, to)| self.variable_runs(name, from, to))
            .collect()
    }

    fn variable_runs(
        &self,
        name: &str,
        from: &StateSlot,
        to: &StateSlot,
    ) -> Vec<serde_json::Value> {
        let rumoca_ir_solve::ScalarSlot::P { index: first, .. } = from.slot else {
            return Vec::new();
        };
        let (mut runs, mut open) = (Vec::new(), None::<OpenRun>);
        for ordinal in 0..from.count {
            let (lane, previous) = self.input_at(first + ordinal);
            let Some(next) = self.next_at(to.slot, ordinal) else {
                continue;
            };
            open = match open.take() {
                Some(run) if run.extends(lane, previous, next) => Some(run.grown()),
                closed => {
                    runs.extend(closed.map(|run| run.json(name)));
                    Some(OpenRun {
                        first: ordinal,
                        previous,
                        next,
                        lane,
                        count: 1,
                    })
                }
            };
        }
        runs.extend(open.map(|run| run.json(name)));
        runs
    }
}

/// A growing run of state scalars adjacent in both the input and output side.
struct OpenRun {
    first: usize,
    previous: usize,
    next: usize,
    lane: NativeOutputLane,
    count: usize,
}

impl OpenRun {
    fn extends(&self, lane: NativeOutputLane, previous: usize, next: usize) -> bool {
        let span = self.count * lane.width();
        self.lane == lane && self.previous + span == previous && self.next + span == next
    }

    fn grown(self) -> Self {
        Self {
            count: self.count + 1,
            ..self
        }
    }

    fn json(&self, name: &str) -> serde_json::Value {
        serde_json::json!({"name": name, "lane": self.lane.as_str(), "previous": self.previous,
            "next": self.next, "count": self.count, "first": self.first})
    }
}

/// Declared-variable tables for a host that reads and writes the module by
/// name: top-level outputs in Y, declared inputs in P, and the carried state
/// as `previous.*` / `next.*` runs. Names, shapes and slots come from the
/// layout's declaration owner, never from splitting flattened scalar names.
fn model_tables(
    host: &rumoca_exec_wasm::RetainedNativeWasmHost,
    (abi, names): (&Abi, &StateNames),
) -> serde_json::Value {
    use rumoca_ir_solve::{ScalarSlot, SolveVariableStorageRole as Role};
    let product = host.product();
    let model = product.inputs().root().model();
    let layout = &model.problem.layout;
    let schedule = product.inputs().schedule();
    let mut roles = std::collections::HashMap::new();
    for run in &model.problem.solve_layout.variable_storage_runs {
        let key = match run.base {
            ScalarSlot::Y { index, .. } => (true, index),
            ScalarSlot::P { index, .. } => (false, index),
            _ => continue,
        };
        roles.insert(key, (run.role, run.value_kind));
    }
    let tables = Tables {
        abi,
        schedule,
        output_lane: schedule
            .derived_outputs()
            .iter()
            .map(|o| (o.p_index(), o.lane_offset()))
            .collect(),
    };
    let (mut model_outputs, mut model_inputs) = (Vec::new(), Vec::new());
    let mut previous = std::collections::BTreeMap::new();
    let mut next = std::collections::BTreeMap::new();
    for (name, &slot) in layout.bindings() {
        let name = name.as_str();
        let shape = layout.shape(name).map(<[usize]>::to_vec);
        let count = shape.as_ref().map_or(1, |s| s.iter().product::<usize>());
        let (y, index) = match slot {
            ScalarSlot::Y { index, .. } => (true, index),
            ScalarSlot::P { index, .. } => (false, index),
            _ => continue,
        };
        let state = StateSlot { slot, count };
        if let Some(rest) = name.strip_prefix(names.output.as_str()) {
            next.insert(rest.to_string(), state);
            continue;
        }
        if let (Some(rest), false) = (name.strip_prefix(names.input.as_str()), y) {
            previous.insert(rest.to_string(), state);
            continue;
        }
        match roles.get(&(y, index)) {
            Some((Role::Output, _)) if y => model_outputs.push(serde_json::json!({
                "name": name, "region": "y", "index": index, "offset": 8 * index,
                "count": count, "shape": shape})),
            Some((Role::ExternalInput, kind)) if !y => {
                let (lane, offset) = tables.input_at(index);
                model_inputs.push(serde_json::json!({"name": name, "region": "p",
                    "p_index": index, "offset": offset, "lane": lane.as_str(),
                    "count": count, "kind": format!("{kind:?}"), "shape": shape}));
            }
            _ => {}
        }
    }
    serde_json::json!({"model_outputs": model_outputs, "model_inputs": model_inputs,
        "state_runs": tables.state_runs(&previous, &next)})
}

/// Everything the in-process wasmi run needs beyond the compiled product.
struct InProcess<'a> {
    model: &'a str,
    steps: usize,
    product: &'a Product<'a>,
    lease: &'a rumoca_exec_wasm::NativeInvocationLease<'a>,
    plan: &'a CarryPlan,
    frames: Option<&'a FrameSource>,
    slots: &'a FrameSlots,
    copies: (f64, f64),
}

fn run_in_process(run: &InProcess<'_>) -> Result<(), Box<dyn Error>> {
    let InProcess {
        model,
        steps,
        product,
        lease,
        plan,
        frames,
        slots,
        copies,
    } = *run;
    let host = product.host;
    let layout = &host.product().inputs().root().model().problem.layout;
    let carry = &plan.runs;
    let (mut instance, instantiate_s) = stage("instantiate", || {
        instantiate(
            host.product().compiled().module_bytes(),
            &product.abi,
            lease,
        )
    })?;
    let mut summaries = Vec::new();
    let (times, status) = run_steps(&mut instance, &product.abi, steps, |instance, step| {
        if step > 0 {
            summaries.push(lane_summary(instance, product)?);
        }
        if step == 0 {
            for (lane, offset, value) in &plan.initial {
                let bytes = match lane {
                    NativeOutputLane::Real => value.to_le_bytes().to_vec(),
                    NativeOutputLane::Integer => (*value as i64).to_le_bytes().to_vec(),
                    NativeOutputLane::Boolean => vec![u8::from(*value != 0.0)],
                };
                write_memory(instance, *offset, &bytes)?;
            }
            for (_, offset, value) in &plan.settings {
                write_memory(instance, *offset, &value.to_le_bytes())?;
            }
        }
        match frames {
            Some(source) => feed_step(instance, source, slots, carry, step),
            None => Ok(()),
        }
    })?;
    summaries.push(lane_summary(&instance, product)?);
    let (y_head, outputs) = read_values(&instance, product)?;
    let steady = if times.len() > 1 {
        &times[1..]
    } else {
        &times[..]
    };
    let mut sorted = steady.to_vec();
    sorted.sort_by(f64::total_cmp);
    println!(
        "{}",
        serde_json::json!({
            "scope": "original-source native WASM in-process step on wasmi",
            "model": model, "steps_requested": steps, "steps_run": times.len(), "final_status": status,
            "seconds": {"compile": product.seconds.iter().cloned().collect::<std::collections::BTreeMap<_, _>>(),
                "instantiate": instantiate_s, "first_step": times.first(), "steady_min": sorted.first(),
                "steady_median": (!sorted.is_empty()).then(|| median(&sorted)), "steady_max": sorted.last(),
                "total_steps": times.iter().sum::<f64>(),
                "host_work_buffer_clone": copies.0, "scratch_zero_fill": copies.1},
            "frames_dir": frames.map(|s| s.dir.display().to_string()),
            "output_summaries": summaries,
            "unsourced_inputs_zero_filled": product.unsourced,
            "module_bytes": host.product().compiled().module_bytes().len(), "memory_pages": product.abi.pages,
            "y_scalars": layout.y_scalars(), "p_scalars": layout.p_scalars(), "scratch_bytes": product.scratch_bytes,
            "typed_lane_bytes": product.abi.typed_bytes, "peak_rss_kib": peak_rss_kib(),
            "y_head": y_head, "derived_outputs_head": outputs,
            "derived_output_count": host.product().inputs().schedule().derived_outputs().len(),
        })
    );
    Ok(())
}

/// Imported functions declared by one import section.
fn imported_function_count(
    section: wasmparser::ImportSectionReader<'_>,
) -> Result<usize, Box<dyn Error>> {
    let mut count = 0usize;
    for import in section {
        if matches!(import?.ty, wasmparser::TypeRef::Func(_)) {
            count += 1;
        }
    }
    Ok(count)
}

/// Function indices one function body calls, in operator order.
fn call_targets(body: wasmparser::FunctionBody<'_>) -> Result<Vec<usize>, Box<dyn Error>> {
    let mut targets = Vec::new();
    for operator in body.get_operators_reader()? {
        if let wasmparser::Operator::Call { function_index } = operator? {
            targets.push(function_index as usize);
        }
    }
    Ok(targets)
}

/// Label every outlined part reachable from an owner entry with that owner:
/// a defined function below the first stage group that no owner has claimed.
fn label_owner_parts(
    label: &mut [(Option<usize>, &str)],
    calls: &[Vec<usize>],
    imports: usize,
    owners_len: usize,
    first_group: usize,
    entry: usize,
    owner: usize,
) {
    let mut pending = vec![entry];
    while let Some(function) = pending.pop() {
        for target in calls[function]
            .iter()
            .filter_map(|t| t.checked_sub(imports))
        {
            if target < owners_len || target >= first_group || label[target].0.is_some() {
                continue;
            }
            label[target] = (Some(owner), "owner_part");
            pending.push(target);
        }
    }
}

/// The module's defined functions in emitter order, each with the owner whose
/// body it holds: owner entries first (the scratch report lists the linked
/// owners in the same order), then the continuation parts they outline, then
/// the stage groups the final entry calls, then the entry itself.
fn function_owners(
    module: &[u8],
    report: &rumoca_exec_wasm::ScratchReport,
    owners: &[(usize, String)],
) -> Result<Vec<serde_json::Value>, Box<dyn Error>> {
    use wasmparser::{Parser, Payload};
    let mut imports = 0usize;
    let mut calls: Vec<Vec<usize>> = Vec::new();
    for payload in Parser::new(0).parse_all(module) {
        match payload? {
            Payload::ImportSection(section) => imports += imported_function_count(section)?,
            Payload::CodeSectionEntry(body) => calls.push(call_targets(body)?),
            _ => {}
        }
    }
    let defined = calls.len();
    let entry = defined.checked_sub(1).ok_or("module has no functions")?;
    let name_of = |owner: usize| {
        owners
            .iter()
            .find(|(id, _)| *id == owner)
            .map(|(_, text)| text.clone())
            .unwrap_or_default()
    };
    let mut label: Vec<(Option<usize>, &str)> = vec![(None, "unmapped"); defined];
    let groups = calls[entry]
        .iter()
        .filter_map(|target| target.checked_sub(imports))
        .collect::<Vec<_>>();
    let first_group = groups.iter().copied().min().unwrap_or(entry);
    for &group in &groups {
        label[group] = (None, "stage_group");
    }
    label[entry] = (None, "entry");
    for (position, owner) in report.owners.iter().enumerate().take(first_group) {
        label[position] = (Some(owner.owner), "owner_entry");
        label_owner_parts(
            &mut label,
            &calls,
            imports,
            report.owners.len(),
            first_group,
            position,
            owner.owner,
        );
    }
    Ok(label
        .iter()
        .enumerate()
        .map(|(index, &(owner, kind))| {
            let name = match owner {
                Some(owner) => name_of(owner),
                None => {
                    let callees = calls[index]
                        .iter()
                        .filter_map(|t| t.checked_sub(imports))
                        .filter_map(|t| label.get(t).and_then(|(owner, _)| *owner))
                        .collect::<std::collections::BTreeSet<_>>();
                    format!("{kind} calling owners {callees:?}")
                }
            };
            serde_json::json!({"function": index + imports, "owner": owner,
                "kind": kind, "name": name})
        })
        .collect())
}
