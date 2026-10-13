//! Central reported sources through checked main assignment owners and real WASM.
use super::*;

const GUARDED: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_403.mo"
));
const INTEGER: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_411.mo"
));

struct SourceProgram {
    compiled: rumoca_exec_wasm::CompiledNativeCallProgramWasm,
    runner: ProgramRunner,
    layout: solve::VarLayout,
    parameters: Vec<f64>,
    input_bytes: usize,
    input_slots: Vec<(solve::ScalarSlot, usize)>,
    integer_output: Option<usize>,
}

impl SourceProgram {
    fn new(source: &str, model: &str) -> Self {
        let mut session = rumoca_compile::Session::default();
        session.add_document("ReportedBug.mo", source).unwrap();
        let model = session
            .compile_model(model)
            .expect("the reported source compiles");
        let package = rumoca_phase_solve::lower_solve_package(&model.dae)
            .expect("the actual main equations lower");
        let problem = &package.problem;
        let owners = &problem.continuous.refresh_owners;
        let schedule = owners.native_assignment_schedule().unwrap_or_else(|| {
            panic!(
                "the main assignment owner is required: {:?}",
                owners.native_assignment_refusal()
            )
        });
        let input_slots = schedule
            .scalar_input_lanes()
            .enumerate()
            .map(|(ordinal, input)| {
                assert_eq!(input.lane(), solve::NativeOutputLane::Integer);
                assert_eq!(input.lane_offset(), ordinal * 8);
                (solve::scalar_slot_p(input.p_index()), input.lane_offset())
            })
            .collect::<Vec<_>>();
        let input_bytes = schedule.input_lane_bytes();
        assert_eq!(input_bytes, input_slots.len() * 8);
        assert!(!input_slots.is_empty());
        let integer_output = schedule.derived_outputs().iter().find_map(|output| {
            (problem.layout.binding("y") == Some(solve::scalar_slot_p(output.p_index()))).then(
                || {
                    assert_eq!(output.lane(), solve::NativeOutputLane::Integer);
                    input_bytes + output.lane_offset()
                },
            )
        });
        let compiled = compile_native_assignment_schedule_with_calls_wasm(
            schedule,
            &problem.layout,
            &package.pure_calls,
        )
        .expect("the actual issued assignment program compiles to WASM");
        let runner = ProgramRunner::new_with_lanes(
            &compiled,
            &problem.layout,
            input_bytes + schedule.lane_bytes(),
        );
        eprintln!(
            "source WASM: {} call owners, {} stages, {} module bytes",
            package.pure_calls.owners().len(),
            schedule.stages().len(),
            compiled.module_bytes().len()
        );
        Self {
            compiled,
            runner,
            layout: problem.layout.clone(),
            parameters: vec![0.; problem.layout.p_scalars()],
            input_bytes,
            input_slots,
            integer_output,
        }
    }

    fn run(&mut self, input: i64) -> (i32, Vec<f64>, Vec<u8>) {
        assert_eq!(self.input_bytes, 8);
        self.runner
            .run_with_lanes(&self.parameters, &input.to_le_bytes())
    }

    fn run_named(&mut self, inputs: &[(&str, i64)]) -> (i32, Vec<f64>, Vec<u8>) {
        assert_eq!(inputs.len(), self.input_slots.len());
        let mut bytes = vec![0; self.input_bytes];
        for (name, value) in inputs {
            let slot = self.layout.binding(name).expect("authored input binding");
            let (_, offset) = self
                .input_slots
                .iter()
                .find(|(input, _)| *input == slot)
                .expect("the input owns an exact Integer lane");
            bytes[*offset..*offset + 8].copy_from_slice(&value.to_le_bytes());
        }
        self.runner.run_with_lanes(&self.parameters, &bytes)
    }

    fn real_output(&self, values: &[f64]) -> f64 {
        let Some(solve::ScalarSlot::Y { index, .. }) = self.layout.binding("y") else {
            panic!("the original Real output has a checked Y slot");
        };
        values[index]
    }

    fn integer_output(&self, lanes: &[u8]) -> i64 {
        let offset = self
            .integer_output
            .expect("the original output has an issued i64 lane");
        assert!(offset >= self.input_bytes);
        i64::from_le_bytes(lanes[offset..offset + 8].try_into().unwrap())
    }

    fn fault(&self, status: i32, kind: TypedCallFaultKind) {
        assert_ne!(status, 0);
        let fault = self
            .compiled
            .faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .expect("source-bound typed fault");
        assert_eq!(fault.kind, kind);
        assert!(fault.operation.is_some());
        assert!(!fault.provenance.is_dummy());
        eprintln!("reported source typed fault: {fault:?}");
    }
}

#[test]
fn issue_403_original_scalar_boolean_guard_short_circuits_in_wasm() {
    let mut program = SourceProgram::new(GUARDED, "GuardedOperands");
    let (status, values, _) = program.run(3);
    assert_eq!(status, 0);
    assert_eq!(program.real_output(&values), 1.);
    let (status, values, _) = program.run(4);
    assert_eq!(
        status, 0,
        "the inactive original a[4] access must not execute"
    );
    assert_eq!(program.real_output(&values), 0.);
}

#[test]
fn issue_403_explicit_if_protects_the_inactive_access_in_wasm() {
    let safe = GUARDED.replace(
        "if k <= n and a[k] > 5 then 1 else 0",
        "if k <= n then (if a[k] > 5 then 1 else 0) else 0",
    );
    assert_ne!(safe, GUARDED);
    let mut program = SourceProgram::new(&safe, "GuardedOperands");
    for (input, expected) in [(4, 0.), (3, 1.), (4, 0.)] {
        let (status, values, _) = program.run(input);
        assert_eq!(status, 0);
        assert_eq!(program.real_output(&values), expected);
    }
}

#[test]
fn issue_403_active_invalid_access_keeps_its_typed_wasm_fault() {
    let active = GUARDED.replace("k, 3);", "k, 4);");
    assert_ne!(active, GUARDED);
    let mut program = SourceProgram::new(&active, "GuardedOperands");
    let (status, _, _) = program.run(4);
    program.fault(status, TypedCallFaultKind::IndexBounds);
    let (status, values, _) = program.run(3);
    assert_eq!(status, 0);
    assert_eq!(program.real_output(&values), 1.);
}

#[test]
fn issue_403_or_short_circuits_the_original_dynamic_access_in_wasm() {
    let source = GUARDED.replace("k <= n and", "k > n or");
    let mut program = SourceProgram::new(&source, "GuardedOperands");
    for (input, expected) in [(4, 1.), (3, 1.), (2, 0.), (4, 1.)] {
        let (status, values, _) = program.run(input);
        assert_eq!(status, 0);
        assert_eq!(program.real_output(&values), expected);
    }
}

#[test]
fn issue_403_independently_demanded_rhs_keeps_its_wasm_fault() {
    let source = GUARDED.replace(
        "if k <= n and a[k] > 5 then 1 else 0",
        "(if k <= n and a[k] > 5 then 1 else 0) + a[k]",
    );
    let mut program = SourceProgram::new(&source, "GuardedOperands");
    let (status, _, _) = program.run(4);
    program.fault(status, TypedCallFaultKind::IndexBounds);
    let (status, values, _) = program.run(3);
    assert_eq!(status, 0);
    assert_eq!(program.real_output(&values), 10.);
}

#[test]
fn issue_411_main_integer_mod_and_div_publish_exact_i64_wasm_lanes() {
    for modulo in [true, false] {
        let source = if modulo {
            INTEGER.to_owned()
        } else {
            INTEGER.replace("mod(", "div(")
        };
        let mut program = SourceProgram::new(&source, "IntegerPrecision");
        for input in [
            0,
            1,
            2,
            -1,
            9_007_199_254_740_993,
            -9_007_199_254_740_993,
            i64::MIN,
        ] {
            let dividend = 9_007_199_254_740_993_i128 + i128::from(input);
            let expected = if modulo {
                dividend.rem_euclid(7)
            } else {
                dividend / 7
            };
            let (status, _, lanes) = program.run(input);
            assert_eq!(status, 0, "modulo={modulo}, input={input}");
            assert_eq!(i128::from(program.integer_output(&lanes)), expected);
        }
    }
}

fn integer_expected(operator: &str, dividend: i128, divisor: i128) -> i128 {
    match operator {
        "div" => dividend / divisor,
        "rem" => dividend % divisor,
        "mod" => {
            let remainder = dividend % divisor;
            if remainder != 0 && (remainder < 0) != (divisor < 0) {
                remainder + divisor
            } else {
                remainder
            }
        }
        _ => panic!("the authored quotient roster is finite"),
    }
}

#[test]
fn main_integer_quotients_preserve_signed_dynamic_divisors_and_checked_faults() {
    for operator in ["div", "mod", "rem"] {
        let source = INTEGER
            .replace(
                "input Integer n(start = 0);",
                "input Integer n(start = 0); input Integer d(start = 7);",
            )
            .replace("mod(", &format!("{operator}("))
            .replace(", 7);", ", d);");
        let mut program = SourceProgram::new(&source, "IntegerPrecision");
        for (input, divisor) in [
            (0, 7),
            (0, 1),
            (1, -7),
            (-9_007_199_254_740_994, 7),
            (0, i64::MIN),
            (i64::MIN, 7),
        ] {
            let expected = integer_expected(
                operator,
                9_007_199_254_740_993_i128 + i128::from(input),
                i128::from(divisor),
            );
            let (status, _, lanes) = program.run_named(&[("n", input), ("d", divisor)]);
            assert_eq!(status, 0, "{operator}, input={input}, divisor={divisor}");
            assert_eq!(i128::from(program.integer_output(&lanes)), expected);
        }
        for (input, divisor) in [(0, 0), (i64::MAX, 7)] {
            let (status, _, _) = program.run_named(&[("n", input), ("d", divisor)]);
            program.fault(status, TypedCallFaultKind::IntegerArithmetic);
        }
        let (status, _, lanes) = program.run_named(&[("n", 0), ("d", 7)]);
        assert_eq!(status, 0);
        assert_eq!(
            i128::from(program.integer_output(&lanes)),
            integer_expected(operator, 9_007_199_254_740_993, 7)
        );
    }
}
