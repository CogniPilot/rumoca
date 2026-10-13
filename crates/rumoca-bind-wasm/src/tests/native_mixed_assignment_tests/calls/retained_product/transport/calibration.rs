use super::*;

// SPEC_0022 FUNC-023/024 (MLS12.4.4), ARR contracts (MLS10), and
// SPEC_0036 Exact Declaration Initialization: the source binding and the input
// binding use the same canonical NumericEvaluator execution owner.
#[test]
fn original_slam_calibration_parameters_and_input_bindings_keep_full_dimensions() {
    let _lock = session_test_guard();
    for (model, height, width) in [
        ("CalibrationBase", 90.0, 160.0),
        ("CalibrationD435", 480.0, 848.0),
    ] {
        with_source_component(include_str!("slam_calibration.mo"), model, |component| {
            let root = component
                .runtime_view()
                .retain_assertion_root()
                .resolved_periodic_schedules_at(0.0)
                .unwrap();
            assert_calibration_bindings(&root, model, height, width);
            assert!(
                !rumoca_ir_solve::solve_has_initialization(&root.model().problem),
                "{model}: original structural dimensions must settle the calibration bindings"
            );
            let inputs = root.native_schedule().unwrap();
            assert!(!inputs.schedule().stages().is_empty());
            let sensor_base = |name| {
                root.metadata()
                    .variables()
                    .iter()
                    .find(|variable| variable.name() == name)
                    .unwrap()
                    .storage()
                    .unwrap()
                    .base()
            };
            let reads = SensorReads::observe(&inputs, sensor_base("rgb"), sensor_base("depth"));
            assert!(
                reads.rgb && reads.depth && reads.arithmetic,
                "{model}: original sensor reads must survive in the genuine Real program"
            );
            // SPEC_0040 SOLVE-C69: the Integer parameters and constants are typed input lanes, so the
            // Real program still has a checked entry; every other input stays a Real P cell.
            assert!(!inputs.schedule().input_runs().is_empty());
            assert!(inputs.schedule().input_runs().iter().all(|run| {
                run.lane() == rumoca_ir_solve::NativeOutputLane::Integer
                    && run.role() != rumoca_ir_solve::SolveVariableStorageRole::ExternalInput
            }));
            rumoca_exec_wasm::compile_retained_native_program_wasm(inputs).unwrap();
            Ok(String::new())
        })
        .unwrap();
    }
}

fn assert_real_binding(
    root: &rumoca_ir_solve::RetainedAssertionRoot,
    model: &str,
    name: &str,
    expected: &[f64; 4],
) {
    let variable = root
        .metadata()
        .variables()
        .iter()
        .find(|variable| variable.name() == name)
        .unwrap();
    let rumoca_ir_solve::SolveDeclarationStart::Real(values) = variable.declaration_start() else {
        panic!("original Real binding {name}")
    };
    assert_eq!(values.len(), 4);
    for (index, expected) in expected.iter().enumerate() {
        let actual = values.value(index).unwrap();
        assert!(
            (actual - expected).abs() <= 1e-12 * expected.abs().max(1.0),
            "{model}.{name}[{index}]: {actual} != {expected}"
        );
    }
}

#[derive(Default)]
struct SensorReads {
    rgb: bool,
    depth: bool,
    arithmetic: bool,
}

impl SensorReads {
    fn observe(inputs: &rumoca_ir_solve::RetainedNativeSchedule, rgb: usize, depth: usize) -> Self {
        let mut reads = Self::default();
        for stage in inputs.schedule().stages() {
            assert!(stage.target_count() > 0);
            for node in &stage.value_kernel().nodes {
                reads.observe_node(node, rgb, depth);
            }
        }
        reads
    }

    fn observe_node(&mut self, node: &rumoca_ir_solve::ComputeNode, rgb: usize, depth: usize) {
        match node {
            rumoca_ir_solve::ComputeNode::ScalarPrograms(block) => {
                for ops in block.programs() {
                    self.observe_ops(ops, rgb, depth);
                }
            }
            rumoca_ir_solve::ComputeNode::AffineStencil { base_ops, .. } => {
                self.observe_ops(base_ops, rgb, depth);
            }
            _ => {}
        }
    }

    fn observe_ops(&mut self, ops: &[rumoca_ir_solve::LinearOp], rgb: usize, depth: usize) {
        for op in ops {
            match op {
                rumoca_ir_solve::LinearOp::LoadP { index, .. } => {
                    self.rgb |= *index == rgb;
                    self.depth |= *index == depth;
                }
                rumoca_ir_solve::LinearOp::Binary {
                    op: rumoca_ir_solve::BinaryOp::Add,
                    ..
                } => self.arithmetic = true,
                _ => {}
            }
        }
    }
}

fn assert_calibration_bindings(
    root: &rumoca_ir_solve::RetainedAssertionRoot,
    model: &str,
    height: f64,
    width: f64,
) {
    for (parameter, input, horizontal, vertical) in [
        (
            "defaultRgbCalibration",
            "rgbCalibration",
            69.0_f64,
            42.0_f64,
        ),
        (
            "defaultDepthCalibration",
            "depthCalibration",
            87.0_f64,
            58.0_f64,
        ),
    ] {
        let expected = [
            width / (2.0 * (horizontal * std::f64::consts::PI / 360.0).tan()),
            height / (2.0 * (vertical * std::f64::consts::PI / 360.0).tan()),
            (width - 1.0) / 2.0,
            (height - 1.0) / 2.0,
        ];
        for name in [parameter, input] {
            assert_real_binding(root, model, name, &expected);
        }
    }
}
