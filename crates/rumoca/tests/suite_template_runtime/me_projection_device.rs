//! The ME projection kernel built for a device (SPEC_0044 ME-PROJ-001).
//!
//! A target that runs one lane per GPU thread compiles the kernel of the C
//! components (`fmi-me-projection.jinja`) as one CUDA C++ unit, from the
//! lowered model alone (`device_instance`, `device_kernel`). These helpers
//! render that unit for a test model and wrap it: on a host C++ compiler, with
//! the CUDA qualifiers defined away, so its refresh can be run and compared
//! with the FMI component's; or as a CUDA source for NVCC.

use std::sync::Arc;

/// The device unit: the instance, a lane that settles no parameter binding,
/// and the kernel.
const DEVICE_UNIT: &str = r#"{%- from "fmi-me-projection.jinja" import device_instance, device_kernel -%}
#include <float.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
{{ device_instance(solve, me_refresh, "rmc") }}
/* The lane's parameters are the values it was given. */
static __device__ rmcStatus refresh_parameters(ModelInstance* m) { m->parameters_dirty = false; return rmcOK; }
{%- if me_refresh.init.rows > 0 %}
static __device__ int rmc_apply_updates(ModelInstance* m, bool initializing) { (void)m; (void)initializing; return 0; }
{%- endif %}
{{ real_extremum_prelude("cuda") }}
{{ device_kernel(solve, me_refresh, "rmc") }}
"#;

/// The device unit of one compiled model, rendered from the model the FMI
/// component of the same source retains, so both run the same plans.
pub(crate) fn device_unit(compiled: &rumoca::CompilationResult) -> String {
    let component =
        rumoca_sim::lower_fmi_c_export_component(&compiled.dae).expect("lower the FMI C component");
    render_device_unit(component.runtime_view().model().clone(), compiled)
}

/// The device unit of one compiled model, rendered from the simulation
/// lowering a device target passes, without an FMI component.
pub(crate) fn solve_device_unit(compiled: &rumoca::CompilationResult) -> String {
    let lowered = rumoca_sim::lower_correlated_for_simulation_with_overrides(
        &compiled.dae,
        &rumoca_sim::SimOptions::default(),
    )
    .expect("lower the Solve model");
    render_device_unit(lowered.model().clone(), compiled)
}

fn render_device_unit(
    model: rumoca_ir_solve::SolveModel,
    compiled: &rumoca::CompilationResult,
) -> String {
    rumoca_phase_codegen::SolveTemplateRenderer::new_owned_model_with_me_projection(
        Arc::new(model),
        &compiled.dae,
    )
    .expect("build the device renderer")
    .render(DEVICE_UNIT)
    .expect("render the device unit")
}

/// The test models: an affine block with its reduced elimination, a torn loop
/// with causal runs, an array equation whose rows are sliced singletons, a
/// loop through a pure function with a loop, a loop through a function whose
/// assertion fails at some points, an initialization residual that
/// calls a function, and a switching chart set.
pub(crate) fn models() -> Vec<(rumoca::CompilationResult, &'static str)> {
    let compile = |source: &str, model: &str| {
        rumoca::Compiler::new()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .unwrap_or_else(|error| panic!("compile {model}: {error:#}"))
    };
    vec![
        (compile(TORN_LOOP, "DeviceTornLoop"), "DeviceTornLoop"),
        (compile(ARRAY_LOOP, "DeviceArrayLoop"), "DeviceArrayLoop"),
        (
            compile(FUNCTION_LOOP, "DeviceFunctionLoop"),
            "DeviceFunctionLoop",
        ),
        (
            compile(AFFINE_SPLIT, "DeviceAffineSplit"),
            "DeviceAffineSplit",
        ),
        (
            compile(ASSERTING_FUNCTION, "DeviceAssertingFunction"),
            "DeviceAssertingFunction",
        ),
        (
            compile(BRANCHING_FUNCTION, "DeviceBranchingFunction"),
            "DeviceBranchingFunction",
        ),
        (
            compile(INITIAL_FUNCTION, "DeviceInitialFunction"),
            "DeviceInitialFunction",
        ),
        (
            compile(
                include_str!("../fixtures/index_reduction/SplitCircleChart.mo"),
                "SplitCircleChart",
            ),
            "SplitCircleChart",
        ),
    ]
}

const TORN_LOOP: &str = "model DeviceTornLoop
  output Real x(start=1, fixed=true);
  output Real a(start=1);
  output Real b(start=1);
  output Real c(start=1);
equation
  der(x) = -0.5*a;
  a = x + 0.2*sin(c);
  b = a*a + 0.1*a;
  c = 1 + b - 0.3*cos(b) + 0.1*sin(c);
end DeviceTornLoop;";

const ARRAY_LOOP: &str = "model DeviceArrayLoop
  Real x[4](start={1, 1.5, 2, 2.5}, each fixed=true);
  Real u[4](each start=0.5);
equation
  der(x) = -u;
  u + 0.1*u.*u.*u = x;
end DeviceArrayLoop;";

const FUNCTION_LOOP: &str = "model DeviceFunctionLoop
  function partial_sum
    input Real v;
    output Real s;
  algorithm
    s := 0;
    for i in 1:3 loop
      s := s + sin(v / i);
    end for;
  end partial_sum;
  Real x(start=1, fixed=true);
  Real z(start=0.5);
equation
  der(x) = -z;
  z + 0.1*partial_sum(z) = x;
end DeviceFunctionLoop;";

const AFFINE_SPLIT: &str = "model DeviceAffineSplit
  parameter Real k = 2;
  Real x[2](start={1, 2}, each fixed=true);
  Real u[3](each start=0);
equation
  der(x) = {-u[1], -u[2]};
  u[1] + k*u[2] - u[3] = x[1];
  u[1] - u[2] + sin(x[2])*u[3] = x[2];
  u[2] + u[3] = x[1]*x[2];
end DeviceAffineSplit;";

/// A loop through a function whose error-level assertion holds at the first
/// points and fails at the last ones.
const ASSERTING_FUNCTION: &str = "model DeviceAssertingFunction
  function bounded
    input Real v;
    output Real s;
  algorithm
    assert(v < 1.1, \"bounded: v out of range\");
    s := v + 0.1*sin(v);
  end bounded;
  Real x(start=1, fixed=true);
  Real z(start=0.5);
equation
  der(x) = -z;
  z + 0.1*bounded(z) = x;
end DeviceAssertingFunction;";

/// A loop through a function whose branches and loop body call another
/// function, and through a conditional a tunable parameter selects, whose
/// arms call it: pure calls inside the regions of a function conditional.
/// A third function takes a minimum in one arm only, so the unit's extremum
/// helper is demanded by a nested region alone.
const BRANCHING_FUNCTION: &str = "model DeviceBranchingFunction
  function leaf
    input Real v;
    output Real s;
  algorithm
    s := v + 0.1*sin(v);
  end leaf;
  function branched
    input Real v;
    output Real s;
  algorithm
    if v > 0.9 then
      s := leaf(v);
    else
      s := 2*leaf(0.5*v);
    end if;
    for i in 1:2 loop
      s := s + 0.01*leaf(s/i);
    end for;
  end branched;
  function clipped
    input Real v;
    output Real s;
  algorithm
    if v > 0 then
      s := min(v, 1);
    else
      s := 0;
    end if;
  end clipped;
  parameter Integer mode = 2;
  Real x(start=1, fixed=true);
  Real z(start=0.5);
equation
  der(x) = -z;
  z + 0.1*branched(z) + 0.01*clipped(z) + (if mode == 1 then leaf(z) else leaf(0.5*z)) = x;
end DeviceBranchingFunction;";

/// No projection block, and an initialization residual that calls a function:
/// its tangent programs are the kernel's only callers of the directional
/// typed functions.
const INITIAL_FUNCTION: &str = "model DeviceInitialFunction
  function shifted
    input Real v;
    output Real s;
  algorithm
    s := v + 0.5*sin(v);
  end shifted;
  parameter Real p(fixed=false, start=1);
  Real x(start=1, fixed=true);
initial equation
  shifted(p) = 2;
equation
  der(x) = -p*x;
end DeviceInitialFunction;";

/// The perturbed state points every comparison refreshes.
pub(crate) const POINTS: usize = 5;

/// The number of continuous states of a compiled model, the leading solver
/// coordinates a point perturbs.
pub(crate) fn state_count(compiled: &rumoca::CompilationResult) -> usize {
    rumoca_sim::lower_fmi_c_export_component(&compiled.dae)
        .expect("lower the FMI C component")
        .problem()
        .solve_layout
        .state_scalar_count()
}

/// The values of one `Y` or `P` line of a start point, as C initializers.
fn start_values(start: &str, tag: &str) -> String {
    start
        .lines()
        .find_map(|line| line.strip_prefix(tag))
        .unwrap_or_else(|| panic!("the start point has a `{tag}` line: {start}"))
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(", ")
}

/// The device unit with one lane per point: each lane starts from `start`
/// (the `Y` and `P` lines a driver prints), perturbs its `states` leading
/// coordinates as the FMI driver does, refreshes its algebraic coordinates
/// and derivative values and, for a switching chart set, decides its chart
/// and transfers when asked. The host build runs the lanes in turn; a CUDA
/// build launches them on the device. Both print each lane's chart, its
/// statuses and flags as one code, and its coordinate, in the FMI driver's
/// format.
pub(crate) fn lane_harness(unit: &str, start: &str, states: usize) -> String {
    let y0 = start_values(start, "Y");
    let p0 = start_values(start, "P");
    let charts = u8::from(unit.contains(" rmc_chart_completed("));
    format!(
        r#"#include <stdio.h>
#include <stdlib.h>
{unit}
static __device__ const double rmc_y0[Y_LEN] = {{ {y0} }};
static __device__ const double rmc_p0[P_LEN] = {{ {p0} }};
static __device__ int rmc_point(ModelInstance* m, int k) {{
    rmc_lane_reset(m); m->time = 0.0; m->parameters_dirty = false;
    for (size_t i = 0; i < Y_LEN; ++i) m->y[i] = rmc_y0[i];
    for (size_t i = 0; i < P_LEN; ++i) m->p[i] = rmc_p0[i];
    for (size_t i = 0; i < {states}; ++i) m->y[i] = rmc_y0[i] * (1.0 + 0.05 * k) + 0.01 * (double)(i + 1) + ({charts} && i == 0 ? 0.196 * k : 0.0);
    int code = refresh_algebraics(m) == rmcOK;
    code |= (refresh_derivative_values(m) == rmcOK) << 1;
#if {charts}
    bool request = false, values = false, nominals = false;
    code |= (rmc_chart_completed(m, &request) == rmcOK) << 2;
    code |= (int)request << 3;
    if (request) code |= (rmc_chart_transfer(m, &values, &nominals) == rmcOK) << 4;
    code |= (int)values << 5;
#endif
    return code | (int)m->assertion_failed << 6;
}}
static void rmc_print(const ModelInstance* m, int code) {{
#if {charts}
    printf("%zu ", m->rmc_chart);
#endif
    printf("%d", code); for (size_t i = 0; i < Y_LEN; ++i) printf(" %a", m->y[i]); printf("\n");
}}
#if defined(__CUDACC__)
__global__ void rmc_points(ModelInstance* lanes, int* ok) {{
    int lane = blockIdx.x * blockDim.x + threadIdx.x;
    if (lane < {POINTS}) ok[lane] = rmc_point(&lanes[lane], lane + 1);
}}
int main(void) {{
    ModelInstance* lanes = NULL; int* ok = NULL;
    if (cudaDeviceSetLimit(cudaLimitStackSize, 65536) != cudaSuccess) return 2;
    if (cudaMalloc((void**)&lanes, {POINTS} * sizeof(ModelInstance)) != cudaSuccess) return 2;
    if (cudaMalloc((void**)&ok, {POINTS} * sizeof(int)) != cudaSuccess) return 2;
    rmc_points<<<1, {POINTS}>>>(lanes, ok);
    if (cudaDeviceSynchronize() != cudaSuccess) return 3;
    ModelInstance* host = (ModelInstance*)malloc({POINTS} * sizeof(ModelInstance)); int status[{POINTS}];
    if (!host) return 2;
    if (cudaMemcpy(host, lanes, {POINTS} * sizeof(ModelInstance), cudaMemcpyDeviceToHost) != cudaSuccess) return 3;
    if (cudaMemcpy(status, ok, sizeof status, cudaMemcpyDeviceToHost) != cudaSuccess) return 3;
    for (int k = 0; k < {POINTS}; ++k) rmc_print(&host[k], status[k]);
    free(host); cudaFree(lanes); cudaFree(ok);
    return 0;
}}
#else
int main(void) {{
    ModelInstance* m = (ModelInstance*)malloc(sizeof *m);
    if (!m) return 2;
    /* Device memory is not zeroed: every lane starts from garbage. */
    for (int k = 1; k <= {POINTS}; ++k) {{ memset((void*)m, 0xA5, sizeof *m); int ok = rmc_point(m, k); rmc_print(m, ok); }}
    free(m);
    return 0;
}}
#endif
"#
    )
}
