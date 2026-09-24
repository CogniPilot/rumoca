# Retained OMC Fourbar runtime math and deferred CPU capture

Source root: `/tmp/rumoca-fluid/target/fluid-campaign/tier2-bulk-clear-7e67b9954/omc_sim_work/`; basename prefix below is `Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1`. Read-only inspection; no simulation, rebuild, or profile executed.

| Runtime system | Concrete generated computation | C anchors |
|---|---|---|
| Nonlinear 1921, 12 unknowns | Unknowns include selected frame rotation entries and `j5.phi`, `rev.phi`, `rev1.phi`, `j3.phi`, `j4.phi`. Candidate `xloc` is assigned, 86 causal equations 1715–1800 run, then 12 closure residuals 1812–1801. Example: `res[10] = dot(j5.R_rel.T[1,:], j5.frame_a.R.T[:,2]) - b3.frame_b.R.T[1,2]`; `res[11]` is an `atan2` of rotation-row dot products. Causal `j5.R_rel.T[1,1] = j5.e[1]^2 + (1-j5.e[1]^2)*cos(j5.phi)`. | `_02nls.c:1318,1329,2324,2530-2561,2754-2773`; `.c:833-851` |
| Linear 1946, 2 unknowns | Solves `j5.frame_b.t[1:2]`; residual calls seven causal torque transforms 1928–1934, then imposes `dot(j5.frame_a.t,j4.e)-j4.constantTorque.tau_constant=0` and `dot(j4.frame_a.t,j3.e)-j3.constantTorque.tau_constant=0`. | `_03lsy.c:4008-4034`; `.c:960-968` |
| Linear 2180, 16 unknowns | Velocity/first-derivative closure: unknowns include joint speeds and derivatives of rotation entries. Residuals include `j5.e[2]*j5.w-j5.R_rel.w[2]` and differentiated rotation composition, e.g. `der(j5.R_rel.T)*j5.frame_a.R.T + j5.R_rel.T*der(j5.frame_a.R.T)-der(b3.frame_b.R.T)`. Causal assignments precede 16 residuals. | `_03lsy.c:3595-3890` (notably 3827-3831); `.c:1045-1080` |
| Linear 2550, 20 unknowns | Force/acceleration/second-derivative closure: unknowns include joint accelerations, torques and second rotation derivatives. Causal force transform `rev1.frame_a.f[1] = -rev1.R_rel.T[2,1]*rev1.frame_b.f[2] - rev1.R_rel.T[3,1]*rev1.frame_b.f[3] - rev1.R_rel.T[1,1]*rev1.frame_b.f[1]`; residuals include differentiated rotation composition with `2*der(R1)*der(R2)` cross terms and `j5.e[1]*j5.a-der(j5.R_rel.w[1])`. | `_03lsy.c:1950-1962,1972-2431` (notably 2342); `.c:1304-1345` |

`_03lsy.c:8241-8281` registers runtime 2550/2180/1946 with `method=1` (symbolic Jacobian), `residualFunc`, analytic Jacobian column callback, and `setA=NULL`, `setb=NULL`. `_12jac.c:8622-8655,10373,13111,13703-13805` has generated derivative-equation callback arrays (9/115/177 equations) and initializes 2x2/16x16/20x20 Jacobians. `_02nls.c:2754-2773` registers the 12x12 nonlinear analytic Jacobian; `_12jac.c:8349,13650-13656` has its derivative callback and initializer. The 9/115/177 and 108 derivative-equation counts are **not solve dimensions**. Generated C supplies residuals/Jacobian columns to external runtime `solve_linear_system`/`solve_nonlinear_system`; it does not establish actual coefficient evaluation frequency, numerical factorization method, or factor reuse. Those require runtime evidence. `strictTearingFunctionCall=NULL`; do not count an additional strict solve. Runtime ODE list: `.c:2693-2735`; state derivatives: `.c:360-371,1981-1990`.

Initialization has distinct nonlinear 897 and linear 922/1186/1574 (sizes 12/2/16/20), registered `_02nls.c:2775-2790` and `_03lsy.c:8283-8320`; these are not additional per-ODE-step systems. The model init XML: `:47-55` specifies 0–5 s, 0.01 output step, tolerance 1e-6, DASSL, CSV. Campaign request is `simulate(Fourbar1, outputFormat="csv", fileNamePrefix="Fourbar1")` or explicit `stopTime` when configured (`crates/rumoca-test-msl/src/msl_tools/omc_simulation_reference/omc_session.rs:170-184`); the exact spawned executable argv was not retained. Reconstructed argv below preserves XML numerical/output settings. Existing OMC `timeSimulation` is an internal timer, not OS system CPU or a whole-process profile.

## Prepared capture; do not run until main releases lane

The retained executable is the `-Os` linked binary (`.makefile:8-17`); keep it. Capture a single unchanged run into a new directory. At 997 Hz CPU clock, a ~0.3 s simulation may yield only a few hundred on-CPU samples. Sampling spans process initialization, simulation, and CSV output; attribute stages only if runtime markers/stacks separate them, otherwise label **whole process**. `perf` is absent from this shell's PATH; supply a compatible installed `perf` on the run lane. Do not silently rebuild or change solver/options. Exit 124/137 means censored capture, not a timing datum.

```bash
set -eu
ref=/tmp/rumoca-fluid/target/fluid-campaign/tier2-bulk-clear-7e67b9954/omc_sim_work
name=Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1
out=/tmp/rumoca-zero-rhs/target/fourbar-omc-perf-7e67
command -v perf >/dev/null || { echo 'perf unavailable; capture not started' >&2; exit 127; }
test -x "$ref/$name"
test ! -e "$out" || { echo 'output exists; preserve prior capture' >&2; exit 1; }
mkdir -p "$out"
sha256sum "$ref/$name" "$ref/${name}_init.xml" "$ref/${name}.c" "$ref/${name}_02nls.c" "$ref/${name}_03lsy.c" "$ref/${name}_12jac.c" > "$out/inputs.sha256"
ldd "$ref/$name" > "$out/ldd.txt"
awk '/=> \// {print $3}' "$out/ldd.txt" | sort -u | xargs -r sha256sum > "$out/runtime-libs.sha256"
printf '%s\n' 'Reconstructed retained settings: XML 0..5, 0.01, 1e-6, dassl, csv; no numerical overrides.' > "$out/scope.txt"
cd "$out"
set +e
timeout 30s perf record -e cpu-clock -F 997 --call-graph dwarf,8192 -o "$out/perf.data" -- \
  "$ref/$name" -f="$ref/${name}_init.xml" -inputPath="$ref" \
  -outputPath="$out" -r="$out/${name}_res.csv" > "$out/stdout.log" 2> "$out/stderr.log"
capture_status=$?
set -e
printf '%s\n' "$capture_status" > "$out/exit-status.txt"
test "$capture_status" -eq 0 || exit "$capture_status"
sha256sum "$out/${name}_res.csv" "$out/perf.data" > "$out/outputs.sha256"
```

Compare output contract against retained reference CSV/trace, and preserve perf's exit status plus input/library hashes. `perf report --stdio -i "$out/perf.data"` can later inspect samples, but source proves no speed attribution by itself. A debug rebuild, if unavoidable for attribution, must be a separately labeled experiment, never substituted for this retained-binary capture.

## One-shot capture observed after lane release

Main ran the retained binary using the explicit Nix perf executable and saved `/tmp/rumoca-zero-rhs/target/fourbar-omc-perf-7e67/`. Exit status is 0; 298 CPU-clock samples, zero lost. Output CSV SHA-256 `ce9cae8c42ac2ccb8bc46d654d7837a7a34288ce71a9edee57f9e031d6a96f16` exactly matches the retained CSV. `stacks.txt:370-388` shows `dgesv_ → solveLapack → solve_linear_system → eqFunction_1946 → functionODE`; `:1047-1054` shows `dgesv_ → linearSolverWrapper → newtonAlgorithm → solveNLS → solve_nonlinear_system`. `:528-536` shows generated `functionJacNLSJac4_column → evalJacobian → getAnalyticalJacobianHomotopy → newtonAlgorithm`; the callback runs derivative equations 1813–1920 with optional selected DAG (`_12jac.c:8349-8476`) and is registered for nonlinear runtime system 1921 (`_02nls.c:2754-2773`). This proves observed runtime factor-solve and analytic-Jacobian call paths, including one linear call under ODE, but not factor reuse or total call counts. `inclusive.txt`/`stack-counts.json` are **whole-process** with overlapping inclusive counts; initialization and CSV output remain in scope. No pure-simulation percentage is inferred.
