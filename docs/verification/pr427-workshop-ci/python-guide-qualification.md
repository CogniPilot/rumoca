# Installed-wheel guide and WASM CI contract evidence

The corrected Python guide at `4168e8e5a46145f8b3ba157309561fa315ec99e7`
passes ten exact Python fences and the actual IPython Model/export frontend.
The installed development wheel was built from
`92f9169585948afafe8042ecb28b6c2763994930`, with Python 3.12.13, JAX 0.9.2,
CasADi 3.7.2, NumPy 2.4.3 and matplotlib 3.10.8. These are separate test-source
and compiler-source identities; this is not fresh combined-head CI proof.

The original published guide's quick start fails at `Session.load` because
`Quadrotor.mo` is not provided. The corrected guide explicitly uses unchanged
tracked SympyDecay (`k`, `x`) and its authored scenario. Its structural example
now supplies the complete ArrayDecay source. Checks assert all sample values
against independent exponential curves (the existing API smoke tolerance
`1e-3`), exact endpoints, requested output grid, compact array metadata before
and after `n=2` to `n=3` recompilation, scalar result channels, dataframe and plot
execution, JAX JIT/forward/reverse derivatives, CasADi Jacobians, generated
files and typed diagnostics. The notebook cell runs through the real IPython
extension and returns Model; export returns SolveExport.

Both exact CMake fences at `547144f010733a0ad62a064d94fe2aa969f0f969`
also configure and build successfully through the installed wheel. They now
use tracked GalecCounter and SympyDecay contexts instead of unprovided
MyModel files. The check proves the generated stamp, Algorithm Code and C
files. It does not claim packaging/schema-asset completeness.

The four authored SympyDecay codegen TOMLs have independent before results:
JAX and C pass with actual generated numerical execution. JAX checks JIT and
forward/reverse derivatives at two state/parameter pairs; C compiles with
warnings as errors and checks defaults, initializers and RHS at those pairs.
The custom report directory and raw Jinja template fail because Python resolves
their targets from the working directory. Source `e1050448c` repairs the
shared scenario-relative target owner for Python and LSP; its AFTER is pending.
The tests retain all authored model/config bytes and override only temporary
output destinations. New linked SympyDecay configs fail the inventory check.

Canonical source `ea91c3537a069a6c1351e7d9c6b540d7f381ea9f` adds
`cargo xtask python test --require-guide`, also selected by `verify full`.
It stages the actual current-source or explicitly supplied wheel using pip into
a process-owned directory, validates the imported package's location, and
reuses one script roster in CI and local verification. Dependencies come from
the selected provisioned interpreter; staging does not download dependencies.
The mandatory Linux GNU wheel lane pins required numerical/plot dependencies.
Other wheel lanes execute base checks and do not claim Linux guide coverage.
The new canonical command and its Rust units are not yet qualified.

The separate WASM strict-command witness at
`b802a1c54cdee6d05154687352d94fe768b14f0d` passes one ordinary Rust test
with `template-runtime-tests,fmu-packaging`. It checks the actual workflow's
strict command array and WASM branch invocation. This repairs the stale
workflow-text expectation; it is not a new execution of the five FMI-LS WASM
runtime examples. Its first attempt was refused before build by the unchanged
scratch-floor guardian.

[The JSON receipt](python-guide-qualification.json) binds raw logs/guardians,
commands, source identities, wheel hash and resource limits. It records the
intermediate prerequisite/path/compact-metadata harness failures plainly.
Generated guide modules, CSVs and build products used process-owned TempDirs
and were removed; those products are not claimed as retained artifacts.
Fresh combined-head tests, remote CI and instrumented coverage remain pending.
