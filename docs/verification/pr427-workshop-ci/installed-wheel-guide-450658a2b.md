# Installed wheel and Python guide checkpoint

The fresh wheel built from frozen combined source `450658a2b` passes the five
mandatory installed-wheel scripts, all ten exact Python guide fences, the
notebook Model/SolveExport checks, four authored codegen scenarios and both
exact CMake fences. Every tracked source entry matched before and after each
attempt. The adjacent receipt binds the wheel, commands and raw artifact hashes.

JAX and CasADi are required and actually imported. The guide evaluates generated
RHS functions for multiple state/parameter inputs, JAX JIT plus forward/reverse
Jacobians and CasADi Jacobians. Analytic simulation curves, metadata, authored
paths and generated file bytes are checked. Execution used CPU and needs no
hardware graphics device. The previously failing ninth Python fence now runs.

The first seven-script attempt passed the five installed API/export scripts,
the complete Python guide and all four scenarios, but failed its CMake step:
pip's generic Linux CMake binary shadowed the Nix executable and exited127 on
NixOS. That attempt remains terminal1 in the receipt. A targeted replay changed
only PATH provisioning to the native CMake supplied by the same Nix shell; all
four scenarios and two CMake fences then passed. No compiler/test source was
changed, no test skipped and no assertion weakened. The wheel installation and
all its bytes remained the same. This is compositional evidence across two
attempts, not a first-attempt seven-script pass.

The unchanged full CPU neural benchmark also passes at the same source:
`NeuralODEBackprop1k`, hidden 30/batch 8, 1082 trainable parameters, 0–0.2 s
with 0.02 s outputs, one warmup and three timed iterations. The test checks
all eleven samples of six native/JAX output channels and all 1082 JAX
manual/reverse gradients without changing tolerances. Final output error is
1.16e-14; JAX gradient agreement error is 5.20e-18. Native internal-state and
gradient parity is unavailable from the output CSV and is not claimed.

The first release CLI build timed out at 600 seconds before runtime (exit124,
peak resident plus swap 3,155,600 KiB, tracked source unchanged). A distinct
same-source warm build completed in 346.84 s. The full benchmark then completed
in 25.87 s, exit 0, peak resident plus swap 394,416 KiB; the sealed CLI bytes
and every tracked source entry matched afterward. Both attempts remain visible.
The benchmark's raw native timings are 0.764 s frontend compilation and 10.863 s
preparation, 11.628 s total ready time, and 0.707 s average hot run for 0.2 s
simulated time (0.283× real time). This proves numerical behavior, not the
separate 10× real-time performance goal or an OMC speed comparison.

Later combined-source changes, the remaining browser guide routes, full/quick,
MSL/OMC and remote CI still need qualification.
