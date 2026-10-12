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

The full neural benchmark is not covered by this checkpoint. Its original
1082-parameter owner and simulation/gradient oracle remain required; a separate
release CLI build keeps build time out of that runtime budget. Its first
600-second attempt timed out before executing the benchmark (exit124, peak
resident plus swap 3,155,600 KiB, all tracked source entries unchanged). A
separately recorded same-source warm build is in progress; no runtime result
is inferred from the completed compilation stages.
Later combined-source changes, the remaining browser guide routes, full/quick,
MSL/OMC and remote CI still need qualification.
