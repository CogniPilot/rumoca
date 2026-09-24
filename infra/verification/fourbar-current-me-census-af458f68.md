# Final current Fourbar census

One capture, exit 0, `sim_ok`; the diagnostic trace SHA-256 `7f0ebb08c690e4654500d632becfd2a34c53dde069ead992d3cfd3b15ee40315` matches the retained f781 focus. Base integration commit `af458f681aa04902c4e6f7452dede5a1ea19303f`, tree `add379da043836b1bbb41e3792961b32caee0f3a`; temporary probe patch SHA-256 `003bf1c3b3a70cac553ab631d2abf2e538d3620498b76adc87a27ccf33a2c24d`; verified frozen ELF SHA-256 `815db68eb8daad7eed08a8b990ba2d846f67e934be38f7bf6eb67b82ea5f6749`. Canonical request differs only in `output_dir`; runtime flags, worker PID and certified interior Sim window are in `capture-identity.json`.

Active BDF instance: 1,265 directly counted attempts, 1,124 accepted internal steps, 141 error-test rejects, 0 nonlinear failures, 1,413 nonlinear iterations. Sim-scoped ME: 2,678 RHS, 40 true JVP, 3,001 observable-error pairs, 6,002 actual pair projections; pair time 4.096 s includes 3.983 s in projections. These host counts match historical 6fb. Two other constructed BDF instances had zero steps. Operator-Jacobian assemblies (229) are recorded as of last accepted step, not equivalent to OMC Jacobian evaluations. BDF stats span instance lifetime; ME counters begin at `run_to_stop`. Instrumented times are not canonical.

The temporary patch is retained as `probe.patch` and removed from source. Worker HEAD/tree are clean at the base commit above. No next optimization was implemented. Raw evidence, `evidence.md`, probe patch and frozen ELF are retained at
`/tmp/rumoca-fluid-speed-bdf/target/fourbar-current-me-census-af458f68/`.
See the [independent review](fourbar-current-me-census-af458f68-review.md) for
scopes, order semantics and OMC comparison limits.
