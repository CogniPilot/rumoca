# Actual FullWeb Flat export roundtrip

Source-only candidate based on
`db0e68b9cd0470f5466e1e2874350505115362c6`. The existing
`packages/playground/tests/target_render_smoke.mjs` now requires
`flat-modelica` in the actual WASM catalog and renders it through
`render_target_for_source` from unchanged `examples/models/SympyDecay.mo`.
Its public descriptor must carry the exact tracked target manifest; the actual
single rendered path must be `SympyDecay_flat.mo`.

The test recompiles that actual generated source with the same WASM compiler,
checks the original balance and current checked DAE schema metadata, then uses
the actual FullWeb `simulate_model` entrypoint to verify all five samples at
0, 0.05, 0.1, 0.15 and 0.2 against `exp(-0.5*t)`. The authored initial value is
exactly 1. The numerical bound uses the unchanged public frontdoor defaults
`atol=rtol=1e-6`; neither the example nor export receives an injected annotation
or parameter override. A DAE-only Flat request must still refuse, preserving
the Flat stage boundary (SPEC_0007/SPEC_0029).

The checked catalog subset grows from six to seven IDs. SympyDecay renders
grow from five to six distinct targets: three Modelica exports and C/JAX/CasADi.
The same retained Flat output supplies the roundtrip; no second renderer,
synthetic frontend or new runner is introduced. Existing GALEC workspace,
capability and artifact refusal controls remain unchanged.

`cargo xtask playground test` already builds the actual single-threaded
release FullWeb package, then `run_wasm_source_root_smoke` executes this script.
The existing mandatory `playground` CI job selects that command. The focused
artifact invocation, only after the source-bound actual package is built, is:

```sh
node packages/playground/tests/target_render_smoke.mjs --pkg-subdir release-full-web
```

The package name must match the actual xtask build's selected subdirectory.
Only Node syntax and diff checks ran for this candidate. Cargo, WASM build,
roundtrip, simulation and fresh CI remain UNRUN; this is additional coverage,
not a passed qualification or a full seventeen-target matrix claim.
