# Installed-wheel FMI source rendering

Micah's `c7d3ab50984367f6b962f03386c33186e2fc3d4a` enables the `fmi`
feature of the Python binding's Rumoca dependency. Without it, the wheel
advertises `fmi2`, `fmi3` and `fmi-ls-wasm`, but the public
`Model.codegen` dispatch refuses them. The installed-wheel smoke script
previously exercised only `c-ode`; the optional live JAX/CasADi checks
could skip and did not test this feature boundary.

Test-only `68d682cbea142ccb6dfd3e0bf326d25fd07cfdf2`, based on
`db0e68b9cd0470f5466e1e2874350505115362c6`, adds three mandatory checks in
`fmi_codegen_test.py` and calls them from both existing installed-wheel
CI steps. The existing five wheel platforms retain their build/install
commands. No optional dependency, mock, environment flag or new runner
is involved.

Each test loads the real `UsesLib.mo` fixture and its `Lib` source root,
then calls `Model.codegen`. It checks the complete rendered-file roster
(8 FMI 2 files, 9 FMI 3 files, 11 FMI-LS-Wasm files), unique safe paths,
nonempty contents, matching typed/iterated file views, and exact UTF-8
bytes from `CodegenResult.save_all`. Parsed FMI metadata must preserve
the model identity, gain 2, state start 1, derivative linkage, unique
value references, both ME/CS identifiers and their generated header
prefix. Source/build declarations must name all six kernel translation
units. The WASM source check binds its Rust ABI adapter to the same
generated C prefix and retains the pinned experimental WIT revision.

Accepted SPEC_0007 and its referenced SPEC_0040 product contract own
FMI component export and real target artifacts. The public rendering
API explicitly returns sources and skips packaging: these tests claim
no FMU archive, native compilation, lifecycle execution or compiled
WASM component. Existing template-runtime suites own those stronger
boundaries. FMI-LS-Wasm is a readiness-zero non-normative draft, not an
adopted layered standard. SPEC_0038 remains DRAFT.

Required installed-wheel command (exactly three checks):

```sh
python crates/rumoca-bind-python/tests/fmi_codegen_test.py
```

This agent ran AST syntax validation and `git diff --check` only.
The installed-wheel command is **UNRUN**, pending ROOT's exclusive build
lane. These source checks also do not qualify optional Python live-object
JAX/CasADi exports; their existing skip paths remain a separate coverage
limitation, distinct from strict generated-template backend CI.

There is a separate source-project completeness concern: the in-memory
`render_manifest_files` owner returns `[[files]]` entries but omits
`[[assets]]`. Consequently the FMI-LS-Wasm source set lacks the WIT,
FMI headers and licenses its generated project references. A separate
test-only witness and owner proposal retain that boundary; the feature
tests neither fabricate an archive nor describe the 11-file source
preview as a buildable component crate.
