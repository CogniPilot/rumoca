# Algorithm source previews classify their actual checked rendered path

Frozen final2c3 rendered a path only after checking the unrendered manifest
string's extension. Valid `{{ model_name ~ '.c' }}` was consequently discarded
as non-source and the whole preview refused as empty, although the same literal
filename was admitted. The focused before-test commit2a5cec249 retains both
cases and requires identical real filenames/content; rendered `.xml` still
refuses a metadata-only successful preview. The before test has not run.

The minimal after change renders the filename once through the same checked
AlgorithmCode renderer already used by this entry point, trims it, and applies
the existing `safe_target_join` relative-path guard. It then checks the actual
`.alg`/`.h`/`.c` extension and renders the source template. A second test requires
the shared guard to reject rendered parent traversal, absolute and empty paths.
No target-name branch, filename guess, new projection, templating context,
capability exception or artifact fact is introduced. Existing built-in paths
are literal or model-name paths; none depends on unavailable packaging facts.

All six existing source-preview tests remain, including the two exact honest
sourceMap declaration assertions. The after source selects eight native preview
tests. Direct rustfmt and diff checks passed; no build/test has run. Root retains
the heavy lease, and final2c3 remains unchanged for its independent qualification.
