# Exact workshop OMC evidence capture

The tests always retain one fresh process-owned temporary directory per OMC
cell, using the standard `TMPDIR` channel. Local bounded runners set `TMPDIR`
under their owned `$HOME/scratch` attempt. Each actual directory is printed as
`OMC evidence retained at <path>`. No bespoke environment variable or shared
fixed-path configuration is read. This policy followup is source-only and
unqualified; the prior c758 receipt and later historical captures remain intact.
No production code, Modelica source, comparison oracle, tolerance, admission or
test selector changes.

Each original/export cell must be new; an existing cell fails rather than mixing
runs. Before the existing OMC status/trace checks, the helper copies the actual
`model.mo`, `check.mos`, stdout, stderr and exit status. It copies `trace_res.csv`
only when OMC produced that real file, and records its actual presence; no CSV is
manufactured on failure. The unchanged required-trace check still fails when it
is absent. Temporary executables, object files and compiler products are excluded.
The source/CSV/log copies can be hashed externally in the combined-run receipt.

A final combined run should use a fresh attempt-owned `TMPDIR`, collect the 28
printed retained paths from its log, and independently hash the retained files
alongside the existing frozen recipe, log and guardian. The tests
still run the same two selected functions and all six originals/eighteen exports
plus the separate fixture. This capture is additional evidence, not qualification
until the unchanged gates execute successfully on the new frozen source.

The existing Modelica template-runtime CI lane creates its `TMPDIR` under
`RUNNER_TEMP`, with the GitHub run ID and attempt in the directory name. It
retains the actual combined runtime log and uploads the directory with an
independent `SHA256SUMS` file in `always()` steps, including failed checks.
The directory is outside Cargo caches; the workflow requires a new directory
and an actual artifact rather than uploading evidence from an earlier run.
