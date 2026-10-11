# Optional exact workshop OMC evidence capture

This separate test-only followup is based on the qualified c758 source and its
receipt. It has not run. `RUMOCA_WORKSHOP_EVIDENCE_DIR` optionally requests a fresh
absolute output root, chosen under `$HOME/scratch` by the bounded runner. With no
request the existing temporary-product behavior remains. No production code,
Modelica source, comparison oracle, tolerance, admission or test selector changes.

Each original/export cell must be new; an existing cell fails rather than mixing
runs. Before the existing OMC status/trace checks, the helper copies the actual
`model.mo`, `check.mos`, stdout, stderr and exit status. It copies `trace_res.csv`
only when OMC produced that real file, and records its actual presence; no CSV is
manufactured on failure. The unchanged required-trace check still fails when it
is absent. Temporary executables, object files and compiler products are excluded.
The source/CSV/log copies can be hashed externally in the combined-run receipt.

A final combined run should set this variable to an empty evidence location
inside its raw attempt, retain all 28 actual cells, then independently hash the
retained files alongside the existing frozen recipe, log and guardian. The tests
still run the same two selected functions and all six originals/eighteen exports
plus the separate fixture. This capture is additional evidence, not qualification
until the unchanged gates execute successfully on the new frozen source.
