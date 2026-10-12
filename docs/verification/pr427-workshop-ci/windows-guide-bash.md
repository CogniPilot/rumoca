# Windows guide Bash prerequisite

Old-head `01e499b4596c20b13886dbb4c8af01622852bb33`, CI job
114762573305, passes 12 example tests and fails only
`guide_cli_contexts::bash_sources_have_valid_syntax`: Python's bare
`bash -n` exits 1. Its actual executable and child stdout are absent from the
failure report. The log establishes Actions' Git Bash shell, but does not prove
which executable Python selected. All 66 original LF and CRLF fences pass local
GNU Bash syntax checks, so newline conversion alone does not reproduce it.

Repair `0547efcfd8f60b63def3693a1e15d3ced03539ce` makes the existing Windows
workflow Bash step publish its actual native executable via `cygpath -w "$BASH"`
and standard `BASH` in `GITHUB_ENV`. The existing probe requires an available
absolute authority on Windows, resolves it once, preserves all 66 source hashes
and ten-second syntax checks, sends UTF-8 bytes, and reports page/line,
executable, exit status and both child channels. Rust also reports both Python
channels. No installation paths, syntax skips or second inventory are added.
This repairs the prerequisite/capture owner under SPEC0033 and preserves the
single existing inventory owner under SPEC0029 and SPEC0021.

The later Windows example/workspace steps use PowerShell (also shown in the
old log), and Cargo/Python are direct children. This preserves the published
native `BASH`; launching a new Bash wrapper would reset that variable and would
require native binding inside that wrapper instead.

```sh
PYTHONDONTWRITEBYTECODE=1 python \
  crates/rumoca/tests/suite_examples_smoke/guide_cli_contexts/inventory_probe.py . syntax
```

Actual Linux checks pass all 66 spans both through normal resolution and explicit
owning Bash. Direct prerequisite controls reject missing, relative and
unavailable explicit authorities. Deliberately invalid Bash source fails with
page/line, actual executable, exit 2 and syntax stderr; an independently failing
launcher preserves both stdout and stderr. Pinned standalone rustfmt check and
`git diff --check` pass. No compiler/Rust build was run. Actual Windows execution
remains **UNRUN locally** and must be qualified by the next remote Windows job.

[The concise receipt](windows-guide-bash.json) binds the exact three source
hashes and raw controls retained under
`$HOME/scratch/rumoca/pr427-windows-guide-bash-20261012/evidence`.
