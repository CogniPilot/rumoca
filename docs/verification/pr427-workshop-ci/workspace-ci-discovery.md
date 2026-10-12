# Actual workspace scenario tests enter Playground CI discovery

The final2c3 audit found a CI reachability gap: `run_web_unit_tests` discovers
only `*.test.mjs`, while the actual three-case workspace regression was named
`default_workspace_smoke.mjs` and had no explicit invocation in CI or xtask.
Its earlier direct Node passes remain real local evidence; those source states
did not automatically select this regression through Playground CI.

The same existing file is renamed to `default_workspace.test.mjs`, preserving
all three assertions and imports. The current inventory names the discovered
file; historical receipts retain their actual executed filename/commands. The
existing discovery owner now selects these tests in `cargo xtask playground test`;
no custom selector, duplicate test, relaxed assertion or model/configuration
change is introduced. The direct focused command is now:

```sh
node --test packages/playground/tests/default_workspace.test.mjs
```

Source filename/discovery review and diff checks passed. No Node/Cargo test or
browser build has run for this followup; root owns the heavy/cache lease.
