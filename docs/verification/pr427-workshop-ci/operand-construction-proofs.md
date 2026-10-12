# Carry construction proofs into consumers

The local quick gate at `86c3bc1a6128e3d3174fefa169ae7bd5e03243e9`
found three totality-debt increases. The repairs preserve the existing debt
ceilings and validators. Compact memory validation now returns the already
validated P-backed target index. The scalar lowering boundary checks binary
operand pairs and conditional branch pairs once, then passes their fixed-size
shapes to consumers instead of recovering them with repeated `expect` calls.
Neither change expands a tensor or reconstructs runtime semantics.

At frozen `c152fd979e1696856568d491250f77b81a32bde4`, all 223 IR-DAE,
609 IR-Solve and 188 PhaseSolve library tests pass (1020 total). Raw Cargo
logs and matching tracked-source before/after manifests are retained under
`$HOME/scratch/rumoca/workshop427-final-broad-20261012/evidence/focused-operand-proof-c152fd979`.
The guardian reported process exit zero and no resource stop. Its JSON was
printed to orchestration output rather than saved to a complete artifact.
The outer login shell subsequently exited one from an unrelated local
`/etc/bash_logout` unset variable; that is not a Cargo test failure.

A private closed model-conditional record now carries the expression and
validated guard operation identities from construction into frozen views.
Open records remain construction-local. Checked current wire replay reissues
the proof, and the current JSON/bincode representation remains unchanged.
At frozen `4be6b10bdb3c25d0321aa08c9f7b295cb71afe0c`, all 223 IR-DAE
tests and scoped all-target/all-feature Clippy pass, with matching tracked
source seals. Tests include unfinished owners, live arms, foreign replay,
nested guard identity and byte-stable current wire round trips. Complete
bounded receipts are retained under
`$HOME/scratch/rumoca/pr427-closed-model-conditional-proof-20261012/evidence`.

These are focused results on the stated source commits. The final combined
architecture, quick/full, canary and cohort gates still need to pass.
