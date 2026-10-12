# PR427 CI source and static-PIE preparation qualification

The published `01e499b4596c` Linux and macOS workspace gates found a reviewed MLS manifest count of 470 for 471 catalog rows after EXPR-042. Signed `cc0225491fe1` updates only that count; the existing exact gate passed 1/1. The manifest retains its `unformalized` default and every gate assertion.

The same Linux job passed both static-PIE libcall controls, then its 600-second watchdog expired during original U4 compilation. U4 did not execute. The shared qualifier now separates preparation from execution; CI bounds cold preparation at 20 minutes and retains the 600-second execution watchdog and 60-second per-test caps. Its default combined mode remains available. Preparation and execution require clean tracked source, matching HEAD/input hashes, and exact sealed ELF checksums.

At frozen `612e602fd5b2`, actual preparation completed in 437.539 seconds and execution in 4.742 seconds. All three exact GNU static-PIE gates passed. The unchanged original U4 retained eleven Cranelift checkpoints, no refusal and no declines. Every ELF is x86-64 static PIE with no interpreter or dynamic library dependency. Source mismatch, a changed tracked compiler file, and ELF tampering independently refused before any test execution. The deliberately edited source was restored and its full tracked-file seal matched.

Each local command retained 600 seconds, 4 GiB aggregate RSS plus swap, an 8 GiB target ceiling, 16 GiB host reserve, 8 GiB scratch reserve and four workers. No guard stopped. This repairs preparation ownership under SPEC0033 and truthful CI qualification under SPEC0025; it changes no model, numerical oracle, solver budget or compiler semantics. Repaired-tip quick/full and platform CI remain separate requirements.

[receipt.json](receipt.json) binds the exact commands, guardians, source seals, raw CI evidence and tested ELFs retained outside the owned cache under `$HOME/scratch/rumoca/pr427-staticpie-ci-preparation-20261012` and `$HOME/scratch/rumoca/pr427-mls-manifest-count-20261012`.
