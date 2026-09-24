# Schedule-owner ce1 capture integrity

Read-only audit of `/tmp/rumoca-fourbar-schedule-owner-ce1`; no kernel/equation attribution or timing comparison.

- Source commit `ce1a85bc9785e3ba9eda10249d1c8316ea8a472d`, tree `01eddd3534b6e82d6b790dc4ab8ed55c58914523`, diagnostic patch SHA-256 `170d0cbbcf4c202dd58fa43dfde0159a8aab763937eb877459c38f9f7980613e`; the recorded capture-time source diff hashed to that patch. Prepared input checksums and built ELF checksum verify. ELF SHA-256 `31ff67414d7373e654158ac6ea1436edf041e67bb6289029f814b642db96cee5`.
- Canonical and capture requests differ only at `output_dir`; capture exited 0, worker PID `3796386`, `sim_ok`, no outer timeout. Sim interval `220088.509743414`–`220094.613288668`; sampled interior `220089`–`220094`.
- Manifest SHA-256 `7eb98c4cd71cbc5365c7d94316e4feff4613a2d1b13aae602667030752ccff1b`, 19 unique symbols (17 exact, two torn). All 19 IDs occur in `/tmp/perf-3796386.map` copied as `jit-perf-3796386.map`; its SHA-256 `1eefb7a1263066833f5abe36eb55d5b26c58ccf01fa867f66b1ebc1134af0d09` matches `capture-identity.json`. The native perf report names six sampled schedule IDs. `stacks-sim.txt` does not spell out schedule symbol names, so any stack-level owner attribution must join addresses against the retained map rather than infer absence of calls.
- Perf recorded 7,637 samples overall. The interior CPU-clock report contains 939 samples and says `Total Lost Samples: 0`; no loss/error is reported in capture logs. The worker Fourbar trace SHA-256 is `1a5edad3539e82a4989ba09b4dd3a204500ffe56a06be36eb05a8a5406231e6f`, byte-identical to canonical `paired-preservation-focused-ce1a85bc`.

Limitation: the patch logs at lazy schedule compilation, which may occur during Sim. This is a diagnostic capture, not an uninstrumented speed comparison; exact sampled symbol-to-owner attribution is a separate analysis.
