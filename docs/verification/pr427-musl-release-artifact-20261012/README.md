# PR427 Linux musl release artifact qualification

The original central `issue_422.mo` U4 passes at all 11 checkpoints from 0 to 1 through the stripped default-feature x86_64 musl CLI. Every checkpoint requires Cranelift, no refusal and no declined helper. The existing strict U4 test retains one pure-call owner and requires its native table to compile. Both direct memory-libcall ABI tests pass.

The final artifact source is `8ca302e1f9257257c4e2d65c340599a81ecaffb8`; the prepared core test executable is `df51406631a107875f9dd43fa78ff8d9baa5ab6a` and the helper executable is `47ec8a7744efba5c2d4bf8621492f32866364d75`. The final smoke executes four exact release gates plus all nine stdio tests: 13 executions, 12 unique tests, zero failed or ignored. Both stripped CLI/LSP hashes remain unchanged before and after. Each ELF has no interpreter or shared-library requirement. Scoped musl `rumoca --test suite_core` Clippy and workspace formatting pass.

The existing release job prepares test executables before its final normal build/strip, then directly executes them without invoking Cargo. Its shared runner rejects absent/zero-test selectors, requires exactly one passing test and preserves failure output. The default gamepad feature obtains a checksum-pinned, static musl libudev-zero 1.0.3; the same source acquisition/install recipe was executed locally. Its ISC notice is added through the existing generated release-notice owner. Regeneration also corrects only the 55 stale workspace version labels to 0.10.2; dependency attribution/counts are unchanged.

Equivalent Cargo preparation:

```sh
cargo test --locked --offline --jobs 4 --release --target x86_64-unknown-linux-musl -p rumoca-exec-cranelift --lib --no-run --message-format=json
cargo test --locked --offline --jobs 4 --release --target x86_64-unknown-linux-musl -p rumoca --test suite_core --no-run --message-format=json
cargo build --locked --offline --jobs 4 --release --target x86_64-unknown-linux-musl --bin rumoca --bin rumoca-lsp
cargo clippy --locked --offline --jobs 4 --release --target x86_64-unknown-linux-musl -p rumoca --test suite_core -- -D warnings
cargo fmt --all -- --check
```

All substantive attempts use the existing 600-second, 4-GiB RSS-plus-swap, 8-GiB target, 16-GiB host and 8-GiB scratch guards with 4/4/4 concurrency. The pinned Nix compiler is GCC15.2, so this local proof does not claim execution on Ubuntu release runners, ARM or gamepad hardware. The two pre-execution tooling refusals are retained, with no semantic retry or guard enlargement. Active SPEC0025/0033 select verification; SPEC0029 owns shared helpers. No compiler/runtime/schema change is introduced by this chain.

[receipt.json](receipt.json) binds source manifests, commands, guardians, raw logs and externally sealed ELFs. Prior GNU static-PIE evidence is separate.
