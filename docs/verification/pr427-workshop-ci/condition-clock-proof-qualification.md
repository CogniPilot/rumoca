# Condition clock proof qualification

The Boolean periodic-task projection (`4022e27ff`) and subsequent synchronous-owner deduplication (`4db664caa`) each passed the same four native gates: one constructor/JSON/bincode test, seven original SIM010 regressions, three current-alias public tests, and the full 110-test GALEC suite. Both runs had zero failures and ignored tests.

The first frozen replay was `6274623eaac70e7c5489f5b0444a5cec8c1c4e25`; the cleanup replay was `aa12a494c4d25eba7ea81ee7d16eebb6928b16ab`. All 4,047 tracked files and HEAD of the cleanup replay matched their pre-build manifest afterward. Executed test binaries were retained outside the Cargo cache. The final full GALEC suite took 6.90 seconds of Rust test runtime. Compilation time is separately recorded in the guardian receipts.

The DAE constructor remains the sole synchronous-clock authority. The cleanup stores its existing derived fact on the condition entry, exposes it through the checked view, and removes the duplicated Solve walker. JSON/bincode omit the derived facts and reconstruct them through current constructors. Boolean `sample` task eligibility remains distinct from MLS synchronous ownership. The wire control verifies both distinctions after roundtrips.

The public alias tests use Interpreter and Auto without claiming that Auto selected native. Their inactive arithmetic control is `sqrt(-count)`. The separate prior scalar-owner unit covers a dynamic index refusal. The GALEC suite independently exercises C/reference/native behavior and tuple sharing identity.

[The compact receipt](condition-clock-proof-qualification.json) records exact commands, source identities, counts, resource limits, raw-log and guardian hashes, and retained executable hashes. Raw evidence remains under the receipt’s scratch evidence root. These results do not qualify a later tip, installed wheel, fresh browser artifacts, or full CI.
