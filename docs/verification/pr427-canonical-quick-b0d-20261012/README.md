# Canonical quick checkpoint b0d

The completed MSL and corpus-pin stages passed at clean source `b0d395786e50437d9b91349ca58a1476ba092755`. The overall quick command terminated with PASS (exit 0). This is a source-bound quick checkpoint, not a full or GitHub CI verdict. Later FMI helper changes are outside this snapshot.

The full 566-model MSL gate passed one Rust test in 1582.57 seconds. Actual evidence records the CI envelope: 20 seconds per compiler phase and 45 seconds per simulation, four Rumoca workers and one OMC thread. All five stage/balance/simulation/trace/speed gate verdicts passed.

Counts were parse 566, flatten 543, DAE 446, Solve 419, initialization 374 and simulation 360. All 306 compared models were strict-high, with no near/deviation bands, no bad/severe channels, 53 skips and one missing trace. The remaining model/refusal outcomes remain in the raw results; this does not mean all 566 simulated or were compared. The comparison used Modelica 4.1.0 and OMC `a96aa1a-cmake`.

Corpus-pin passed all 28 rows with zero failures in 84.5 seconds against its unchanged 180-second wall budget.

The compact receipt binds original stage JSON hashes, a sealed completed-stage log prefix, and the original 4201-row source manifest. A read-only mid-stage check matched all source bytes. Copies of only the small top-level JSON metadata were retained outside Root; no cache/source/raw-result mutation, archive, or deletion was performed. The final guardian, complete command log and unchanged source-before/source-after pair are SHA-bound in the receipt.

Architecture passed 247 tests plus 19 suite gates. Workspace nextest passed all 10,400 tests (four slow; zero skipped) in 345.024 seconds. The complete log retains subsequent doctest results.
