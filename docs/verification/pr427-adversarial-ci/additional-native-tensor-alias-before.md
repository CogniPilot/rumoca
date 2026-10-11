# Additional aggregate alias before probes

The test-only candidate starts at `30b121366`. Production is unchanged. The
three tests select ordinary checked construction, checked reference evaluation,
and public prepared evaluation; no consumer flag bypasses construction.

Each test first checks a disjoint destination against an independent expected
vector. It then checks a unique-writer, 64-input row with a `Select` through
checked evaluation and public preparation. Lazy-plan admission is asserted,
and conditions `1, 0, 1` cover changing and repeated selections with the same
prepared owner. The final control uses the overlapping destination.

| Operation | Original initialized storage and alias | Expected | Source-traced native hypothesis, not an executed result |
| --- | --- | --- | --- |
| Concatenate | `[1,2,3,4]`, destination 0, sources `(2,[2])`, `(0,[2])`, result `[4]`, axis 0 | `[3,4,1,2]` | `[3,4,3,4]`: the first source overwrites the later source |
| Update | `[1,2,3,2,3]`, destination/base/replacement 0, shape `[3]`, slice coordinates at 3 with shape `[2]` | `[1,1,2]` | `[1,1,1]`: replacement element 0 overwrites replacement element 1 |
| Fill | `[0,1,2,0]`, destination 0, packed lane tuple at 1, count 2, lanes 2 | `[1,2,1,2]` | `[1,2,2,2]`: writes overwrite the reused lane tuple |

The canonical checked evaluator buffers concatenation/update outputs and captures
the fill tuple before writes. The fast evaluator currently writes while reading
these aliased inputs. Legal unique-writer rows also exercise the existing lazy
consumers, with disjoint storage. These tests do not claim that the lazy planner
admits duplicate writers; the existing repair correctly keeps those rows ordered.

The earliest candidate owner is private aggregate execution in `rumoca-eval-solve`,
including its shared compact `tensor_snapshot` admission and reusable native
scratch. A repair must preserve aggregate-native operations rather than expand
them into scalar IR. Governing accepted sections are SPEC_0007 Stage 4 (the
ordered Solve vocabulary), SPEC_0032 sections 2 and 4 (derived scalar views and
shared aggregate-native tensor execution), SPEC_0029 sections 3b and 5
(single-source helpers and Solve evaluation ownership), and SPEC_0033 sections
2 and 3 (concrete before proof and earliest divergent owner). The probes concern
the already-admitted Solve contract; they do not establish source-language
admission for a new Modelica profile.

Reproduction, exactly three selected tests:

```sh
cargo test --locked --offline -p rumoca-eval-solve --lib prepared_tensor_additional_alias_
```

These ordinary unignored library tests are also selected by the existing
workspace CI tests and coverage run. Rustfmt and diff checks were performed;
Cargo execution remains pending the root agent's guarded qualification lane.
No production repair or before execution is included in this candidate.
