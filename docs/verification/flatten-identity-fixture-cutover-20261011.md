# Flatten fixture cutover qualification

The full Flatten unit gate at `e5388bd4bf2c08e4c20d29751bd3fedb95cb8fb6`
executed 676 tests: 673 passed and three failed, with no resource stop.
The retained `pr427-provenance-crate-units-e538-20261011/rumoca-phase-flatten-units.log`
has SHA256 `18740443b74b5a82cbf90f5aca57c898ade01a91d68d376f12a09568b154cc78`.

The failed substitution fixture combined a Generated iterator request with a
resolved Source reference. The current matcher correctly leaves that reference
alone. Its replacement parses and resolves the same `j+k` comprehension body,
uses the actual outer iterator identity, preserves the inner identity and source
span, and also checks that a Generated request cannot capture that source read.
SPEC0007's post-resolution declaration identity rule governs this distinction.

The when-for fixture combined Generated iterators, fake Source reference IDs,
and dummy spans. Its replacement resolves an authored when/for equation with
the same two ranges and `y[i,j]=i+j` body. It checks the four ordered targets,
both substituted operands, and retained source spans. No source check is bypassed.

The nested conditional fixture already compared every scalar row with the
untemplated route, but required every inner template to disappear. The outer
conditional capture refuses nested for-equations; static outer iteration then
selects a branch whose inner loop independently owns a valid template. The two
branch-local j declarations have distinct Resolve identities, so their domains
cannot become a single lifted parent. The replacement preserves the original
model and row comparison, checks three nonoverlapping two-point child domains,
and requests checked DAE construction. This follows SPEC0032's compact-family
ownership and deterministic scalar-view order; it does not widen conditional
capture eligibility.

No production code changes. Rustfmt and git diff --check passed. After tests
remain unrun here because ROOT owns the build lane. ROOT should execute each
following selector with `cargo test --locked --offline --jobs 4 -p
rumoca-phase-flatten --lib SELECTOR -- --exact --nocapture --test-threads=4`
(exactly one test each), followed by the full library gate (676 tests):

- `equations::conditional_and_eval::tests::test_substitute_index_descends_into_array_comprehension_body`
- `equations::template_conditionals::tests::conditional_template_retains_selected_child_domains_without_changing_rows`
- `when_equations::tests::when_for_equation_expands_all_index_ranges`
