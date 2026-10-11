# Public Integer divisor control profile

At source `1447ed716d44cc3b020935f38bfb16372bab7fda`, the original
public suite ran 14 tests: 12 passed and two additional Integer controls
failed. The zero/overflow control stopped at its first model compilation:
the input divisor in `mod(9007199254740993+n,d)` was refused with ED018.
The additive, multiplicative and minimum-Integer controls in that test
therefore had not executed. The separate structured B.1c failure is not
changed here.

`rumoca-ir-dae::expression::type_rules::validate_runtime_quotient` admits
model-level scalar divisors of constant or parameter variability and refuses
discrete/continuous divisors. Function-body ownership is a separate profile.
The checked constructor rejects a statically zero divisor; a tunable
parameter that evaluates to zero retains an ordinary runtime domain fault.
These rules are implemented by the existing DAE owner; this correction
changes no compiler admission or arithmetic semantics.

The test keeps the original input-divisor source as an ED018 negative.
Its runtime controls use one compiled model with `parameter Integer d`
and `Evaluate=false`, then explicit parameter overrides: 7/0 for modulo
and 1/-1 for the overflowing minimum-Integer division. Valid and invalid
runs use both Interpreter and Auto. The zero-parameter error must name
a domain, zero or non-finite failure: the generated quotient boundary
indicator can observe the undefined reciprocal before Integer publication.
Arithmetic-overflow controls retain their Integer/domain assertions.
Addition and multiplication controls are unchanged.

Qualification is pending. The narrow ordinary workspace selector has one
test, preserving the full public suite's count of 14:

```sh
cargo test -p rumoca --test suite_core \
  main_dae_integer_quotients_refuse_zero_divisors_and_checked_arithmetic_overflow \
  -- --nocapture
```

The retained before log is
`pr427-workshop-and-native-final-headroom-after-20261011/original-public-runtime-plus-native-14.log`
under the owned qualification attempt directory. Exact source/log/guardian
hashes are recorded by the source-pinned qualification ledger; no runtime
after-pass is claimed by this test-only correction.
