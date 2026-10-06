# Interactive changed-input events

Changing a declared input through the common Model Exchange host now performs
one input-event transaction. Previously the setter wrote parameter storage and
restarted solver history without entering Event Mode. Ordinary Modelica
relations retained their previous event-domain truth, so a conditional could
remain on the old branch despite the visible input changing.

The minimal reproduction is:

```modelica
model InputConditional
  input Real u = 1.0;
  output Real y;
equation
  y = if u > 0.0 then 2.0 else 1.0;
end InputConditional;
```

For inputs `1, 0, 0.5, -1, 2, 0`, the old review browser package returned
`2, 2, 2, 2, 2, 2` even after advances. The expected values are
`2, 1, 2, 1, 2, 1`. A `noEvent` relation already returned the expected values,
localizing the defect to the host's event lifecycle rather than expression
lowering. The corrected native regression fails before the change and passes
after it, for both time-only and stateful sessions.

The common host enters Event Mode before writing a changed batch. The component
captures the event-left state and then observes the new input domains without
overwriting that capture. Its existing discrete iteration and continuous-mode
refresh settle the new event-right state; the host correlates solver history
once. This follows FMI 3.0.2 section 2.3.5's input-event transition and the
common-host ownership rules in SPEC_0044. It does not change relation lowering,
the trace replacement matrix, or plugin-specific stepping rules.

The whole batch retains last-write ordering, and its intermediate writes do
not manufacture additional `when` edges. Bit-identical batches retain history.
Unknown inputs or invalid values reject the batch; a failed component write
restores the standard FMI state, and restoration failures retain the existing
typed accepted-point-loss behavior. A setter requests no trace observation:
the next explicit observation or advance owns publication at that coordinate.

Focused end-to-end regressions cover ordinary conditional output changes,
piecewise state integration, rising `when` edges, duplicate names, invalid
names, and repeated unchanged writes. The existing common-host suite also
checks invalid NaN/infinity batches, one restart for a complete batch, signed
zero, owner correlation, and plugin-history failure handling.

Verified on the combined branch after the covariance-preparation repair:

```sh
cargo test -p rumoca-solver --lib
cargo test -p rumoca-bind-wasm --features full-web --lib
cargo clippy -p rumoca-solver -p rumoca-bind-wasm -p rumoca-sim \
  --all-targets --all-features -- -D warnings
```

All 521 solver and 81 binding tests pass; strict scoped Clippy passes. The two
new end-to-end tests were also run against the unchanged implementation and
both failed at the stale branch/edge assertions. Actual browser acceptance of
the rebuilt package, remaining combined workspace gates, and the measured
fixed-20 MSL canary remain separate checks; these native results do not imply
those gates have passed or change an application's production package pin.

Standards source: [FMI 3.0.2](https://fmi-standard.org/docs/3.0.2/).
