# Diagnostics and Spans

Good diagnostics are a feature of every phase, not a layer bolted on at the
end. The normative rules are
[SPEC_0008](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0008_PHASE_ERRORS.md);
this page is the working model.

## Phase-Local Errors

Each phase defines its own error enum with phase-specific error codes
(`EM001` duplicate class, `ED013` unsupported algorithm lowering, …),
defined next to the code that emits them. There is no central error enum:
errors evolve with their phase, ownership is obvious, and code ranges stay
consistent per phase.

When you add a failure path, add it to the owning phase's enum with a span
and a code — do not widen a generic error or stringify early.

## Spans and Source Identity

`Span.source` is a stable identity derived from the source name, assigned
by the parser. Spans must be carried from source data through every
diagnostic-producing IR. Fallback spans are acceptable only when no source
exists; if source data exists and a diagnostic lacks a span, the correct
fix is to preserve the span at the phase boundary where it was lost — not
to synthesize one downstream.

This is why diagnostics work identically across the CLI (rich terminal
rendering), VS Code (Problems panel via LSP ranges), and the browser
editors (Monaco markers): they all consume the same span-carrying
diagnostics, only the presentation differs.

## Failing Early and Loudly

The repository's defensive-coding posture: catch errors in the phase that
owns the invariant, with a typed error — never weaken a check because a
downstream consumer can "cope". Validation gates (such as the DAE's
`appendix_b_validation`) are positive enforcement of contracts, and tests
assert the *specific* expected error, not just "some error".

## Tracing

Phases use the `tracing` crate for structured diagnostics during
development. Debugging knobs are documented CLI flags — Rumoca has a
zero-`RUMOCA_*`-environment-variable policy
([SPEC_0018](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0018_TOOL_CONFIG.md)),
so a debugging affordance worth keeping becomes a flag (like `--inspect`
and `-v`), not an env var.
