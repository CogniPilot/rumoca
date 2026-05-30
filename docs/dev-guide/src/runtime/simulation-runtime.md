# Simulation Runtime

The simulation runtime is centered on shared solver interfaces. Backends such as
RK-like integration and Diffsol should be thin adapters over common runtime
logic.

Shared runtime responsibilities include:

- event schedules and root handling
- input routing and zero-order hold behavior
- result collection
- termination and assertion handling
- trace output
- pacing for fast, realtime, and lockstep modes

Backend crates should implement the solver interface and avoid carrying generic
simulation policy. If the same code is needed by two solvers, put it in the
shared runtime or solver API layer.

Interactive input belongs in `rumoca-input` and signal payloads belong in
`rumoca-signal-frame`; solver backends should consume resolved values, not know
about keyboard or gamepad details.
