# Test-only Solve C spelling fixtures

These two jinja files are TEST FIXTURES, not a code generation target. They
carry the checked Solve scalar-plan C spelling so the renderer machinery
(context projection, scalar plans, compact output ranges, failure ABIs) stays
verified by compile-and-execute tests while no product target emits this
spelling. The deleted `c-ode` target must not be recreated from them: the C
export surface is the combined `fmi3` ME+CS target (Model Exchange exposes the
model to host-owned integration; Co-Simulation owns the built-in solver), and
the future Solve-rendered embedded C target owns the next product C spelling.
