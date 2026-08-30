# Test-only Solve C spelling fixtures

These two jinja files are TEST FIXTURES, not a code generation target. They
carry the checked Solve scalar-plan C spelling so the renderer machinery
(context projection, scalar plans, sparse outputs, failure ABIs) stays
verified by compile-and-execute tests while no product target emits this
spelling. The retired `c-ode` target must not be recreated from them: the C
export surface is the `fmi3` Model Exchange target with the built-in solver,
and the future Solve-rendered embedded C target owns the next product C
spelling.
