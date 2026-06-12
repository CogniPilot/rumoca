# Front End: Parse to Flat

The front end takes source text to the Flat IR. Five phases, each a crate.

## Parse (`rumoca-phase-parse`)

Produces the AST: syntax structure, comments, spans. The parser assigns each
file a stable source identity (`Span.source` derives from the source name,
not an insertion index), which is what lets diagnostics, the LSP, and the
formatter agree about locations across sessions.

The parser is also the foundation of the editor experience — it is fast and
error-tolerant enough to run on every keystroke in `rumoca-lsp` and the
browser playground.

## Resolve (`rumoca-phase-resolve`)

Builds the scope tree and assigns every definition a `DefId` — a stable,
structural identity for classes, components, and variables. Name lookup
walks the scope tree
([SPEC_0002](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0002_SCOPE_TREE.md));
the `DefId` design and its invariants are
[SPEC_0001](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0001_DEFID.md).

The rule that shapes everything downstream: **after resolution, identity is
`DefId`, never a rendered name string.** Hashing or comparing flattened
name strings (`"a.b.c"`) inside semantic code means structure was lost too
early; carry the `DefId` instead. Textual path parsing is allowed only at
true source/protocol/config/display boundaries.

## Typecheck (`rumoca-phase-typecheck`)

Checks the resolved tree and evaluates *structural* parameters — the values
that determine array dimensions and `for`-equation ranges, which must be
known before instantiation can size anything. Type errors carry spans and
phase-local error codes.

## Instantiate (`rumoca-phase-instantiate`)

Builds the instance tree for the requested model: applies modification
chains (`Tank tank1(area = 2.0, h(start = 1.0))`), handles `extends`,
redeclarations, and conditional components, producing an
`InstanceOverlay`/`InstancedTree`. Instantiation and flattening are
deliberately separate phases — the production path runs instanced
typechecking between them, and no cross-phase shortcuts are allowed.

## Flatten (`rumoca-phase-flatten`)

Traverses the instance overlay into `flat::Model`: one variable list with
fully qualified names, one equation list, `connect` sets expanded into
potential-equality and flow-sum equations, `for`-equations unrolled. What
flattening does *not* do is equally important: arrays stay symbolic,
function bodies stay structured, and Modelica operators (`der`, `pre`,
`sample`) survive untouched for the DAE phase.

## Seeing It

```bash
rumoca compile Model.mo --emit ast-mo    # what the parser saw
rumoca compile Model.mo --emit flat-mo   # what flattening produced
```

Comparing `flat-mo` against your mental model of the hierarchy is the
fastest way to debug modification and connection handling.
