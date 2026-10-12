# Authored Backprop modifiers and record observable transport

The [frozen 4659 attempt](qualification-4659-20261011.md) retains independent
before evidence for both corrections.

`examples/models/NeuralODEBackprop.mo` declared six state arrays with scalar
`fixed = true`, which OMC rejected before simulating the original. The actual
authored example now uses `each fixed = true` for W1, b1, Wmid, bmid, W2 and b2.
This is the array modifier required by [MLS 3.7 §7.2.5](https://specification.modelica.org/maint/3.7/MLS.pdf).
It expresses the existing intent to fix every state to its supplied start;
the array-valued starts, equations, dimensions, default parameters and inherited
1k variant are retained. The test reads the corrected tracked model directly;
there is no fixture-only source rewrite or relaxed OMC check.

The focused record-member fixture had an exact-column harness error. Original
`pair.position[1]` and `pair.position[2]` become `'pair.position'[1]` and
`'pair.position'[2]` when flattening quotes the declaration identifier. The
fixture explicitly maps those two original/exported physical columns and checks
that the original compiler's flat declaration `pair.position` is Real with
extent [2]. No substring matching, guessed index splitting, synthesized channel
or default value is used. Each mapped exported column must actually exist;
the same original values and settled/grid/trace comparison remain authoritative.

These source/fixture corrections passed rustfmt and `git diff --check`, followed
by the [actual c758 OMC/Node qualification](qualification-c758-20261011.md). The main roster remains six actual models;
the additional fixture remains separate. No compiler admission rule, exporter
guard, tolerance or runtime resource limit changed.
