# Scenario Config and VS Code

Scenario and tool configuration are covered by
`spec/SPEC_0018_TOOL_CONFIG.md`.

Rumoca now prefers colocated `rumoca-scenario.toml` files for runnable scenarios. A `rumoca-scenario.toml` file
uses TOML content, lives next to the example it controls, and describes one
task: simulation, code generation, or another explicit tool action.

VS Code should run `rumoca-scenario.toml` scenarios directly. The play action belongs on the scenario,
not on a `.mo` file that requires guessing the model, source roots, solver, and
viewer.

Workspace Modelica paths are visible `rumoca-workspace.toml` files. Keep
repo-committable dependency paths there for examples. Scenario-specific paths
belong in the `rumoca-scenario.toml` scenario.
