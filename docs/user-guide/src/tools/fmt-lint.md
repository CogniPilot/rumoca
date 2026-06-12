# Formatter and Linter

Rumoca ships a formatter and a linter for Modelica source. Both accept files
or directories (defaulting to the current directory) and are also exposed
through the VS Code extension and LSP.

## `rumoca fmt`

```bash
rumoca fmt                 # format the current directory in place
rumoca fmt src/ Model.mo   # format specific paths
rumoca fmt --check         # report differences without writing (CI-friendly)
```

### Profiles

| Profile | Behavior |
|---|---|
| `dymola` | Preserves MSL/Dymola-compatible local whitespace |
| `canonical` | Stricter spacing and indentation defaults |

```bash
rumoca fmt --profile canonical
```

Individual rules can be toggled regardless of profile:

| Flag | Effect |
|---|---|
| `--indent-size <N>` | Spaces per indentation level |
| `--use-tabs[=true\|false]` | Tabs instead of spaces |
| `--normalize-indentation[=true\|false]` | Normalize structural indentation (enabled by `canonical`) |
| `--repair-missing-indentation[=true\|false]` | Indent only lines that have none |
| `--normalize-equation-spacing[=true\|false]` | Normalize spacing inside equations |

`--coverage` reports how much of the eligible source trivia the formatter
rules cover, without writing changes.

## `rumoca lint`

```bash
rumoca lint                       # lint the current directory
rumoca lint --min-level warning   # filter: help | note | warning | error
rumoca lint --warnings-as-errors  # CI gating
rumoca lint --disable-rule <id>   # repeatable
rumoca lint --max-messages 50
```

Lint diagnostics use the same rich terminal rendering as compiler
diagnostics, with source spans. In VS Code they appear in the Problems
panel.
