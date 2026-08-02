# Custom GALEC Templates

This is the quick reference for selecting user-written templates that consume
Rumoca's GALEC IR. Search terms: `custom GALEC template`, `--template`,
`FILE_ID=PATH`, `render_context`.

## Built-in by default, custom when specified

Select the GALEC product normally. With no `--template`, Rumoca uses every
template embedded in that target:

```bash
rumoca compile Model.mo --model Model --target galec -o generated/
```

Override one file by its stable `[[files]] id`:

```bash
rumoca compile Model.mo --model Model \
  --target galec \
  --template alg=path/to/my-algorithm.jinja \
  -o generated/
```

The rest of the target still uses its built-in templates. Overrides are
repeatable:

```bash
rumoca compile Model.mo --model Model \
  --target efmi \
  --template alg=path/to/my-algorithm.jinja \
  --template c_header=path/to/my-header.jinja \
  --template c_source=path/to/my-source.jinja \
  -o generated/
```

The available IDs are:

| Target | Override IDs |
|---|---|
| `galec` | `alg`, `ac_manifest`, `content` |
| `efmi` | `alg`, `ac_manifest`, `c_header`, `c_source`, `pc_manifest`, `content` |
| `galec-c` | `c_header`, `c_source` |

An unknown ID fails with an error listing the IDs available for that target.

## Custom target directory

For a reusable new product rather than a one-command override, copy the closest
built-in target directory. Each file has an explicit context:

| `render_context` | Context supplied |
|---|---|
| `galec-algorithm` | GALEC Algorithm Code context under `ctx` |
| `galec-c` | GALEC-derived C context under `ctx` and at top level |
| `efmi-algorithm-manifest` | Algorithm Code manifest context |
| `efmi-production-manifest` | Production Code manifest context |
| `efmi-content` | eFMU `__content.xml` context |

Keep the copied `id`, `render_context`, and `[[files.checksums]]` declarations
when customizing an eFMU. They define template selection, typed context, and
the checksum dependency graph.

The built-in examples are:

- `galec/target.toml` — Algorithm Code eFMU.
- `efmi/target.toml` — Algorithm Code plus Production Code eFMU.
- `galec-c/target.toml` — unpackaged GALEC-derived C/H.
