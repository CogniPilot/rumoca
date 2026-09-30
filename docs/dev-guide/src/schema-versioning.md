# IR Schema Versioning

Rumoca's serialized IRs are exact-version wires. DAE and Solve JSON must carry
an explicit `schema_version`, and deserializers reject every version other than
the current one instead of guessing.

## The JSON dumps are not a stable interface

`--emit dae-json` and `--emit flat-json` are exact-version debug and replay
dumps. A DAE dump carries `schema_version` (the `DAE_SCHEMA_VERSION` constant),
but the reader accepts only the version of the compiler build that wrote it,
and the number changes as the IR evolves; there are no readers or adapters for
older versions. A Flat dump has no schema version at all. Neither format
promises field names, enum tags, or layout across releases, so tools should not
parse them as an interface. The stable exchange format for model interfaces is
the FMI `modelDescription.xml` of the `fmi2` and `fmi3` targets.

A DAE variable's `role` is its runtime classification in the MLS Appendix B
partition (`parameter`, `constant`, `input`, `state`, `algebraic`, `output`,
`discrete_real`, `discrete_value`), not its declared prefix. A declared
`output` that is differentiated has role `state`, and one assigned by a discrete
equation has role `discrete_value`, so `role == "output"` misses such outputs.
The declaration's prefix is the separate `declared_causality` attribute
(`none`, `input`, or `output`) at every nesting depth, while `causality` is the
exported causality, which is `input` or `output` only for top-level
declarations. In FMI exports a nested declaration keeps `causality="local"` and
carries its declared prefix as a namespaced `rumoca` annotation.

The policy is:

- Keep the same schema version only for additive fields that have a semantic
  default and are annotated with `#[serde(default)]`.
- Bump the schema version for renamed fields, removed fields, changed enum
  tags, changed units, changed indexing conventions, or changed interpretation
  of an existing field.
- Do not add silent in-crate migrators to `Deserialize`. Migration should be an
  explicit tool or phase so stale fixtures and artifacts fail visibly.
- Commit or update golden fixtures with every intentional schema-shape change.

Worked example:

```rust
pub const DAE_SCHEMA_VERSION: u16 = 1;

#[derive(Deserialize, Serialize)]
pub struct DaeClockPartition {
    pub schedules: Vec<ClockSchedule>,

    // Same-version additive change: old artifacts deserialize as no triggers.
    #[serde(default)]
    pub triggered_conditions: Vec<Expression>,
}
```

If `schedules` is renamed to `periodic_schedules`, the change is incompatible:

```rust
pub const DAE_SCHEMA_VERSION: u16 = 2;

#[derive(Deserialize)]
struct DaeWire {
    schema_version: u16,
    periodic_schedules: Vec<ClockSchedule>,
}

impl<'de> Deserialize<'de> for Dae {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = DaeWire::deserialize(deserializer)?;
        if wire.schema_version != DAE_SCHEMA_VERSION {
            return Err(serde::de::Error::custom("unsupported DAE schema_version"));
        }
        Ok(Self::from_wire(wire))
    }
}
```

Review checklist:

- Update the relevant `*_SCHEMA_VERSION` constant for incompatible changes.
- Keep old versions rejected by the primary IR deserializer.
- Add JSON and bincode round-trip coverage for the changed shape.
- Update committed goldens so reviewers can see the exact wire-format delta.
