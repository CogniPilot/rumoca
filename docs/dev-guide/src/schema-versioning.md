# IR Schema Versioning

Rumoca's serialized IRs are compatibility contracts, not debug dumps. DAE and
Solve JSON must carry an explicit `schema_version`, and deserializers reject
unsupported versions instead of guessing.

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
