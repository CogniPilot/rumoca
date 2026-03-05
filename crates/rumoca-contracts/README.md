# rumoca-contracts

MLS contract registry, runner, and compliance reporting framework.

## Role in Rumoca
Encodes compiler-compliance contracts as executable checks and metadata, so compliance progress is measurable and reportable.

## Public Surface
- Public modules: `registry`, `runner`, `report`, `test_support`.
- Key types: `ContractRegistry`, `Contract`, `ContractId`, `ContractStatus`, `ComplianceReport`, `TestRunner`, `ContractResult`.
- Registry entry points: `create_registry()`, `registry_template()`.
- Public implemented-contract manifest: `IMPLEMENTED_CONTRACT_IDS`.

## Inputs
- Static contract tables and contract execution outcomes.

## Outputs
- Contract-level pass/fail results.
- Aggregated compliance summaries and reports.

## Design Constraints
- Keep contract IDs and metadata stable for traceability.
- Separate compliance framework logic from phase implementations.
- Preserve deterministic counting and status aggregation.
