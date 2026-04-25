# Open Source Readiness

This checklist defines what must be true before making the ClimaMind-maintained
Rumoca repository public or publishing artifacts from it.

## Repository Boundary

Publish only the Rumoca compiler, tooling, editor, binding, and packaging
sources tracked by git. Keep the ClimaMind product layer outside this
repository:

- no customer, building, site, or calibration data;
- no Kelvin runtime artifacts, training outputs, or top-down experiment results;
- no cloud account identifiers, private buckets, tokens, API keys, or local
  machine credentials;
- no generated `target/`, downloaded MSL trees, local caches, release staging
  directories, or one-off probe outputs.

## Required Preflight

Run these checks from the repository root before flipping repository visibility:

```bash
git status --short --branch
git ls-files | rg -i '(^|/)(\.env|.*\.pem|.*\.key|.*\.p12|.*\.sqlite|.*\.db|.*\.zip|.*\.tar|.*\.gz|.*\.parquet|.*\.csv|.*\.jsonl)$|secret|credential|token'
git log --all --name-only --pretty=format: | sort -u | rg -i '(^|/)(\.env|.*\.pem|.*\.key|.*\.p12|.*\.sqlite|.*\.db|.*\.zip|.*\.tar|.*\.gz|.*\.parquet|.*\.csv|.*\.jsonl)$|secret|credential|token'
cargo metadata --format-version 1 > target/open-source-cargo-metadata.json
jq -r '.packages[] | select(.source != null) | [.name, .version, (.license // "NO_LICENSE")] | @tsv' target/open-source-cargo-metadata.json | sort -u
```

Treat any secret, credential, private artifact, or unexplained non-source file
as a blocker. If a blocker appears only in git history, rewrite or recreate the
public repository instead of publishing that history.

## License Position

Rumoca is Apache-2.0. Preserve:

- `LICENSE`;
- `NOTICE`;
- license fields in Cargo, Python, and VS Code package metadata;
- third-party notices required by bundled binary/editor artifacts.

The observed Cargo graph is mostly permissive. Known licenses that require
release attention are MPL-2.0 for `option-ext`, Unicode-3.0 packages, and
dual-licensed packages where the permissive option should be selected. npm
packages must be checked separately before VS Code or web-editor distribution.

## Release Surfaces

Source publication is lower risk than binary publication. Before publishing
GitHub Releases, PyPI wheels, VS Code `.vsix` files, Docker images, or GitHub
Pages assets, verify each artifact independently:

- the artifact contains `LICENSE` and required notices;
- package metadata points at the public ClimaMind repository;
- generated files do not embed local absolute paths except in tests or fixtures;
- release workflows publish under the intended GitHub organization and package
  namespace;
- the `ghcr.io/climamind/rumoca-dev:main` image exists before requiring CI jobs
  that use it as a container;
- MSL data is downloaded from the official release during CI and not vendored
  into the repository.

## Upstream Attribution

This repository should state that Rumoca originated in the CogniPilot community
and that ClimaMind maintains this public line. Keep attribution factual; do not
imply that ClimaMind owns upstream trademarks, third-party packages, or Modelica
Association materials.
