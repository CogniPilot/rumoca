# Security Policy

## Supported Versions

Security fixes are handled on the default branch first. Release tags and binary
artifacts are supported only when they are published from this repository's
GitHub Releases workflow.

## Reporting a Vulnerability

Please report suspected vulnerabilities privately by emailing
security@climamind.com. Include:

- affected commit, release, or artifact;
- reproduction steps or a minimal input file when possible;
- expected impact and whether the issue affects CLI, LSP, Python bindings,
  WASM, VS Code packaging, or generated code.

Do not open a public issue for vulnerabilities involving arbitrary code
execution, path traversal, malicious Modelica inputs, supply-chain compromise,
or release artifact integrity.

## Public Disclosure

ClimaMind will coordinate a fix, credit, and disclosure timeline after the issue
is confirmed. If the issue also affects upstream Rumoca or third-party
dependencies, we will coordinate with the relevant maintainers.
