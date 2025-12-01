---
name: LSP Feature Request
about: Request a new Language Server feature for the VS Code extension
title: '[LSP] '
labels: enhancement, lsp
assignees: ''
---

## Feature Summary
<!-- Brief description of the LSP feature -->

## Roadmap Phase
<!-- Which phase from ROADMAP.md does this belong to? -->
- [ ] Phase 1: Foundation
- [ ] Phase 2: Navigation
- [ ] Phase 3: Modelica Intelligence
- [ ] Phase 4: Refactoring
- [ ] Phase 5: Polish

## LSP Method(s)
<!-- Which LSP protocol methods are involved? -->
- `textDocument/...`

## Expected Behavior
<!-- What should this feature do? -->

## Example
<!-- Code example showing the feature in action -->
```modelica
model Example
  Real x;
equation
  der(x) = 1;
end Example;
```

## Implementation Notes
<!-- Any technical considerations -->

## Acceptance Criteria
- [ ] Feature implemented in `rumoca_lsp.rs`
- [ ] Server capabilities updated
- [ ] Tests added
- [ ] ROADMAP.md updated
