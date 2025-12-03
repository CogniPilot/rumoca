# Release Setup Guide

This guide explains how to set up the secrets required for automated releases.

## Overview

When you push a tag matching `v*` (e.g., `v0.7.4`), the release workflow automatically:

1. Creates a GitHub Release with Rust binaries
2. Publishes to **crates.io** (Rust package)
3. Publishes to **PyPI** (Python package)
4. Publishes to **VS Code Marketplace** (extension)
5. Optionally publishes to **Open VSX** (VS Code alternative)

## Required Secrets

Go to your repository: **Settings → Secrets and variables → Actions → New repository secret**

### 1. CARGO_REGISTRY_TOKEN (crates.io)

**Purpose:** Publish Rust crate to crates.io

**Setup:**
1. Go to [crates.io](https://crates.io/)
2. Log in with your GitHub account
3. Go to **Account Settings** → **API Tokens**
4. Click **New Token**
5. Name it `github-actions` or similar
6. Copy the token (starts with `cio...`)
7. Add as secret: `CARGO_REGISTRY_TOKEN`

**First-time setup:**
```bash
# Verify you own the crate (one-time)
cargo login <your-token>
cargo publish --dry-run
```

### 2. PYPI_API_TOKEN (PyPI)

**Purpose:** Publish Python wheels to PyPI

**Setup:**
1. Go to [pypi.org](https://pypi.org/)
2. Create an account or log in
3. Go to **Account Settings** → **API tokens**
4. Click **Add API token**
5. Name: `github-actions-rumoca`
6. Scope: **Entire account** (first time) or **Project: rumoca** (after first publish)
7. Copy the token (starts with `pypi-...`)
8. Add as secret: `PYPI_API_TOKEN`

**First-time setup:**
The first publish must be done manually to claim the package name:
```bash
cd python
pip install maturin twine
maturin build --release
twine upload target/wheels/*.whl
```

**Alternative: Trusted Publishing (recommended)**

PyPI supports trusted publishing which doesn't require a token:
1. Go to [pypi.org](https://pypi.org/) → Your project → **Publishing**
2. Add a new **trusted publisher**:
   - Owner: `jgoppert` (or your GitHub username)
   - Repository: `rumoca`
   - Workflow: `release.yml`
   - Environment: `pypi`
3. Remove the `password:` line from the workflow (it uses OIDC instead)

### 3. VSCE_PAT (VS Code Marketplace)

**Purpose:** Publish VS Code extension to the marketplace

**Setup:**
1. Go to [Azure DevOps](https://dev.azure.com/)
2. Sign in with the same Microsoft account used for VS Code Marketplace
3. Click your profile icon → **Personal access tokens**
4. Click **New Token**
5. Name: `vsce-github-actions`
6. Organization: **All accessible organizations**
7. Expiration: **Custom defined** (max 1 year)
8. Scopes: Click **Show all scopes**, then select:
   - **Marketplace** → **Manage** (check this)
9. Click **Create**
10. Copy the token immediately (you won't see it again)
11. Add as secret: `VSCE_PAT`

**First-time setup:**
```bash
# Verify publisher exists
npx @vscode/vsce login <publisher-name>

# If you need to create a publisher:
# Go to https://marketplace.visualstudio.com/manage
# Click "Create publisher" and follow the steps
```

**Note:** The publisher name in `package.json` must match your marketplace publisher.

### 4. OVSX_TOKEN (Open VSX - Optional)

**Purpose:** Publish to Open VSX Registry (VS Code alternative for open source)

**Setup:**
1. Go to [open-vsx.org](https://open-vsx.org/)
2. Log in with GitHub
3. Go to **Settings** → **Access Tokens**
4. Create a new token
5. Add as secret: `OVSX_TOKEN`

This is optional - the workflow will continue if it fails.

## GitHub Environment (Optional but Recommended)

For PyPI publishing, you can set up a GitHub environment for additional protection:

1. Go to **Settings → Environments → New environment**
2. Name it `pypi`
3. Configure protection rules:
   - **Required reviewers**: Add yourself (optional)
   - **Wait timer**: 0 minutes
   - **Deployment branches**: Only `main` or tags matching `v*`

This adds an extra layer of security for PyPI publishing.

## Creating a Release

Once secrets are configured:

```bash
# Update versions in:
# - Cargo.toml (version = "x.y.z")
# - python/pyproject.toml (version = "x.y.z")
# - editors/vscode/package.json (version = "x.y.z")

# Commit changes
git add -A
git commit -m "Release v0.7.4"

# Create and push tag
git tag v0.7.4
git push origin main --tags
```

The release workflow will automatically:
1. Build binaries for Linux, macOS, Windows
2. Publish to crates.io
3. Build Python wheels for all platforms
4. Publish to PyPI
5. Build and publish VS Code extension

## Troubleshooting

### "Authentication failed" for crates.io
- Check that `CARGO_REGISTRY_TOKEN` is set correctly
- Tokens expire - generate a new one if needed
- Make sure the crate name isn't taken by someone else

### "403 Forbidden" for PyPI
- The token may have expired
- First publish must be done manually to claim the package name
- Check token scope matches the project

### "Personal Access Token is invalid" for VS Code
- PAT tokens expire (max 1 year)
- Check the token has "Marketplace: Manage" scope
- Verify the organization is "All accessible organizations"

### Version already exists
- Workflows use `skip-existing` / `continue-on-error` for this
- Bump the version number before releasing

## Security Notes

- Never commit tokens to the repository
- Rotate tokens periodically (especially VSCE_PAT which expires)
- Use minimal scopes for each token
- Consider using GitHub Environments for production deployments
