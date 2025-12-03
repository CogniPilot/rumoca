# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.7.14] - 2025-12-03

### Added
- **VS Code Extension: Bundled Language Server**
  - Platform-specific extensions now include bundled `rumoca-lsp` binary
  - No separate installation required for most users
  - Automatic fallback to system-installed server with warning
  - New `rumoca.useSystemServer` setting to prefer system server
  - New `rumoca.debug` setting for verbose logging

### Changed
- VS Code extension installation simplified - works out of the box
- CI workflow builds platform-specific `.vsix` files (win32-x64, darwin-x64, darwin-arm64, linux-x64, linux-arm64)

## [0.7.0] - 2025-11-30

### Added
- Enhanced error messages with source location information
  - Added `get_location()` methods to `Expression`, `ComponentReference`, and `Equation` AST nodes
  - Added `loc_info()` and `expr_loc_info()` helper functions for error formatting
  - All `todo!()` calls converted to proper `anyhow::bail!()` with location context
- Beautiful error diagnostics using miette with syntax highlighting
- Support for `type` class specifiers (type aliases like `type Voltage = Real(unit="V")`)

### Changed
- Improved BLT (Block Lower Triangular) transformation
  - Removed unused `index` field from `EquationInfo` struct
  - Cleaner Tarjan's SCC algorithm implementation
- Code quality improvements
  - Applied clippy suggestions for cleaner code
  - Removed all dead/unused code

### Fixed
- All compiler warnings resolved
- Removed unused imports and functions

## [0.6.0] - 2024-11-15

### Added
- Comprehensive GitHub Actions CI/CD pipeline
  - Multi-platform testing (Linux, macOS, Windows)
  - Code formatting checks with `rustfmt`
  - Linting with `clippy`
  - Documentation building
  - Code coverage with `cargo-tarpaulin`
  - MSRV (Minimum Supported Rust Version) checking
- Automated release workflow
  - Cross-platform binary builds
  - Automatic crates.io publishing
- High-level `Compiler` API for library usage
  - Builder pattern for configuration
  - `compile_file()` and `compile_str()` methods
  - `CompilationResult` with timing information
  - Template rendering methods
- Two complete usage examples:
  - `examples/basic_usage.rs` - String-based compilation
  - `examples/file_compilation.rs` - File-based compilation
- Comprehensive code quality improvements:
  - Created `src/ir/constants.rs` for centralized constants
  - Replaced all panic!() calls with proper Result-based error handling
  - Created custom error types (`IrError`, `DaeError`)
  - Added 20 automated tests (parser, flattening, DAE creation)
  - Removed all dead code and magic strings
- Enhanced documentation:
  - Completely rewritten README with examples
  - Added API documentation to public functions
  - Created CONTRIBUTING.md guidelines
  - Added CHANGELOG.md

### Changed
- Refactored main.rs to use new Compiler API (38% code reduction)
- Improved error messages with better context
- Updated documentation for clarity and completeness
- Made `verbose` flag consistent across CLI and API

### Fixed
- Replaced unsafe unwrap() calls with proper error handling
- Fixed test capitalization issue (Nightvapor → NightVapor)
- Removed typo in flatten.rs ("expaand" → "expand")
- Fixed pre_finder.rs documentation (was copy-pasted from state_finder)

### Supported Modelica Features
- Basic models with equations
- State variables and derivatives (der())
- Previous values (pre())
- Parameters and constants
- Input/output variables
- Hierarchical models with flattening
- Extend clauses
- When clauses
- If equations
- Mathematical functions (sin, cos, tan)

## [0.5.0] and earlier

See git history for changes in earlier versions.

---

## Legend

- `Added` for new features
- `Changed` for changes in existing functionality
- `Deprecated` for soon-to-be removed features
- `Removed` for now removed features
- `Fixed` for any bug fixes
- `Security` in case of vulnerabilities
