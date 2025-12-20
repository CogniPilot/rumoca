# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.7.23] - 2025-12-19

### Changed
- **Simplified DAE IR format** - Removed custom `dae_ir` module; `to_dae_ir_json()` now directly serializes the `Dae` struct using serde for better maintainability, this should also allow array output mode
- **Renamed `inferred_type` to `declared_type`** in `DefinedSymbol` for clarity (the type is parsed from declarations, not inferred)
- **Unified symbol resolution** - LSP and linter now share `ReferenceCheckConfig` for consistent undefined variable detection

### Removed
- Removed `src/dae/dae_ir/` module (5 files) - replaced with direct serde serialization
- Removed legacy/backward compatibility code and misleading comments
- Removed redundant `is_balanced` field from `BalanceResult` (replaced with `is_balanced()` method)
- Removed unused `as_map()` method from `ScopeResolver`

### Fixed
- Fixed 3 ignored doc tests by using proper `rumoca::` imports instead of `crate::`
- Fixed VSCode extension icon location
- Fixed misleading MODELICAPATH warning when compiling standalone model files

## [0.7.22] - 2025-12-08

### Changed
- Moved templates from `templates/examples/` to `examples/templates/` for better project organization
- Reorganized examples directory structure:
  - `examples/rust/` - Rust usage examples
  - `examples/templates/` - Jinja templates (CasADi, SymPy, Base Modelica)
  - `examples/wasm_editor/` - Browser-based WASM demo

### Fixed
- Fixed Python bindings CI paths after move to `bindings/python/`

## [0.7.21] - 2025-12-07

### Added
- **WebAssembly (WASM) Support**
  - Compile Modelica models directly in the browser
  - Multi-threaded compilation using Web Workers and `wasm-bindgen-rayon`
  - Full LSP features available in WASM (diagnostics, hover, completion)
  - Template rendering with MiniJinja in browser
  - Live demo at [cognipilot.github.io/rumoca](https://cognipilot.github.io/rumoca/)
- **WASM Editor Demo**
  - Monaco-based editor with split-pane Modelica and Jinja2 editing
  - Real-time template preview
  - DAE IR JSON export
  - Autocomplete for template variables (`dae.x`, `dae.u`, etc.)
- GitHub Pages deployment for WASM demo in CI

### Changed
- Upgraded to Parol 4.2.1
- Reorganized project scripts to `tools/` directory

## [0.7.20] - 2025-12-06

### Added
- **Flattened model caching** - DAE compilation results cached to `~/.cache/rumoca/dae/`
- Cache works like `ccache` - invalidated when source files or compiler version change

### Changed
- **Significant performance improvements**
  - Optimized parsing and flattening pipeline
  - Balance check warm cache: 263,242 models/sec (vs 73.5 cold)
- Improved documentation on caching behavior

## [0.7.19] - 2025-12-05

### Fixed
- Formatter losing type modifications and assignment issues
- Various formatter edge cases

## [0.7.18] - 2025-12-05

### Added
- LSP caching for faster responses
- Collapsable annotations in LSP

### Changed
- Improved MSL (Modelica Standard Library) support

## [0.7.17] - 2025-12-04

### Added
- Import autocomplete in LSP

## [0.7.16] - 2025-12-04

### Added
- Array comprehension support
- `each` modifier support
- `-L` command line option for library paths
- VS Code extension setting for Modelica library paths

### Changed
- Improved MSL compatibility

## [0.7.15] - 2025-12-03

### Added
- MSL (Modelica Standard Library) test suite
- Demo GIF in documentation

### Changed
- Improved MSL compatibility
- Added timing information to compilation output

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
