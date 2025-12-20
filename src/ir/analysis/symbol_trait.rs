//! Common trait for symbol information.
//!
//! This module provides a unified interface for accessing symbol information
//! across different symbol representations in the codebase.

use super::type_inference::SymbolType;

/// Common interface for symbol information.
///
/// This trait provides a unified way to access symbol properties across
/// different representations:
/// - [`DefinedSymbol`](super::symbols::DefinedSymbol) - Used in analysis/diagnostics
/// - [`Symbol`](super::symbol_table::Symbol) - Used in flattening/scope resolution
/// - [`WorkspaceSymbol`](crate::lsp::workspace::WorkspaceSymbol) - Used in LSP features
///
/// # Example
///
/// ```
/// use rumoca::ir::analysis::symbol_trait::SymbolInfo;
///
/// fn print_symbol_info(sym: &impl SymbolInfo) {
///     println!("Symbol: {} at line {}", sym.name(), sym.line());
///     if sym.is_parameter() {
///         println!("  (parameter)");
///     }
/// }
/// ```
pub trait SymbolInfo {
    /// The symbol's local name (identifier).
    ///
    /// For a component `x` in model `M`, this returns `"x"`.
    fn name(&self) -> &str;

    /// The symbol's qualified name (full path).
    ///
    /// For a component `x` in model `M.N`, this might return `"M.N.x"`.
    /// If not available, defaults to the local name.
    fn qualified_name(&self) -> &str {
        self.name()
    }

    /// The symbol's type.
    ///
    /// Returns `SymbolType::Unknown` if type information is not available.
    fn symbol_type(&self) -> SymbolType;

    /// Source line number (1-based).
    fn line(&self) -> u32;

    /// Source column number (1-based).
    fn column(&self) -> u32;

    /// Whether this symbol is a parameter.
    fn is_parameter(&self) -> bool;

    /// Whether this symbol is a constant.
    fn is_constant(&self) -> bool {
        false
    }

    /// Whether this symbol is a class (type, function, etc.).
    fn is_class(&self) -> bool {
        false
    }
}
