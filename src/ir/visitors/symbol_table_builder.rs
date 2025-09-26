use crate::ir::ast::{ClassDefinition, Component};
use crate::ir::symbol_table::{SymbolInfo, SymbolTable};
use crate::ir::visitor::Visitor;

/// SymbolTableBuilder constructs and manages a hierarchical symbol table structure.
///
/// It maintains a stack of scope indices (`scope_stack`) to track the current scope
/// as the AST is traversed. Each scope corresponds to a `SymbolTable`, allowing for
/// nested symbol tables (e.g., for classes or blocks).
pub struct SymbolTableBuilder {
    /// The root symbol table, representing the global scope.
    pub root: SymbolTable,
    /// Stack of indices representing the path from the root to the current scope.
    scope_stack: Vec<usize>,
}

impl SymbolTableBuilder {
    /// Creates a new `SymbolTableBuilder` with an empty root symbol table and no active scopes.
    pub fn new() -> Self {
        Self {
            root: SymbolTable::new(),
            scope_stack: vec![],
        }
    }

    /// Returns a mutable reference to the current symbol table for the active scope.
    ///
    /// Traverses the `scope_stack` from the root, following child indices to reach
    /// the symbol table corresponding to the innermost scope.
    ///
    /// # Panics
    /// Panics if the scope stack contains invalid indices.
    fn current_table_mut(&mut self) -> &mut SymbolTable {
        let mut table = &mut self.root;
        for &idx in &self.scope_stack {
            table = &mut table.children[idx];
        }
        table
    }
}

impl Visitor for SymbolTableBuilder {
    /// Called when entering a class definition node in the AST.
    ///
    /// Inserts the class name into the current symbol table, then creates a new child
    /// symbol table for the class scope and updates the scope stack to enter it.
    fn enter_class_definition(&mut self, class: &mut ClassDefinition) {
        //println!("Entering class: {}", class.name.text);
        let class_name = class.name.text.clone();
        let symbol_info = SymbolInfo {
            name: class_name.clone(),
        };
        self.current_table_mut().insert(class_name, symbol_info);
        let children = &mut self.current_table_mut().children;
        let idx_of_new_child = children.len();
        children.push(SymbolTable::new());
        self.scope_stack.push(idx_of_new_child);
    }

    /// Called when exiting a class definition node in the AST.
    ///
    /// Pops the scope stack to return to the parent scope.
    fn exit_class_definition(&mut self, _class: &mut ClassDefinition) {
        //println!("Exiting class: {}", _class.name.text);
        self.scope_stack.pop();
    }

    /// Called when entering a component node in the AST.
    ///
    /// Inserts the component name into the current symbol table.
    fn enter_component(&mut self, comp: &mut Component) {
        //println!("Entering component: {}", comp.name);
    }
}
