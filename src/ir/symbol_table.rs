use crate::ir::visitor::Visitable;
use crate::ir::visitors::symbol_table_builder::SymbolTableBuilder;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub name: String,
    // You can add more fields here, such as type, location, etc.
}

#[derive(Debug, Default, Clone)]
pub struct SymbolTable {
    pub symbols: HashMap<String, SymbolInfo>,
    pub children: Vec<SymbolTable>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            children: Vec::new(),
        }
    }

    pub fn insert(&mut self, name: String, info: SymbolInfo) {
        self.symbols.insert(name, info);
    }

    pub fn get(&self, name: &str) -> Option<&SymbolInfo> {
        self.symbols.get(name)
    }
}

pub fn build_symbol_table(def: &mut crate::ir::ast::StoredDefinition) -> SymbolTable {
    let mut builder = SymbolTableBuilder::new();
    def.accept(&mut builder);
    builder.root
}
