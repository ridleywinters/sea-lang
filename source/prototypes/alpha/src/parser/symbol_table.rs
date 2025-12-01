use std::collections::HashMap;

/// Represents different kinds of symbols that can be defined
#[derive(Debug, Clone)]
pub enum SymbolKind {
    Function,
    Variable,
    Parameter,
    Type,
}

/// Information about a symbol
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub is_public: bool,
}

/// A symbol table for tracking definitions within a scope.
/// Each file gets its own symbol table to prevent symbol leakage.
#[derive(Debug)]
pub struct SymbolTable {
    /// Stack of scopes, innermost scope is last
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    /// Create a new symbol table with a single (file-level) scope
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    /// Enter a new scope (e.g., function body, block)
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a symbol in the current scope
    pub fn define(&mut self, symbol: Symbol) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&symbol.name) {
            return Err(format!("Symbol '{}' is already defined in this scope", symbol.name));
        }
        current_scope.insert(symbol.name.clone(), symbol);
        Ok(())
    }

    /// Look up a symbol, searching from innermost to outermost scope
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    /// Check if a symbol is defined in the current scope only
    pub fn is_defined_in_current_scope(&self, name: &str) -> bool {
        self.scopes.last().unwrap().contains_key(name)
    }

    /// Get all public symbols from the file-level scope
    pub fn public_symbols(&self) -> Vec<&Symbol> {
        self.scopes
            .first()
            .map(|scope| scope.values().filter(|s| s.is_public).collect())
            .unwrap_or_default()
    }
}
