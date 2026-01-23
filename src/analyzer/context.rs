use crate::parser::ast::{Type, TypeDecl};
use std::borrow::Cow;
use std::collections::HashMap;

/// Type context that tracks variables and types in the current scope
#[derive(Debug, Clone)]
pub struct TypeContext<'de> {
    /// Variables in the current scope
    variables: HashMap<Cow<'de, str>, Type<'de>>,
    /// Type declarations (structs, enums)
    types: HashMap<Cow<'de, str>, TypeDecl<'de>>,
    /// Parent scope (for nested scopes)
    parent: Option<Box<TypeContext<'de>>>,
}

impl<'de> TypeContext<'de> {
    /// Create a new root type context
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            types: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new child scope
    pub fn new_scope(&self) -> Self {
        Self {
            variables: HashMap::new(),
            types: HashMap::new(),
            parent: Some(Box::new(self.clone())),
        }
    }

    /// Add a variable to the current scope
    pub fn add_variable(&mut self, name: Cow<'de, str>, ty: Type<'de>) {
        self.variables.insert(name, ty);
    }

    /// Lookup a variable, searching parent scopes
    pub fn lookup_variable(&self, name: &str) -> Option<&Type<'de>> {
        self.variables.get(name).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.lookup_variable(name))
        })
    }

    /// Add a type declaration
    pub fn add_type(&mut self, name: Cow<'de, str>, decl: TypeDecl<'de>) {
        self.types.insert(name, decl);
    }

    /// Lookup a type declaration
    pub fn lookup_type(&self, name: &str) -> Option<&TypeDecl<'de>> {
        self.types.get(name).or_else(|| {
            self.parent
                .as_ref()
                .and_then(|p| p.lookup_type(name))
        })
    }

    /// Check if a variable exists in the current scope (not parent scopes)
    #[allow(dead_code)]
    pub fn has_variable_in_current_scope(&self, name: &str) -> bool {
        self.variables.contains_key(name)
    }

    /// Get all variables in the current scope
    #[allow(dead_code)]
    pub fn current_scope_variables(&self) -> &HashMap<Cow<'de, str>, Type<'de>> {
        &self.variables
    }
}

impl<'de> Default for TypeContext<'de> {
    fn default() -> Self {
        Self::new()
    }
}
