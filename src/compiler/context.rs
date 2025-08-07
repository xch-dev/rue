use crate::{Database, Scope};

#[derive(Debug, Clone)]
pub struct Context {
    db: Database,
    scope_stack: Vec<Scope>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            db: Database::new(),
            scope_stack: vec![Scope::new()],
        }
    }
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }
}
