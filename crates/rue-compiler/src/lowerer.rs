use crate::Database;

pub struct Lowerer<'a> {
    database: &'a mut Database,
}

impl<'a> Lowerer<'a> {
    pub fn new(database: &'a mut Database) -> Self {
        Self { database }
    }
}
