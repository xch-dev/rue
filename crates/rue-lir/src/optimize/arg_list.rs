use std::mem;

use crate::LirId;

#[derive(Debug, Clone)]
pub struct ArgList {
    args: Vec<LirId>,
}

impl ArgList {
    pub fn new(args: Vec<LirId>) -> Self {
        Self { args }
    }

    pub fn next(&mut self) -> Option<LirId> {
        if self.args.is_empty() {
            None
        } else {
            Some(self.args.remove(0))
        }
    }

    pub fn prepend(&mut self, args: Vec<LirId>) {
        let new_args = [args, mem::take(&mut self.args)].concat();
        self.args = new_args;
    }
}
