use crate::LirId;

/// A queue of operator arguments that nested operators can be flattened into.
///
/// The arguments are stored in reverse, so that taking the next argument and prepending a nested
/// operator's arguments both work at the end of the vector, without shifting the rest.
#[derive(Debug, Clone)]
pub struct ArgList {
    reversed: Vec<LirId>,
}

impl ArgList {
    pub fn new(mut args: Vec<LirId>) -> Self {
        args.reverse();
        Self { reversed: args }
    }

    pub fn next(&mut self) -> Option<LirId> {
        self.reversed.pop()
    }

    /// Puts the arguments before the remaining ones, in their order.
    pub fn prepend(&mut self, args: Vec<LirId>) {
        self.reversed.extend(args.into_iter().rev());
    }
}

#[cfg(test)]
mod tests {
    use id_arena::Arena;

    use super::*;
    use crate::Lir;

    #[test]
    fn prepend_keeps_order() {
        let mut arena = Arena::<Lir>::new();
        let ids: Vec<LirId> = (0..5u8).map(|i| arena.alloc(Lir::Atom(vec![i]))).collect();

        let mut list = ArgList::new(ids[3..5].to_vec());
        assert_eq!(list.next(), Some(ids[3]));
        list.prepend(ids[0..3].to_vec());
        list.prepend(vec![]);
        let mut rest = Vec::new();
        while let Some(id) = list.next() {
            rest.push(id);
        }
        assert_eq!(rest, vec![ids[0], ids[1], ids[2], ids[4]]);
        assert_eq!(list.next(), None);
    }
}
