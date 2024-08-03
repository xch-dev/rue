#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypePath {
    First,
    Rest,
}

pub fn index_to_path(index: usize, nil_terminated: bool) -> Vec<TypePath> {
    let mut path = Vec::with_capacity(index);
    for _ in 0..index {
        path.push(TypePath::Rest);
    }
    if nil_terminated {
        path.push(TypePath::First);
    }
    path
}
