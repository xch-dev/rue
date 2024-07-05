use clvmr::{reduction::EvalErr, Allocator, NodePtr};
use num_bigint::BigInt;
use num_traits::One;

pub fn path_to_node(allocator: &mut Allocator, path: &BigInt) -> Result<NodePtr, EvalErr> {
    let bytes = path.to_signed_bytes_be();
    let mut slice = bytes.as_slice();
    while (!slice.is_empty()) && (slice[0] == 0) {
        slice = &slice[1..];
    }
    allocator.new_atom(slice)
}

pub fn compose_paths(a: BigInt, mut b: BigInt) -> BigInt {
    let mut mask = BigInt::one();
    let mut temp_path = a.clone();
    while temp_path > BigInt::one() {
        b <<= 1;
        mask <<= 1;
        temp_path >>= 1;
    }

    mask -= 1;
    b | (a & mask)
}

pub fn first_path(path: BigInt) -> BigInt {
    compose_paths(path, BigInt::from(2))
}

pub fn rest_path(path: BigInt) -> BigInt {
    compose_paths(path, BigInt::from(3))
}
