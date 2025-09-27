use clvmr::Allocator;
use num_bigint::BigInt;

pub fn bigint_atom(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let ptr = allocator.new_number(value).unwrap();
    allocator.atom(ptr).to_vec()
}

pub fn atom_bigint(value: impl AsRef<[u8]>) -> BigInt {
    let mut allocator = Allocator::new();
    let ptr = allocator.new_atom(value.as_ref()).unwrap();
    allocator.number(ptr)
}
