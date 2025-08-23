use clvmr::Allocator;
use num_bigint::BigInt;

pub fn bigint_atom(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let ptr = allocator.new_number(value).unwrap();
    allocator.atom(ptr).to_vec()
}
