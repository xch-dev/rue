use clvmr::Allocator;
use num_bigint::BigInt;

pub fn bigint_to_bytes(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let ptr = allocator.new_number(value).unwrap();
    let atom = allocator.atom(ptr);
    atom.as_ref().to_vec()
}
