use clvmr::Allocator;
use num_bigint::BigInt;

pub fn int_to_atom(num: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let ptr = allocator.new_number(num).unwrap();
    let atom = allocator.atom(ptr);
    let slice = atom.as_ref();
    slice.to_vec()
}
