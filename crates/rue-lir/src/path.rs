use num_bigint::BigUint;

pub fn path_to_atom(path: &BigUint) -> Vec<u8> {
    let bytes = path.to_bytes_be();
    let first_nonzero = bytes
        .iter()
        .position(|&byte| byte != 0)
        .unwrap_or(bytes.len());
    bytes[first_nonzero..].to_vec()
}

pub fn first_path(path: &BigUint) -> BigUint {
    add_path(path, BigUint::from(2u8))
}

pub fn rest_path(path: &BigUint) -> BigUint {
    add_path(path, BigUint::from(3u8))
}

fn add_path(a: &BigUint, b: BigUint) -> BigUint {
    let depth = a.bits().saturating_sub(1);
    let mask = (BigUint::from(1u8) << depth) - 1u8;
    (b << depth) | (a & mask)
}

pub fn parent_path(path: &BigUint) -> Option<BigUint> {
    let result_depth = path.bits().saturating_sub(1);

    if result_depth == 0 {
        return None;
    }

    let original_depth = result_depth - 1;
    let mask = (BigUint::from(1u8) << original_depth) - 1u8;
    let lower_bits = path & mask;
    let result = (BigUint::from(1u8) << original_depth) | lower_bits;

    Some(result)
}

/// Whether the path reads into the value at the other one, so that it fails if that value is an
/// atom. Path 0 is nil rather than a value in the environment, so nothing is below it.
pub fn is_below(path: &BigUint, ancestor: &BigUint) -> bool {
    let depth = ancestor.bits().saturating_sub(1);
    let mask = (BigUint::from(1u8) << depth) - 1u8;
    ancestor.bits() > 0 && path.bits() > ancestor.bits() && (path & &mask) == (ancestor & &mask)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_to_atom() {
        assert_eq!(path_to_atom(&0u8.into()), Vec::<u8>::new());
        assert_eq!(path_to_atom(&1u8.into()), [0x01]);
        assert_eq!(path_to_atom(&3u8.into()), [0x03]);
        assert_eq!(path_to_atom(&128u8.into()), [0x80]);
        assert_eq!(path_to_atom(&300u16.into()), [0x01, 0x2C]);
    }

    #[test]
    fn test_first_path() {
        assert_eq!(first_path(&1u8.into()), 2u8.into());
        assert_eq!(first_path(&2u8.into()), 4u8.into());
        assert_eq!(first_path(&5u8.into()), 9u8.into());
    }

    #[test]
    fn test_rest_path() {
        assert_eq!(rest_path(&1u8.into()), 3u8.into());
        assert_eq!(rest_path(&2u8.into()), 6u8.into());
        assert_eq!(rest_path(&5u8.into()), 13u8.into());
    }

    #[test]
    fn test_parent_path() {
        assert_eq!(parent_path(&0u8.into()), None);
        assert_eq!(parent_path(&1u8.into()), None);
        assert_eq!(parent_path(&2u8.into()), Some(1u8.into()));
        assert_eq!(parent_path(&3u8.into()), Some(1u8.into()));
        assert_eq!(parent_path(&4u8.into()), Some(2u8.into()));
        assert_eq!(parent_path(&6u8.into()), Some(2u8.into()));
        assert_eq!(parent_path(&5u8.into()), Some(3u8.into()));
        assert_eq!(parent_path(&7u8.into()), Some(3u8.into()));
        assert_eq!(parent_path(&8u8.into()), Some(4u8.into()));
        assert_eq!(parent_path(&12u8.into()), Some(4u8.into()));
        assert_eq!(parent_path(&10u8.into()), Some(6u8.into()));
        assert_eq!(parent_path(&14u8.into()), Some(6u8.into()));
        assert_eq!(parent_path(&9u8.into()), Some(5u8.into()));
        assert_eq!(parent_path(&13u8.into()), Some(5u8.into()));
        assert_eq!(parent_path(&11u8.into()), Some(7u8.into()));
        assert_eq!(parent_path(&15u8.into()), Some(7u8.into()));

        for path in 1..u32::from(u16::MAX) {
            let path = BigUint::from(path);
            assert_eq!(parent_path(&first_path(&path)), Some(path.clone()));
            assert_eq!(parent_path(&rest_path(&path)), Some(path));
        }
    }

    #[test]
    fn test_is_below() {
        let below = |path: u32, ancestor: u32| is_below(&path.into(), &ancestor.into());

        assert!(below(2, 1));
        assert!(below(3, 1));
        assert!(below(4, 2));
        assert!(below(6, 2));
        assert!(below(10, 2));
        assert!(below(5, 3));
        assert!(below(13, 3));
        assert!(!below(1, 1));
        assert!(!below(2, 2));
        assert!(!below(3, 2));
        assert!(!below(5, 2));
        assert!(!below(1, 2));
        assert!(!below(2, 0));
        assert!(!below(0, 1));

        for path in 2..u32::from(u16::MAX) {
            let path = BigUint::from(path);
            let parent = parent_path(&path).unwrap();
            assert!(is_below(&path, &parent));
            assert!(is_below(&first_path(&path), &parent));
            assert!(!is_below(&parent, &path));
        }
    }

    #[test]
    fn test_arbitrarily_large_path() {
        let mut path = BigUint::from(1u8);

        for _ in 0..100 {
            path = rest_path(&path);
        }

        assert_eq!(path.bits(), 101);
        assert_eq!(path_to_atom(&path).len(), 13);

        for _ in 0..100 {
            path = parent_path(&path).unwrap();
        }

        assert_eq!(path, BigUint::from(1u8));
    }
}
