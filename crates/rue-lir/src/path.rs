pub fn path_to_atom(path: u32) -> Vec<u8> {
    let bytes = path.to_be_bytes();
    let mut slice = bytes.as_slice();
    while (!slice.is_empty()) && (slice[0] == 0) {
        slice = &slice[1..];
    }
    slice.to_vec()
}

pub fn first_path(path: u32) -> u32 {
    add_path(path, 2)
}

pub fn rest_path(path: u32) -> u32 {
    add_path(path, 3)
}

fn add_path(a: u32, mut b: u32) -> u32 {
    let mut mask = 1;
    let mut temp_path = a;
    while temp_path > 1 {
        b <<= 1;
        mask <<= 1;
        temp_path >>= 1;
    }

    mask -= 1;
    b | (a & mask)
}

pub fn parent_path(path: u32) -> u32 {
    let result_depth = 31 - path.leading_zeros();
    let original_depth = result_depth - 1;
    let mask = (1 << original_depth) - 1;
    let lower_bits = path & mask;
    (1 << original_depth) | lower_bits
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_to_atom() {
        assert_eq!(path_to_atom(1), [0x01]);
        assert_eq!(path_to_atom(3), [0x03]);
        assert_eq!(path_to_atom(300), [0x01, 0x2C]);
    }

    #[test]
    fn test_first_path() {
        assert_eq!(first_path(1), 2);
        assert_eq!(first_path(2), 4);
        assert_eq!(first_path(5), 9);
    }

    #[test]
    fn test_rest_path() {
        assert_eq!(rest_path(1), 3);
        assert_eq!(rest_path(2), 6);
        assert_eq!(rest_path(5), 13);
    }

    #[test]
    fn test_parent_path() {
        assert_eq!(parent_path(2), 1);
        assert_eq!(parent_path(3), 1);
        assert_eq!(parent_path(4), 2);
        assert_eq!(parent_path(6), 2);
        assert_eq!(parent_path(5), 3);
        assert_eq!(parent_path(7), 3);
        assert_eq!(parent_path(8), 4);
        assert_eq!(parent_path(12), 4);
        assert_eq!(parent_path(10), 6);
        assert_eq!(parent_path(14), 6);
        assert_eq!(parent_path(9), 5);
        assert_eq!(parent_path(13), 5);
        assert_eq!(parent_path(11), 7);
        assert_eq!(parent_path(15), 7);

        for path in 1..u32::from(u16::MAX) {
            assert_eq!(parent_path(first_path(path)), path);
            assert_eq!(parent_path(rest_path(path)), path);
        }
    }
}
