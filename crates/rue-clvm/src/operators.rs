/// The atom and name of every CLVM operator that has a textual keyword.
pub const OPERATORS: &[(&[u8], &str)] = &[
    (&[0x01], "q"),
    (&[0x02], "a"),
    (&[0x03], "i"),
    (&[0x04], "c"),
    (&[0x05], "f"),
    (&[0x06], "r"),
    (&[0x07], "l"),
    (&[0x08], "x"),
    (&[0x09], "="),
    (&[0x0a], ">s"),
    (&[0x0b], "sha256"),
    (&[0x0c], "substr"),
    (&[0x0d], "strlen"),
    (&[0x0e], "concat"),
    (&[0x10], "+"),
    (&[0x11], "-"),
    (&[0x12], "*"),
    (&[0x13], "/"),
    (&[0x14], "divmod"),
    (&[0x15], ">"),
    (&[0x16], "ash"),
    (&[0x17], "lsh"),
    (&[0x18], "logand"),
    (&[0x19], "logior"),
    (&[0x1a], "logxor"),
    (&[0x1b], "lognot"),
    (&[0x1d], "point_add"),
    (&[0x1e], "pubkey_for_exp"),
    (&[0x20], "not"),
    (&[0x21], "any"),
    (&[0x22], "all"),
    (&[0x24], "softfork"),
    (&[0x30], "coinid"),
    (&[0x31], "g1_subtract"),
    (&[0x32], "g1_multiply"),
    (&[0x33], "g1_negate"),
    (&[0x34], "g2_add"),
    (&[0x35], "g2_subtract"),
    (&[0x36], "g2_multiply"),
    (&[0x37], "g2_negate"),
    (&[0x38], "g1_map"),
    (&[0x39], "g2_map"),
    (&[0x3a], "bls_pairing_identity"),
    (&[0x3b], "bls_verify"),
    (&[0x3c], "modpow"),
    (&[0x3d], "%"),
    (&[0x3e], "keccak256"),
    (&[0x13, 0xd6, 0x1f, 0x00], "secp256k1_verify"),
    (&[0x1c, 0x3a, 0x8f, 0x00], "secp256r1_verify"),
];

pub fn operator_name(atom: &[u8]) -> Option<&'static str> {
    OPERATORS
        .iter()
        .find(|(value, _)| *value == atom)
        .map(|(_, name)| *name)
}

pub fn operator_atom(name: &str) -> Option<&'static [u8]> {
    OPERATORS
        .iter()
        .find(|(_, value)| *value == name)
        .map(|(atom, _)| *atom)
}
