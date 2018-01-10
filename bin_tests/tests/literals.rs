extern crate bin;
#[macro_use]
extern crate bin_literals;
extern crate byteorder;

use byteorder::{ByteOrder, NativeEndian};

const STR: &[u8] = bin!["hello"];
const UNICODE: &[u8] = bin!["你好"];
const BYTES: &[u8] = bin![b"hello"];
const BYTE: &[u8] = bin![b'a'];
const CHAR: &[u8] = bin!['a'];
const WCHAR: &[u8] = bin!['√'];

const I8: &[u8] = bin![-128i8];
const I16: &[u8] = bin![-32768i16];
const I32: &[u8] = bin![-2147483648i32];
const I64: &[u8] = bin![-9223372036854775808i64];
const I128: &[u8] = bin![-9223372036854775808i128];
const I128_MIN: &[u8] = bin![-170_141_183_460_469_231_731_687_303_715_884_105_728i128];
const I128_MAX: &[u8] = bin![-170_141_183_460_469_231_731_687_303_715_884_105_727i128];
const ISIZE: &[u8] = bin![-9223372036854775808isize];

const U8: &[u8] = bin![128u8];
const U16: &[u8] = bin![32768u16];
const U32: &[u8] = bin![2147483648u32];
const U64: &[u8] = bin![9223372036854775808u64];
const U128: &[u8] = bin![9223372036854775808u128];
const U128_MAX: &[u8] = bin![340_282_366_920_938_463_463_374_607_431_768_211_455u128];
const USIZE: &[u8] = bin![9223372036854775808u64];

const F32: &[u8] = bin![3.14f32];
const F64: &[u8] = bin![3.14f64];
const F32_NEG: &[u8] = bin![-3.14f32];
const F64_NEG: &[u8] = bin![-3.14f64];
const FLOAT: &[u8] = bin![3.14];
const FLOAT64: &[u8] = bin![3.14e+39];

const TRUE: &[u8] = bin![true];
const FALSE: &[u8] = bin![false];

const SLICE: &[u8] = bin![0x00, 0x01, 0x02, 0x03];
const MIXED_SLICE: &[u8] = bin![
    "hello",
    "你好",
    b"hello",
    b'a',
    'a',
    '√',
    128,
    0x0123456789ABCDEF,
    340_282_366_920_938_463_463_374_607_431_768_211_455u128,
    3.14,
    true,
    000102030405060708090a0b0c0d0e0f
];

const HEX: &[u8] = bin![
    000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f
];
const HEX_WITH_TAG: &[u8] = bin![
    hex!000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f
];

const BASE64: &[u8] = bin![YW55IGNhcm5hbCBwbGVhc3VyZQ==];
const BASE64_WITH_TAG: &[u8] = bin![base64!YW55IGNhcm5hbCBwbGVhc3VyZQ==];

const REPEAT: &[u8] = bin![[0xFF; 8], [0; 4]];
const RANGE: &[u8] = bin![(b'a'..=b'z'), (b'0'..=b'9')];
const ARRAY: &[u8] = bin![[0, 1, 2, 3], [8000, 8080]];
const TUPLE: &[u8] = bin![(0, 1, 2, 3), (8000, 8080)];

const OPS: &[u8] = bin![
    (
        !0x80,
        1 + 2,
        2 - 1,
        2 * 3,
        4 / 2,
        7 % 3,
        33 ^ 0x80,
        33 & 0xF0,
        33 | 0x80,
        1 << 4,
        0x80 >> 3,
    )
];

#[test]
fn test_literal() {
    assert_eq!(STR, b"hello");
    assert_eq!(UNICODE, &[228, 189, 160, 229, 165, 189]);
    assert_eq!(BYTES, b"hello");
    assert_eq!(BYTE, b"a");
    assert_eq!(CHAR, b"a");
    assert_eq!(WCHAR, &[226, 136, 154]);

    assert_eq!(I8, &[0x80]);
    assert_eq!(I16, &[0, 0x80]);
    assert_eq!(I32, &[0, 0, 0, 0x80]);
    assert_eq!(I64, &[0, 0, 0, 0, 0, 0, 0, 0x80]);
    assert_eq!(
        I128,
        &[
            0, 0, 0, 0, 0, 0, 0, 0x80, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
        ]
    );
    assert_eq!(
        I128_MIN,
        &[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80]
    );
    assert_eq!(
        I128_MAX,
        &[1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x80]
    );
    assert_eq!(ISIZE, &[0, 0, 0, 0, 0, 0, 0, 0x80]);

    assert_eq!(bin![-128], &[0x80]);
    assert_eq!(bin![-32768], &[0, 0x80]);
    assert_eq!(bin![-2147483648], &[0, 0, 0, 0x80]);
    assert_eq!(bin![-9223372036854775808], &[0, 0, 0, 0, 0, 0, 0, 0x80]);

    assert_eq!(bin![128], &[0x80]);
    assert_eq!(bin![32768], &[0, 0x80]);
    assert_eq!(bin![2147483648], &[0, 0, 0, 0x80]);
    assert_eq!(bin![9223372036854775808], &[0, 0, 0, 0, 0, 0, 0, 0x80]);

    assert_eq!(
        bin![340_282_366_920_938_463_463_374_607_431_768_211_455],
        &[
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF,
        ]
    );

    assert_eq!(U8, &[0x80]);
    assert_eq!(U16, &[0, 0x80]);
    assert_eq!(U32, &[0, 0, 0, 0x80]);
    assert_eq!(U64, &[0, 0, 0, 0, 0, 0, 0, 0x80]);
    assert_eq!(U128, &[0, 0, 0, 0, 0, 0, 0, 0x80, 0, 0, 0, 0, 0, 0, 0, 0]);
    assert_eq!(
        U128_MAX,
        &[
            0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
            0xFF, 0xFF,
        ]
    );
    assert_eq!(USIZE, &[0, 0, 0, 0, 0, 0, 0, 0x80]);

    assert_eq!(bin![0xFF], &[0xFF]);
    assert_eq!(bin![0xFFFF], &[0xFF, 0xFF]);
    assert_eq!(bin![0xFFFFFFFF], &[0xFF, 0xFF, 0xFF, 0xFF]);
    assert_eq!(
        bin![0xFFFFFFFFFFFFFFFF],
        &[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]
    );
    assert_eq!(
        bin![0x0123456789ABCDEF],
        &[0xEF, 0xCD, 0xAB, 0x89, 0x67, 0x45, 0x23, 0x01]
    );

    assert_eq!(NativeEndian::read_u32(F32), 3.14f32.to_bits());
    assert_eq!(NativeEndian::read_u64(F64), 3.14f64.to_bits());
    assert_eq!(NativeEndian::read_u32(F32_NEG), (-3.14f32).to_bits());
    assert_eq!(NativeEndian::read_u64(F64_NEG), (-3.14f64).to_bits());
    assert_eq!(NativeEndian::read_u32(FLOAT), 3.14f32.to_bits());
    assert_eq!(NativeEndian::read_u64(FLOAT64), 3.14e+39f64.to_bits());

    assert_eq!(TRUE, &[1]);
    assert_eq!(FALSE, &[0]);
}

#[test]
fn test_slice() {
    assert_eq!(SLICE, &[0x00, 0x01, 0x02, 0x03]);
    assert_eq!(
        MIXED_SLICE,
        &[
            // "hello"
            b'h',
            b'e',
            b'l',
            b'l',
            b'o',
            // "您好"
            228,
            189,
            160,
            229,
            165,
            189,
            // b"hello"
            b'h',
            b'e',
            b'l',
            b'l',
            b'o',
            // b'a'
            b'a',
            // 'a'
            b'a',
            // '√'
            226,
            136,
            154,
            // 128
            128,
            // 0x0123456789ABCDEF,
            0xEF,
            0xCD,
            0xAB,
            0x89,
            0x67,
            0x45,
            0x23,
            0x01,
            // 340_282_366_920_938_463_463_374_607_431_768_211_455u128
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            0xFF,
            // 3.14
            195,
            245,
            72,
            64,
            // true
            1,
            // 000102030405060708090a0b0c0d0e0f
            0x00,
            0x01,
            0x02,
            0x03,
            0x04,
            0x05,
            0x06,
            0x07,
            0x08,
            0x09,
            0x0a,
            0x0b,
            0x0c,
            0x0d,
            0x0e,
            0x0f
        ][..]
    );
}

#[test]
fn test_hex() {
    assert_eq!(
        HEX,
        &[
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
            0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b,
            0x1c, 0x1d, 0x1e, 0x1f,
        ]
    );

    assert_eq!(
        HEX_WITH_TAG,
        &[
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
            0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b,
            0x1c, 0x1d, 0x1e, 0x1f,
        ]
    );
}

#[test]
fn test_base64() {
    assert_eq!(BASE64, b"any carnal pleasure");
    assert_eq!(BASE64_WITH_TAG, b"any carnal pleasure");
}

#[test]
fn test_repeat() {
    assert_eq!(
        REPEAT,
        &[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0, 0, 0, 0]
    );
}

#[test]
fn test_range() {
    assert_eq!(RANGE, &b"abcdefghijklmnopqrstuvwxyz0123456789"[..]);
}

#[test]
fn test_array() {
    assert_eq!(ARRAY, &[0, 1, 2, 3, 0x40, 0x1F, 0x90, 0x1F]);
}

#[test]
fn test_tuple() {
    assert_eq!(TUPLE, &[0, 1, 2, 3, 0x40, 0x1F, 0x90, 0x1F]);
}

#[test]
fn test_ops() {
    assert_eq!(
        OPS,
        &[
            !0x80,
            1 + 2,
            2 - 1,
            2 * 3,
            4 / 2,
            7 % 3,
            33 ^ 0x80,
            33 & 0xF0,
            33 | 0x80,
            1 << 4,
            0x80 >> 3
        ]
    );
}
