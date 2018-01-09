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
        &[0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,]
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
