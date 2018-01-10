use std::slice;
use std::iter;
use std::{isize, usize, f32, i16, i32, i64, i8, u16, u32, u64, u8};

use syn;
use bytes::BufMut;
use failure::Error;
use byteorder::{BigEndian, ByteOrder};
use extprim::u128::{BYTES as U128_BYTES, u128};
use extprim::i128::{BYTES as I128_BYTES, i128};
use quote::ToTokens;
use num::FromPrimitive;

/// Parses a Rust literal into an actual binary type.
///
/// If `is_negative` is true, a negative sign will be added to the number before the conversion.
///
/// # Examples
///
/// ```rust
/// # extern crate byteorder;
/// # extern crate bin;
/// #
/// # use byteorder::NativeEndian;
/// #
/// # use bin::traits::parse_rust_bin_lit;
/// #
/// #
/// # fn main() {
/// assert_eq!(parse_rust_bin_lit::<NativeEndian>("0, 1", false).unwrap(), &[0, 1]);
/// assert_eq!(parse_rust_bin_lit::<NativeEndian>("\"你好\"", false).unwrap(),
///            &[228, 189, 160, 229, 165, 189]);
/// assert_eq!(parse_rust_bin_lit::<NativeEndian>("0b111", true).unwrap(), &[0xF9]);
/// assert_eq!(parse_rust_bin_lit::<NativeEndian>("3.14", true).unwrap(),
///            &[195, 245, 72, 192]);
/// assert_eq!(parse_rust_bin_lit::<NativeEndian>("000102030405060708090a0b0c0d0e0f", false).unwrap(),
///            &[0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]);
/// # }
/// ```
pub fn parse_rust_bin_lit<E>(code: &str, is_negative: bool) -> Result<Vec<u8>, Error>
where
    E: ByteOrder,
{
    let mut bytes = vec![];

    syn::parse_str::<syn::Expr>(code)
        .or_else(|_| syn::parse_str::<syn::Expr>(format!("({})", code).as_str()))
        .map_err(|err| Error::from(err))
        .and_then(|expr| parse_expr::<E>(&mut bytes, &expr, is_negative))
        .or_else(|_| {
            for input in code.split(',').map(|s| s.trim()).filter(|s| !s.is_empty()) {
                syn::parse_str::<syn::Expr>(input)
                    .or_else(|_| syn::parse_str::<syn::Expr>(format!("({})", code).as_str()))
                    .map_err(|err| Error::from(err))
                    .and_then(|expr| parse_expr::<E>(&mut bytes, &expr, is_negative))
                    .or_else(|_| parse_raw_hex_literal(&mut bytes, input))
                    .or_else(|_| parse_raw_base64_literal(&mut bytes, input))
                    .or_else(|_| bail!("syntax error: {}", input))?;
            }

            Ok(())
        })
        .map(|_| bytes)
}

fn parse_expr<E>(bytes: &mut Vec<u8>, expr: &syn::Expr, is_negative: bool) -> Result<(), Error>
where
    E: ByteOrder,
{
    match *expr {
        syn::Expr::Lit(syn::ExprLit { ref lit, .. }) => {
            parse_lit_expr::<E>(bytes, lit, is_negative)
        }
        syn::Expr::Repeat(syn::ExprRepeat {
            ref expr, ref len, ..
        }) => parse_repeat_expr::<E>(bytes, expr, len),
        syn::Expr::Range(syn::ExprRange {
            ref from,
            limits,
            ref to,
            ..
        }) if from.is_some() && to.is_some() =>
        {
            parse_range_expr::<E>(bytes, from.as_ref().unwrap(), limits, to.as_ref().unwrap())
        }
        syn::Expr::Array(syn::ExprArray { ref elems, .. })
        | syn::Expr::Tuple(syn::ExprTuple { ref elems, .. }) => {
            for element in elems {
                parse_expr::<E>(bytes, element, false)?;
            }

            Ok(())
        }
        syn::Expr::Paren(syn::ExprParen { ref expr, .. }) => parse_expr::<E>(bytes, expr, false),
        ref expr => panic!("unsupport expr, {:?}", expr),
    }
}

fn parse_lit_expr<E>(bytes: &mut Vec<u8>, lit: &syn::Lit, is_negative: bool) -> Result<(), Error>
where
    E: ByteOrder,
{
    match *lit {
        syn::Lit::Str(ref s) => {
            bytes.put(s.value());
        }
        syn::Lit::ByteStr(ref s) => {
            bytes.put(s.value());
        }
        syn::Lit::Byte(ref b) => {
            bytes.put(b.value());
        }
        syn::Lit::Char(ref c) => {
            let mut buf = [0; 4];

            bytes.put(c.value().encode_utf8(&mut buf).as_bytes());
        }
        syn::Lit::Int(ref i) => {
            let v = if is_negative {
                i.value().wrapping_neg()
            } else {
                i.value()
            };

            match i.suffix() {
                syn::IntSuffix::I8 => {
                    bytes.put_i8(v as i8);
                }
                syn::IntSuffix::I16 => {
                    bytes.put_i16::<E>(v as i16);
                }
                syn::IntSuffix::I32 => {
                    bytes.put_i32::<E>(v as i32);
                }
                syn::IntSuffix::I64 => {
                    bytes.put_i64::<E>(v as i64);
                }
                syn::IntSuffix::I128 => {
                    let s = i.into_tokens().to_string();
                    let mut v: i128 = s.split('i').next().unwrap().parse()?;

                    if is_negative {
                        v = v.wrapping_neg()
                    }

                    bytes.put(unsafe {
                        slice::from_raw_parts(&v as *const i128 as *const u8, I128_BYTES)
                    });
                }
                syn::IntSuffix::Isize => if isize::MAX as i64 == i64::MAX {
                    bytes.put_i64::<E>(v as i64);
                } else {
                    bytes.put_i32::<E>(v as i32);
                },
                syn::IntSuffix::U8 => {
                    bytes.put_u8(v as u8);
                }
                syn::IntSuffix::U16 => {
                    bytes.put_u16::<E>(v as u16);
                }
                syn::IntSuffix::U32 => {
                    bytes.put_u32::<E>(v as u32);
                }
                syn::IntSuffix::U64 => {
                    bytes.put_u64::<E>(v);
                }
                syn::IntSuffix::U128 => {
                    let s = i.into_tokens().to_string();
                    let v: u128 = s.split('u').next().unwrap().parse()?;

                    bytes.put(unsafe {
                        slice::from_raw_parts(&v as *const u128 as *const u8, U128_BYTES)
                    });
                }
                syn::IntSuffix::Usize => if usize::MAX as u64 == u64::MAX {
                    bytes.put_u64::<E>(v as u64);
                } else {
                    bytes.put_u32::<E>(v as u32);
                },
                syn::IntSuffix::None => if is_negative {
                    let v = v as i64;

                    if i8::MIN as i64 <= v && v <= i8::MAX as i64 {
                        bytes.put_i8(v as i8)
                    } else if i16::MIN as i64 <= v && v <= i16::MAX as i64 {
                        bytes.put_i16::<E>(v as i16)
                    } else if i32::MIN as i64 <= v && v <= i32::MAX as i64 {
                        bytes.put_i32::<E>(v as i32)
                    } else {
                        bytes.put_i64::<E>(v);
                    }
                } else {
                    if v <= u8::MAX as u64 {
                        bytes.put_u8(v as u8);
                    } else if v <= u16::MAX as u64 {
                        bytes.put_u16::<E>(v as u16);
                    } else if v <= u32::MAX as u64 {
                        bytes.put_u32::<E>(v as u32);
                    } else {
                        bytes.put_u64::<E>(v);
                    }
                },
            }
        }
        syn::Lit::Float(ref f) => {
            let v = if is_negative {
                -f.value()
            } else {
                f.value()
            };

            match f.suffix() {
                syn::FloatSuffix::F32 => bytes.put_f32::<E>(v as f32),
                syn::FloatSuffix::F64 => bytes.put_f64::<E>(v),
                syn::FloatSuffix::None => if v > f32::MAX as f64 {
                    bytes.put_f64::<E>(v)
                } else {
                    bytes.put_f32::<E>(v as f32)
                },
            }
        }
        syn::Lit::Bool(ref b) => if b.value {
            bytes.put(1u8);
        } else {
            bytes.put(0u8);
        },
        syn::Lit::Verbatim(ref v) => {
            let s = v.into_tokens().to_string().replace('_', "");
            let mut v: u128 = s.split(|b| b == 'i' || b == 'u').next().unwrap().parse()?;

            if is_negative {
                v = v.wrapping_neg()
            }

            bytes.put(unsafe { slice::from_raw_parts(&v as *const u128 as *const u8, U128_BYTES) });
        }
    }

    Ok(())
}

fn parse_repeat_expr<E>(bytes: &mut Vec<u8>, expr: &syn::Expr, len: &syn::Expr) -> Result<(), Error>
where
    E: ByteOrder,
{
    let len = eval_const_expr_as_num::<usize>(&len)?;
    let mut value = vec![];

    parse_expr::<E>(&mut value, &expr, false)?;

    for _ in 0..len {
        bytes.extend_from_slice(&value);
    }

    Ok(())
}

fn parse_range_expr<E>(
    bytes: &mut Vec<u8>,
    from: &syn::Expr,
    limits: syn::RangeLimits,
    to: &syn::Expr,
) -> Result<(), Error>
where
    E: ByteOrder,
{
    let from = eval_const_expr_as_num::<u16>(from)?;
    let to = eval_const_expr_as_num::<u16>(to)?;

    match limits {
        syn::RangeLimits::HalfOpen(_) => bytes.extend((from..to).map(|b| b as u8)),
        syn::RangeLimits::Closed(_) => bytes.extend((from..to + 1).map(|b| b as u8)),
    }

    Ok(())
}

fn eval_const_expr_as_num<T>(expr: &syn::Expr) -> Result<T, Error>
where
    T: FromPrimitive,
{
    if let syn::Expr::Lit(syn::ExprLit { ref lit, .. }) = *expr {
        match *lit {
            syn::Lit::Byte(ref b) => {
                if let Some(n) = T::from_u8(b.value()) {
                    return Ok(n);
                }
            }
            syn::Lit::Int(ref i) => {
                if let Some(n) = T::from_u64(i.value()) {
                    return Ok(n);
                }
            }
            _ => {}
        }
    }

    bail!("unsupport expr, {:?}", expr)
}

pub fn parse_raw_hex_literal(bytes: &mut Vec<u8>, input: &str) -> Result<(), Error> {
    if input.len() % 2 == 0 && input.chars().all(|c| match c {
        '0'...'9' | 'a'...'f' | 'A'...'F' => true,
        _ => false,
    }) {
        let mut chars = input.chars();

        while let (Some(hi), Some(lo)) = (
            chars.next().and_then(|c| c.to_digit(16)),
            chars.next().and_then(|c| c.to_digit(16)),
        ) {
            bytes.push(((hi << 4) + lo) as u8);
        }

        Ok(())
    } else {
        bail!("illegel hex literal");
    }
}

const BASE64_CHUNK_SIZE: usize = 4;
const BASE64_INDEX_TABLE: &[u8; 64] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

pub fn parse_raw_base64_literal(bytes: &mut Vec<u8>, input: &str) -> Result<(), Error> {
    if (input.as_bytes().len() % BASE64_CHUNK_SIZE) != 1
        && input.as_bytes().iter().all(|&b| match b {
            b'A'...b'Z' | b'a'...b'z' | b'0'...b'9' | b'+' | b'/' | b'=' => true,
            _ => false,
        }) {
        let mut chunks = input.as_bytes().chunks(BASE64_CHUNK_SIZE);

        while let Some(chunk) = chunks.next() {
            let n = chunk
                .iter()
                .cloned()
                .chain(iter::repeat(b'='))
                .take(4)
                .map(|b| BASE64_INDEX_TABLE.iter().position(|&x| x == b).unwrap_or(0))
                .fold(0u64, |acc, v| acc.wrapping_shl(6) + v as u64);

            let (n, l) = match chunk.iter().cloned().take_while(|&b| b != b'=').count() {
                1 | 2 => (n.wrapping_shr(16) & 0xFF, 1),
                3 => (n.wrapping_shr(8) & 0xFFFF, 2),
                _ => (n, 3),
            };

            let mut v = vec![0; l];

            BigEndian::write_uint(&mut v, n as u64, l);

            bytes.extend(v)
        }

        Ok(())
    } else {
        bail!("illegel base64 literal");
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_raw_hex_literal() {
        let mut v = vec![];

        assert!(
            parse_raw_hex_literal(
                &mut v,
                "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
            ).is_ok()
        );
        assert_eq!(
            &v,
            &[
                0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
                0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b,
                0x1c, 0x1d, 0x1e, 0x1f,
            ]
        );

        v.clear();

        assert!(parse_raw_hex_literal(&mut v, "").is_ok());
        assert!(v.is_empty());

        assert!(parse_raw_hex_literal(&mut v, "123").is_err());
        assert!(parse_raw_hex_literal(&mut v, "123+").is_err());
    }

    #[test]
    fn test_parse_raw_base64_literal() {
        let mut v = vec![];

        assert!(
            parse_raw_base64_literal(
                &mut v,
                "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4="
            ).is_ok()
        );
        assert_eq!(
            v.as_slice(),
            &b"Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."[..]
        );

        // Decoding Base64 with padding
        //
        // When decoding Base64 text, four characters are typically converted back to three bytes.
        // The only exceptions are when padding characters exist.
        // A single '=' indicates that the four characters will decode to only two bytes,
        // while '==' indicates that the four characters will decode to only a single byte.
        //
        // For example:

        v.clear();
        assert!(parse_raw_base64_literal(&mut v, "YW55IGNhcm5hbCBwbGVhcw==").is_ok());
        assert_eq!(v.as_slice(), b"any carnal pleas");

        v.clear();
        assert!(parse_raw_base64_literal(&mut v, "YW55IGNhcm5hbCBwbGVhc3U=").is_ok());
        assert_eq!(v.as_slice(), b"any carnal pleasu");

        v.clear();
        assert!(parse_raw_base64_literal(&mut v, "YW55IGNhcm5hbCBwbGVhc3Vy").is_ok());
        assert_eq!(v.as_slice(), b"any carnal pleasur");

        // Decoding Base64 without padding
        //
        // Without padding, after normal decoding of four characters to three bytes over and over again,
        // less than four encoded characters may remain. In this situation only two or three characters shall remain.
        // A single remaining encoded character is not possible (because a single base 64 character only contains 6 bits,
        // and 8 bits are required to create a byte, so a minimum of 2 base 64 characters are required :
        // the first character contributes 6 bits, and the second character contributes its first 2 bits) .
        //
        // For example:

        v.clear();
        assert!(parse_raw_base64_literal(&mut v, "YW55IGNhcm5hbCBwbGVhcw").is_ok());
        assert_eq!(v.as_slice(), b"any carnal pleas");

        v.clear();
        assert!(parse_raw_base64_literal(&mut v, "YW55IGNhcm5hbCBwbGVhc3U").is_ok());
        assert_eq!(v.as_slice(), b"any carnal pleasu");

        v.clear();
        assert!(parse_raw_base64_literal(&mut v, "YW55IGNhcm5hbCBwbGVhc3Vy").is_ok());
        assert_eq!(v.as_slice(), b"any carnal pleasur");
    }
}
