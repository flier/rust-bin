use std::{isize, slice, usize, f32, i16, i32, i64, i8, u16, u32, u64, u8};

use syn;
use bytes::BufMut;
use failure::Error;
use byteorder::ByteOrder;
use extprim::u128::{BYTES as U128_BYTES, u128};
use extprim::i128::{BYTES as I128_BYTES, i128};
use quote::ToTokens;

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
/// assert_eq!(parse_rust_bin_lit::<NativeEndian>("000102030405060708090a0b0c0d0e0f", true).unwrap(),
///            &[0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]);
/// # }
/// ```
pub fn parse_rust_bin_lit<E>(code: &str, is_negative: bool) -> Result<Vec<u8>, Error>
where
    E: ByteOrder,
{
    let mut bytes = vec![];

    for input in code.split(',').map(|s| s.trim()).collect::<Vec<&str>>() {
        match syn::parse_str::<syn::Expr>(input) {
            Ok(syn::Expr::Lit(syn::ExprLit { lit, .. })) => {
                parse_lit_expr::<E>(&mut bytes, lit, is_negative)?;
            }
            Ok(expr) => panic!("unsupport expr, {:?}", expr),
            Err(err) => if code.len() % 2 == 0 && code.chars().all(|c| match c {
                '0'...'9' | 'a'...'f' | 'A'...'F' => true,
                _ => false,
            }) {
                let mut chars = code.chars();

                while let (Some(hi), Some(lo)) = (
                    chars.next().and_then(|c| c.to_digit(16)),
                    chars.next().and_then(|c| c.to_digit(16)),
                ) {
                    bytes.push(((hi << 4) + lo) as u8);
                }
            } else {
                panic!("unknown syntax, {}", err)
            },
        };
    }

    Ok(bytes)
}

fn parse_lit_expr<E>(bytes: &mut Vec<u8>, lit: syn::Lit, is_negative: bool) -> Result<(), Error>
where
    E: ByteOrder,
{
    match lit {
        syn::Lit::Str(s) => {
            bytes.put(s.value());
        }
        syn::Lit::ByteStr(s) => {
            bytes.put(s.value());
        }
        syn::Lit::Byte(b) => {
            bytes.put(b.value());
        }
        syn::Lit::Char(c) => {
            let mut buf = [0; 4];

            bytes.put(c.value().encode_utf8(&mut buf).as_bytes());
        }
        syn::Lit::Int(i) => {
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
        syn::Lit::Float(f) => {
            let v = if is_negative { -f.value() } else { f.value() };

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
        syn::Lit::Bool(b) => if b.value {
            bytes.put(1u8);
        } else {
            bytes.put(0u8);
        },
        syn::Lit::Verbatim(v) => {
            let s = v.into_tokens().to_string().replace('_', "");
            let mut v: u128 = s.split(|b| b == 'i' || b == 'u').next().unwrap().parse()?;

            if is_negative {
                v = v.wrapping_neg()
            }

            bytes.put(unsafe {
                slice::from_raw_parts(&v as *const u128 as *const u8, U128_BYTES)
            });
        }
    }

    Ok(())
}
