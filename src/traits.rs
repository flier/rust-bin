use std::{isize, slice, usize, f32, i16, i32, i64, i8, u16, u32, u64, u8};

use syn;
use bytes::{BufMut, Bytes, BytesMut};
use failure::Error;
use byteorder::NativeEndian;
use extprim::u128::{BYTES as U128_BYTES, u128};
use extprim::i128::{BYTES as I128_BYTES, i128};
use quote::ToTokens;

pub fn parse_rust_bin_lit(code: &str, is_negative: bool) -> Result<Bytes, Error> {
    let mut bytes = BytesMut::new();

    match syn::parse_str::<syn::Expr>(code)? {
        syn::Expr::Lit(syn::ExprLit { lit, .. }) => {
            parse_lit_expr(&mut bytes, lit, is_negative)?;
        }
        expr => panic!("unsupport expr, {:?}", expr),
    };

    Ok(bytes.freeze())
}

fn parse_lit_expr(bytes: &mut BytesMut, lit: syn::Lit, is_negative: bool) -> Result<(), Error> {
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
                    bytes.put_i16::<NativeEndian>(v as i16);
                }
                syn::IntSuffix::I32 => {
                    bytes.put_i32::<NativeEndian>(v as i32);
                }
                syn::IntSuffix::I64 => {
                    bytes.put_i64::<NativeEndian>(v as i64);
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
                    bytes.put_i64::<NativeEndian>(v as i64);
                } else {
                    bytes.put_i32::<NativeEndian>(v as i32);
                },
                syn::IntSuffix::U8 => {
                    bytes.put_u8(v as u8);
                }
                syn::IntSuffix::U16 => {
                    bytes.put_u16::<NativeEndian>(v as u16);
                }
                syn::IntSuffix::U32 => {
                    bytes.put_u32::<NativeEndian>(v as u32);
                }
                syn::IntSuffix::U64 => {
                    bytes.put_u64::<NativeEndian>(v);
                }
                syn::IntSuffix::U128 => {
                    let s = i.into_tokens().to_string();
                    let v: u128 = s.split('u').next().unwrap().parse()?;

                    bytes.put(unsafe {
                        slice::from_raw_parts(&v as *const u128 as *const u8, U128_BYTES)
                    });
                }
                syn::IntSuffix::Usize => if usize::MAX as u64 == u64::MAX {
                    bytes.put_u64::<NativeEndian>(v as u64);
                } else {
                    bytes.put_u32::<NativeEndian>(v as u32);
                },
                syn::IntSuffix::None => {
                    if is_negative {
                        let v = v as i64;

                        if i8::MIN as i64 <= v && v <= i8::MAX as i64 {
                            bytes.put_i8(v as i8)
                        } else if i16::MIN as i64 <= v && v <= i16::MAX as i64 {
                            bytes.put_i16::<NativeEndian>(v as i16)
                        } else if i32::MIN as i64 <= v && v <= i32::MAX as i64 {
                            bytes.put_i32::<NativeEndian>(v as i32)
                        } else {
                            bytes.put_i64::<NativeEndian>(v);
                        }
                    } else {
                        if v <= u8::MAX as u64 {
                            bytes.put_u8(v as u8);
                        } else if v <= u16::MAX as u64 {
                            bytes.put_u16::<NativeEndian>(v as u16);
                        } else if v <= u32::MAX as u64 {
                            bytes.put_u32::<NativeEndian>(v as u32);
                        } else {
                            bytes.put_u64::<NativeEndian>(v);
                        }
                    }
                }
            }
        }
        syn::Lit::Float(f) => {
            let v = if is_negative {
                -f.value()
            } else {
                f.value()
            };

            match f.suffix() {
                syn::FloatSuffix::F32 => bytes.put_f32::<NativeEndian>(v as f32),
                syn::FloatSuffix::F64 => bytes.put_f64::<NativeEndian>(v),
                syn::FloatSuffix::None => {
                    if v > f32::MAX as f64 {
                        bytes.put_f64::<NativeEndian>(v)
                    } else {
                        bytes.put_f32::<NativeEndian>(v as f32)
                    }
                }
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

            bytes.put(unsafe { slice::from_raw_parts(&v as *const u128 as *const u8, U128_BYTES) });
        }
    }

    Ok(())
}
