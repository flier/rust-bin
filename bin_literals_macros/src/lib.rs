extern crate bin;
extern crate byteorder;
extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;

use byteorder::NativeEndian;

use bin::traits::{parse_raw_hex_literal, parse_rust_bin_lit, parse_raw_base64_literal};

define_proc_macros! {
    pub fn internal_bin_literals_macros(input: &str) -> String {
        let mut trimmed_input = input;
        let is_negative = input.starts_with("-");
        if is_negative {
            trimmed_input = input[1..].trim_left();
        }
        match parse_rust_bin_lit::<NativeEndian>(trimmed_input, is_negative) {
            Ok(value) => format!("const VALUE: &'static [u8] = &{:?};", value.as_slice()),
            Err(err) => panic!("{}", err),
        }
    }

    pub fn internal_hex_literals_macros(input: &str) -> String {
        let mut value = Vec::with_capacity((input.len() + 3) / 4);

        match parse_raw_hex_literal(&mut value, input) {
            Ok(_) => format!("const VALUE: &'static [u8] = &{:?};", value.as_slice()),
            Err(err) => panic!("{}", err),
        }
    }

    pub fn internal_base64_literals_macros(input: &str) -> String {
        let mut value = Vec::with_capacity((input.len() + 3) / 4);

        match parse_raw_base64_literal(&mut value, input) {
            Ok(_) => format!("const VALUE: &'static [u8] = &{:?};", value.as_slice()),
            Err(err) => panic!("{}", err),
        }
    }
}
