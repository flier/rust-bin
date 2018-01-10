extern crate bin;
extern crate byteorder;
extern crate proc_macro2;
extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;
#[macro_use]
extern crate quote;

use byteorder::NativeEndian;

use bin::traits::{parse_raw_hex_literal, parse_rust_bin_lit, parse_raw_base64_literal};

define_proc_macros! {
    pub fn internal_bin_literals_macros(input: &str) -> String {
        let mut trimmed_input = input;
        let is_negative = input.starts_with("-");
        if is_negative {
            trimmed_input = input[1..].trim_left();
        }

        let output = match parse_rust_bin_lit::<NativeEndian>(trimmed_input, is_negative) {
            Ok(value) => {
                let mut tokens = quote::Tokens::new();

                tokens.append_separated(value, proc_macro2::Term::intern(","));

                quote! {
                    const VALUE: &'static [u8] = &[ #tokens ];
                }
            },
            Err(err) => panic!("{}", err),
        };

        output.to_string()
    }

    pub fn internal_hex_literals_macros(input: &str) -> String {
        match parse_raw_hex_literal(input) {
            Ok(bytes) => format!("const VALUE: &'static [u8] = &{:?};", bytes.as_slice()),
            Err(err) => panic!("{}", err),
        }
    }

    pub fn internal_base64_literals_macros(input: &str) -> String {
        match parse_raw_base64_literal(input) {
            Ok(bytes) => format!("const VALUE: &'static [u8] = &{:?};", bytes.as_slice()),
            Err(err) => panic!("{}", err),
        }
    }
}
