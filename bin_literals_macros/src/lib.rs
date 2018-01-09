extern crate bin;
extern crate proc_macro;
#[macro_use]
extern crate procedural_masquerade;

use bin::traits::parse_rust_bin_lit;

define_proc_macros! {
    pub fn internal_bin_literals_macros(input: &str) -> String {
        let mut trimmed_input = input;
        let is_negative = input.starts_with("-");
        if is_negative {
            trimmed_input = input[1..].trim_left();
        }
        match parse_rust_bin_lit(trimmed_input, is_negative) {
            Ok(value) => format!("const VALUE: &'static [u8] = {:?};", &value),
            Err(e) => panic!("{}", e),
        }
    }
}
