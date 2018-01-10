#![allow(unused_imports)]

#[macro_use]
extern crate bin_literals_macros;
#[macro_use]
extern crate procedural_masquerade;

pub use bin_literals_macros::*;

define_invoke_proc_macro!(internal_bin_literals_macros_invoke);

#[macro_export]
macro_rules! bin {
    (+ $e:tt) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_bin_literals_macros!($e)
            }
            VALUE
        }
    };
    (- $e:tt) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_bin_literals_macros!(-$e)
            }
            VALUE
        }
    };
    ( $( $e:tt ),* ) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_bin_literals_macros!( $( $e ),* )
            }
            VALUE
        }
    };
    (hex! $e:tt) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_hex_literals_macros!($e)
            }
            VALUE
        }
    };
    ($e:tt ==) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_base64_literals_macros!( $e )
            }
            VALUE
        }
    };
    ( $e:tt =) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_base64_literals_macros!( $e )
            }
            VALUE
        }
    };
    (base64! $e:tt ==) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_base64_literals_macros!( $e )
            }
            VALUE
        }
    };
    (base64! $e:tt =) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_base64_literals_macros!( $e )
            }
            VALUE
        }
    };
    (base64! $e:tt) => {
        {
            internal_bin_literals_macros_invoke! {
                internal_base64_literals_macros!( $e )
            }
            VALUE
        }
    };
}
