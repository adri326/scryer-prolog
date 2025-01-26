/// Matches `target_cat` with the different cases of [`NumberCategory`](crate::forms::NumberCategory),
/// converting the list of `inputs` into their corresponding representation (matching `target_cat`),
/// binding each input term in `inputs` to its corresponding term in `outputs`.
///
/// For simple operations (like addition or multiplication), it is sufficient to use [`binary_op!`],
/// which determines the target category using
/// [`lhs.category().meet(rhs.category())`](crate::forms::NumberCategory::meet).
macro_rules! convert_cat {
    ( $target_cat:expr, $inputs:tt, {
        $(
            NumberCategory :: $cat:ident
            ( $( $outputs:ident ),* $(,)? )
            => $block:block
            $(,)?
        )*
    }) => {{
        let category = $target_cat;
        match category {
            $(
                $crate::forms::NumberCategory::$cat => {
                    $crate::arithmetic::operator::macros::convert_cat!(
                        __cast_each, $cat; $inputs, ( $( $outputs ),* )
                    );
                    $block
                }
            )*
        }
    }};

    ( __cast_each, $cat:ident; (), ()) => {};
    ( __cast_each, $cat:ident;
        ( $input:expr $(, $inputs:expr)*),
        ( $output:ident $(, $outputs:ident)*)
    ) => {
        $crate::arithmetic::operator::macros::convert_cat!(__cast, $cat, $input, $output);
        $crate::arithmetic::operator::macros::convert_cat!(
            __cast_each, $cat;
            ( $( $inputs ),* ),
            ( $( $outputs ),* )
        );
    };

    ( __cast, Float, $in:expr, $out:ident) => {
        let $out = $in.to_f64();
    };
    ( __cast, Rational, $in:expr, $out:ident) => {
        let $out: Rational = $in.try_into().unwrap_or_else(|err| {
            unreachable!("Numerical upcasting failure: {err:?}");
        });
    };
    ( __cast, Integer, $in:expr, $out:ident) => {
        let $out: Integer = $in.try_into().unwrap_or_else(|err| {
            unreachable!("Numerical upcasting failure: {err:?}");
        });
    };
    ( __cast, Fixnum, $in:expr, $out:ident) => {
        let $out: i64 = $in.try_into().unwrap_or_else(|err| {
            unreachable!("Numerical upcasting failure: {err:?}");
        });
    }
}

pub(crate) use convert_cat;

/// A helper macro for binary operators that convert their operands to the
/// [smallest number representation](crate::forms::NumberCategory::meet)
/// that can encode both.
///
/// If you want more control over which category the conversion should be made,
/// consider using [`convert_cat!`].
///
/// ## Example
///
/// ```no_test
/// fn is_bigger(lhs: &Number, rhs: &Number) -> bool {
///     binary_op!((*lhs, *rhs), {
///         NumberCategory::Float(lhs_float, rhs_float) => {
///             lhs_float >= rhs_float
///         }
///         NumberCategory::Rational(lhs_rat, rhs_rat) => {
///             lhs_rat >= rhs_rat
///         }
///         // ...
///     })
/// }
/// ```
macro_rules! binary_op {
    (( $lhs:expr, $rhs:expr ), {
        $(
            NumberCategory :: $cat:ident
            ( $target_lhs:ident, $target_rhs:ident )
            => $block:block
            $(,)?
        )*
    }) => {{
        let lhs = $lhs;
        let rhs = $rhs;
        $crate::arithmetic::operator::macros::convert_cat!(
            lhs.category().meet(rhs.category()),
            ( lhs, rhs ),
            {
                $(
                    NumberCategory::$cat($target_lhs, $target_rhs) => $block
                ),*
            }
        )
    }}
}

pub(crate) use binary_op;
