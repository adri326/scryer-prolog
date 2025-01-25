use super::*;
use crate::machine::machine_state::MachineState;
use std::borrow::Cow;
use std::convert::Infallible;

#[derive(Clone)]
pub(crate) struct NativeArithmeticOperator {
    get_instr: fn(&[ArithmeticTerm], usize) -> Instruction,
    run: fn(&[Number], &mut Arena) -> Result<Number, MachineStubGen>,
}

impl NativeArithmeticOperator {
    pub(super) fn get_instr(
        &self,
        arith_terms: &[ArithmeticTerm],
        output_index: usize,
    ) -> Instruction {
        (self.get_instr)(arith_terms, output_index)
    }

    pub(super) fn run(
        &self,
        values: &[Number],
        arena: &mut Arena,
    ) -> Result<Number, MachineStubGen> {
        (self.run)(values, arena)
    }
}

impl std::fmt::Debug for NativeArithmeticOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeArithmeticOperator").finish()
    }
}

trait NativeOpError {
    fn as_machine_stub_gen(self, name: Atom, arity: usize) -> MachineStubGen;
}

impl NativeOpError for MachineStubGen {
    fn as_machine_stub_gen(self, _name: Atom, _arity: usize) -> MachineStubGen {
        self
    }
}

impl NativeOpError for Infallible {
    fn as_machine_stub_gen(self, _name: Atom, _arity: usize) -> MachineStubGen {
        match self {}
    }
}

impl NativeOpError for EvalError {
    fn as_machine_stub_gen(self, name: Atom, arity: usize) -> MachineStubGen {
        Box::new(move |machine_st| {
            let eval_error = machine_st.evaluation_error(self);
            let stub = crate::machine::machine_errors::functor_stub(name, arity);

            machine_st.error_form(eval_error, stub)
        })
    }
}

/// A helper function for unwrapping a [`Result<T, Infallible>`].
///
/// Because of the way [`native_ops!()`](native_ops) is implemented,
/// infallible operators can't just return a [`Number`]: instead,
/// they need to return a `Result<Number, Infallible>`.
///
/// This function makes it easier to use their return value within other
/// operator implementations.
fn unwrap_infallible<T>(result: Result<T, Infallible>) -> T {
    match result {
        Ok(res) => res,
    }
}

macro_rules! native_ops {
    (
        dispatch: $dispatch_vis:vis $dispatch_name:ident,
        array: $array_vis:vis $array_name:ident;
        $($root:ident $(:: $path:ident)* ($($param:ident),*) => ( $name:tt, $run:path )),*
        $(,)?
    ) => {
        impl Instruction {
            pub(crate) const fn is_native_op(&self) -> bool {
                #[allow(unused_variables)]
                match self {
                    $(
                        $root $(:: $path)* ($($param),*) => true,
                    )*
                    _ => false,
                }
            }
        }

        #[inline]
        $dispatch_vis fn $dispatch_name(machine_st: &mut MachineState, instr: &Instruction) -> Result<(), MachineStub> {
            match instr {
                $(
                    $root $(:: $path)* ($($param),*) => {
                        native_ops!(__match_body; ($($param),*), machine_st, $name, $run)
                    }
                )*
                _ => {
                    unreachable!("{} called with non-native operator", stringify!($dispatch_name));
                }
            }
        }

        $array_vis const $array_name: [ArithmeticOperator; native_ops!(__count; $($root),*)] = [$(
            native_ops!(__operator; $root $(:: $path)* ($($param),*) => ( $name, $run ))
        ),*];
    };

    ( __match_body; ($($param:ident),*), $machine_st:expr, $name:tt, $run:path ) => {{
        let input = native_ops!(__get_input; $machine_st, ($($param),*));
        let out = native_ops!(__get_output; ($($param),*));

        match native_ops!(
            __repeat_index;
            $run,
            input,
            ($($param),*),
            &mut $machine_st.arena
        ) {
            Ok(res) => {
                $machine_st.interms[out - 1] = res;
            }
            Err(err) => {
                return Err(NativeOpError::as_machine_stub_gen(
                    err,
                    atom!($name),
                    native_ops!(__get_arity; ($($param),*))
                )($machine_st));
            }
        }

        $machine_st.p += 1;

        Ok(())
    }};

    ( __operator; $root:ident $(:: $path:ident)* ($($param:ident),*) => ( $name:tt, $run:path ) ) => {{
        fn get_instr(terms: &[ArithmeticTerm], out: usize) -> Instruction {
            assert_eq!(terms.len(), native_ops!(__get_arity; ($($param),*)));
            native_ops!(__repeat_index; $root $(:: $path)*, terms, ($($param),*), out)
        }

        #[allow(unused_variables)]
        fn run(nums: &[Number], arena: &mut Arena) -> Result<Number, MachineStubGen> {
            match native_ops!(__repeat_index; $run, nums, ($($param),*), arena) {
                Ok(res) => Ok(res),
                Err(err) => Err(NativeOpError::as_machine_stub_gen(
                    err,
                    atom!($name),
                    native_ops!(__get_arity; ($($param),*))
                )),
            }
        }

        ArithmeticOperator {
            name: Cow::Borrowed($name),
            arity: native_ops!(__get_arity; ($($param),*)),
            mode: ArithmeticMode::Native(NativeArithmeticOperator {
                get_instr,
                run,
            })
        }
    }};

    (__get_arity; ($x0:ident, $out:ident)) => {
        1
    };
    (__get_arity; ($x0:ident, $x1:ident, $out:ident)) => {
        2
    };
    (__get_arity; ($x0:ident, $x1:ident, $x2:ident, $out:ident)) => {
        3
    };
    (__get_arity; ($x0:ident, $x1:ident, $x2:ident, $x3:ident, $out:ident)) => {
        4
    };

    (__get_input; $machine_st:expr, ($x0:ident, $out:ident)) => {
        [
            $machine_st.get_number(&$x0)?,
        ]
    };
    (__get_input; $machine_st:expr, ($x0:ident, $x1:ident, $out:ident)) => {
        [
            $machine_st.get_number(&$x0)?,
            $machine_st.get_number(&$x1)?,
        ]
    };
    (__get_input; $machine_st:expr, ($x0:ident, $x1:ident, $x2:ident, $out:ident)) => {
        [
            $machine_st.get_number(&$x0)?,
            $machine_st.get_number(&$x1)?,
            $machine_st.get_number(&$x2)?,
        ]
    };
    (__get_input; $machine_st:expr, ($x0:ident, $x1:ident, $x2:ident, $x3:ident, $out:ident)) => {
        [
            $machine_st.get_number(&$x0)?,
            $machine_st.get_number(&$x1)?,
            $machine_st.get_number(&$x2)?,
            $machine_st.get_number(&$x3)?,
        ]
    };

    (__get_output; ($out:ident)) => {
        $out
    };
    (__get_output; ($head:ident, $($xs:ident),+)) => {
        native_ops!(__get_output; ($($xs),+))
    };

    ( __repeat_index; $fn:path, $array:expr, ($x0:ident, $out:ident) $(,$rest:expr)* ) => {
        $fn($array[0] $(, $rest)*)
    };
    ( __repeat_index; $fn:path, $array:expr, ($x0:ident, $x1:ident, $out:ident) $(,$rest:expr)* ) => {
        $fn($array[0], $array[1] $(, $rest)*)
    };
    ( __repeat_index; $fn:path, $array:expr, ($x0:ident, $x1:ident, $x2:ident, $out:ident) $(,$rest:expr)* ) => {
        $fn($array[0], $array[1], $array[2] $(, $rest)*)
    };
    ( __repeat_index; $fn:path, $array:expr, ($x0:ident, $x1:ident, $x2:ident, $x3:ident, $out:ident) $(,$rest:expr)* ) => {
        $fn($array[0], $array[1], $array[2], $array[3] $(, $rest)*)
    };

    ( __count; ) => {
        0
    };
    ( __count; $head:ident $(, $rest:ident)* ) => {
        1 + native_ops!(__count; $( $rest ),*)
    };
}

native_ops!(
    dispatch: pub(crate) dispatch_native_op,
    array: pub(super) NATIVE_OPS;

    Instruction::Neg(a, t) => ("-", unary::neg),
    Instruction::Plus(a, t) => ("+", unary::plus),
    Instruction::Float(a, t) => ("float", unary::float),
    Instruction::Abs(a, t) => ("abs", unary::abs),
    Instruction::Floor(a, t) => ("floor", unary::floor),
    Instruction::Truncate(a, t) => ("truncate", unary::truncate),
    Instruction::Ceiling(a, t) => ("ceiling", unary::ceiling),
    Instruction::Round(a, t) => ("round", unary::round),
    Instruction::Cos(a, t) => ("cos", unary::cos),
    Instruction::Sin(a, t) => ("sin", unary::sin),
    Instruction::Tan(a, t) => ("tan", unary::tan),
    Instruction::ACos(a, t) => ("acos", unary::acos),
    Instruction::ASin(a, t) => ("asin", unary::asin),
    Instruction::ATan(a, t) => ("atan", unary::atan),
    Instruction::Log(a, t) => ("log", unary::log),
    Instruction::Exp(a, t) => ("exp", unary::exp),
    Instruction::Sqrt(a, t) => ("sqrt", unary::sqrt),
    Instruction::FloatFractionalPart(a, t) => ("float_fractional_part", unary::float_fractional_part),
    Instruction::FloatIntegerPart(a, t) => ("float_integer_part", unary::float_integer_part),
    Instruction::BitwiseComplement(a, t) => ("\\", unary::bitwise_complement),
    Instruction::Sign(a, t) => ("sign", unary::sign),

    Instruction::Add(a, b, t) => ("+", binary::add),
    Instruction::Sub(a, b, t) => ("-", binary::sub),
    Instruction::Mul(a, b, t) => ("*", binary::mul),
    Instruction::Div(a, b, t) => ("/", binary::div),
);

mod unary {
    use super::*;

    #[inline(always)]
    fn float_template<F: Fn(f64) -> f64>(num: Number, cb: F) -> Result<Number, EvalError> {
        let float = result_f(&num)?;
        let res = classify_float(cb(float))?;
        Ok(Number::Float(OrderedFloat(res)))
    }

    #[inline(always)]
    pub(crate) fn plus(num: Number, _arena: &mut Arena) -> Result<Number, Infallible> {
        Ok(num)
    }

    #[inline]
    pub(crate) fn float(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |x| x)
    }

    #[inline]
    pub(crate) fn abs(n: Number, arena: &mut Arena) -> Result<Number, Infallible> {
        Ok(match n {
            Number::Fixnum(n) => {
                if let Some(n) = n.get_num().checked_abs() {
                    fixnum!(Number, n, arena)
                } else {
                    let arena_int = Integer::from(n.get_num());
                    Number::arena_from(arena_int.abs(), arena)
                }
            }
            Number::Integer(n) => {
                let n_clone: Integer = (*n).clone();
                Number::arena_from(Integer::from(n_clone.abs()), arena)
            }
            Number::Float(f) => Number::Float(f.abs()),
            Number::Rational(r) => {
                let r_clone: Rational = (*r).clone();
                Number::arena_from(Rational::from(r_clone.abs()), arena)
            }
        })
    }

    #[inline]
    pub(crate) fn neg(n: Number, arena: &mut Arena) -> Result<Number, Infallible> {
        Ok(match n {
            Number::Fixnum(n) => {
                if let Some(n) = n.get_num().checked_neg() {
                    fixnum!(Number, n, arena)
                } else {
                    Number::arena_from(-Integer::from(n.get_num()), arena)
                }
            }
            Number::Integer(n) => {
                let n_clone: Integer = (*n).clone();
                Number::arena_from(-Integer::from(n_clone), arena)
            }
            Number::Float(OrderedFloat(f)) => Number::Float(OrderedFloat(-f)),
            Number::Rational(r) => {
                let r_clone: Rational = (*r).clone();
                Number::arena_from(-Rational::from(r_clone), arena)
            }
        })
    }

    #[inline(always)]
    pub(crate) fn floor(num: Number, arena: &mut Arena) -> Result<Number, EvalError> {
        rnd_i(&num, arena)
    }

    #[inline]
    pub(crate) fn truncate(n: Number, arena: &mut Arena) -> Result<Number, EvalError> {
        if n.is_negative() {
            let n = unwrap_infallible(abs(n, arena));
            let n = floor(n, arena)?;

            Ok(unwrap_infallible(neg(n, arena)))
        } else {
            floor(n, arena)
        }
    }

    #[inline]
    pub(crate) fn ceiling(n1: Number, arena: &mut Arena) -> Result<Number, EvalError> {
        let n1 = unwrap_infallible(neg(n1, arena));
        let n1 = floor(n1, arena)?;

        Ok(unwrap_infallible(neg(n1, arena)))
    }

    pub(crate) fn round(num: Number, arena: &mut Arena) -> Result<Number, EvalError> {
        let res = match num {
            Number::Fixnum(_) | Number::Integer(_) => num,
            Number::Rational(rat) => Number::arena_from(rat.round(), arena),
            Number::Float(f) => Number::Float(OrderedFloat((*f).round())),
        };

        rnd_i(&res, arena)
    }

    #[inline]
    pub(crate) fn sin(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.sin())
    }

    #[inline]
    pub(crate) fn cos(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.cos())
    }

    #[inline]
    pub(crate) fn tan(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.tan())
    }

    #[inline]
    pub(crate) fn log(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        if num.is_negative() || num.is_zero() {
            return Err(EvalError::Undefined);
        }

        float_template(num, |f| f.ln())
    }

    #[inline]
    pub(crate) fn exp(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.exp())
    }

    #[inline]
    pub(crate) fn asin(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.asin())
    }

    #[inline]
    pub(crate) fn acos(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.acos())
    }

    #[inline]
    pub(crate) fn atan(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.atan())
    }

    #[inline]
    pub(crate) fn float_fractional_part(
        num: Number,
        _arena: &mut Arena,
    ) -> Result<Number, EvalError> {
        float_template(num, |f| f.fract())
    }

    #[inline]
    pub(crate) fn float_integer_part(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        float_template(num, |f| f.trunc())
    }

    #[inline]
    pub(crate) fn sqrt(num: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        if num.is_negative() {
            return Err(EvalError::Undefined);
        }

        float_template(num, |f| f.sqrt())
    }

    #[inline]
    pub(crate) fn bitwise_complement(
        num: Number,
        arena: &mut Arena,
    ) -> Result<Number, MachineStubGen> {
        match num {
            Number::Fixnum(n) => Ok(Number::Fixnum(Fixnum::build_with(!n.get_num()))),
            Number::Integer(n) => Ok(Number::arena_from(Integer::from(!&*n), arena)),
            _ => Err(numerical_type_error(
                ValidType::Integer,
                num,
                atom!("\\"),
                2,
            )),
        }
    }

    #[inline]
    pub(crate) fn sign(num: Number, _arena: &mut Arena) -> Result<Number, Infallible> {
        Ok(num.sign())
    }
}

mod binary {
    use super::*;

    pub(crate) fn add(lhs: Number, rhs: Number, arena: &mut Arena) -> Result<Number, EvalError> {
        match (lhs, rhs) {
            (Number::Float(n1), n2) | (n2, Number::Float(n1)) => {
                // Any addition involving a float returns a float
                let n2: f64 = n2.try_into()?;
                let res = classify_float(*n1 + n2)?;
                Ok(Number::Float(OrderedFloat(res)))
            }
            (Number::Rational(n1), n2) | (n2, Number::Rational(n1)) => {
                // Any addition involving a rational and a non-float returns a rational
                let n2: Rational = n2.try_into()?;
                Ok(Number::arena_from(n2 + &*n1, arena))
            }
            (Number::Fixnum(n1), Number::Fixnum(n2)) => Ok(
                if let Some(result) = n1.get_num().checked_add(n2.get_num()) {
                    fixnum!(Number, result, arena)
                } else {
                    Number::arena_from(
                        Integer::from(n1.get_num()) + Integer::from(n2.get_num()),
                        arena,
                    )
                },
            ),
            (Number::Fixnum(n1), Number::Integer(n2))
            | (Number::Integer(n2), Number::Fixnum(n1)) => Ok(Number::arena_from(
                Integer::from(n1.get_num()) + &*n2,
                arena,
            )),
            (Number::Integer(n1), Number::Integer(n2)) => {
                Ok(Number::arena_from(&*n1 + &*n2, arena)) // add_i
            }
        }
    }

    #[inline]
    pub(crate) fn sub(lhs: Number, rhs: Number, arena: &mut Arena) -> Result<Number, EvalError> {
        let neg_result = super::unary::neg(rhs, arena).map_err::<EvalError, _>(Into::into)?;
        add(lhs, neg_result, arena)
    }

    pub(crate) fn mul(lhs: Number, rhs: Number, arena: &mut Arena) -> Result<Number, EvalError> {
        match (lhs, rhs) {
            (Number::Float(n1), n2) | (n2, Number::Float(n1)) => {
                // Any multiplication involving a float returns a float
                let n2: f64 = n2.try_into()?;
                let res = classify_float(*n1 * n2)?;
                Ok(Number::Float(OrderedFloat(res)))
            }
            (Number::Rational(n1), n2) | (n2, Number::Rational(n1)) => {
                // Any multiplication involving a rational and a non-float returns a rational
                let n2: Rational = n2.try_into()?;
                Ok(Number::arena_from(n2 * &*n1, arena))
            }
            (Number::Fixnum(n1), Number::Fixnum(n2)) => Ok(
                if let Some(result) = n1.get_num().checked_mul(n2.get_num()) {
                    fixnum!(Number, result, arena)
                } else {
                    Number::arena_from(
                        Integer::from(n1.get_num()) * Integer::from(n2.get_num()),
                        arena,
                    )
                },
            ),
            (Number::Fixnum(n1), Number::Integer(n2))
            | (Number::Integer(n2), Number::Fixnum(n1)) => Ok(Number::arena_from(
                Integer::from(n1.get_num()) * &*n2,
                arena,
            )),
            (Number::Integer(n1), Number::Integer(n2)) => {
                let n1_clone: Integer = (*n1).clone();
                Ok(Number::arena_from(Integer::from(n1_clone) * &*n2, arena)) // mul_i
            }
        }
    }

    #[inline]
    pub(crate) fn div(n1: Number, n2: Number, _arena: &mut Arena) -> Result<Number, EvalError> {
        n1 / n2
    }
}
