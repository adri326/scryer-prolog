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

macro_rules! native_ops {
    (
        dispatch: $dispatch_vis:vis $dispatch_name:ident,
        array: $array_vis:vis $array_name:ident;
        $($root:ident $(:: $path:ident)* ($($param:ident),*) => ( $name:tt, $run:path )),*
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
    ( __count; $head:ident $(, $($rest:ident),+)? ) => {
        1 + native_ops!(__count; $($rest),*)
    };
}

native_ops!(
    dispatch: pub(crate) dispatch_native_op,
    array: pub(super) NATIVE_OPS;

    Instruction::Abs(a, t) => ("abs", abs)
);

// const NATIVE_OPS: [ArithmeticOperator; 21] = [
//     native_op!("abs", Instruction::Abs(a, t), abs),
//     native_op!("-", 1, Neg, todo),
//     native_op!("+", 1, Plus, todo),
//     native_op!("cos", 1, Cos, todo),
//     native_op!("sin", 1, Sin, todo),
//     native_op!("tan", 1, Tan, todo),
//     native_op!("log", 1, Log, todo),
//     native_op!("exp", 1, Exp, todo),
//     native_op!("sqrt", 1, Sqrt, todo),
//     native_op!("acos", 1, ACos, todo),
//     native_op!("asin", 1, ASin, todo),
//     native_op!("atan", 1, ATan, todo),
//     native_op!("float", 1, Float, todo),
//     native_op!("truncate", 1, Truncate, todo),
//     native_op!("round", 1, Round, todo),
//     native_op!("ceiling", 1, Ceiling, todo),
//     native_op!("floor", 1, Floor, todo),
//     native_op!("float_integer_part", 1, FloatIntegerPart, todo),
//     native_op!("float_fractional_part", 1, FloatFractionalPart, todo),
//     native_op!("sign", 1, Sign, todo),
//     native_op!("\\", 1, BitwiseComplement, todo),
// ];

fn abs(n: Number, arena: &mut Arena) -> Result<Number, Infallible> {
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
