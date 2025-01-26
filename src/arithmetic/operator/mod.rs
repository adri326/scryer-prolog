use std::{borrow::Cow, collections::HashMap};

mod native;
pub(crate) use native::dispatch_native_op;
use native::NativeArithmeticOperator;

pub(crate) mod macros;

use super::*;

#[derive(Clone, Debug)]
pub(super) enum ArithmeticMode {
    /// Operators with dedicated instructions and runners, like `+/2`, `-/1`, etc.
    Native(NativeArithmeticOperator),
}

#[derive(Debug, Clone)]
pub struct ArithmeticOperator {
    pub(super) name: Cow<'static, str>,
    pub(super) arity: usize,
    pub(super) mode: ArithmeticMode,
}

impl ArithmeticOperator {
    fn arity(&self) -> usize {
        self.arity
    }

    fn name(&self) -> &str {
        self.name.as_ref()
    }
}

#[derive(Debug)]
pub struct ArithmeticOperatorTable {
    arity_map: Vec<HashMap<String, ArithmeticOperator>>,
}

impl Default for ArithmeticOperatorTable {
    fn default() -> Self {
        Self::with_native()
    }
}

impl ArithmeticOperatorTable {
    pub fn with_native() -> Self {
        let max_arity = native::NATIVE_OPS
            .iter()
            .map(|op| op.arity())
            .max()
            .unwrap_or_default();
        let mut arity_map = vec![HashMap::default(); max_arity + 1];

        for native_op in native::NATIVE_OPS.iter() {
            arity_map[native_op.arity()].insert(native_op.name().to_string(), native_op.clone());
        }

        Self { arity_map }
    }

    pub fn run(
        &self,
        name: Atom,
        arity: usize,
        interms: &mut Vec<Number>,
        arena: &mut Arena,
    ) -> Result<(), MachineStubGen> {
        let Some(op) = self.get(name.as_str(), arity) else {
            return Err(Box::new(move |machine_st| {
                let evaluable_stub = functor_stub(name, arity);
                let stub = functor_stub(atom!("is"), 2);

                let type_error = machine_st.type_error(ValidType::Evaluable, evaluable_stub);
                machine_st.error_form(type_error, stub)
            }));
        };

        let interms_len = interms.len();
        // TODO: smallvec optimization
        let values = interms
            .drain((interms_len - arity)..)
            .map(|num| num)
            .collect::<Vec<_>>();

        match &op.mode {
            ArithmeticMode::Native(native_op) => {
                let res = native_op.run(&values, arena)?;
                interms.push(res);
            }
        }

        Ok(())
    }

    fn get<'a>(&'a self, name: impl AsRef<str>, arity: usize) -> Option<&'a ArithmeticOperator> {
        self.arity_map
            .get(arity)
            .and_then(|map| map.get(name.as_ref()))
    }

    pub fn get_instr(
        &self,
        arith_terms: &[ArithmeticTerm],
        name: Atom,
        output_index: usize,
    ) -> Option<Instruction> {
        let op = self.get(name.as_str(), arith_terms.len())?;

        match &op.mode {
            ArithmeticMode::Native(native_op) => {
                Some(native_op.get_instr(arith_terms, output_index))
            }
        }
    }
}
