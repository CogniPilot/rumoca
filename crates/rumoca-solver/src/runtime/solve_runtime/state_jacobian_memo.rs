//! Parameter-static state Jacobian (SPEC_0043 parameter-static gradient).
//!
//! When every implicit row of the derivative refresh plan's projection blocks
//! and every derivative program carries the parameter-static solver-Y gradient
//! certificate, each row's gradient is a function of its certified parameter
//! slots alone. The state Jacobian-vector product, which composes the block
//! sensitivities with the derivative gradient, is then a function of those
//! slots and the seed only: neither the coordinate nor time enters it. The
//! certificate is derived once when the runtime is built, and a product is
//! formed once per distinct parameter snapshot and seed, then reused. Between
//! events the discrete coordinates held in parameter slots do not change, so a
//! switching mode forms its products once.

use std::cell::RefCell;

use rumoca_ir_solve as solve;

use super::ProgramAnalysis;

/// Products retained per runtime: every color seed of the few switching
/// modes a model revisits, with room to spare.
const RETAINED_PRODUCTS: usize = 256;

/// The certified parameter slots and the products formed under them.
pub(crate) struct StateJacobianMemo {
    /// The union of the certified rows' parameter slots.
    slots: Box<[usize]>,
    products: RefCell<Vec<Product>>,
}

struct Product {
    parameter_bits: Box<[u64]>,
    seed_bits: Box<[u64]>,
    value: Box<[f64]>,
}

impl StateJacobianMemo {
    /// The memo of a runtime whose state Jacobian is parameter-static, or
    /// `None` when a row of `plan`'s blocks or a derivative program is not
    /// certified.
    pub(super) fn certify(
        plan: &solve::AlgebraicProjectionPlan,
        implicit: ProgramAnalysis<'_>,
        derivative: &solve::ScalarProgramBlock,
    ) -> Option<Self> {
        let mut slots = std::collections::BTreeSet::new();
        for row in plan
            .blocks
            .scalar_blocks()
            .iter()
            .flat_map(|block| &block.rows)
        {
            let (program, _) = implicit.row_output_position(*row)?;
            slots.extend(implicit.parameter_static_y_gradient_params(program)?);
        }
        for program in derivative.programs() {
            slots.extend(rumoca_eval_solve::parameter_static_y_gradient_params(
                program,
            )?);
        }
        Some(Self {
            slots: slots.into_iter().collect(),
            products: RefCell::new(Vec::new()),
        })
    }

    /// The product along `seed` under `parameters`, into `out`, when it was
    /// formed before.
    pub(crate) fn get(&self, parameters: &[f64], seed: &[f64], out: &mut [f64]) -> bool {
        let products = self.products.borrow();
        let Some(product) = products.iter().find(|product| {
            product.value.len() == out.len()
                && bits_equal(&product.seed_bits, seed)
                && self
                    .slots
                    .iter()
                    .zip(&product.parameter_bits)
                    .all(|(&slot, &bits)| parameters.get(slot).map(|v| v.to_bits()) == Some(bits))
        }) else {
            return false;
        };
        out.copy_from_slice(&product.value);
        true
    }

    /// Retain the product along `seed` under `parameters`.
    pub(crate) fn put(&self, parameters: &[f64], seed: &[f64], value: &[f64]) {
        let Some(parameter_bits) = self
            .slots
            .iter()
            .map(|&slot| parameters.get(slot).map(|v| v.to_bits()))
            .collect::<Option<Box<[u64]>>>()
        else {
            return;
        };
        let mut products = self.products.borrow_mut();
        if products.len() == RETAINED_PRODUCTS {
            products.remove(0);
        }
        products.push(Product {
            parameter_bits,
            seed_bits: seed.iter().map(|v| v.to_bits()).collect(),
            value: value.into(),
        });
    }
}

fn bits_equal(bits: &[u64], values: &[f64]) -> bool {
    bits.len() == values.len() && bits.iter().zip(values).all(|(&b, v)| b == v.to_bits())
}
