//! MLS §3.7.2/§8.5: the discontinuous part of a model quotient is held.

use rumoca_ir_dae as dae;

use crate::LowerError;

#[derive(Clone, Copy)]
pub(crate) struct QuotientMemory<'dae> {
    pub(crate) quotient: dae::ExprId<'dae>,
    pub(crate) ratio: dae::ExprId<'dae>,
    pub(crate) builtin: dae::PureBuiltin,
    pub(crate) parameter: usize,
}

impl QuotientMemory<'_> {
    pub(crate) fn step(self) -> rumoca_ir_solve::UnaryOp {
        quotient_step_operator(self.builtin)
    }
}

pub(crate) fn quotient_step_operator(builtin: dae::PureBuiltin) -> rumoca_ir_solve::UnaryOp {
    if builtin == dae::PureBuiltin::Mod {
        rumoca_ir_solve::UnaryOp::Floor
    } else {
        rumoca_ir_solve::UnaryOp::Trunc
    }
}

pub(crate) struct BufferedQuotients<'dae> {
    entries: Vec<QuotientMemory<'dae>>,
    by_expression: Vec<Option<usize>>,
    end: usize,
}

impl<'dae> BufferedQuotients<'dae> {
    pub(super) fn construct(
        view: dae::DaeView<'dae>,
        base: usize,
        relations: &super::relation_memory::BufferedRelations<'dae>,
    ) -> Result<Self, LowerError> {
        let mut entries = Vec::new();
        let mut by_expression = vec![None; view.expression_count()];
        let mut end = base;
        for index in 0..view.runtime_quotient_owner_count() {
            let owner = view
                .runtime_quotient_owner_at(index)
                .expect("dense quotient owner");
            let dae::RuntimeQuotientOwnerKind::ModelEvent {
                generated,
                relation,
                ..
            } = owner.kind()
            else {
                continue;
            };
            // Reuse the selected continuous-root plan. Clock-owned relations
            // are evaluated on their tick and have no continuous buffer.
            if relations.target(relation).is_none() {
                continue;
            }
            let expression = view
                .expression(owner.quotient())
                .expect("checked quotient expression");
            // Exact Integer owners keep their typed transport; their operands
            // are discrete already. This memory is the Real quotient's step.
            if expression.value_type().scalar_type() != dae::ScalarType::Real {
                continue;
            }
            let parameter = end;
            end = end.checked_add(1).ok_or_else(|| {
                LowerError::contract(
                    "quotient-memory layout overflow",
                    expression.provenance().span(),
                )
            })?;
            by_expression[owner.quotient().index() as usize] = Some(entries.len());
            entries.push(QuotientMemory {
                quotient: owner.quotient(),
                ratio: generated[0],
                builtin: owner.builtin(),
                parameter,
            });
        }
        Ok(Self {
            entries,
            by_expression,
            end,
        })
    }

    pub(crate) fn get(&self, expression: dae::ExprId<'dae>) -> Option<QuotientMemory<'dae>> {
        self.by_expression
            .get(expression.index() as usize)
            .copied()
            .flatten()
            .map(|index| self.entries[index])
    }

    pub(crate) fn entries(&self) -> &[QuotientMemory<'dae>] {
        &self.entries
    }

    pub(super) const fn end(&self) -> usize {
        self.end
    }
}
