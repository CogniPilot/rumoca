//! MLS §3.7.2/§8.5: the discontinuous part of a model quotient is held.

use rumoca_ir_dae as dae;

use crate::LowerError;

#[derive(Clone, Copy)]
pub(crate) struct QuotientMemory<'dae> {
    pub(crate) quotient: dae::ExprId<'dae>,
    pub(crate) activation_guard: Option<dae::ExprId<'dae>>,
    pub(crate) ratio: dae::ExprId<'dae>,
    pub(crate) builtin: dae::PureBuiltin,
    pub(crate) parameter: usize,
    pub(crate) root_index: usize,
    pub(crate) domain: Option<dae::DomainId<'dae>>,
    pub(crate) count: usize,
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
        let structured_roots = structured_root_ordinals(view, relations.continuous_root_count())?;
        for index in 0..view.runtime_quotient_owner_count() {
            let owner = view
                .runtime_quotient_owner_at(index)
                .expect("dense quotient owner");
            let Some((generated, root_index, domain, count)) = (match owner.kind() {
                dae::RuntimeQuotientOwnerKind::ModelEvent {
                    generated, root, ..
                } => relations
                    .root_ordinal(root)
                    .map(|root_index| (generated, root_index, None, 1)),
                dae::RuntimeQuotientOwnerKind::StructuredModelEvent {
                    generated,
                    domain,
                    root,
                } => {
                    let count = structured_quotient_count(view, domain, owner.quotient())?;
                    Some((
                        generated,
                        structured_roots[root.index() as usize],
                        Some(domain),
                        count,
                    ))
                }
                dae::RuntimeQuotientOwnerKind::FunctionBody { .. } => None,
            }) else {
                // Function and clock-owned relations have no continuous buffer.
                continue;
            };
            let expression = view
                .expression(owner.quotient())
                .expect("checked quotient expression");
            // Exact Integer owners keep their typed transport; their operands
            // are discrete already. This memory is the Real quotient's step.
            if expression.value_type().scalar_type() != dae::ScalarType::Real {
                continue;
            }
            let parameter = end;
            end = end.checked_add(count).ok_or_else(|| {
                LowerError::contract(
                    "quotient-memory layout overflow",
                    expression.provenance().span(),
                )
            })?;
            by_expression[owner.quotient().index() as usize] = Some(entries.len());
            entries.push(QuotientMemory {
                quotient: owner.quotient(),
                activation_guard: owner.activation_guard(),
                ratio: generated[0],
                builtin: owner.builtin(),
                parameter,
                root_index,
                domain,
                count,
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

fn structured_quotient_count<'dae>(
    view: dae::DaeView<'dae>,
    domain: dae::DomainId<'dae>,
    quotient: dae::ExprId<'dae>,
) -> Result<usize, LowerError> {
    view.domain(domain)
        .expect("checked quotient domain")
        .structured()
        .scalar_count()
        .map_err(|error| {
            LowerError::contract(
                error.to_string(),
                view.expression(quotient)
                    .expect("checked quotient")
                    .provenance()
                    .span(),
            )
        })
}

fn structured_root_ordinals(
    view: dae::DaeView<'_>,
    mut root_end: usize,
) -> Result<Vec<usize>, LowerError> {
    let mut structured_roots = vec![0; view.structured_root_count()];
    for (id, root) in view.structured_roots() {
        structured_roots[id.index() as usize] = root_end;
        let count = view
            .domain(root.domain())
            .expect("checked root domain")
            .structured()
            .scalar_count()
            .map_err(|error| LowerError::contract(error.to_string(), root.provenance().span()))?;
        root_end = root_end.checked_add(count).ok_or_else(|| {
            LowerError::contract("structured root ordinal overflow", root.provenance().span())
        })?;
    }
    Ok(structured_roots)
}
