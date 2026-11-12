//! Exact compact outer-product Map. The checked Map owns capture/binder types.
use crate::{SolveBinaryOperator, SolveOperation as O, SolveProgramRegion, TypedProgram};
use rumoca_core::StructuredIndexDomain;
fn six(domain: &StructuredIndexDomain) -> bool {
    matches!(domain.binders.as_slice(),[b] if b.lower==1&&b.upper==6&&b.step==1)
}
pub(super) fn matches(domain: &StructuredIndexDomain, region: &SolveProgramRegion) -> bool {
    if !six(domain) || region.inputs().len() != 2 || region.outputs().len() != 1 {
        return false;
    }
    let ops = region
        .body()
        .operations()
        .iter()
        .map(|o| o.operation())
        .collect::<Vec<_>>();
    let [
        O::Load {
            destination: u,
            slot: u_slot,
        },
        O::Load {
            destination: i,
            slot: i_slot,
        },
        O::Map {
            destination: row,
            domain: inner_domain,
            captures,
            body: inner,
        },
        O::Store {
            slot: output,
            source,
        },
    ] = ops.as_slice()
    else {
        return false;
    };
    u_slot.index() == 0
        && i_slot.index() == 1
        && output.index() == 2
        && source == row
        && captures.as_ref() == [*u, *i]
        && six(inner_domain)
        && inner.inputs().len() == 3
        && inner.outputs().len() == 1
        && element(inner.body())
}
fn element(body: &TypedProgram) -> bool {
    let ops = body
        .operations()
        .iter()
        .map(|o| o.operation())
        .collect::<Vec<_>>();
    let [
        O::Load {
            destination: u,
            slot: su,
        },
        O::Load {
            destination: i,
            slot: si,
        },
        O::Load {
            destination: j,
            slot: sj,
        },
        O::ProjectElementDynamic {
            destination: a,
            aggregate: ua,
            indices: ia,
        },
        O::ProjectElementDynamic {
            destination: b,
            aggregate: ub,
            indices: ib,
        },
        O::Binary {
            destination: product,
            operator: SolveBinaryOperator::Multiply,
            lhs,
            rhs,
        },
        O::Store {
            slot: output,
            source,
        },
    ] = ops.as_slice()
    else {
        return false;
    };
    su.index() == 0
        && si.index() == 1
        && sj.index() == 2
        && output.index() == 3
        && ua == u
        && ub == u
        && ia.as_ref() == [*i]
        && ib.as_ref() == [*j]
        && lhs == a
        && rhs == b
        && source == product
}
