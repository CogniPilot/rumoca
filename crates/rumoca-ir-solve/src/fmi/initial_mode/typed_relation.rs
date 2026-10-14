use crate::{
    LinearOp, Reg, SolveOperation, SolvePureCallOutputKind, SolvePureCallTable, SolveScalarType,
    SolveStorageClass, TypedProgram,
};

pub(super) fn comparison(op: &LinearOp, returned: Reg, calls: &SolvePureCallTable) -> bool {
    let LinearOp::PureCall {
        dst_start, site, ..
    } = op
    else {
        return false;
    };
    let Some(owner) = calls.owner(site.owner()) else {
        return false;
    };
    *dst_start == returned
        && owner.call_site() == *site
        && matches!(owner.outputs(), [output]
            if output.kind() == SolvePureCallOutputKind::Result
                && output.value_type().dimensions().is_empty()
                && output.value_type().element_type() == SolveScalarType::Boolean)
        && returned_comparison(owner.body())
}

fn returned_comparison(body: &TypedProgram) -> bool {
    let [.., comparison, publication] = body.operations() else {
        return false;
    };
    let (SolveOperation::Compare { destination, .. }, SolveOperation::Store { slot, source }) =
        (comparison.operation(), publication.operation())
    else {
        return false;
    };
    let mut outputs = body
        .slots()
        .iter()
        .filter(|slot| slot.storage() == SolveStorageClass::Output);
    let Some(output) = outputs.next() else {
        return false;
    };
    destination == source
        && output.id() == *slot
        && outputs.next().is_none()
        && output.value_type().dimensions().is_empty()
        && output.value_type().element_type() == SolveScalarType::Boolean
}
