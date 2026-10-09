use super::*;
use rumoca_core::AffineForm;

pub(super) fn derive(
    body: &TypedProgram,
    operation: &SolveOperation,
    registers: &mut [Vec<SolveCallDependency>],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let mut outputs = Vec::new();
    operation.visit_output_registers(|output| outputs.push(output));
    if matches!(operation_access(operation), OperationAccess::Whole) {
        let Some((&last, preceding)) = outputs.split_last() else {
            return Ok(());
        };
        let mut dependencies = Dependencies::default();
        operation.visit_input_registers(|input| {
            for source in &registers[input.index()] {
                dependencies.insert(SolveCallDependency::whole(source.input));
            }
        });
        let dependencies = dependencies.finish();
        for output in preceding {
            registers[output.index()] = dependencies.clone();
        }
        registers[last.index()] = dependencies;
        return Ok(());
    }
    for output in outputs {
        let mut dependencies = Dependencies::default();
        for (input, access) in input_accesses(body, operation, output) {
            for source in &registers[input.index()] {
                let dependency = match &access {
                    Some(access) => source.remap(access, provenance)?,
                    None => SolveCallDependency::whole(source.input),
                };
                dependencies.insert(dependency);
            }
        }
        registers[output.index()] = dependencies.finish();
    }
    Ok(())
}

fn input_accesses(
    body: &TypedProgram,
    operation: &SolveOperation,
    output: SolveRegisterId,
) -> Vec<(SolveRegisterId, Option<Coordinates>)> {
    if let OperationAccess::Matrix { lhs, rhs } = operation_access(operation) {
        let [left, right] = matrix_accesses(
            body.register_types()[lhs.index()].dimensions(),
            body.register_types()[rhs.index()].dimensions(),
        );
        return vec![(lhs, Some(left)), (rhs, Some(right))];
    }
    let mut inputs = Vec::new();
    operation.visit_input_registers(|input| {
        inputs.push((input, access(body, operation, output, input)));
    });
    inputs
}

/// One access policy supplies both per-output coordinate projection and the
/// shared whole-input union. Region results retain their conservative rule.
enum OperationAccess<'operation> {
    Whole,
    Pointwise,
    Transpose,
    Element(&'operation [u32]),
    Slice(&'operation [u32]),
    Matrix {
        lhs: SolveRegisterId,
        rhs: SolveRegisterId,
    },
}

fn operation_access(operation: &SolveOperation) -> OperationAccess<'_> {
    match operation {
        SolveOperation::Unary { .. }
        | SolveOperation::Convert { .. }
        | SolveOperation::Binary { .. }
        | SolveOperation::Compare { .. }
        | SolveOperation::Select { .. }
        | SolveOperation::Scale { .. }
        | SolveOperation::BroadcastBinary { .. }
        | SolveOperation::Fill { .. } => OperationAccess::Pointwise,
        SolveOperation::Transpose { .. } => OperationAccess::Transpose,
        SolveOperation::ProjectElement { indices, .. } => OperationAccess::Element(indices),
        SolveOperation::ProjectSlice { origin, .. } => OperationAccess::Slice(origin),
        SolveOperation::MatrixMultiply { lhs, rhs, .. } => OperationAccess::Matrix {
            lhs: *lhs,
            rhs: *rhs,
        },
        _ => OperationAccess::Whole,
    }
}

pub(super) fn access(
    body: &TypedProgram,
    operation: &SolveOperation,
    output: SolveRegisterId,
    input: SolveRegisterId,
) -> Option<Coordinates> {
    let output_dimensions = body.register_types()[output.index()].dimensions();
    let input_dimensions = body.register_types()[input.index()].dimensions();
    let rank = output_dimensions.len();
    match operation_access(operation) {
        OperationAccess::Pointwise => pointwise_access(output_dimensions, input_dimensions),
        OperationAccess::Transpose => {
            let mut axes: Vec<_> = (0..rank)
                .map(|axis| AffineForm::unit_binder(axis, rank))
                .collect();
            axes.swap(0, 1);
            Some(Coordinates::access(rank, &[], axes))
        }
        OperationAccess::Element(indices) => Some(Coordinates::access(
            0,
            &[],
            indices
                .iter()
                .map(|&index| AffineForm::constant(i64::from(index), 0))
                .collect(),
        )),
        OperationAccess::Slice(origin) => Some(Coordinates::access(
            rank,
            &[],
            origin
                .iter()
                .enumerate()
                .map(|(axis, &start)| {
                    let mut form = AffineForm::unit_binder(axis, rank);
                    form.constant = i64::from(start);
                    form
                })
                .collect(),
        )),
        // The closed operand visitor supplies a conservative whole-input rule
        // for reductions, assembly, dynamic indexing, updates, and regions.
        OperationAccess::Whole | OperationAccess::Matrix { .. } => None,
    }
}

fn pointwise_access(output: &[u32], input: &[u32]) -> Option<Coordinates> {
    if output == input {
        return Some(Coordinates::identity(output.len()));
    }
    input
        .is_empty()
        .then(|| Coordinates::access(output.len(), &[], Vec::new()))
}

fn matrix_accesses(lhs: &[u32], rhs: &[u32]) -> [Coordinates; 2] {
    // Typed matrix construction admits exactly these four MLS products.
    let row_rank = usize::from(lhs.len() == 2);
    let output_rank = row_rank + usize::from(rhs.len() == 2);
    let free_axis = output_rank;
    let inner = lhs[lhs.len() - 1];
    let left = (0..row_rank).chain([free_axis]);
    let right = std::iter::once(free_axis).chain(row_rank..output_rank);
    [left.collect::<Vec<_>>(), right.collect()].map(|axes| {
        Coordinates::access(
            output_rank,
            &[inner],
            axes.into_iter()
                .map(|axis| AffineForm::unit_binder(axis, output_rank + 1))
                .collect(),
        )
    })
}
