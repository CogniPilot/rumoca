//! Opt-in attribution for compiler-issued call owners.

use std::collections::BTreeMap;
use std::convert::Infallible;

use rumoca_ir_solve as solve;
use solve::SolveVisitor as _;

#[derive(Default)]
struct SiteCount {
    direct: usize,
    nested: usize,
    scalar: usize,
    guarded: usize,
    compute: usize,
}

#[derive(Default)]
struct PureCallSiteProfiler {
    counts: BTreeMap<u32, SiteCount>,
}

impl solve::SolveVisitor for PureCallSiteProfiler {
    type Error = Infallible;

    fn visit_linear_op_slice(
        &mut self,
        kind: solve::LinearOpSliceKind,
        operations: &[solve::LinearOp],
    ) -> Result<(), Self::Error> {
        for operation in operations {
            collect_operation(&mut self.counts, kind, operation, false);
        }
        Ok(())
    }
}

pub(super) fn pure_call_sites(problem: &solve::SolveProblem) {
    let mut profiler = PureCallSiteProfiler::default();
    let result = profiler.visit_solve_problem(problem);
    debug_assert!(result.is_ok(), "the pure-call profiler is infallible");
    for (owner, count) in profiler.counts {
        eprintln!(
            "rumoca-ir-profile kind=pure-call-sites owner={owner} direct={} nested={} scalar={} guarded={} compute={}",
            count.direct, count.nested, count.scalar, count.guarded, count.compute,
        );
    }
}

pub(super) fn typed_owner_calls(table: &solve::SolvePureCallTable) {
    for owner in table.owners() {
        let mut calls = BTreeMap::<u32, usize>::new();
        let mut scopes = BTreeMap::<u32, TypedCallScopes>::new();
        collect_typed_calls(owner.body(), &mut calls, &mut scopes, 0, 0);
        let calls = calls
            .into_iter()
            .map(|(callee, count)| format!("{callee}:{count}"))
            .collect::<Vec<_>>()
            .join(",");
        let scopes = scopes
            .into_iter()
            .map(|(callee, scope)| {
                format!(
                    "{callee}:{}/{}/{}",
                    scope.direct, scope.conditional, scope.iterative
                )
            })
            .collect::<Vec<_>>()
            .join(",");
        eprintln!(
            "rumoca-ir-profile kind=typed-owner-calls owner={} direct_ops={} calls=[{calls}] scopes=[{scopes}]",
            owner.id().index(),
            owner.body().operations().len(),
        );
    }
}

#[derive(Default)]
struct TypedCallScopes {
    direct: usize,
    conditional: usize,
    iterative: usize,
}

fn collect_typed_calls(
    program: &solve::TypedProgram,
    calls: &mut BTreeMap<u32, usize>,
    scopes: &mut BTreeMap<u32, TypedCallScopes>,
    conditional_depth: usize,
    iterative_depth: usize,
) {
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::Call { owner, .. } => {
                *calls.entry(owner.index()).or_default() += 1;
                let scope = scopes.entry(owner.index()).or_default();
                if iterative_depth != 0 {
                    scope.iterative += 1;
                } else if conditional_depth != 0 {
                    scope.conditional += 1;
                } else {
                    scope.direct += 1;
                }
            }
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                collect_typed_calls(
                    if_true.body(),
                    calls,
                    scopes,
                    conditional_depth + 1,
                    iterative_depth,
                );
                collect_typed_calls(
                    if_false.body(),
                    calls,
                    scopes,
                    conditional_depth + 1,
                    iterative_depth,
                );
            }
            solve::SolveOperation::Map { body, .. } => collect_typed_calls(
                body.body(),
                calls,
                scopes,
                conditional_depth,
                iterative_depth + 1,
            ),
            solve::SolveOperation::Fold { transition, .. } => {
                collect_typed_calls(
                    transition.body(),
                    calls,
                    scopes,
                    conditional_depth,
                    iterative_depth + 1,
                );
            }
            _ => {}
        }
    }
}

fn collect_operation(
    counts: &mut BTreeMap<u32, SiteCount>,
    kind: solve::LinearOpSliceKind,
    operation: &solve::LinearOp,
    nested: bool,
) {
    match operation {
        solve::LinearOp::PureCall { site, .. } => {
            let count = counts.entry(site.owner().index()).or_default();
            if nested {
                count.nested += 1;
            } else {
                count.direct += 1;
            }
            match kind {
                solve::LinearOpSliceKind::ScalarProgram { .. } => count.scalar += 1,
                solve::LinearOpSliceKind::GuardedAssignmentProgram { .. } => count.guarded += 1,
                _ => count.compute += 1,
            }
            let (scope, index, span) = slice_location(kind);
            eprintln!(
                "rumoca-ir-profile kind=pure-call-site owner={} nested={nested} scope={scope} index={index} source={} start={} end={}",
                site.owner().index(),
                span.map_or(0, |span| span.source.0),
                span.map_or(0, |span| span.start.0),
                span.map_or(0, |span| span.end.0),
            );
        }
        solve::LinearOp::FunctionFold { program, .. }
        | solve::LinearOp::GuardedFunctionFold { program, .. }
        | solve::LinearOp::StoreOutputFunctionFold { program, .. } => {
            collect_operations(counts, kind, &program.update);
        }
        solve::LinearOp::FunctionConditional { program, .. } => {
            for arm in &program.arms {
                collect_operations(counts, kind, &arm.condition);
                collect_operations(counts, kind, &arm.result);
            }
            collect_operations(counts, kind, &program.fallback);
        }
        _ => {}
    }
}

fn slice_location(
    kind: solve::LinearOpSliceKind,
) -> (&'static str, usize, Option<rumoca_core::Span>) {
    match kind {
        solve::LinearOpSliceKind::ScalarProgram {
            program_index,
            span,
        } => ("scalar", program_index, span),
        solve::LinearOpSliceKind::GuardedAssignmentProgram {
            program_index,
            span,
        } => ("guarded", program_index, Some(span)),
        solve::LinearOpSliceKind::MatMulLhs { node_index, span } => {
            ("matmul-lhs", node_index, Some(span))
        }
        solve::LinearOpSliceKind::MatMulRhs { node_index, span } => {
            ("matmul-rhs", node_index, Some(span))
        }
        solve::LinearOpSliceKind::LinSolveSetup { node_index, span } => {
            ("linsolve", node_index, Some(span))
        }
        solve::LinearOpSliceKind::MapBase { node_index, span } => ("map", node_index, Some(span)),
        solve::LinearOpSliceKind::AffineStencilBase { node_index, span } => {
            ("stencil", node_index, Some(span))
        }
    }
}

fn collect_operations(
    counts: &mut BTreeMap<u32, SiteCount>,
    kind: solve::LinearOpSliceKind,
    operations: &[solve::LinearOp],
) {
    for operation in operations {
        collect_operation(counts, kind, operation, true);
    }
}
