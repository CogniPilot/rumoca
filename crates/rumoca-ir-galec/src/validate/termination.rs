//! Termination analysis (S-2.10/S-2.11, GAL-017): the static function call
//! graph must be cycle-free (no direct or mutual recursion); every user
//! function must be transitively called from `DoStep` (dead functions are
//! illegal); `Startup` may call builtins only.
//!
//! `Recalibrate` presence and the exactly-three-methods rule are guaranteed
//! by construction ([`crate::ast::Block`] has the three methods as dedicated
//! fields), so they need no checks here. Calls to unknown targets are
//! reported by the type analysis (EG015).

use std::collections::HashSet;

use crate::diagnostic::{GalecError, Location, PathSegment};

use super::context::{BlockContext, lexeme};
use super::retained::{
    CallPathSegment, RetainedValidationBuilder, RetainedValidationError, UserCallEdge,
    UserCallGraph,
};

pub(super) fn check(
    ctx: &BlockContext<'_>,
    retained: &mut RetainedValidationBuilder,
    diags: &mut Vec<GalecError>,
) -> Result<(), RetainedValidationError> {
    retained.close_user_call_graph()?;
    let graph = retained.user_call_graph()?;
    startup_builtins_only(ctx, graph, diags);
    report_cycles(ctx, graph, diags)?;
    report_unreachable(ctx, graph, diags);
    Ok(())
}

fn startup_builtins_only(
    ctx: &BlockContext<'_>,
    graph: &UserCallGraph,
    diags: &mut Vec<GalecError>,
) {
    for edge in &graph.methods[0] {
        diags.push(GalecError::StartupCallsUserFunction {
            location: call_location(ctx, crate::ast::BlockMethodKind::Startup, edge),
            name: function_name(ctx, edge.callee.index()),
        });
    }
}

fn user_functions<'a>(
    ctx: &BlockContext<'a>,
) -> impl Iterator<Item = &'a crate::ast::UserFunction> {
    ctx.block
        .protected_functions
        .iter()
        .chain(&ctx.block.public_functions)
}

/// Iterative DFS cycle detection with explicit colors; valid deep call chains
/// consume heap, not the compiler's call stack.
fn report_cycles(
    ctx: &BlockContext<'_>,
    graph: &UserCallGraph,
    diags: &mut Vec<GalecError>,
) -> Result<(), RetainedValidationError> {
    // The retained graph keeps every exact call occurrence. Termination is a
    // relation over caller/callee identities, so project a stable unique
    // adjacency in first-occurrence order; duplicate source calls must not
    // duplicate one EG026 semantic defect.
    let adjacency = graph
        .functions
        .iter()
        .map(|edges| {
            let mut seen = HashSet::new();
            edges
                .iter()
                .filter_map(|edge| seen.insert(edge.callee).then_some(edge.callee))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let mut colors = vec![0_u8; adjacency.len()];
    for start in 0..adjacency.len() {
        if colors[start] != 0 {
            continue;
        }
        colors[start] = 1;
        let mut path = vec![start];
        let mut frames = vec![(start, 0_usize)];
        while let Some((node, next_edge)) = frames.last_mut() {
            let Some(callee) = adjacency[*node].get(*next_edge).copied() else {
                colors[*node] = 2;
                frames.pop();
                path.pop();
                continue;
            };
            *next_edge += 1;
            let callee = callee.index();
            match colors[callee] {
                0 => {
                    colors[callee] = 1;
                    path.push(callee);
                    frames.push((callee, 0));
                }
                1 => report_cycle(ctx, &path, callee, diags)?,
                _ => {}
            }
        }
    }
    Ok(())
}

fn report_cycle(
    ctx: &BlockContext<'_>,
    path: &[usize],
    callee: usize,
    diags: &mut Vec<GalecError>,
) -> Result<(), RetainedValidationError> {
    let Some(position) = path.iter().position(|node| *node == callee) else {
        return Err(RetainedValidationError::InconsistentFact {
            family: "user-call-graph-color",
            index: u32::try_from(callee).map_err(|_| RetainedValidationError::LocatorOverflow)?,
        });
    };
    let cycle = path[position..]
        .iter()
        .copied()
        .chain(std::iter::once(callee))
        .map(|index| function_name(ctx, index))
        .collect::<Vec<_>>()
        .join(" -> ");
    diags.push(GalecError::RecursiveCall {
        location: function_location(ctx, callee),
        cycle,
    });
    Ok(())
}

fn report_unreachable(ctx: &BlockContext<'_>, graph: &UserCallGraph, diags: &mut Vec<GalecError>) {
    let mut reached = vec![false; graph.functions.len()];
    let mut frontier = graph.methods[2]
        .iter()
        .map(|edge| edge.callee.index())
        .collect::<Vec<_>>();
    while let Some(index) = frontier.pop() {
        if !reached[index] {
            reached[index] = true;
            frontier.extend(
                graph.functions[index]
                    .iter()
                    .map(|edge| edge.callee.index()),
            );
        }
    }
    for (index, function) in user_functions(ctx).enumerate() {
        if !reached[index] {
            let name = lexeme(&function.name);
            diags.push(GalecError::UnreachableFunction {
                location: function_location(ctx, index),
                name,
            });
        }
    }
}

fn call_location(
    ctx: &BlockContext<'_>,
    method: crate::ast::BlockMethodKind,
    edge: &UserCallEdge,
) -> Location {
    let mut path = vec![
        PathSegment::Block(lexeme(&ctx.block.name)),
        PathSegment::Method(method),
    ];
    path.extend(edge.path.iter().map(|segment| match segment {
        CallPathSegment::Statement(index) => PathSegment::Statement(*index as usize),
        CallPathSegment::Branch(index) => PathSegment::Branch(*index as usize),
        CallPathSegment::Else => PathSegment::Else,
        CallPathSegment::Condition => PathSegment::Condition,
    }));
    Location::at(path)
}

fn function_name(ctx: &BlockContext<'_>, index: usize) -> String {
    user_functions(ctx)
        .nth(index)
        .map(|function| lexeme(&function.name))
        .unwrap_or_else(|| format!("<invalid-function-{index}>"))
}

fn function_location(ctx: &BlockContext<'_>, index: usize) -> Location {
    Location::at(vec![
        PathSegment::Block(lexeme(&ctx.block.name)),
        PathSegment::Function(function_name(ctx, index)),
    ])
}
