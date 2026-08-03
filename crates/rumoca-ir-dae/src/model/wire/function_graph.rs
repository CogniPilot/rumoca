use super::*;

#[derive(Debug)]
pub(super) struct WireFunctionComponent {
    pub(super) functions: Box<[usize]>,
    pub(super) recursive: bool,
    pub(super) expression_end: usize,
}

pub(super) fn function_components(
    wire: &StorageWire,
) -> Result<Vec<WireFunctionComponent>, DaeConstructionError> {
    let children = ExpressionChildren::new(&wire.expressions)?;
    let expression_scopes = expression_scopes(&wire.expressions, &children)?;
    let mut dependencies = Vec::with_capacity(wire.functions.len());
    let mut expression_ends = Vec::with_capacity(wire.functions.len());
    for function in &wire.functions {
        let (function_dependencies, expression_end) =
            function_dependencies(wire, function, &children)?;
        dependencies.push(function_dependencies);
        expression_ends.push(expression_end);
    }
    for (expression, scope) in expression_scopes.into_iter().enumerate() {
        if let Some(function) = scope {
            let end = expression_ends
                .get_mut(function)
                .ok_or_else(|| malformed("expressions.nodes.function_scope"))?;
            *end = (*end).max(expression + 1);
        }
    }
    let mut components = rumoca_core::dependency_first_sccs(&dependencies)
        .map_err(|_| malformed("functions.dependencies"))?
        .into_iter()
        .map(|component| WireFunctionComponent {
            expression_end: component
                .members
                .iter()
                .map(|&function| expression_ends[function])
                .max()
                .unwrap_or(0),
            functions: component.members,
            recursive: component.recursive,
        })
        .collect::<Vec<_>>();
    let mut expected = 0;
    for component in &components {
        for &function in &component.functions {
            if function != expected {
                return Err(malformed("functions.dependency_order"));
            }
            expected += 1;
        }
    }
    if expected != wire.functions.len() {
        return Err(malformed("functions.dependency_order"));
    }
    let mut previous_end = 0;
    for component in &mut components {
        if component.expression_end < previous_end {
            return Err(malformed("functions.expression_order"));
        }
        component.expression_end = component.expression_end.max(previous_end);
        previous_end = component.expression_end;
    }
    Ok(components)
}

struct ExpressionChildren {
    operand_starts: Vec<usize>,
    subscript_starts: Vec<usize>,
}

impl ExpressionChildren {
    fn new(arena: &ExpressionArenaWire) -> Result<Self, DaeConstructionError> {
        let mut operand_starts = Vec::with_capacity(arena.nodes.len());
        let mut subscript_starts = Vec::with_capacity(arena.nodes.len());
        let mut operand = 0_usize;
        let mut subscript = 0_usize;
        for node in &arena.nodes {
            operand_starts.push(operand);
            subscript_starts.push(subscript);
            operand = operand
                .checked_add(operand_count(node) as usize)
                .ok_or_else(|| malformed("expressions.operands"))?;
            subscript = subscript
                .checked_add(subscript_count(node) as usize)
                .ok_or_else(|| malformed("expressions.subscripts"))?;
        }
        if operand != arena.operands.len() || subscript != arena.subscripts.len() {
            return Err(malformed("expressions.packed_buffers"));
        }
        Ok(Self {
            operand_starts,
            subscript_starts,
        })
    }

    fn push(
        &self,
        arena: &ExpressionArenaWire,
        expression: usize,
        pending: &mut Vec<u32>,
    ) -> Result<(), DaeConstructionError> {
        let node = arena
            .nodes
            .get(expression)
            .ok_or_else(|| malformed("expressions.nodes"))?;
        match node {
            ExprNodeWire::Unary { operand, .. }
            | ExprNodeWire::Field { base: operand, .. }
            | ExprNodeWire::Comprehension { body: operand, .. } => pending.push(*operand),
            ExprNodeWire::Binary { lhs, rhs, .. } => pending.extend([*lhs, *rhs]),
            ExprNodeWire::Index { base, .. } => pending.push(*base),
            ExprNodeWire::ArrayUpdate { base, value, .. } => pending.extend([*base, *value]),
            _ => {}
        }
        let operand_start = self.operand_starts[expression];
        let operand_end = operand_start + operand_count(node) as usize;
        pending.extend(arena.operands[operand_start..operand_end].iter().copied());
        let subscript_start = self.subscript_starts[expression];
        let subscript_end = subscript_start + subscript_count(node) as usize;
        for subscript in &arena.subscripts[subscript_start..subscript_end] {
            match subscript.kind {
                PackedSubscriptKindWire::Index(value) | PackedSubscriptKindWire::Slice(value) => {
                    pending.push(value)
                }
                PackedSubscriptKindWire::Whole => {}
            }
        }
        Ok(())
    }
}

fn operand_count(node: &ExprNodeWire) -> u32 {
    match node {
        ExprNodeWire::Conditional { operand_count }
        | ExprNodeWire::Array { operand_count, .. }
        | ExprNodeWire::Record { operand_count, .. }
        | ExprNodeWire::Builtin { operand_count, .. }
        | ExprNodeWire::Call { operand_count, .. } => *operand_count,
        _ => 0,
    }
}

fn subscript_count(node: &ExprNodeWire) -> u32 {
    match node {
        ExprNodeWire::Index {
            subscript_count, ..
        }
        | ExprNodeWire::ArrayUpdate {
            subscript_count, ..
        } => *subscript_count,
        _ => 0,
    }
}

fn function_dependencies(
    wire: &StorageWire,
    function: &FunctionEntryWire,
    children: &ExpressionChildren,
) -> Result<(Vec<usize>, usize), DaeConstructionError> {
    let mut pending = Vec::new();
    push_statement_roots(&function.statements, &mut pending);
    // An external interface roots its own argument expressions, so they are
    // replayed inside the owning component exactly like body assignments.
    if let Some(external) = &function.external {
        for argument in &external.arguments {
            if let ExternalArgumentEntry::Input(expression) = argument {
                pending.push(*expression);
            }
        }
    }
    let mut seen = vec![false; wire.expressions.nodes.len()];
    let mut dependencies = Vec::new();
    let mut expression_end = 0;
    while let Some(raw) = pending.pop() {
        let expression = raw as usize;
        expression_end = expression_end.max(expression + 1);
        let visited = seen
            .get_mut(expression)
            .ok_or_else(|| malformed("functions.statements.rhs"))?;
        if std::mem::replace(visited, true) {
            continue;
        }
        if let ExprNodeWire::Call {
            function: callee, ..
        } = wire.expressions.nodes[expression]
        {
            let callee = callee as usize;
            if callee >= wire.functions.len() {
                return Err(malformed("expressions.nodes.call.function"));
            }
            if !dependencies.contains(&callee) {
                dependencies.push(callee);
            }
        }
        children.push(&wire.expressions, expression, &mut pending)?;
    }
    dependencies.sort_unstable();
    Ok((dependencies, expression_end))
}

fn expression_scopes(
    arena: &ExpressionArenaWire,
    children: &ExpressionChildren,
) -> Result<Vec<Option<usize>>, DaeConstructionError> {
    let mut scopes = Vec::with_capacity(arena.nodes.len());
    for (index, node) in arena.nodes.iter().enumerate() {
        let leaf = match node {
            ExprNodeWire::Coordinate(CoordinateWire::FunctionParameter { function, .. })
            | ExprNodeWire::FunctionValue { function, .. }
            | ExprNodeWire::FunctionFoldParameter { function, .. }
            | ExprNodeWire::FunctionFoldOutput { function, .. } => Some(*function as usize),
            _ => None,
        };
        let mut scope = leaf;
        let mut operands = Vec::new();
        children.push(arena, index, &mut operands)?;
        for operand in operands {
            let operand = operand as usize;
            if operand >= index {
                return Err(malformed("expressions.nodes.dependency_order"));
            }
            let child = scopes[operand];
            scope = match (scope, child) {
                (None, child) => child,
                (scope, None) => scope,
                (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
                (Some(_), Some(_)) => {
                    return Err(malformed("expressions.nodes.function_scope"));
                }
            };
        }
        scopes.push(scope);
    }
    Ok(scopes)
}

fn push_statement_roots(statements: &[FunctionStatementInput], pending: &mut Vec<u32>) {
    for statement in statements {
        match statement {
            FunctionStatementInput::Assignment { rhs, .. } => pending.push(*rhs),
            FunctionStatementInput::Assertion {
                condition, message, ..
            } => {
                pending.push(*condition);
                pending.push(*message);
            }
            FunctionStatementInput::For { statements, .. } => {
                push_statement_roots(statements, pending);
            }
        }
    }
}
