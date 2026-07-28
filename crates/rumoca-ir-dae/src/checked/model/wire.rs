use serde::Deserialize;

use super::*;
use crate::checked::ExpressionAt;
use crate::checked::expression::{OperandRange, PackedSubscriptKind, Subscript};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DaeWire {
    schema_version: u16,
    source_map: SourceMap,
    storage: Storage,
}

impl<'de> Deserialize<'de> for Dae {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut wire = DaeWire::deserialize(deserializer)?;
        if wire.schema_version != CHECKED_DAE_SCHEMA_VERSION {
            return Err(serde::de::Error::custom(
                DaeConstructionError::InvalidSchemaVersion {
                    expected: CHECKED_DAE_SCHEMA_VERSION,
                    found: wire.schema_version,
                },
            ));
        }
        wire.source_map.rebuild_index();
        wire.storage
            .validate_columns()
            .map_err(serde::de::Error::custom)?;
        Dae::construct(wire.source_map, |dae| reconstruct(&wire.storage, dae))
            .map_err(serde::de::Error::custom)
    }
}

struct WireIds<'dae> {
    types: Vec<ValueTypeId<'dae>>,
    variables: Vec<VariableId<'dae>>,
    functions: Vec<FunctionId<'dae>>,
    domains: Vec<DomainId<'dae>>,
    conditions: Vec<ConditionId<'dae>>,
    expressions: Vec<ExprId<'dae>>,
}

fn reconstruct<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
) -> Result<(), DaeConstructionError> {
    let types = reconstruct_types(wire, dae)?;
    let variables = reconstruct_variables(wire, dae, &types)?;
    let functions = reconstruct_functions(wire, dae, &types)?;
    let domains = reconstruct_domains(wire, dae)?;
    let conditions = reconstruct_conditions(wire, dae)?;
    let mut ids = WireIds {
        types,
        variables,
        functions,
        domains,
        conditions,
        expressions: Vec::with_capacity(wire.expressions.nodes.len()),
    };
    reconstruct_expressions(wire, dae, &mut ids)?;
    define_variables(wire, dae, &ids)?;
    define_functions(wire, dae, &ids)?;
    define_conditions(wire, dae, &ids)?;
    reconstruct_equations(wire, dae, &ids)
}

fn reconstruct_types<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
) -> Result<Vec<ValueTypeId<'dae>>, DaeConstructionError> {
    let mut ids = Vec::with_capacity(wire.value_types.len());
    for (index, (ty, provenance)) in wire
        .value_types
        .iter()
        .zip(&wire.value_type_provenance)
        .enumerate()
    {
        let id = dae.types(|types| types.intern(ty.clone(), *provenance))?;
        expect_ordinal("value type", index, id.index(), *provenance)?;
        ids.push(id);
    }
    Ok(ids)
}

fn reconstruct_variables<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
    types: &[ValueTypeId<'dae>],
) -> Result<Vec<VariableId<'dae>>, DaeConstructionError> {
    let mut ids = Vec::with_capacity(wire.variables.len());
    for variable in &wire.variables {
        let ty = mapped(
            types,
            variable.value_type,
            "value type",
            variable.declaration,
        )?;
        let id = dae.variables(|variables| {
            variables.reserve_forward(variable.name.clone(), ty, variable.declaration)
        })?;
        ids.push(id);
    }
    Ok(ids)
}

fn reconstruct_functions<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
    types: &[ValueTypeId<'dae>],
) -> Result<Vec<FunctionId<'dae>>, DaeConstructionError> {
    let mut ids = Vec::with_capacity(wire.functions.len());
    for function in &wire.functions {
        let parameters = map_many(
            types,
            &function.parameters,
            "value type",
            function.declaration,
        )?;
        let results = map_many(types, &function.results, "value type", function.declaration)?;
        let id = dae.functions(|functions| {
            functions.reserve_recursive(
                function.name.clone(),
                parameters,
                results,
                function.declaration,
            )
        })?;
        ids.push(id);
    }
    Ok(ids)
}

fn reconstruct_domains<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
) -> Result<Vec<DomainId<'dae>>, DaeConstructionError> {
    wire.domains
        .iter()
        .map(|domain| dae.domains(|domains| domains.compact(domain.extent, domain.provenance)))
        .collect()
}

fn reconstruct_conditions<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
) -> Result<Vec<ConditionId<'dae>>, DaeConstructionError> {
    wire.conditions
        .iter()
        .map(|condition| dae.conditions(|conditions| conditions.reserve(condition.provenance)))
        .collect()
}

fn reconstruct_expressions<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
    ids: &mut WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, node) in wire.expressions.nodes.iter().enumerate() {
        let provenance = wire.expressions.provenance[index];
        let id = dae.expressions(|expressions| {
            rebuild_node(wire, ids, expressions.at(provenance), node, provenance)
        })?;
        let expected_type = mapped(
            &ids.types,
            wire.expressions.value_types[index],
            "value type",
            provenance,
        )?;
        let found_type = dae.storage.expressions.value_types[id.index() as usize];
        if found_type != expected_type.index() {
            return Err(DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        ids.expressions.push(id);
    }
    Ok(())
}

fn rebuild_node<'dae>(
    wire: &Storage,
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    node: &ExprNode,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    match node {
        ExprNode::Literal(value) => at.literal(value.clone()),
        ExprNode::Coordinate(coordinate) => {
            at.coordinate(rebuild_coordinate(ids, *coordinate, provenance)?)
        }
        ExprNode::Unary { operator, operand } => at.unary(
            *operator,
            mapped(&ids.expressions, *operand, "expression", provenance)?,
        ),
        ExprNode::Binary { operator, lhs, rhs } => at.binary(
            *operator,
            mapped(&ids.expressions, *lhs, "expression", provenance)?,
            mapped(&ids.expressions, *rhs, "expression", provenance)?,
        ),
        ExprNode::Conditional { operands } => {
            rebuild_conditional(wire, ids, at, *operands, provenance)
        }
        ExprNode::Array { operands } => {
            at.array(map_expression_operands(wire, ids, *operands, provenance)?)
        }
        ExprNode::Range { start, step, stop } => at.range(*start, *step, *stop),
        ExprNode::Comprehension { domain, body } => at.comprehension(
            mapped(&ids.domains, *domain, "domain", provenance)?,
            mapped(&ids.expressions, *body, "expression", provenance)?,
        ),
        ExprNode::Index { base, subscripts } => {
            rebuild_index(wire, ids, at, *base, *subscripts, provenance)
        }
        ExprNode::Builtin { builtin, operands } => at.builtin(
            *builtin,
            map_expression_operands(wire, ids, *operands, provenance)?,
        ),
        ExprNode::Call {
            function,
            output,
            operands,
        } => at.call(
            mapped(&ids.functions, *function, "function", provenance)?,
            *output as usize,
            map_expression_operands(wire, ids, *operands, provenance)?,
        ),
    }
}

fn rebuild_coordinate<'dae>(
    ids: &WireIds<'dae>,
    coordinate: Coordinate,
    at: DaeProvenance,
) -> Result<CoordinateInput<'dae>, DaeConstructionError> {
    Ok(match coordinate {
        Coordinate::Variable(variable) => {
            CoordinateInput::Variable(mapped(&ids.variables, variable, "variable", at)?)
        }
        Coordinate::Time => CoordinateInput::Time,
        Coordinate::Condition(condition) => {
            CoordinateInput::Condition(mapped(&ids.conditions, condition, "condition", at)?)
        }
        Coordinate::Delay(delay) => CoordinateInput::Delay(delay),
        Coordinate::Previous(variable) => {
            CoordinateInput::Previous(mapped(&ids.variables, variable, "variable", at)?)
        }
        Coordinate::Terminal => CoordinateInput::Terminal,
    })
}

fn rebuild_conditional<'dae>(
    wire: &Storage,
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    range: OperandRange,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let operands = wire_operands(wire, range, provenance)?;
    let Some((&fallback, branch_operands)) = operands.split_last() else {
        return Err(invalid_arity(1, 0, provenance));
    };
    if branch_operands.len() % 2 != 0 {
        return Err(invalid_arity(
            branch_operands.len() + 1,
            operands.len(),
            provenance,
        ));
    }
    let branches = branch_operands
        .chunks_exact(2)
        .map(|pair| {
            Ok((
                mapped(&ids.expressions, pair[0], "expression", provenance)?,
                mapped(&ids.expressions, pair[1], "expression", provenance)?,
            ))
        })
        .collect::<Result<Vec<_>, DaeConstructionError>>()?;
    at.conditional(
        branches,
        mapped(&ids.expressions, fallback, "expression", provenance)?,
    )
}

fn rebuild_index<'dae>(
    wire: &Storage,
    ids: &WireIds<'dae>,
    at: ExpressionAt<'_, 'dae>,
    base: u32,
    range: OperandRange,
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let packed = wire
        .expressions
        .subscripts
        .get(range.indices())
        .ok_or_else(|| unknown("subscript range", range.start, provenance))?;
    let subscripts = packed
        .iter()
        .map(|subscript| rebuild_subscript(ids, subscript, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    at.index(
        mapped(&ids.expressions, base, "expression", provenance)?,
        subscripts,
    )
}

fn rebuild_subscript<'dae>(
    ids: &WireIds<'dae>,
    subscript: &crate::checked::expression::PackedSubscript,
    at: DaeProvenance,
) -> Result<Subscript<'dae>, DaeConstructionError> {
    Ok(match subscript.kind {
        PackedSubscriptKind::Index(expression) => Subscript::Index {
            expression: mapped(&ids.expressions, expression, "expression", at)?,
            provenance: subscript.provenance,
        },
        PackedSubscriptKind::Whole => Subscript::Whole {
            provenance: subscript.provenance,
        },
        PackedSubscriptKind::Slice(expression) => Subscript::Slice {
            expression: mapped(&ids.expressions, expression, "expression", at)?,
            provenance: subscript.provenance,
        },
    })
}

fn define_variables<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, variable) in wire.variables.iter().enumerate() {
        let Some(definition) = &variable.definition else {
            return Err(incomplete("variable", index, variable.declaration));
        };
        let binding = definition
            .binding
            .map(|raw| mapped(&ids.expressions, raw, "expression", variable.declaration))
            .transpose()?;
        dae.variables(|variables| {
            variables.define(
                ids.variables[index],
                VariableDefinition { binding },
                variable.declaration,
            )
        })?;
    }
    Ok(())
}

fn define_functions<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, function) in wire.functions.iter().enumerate() {
        let Some(definition) = &function.definition else {
            return Err(incomplete("function", index, function.declaration));
        };
        let results = map_many(
            &ids.expressions,
            &definition.results,
            "expression",
            function.declaration,
        )?;
        dae.functions(|functions| {
            functions.define(ids.functions[index], results, function.declaration)
        })?;
    }
    Ok(())
}

fn define_conditions<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for (index, condition) in wire.conditions.iter().enumerate() {
        let Some(expression) = condition.expression else {
            return Err(incomplete("condition", index, condition.provenance));
        };
        let expression = mapped(
            &ids.expressions,
            expression,
            "expression",
            condition.provenance,
        )?;
        dae.conditions(|conditions| {
            conditions.define(ids.conditions[index], expression, condition.provenance)
        })?;
    }
    Ok(())
}

fn reconstruct_equations<'dae>(
    wire: &Storage,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for equation in &wire.equations {
        let residual = mapped(
            &ids.expressions,
            equation.residual,
            "expression",
            equation.provenance,
        )?;
        dae.equation(equation.provenance, |owner| owner.residual(residual))?;
    }
    Ok(())
}

fn map_expression_operands<'dae>(
    wire: &Storage,
    ids: &WireIds<'dae>,
    range: OperandRange,
    at: DaeProvenance,
) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
    let operands = wire_operands(wire, range, at)?;
    map_many(&ids.expressions, operands, "expression", at)
}

fn wire_operands(
    wire: &Storage,
    range: OperandRange,
    at: DaeProvenance,
) -> Result<&[u32], DaeConstructionError> {
    wire.expressions
        .operands
        .get(range.indices())
        .ok_or_else(|| unknown("operand range", range.start, at))
}

fn mapped<T: Copy>(
    ids: &[T],
    raw: u32,
    kind: &'static str,
    at: DaeProvenance,
) -> Result<T, DaeConstructionError> {
    ids.get(raw as usize)
        .copied()
        .ok_or_else(|| unknown(kind, raw, at))
}

fn map_many<T: Copy>(
    ids: &[T],
    raw: &[u32],
    kind: &'static str,
    at: DaeProvenance,
) -> Result<Vec<T>, DaeConstructionError> {
    raw.iter().map(|id| mapped(ids, *id, kind, at)).collect()
}

fn expect_ordinal(
    kind: &'static str,
    expected: usize,
    found: u32,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if usize::try_from(found) == Ok(expected) {
        return Ok(());
    }
    Err(duplicate(kind, found, at))
}

fn invalid_arity(expected: usize, found: usize, at: DaeProvenance) -> DaeConstructionError {
    DaeConstructionError::InvalidArity {
        expected,
        found,
        span: at.span(),
    }
}
