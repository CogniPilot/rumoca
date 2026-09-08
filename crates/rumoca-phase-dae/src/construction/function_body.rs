use super::*;

pub(super) fn lower_generated_boolean_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    target: &VarName,
    value: &Expression,
    span: Span,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let lowered = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        &body,
        value,
    )?;
    let target = function_value_coordinate(symbols.coordinates, target, span)?;
    let provenance = dae::DaeProvenance::source(span)?;
    construction.functions(|functions| functions.assign(&mut body, target, lowered, provenance))?;
    Ok(body)
}

pub(super) fn lower_integer_reduction<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    _function: &rumoca_core::Function,
    initial: &FunctionStatementSequence,
    result: &VarName,
    reduction: &FunctionIntegerReduction,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let mut body = lower_function_statements(construction, symbols, body, initial)?;
    match reduction {
        FunctionIntegerReduction::WhileExclusive { bound, one, span } => {
            let target = function_value_coordinate(symbols.coordinates, result, *span)?;
            lower_while_sum(construction, symbols, &mut body, bound, one, *span, target)?;
        }
        FunctionIntegerReduction::ForInclusiveCapped {
            one,
            end,
            cap,
            span,
        } => {
            let target = function_value_coordinate(symbols.coordinates, result, *span)?;
            lower_capped_for_sum(
                construction,
                symbols,
                &mut body,
                CappedForSum {
                    one,
                    end,
                    cap,
                    span: *span,
                    target,
                },
            )?;
        }
    }
    Ok(body)
}

fn lower_while_sum<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    bound: &Expression,
    one_source: &Expression,
    span: Span,
    target: dae::FunctionValueId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let owner = dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, span)?;
    let zero = construction.functions(|functions| functions.read(body, target, owner))?;
    let bound = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        body,
        bound,
    )?;
    let one = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        body,
        one_source,
    )?;
    let positive = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Greater, bound, zero)
    })?;
    let count = construction
        .expressions(|expressions| expressions.at(owner).conditional([(positive, bound)], zero))?;
    let value = lower_integer_series(construction, owner, zero, one, count, false)?;
    construction.functions(|functions| functions.assign(body, target, value, owner))
}

struct CappedForSum<'source, 'dae> {
    one: &'source Expression,
    end: &'source Expression,
    cap: &'source Expression,
    span: Span,
    target: dae::FunctionValueId<'dae>,
}

fn lower_capped_for_sum<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &mut dae::FunctionBody<'dae>,
    request: CappedForSum<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let CappedForSum {
        one,
        end,
        cap,
        span,
        target,
    } = request;
    let owner = dae::DaeProvenance::generated(dae::DaeGeneration::FunctionLoopLowering, span)?;
    let zero = construction.functions(|functions| functions.read(body, target, owner))?;
    let one = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        body,
        one,
    )?;
    let bound = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        body,
        end,
    )?;
    let cap = lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        body,
        cap,
    )?;
    let empty = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Less, bound, one)
    })?;
    let capped = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Greater, bound, cap)
    })?;
    let count = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .conditional([(empty, zero), (capped, cap)], bound)
    })?;
    let value = lower_integer_series(construction, owner, zero, one, count, true)?;
    construction.functions(|functions| functions.assign(body, target, value, owner))
}

fn lower_integer_series<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    owner: dae::DaeProvenance,
    zero: dae::ExprId<'dae>,
    one: dae::ExprId<'dae>,
    count: dae::ExprId<'dae>,
    inclusive: bool,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let adjacent = construction.expressions(|expressions| {
        expressions.at(owner).binary(
            if inclusive {
                dae::BinaryOperator::Add
            } else {
                dae::BinaryOperator::Subtract
            },
            count,
            one,
        )
    })?;
    let product = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Multiply, count, adjacent)
    })?;
    let two = construction
        .expressions(|expressions| expressions.at(owner).literal(dae::DaeLiteral::Integer(2)))?;
    let quotient = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Divide, product, two)
    })?;
    let sum = construction.expressions(|expressions| {
        expressions
            .at(owner)
            .builtin(dae::PureBuiltin::Integer, [quotient])
    })?;
    construction.expressions(|expressions| {
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Add, zero, sum)
    })
}

pub(super) struct GuardedFunctionReturn<'source> {
    pub(super) conditions: &'source [Expression],
    pub(super) branches: &'source [FunctionStatementSequence],
    pub(super) tail: &'source FunctionStatementSequence,
    pub(super) targets: &'source [VarName],
    pub(super) span: Span,
}

pub(super) fn lower_guarded_function_return<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: dae::FunctionBody<'dae>,
    request: GuardedFunctionReturn<'_>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let GuardedFunctionReturn {
        conditions,
        branches,
        tail,
        targets,
        span,
    } = request;
    let mut body = body;
    let conditions = conditions
        .iter()
        .map(|condition| {
            lower_function_expression(
                construction,
                symbols.coordinates,
                Some(symbols.record_staging_scope()),
                symbols.functions,
                symbols.shapes,
                &body,
                condition,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let returned = targets
        .iter()
        .map(|target| {
            branches
                .iter()
                .map(|branch| {
                    lower_guarded_return_value(construction, symbols, &body, branch, target)
                })
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;

    body = lower_function_statements(construction, symbols, body, tail)?;
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, span)?;
    for (target, returned) in targets.iter().zip(returned) {
        let target = function_value_coordinate(symbols.coordinates, target, span)?;
        let fallback =
            construction.functions(|functions| functions.read(&body, target, provenance))?;
        let branches = conditions.iter().copied().zip(returned);
        let value = construction.expressions(|expressions| {
            expressions.at(provenance).conditional(branches, fallback)
        })?;
        construction
            .functions(|functions| functions.assign(&mut body, target, value, provenance))?;
    }
    Ok(body)
}

fn lower_guarded_return_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    body: &dae::FunctionBody<'dae>,
    sequence: &FunctionStatementSequence,
    selected: &VarName,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let assignment =
        sequence
            .products()
            .iter()
            .find_map(|product| match (product.source(), product.plan()) {
                (
                    [rumoca_core::Statement::Assignment { value, .. }],
                    FunctionStatementPlan::Assignment(assignment),
                ) if assignment.target() == selected => Some(value),
                _ => None,
            });
    let Some(assignment) = assignment else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: sequence.span(),
        });
    };
    lower_function_expression(
        construction,
        symbols.coordinates,
        Some(symbols.record_staging_scope()),
        symbols.functions,
        symbols.shapes,
        body,
        assignment,
    )
}

pub(super) struct FunctionConditional<'scope, 'statement, 'dae> {
    pub(super) symbols: FunctionSymbols<'scope, 'dae>,
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) conditions: &'statement [Expression],
    pub(super) branches: &'statement [FunctionStatementSequence],
    pub(super) fallback: Option<&'statement FunctionStatementSequence>,
    pub(super) targets: &'statement [FunctionConditionalTarget],
    pub(super) span: Span,
}

/// Lower one MLS §11.5 function conditional into its checked value owners.
///
/// A branch is an ordinary algorithm section: its assignments run in order, a
/// later assignment reads what an earlier one wrote, and the last write to a
/// value is the one the branch defines. Each branch therefore builds its own
/// value environment first, and the join then owns one conditional expression
/// per value the conditional defines on all of its paths.
pub(super) fn lower_function_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    input: FunctionConditional<'_, '_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    let lowered = lower_function_conditional_values(construction, body, input)?;
    construction.functions(|functions| {
        functions.assign_conditional_all(
            body,
            &lowered.targets,
            &lowered.conditions,
            &lowered.branches,
            &lowered.fallback,
            provenance,
        )
    })
}

struct LoweredFunctionConditional<'dae> {
    targets: Vec<dae::FunctionValueId<'dae>>,
    conditions: Vec<dae::ExprId<'dae>>,
    branches: Vec<Vec<dae::ExprId<'dae>>>,
    fallback: Vec<dae::ExprId<'dae>>,
}

fn lower_function_conditional_values<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: FunctionConditional<'_, '_, 'dae>,
) -> Result<LoweredFunctionConditional<'dae>, dae::DaeConstructionError> {
    let conditions = input
        .conditions
        .iter()
        .map(|condition| {
            lower_function_expression_scoped(
                construction,
                FunctionExpressionValues {
                    coordinates: input.symbols.coordinates,
                    record_staging: Some(input.symbols.record_staging_scope()),
                },
                input.symbols.functions,
                input.symbols.shapes,
                body,
                input.binders,
                condition,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    let mut branch_values = Vec::with_capacity(input.branches.len());
    for branch in input.branches {
        branch_values.push(lower_conditional_branch(
            construction,
            body,
            ConditionalBranch {
                symbols: input.symbols,
                binders: input.binders,
                sequence: branch,
            },
        )?);
    }
    let fallback_values = match input.fallback {
        Some(sequence) => Some(lower_conditional_branch(
            construction,
            body,
            ConditionalBranch {
                symbols: input.symbols,
                binders: input.binders,
                sequence,
            },
        )?),
        None => None,
    };
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    // Correlate every branch value against the shared pre-conditional
    // definitions, then let the DAE constructor derive and commit all joined
    // definitions together. A target's branch value may read a sibling target's
    // pre-conditional definition (`X[i] := value` while `value := value - L·X[k]`),
    // and those reads can be mutual, so no per-target assignment order keeps
    // every read fact current — only an atomic commit does.
    let mut targets = Vec::with_capacity(input.targets.len());
    let mut branches = vec![Vec::with_capacity(input.targets.len()); branch_values.len()];
    let mut fallback = Vec::with_capacity(input.targets.len());
    for target in input.targets {
        let target_id = function_conditional_target_coordinate(input.symbols, target, input.span)?;
        targets.push(target_id);
        for (lowered_branch, branch) in branches.iter_mut().zip(&branch_values) {
            lowered_branch.push(match branch.get_target(target) {
                Some(value) => value,
                None => construction
                    .functions(|functions| functions.read(body, target_id, provenance))?,
            });
        }
        fallback.push(
            match fallback_values
                .as_ref()
                .and_then(|values| values.get_target(target))
            {
                Some(value) => value,
                None => construction
                    .functions(|functions| functions.read(body, target_id, provenance))?,
            },
        );
    }
    Ok(LoweredFunctionConditional {
        targets,
        conditions,
        branches,
        fallback,
    })
}

struct ConditionalBranch<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    sequence: &'statement FunctionStatementSequence,
}

#[derive(Clone, Default)]
struct ConditionalBranchValues<'dae> {
    named: HashMap<VarName, dae::ExprId<'dae>>,
    record_fields: HashMap<FunctionRecordFieldIdentity, dae::ExprId<'dae>>,
}

impl<'dae> ConditionalBranchValues<'dae> {
    fn record_staging_scope<'scope>(
        &'scope self,
        symbols: FunctionSymbols<'scope, 'dae>,
    ) -> FunctionRecordStagingScope<'scope, 'dae> {
        symbols
            .record_staging_scope()
            .with_overrides(&self.record_fields)
    }

    fn get_assignment(&self, assignment: &FunctionAssignmentPlan) -> Option<dae::ExprId<'dae>> {
        assignment.record_field().map_or_else(
            || self.named.get(assignment.target()).copied(),
            |identity| self.record_fields.get(&identity).copied(),
        )
    }

    fn insert_assignment(&mut self, assignment: &FunctionAssignmentPlan, value: dae::ExprId<'dae>) {
        match assignment.record_field() {
            Some(identity) => {
                self.record_fields.insert(identity, value);
            }
            None => {
                self.record_fields
                    .retain(|identity, _| identity.target != assignment.target_def_id());
                self.named.insert(assignment.target().clone(), value);
            }
        }
    }

    fn get_target(&self, target: &FunctionConditionalTarget) -> Option<dae::ExprId<'dae>> {
        target.record_field.map_or_else(
            || self.named.get(&target.name).copied(),
            |identity| self.record_fields.get(&identity).copied(),
        )
    }

    fn insert_target(&mut self, target: &FunctionConditionalTarget, value: dae::ExprId<'dae>) {
        match target.record_field {
            Some(identity) => {
                self.record_fields.insert(identity, value);
            }
            None => {
                self.record_fields
                    .retain(|identity, _| identity.target != target.target_def_id);
                self.named.insert(target.name.clone(), value);
            }
        }
    }
}

/// Build the value every assignment of one branch leaves behind.
///
/// The environment shadows the enclosing body for exactly the values the branch
/// has already written, which is what keeps the source assignment order.
/// Expression IDs are shared DAG nodes, so keeping a branch-local
/// definition here neither repeats its evaluation nor leaks it outside the
/// conditional that owns it.
fn lower_conditional_branch<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: ConditionalBranch<'_, '_, 'dae>,
) -> Result<ConditionalBranchValues<'dae>, dae::DaeConstructionError> {
    let ConditionalBranch {
        symbols,
        binders,
        sequence,
    } = input;
    let mut values = ConditionalBranchValues::default();
    lower_conditional_statements(construction, body, symbols, binders, sequence, &mut values)?;
    Ok(values)
}

fn lower_conditional_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    sequence: &FunctionStatementSequence,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let mut record_staging_available = symbols.record_staging_available.clone();
    for product in sequence.products() {
        let current_symbols = symbols.with_record_staging_available(&record_staging_available);
        if lower_conditional_record_assembly(construction, body, current_symbols, product, values)?
        {
            record_staging_available.advance(product.plan());
            continue;
        }
        lower_one_conditional_statement(
            construction,
            body,
            current_symbols,
            binders,
            product,
            values,
        )?;
        record_staging_available.advance(product.plan());
    }
    Ok(())
}

fn lower_one_conditional_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    product: &FunctionStatementProduct,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let plan = product.plan();
    match (product.source(), plan) {
        (_, FunctionStatementPlan::ProvenAssertion) => Ok(()),
        (_, FunctionStatementPlan::RuntimeAssertion) => {
            Err(dae::DaeConstructionError::InvalidExpressionForm {
                span: product.span(),
            })
        }
        (
            [rumoca_core::Statement::Assignment { value, span, .. }],
            FunctionStatementPlan::Assignment(assignment),
        ) => lower_conditional_assignment(
            construction,
            body,
            ConditionalAssignment {
                symbols,
                binders,
                assignment,
                value,
                span: *span,
            },
            values,
        ),
        (
            [
                rumoca_core::Statement::FunctionCall {
                    comp, args, span, ..
                },
            ],
            FunctionStatementPlan::MultiOutputCall { outputs },
        ) => lower_conditional_multi_output_call(
            construction,
            body,
            symbols,
            binders,
            FunctionMultiOutputCall {
                callee: comp,
                args,
                span: *span,
                outputs,
            },
            values,
        ),
        (
            [_],
            FunctionStatementPlan::If {
                conditions,
                branches,
                fallback,
                targets,
                span,
            },
        ) => lower_nested_conditional(
            construction,
            body,
            NestedFunctionConditional {
                symbols,
                binders,
                conditions,
                branches,
                fallback: fallback.as_ref(),
                targets,
                span: *span,
            },
            values,
        ),
        ([_], FunctionStatementPlan::ProvenBranch { statements, .. }) => {
            lower_selected_conditional(construction, body, symbols, binders, statements, values)
        }
        _ => Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: product.span(),
        }),
    }
}

fn lower_conditional_record_assembly<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    product: &FunctionStatementProduct,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<bool, dae::DaeConstructionError> {
    let plan = product.plan();
    let FunctionStatementPlan::RecordAssembly(assembly) = plan else {
        return Ok(false);
    };
    let (_, record, _) =
        lower_function_record_value(construction, symbols, body, product.source(), assembly)?;
    values
        .record_fields
        .retain(|identity, _| identity.target != assembly.target_def_id);
    values.named.insert(assembly.target.clone(), record);
    Ok(true)
}

fn lower_selected_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    sequence: &FunctionStatementSequence,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    lower_conditional_statements(construction, body, symbols, binders, sequence, values)
}

fn lower_conditional_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    call: FunctionMultiOutputCall<'_>,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            record_staging: Some(values.record_staging_scope(symbols)),
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: Some(&values.named),
            owner_clock: None,
        },
        binders,
        call.callee,
        call.args,
        provenance,
    )?;
    let selected = call
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, output)| output.as_ref().map(|output| (ordinal, output)))
        .collect::<Vec<_>>();
    let results = operands.results(
        construction,
        selected.iter().map(|(ordinal, _)| *ordinal),
        provenance,
    )?;
    for ((_, output), mut value) in selected.into_iter().zip(results) {
        let target = function_assignment_coordinate(symbols, output, call.span)?;
        if !output.subscripts().is_empty() {
            let base = match values.get_assignment(output) {
                Some(value) => Some(value),
                None => output
                    .seed()
                    .map(|seed| lower_function_value_seed(construction, seed, call.span))
                    .transpose()?,
            };
            value = lower_function_array_update(
                construction,
                FunctionArrayUpdate {
                    symbols: LoweringSymbols {
                        coordinates: symbols.coordinates,
                        record_staging: Some(values.record_staging_scope(symbols)),
                        functions: symbols.functions,
                        shapes: symbols.shapes,
                        function_body: Some(body),
                        values: Some(&values.named),
                        owner_clock: None,
                    },
                    binders,
                    base,
                    target,
                    subscripts: output.subscripts(),
                    value,
                    provenance,
                },
            )?;
        }
        values.insert_assignment(output, value);
    }
    Ok(())
}

struct ConditionalAssignment<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    assignment: &'statement FunctionAssignmentPlan,
    value: &'statement Expression,
    span: Span,
}

fn lower_conditional_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: ConditionalAssignment<'_, '_, 'dae>,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let ConditionalAssignment {
        symbols,
        binders,
        assignment,
        value,
        span,
    } = input;
    let target = function_assignment_coordinate(symbols, assignment, span)?;
    let provenance = dae::DaeProvenance::source(span)?;
    let mut lowered = lower_expression_scoped(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            record_staging: Some(values.record_staging_scope(symbols)),
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(body),
            values: Some(&values.named),
            owner_clock: None,
        },
        binders,
        value,
        None,
    )?;
    let subscripts = assignment.subscripts();
    let base = conditional_assignment_base(construction, assignment, span, values)?;
    if !subscripts.is_empty() {
        lowered = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    record_staging: Some(values.record_staging_scope(symbols)),
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(body),
                    values: Some(&values.named),
                    owner_clock: None,
                },
                binders,
                base,
                target,
                subscripts,
                value: lowered,
                provenance,
            },
        )?;
    }
    values.insert_assignment(assignment, lowered);
    Ok(())
}

fn conditional_assignment_base<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    assignment: &FunctionAssignmentPlan,
    span: Span,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    if assignment.subscripts().is_empty() {
        return Ok(None);
    }
    if let Some(seed) = assignment.seed() {
        let seeded = lower_function_value_seed(construction, seed, span)?;
        values.insert_assignment(assignment, seeded);
    }
    Ok(values.get_assignment(assignment))
}

struct NestedFunctionConditional<'scope, 'statement, 'dae> {
    symbols: FunctionSymbols<'scope, 'dae>,
    binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    conditions: &'statement [Expression],
    branches: &'statement [FunctionStatementSequence],
    fallback: Option<&'statement FunctionStatementSequence>,
    targets: &'statement [FunctionConditionalTarget],
    span: Span,
}

fn lower_nested_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    input: NestedFunctionConditional<'_, '_, 'dae>,
    values: &mut ConditionalBranchValues<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let mut conditions = Vec::with_capacity(input.conditions.len());
    for condition in input.conditions {
        conditions.push(lower_expression_scoped(
            construction,
            LoweringSymbols {
                coordinates: input.symbols.coordinates,
                record_staging: Some(values.record_staging_scope(input.symbols)),
                functions: input.symbols.functions,
                shapes: input.symbols.shapes,
                function_body: Some(body),
                values: Some(&values.named),
                owner_clock: None,
            },
            input.binders,
            condition,
            None,
        )?);
    }
    let incoming = values.clone();
    let mut branch_values = Vec::with_capacity(input.branches.len());
    for sequence in input.branches {
        let mut branch = incoming.clone();
        lower_conditional_statements(
            construction,
            body,
            input.symbols,
            input.binders,
            sequence,
            &mut branch,
        )?;
        branch_values.push(branch);
    }
    let fallback_values = match input.fallback {
        Some(sequence) => {
            let mut branch = incoming.clone();
            lower_conditional_statements(
                construction,
                body,
                input.symbols,
                input.binders,
                sequence,
                &mut branch,
            )?;
            branch
        }
        None => incoming.clone(),
    };
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionConditionLowering, input.span)?;
    for target in input.targets {
        let target_id = function_conditional_target_coordinate(input.symbols, target, input.span)?;
        let mut arms = Vec::with_capacity(branch_values.len());
        for branch in &branch_values {
            arms.push(match branch.get_target(target) {
                Some(value) => value,
                None => conditional_incoming_value(
                    construction,
                    body,
                    target_id,
                    target,
                    &incoming,
                    provenance,
                )?,
            });
        }
        let fallback = match fallback_values.get_target(target) {
            Some(value) => value,
            None => conditional_incoming_value(
                construction,
                body,
                target_id,
                target,
                &incoming,
                provenance,
            )?,
        };
        let branches = conditions.iter().copied().zip(arms);
        let joined = construction.expressions(|expressions| {
            expressions.at(provenance).conditional(branches, fallback)
        })?;
        values.insert_target(target, joined);
    }
    Ok(())
}

fn conditional_incoming_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    target_id: dae::FunctionValueId<'dae>,
    target: &FunctionConditionalTarget,
    incoming: &ConditionalBranchValues<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match incoming.get_target(target) {
        Some(value) => Ok(value),
        None => construction.functions(|functions| functions.read(body, target_id, provenance)),
    }
}

/// Build the aggregate one element-wise function definition starts from.
///
/// MLS §12.4.4 gives an unwritten function value no initial value, so an
/// element write needs an aggregate of the declared shape to update. Analysis
/// only plans a seed once it has proven the algorithm writes every declared
/// element before anything reads the value, which makes the seed a dead value
/// rather than a default: the certificate, not the constant, carries the
/// meaning.
pub(super) fn lower_function_value_seed<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    seed: &FunctionValueSeed,
    span: Span,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let provenance =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, span)?;
    lower_seed_value(construction, seed, provenance).map(|(_, value)| value)
}

fn lower_seed_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    seed: &FunctionValueSeed,
    provenance: dae::DaeProvenance,
) -> Result<(dae::ValueTypeId<'dae>, dae::ExprId<'dae>), dae::DaeConstructionError> {
    match seed {
        FunctionValueSeed::Scalar { dimensions, scalar } => {
            let value_type = construction.types(|types| {
                types.derived(
                    dae::ValueType::array(*scalar, dimensions.clone()),
                    provenance,
                )
            })?;
            let element = match scalar {
                dae::ScalarType::Real => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(0.0))
                })?,
                dae::ScalarType::Integer => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Integer(0))
                })?,
                dae::ScalarType::Enumeration => construction
                    .expressions(|expressions| expressions.at(provenance).enumeration_literal(1))?,
                dae::ScalarType::Boolean => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Boolean(false))
                })?,
                dae::ScalarType::String => construction.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::String(String::new()))
                })?,
                dae::ScalarType::Record => {
                    return Err(dae::DaeConstructionError::InvalidExpressionForm {
                        span: provenance.span(),
                    });
                }
            };
            let value = lower_seed_array(construction, dimensions, element, provenance)?;
            Ok((value_type, value))
        }
        FunctionValueSeed::Record {
            name,
            dimensions,
            fields,
        } => {
            let fields = fields
                .iter()
                .map(|(name, seed)| {
                    lower_seed_value(construction, seed, provenance)
                        .map(|(value_type, value)| (name.clone(), value_type, value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let scalar_type = construction.types(|types| {
                types.record(
                    name.clone(),
                    fields
                        .iter()
                        .map(|(name, value_type, _)| (name.clone(), *value_type)),
                    provenance,
                )
            })?;
            let record = construction.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .record(scalar_type, fields.iter().map(|(_, _, value)| *value))
            })?;
            let value = lower_seed_array(construction, dimensions, record, provenance)?;
            let value_type = construction.types(|types| {
                types.record_array(
                    name.clone(),
                    fields
                        .iter()
                        .map(|(name, value_type, _)| (name.clone(), *value_type)),
                    dimensions.clone(),
                    provenance,
                )
            })?;
            Ok((value_type, value))
        }
    }
}

fn lower_seed_array<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    dimensions: &[u32],
    element: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if dimensions.is_empty() {
        return Ok(element);
    }
    let binders = dimensions
        .iter()
        .zip(0u32..)
        .map(|(extent, ordinal)| StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(ordinal),
            display_name: format!("seed{ordinal}"),
            lower: 1,
            upper: i64::from(*extent),
            step: 1,
        })
        .collect::<Vec<_>>();
    let domain = construction
        .domains(|domains| domains.structured(StructuredIndexDomain { binders }, provenance))?;
    construction
        .expressions(|expressions| expressions.at(provenance).comprehension(domain, element))
}

pub(super) struct TotalArrayDefinition<'scope, 'statement, 'dae> {
    pub(super) symbols: FunctionSymbols<'scope, 'dae>,
    pub(super) domain: dae::DomainId<'dae>,
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) statements: &'statement FunctionStatementSequence,
    pub(super) owner: dae::DaeProvenance,
}

pub(super) fn lower_total_function_array_definition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: TotalArrayDefinition<'_, '_, 'dae>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    if input
        .statements
        .products()
        .iter()
        .any(|product| matches!(product.plan(), FunctionStatementPlan::RuntimeAssertion))
    {
        let mut loop_body = construction
            .functions(|functions| functions.begin_loop(body, input.domain, [], input.owner))?;
        lower_total_function_assertions(construction, &input, &mut loop_body)?;
        body = construction.functions(|functions| functions.finish_loop(loop_body, input.owner))?;
    }
    for product in input.statements.products() {
        if matches!(product.plan(), FunctionStatementPlan::Assignment(_)) {
            lower_one_total_function_array_definition(construction, &mut body, &input, product)?;
        }
    }
    Ok(body)
}

fn lower_total_function_assertions<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    input: &TotalArrayDefinition<'_, '_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    for product in input.statements.products() {
        if !matches!(product.plan(), FunctionStatementPlan::RuntimeAssertion) {
            continue;
        }
        let assertion = product.runtime_assertion().ok_or(
            dae::DaeConstructionError::InvalidExpressionForm {
                span: product.span(),
            },
        )?;
        let condition = lower_function_expression_scoped(
            construction,
            FunctionExpressionValues {
                coordinates: input.symbols.coordinates,
                record_staging: Some(input.symbols.record_staging_scope()),
            },
            input.symbols.functions,
            input.symbols.shapes,
            loop_body.body(),
            input.binders,
            &assertion.condition,
        )?;
        let message = lower_function_expression_scoped(
            construction,
            FunctionExpressionValues {
                coordinates: input.symbols.coordinates,
                record_staging: Some(input.symbols.record_staging_scope()),
            },
            input.symbols.functions,
            input.symbols.shapes,
            loop_body.body(),
            input.binders,
            &assertion.message,
        )?;
        let provenance = dae::DaeProvenance::source(assertion.span)?;
        construction.functions(|functions| {
            functions.assertion_loop(loop_body, condition, message, provenance)
        })?;
    }
    Ok(())
}

fn lower_one_total_function_array_definition<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    input: &TotalArrayDefinition<'_, '_, 'dae>,
    product: &FunctionStatementProduct,
) -> Result<(), dae::DaeConstructionError> {
    let (
        [rumoca_core::Statement::Assignment { value, span, .. }],
        FunctionStatementPlan::Assignment(assignment),
    ) = (product.source(), product.plan())
    else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: product.span(),
        });
    };
    let element = lower_function_expression_scoped(
        construction,
        FunctionExpressionValues {
            coordinates: input.symbols.coordinates,
            record_staging: Some(input.symbols.record_staging_scope()),
        },
        input.symbols.functions,
        input.symbols.shapes,
        body,
        input.binders,
        value,
    )?;
    let generated = dae::DaeProvenance::generated(
        dae::DaeGeneration::FunctionLoopLowering,
        input.owner.span(),
    )?;
    let array = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .comprehension(input.domain, element)
    })?;
    let provenance = dae::DaeProvenance::source(*span)?;
    let target = function_assignment_coordinate(input.symbols, assignment, *span)?;
    construction.functions(|functions| functions.assign(body, target, array, provenance))
}

pub(super) struct FunctionFold<'scope, 'statement, 'dae> {
    pub(super) domain: dae::DomainId<'dae>,
    pub(super) binders: &'scope HashMap<VarName, dae::DomainBinderId<'dae>>,
    pub(super) statements: &'statement FunctionStatementSequence,
    pub(super) targets: &'statement [VarName],
    pub(super) iteration_locals: &'statement [VarName],
    pub(super) owner: dae::DaeProvenance,
}

pub(super) fn lower_function_fold<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut body: dae::FunctionBody<'dae>,
    input: FunctionFold<'_, '_, 'dae>,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let mut seeds = Vec::new();
    collect_function_sequence_seeds(input.statements, &mut seeds);
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::FunctionAggregateLowering,
        input.owner.span(),
    )?;
    for (target, seed) in seeds {
        let seeded = lower_function_value_seed(construction, seed, input.owner.span())?;
        let target = function_value_coordinate(symbols.coordinates, target, input.owner.span())?;
        construction
            .functions(|functions| functions.assign(&mut body, target, seeded, provenance))?;
    }
    let target_ids = input
        .targets
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target, input.owner.span()))
        .collect::<Result<Vec<_>, _>>()?;
    let iteration_local_ids = input
        .iteration_locals
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target, input.owner.span()))
        .collect::<Result<Vec<_>, _>>()?;
    let mut loop_body = construction.functions(|functions| {
        functions.begin_loop_with_iteration_locals(
            body,
            input.domain,
            target_ids,
            iteration_local_ids,
            input.owner,
        )
    })?;
    loop_body = lower_function_loop_statements(
        construction,
        symbols,
        loop_body,
        input.binders,
        input.statements,
    )?;
    construction.functions(|functions| functions.finish_loop(loop_body, input.owner))
}

fn lower_function_loop_statements<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut loop_body: dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    sequence: &FunctionStatementSequence,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let mut record_staging_available = symbols.record_staging_available.clone();
    for product in sequence.products() {
        let current_symbols = symbols.with_record_staging_available(&record_staging_available);
        if let FunctionStatementPlan::RecordAssembly(assembly) = product.plan() {
            lower_function_loop_record_assembly(
                construction,
                current_symbols,
                &mut loop_body,
                product.source(),
                assembly,
            )?;
            record_staging_available.advance(product.plan());
            continue;
        }
        loop_body = lower_one_function_loop_statement(
            construction,
            current_symbols,
            loop_body,
            binders,
            product,
        )?;
        record_staging_available.advance(product.plan());
    }
    Ok(loop_body)
}

fn lower_one_function_loop_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut loop_body: dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    product: &FunctionStatementProduct,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let plan = product.plan();
    if matches!(plan, FunctionStatementPlan::ProvenAssertion) {
        return Ok(loop_body);
    }
    if matches!(plan, FunctionStatementPlan::RuntimeAssertion) {
        lower_function_loop_assertion(construction, symbols, &mut loop_body, binders, product)?;
        return Ok(loop_body);
    }
    if lower_loop_multi_output_statement(construction, symbols, &mut loop_body, binders, product)? {
        return Ok(loop_body);
    }
    if matches!(plan, FunctionStatementPlan::For { .. }) {
        return lower_nested_function_loop(construction, symbols, loop_body, binders, product);
    }
    if let FunctionStatementPlan::If {
        conditions,
        branches,
        fallback,
        targets,
        span,
    } = plan
    {
        return lower_loop_conditional(
            construction,
            symbols,
            loop_body,
            FunctionConditional {
                symbols,
                binders,
                conditions,
                branches,
                fallback: fallback.as_ref(),
                targets,
                span: *span,
            },
        );
    }
    if let FunctionStatementPlan::ProvenBranch { statements, .. } = plan {
        return lower_function_loop_statements(
            construction,
            symbols,
            loop_body,
            binders,
            statements,
        );
    }
    lower_function_loop_assignment(construction, symbols, &mut loop_body, binders, product)?;
    Ok(loop_body)
}

fn lower_loop_multi_output_statement<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    product: &FunctionStatementProduct,
) -> Result<bool, dae::DaeConstructionError> {
    let (
        [
            rumoca_core::Statement::FunctionCall {
                comp, args, span, ..
            },
        ],
        FunctionStatementPlan::MultiOutputCall { outputs },
    ) = (product.source(), product.plan())
    else {
        return Ok(false);
    };
    lower_function_loop_multi_output_call(
        construction,
        symbols,
        loop_body,
        binders,
        FunctionMultiOutputCall {
            callee: comp,
            args,
            span: *span,
            outputs,
        },
    )?;
    Ok(true)
}

fn lower_loop_conditional<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    mut loop_body: dae::FunctionLoop<'dae>,
    conditional: FunctionConditional<'_, '_, 'dae>,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::generated(
        dae::DaeGeneration::FunctionConditionLowering,
        conditional.span,
    )?;
    let lowered = lower_function_conditional_values(
        construction,
        loop_body.body(),
        FunctionConditional {
            symbols,
            ..conditional
        },
    )?;
    construction.functions(|functions| {
        functions.assign_conditional_all_loop(
            &mut loop_body,
            &lowered.targets,
            &lowered.conditions,
            &lowered.branches,
            &lowered.fallback,
            provenance,
        )
    })?;
    Ok(loop_body)
}

fn lower_function_loop_multi_output_call<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    call: FunctionMultiOutputCall<'_>,
) -> Result<(), dae::DaeConstructionError> {
    let provenance = dae::DaeProvenance::source(call.span)?;
    let operands = lower_call_operands(
        construction,
        LoweringSymbols {
            coordinates: symbols.coordinates,
            record_staging: Some(symbols.record_staging_scope()),
            functions: symbols.functions,
            shapes: symbols.shapes,
            function_body: Some(loop_body.body()),
            values: None,
            owner_clock: None,
        },
        binders,
        call.callee,
        call.args,
        provenance,
    )?;
    let selected = call
        .outputs
        .iter()
        .enumerate()
        .filter_map(|(ordinal, output)| output.as_ref().map(|output| (ordinal, output)))
        .collect::<Vec<_>>();
    let results = operands.results(
        construction,
        selected.iter().map(|(ordinal, _)| *ordinal),
        provenance,
    )?;
    for ((_, output), mut value) in selected.into_iter().zip(results) {
        let target = function_assignment_coordinate(symbols, output, call.span)?;
        if !output.subscripts().is_empty() {
            value = lower_function_array_update(
                construction,
                FunctionArrayUpdate {
                    symbols: LoweringSymbols {
                        coordinates: symbols.coordinates,
                        record_staging: Some(symbols.record_staging_scope()),
                        functions: symbols.functions,
                        shapes: symbols.shapes,
                        function_body: Some(loop_body.body()),
                        values: None,
                        owner_clock: None,
                    },
                    binders,
                    base: None,
                    target,
                    subscripts: output.subscripts(),
                    value,
                    provenance,
                },
            )?;
        }
        construction
            .functions(|functions| functions.assign_loop(loop_body, target, value, provenance))?;
    }
    Ok(())
}

fn lower_nested_function_loop<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    parent: dae::FunctionLoop<'dae>,
    enclosing_binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    product: &FunctionStatementProduct,
) -> Result<dae::FunctionLoop<'dae>, dae::DaeConstructionError> {
    let FunctionStatementPlan::For {
        domain,
        binder_spans,
        lowering,
        statements,
        indices,
        span,
    } = product.plan()
    else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: product.span(),
        });
    };
    let owner = dae::DaeProvenance::source(*span)?;
    let domain_provenance = match binder_spans.as_slice() {
        [span] => dae::DaeProvenance::source(*span)?,
        _ => owner,
    };
    let child_domain = construction
        .domains(|domains| domains.nested(parent.domain(), domain.clone(), domain_provenance))?;
    let child_indices = indices.iter().collect::<Vec<_>>();
    let child_binders =
        lower_function_binders(construction, child_domain, &child_indices, binder_spans)?;
    let mut binders = enclosing_binders.clone();
    binders.extend(child_binders);
    let mut child_shapes = symbols.shapes.clone();
    for binder in &domain.binders {
        child_shapes.bind_integer_bounds(
            VarName::new(&binder.display_name),
            binder.lower.min(binder.upper),
            binder.lower.max(binder.upper),
        );
    }
    let child_symbols = FunctionSymbols {
        coordinates: symbols.coordinates,
        function_values: symbols.function_values,
        record_staging: symbols.record_staging,
        record_staging_available: symbols.record_staging_available,
        functions: symbols.functions,
        shapes: &child_shapes,
    };
    let FunctionLoopLowering::Fold {
        targets,
        iteration_locals,
    } = lowering
    else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: product.span(),
        });
    };
    let target_ids = targets
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target, product.span()))
        .collect::<Result<Vec<_>, _>>()?;
    let iteration_local_ids = iteration_locals
        .iter()
        .map(|target| function_value_coordinate(symbols.coordinates, target, product.span()))
        .collect::<Result<Vec<_>, _>>()?;
    let child = construction.functions(|functions| {
        functions.begin_nested_loop_with_iteration_locals(
            parent,
            child_domain,
            target_ids,
            iteration_local_ids,
            owner,
        )
    })?;
    let child =
        lower_function_loop_statements(construction, child_symbols, child, &binders, statements)?;
    construction.functions(|functions| functions.finish_nested_loop(child, owner))
}

fn lower_function_loop_assertion<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    product: &FunctionStatementProduct,
) -> Result<(), dae::DaeConstructionError> {
    let assertion =
        product
            .runtime_assertion()
            .ok_or(dae::DaeConstructionError::InvalidExpressionForm {
                span: product.span(),
            })?;
    let condition = lower_function_expression_scoped(
        construction,
        FunctionExpressionValues {
            coordinates: symbols.coordinates,
            record_staging: Some(symbols.record_staging_scope()),
        },
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        &assertion.condition,
    )?;
    let message = lower_function_expression_scoped(
        construction,
        FunctionExpressionValues {
            coordinates: symbols.coordinates,
            record_staging: Some(symbols.record_staging_scope()),
        },
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        &assertion.message,
    )?;
    let provenance = dae::DaeProvenance::source(assertion.span)?;
    construction
        .functions(|functions| functions.assertion_loop(loop_body, condition, message, provenance))
}

fn lower_function_loop_assignment<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: FunctionSymbols<'_, 'dae>,
    loop_body: &mut dae::FunctionLoop<'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    product: &FunctionStatementProduct,
) -> Result<(), dae::DaeConstructionError> {
    let (
        [rumoca_core::Statement::Assignment { value, span, .. }],
        FunctionStatementPlan::Assignment(assignment),
    ) = (product.source(), product.plan())
    else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm {
            span: product.span(),
        });
    };
    let target = function_assignment_coordinate(symbols, assignment, *span)?;
    let mut value = lower_function_expression_scoped(
        construction,
        FunctionExpressionValues {
            coordinates: symbols.coordinates,
            record_staging: Some(symbols.record_staging_scope()),
        },
        symbols.functions,
        symbols.shapes,
        loop_body.body(),
        binders,
        value,
    )?;
    let provenance = dae::DaeProvenance::source(*span)?;
    let subscripts = assignment.subscripts();
    if !subscripts.is_empty() {
        value = lower_function_array_update(
            construction,
            FunctionArrayUpdate {
                symbols: LoweringSymbols {
                    coordinates: symbols.coordinates,
                    record_staging: Some(symbols.record_staging_scope()),
                    functions: symbols.functions,
                    shapes: symbols.shapes,
                    function_body: Some(loop_body.body()),
                    values: None,
                    owner_clock: None,
                },
                binders,
                // Analysis proves a loop-carried value already owns every
                // element, so the transition updates its current value.
                base: None,
                target,
                subscripts,
                value,
                provenance,
            },
        )?;
    }
    construction.functions(|functions| functions.assign_loop(loop_body, target, value, provenance))
}

pub(super) fn function_value_coordinate<'dae>(
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    target: &VarName,
    span: Span,
) -> Result<dae::FunctionValueId<'dae>, dae::DaeConstructionError> {
    let Some(Coordinate::FunctionValue(target)) = coordinates.get(target).copied() else {
        return Err(dae::DaeConstructionError::InvalidExpressionForm { span });
    };
    Ok(target)
}

pub(super) fn function_assignment_coordinate<'dae>(
    symbols: FunctionSymbols<'_, 'dae>,
    assignment: &FunctionAssignmentPlan,
    span: Span,
) -> Result<dae::FunctionValueId<'dae>, dae::DaeConstructionError> {
    let Some(identity) = assignment.record_field() else {
        return function_value_coordinate(symbols.coordinates, assignment.target(), span);
    };
    match symbols.record_staging.get(identity) {
        Some(FunctionRecordStagedValue::Local(local)) => Ok(local),
        Some(FunctionRecordStagedValue::Expression(_)) | None => {
            Err(dae::DaeConstructionError::InvalidExpressionForm { span })
        }
    }
}

fn function_conditional_target_coordinate<'dae>(
    symbols: FunctionSymbols<'_, 'dae>,
    target: &FunctionConditionalTarget,
    span: Span,
) -> Result<dae::FunctionValueId<'dae>, dae::DaeConstructionError> {
    let Some(identity) = target.record_field else {
        return function_value_coordinate(symbols.coordinates, &target.name, span);
    };
    match symbols.record_staging.get(identity) {
        Some(FunctionRecordStagedValue::Local(local)) => Ok(local),
        Some(FunctionRecordStagedValue::Expression(_)) | None => {
            Err(dae::DaeConstructionError::InvalidExpressionForm { span })
        }
    }
}
