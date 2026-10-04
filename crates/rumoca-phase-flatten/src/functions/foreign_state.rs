//! Hidden foreign state threaded through explicit values (SPEC_0040 FLAT-C05).
//!
//! MLS 3.7 §12.3 lets an impure function have side effects, and §12.9 gives an
//! external function the meaning of its foreign body. Some foreign bodies keep
//! state between calls in library storage (`ModelicaRandom.c` keeps its impure
//! generator that way). The executable IR has no hidden storage, so that state
//! becomes an explicit Integer vector, the cell, here, once: every function
//! that reaches a stateful entry point is specialized to take the cell as its
//! last input and return the updated cell as its last output, each call passes
//! the cell along in statement order, and the model holds the cell as one
//! discrete variable that the single algorithm section drawing from it
//! updates. A parameter binding that initializes the library state becomes
//! the cell's start value. The specialized functions are pure: their effect
//! is a data dependence, so re-evaluating an event iteration reads the cell's
//! `pre` value again instead of advancing the state twice.
//!
//! Any position that would make the order of state accesses unspecified (two
//! algorithm sections, an equation, an expression operand) is refused.

use super::*;
use rumoca_core::native_body::{ForeignStateCell, NativeBody};
use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, Expression, FallibleExpressionRewriter,
    FallibleStatementRewriter, Literal, Reference, Span, Statement, VarName,
};
use std::collections::{BTreeMap, BTreeSet};

/// One cell with the declaration that owns it.
#[derive(Clone, Copy)]
struct Cell {
    cell: ForeignStateCell,
    /// The stateful external declaration whose library state this is.
    owner: DefId,
    span: Span,
}

/// The specialization of one function that reaches stateful foreign code.
struct Threaded {
    name: VarName,
    instance_id: rumoca_core::FunctionInstanceId,
    /// Declared outputs before the cell outputs.
    outputs: usize,
    cells: Vec<ForeignStateCell>,
}

pub(crate) fn thread_foreign_state(flat: &mut flat::Model) -> Result<(), FlattenError> {
    let cells = stateful_entry_points(flat)?;
    if cells.is_empty() {
        return Ok(());
    }
    let reached = reaching_functions(flat, &cells)?;
    let first_instance = flat
        .functions
        .values()
        .filter_map(|function| function.instance_id)
        .map(|id| id.index())
        .max()
        .map_or(0, |max| max + 1);
    let mut threaded = IndexMap::default();
    for (offset, (name, touched)) in (0u32..).zip(&reached) {
        let function = &flat.functions[name];
        let specialized = fresh_function_name(flat, &format!("{name}__foreign_state"));
        threaded.insert(
            name.clone(),
            Threaded {
                name: specialized,
                instance_id: rumoca_core::FunctionInstanceId::new(first_instance + offset),
                outputs: function.outputs.len(),
                cells: touched.iter().copied().collect(),
            },
        );
    }
    let owners = cells
        .values()
        .map(|cell| (cell.cell, *cell))
        .collect::<BTreeMap<_, _>>();
    let mut created = Vec::new();
    for (name, plan) in &threaded {
        created.push(thread_function(
            flat.predefined_types.integer,
            &flat.functions[name],
            plan,
            &threaded,
            &owners,
        )?);
    }
    for function in created {
        flat.functions.insert(function.name.clone(), function);
    }
    thread_model(flat, &threaded, &owners)
}

/// Stateful entry points: C declarations whose entry point is a catalog row
/// with hidden state and whose declared argument count matches the row.
/// The DAE proves the threaded interface exactly (SPEC_0040 DAE-C30).
fn stateful_entry_points(flat: &flat::Model) -> Result<IndexMap<VarName, Cell>, FlattenError> {
    let mut cells = IndexMap::default();
    for (name, function) in &flat.functions {
        let Some(external) = function.external.as_ref() else {
            continue;
        };
        let Some(access) = external
            .function_name
            .as_deref()
            .filter(|_| external.language == "C")
            .and_then(NativeBody::from_c_entry_point)
            .and_then(NativeBody::foreign_state)
        else {
            continue;
        };
        if external.args.len() != access.declared.len()
            || external.output_name.is_some() != access.declared_return.is_some()
        {
            continue;
        }
        let owner = function.def_id.ok_or_else(|| {
            FlattenError::unordered_foreign_state(
                format!("`{name}` has no declaration identity to own its library state"),
                function.span,
            )
        })?;
        cells.insert(
            name.clone(),
            Cell {
                cell: access.cell,
                owner,
                span: function.span,
            },
        );
    }
    Ok(cells)
}

/// Every function that reaches a stateful entry point, with the cells it
/// reaches, in Flat function order.
fn reaching_functions(
    flat: &flat::Model,
    entries: &IndexMap<VarName, Cell>,
) -> Result<IndexMap<VarName, BTreeSet<ForeignStateCell>>, FlattenError> {
    let mut reached = entries
        .iter()
        .map(|(name, cell)| (name.clone(), BTreeSet::from([cell.cell])))
        .collect::<IndexMap<_, _>>();
    loop {
        let mut changed = false;
        for (name, function) in &flat.functions {
            if function.external.is_some() {
                continue;
            }
            let mut callees = CalleeCollector::default();
            callees.rewrite_statements(&function.body)?;
            let touched = callees
                .names
                .iter()
                .filter_map(|callee| reached.get(callee))
                .flatten()
                .copied()
                .collect::<BTreeSet<_>>();
            if touched.is_empty() {
                continue;
            }
            let entry = reached.entry(name.clone()).or_default();
            let before = entry.len();
            entry.extend(touched);
            changed |= entry.len() != before;
        }
        if !changed {
            break;
        }
    }
    Ok(flat
        .functions
        .keys()
        .filter_map(|name| reached.get(name).map(|cells| (name.clone(), cells.clone())))
        .collect())
}

#[derive(Default)]
struct CalleeCollector {
    names: HashSet<VarName>,
}

impl FallibleExpressionRewriter for CalleeCollector {
    type Error = FlattenError;

    fn rewrite_expression(&mut self, expression: &Expression) -> Result<Expression, FlattenError> {
        if let Expression::FunctionCall { name, .. } = expression {
            self.names.insert(name.var_name().clone());
        }
        self.walk_expression(expression)
    }
}

impl FallibleStatementRewriter for CalleeCollector {
    fn rewrite_statement(&mut self, statement: &Statement) -> Result<Statement, FlattenError> {
        if let Statement::FunctionCall { comp, .. } = statement {
            self.names.insert(comp.var_name().clone());
        }
        self.walk_statement(statement)
    }
}

fn fresh_function_name(flat: &flat::Model, base: &str) -> VarName {
    let mut name = VarName::new(base);
    let mut suffix = 2;
    while flat.functions.contains_key(&name) {
        name = VarName::new(format!("{base}__{suffix}"));
        suffix += 1;
    }
    name
}

fn cell_input_name(cell: ForeignStateCell) -> String {
    format!("{}_in", cell.name())
}

fn integer_vector_param(
    flat_integer: rumoca_core::TypeId,
    name: String,
    extent: u32,
    span: Span,
) -> Result<rumoca_core::FunctionParam, FlattenError> {
    let effective =
        rumoca_core::EffectiveType::new(flat_integer, flat_integer, [i64::from(extent)]).map_err(
            |error| FlattenError::internal(format!("foreign state cell type: {error:?}")),
        )?;
    Ok(
        rumoca_core::FunctionParam::new(name, "Integer", effective, span)
            .with_shape_expr(vec![rumoca_core::Subscript::index(i64::from(extent), span)]),
    )
}

fn integer_literal(value: i64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span,
    }
}

fn var_ref(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span,
    }
}

fn target(name: &str, owner: DefId, span: Span) -> Result<ComponentReference, FlattenError> {
    ComponentReference::construct(
        false,
        span,
        vec![ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
            def_id: owner,
        }],
    )
    .map_err(|error| FlattenError::internal(format!("foreign state cell reference: {error}")))
}

/// The value the library storage of `cell` holds before any call.
fn initial_cell(cell: ForeignStateCell, span: Span) -> Expression {
    Expression::Array {
        elements: cell
            .initial_value()
            .into_iter()
            .map(|value| integer_literal(value, span))
            .collect(),
        kind: rumoca_core::ArrayConstructor::Array,
        span,
    }
}

/// The reference to `plan`'s specialization at a call of `original`. The
/// structured reference keeps the call's resolved path with the
/// specialization suffix on its final part.
fn threaded_reference(plan: &Threaded, original: &Reference) -> Reference {
    let suffix = plan
        .name
        .as_str()
        .strip_prefix(original.var_name().as_str())
        .unwrap_or_default();
    let structured = original.component_ref().and_then(|component| {
        let mut parts = component.parts().to_vec();
        let last = parts.last_mut()?;
        last.ident.push_str(suffix);
        component.with_replaced_parts(parts).ok()
    });
    let reference = match structured {
        Some(component) => Reference::with_component_reference(plan.name.as_str(), component),
        None => Reference::from_var_name(plan.name.clone()),
    };
    reference.with_resolved_function(rumoca_core::ResolvedFunctionReference {
        instance_id: plan.instance_id,
        base_part_count: original
            .resolved_function()
            .map_or(0, |resolved| resolved.base_part_count),
        transitively_non_replaceable: original
            .resolved_function()
            .is_some_and(|resolved| resolved.transitively_non_replaceable),
    })
}

/// Build the specialization of one reaching function.
fn thread_function(
    integer: rumoca_core::TypeId,
    function: &rumoca_core::Function,
    plan: &Threaded,
    threaded: &IndexMap<VarName, Threaded>,
    owners: &BTreeMap<ForeignStateCell, Cell>,
) -> Result<rumoca_core::Function, FlattenError> {
    let mut specialized = function.clone();
    specialized.name = plan.name.clone();
    specialized.instance_id = Some(plan.instance_id);
    specialized.pure = true;
    specialized.purity_declared = true;
    for cell in &plan.cells {
        let owner = owners[cell];
        let input =
            integer_vector_param(integer, cell_input_name(*cell), cell.extent(), owner.span)?;
        let mut output =
            integer_vector_param(integer, cell.name().to_string(), cell.extent(), owner.span)?;
        if function.external.is_none() {
            output.default = Some(var_ref(&input.name, owner.span));
        }
        specialized.inputs.push(input);
        specialized.outputs.push(output);
    }
    if let Some(external) = specialized.external.as_mut() {
        // The threaded interface: the declared arguments, the return-form
        // output as an output argument, then each cell's input and output.
        if let Some(result) = external.output_name.take() {
            external.args.push(var_ref(&result, function.span));
        }
        for cell in &plan.cells {
            external
                .args
                .push(var_ref(&cell_input_name(*cell), function.span));
            external.args.push(var_ref(cell.name(), function.span));
        }
        return Ok(specialized);
    }
    let mut threader = Threader {
        threaded,
        cells: plan
            .cells
            .iter()
            .map(|cell| (*cell, owners[cell]))
            .collect(),
        touched: BTreeSet::new(),
        context: ReachingContext::FunctionBody {
            declared_pure: function.pure && function.purity_declared,
        },
    };
    specialized.body = threader.rewrite_statements(&function.body)?;
    Ok(specialized)
}

/// Rewrites calls of reaching functions into calls of their specializations
/// that pass each cell along in statement order.
struct Threader<'a> {
    threaded: &'a IndexMap<VarName, Threaded>,
    /// The cells in scope, with the declaration that owns each.
    cells: Vec<(ForeignStateCell, Cell)>,
    /// The cells a rewritten statement passed along.
    touched: BTreeSet<ForeignStateCell>,
    /// Where the statements being rewritten execute.
    context: ReachingContext,
}

/// The statement context of a reaching call. Every reaching function reaches
/// stateful foreign code, so MLS 3.7 §12.3 treats it as impure (applied
/// recursively, FUNC-032) whatever its written prefix. The specializations
/// are pure, so the FUNC-022 call-context proof for these calls is made here,
/// before threading hides it from the DAE's proof.
#[derive(Clone, Copy)]
enum ReachingContext {
    /// A model algorithm section: a call is admitted only in a `when`
    /// statement, which executes once per event; elsewhere the section runs
    /// an unspecified number of times.
    ModelAlgorithm { in_when: bool },
    /// The body of a reaching function, which may not be declared `pure`.
    FunctionBody { declared_pure: bool },
}

impl Threader<'_> {
    /// Refuse a reaching call where MLS 3.7 §12.3 forbids an impure call or
    /// where its state accesses run an unspecified number of times.
    fn admit_call(&self, callee: &Reference, span: Span) -> Result<(), FlattenError> {
        let refused = match self.context {
            ReachingContext::ModelAlgorithm { in_when } => (!in_when).then_some(
                "an algorithm statement outside a `when` statement, which runs an unspecified \
                 number of times",
            ),
            ReachingContext::FunctionBody { declared_pure } => {
                declared_pure.then_some("a function body declared `pure`")
            }
        };
        match refused {
            Some(context) => Err(FlattenError::unordered_foreign_state(
                format!(
                    "`{callee}` reaches library state, so MLS 3.7 §12.3 treats it as impure, and \
                     it is called from {context}"
                ),
                span,
            )),
            None => Ok(()),
        }
    }

    /// One threaded call statement assigning `receivers` (declared outputs,
    /// in order) and every cell the callee reaches.
    fn threaded_call(
        &mut self,
        callee: &Reference,
        args: &[Expression],
        mut receivers: Vec<Option<ComponentReference>>,
        span: Span,
    ) -> Result<Statement, FlattenError> {
        self.admit_call(callee, span)?;
        let plan = &self.threaded[callee.var_name()];
        let mut args = args
            .iter()
            .map(|argument| self.rewrite_expression(argument))
            .collect::<Result<Vec<_>, _>>()?;
        receivers.resize(plan.outputs, None);
        for cell in &plan.cells {
            let owner = self
                .cells
                .iter()
                .find(|(candidate, _)| candidate == cell)
                .map(|(_, owner)| *owner)
                .ok_or_else(|| {
                    FlattenError::unordered_foreign_state(
                        format!("`{callee}` reaches library state outside its caller's cells"),
                        span,
                    )
                })?;
            args.push(var_ref(cell.name(), span));
            receivers.push(Some(target(cell.name(), owner.owner, span)?));
            self.touched.insert(*cell);
        }
        Ok(Statement::FunctionCall {
            comp: threaded_reference(plan, callee),
            args,
            outputs: receivers,
            span,
        })
    }
}

impl FallibleExpressionRewriter for Threader<'_> {
    type Error = FlattenError;

    fn rewrite_expression(&mut self, expression: &Expression) -> Result<Expression, FlattenError> {
        if let Expression::FunctionCall { name, span, .. } = expression
            && self.threaded.contains_key(name.var_name())
        {
            return Err(FlattenError::unordered_foreign_state(
                format!(
                    "`{name}` reaches library state from inside an expression, where the order \
                     of its state accesses is unspecified; call it as a statement"
                ),
                *span,
            ));
        }
        self.walk_expression(expression)
    }
}

impl FallibleStatementRewriter for Threader<'_> {
    fn rewrite_statement(&mut self, statement: &Statement) -> Result<Statement, FlattenError> {
        match statement {
            Statement::Assignment {
                comp,
                value: Expression::FunctionCall { name, args, .. },
                span,
            } if self.threaded.contains_key(name.var_name()) => {
                self.threaded_call(name, args, vec![Some(comp.clone())], *span)
            }
            Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } if self.threaded.contains_key(comp.var_name()) => {
                self.threaded_call(comp, args, outputs.clone(), *span)
            }
            Statement::When { .. } => {
                let outer = self.context;
                if let ReachingContext::ModelAlgorithm { .. } = outer {
                    self.context = ReachingContext::ModelAlgorithm { in_when: true };
                }
                let rewritten = self.walk_statement(statement);
                self.context = outer;
                rewritten
            }
            _ => self.walk_statement(statement),
        }
    }
}

/// Thread the cells through the model: the one algorithm section reaching
/// each cell, the parameter binding initializing it, and its variable.
fn thread_model(
    flat: &mut flat::Model,
    threaded: &IndexMap<VarName, Threaded>,
    owners: &BTreeMap<ForeignStateCell, Cell>,
) -> Result<(), FlattenError> {
    let all_cells = owners
        .iter()
        .map(|(cell, owner)| (*cell, *owner))
        .collect::<Vec<_>>();
    let mut drawn: BTreeMap<ForeignStateCell, Span> = BTreeMap::new();
    for algorithm in &mut flat.algorithms {
        let mut threader = Threader {
            threaded,
            cells: all_cells.clone(),
            touched: BTreeSet::new(),
            context: ReachingContext::ModelAlgorithm { in_when: false },
        };
        algorithm.statements = threader.rewrite_statements(&algorithm.statements)?;
        for cell in threader.touched {
            if drawn.insert(cell, algorithm.span).is_some() {
                return Err(FlattenError::unordered_foreign_state(
                    format!(
                        "two algorithm sections reach the library state `{}`, so the order of \
                         its accesses is unspecified",
                        cell.name()
                    ),
                    algorithm.span,
                ));
            }
            algorithm.outputs.push(Reference::new(cell.name()));
        }
    }
    let initializers = thread_parameter_bindings(flat, threaded, owners)?;
    reject_other_positions(flat, threaded)?;
    for (cell, span) in drawn {
        let owner = owners[&cell];
        let start = match initializers.get(&cell) {
            Some((callee, args)) => cell_projection_call(flat, threaded, owner, callee, args)?,
            None => initial_cell(cell, span),
        };
        add_cell_variable(flat, owner, start, span)?;
    }
    Ok(())
}

/// A parameter binding that is exactly a call of a reaching function calls
/// its specialization with each cell's initial value. At most one binding
/// may initialize a cell, since binding order is unspecified.
fn thread_parameter_bindings(
    flat: &mut flat::Model,
    threaded: &IndexMap<VarName, Threaded>,
    owners: &BTreeMap<ForeignStateCell, Cell>,
) -> Result<BTreeMap<ForeignStateCell, (Reference, Vec<Expression>)>, FlattenError> {
    let mut initializers = BTreeMap::new();
    for variable in flat.variables.values_mut() {
        let Some(Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            span,
        }) = variable.binding.as_ref()
        else {
            continue;
        };
        let Some(plan) = threaded.get(name.var_name()) else {
            continue;
        };
        if !matches!(
            variable.variability,
            rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
        ) {
            return Err(FlattenError::unordered_foreign_state(
                format!(
                    "`{}` reaches library state from a binding that is not a parameter binding",
                    variable.name
                ),
                *span,
            ));
        }
        let (name, args, span) = (name.clone(), args.clone(), *span);
        let mut threaded_args = args.clone();
        for cell in &plan.cells {
            if initializers
                .insert(*cell, (name.clone(), args.clone()))
                .is_some()
            {
                return Err(FlattenError::unordered_foreign_state(
                    format!(
                        "two parameter bindings initialize the library state `{}`, so their \
                         order is unspecified",
                        cell.name()
                    ),
                    span,
                ));
            }
            threaded_args.push(initial_cell(*cell, owners[cell].span));
        }
        let threaded_binding = Expression::FunctionCall {
            name: threaded_reference(plan, &name),
            args: threaded_args,
            is_constructor: false,
            span,
        };
        // A parameter's start value is its binding (MLS 3.7 §4.4.4); the same
        // call is the same initialization, not a second one.
        if variable.start.as_ref() == variable.binding.as_ref() {
            variable.start = Some(threaded_binding.clone());
        }
        variable.binding = Some(threaded_binding);
    }
    Ok(initializers)
}

/// A call of the cell output of `callee`'s specialization: a generated pure
/// function that returns only that cell.
fn cell_projection_call(
    flat: &mut flat::Model,
    threaded: &IndexMap<VarName, Threaded>,
    owner: Cell,
    callee: &Reference,
    args: &[Expression],
) -> Result<Expression, FlattenError> {
    let plan = &threaded[callee.var_name()];
    let specialized = flat.functions[&plan.name].clone();
    let name = fresh_function_name(flat, &format!("{}__{}", plan.name, owner.cell.name()));
    let instance_id = rumoca_core::FunctionInstanceId::new(
        flat.functions
            .values()
            .filter_map(|function| function.instance_id)
            .map(|id| id.index())
            .max()
            .map_or(0, |max| max + 1),
    );
    let mut projection = rumoca_core::Function::new(name.as_str(), owner.span);
    projection.def_id = specialized.def_id;
    projection.instance_id = Some(instance_id);
    projection.purity_declared = true;
    projection.inputs = specialized.inputs.clone();
    let mut receivers = vec![None; specialized.outputs.len()];
    for (index, output) in specialized.outputs.iter().enumerate() {
        if output.name == owner.cell.name() {
            let mut cell = output.clone();
            cell.default = None;
            projection.outputs.push(cell);
            receivers[index] = Some(target(owner.cell.name(), owner.owner, owner.span)?);
        }
    }
    projection.body.push(Statement::FunctionCall {
        comp: threaded_reference(plan, callee),
        args: specialized
            .inputs
            .iter()
            .map(|input| var_ref(&input.name, owner.span))
            .collect(),
        outputs: receivers,
        span: owner.span,
    });
    let reference = Reference::from_var_name(name.clone()).with_resolved_function(
        rumoca_core::ResolvedFunctionReference {
            instance_id,
            base_part_count: 0,
            transitively_non_replaceable: specialized.transitively_non_replaceable,
        },
    );
    flat.functions.insert(name, projection);
    let mut args = args.to_vec();
    args.extend(
        plan.cells
            .iter()
            .map(|cell| initial_cell(*cell, owner.span)),
    );
    Ok(Expression::FunctionCall {
        name: reference,
        args,
        is_constructor: false,
        span: owner.span,
    })
}

/// The effective type of a scalar Integer instance, issued when the model has
/// none; the shape is interned when Flat effective types are finalized.
fn scalar_integer_type(flat: &mut flat::Model) -> Result<rumoca_core::TypeId, FlattenError> {
    let integer = flat.predefined_types.integer;
    let scalar = rumoca_core::EffectiveType::new(integer, integer, [])
        .map_err(|error| FlattenError::internal(format!("Integer effective type: {error:?}")))?;
    if let Some((id, _)) = flat
        .effective_types
        .iter()
        .find(|(_, effective)| **effective == scalar)
    {
        return Ok(*id);
    }
    let id = rumoca_core::TypeId::new(
        flat.effective_types
            .keys()
            .map(|id| id.index())
            .chain([integer.index()])
            .max()
            .map_or(0, |max| max + 1),
    );
    flat.effective_types.insert(id, scalar);
    Ok(id)
}

fn add_cell_variable(
    flat: &mut flat::Model,
    owner: Cell,
    start: Expression,
    span: Span,
) -> Result<(), FlattenError> {
    let name = VarName::new(owner.cell.name());
    if flat.variables.contains_key(&name) {
        return Err(FlattenError::unordered_foreign_state(
            format!("the model already declares `{name}`, the library state's variable"),
            span,
        ));
    }
    let instance = flat
        .variables
        .values()
        .map(|variable| variable.instance_id.0)
        .max()
        .unwrap_or(0)
        + 1;
    let type_id = scalar_integer_type(flat)?;
    let extent = owner.cell.extent() as usize;
    flat.variables.insert(
        name.clone(),
        flat::Variable {
            instance_id: rumoca_core::InstanceId::new(instance),
            name: name.clone(),
            type_id,
            dims: vec![extent as i64],
            start: Some(start),
            fixed: Some(vec![true; extent]),
            is_discrete_type: true,
            is_primitive: true,
            is_protected: true,
            ..flat::Variable::empty_with_span(span)
        },
    );
    flat.variable_type_names.insert(name, "Integer".to_string());
    Ok(())
}

/// Every model position other than an algorithm statement or a parameter
/// binding leaves the order of state accesses unspecified. The reaching
/// functions are replaced by their specializations first, so any call of one
/// that remains is in such a position.
fn reject_other_positions(
    flat: &mut flat::Model,
    threaded: &IndexMap<VarName, Threaded>,
) -> Result<(), FlattenError> {
    flat.functions
        .retain(|name, _| !threaded.contains_key(name));
    super::call_args::rewrite_model_expressions(flat, &mut ReachingCallFinder { threaded })
}

struct ReachingCallFinder<'a> {
    threaded: &'a IndexMap<VarName, Threaded>,
}

impl FallibleExpressionRewriter for ReachingCallFinder<'_> {
    type Error = FlattenError;

    fn rewrite_expression(&mut self, expression: &Expression) -> Result<Expression, FlattenError> {
        if let Expression::FunctionCall { name, span, .. } = expression
            && self.threaded.contains_key(name.var_name())
        {
            return Err(FlattenError::unordered_foreign_state(
                format!(
                    "`{name}` reaches library state from an equation or attribute, where the \
                     order of its state accesses is unspecified"
                ),
                *span,
            ));
        }
        self.walk_expression(expression)
    }
}

impl FallibleStatementRewriter for ReachingCallFinder<'_> {
    fn rewrite_statement(&mut self, statement: &Statement) -> Result<Statement, FlattenError> {
        if let Statement::FunctionCall { comp, span, .. } = statement
            && self.threaded.contains_key(comp.var_name())
        {
            return Err(FlattenError::unordered_foreign_state(
                format!(
                    "`{comp}` reaches library state from an initial algorithm, where its order \
                     relative to the parameter bindings is unspecified"
                ),
                *span,
            ));
        }
        self.walk_statement(statement)
    }
}
