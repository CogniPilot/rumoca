//! Which record result fields may be returned without a value, and which
//! function inputs may therefore arrive without one.
//!
//! MLS 3.7 §12.4.4: "It is an error to use or return an un-initialized
//! variable in a function." A function that assigns the fields of its record
//! result under an `if` (the IF97 `boilingcurve_p` writes `cv` in one arm and
//! `cp`, `vt`, `vp` in the other) leaves a field unwritten on some executed
//! paths, and its callers read only the fields their own path wrote. The DAE
//! therefore checks such a field where it is read: the field's assigned state
//! travels with it through the return and through a call argument that hands
//! it on whole.
//!
//! This pass names, from the Flat function table alone, the two sets the
//! construction needs before it builds any function:
//!
//! - the field locals of a split record result that some path may leave
//!   unwritten when the trailing assembly reads them (Flat splits a record
//!   assigned field by field into one local per field and assembles the
//!   result from them at the end of the body);
//! - the inputs that may arrive absent: an input some call passes such a
//!   field to directly, or passes its own such input to. This is the least
//!   fixed point over the call graph.
//!
//! Both are over-approximations by syntax. A field local the definedness plan
//! proves written on every path is simply always present, and a call site
//! this pass does not visit passes a checked read, which fails exactly as an
//! unwritten value did before.

use super::*;
use rumoca_core::{Expression, Statement};
use std::collections::BTreeSet;

/// What one pass over the call sites finds.
#[derive(Default)]
struct Found {
    /// Inputs a call hands a value that may be absent.
    parameters: Vec<(VarName, usize)>,
    /// Result fields some call hands on whole, by producer and field path.
    forwarded: Vec<(VarName, Vec<String>)>,
}

#[derive(Debug, Default)]
pub(in crate::construction) struct AbsentFieldStates {
    /// Per function: the field locals of its record result that may be
    /// unwritten at the trailing assembly, keyed by the field path they fill.
    fields: HashMap<VarName, HashMap<Vec<String>, VarName>>,
    /// Per function: the input ordinals that may arrive absent.
    parameters: HashMap<VarName, BTreeSet<usize>>,
    /// The candidate fields some call site hands on whole. Only these are
    /// carried; a field nobody forwards keeps today's check at the result's
    /// assembly, so a model that reads its records whole is unchanged.
    forwarded: std::collections::HashSet<(VarName, Vec<String>)>,
}

impl AbsentFieldStates {
    pub(in crate::construction) fn analyze(flat: &flat::Model) -> Self {
        let mut states = Self::default();
        for (name, function) in &flat.functions {
            if function.is_constructor || function.external.is_some() {
                continue;
            }
            let fields = unwritten_result_fields(function);
            if !fields.is_empty() {
                states.fields.insert(name.clone(), fields);
            }
        }
        if states.fields.is_empty() {
            return states;
        }
        // An input joins when a call passes it a field that may be absent or
        // an input already in the set; the sets only grow, so this ends.
        loop {
            let mut found = Found::default();
            states.visit_model(flat, &mut found);
            let mut grew = false;
            for (callee, ordinal) in found.parameters {
                grew |= states.parameters.entry(callee).or_default().insert(ordinal);
            }
            states.forwarded.extend(found.forwarded);
            if !grew {
                return states;
            }
        }
    }

    /// The input ordinals of `function` that may arrive absent, increasing.
    pub(in crate::construction) fn absent_parameters(
        &self,
        function: &VarName,
    ) -> impl Iterator<Item = u32> + '_ {
        self.parameters
            .get(function)
            .into_iter()
            .flatten()
            .map(|&ordinal| ordinal as u32)
    }

    pub(in crate::construction) fn accepts_absence(
        &self,
        function: &VarName,
        ordinal: usize,
    ) -> bool {
        self.parameters
            .get(function)
            .is_some_and(|ordinals| ordinals.contains(&ordinal))
    }

    /// Whether `local` is a field local of `function`'s record result whose
    /// assigned state the trailing assembly carries instead of checking.
    pub(in crate::construction) fn carries_field_local(
        &self,
        function: &VarName,
        local: &VarName,
    ) -> bool {
        self.fields.get(function).is_some_and(|fields| {
            fields.iter().any(|(path, candidate)| {
                candidate == local && self.forwarded.contains(&(function.clone(), path.clone()))
            })
        })
    }

    /// One pass over every call site of the model and its functions.
    fn visit_model(&self, flat: &flat::Model, found: &mut Found) {
        for equation in flat.equations.iter().chain(&flat.initial_equations) {
            self.visit_expression(flat, None, &equation.residual, found);
        }
        // A declaration binding is an equation of the model too.
        for binding in flat
            .variables
            .values()
            .filter_map(|variable| variable.binding.as_ref())
        {
            self.visit_expression(flat, None, binding, found);
        }
        for algorithm in flat.algorithms.iter().chain(&flat.initial_algorithms) {
            self.visit_statements(flat, None, &algorithm.statements, found);
        }
        for (name, function) in &flat.functions {
            self.visit_statements(flat, Some((name, function)), &function.body, found);
        }
    }

    fn visit_statements(
        &self,
        flat: &flat::Model,
        owner: Option<(&VarName, &rumoca_core::Function)>,
        statements: &[Statement],
        found: &mut Found,
    ) {
        for statement in statements {
            self.visit_statement(flat, owner, statement, found);
        }
    }

    fn visit_statement(
        &self,
        flat: &flat::Model,
        owner: Option<(&VarName, &rumoca_core::Function)>,
        statement: &Statement,
        found: &mut Found,
    ) {
        let mut expression = |value: &Expression| self.visit_expression(flat, owner, value, found);
        match statement {
            Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => {}
            Statement::Assignment { value, .. } | Statement::Reinit { value, .. } => {
                expression(value)
            }
            Statement::Assert {
                condition,
                message,
                level,
                ..
            } => {
                expression(condition);
                expression(message);
                if let Some(level) = level {
                    expression(level);
                }
            }
            Statement::FunctionCall { comp, args, .. } => {
                let name = VarName::new(
                    comp.parts()
                        .iter()
                        .map(|part| part.ident.as_str())
                        .collect::<Vec<_>>()
                        .join("."),
                );
                self.visit_call(flat, owner, &name, args, found);
            }
            Statement::For {
                indices, equations, ..
            } => {
                for index in indices {
                    self.visit_expression(flat, owner, &index.range, found);
                }
                self.visit_statements(flat, owner, equations, found);
            }
            Statement::While { block, .. } => {
                self.visit_expression(flat, owner, &block.cond, found);
                self.visit_statements(flat, owner, &block.stmts, found);
            }
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    self.visit_expression(flat, owner, &block.cond, found);
                    self.visit_statements(flat, owner, &block.stmts, found);
                }
                if let Some(else_block) = else_block {
                    self.visit_statements(flat, owner, else_block, found);
                }
            }
            Statement::When { blocks, .. } => {
                for block in blocks {
                    self.visit_expression(flat, owner, &block.cond, found);
                    self.visit_statements(flat, owner, &block.stmts, found);
                }
            }
        }
    }

    fn visit_expression(
        &self,
        flat: &flat::Model,
        owner: Option<(&VarName, &rumoca_core::Function)>,
        expression: &Expression,
        found: &mut Found,
    ) {
        let mut record = |name: &VarName, args: &[Expression]| {
            self.visit_call(flat, owner, name, args, found);
        };
        rumoca_core::ExpressionVisitor::visit_expression(&mut Calls(&mut record), expression);
    }

    /// Record each input of `callee` this call hands a possibly absent value.
    fn visit_call(
        &self,
        flat: &flat::Model,
        owner: Option<(&VarName, &rumoca_core::Function)>,
        callee: &VarName,
        arguments: &[Expression],
        found: &mut Found,
    ) {
        let Some(function) = flat.functions.get(callee) else {
            return;
        };
        // Only a Modelica body can read an input through a checked read.
        if function.is_constructor || function.external.is_some() {
            return;
        }
        for (ordinal, argument) in arguments.iter().enumerate().take(function.inputs.len()) {
            if self.may_be_absent(owner, argument, &mut found.forwarded) {
                found.parameters.push((callee.clone(), ordinal));
            }
        }
    }

    /// A call argument that is exactly a possibly unwritten field of a call
    /// result, or exactly an input of the calling function that may itself
    /// arrive absent. Anything computed from such a value is a read.
    fn may_be_absent(
        &self,
        owner: Option<(&VarName, &rumoca_core::Function)>,
        argument: &Expression,
        forwarded: &mut Vec<(VarName, Vec<String>)>,
    ) -> bool {
        if let Some((callee, path)) = result_field_path(argument) {
            let candidate = self
                .fields
                .get(callee)
                .is_some_and(|fields| fields.contains_key(&path));
            if candidate {
                forwarded.push((callee.clone(), path));
            }
            return candidate;
        }
        let (
            Some((name, function)),
            Expression::VarRef {
                name: reference,
                subscripts,
                ..
            },
        ) = (owner, argument)
        else {
            return false;
        };
        subscripts.is_empty()
            && function
                .inputs
                .iter()
                .position(|input| input.name == reference.as_str())
                .is_some_and(|ordinal| self.accepts_absence(name, ordinal))
    }
}

/// `callee(..).a.b` as the callee and the field path `["a", "b"]`.
pub(in crate::construction) fn result_field_path(
    expression: &Expression,
) -> Option<(&VarName, Vec<String>)> {
    let mut path = Vec::new();
    let mut current = expression;
    loop {
        match current {
            Expression::FieldAccess { base, field, .. } => {
                path.push(field.clone());
                current = base;
            }
            Expression::FunctionCall { name, .. } if !path.is_empty() => {
                path.reverse();
                return Some((name.var_name(), path));
            }
            _ => return None,
        }
    }
}

/// The field locals the trailing assembly of `function`'s first result reads
/// that a preceding statement does not write on every path.
fn unwritten_result_fields(function: &rumoca_core::Function) -> HashMap<Vec<String>, VarName> {
    let Some(result) = function.outputs.first() else {
        return HashMap::new();
    };
    // The assembly group Flat appends: `result.path := local`, one row per
    // field, each local a plain reference.
    let mut rows = Vec::new();
    let mut assembly_start = function.body.len();
    for statement in function.body.iter().rev() {
        let Statement::Assignment { comp, value, .. } = statement else {
            break;
        };
        let parts = comp.parts();
        let Expression::VarRef {
            name, subscripts, ..
        } = value
        else {
            break;
        };
        let local = VarName::new(name.as_str());
        if parts.len() < 2
            || parts[0].ident != result.name
            || parts.iter().any(|part| !part.subs.is_empty())
            || !subscripts.is_empty()
            || !function
                .locals
                .iter()
                .any(|value| value.name == local.as_str())
        {
            break;
        }
        rows.push((
            parts[1..].iter().map(|part| part.ident.clone()).collect(),
            local,
        ));
        assembly_start -= 1;
    }
    let body = &function.body[..assembly_start];
    rows.into_iter()
        .filter(|(_, local)| !writes_on_every_path(body, local))
        .collect()
}

/// Whether the sequence assigns `local` whole on every path through it: a
/// top-level assignment, or an `if` with an `else` whose every arm does.
fn writes_on_every_path(statements: &[Statement], local: &VarName) -> bool {
    statements.iter().any(|statement| match statement {
        Statement::Assignment { comp, .. } => {
            let parts = comp.parts();
            parts.len() == 1 && parts[0].ident == local.as_str() && parts[0].subs.is_empty()
        }
        Statement::If {
            cond_blocks,
            else_block: Some(else_block),
            ..
        } => {
            cond_blocks
                .iter()
                .all(|block| writes_on_every_path(&block.stmts, local))
                && writes_on_every_path(else_block, local)
        }
        _ => false,
    })
}

/// Reports every call an expression makes, nested ones included.
struct Calls<'a, F: FnMut(&VarName, &[Expression])>(&'a mut F);

impl<F: FnMut(&VarName, &[Expression])> rumoca_core::ExpressionVisitor for Calls<'_, F> {
    fn visit_expression(&mut self, expression: &Expression) {
        if let Expression::FunctionCall { name, args, .. } = expression {
            (self.0)(name.var_name(), args);
        }
        self.walk_expression(expression);
    }
}
