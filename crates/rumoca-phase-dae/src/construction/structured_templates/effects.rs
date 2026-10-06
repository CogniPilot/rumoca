//! Conservative source-owned effect eligibility for optional equation views.

use super::*;
use rumoca_core::ExpressionVisitor;
use rumoca_ir_flat::StatementVisitor;

#[derive(Default)]
pub(super) struct CallEffects {
    completed: HashMap<VarName, bool>,
    active: HashSet<VarName>,
}

impl CallEffects {
    pub(super) fn permits(&mut self, flat: &flat::Model, name: &VarName) -> bool {
        if let Some(value) = self.completed.get(name) {
            return *value;
        }
        let Some(function) = flat.functions.get(name) else {
            return false;
        };
        if !function.body_is_pure()
            || function.external.is_some()
            || !self.active.insert(name.clone())
        {
            return false;
        }
        let mut visitor = BodyEffects {
            flat,
            calls: self,
            permitted: true,
        };
        for statement in &function.body {
            visitor.visit_statement(statement);
        }
        let permitted = visitor.permitted;
        self.active.remove(name);
        self.completed.insert(name.clone(), permitted);
        permitted
    }
}

struct BodyEffects<'a, 'cache> {
    flat: &'a flat::Model,
    calls: &'cache mut CallEffects,
    permitted: bool,
}

impl ExpressionVisitor for BodyEffects<'_, '_> {
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        self.permitted &= !temporal(*function);
        self.walk_builtin_call(function, args);
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        _constructor: bool,
    ) {
        self.permitted &= self.calls.permits(self.flat, name.var_name());
        self.walk_function_call(name, args, false);
    }
}

impl StatementVisitor for BodyEffects<'_, '_> {
    fn visit_statement_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        outputs: &[Option<rumoca_core::ComponentReference>],
    ) {
        self.visit_function_call(name, args, false);
        for output in outputs.iter().flatten() {
            self.visit_component_reference(output);
        }
    }

    fn visit_reinit(&mut self, variable: &rumoca_core::ComponentReference, value: &Expression) {
        self.permitted = false;
        self.visit_component_reference(variable);
        self.visit_expression(value);
    }

    fn visit_when_statement(&mut self, blocks: &[rumoca_core::StatementBlock]) {
        self.permitted = false;
        for block in blocks {
            self.visit_statement_block(block);
        }
    }
}

pub(super) fn temporal(function: BuiltinFunction) -> bool {
    matches!(
        function,
        BuiltinFunction::Der
            | BuiltinFunction::Pre
            | BuiltinFunction::Edge
            | BuiltinFunction::Change
            | BuiltinFunction::Reinit
            | BuiltinFunction::Sample
            | BuiltinFunction::Clock
            | BuiltinFunction::Hold
            | BuiltinFunction::Previous
            | BuiltinFunction::Interval
            | BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock
            | BuiltinFunction::Initial
            | BuiltinFunction::Terminal
            | BuiltinFunction::Delay
    )
}
