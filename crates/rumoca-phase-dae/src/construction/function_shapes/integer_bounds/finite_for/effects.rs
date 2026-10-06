//! Declaration-identity mutation/read inventory for one complete sequence.
use super::*;
use rumoca_core::{ComponentReference, ExpressionVisitor, Reference};
use rumoca_ir_flat::visitor::StatementVisitor;

pub(super) struct Effects<'a> {
    counter: DefId,
    display_name: &'a VarName,
    pub counter_writes: u8,
    pub unknown_counter_write: bool,
    pub shadowed: bool,
    roots_written: HashSet<DefId>,
    names_written: HashSet<VarName>,
    binder_names: HashSet<VarName>,
}

impl<'a> Effects<'a> {
    pub(super) fn inspect(statements: &[Statement], name: &'a VarName, counter: DefId) -> Self {
        let mut result = Self {
            counter,
            display_name: name,
            counter_writes: 0,
            unknown_counter_write: false,
            shadowed: false,
            roots_written: HashSet::new(),
            names_written: HashSet::new(),
            binder_names: HashSet::new(),
        };
        for statement in statements {
            result.visit_statement(statement);
        }
        result
    }

    fn write(&mut self, target: &ComponentReference, unknown: bool) {
        self.roots_written.insert(target.root_def_id());
        self.names_written
            .insert(VarName::new(&target.parts()[0].ident));
        if target.root_def_id() == self.counter {
            self.counter_writes = self.counter_writes.saturating_add(1);
            self.unknown_counter_write |= unknown || !body::whole_counter(target, self.counter);
        }
    }

    pub(super) fn has_immutable_operands(
        &self,
        expression: &Expression,
        inputs: &[(VarName, DefId)],
    ) -> bool {
        let mut proof = Immutable {
            mutations: &self.roots_written,
            names_written: &self.names_written,
            binders: &self.binder_names,
            inputs,
            accepted: true,
        };
        proof.visit_expression(expression);
        proof.accepted
    }
}

struct Immutable<'a> {
    mutations: &'a HashSet<DefId>,
    names_written: &'a HashSet<VarName>,
    binders: &'a HashSet<VarName>,
    inputs: &'a [(VarName, DefId)],
    accepted: bool,
}

impl Immutable<'_> {
    fn known_input(&self, reference: &Reference, subscripts: &[Subscript]) -> bool {
        let Some(component) = reference.component_ref() else {
            return false;
        };
        let [part] = component.parts() else {
            return false;
        };
        let name = VarName::new(&part.ident);
        subscripts.is_empty()
            && part.subs.is_empty()
            && reference.var_name() == &name
            && self
                .inputs
                .iter()
                .any(|(input, id)| input == &name && *id == part.def_id)
            && !self.mutations.contains(&part.def_id)
            && !self.names_written.contains(&name)
            && !self.binders.contains(&name)
    }
}

impl ExpressionVisitor for Immutable<'_> {
    fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
        self.accepted &= self.known_input(reference, subscripts);
        self.walk_var_ref(reference, subscripts);
    }
}

impl ExpressionVisitor for Effects<'_> {}

impl StatementVisitor for Effects<'_> {
    fn visit_assignment(&mut self, target: &ComponentReference, _: &Expression) {
        self.write(target, false);
    }

    fn visit_for_statement(&mut self, indices: &[rumoca_core::ForIndex], statements: &[Statement]) {
        // Lexical shadow rejection is conservative syntactic scope analysis,
        // not declaration identification by display spelling.
        self.binder_names
            .extend(indices.iter().map(|index| VarName::new(&index.ident)));
        self.shadowed |= indices
            .iter()
            .any(|index| index.ident == self.display_name.as_str());
        for statement in statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement_function_call(
        &mut self,
        _: &Reference,
        _: &[Expression],
        outputs: &[Option<ComponentReference>],
    ) {
        for output in outputs.iter().flatten() {
            self.write(output, true);
        }
    }

    fn visit_reinit(&mut self, variable: &ComponentReference, _: &Expression) {
        self.write(variable, true);
    }
}

pub(super) fn reads_counter(statements: &[Statement], declaration: DefId) -> bool {
    struct Reads {
        declaration: DefId,
        found: bool,
    }
    impl ExpressionVisitor for Reads {
        fn visit_var_ref(&mut self, reference: &Reference, subscripts: &[Subscript]) {
            self.found |= reference.target_def_id() == Some(self.declaration);
            self.walk_var_ref(reference, subscripts);
        }
    }
    impl StatementVisitor for Reads {}
    let mut reader = Reads {
        declaration,
        found: false,
    };
    for statement in statements {
        reader.visit_statement(statement);
    }
    reader.found
}
