use std::collections::BTreeMap;

use rumoca_ir_galec::ast;

use crate::Value;
use crate::builtins::{
    interpolation_1d, interpolation_2d, interpolation_3d, lift_builtin, lu_factorize_builtin,
    lu_solve_builtin, scalar_builtin, solve_linear_equations,
};
use crate::interpreter::{EvaluationError, Evaluator};
use crate::runtime::{
    assign_indices, assign_state_parts, flatten_dimension, indexed, indexed_mut, indexed_owned,
    indices, initialized, limit_value, record_field, require_arity, state_path_declaration,
    state_path_mut, uninitialized_declaration, value_matches_type,
};

impl<'a> Evaluator<'a> {
    pub(super) fn call(
        &mut self,
        call: &ast::FunctionCall,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<Vec<Value>, EvaluationError> {
        let arguments = call
            .arguments
            .iter()
            .map(|argument| self.expression(argument, locals))
            .collect::<Result<Vec<_>, _>>()?;
        let name = call.function.lexeme();
        if rumoca_ir_galec::builtins::is_builtin_name(name) {
            let builtin = rumoca_ir_galec::builtins::find_builtin(name)
                .or_else(|| rumoca_ir_galec::builtins::find_lifted_base(name))
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "builtin catalog cannot resolve `{name}`"
                    ))
                })?;
            require_arity(name, builtin.inputs.len(), arguments.len())?;
            return self.builtin(name, arguments);
        }
        let function = self
            .block
            .protected_functions
            .iter()
            .chain(&self.block.public_functions)
            .find(|function| function.name.lexeme() == name)
            .ok_or_else(|| EvaluationError::UnknownName(name.to_owned()))?
            .clone();
        let mut frame = BTreeMap::new();
        let inputs = function
            .parameters
            .iter()
            .filter(|parameter| parameter.direction == ast::Direction::Input)
            .collect::<Vec<_>>();
        require_arity(name, inputs.len(), arguments.len())?;
        for (parameter, argument) in inputs.into_iter().zip(arguments) {
            if !self.value_matches_runtime_declaration(&parameter.decl, &argument, &frame)? {
                return Err(EvaluationError::Type("function input argument"));
            }
            frame.insert(parameter.decl.name.lexeme().to_owned(), argument);
        }
        for parameter in function
            .parameters
            .iter()
            .filter(|parameter| parameter.direction == ast::Direction::Output)
        {
            let value = self.uninitialized_runtime_declaration(&parameter.decl, &frame)?;
            frame.insert(parameter.decl.name.lexeme().to_owned(), value);
        }
        for declaration in &function.locals {
            let value = self.uninitialized_runtime_declaration(declaration, &frame)?;
            frame.insert(declaration.name.lexeme().to_owned(), value);
        }
        self.declaration_scopes.push(
            function
                .parameters
                .iter()
                .map(|parameter| &parameter.decl)
                .chain(&function.locals)
                .map(|declaration| (declaration.name.lexeme().to_owned(), declaration.clone()))
                .collect(),
        );
        let result = self.statements(&function.statements, &mut frame);
        self.declaration_scopes.pop();
        result?;
        function
            .parameters
            .iter()
            .filter(|parameter| parameter.direction == ast::Direction::Output)
            .map(|parameter| {
                frame
                    .get(parameter.decl.name.lexeme())
                    .cloned()
                    .ok_or_else(|| {
                        EvaluationError::MalformedCheckedBlock(format!(
                            "function `{name}` lost output `{}`",
                            parameter.decl.name.lexeme()
                        ))
                    })
                    .and_then(|value| {
                        initialized(&value, parameter.decl.name.lexeme())?;
                        Ok(value)
                    })
            })
            .collect()
    }

    fn builtin(&mut self, name: &str, args: Vec<Value>) -> Result<Vec<Value>, EvaluationError> {
        if let Some(base) = name.strip_suffix("1D").or_else(|| name.strip_suffix("2D")) {
            return lift_builtin(self, base, args).map(|value| vec![value]);
        }
        match name {
            "solveLinearEquations" => solve_linear_equations(self, args).map(|value| vec![value]),
            "luFactorize" => lu_factorize_builtin(self, args),
            "luSolve" => lu_solve_builtin(self, args).map(|value| vec![value]),
            "interpolation1D" => interpolation_1d(args).map(|value| vec![value]),
            "interpolation2D" => interpolation_2d(args).map(|value| vec![value]),
            "interpolation3D" => interpolation_3d(args).map(|value| vec![value]),
            _ => scalar_builtin(self, name, args).map(|value| vec![value]),
        }
    }

    fn uninitialized_runtime_declaration(
        &mut self,
        declaration: &ast::VariableDeclaration,
        frame: &BTreeMap<String, Value>,
    ) -> Result<Value, EvaluationError> {
        let scalar_decl = ast::VariableDeclaration {
            dimensions: Vec::new(),
            ..declaration.clone()
        };
        let scalar = uninitialized_declaration(self.block, &scalar_decl)?;
        declaration
            .dimensions
            .iter()
            .rev()
            .try_fold(scalar, |value, dimension| {
                let ast::Dimension::Expr(expression) = dimension else {
                    return Err(EvaluationError::MalformedCheckedBlock(format!(
                        "derived dimension used for non-input `{}`",
                        declaration.name.lexeme()
                    )));
                };
                let size = self
                    .expression(expression, frame)?
                    .integer()
                    .ok_or(EvaluationError::Type("runtime dimension"))?;
                let size = usize::try_from(size).map_err(|_| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "invalid runtime dimension {size} on `{}`",
                        declaration.name.lexeme()
                    ))
                })?;
                if size == 0 {
                    return Err(EvaluationError::MalformedCheckedBlock(format!(
                        "zero runtime dimension on `{}`",
                        declaration.name.lexeme()
                    )));
                }
                Ok(Value::Array(vec![value; size]))
            })
    }

    fn value_matches_runtime_declaration(
        &mut self,
        declaration: &ast::VariableDeclaration,
        value: &Value,
        frame: &BTreeMap<String, Value>,
    ) -> Result<bool, EvaluationError> {
        let mut elements = vec![value];
        for dimension in &declaration.dimensions {
            let expected = self.runtime_dimension(declaration, dimension, frame)?;
            let Some(nested) = flatten_dimension(elements, expected) else {
                return Ok(false);
            };
            elements = nested;
        }
        elements
            .into_iter()
            .map(|element| value_matches_type(self.block, &declaration.ty, element))
            .collect::<Result<Vec<_>, _>>()
            .map(|matches| matches.into_iter().all(|matches| matches))
    }

    fn runtime_dimension(
        &mut self,
        declaration: &ast::VariableDeclaration,
        dimension: &ast::Dimension,
        frame: &BTreeMap<String, Value>,
    ) -> Result<Option<usize>, EvaluationError> {
        let ast::Dimension::Expr(expression) = dimension else {
            return Ok(None);
        };
        let size = self
            .expression(expression, frame)?
            .integer()
            .ok_or(EvaluationError::Type("runtime dimension"))?;
        usize::try_from(size).map(Some).map_err(|_| {
            EvaluationError::MalformedCheckedBlock(format!(
                "invalid runtime dimension {size} on `{}`",
                declaration.name.lexeme()
            ))
        })
    }

    pub(super) fn reference(
        &self,
        reference: &ast::Reference,
        locals: &BTreeMap<String, Value>,
    ) -> Result<Value, EvaluationError> {
        match reference {
            ast::Reference::Local(part) => {
                let value = locals
                    .get(part.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
                let value = indexed(value, &part.subscripts, self, locals)?;
                initialized(&value, part.name.lexeme())?;
                Ok(value)
            }
            ast::Reference::State(parts) => {
                let first = parts.first().ok_or_else(|| {
                    EvaluationError::UnknownName("empty self reference".to_owned())
                })?;
                let mut value = self
                    .state
                    .get(first.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?
                    .value
                    .clone();
                value = indexed_owned(value, &first.subscripts, self, locals)?;
                for part in &parts[1..] {
                    value = record_field(value, part.name.lexeme())?;
                    value = indexed_owned(value, &part.subscripts, self, locals)?;
                }
                initialized(&value, first.name.lexeme())?;
                Ok(value)
            }
        }
    }

    pub(super) fn assign(
        &mut self,
        reference: &ast::Reference,
        value: Value,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        match reference {
            ast::Reference::Local(part) => {
                let indices = indices(&part.subscripts, self, locals)?;
                let slot = locals
                    .get_mut(part.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
                assign_indices(slot, &indices, value)
            }
            ast::Reference::State(parts) => {
                let first = parts.first().ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(
                        "empty checked state reference".to_owned(),
                    )
                })?;
                let evaluated = parts
                    .iter()
                    .map(|part| indices(&part.subscripts, self, locals))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut slot = self
                    .state
                    .remove(first.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?;
                let result = assign_state_parts(&mut slot.value, parts, &evaluated, value);
                self.state.insert(first.name.lexeme().to_owned(), slot);
                result
            }
        }
    }

    pub(super) fn limit_all(&mut self) -> Result<(), EvaluationError> {
        let names = self.state.keys().cloned().collect::<Vec<_>>();
        for name in names {
            let mut slot = self
                .state
                .remove(&name)
                .ok_or_else(|| EvaluationError::UnknownName(name.clone()))?;
            if !slot.value.is_initialized() {
                self.state.insert(name, slot);
                continue;
            }
            self.limit_declaration_value(slot.declaration, &mut slot.value, &BTreeMap::new())?;
            self.state.insert(name, slot);
        }
        Ok(())
    }

    pub(super) fn limit_reference(
        &mut self,
        reference: &ast::Reference,
        locals: &mut BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        match reference {
            ast::Reference::Local(part) => {
                let declaration = self
                    .declaration_scopes
                    .last()
                    .and_then(|scope| scope.get(part.name.lexeme()))
                    .cloned()
                    .ok_or_else(|| {
                        EvaluationError::MalformedCheckedBlock(format!(
                            "checked local limit target `{}` has no runtime declaration",
                            part.name.lexeme()
                        ))
                    })?;
                let evaluated = indices(&part.subscripts, self, locals)?;
                let mut value = locals
                    .remove(part.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(part.name.lexeme().to_owned()))?;
                let result = self.limit_declaration_value(
                    &declaration,
                    indexed_mut(&mut value, &evaluated)?,
                    locals,
                );
                locals.insert(part.name.lexeme().to_owned(), value);
                result
            }
            ast::Reference::State(parts) => {
                let first = parts.first().ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(
                        "empty checked limit reference".to_owned(),
                    )
                })?;
                let declaration = state_path_declaration(self.block, parts)?.clone();
                let evaluated = parts
                    .iter()
                    .map(|part| indices(&part.subscripts, self, locals))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut slot = self
                    .state
                    .remove(first.name.lexeme())
                    .ok_or_else(|| EvaluationError::UnknownName(first.name.lexeme().to_owned()))?;
                let result = self.limit_declaration_value(
                    &declaration,
                    state_path_mut(&mut slot.value, parts, &evaluated)?,
                    locals,
                );
                self.state.insert(first.name.lexeme().to_owned(), slot);
                result
            }
        }
    }

    fn limit_declaration_value(
        &mut self,
        declaration: &ast::VariableDeclaration,
        value: &mut Value,
        locals: &BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        let min = declaration
            .range
            .min
            .as_ref()
            .map(|bound| self.expression(bound, locals))
            .transpose()?;
        let max = declaration
            .range
            .max
            .as_ref()
            .map(|bound| self.expression(bound, locals))
            .transpose()?;
        limit_value(value, min.as_ref(), max.as_ref())?;
        if let ast::TypeRef::Compartment(name) = &declaration.ty {
            let compartment = self
                .block
                .compartments
                .iter()
                .find(|compartment| compartment.name.lexeme() == name.lexeme())
                .cloned()
                .ok_or_else(|| {
                    EvaluationError::MalformedCheckedBlock(format!(
                        "unknown compartment `{}`",
                        name.lexeme()
                    ))
                })?;
            self.limit_compartment_value(value, &compartment, locals)?;
        }
        Ok(())
    }

    fn limit_compartment_value(
        &mut self,
        value: &mut Value,
        compartment: &ast::StateCompartment,
        locals: &BTreeMap<String, Value>,
    ) -> Result<(), EvaluationError> {
        if let Value::Array(values) = value {
            for value in values {
                self.limit_compartment_value(value, compartment, locals)?;
            }
            return Ok(());
        }
        let Value::Record(fields) = value else {
            return Err(EvaluationError::Type("compartment value"));
        };
        for entity in &compartment.entities {
            let field = fields.get_mut(entity.decl.name.lexeme()).ok_or_else(|| {
                EvaluationError::MalformedCheckedBlock(format!(
                    "compartment value lost `{}`",
                    entity.decl.name.lexeme()
                ))
            })?;
            self.limit_declaration_value(&entity.decl, field, locals)?;
        }
        Ok(())
    }
}
