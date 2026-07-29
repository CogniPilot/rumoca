//! Modelica presentation derived directly from the checked DAE.

use rumoca_ir_dae as dae;
use serde_json::{Value, json};

use super::dae_backend::DaeBackendError;

pub(super) fn project(view: dae::DaeView<'_>) -> Result<Value, DaeBackendError> {
    let renderer = Renderer { view };
    Ok(json!({
        "functions": renderer.functions()?,
        "declarations": renderer.declarations()?,
        "equations": renderer.continuous_equations()?,
        "initial_equations": renderer.initialization_equations()?,
    }))
}

struct Renderer<'dae> {
    view: dae::DaeView<'dae>,
}

impl<'dae> Renderer<'dae> {
    fn functions(&self) -> Result<Vec<String>, DaeBackendError> {
        (0..self.view.function_count())
            .map(|index| {
                let function = self
                    .view
                    .function(self.view.function_id(index).unwrap())
                    .expect("dense checked function resolves");
                self.function(function)
            })
            .collect()
    }

    fn function(&self, function: dae::FunctionView<'dae>) -> Result<String, DaeBackendError> {
        let mut lines = vec![format!("function {}", function.name())];
        for parameter in function.parameters() {
            lines.push(format!(
                "  input {};",
                self.function_value_declaration(parameter.name(), parameter.value_type())
            ));
        }
        let values = function.values().collect::<Vec<_>>();
        for output in values
            .iter()
            .copied()
            .filter(|value| value.role() == dae::FunctionValueRole::Output)
        {
            lines.push(format!(
                "  output {};",
                self.function_value_declaration(output.name(), output.value_type())
            ));
        }
        let locals = values
            .iter()
            .copied()
            .filter(|value| value.role() == dae::FunctionValueRole::Local)
            .collect::<Vec<_>>();
        if !locals.is_empty() {
            lines.push("protected".to_string());
            for local in locals {
                lines.push(format!(
                    "  {};",
                    self.function_value_declaration(local.name(), local.value_type())
                ));
            }
        }
        lines.push("algorithm".to_string());
        for statement in function.statements() {
            lines.extend(self.function_statement(function, statement, 1)?);
        }
        lines.push(format!("end {};", function.name()));
        Ok(lines.join("\n"))
    }

    fn function_statement(
        &self,
        function: dae::FunctionView<'dae>,
        statement: dae::FunctionStatementView<'dae>,
        depth: usize,
    ) -> Result<Vec<String>, DaeBackendError> {
        let indent = "  ".repeat(depth);
        match statement {
            dae::FunctionStatementView::Assignment { target, value, .. } => {
                let target = function
                    .values()
                    .find(|candidate| candidate.id() == target)
                    .expect("checked function assignment target resolves");
                if let dae::ExpressionOperation::ArrayUpdate {
                    value, subscripts, ..
                } = self
                    .view
                    .expression(value)
                    .expect("checked function assignment value resolves")
                    .operation()
                {
                    return Ok(vec![format!(
                        "{indent}{}[{}] := {};",
                        target.name(),
                        self.subscripts(subscripts)?,
                        self.expression(value)?
                    )]);
                }
                Ok(vec![format!(
                    "{indent}{} := {};",
                    target.name(),
                    self.expression(value)?
                )])
            }
            dae::FunctionStatementView::For {
                fold, statements, ..
            } => {
                let fold = self
                    .view
                    .function_fold(fold)
                    .expect("checked function fold resolves");
                let domain = self
                    .view
                    .domain(fold.domain())
                    .expect("checked function fold domain resolves");
                let clauses = domain
                    .structured()
                    .binders
                    .iter()
                    .map(render_binder_clause)
                    .collect::<Vec<_>>()
                    .join(", ");
                let mut lines = vec![format!("{indent}for {clauses} loop")];
                for nested in statements {
                    lines.extend(self.function_statement(function, nested, depth + 1)?);
                }
                lines.push(format!("{indent}end for;"));
                Ok(lines)
            }
        }
    }

    fn function_value_declaration(
        &self,
        name: &rumoca_core::VarName,
        value_type: dae::ValueTypeId<'dae>,
    ) -> String {
        let value_type = self
            .view
            .value_type(value_type)
            .expect("checked function value type resolves");
        let scalar = value_type.record_name().map_or_else(
            || scalar_type_name(value_type.scalar_type()),
            |name| name.as_str(),
        );
        let dimensions = value_type
            .dimensions()
            .iter()
            .map(u32::to_string)
            .collect::<Vec<_>>();
        if dimensions.is_empty() {
            format!("{scalar} {name}")
        } else {
            format!("{scalar} {name}[{}]", dimensions.join(", "))
        }
    }

    fn declarations(&self) -> Result<Vec<String>, DaeBackendError> {
        self.view
            .variables()
            .map(|(_, variable)| self.declaration(variable))
            .collect()
    }

    fn declaration(&self, variable: dae::VariableView<'dae>) -> Result<String, DaeBackendError> {
        let role = match variable.role() {
            dae::VariableRole::Parameter => "parameter ",
            dae::VariableRole::Constant => "constant ",
            dae::VariableRole::Input => "input ",
            dae::VariableRole::Output => "output ",
            dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue => "discrete ",
            dae::VariableRole::State | dae::VariableRole::Algebraic => "",
        };
        let scalar = match variable.value_type().scalar_type() {
            dae::ScalarType::Real => "Real",
            dae::ScalarType::Integer => "Integer",
            dae::ScalarType::Boolean => "Boolean",
            dae::ScalarType::String => "String",
            dae::ScalarType::Record => {
                return Err(DaeBackendError::EscapedRecord {
                    span: variable.declaration().span(),
                });
            }
        };
        let dimensions = variable
            .value_type()
            .dimensions()
            .iter()
            .map(u32::to_string)
            .collect::<Vec<_>>();
        let dimensions = if dimensions.is_empty() {
            String::new()
        } else {
            format!("[{}]", dimensions.join(", "))
        };
        let mut attributes = Vec::new();
        self.push_attribute(&mut attributes, "start", variable.start())?;
        self.push_attribute(&mut attributes, "min", variable.minimum())?;
        self.push_attribute(&mut attributes, "max", variable.maximum())?;
        self.push_attribute(&mut attributes, "nominal", variable.nominal())?;
        if let Some(fixed) = variable.fixed() {
            attributes.push(format!("fixed = {fixed}"));
        }
        if let Some(unit) = variable.unit() {
            attributes.push(format!("unit = {}", quote(unit)));
        }
        let attributes = if attributes.is_empty() {
            String::new()
        } else {
            format!("({})", attributes.join(", "))
        };
        let binding = match variable.binding() {
            Some(expression) => format!(" = {}", self.expression(expression)?),
            None => String::new(),
        };
        let description = match variable.description() {
            Some(description) => format!(" {}", quote(description)),
            None => String::new(),
        };
        Ok(format!(
            "{role}{scalar}{dimensions} {}{attributes}{binding}{description};",
            variable.name()
        ))
    }

    fn push_attribute(
        &self,
        attributes: &mut Vec<String>,
        name: &'static str,
        expression: Option<dae::ExprId<'dae>>,
    ) -> Result<(), DaeBackendError> {
        if let Some(expression) = expression {
            attributes.push(format!("{name} = {}", self.expression(expression)?));
        }
        Ok(())
    }

    fn continuous_equations(&self) -> Result<Vec<String>, DaeBackendError> {
        self.view
            .continuous_owners()
            .map(|owner| match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    self.residual_equation(equation.residual())
                }
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    self.structured_family(family)
                }
            })
            .collect()
    }

    fn initialization_equations(&self) -> Result<Vec<String>, DaeBackendError> {
        (0..self.view.initialization_owner_count())
            .map(|index| {
                let owner = self
                    .view
                    .initialization_owner(index)
                    .expect("dense checked initialization owner resolves");
                match owner {
                    dae::InitializationOwnerView::Residual { equation, .. } => {
                        self.residual_equation(equation.residual())
                    }
                    dae::InitializationOwnerView::Structured { family, .. } => {
                        self.structured_family(family)
                    }
                }
            })
            .collect()
    }

    fn residual_equation(&self, residual: dae::ExprId<'dae>) -> Result<String, DaeBackendError> {
        let expression = self
            .view
            .expression(residual)
            .expect("checked residual expression resolves");
        let zero = if expression.value_type().dimensions().is_empty() {
            "0.0".to_string()
        } else {
            format!(
                "zeros({})",
                expression
                    .value_type()
                    .dimensions()
                    .iter()
                    .map(u32::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        Ok(format!("{zero} = {};", self.expression(residual)?))
    }

    fn structured_family(
        &self,
        family: dae::StructuredFamilyView<'dae>,
    ) -> Result<String, DaeBackendError> {
        let domain = self
            .view
            .domain(family.domain())
            .expect("checked structured domain resolves");
        let clauses = domain
            .structured()
            .binders
            .iter()
            .map(render_binder_clause)
            .collect::<Vec<_>>()
            .join(", ");
        let equations = family
            .bodies()
            .iter()
            .map(|body| self.residual_equation(body).map(|line| format!("  {line}")))
            .collect::<Result<Vec<_>, _>>()?
            .join("\n");
        Ok(format!("for {clauses} loop\n{equations}\nend for;"))
    }

    fn expression(&self, id: dae::ExprId<'dae>) -> Result<String, DaeBackendError> {
        let expression = self
            .view
            .expression(id)
            .expect("checked expression identity resolves");
        match expression.operation() {
            dae::ExpressionOperation::Literal(literal) => Ok(render_literal(literal)),
            dae::ExpressionOperation::Coordinate(coordinate) => self.coordinate(coordinate),
            dae::ExpressionOperation::Unary { operator, operand } => {
                let operator = match operator {
                    dae::UnaryOperator::Plus => "+",
                    dae::UnaryOperator::Negate => "-",
                    dae::UnaryOperator::Not => "not ",
                };
                Ok(format!("({operator}{})", self.expression(operand)?))
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => Ok(format!(
                "({} {} {})",
                self.expression(lhs)?,
                binary_operator(operator),
                self.expression(rhs)?
            )),
            dae::ExpressionOperation::Conditional(operands) => self.conditional(operands),
            dae::ExpressionOperation::Array(elements) => self.array_expression(elements),
            dae::ExpressionOperation::Record(fields) => self.record_expression(expression, fields),
            dae::ExpressionOperation::Field { base, field } => self.field_expression(base, field),
            dae::ExpressionOperation::Range { start, step, stop } => {
                if step == 1 {
                    Ok(format!("{start}:{stop}"))
                } else {
                    Ok(format!("{start}:{step}:{stop}"))
                }
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.comprehension(domain, body)
            }
            dae::ExpressionOperation::Index { base, subscripts } => Ok(format!(
                "{}[{}]",
                self.expression(base)?,
                self.subscripts(subscripts)?
            )),
            dae::ExpressionOperation::ArrayUpdate { .. } => {
                Err(DaeBackendError::EscapedArrayUpdate {
                    span: expression.provenance().span(),
                })
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.call_expression(builtin_name(builtin), arguments)
            }
            dae::ExpressionOperation::Call {
                function,
                arguments,
                ..
            } => {
                let name = self
                    .view
                    .function(function)
                    .expect("checked function identity resolves")
                    .name();
                self.call_expression(name.as_str(), arguments)
            }
            dae::ExpressionOperation::FunctionValue { value, .. } => Ok(self
                .view
                .function(value.function())
                .and_then(|function| function.values().find(|candidate| candidate.id() == value))
                .expect("checked function value resolves")
                .name()
                .to_string()),
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried }
            | dae::ExpressionOperation::FunctionFoldOutput { fold, carried } => {
                Ok(self.function_fold_target_name(fold, carried))
            }
        }
    }

    fn array_expression(
        &self,
        elements: dae::ExpressionOperands<'dae>,
    ) -> Result<String, DaeBackendError> {
        Ok(format!(
            "{{{}}}",
            elements
                .iter()
                .map(|id| self.expression(id))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ))
    }

    fn record_expression(
        &self,
        expression: dae::ExpressionView<'dae>,
        fields: dae::ExpressionOperands<'dae>,
    ) -> Result<String, DaeBackendError> {
        let name = expression
            .value_type()
            .record_name()
            .expect("checked record expression has a type name");
        self.call_expression(name.as_str(), fields)
    }

    fn field_expression(
        &self,
        base: dae::ExprId<'dae>,
        field: u32,
    ) -> Result<String, DaeBackendError> {
        let base_expression = self
            .view
            .expression(base)
            .expect("checked record base resolves");
        let field = base_expression
            .value_type()
            .record_field_name(field as usize)
            .expect("checked record field ordinal resolves");
        Ok(format!("{}.{}", self.expression(base)?, field))
    }

    fn call_expression(
        &self,
        name: &str,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<String, DaeBackendError> {
        Ok(format!(
            "{name}({})",
            arguments
                .iter()
                .map(|id| self.expression(id))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ))
    }

    fn subscripts(&self, subscripts: dae::SubscriptsView<'dae>) -> Result<String, DaeBackendError> {
        Ok(subscripts
            .iter()
            .map(|subscript| match subscript {
                dae::SubscriptView::Index { expression, .. }
                | dae::SubscriptView::Slice { expression, .. } => self.expression(expression),
                dae::SubscriptView::Whole { .. } => Ok(":".to_string()),
            })
            .collect::<Result<Vec<_>, _>>()?
            .join(", "))
    }

    fn function_fold_target_name(&self, fold: dae::FunctionFoldId<'dae>, carried: u32) -> String {
        let target = self
            .view
            .function_fold(fold)
            .and_then(|fold| fold.targets().nth(carried as usize))
            .expect("checked function fold target resolves");
        self.view
            .function(target.function())
            .and_then(|function| function.values().find(|value| value.id() == target))
            .expect("checked function fold target value resolves")
            .name()
            .to_string()
    }

    fn conditional(
        &self,
        operands: dae::ExpressionOperands<'dae>,
    ) -> Result<String, DaeBackendError> {
        let mut rendered = String::new();
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let keyword = if ordinal == 0 { "if" } else { "elseif" };
            rendered.push_str(&format!(
                "{keyword} {} then {} ",
                self.expression(
                    operands
                        .get(ordinal)
                        .expect("checked conditional guard resolves")
                )?,
                self.expression(
                    operands
                        .get(ordinal + 1)
                        .expect("checked conditional branch resolves")
                )?
            ));
        }
        rendered.push_str(&format!(
            "else {}",
            self.expression(
                operands
                    .get(operands.len() - 1)
                    .expect("checked conditional fallback resolves")
            )?
        ));
        Ok(rendered)
    }

    fn comprehension(
        &self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
    ) -> Result<String, DaeBackendError> {
        let domain = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves");
        let clauses = domain
            .structured()
            .binders
            .iter()
            .map(render_binder_clause)
            .collect::<Vec<_>>()
            .join(", ");
        Ok(format!("{{{} for {clauses}}}", self.expression(body)?))
    }

    fn coordinate(&self, coordinate: dae::CoordinateView<'dae>) -> Result<String, DaeBackendError> {
        let variable = |id| {
            self.view
                .variable(id)
                .expect("checked variable coordinate resolves")
                .name()
                .to_string()
        };
        Ok(match coordinate {
            dae::CoordinateView::Parameter(id) => variable(id.into()),
            dae::CoordinateView::Input(id) => variable(id.into()),
            dae::CoordinateView::State(id) => variable(id.into()),
            dae::CoordinateView::Derivative(id) => format!("der({})", variable(id.into())),
            dae::CoordinateView::Algebraic(id) => variable(id.into()),
            dae::CoordinateView::DiscreteReal(id) => variable(id.into()),
            dae::CoordinateView::DiscreteValue(id) => variable(id.into()),
            dae::CoordinateView::PreDiscreteReal(id) => format!("pre({})", variable(id.into())),
            dae::CoordinateView::PreDiscreteValue(id) => {
                format!("pre({})", variable(id.into()))
            }
            dae::CoordinateView::Time => "time".to_string(),
            dae::CoordinateView::Condition(id) => format!("$condition{}", id.index()),
            dae::CoordinateView::Delay(id) => format!("$delay{}", id.index()),
            dae::CoordinateView::Previous(id) => format!("$previous{}", id.index()),
            dae::CoordinateView::Terminal(id) => format!("$terminal{}", id.index()),
            dae::CoordinateView::Binder(id) => {
                let domain = self
                    .view
                    .domain(id.domain())
                    .expect("checked binder domain resolves");
                domain.structured().binders[id.ordinal() as usize]
                    .display_name
                    .clone()
            }
            dae::CoordinateView::FunctionParameter(id) => self
                .view
                .function(id.function())
                .and_then(|function| function.parameters().find(|parameter| parameter.id() == id))
                .expect("checked function parameter resolves")
                .name()
                .to_string(),
        })
    }
}

fn render_binder_clause(binder: &rumoca_core::StructuredIndexBinder) -> String {
    if binder.step == 1 {
        format!(
            "{} in {}:{}",
            binder.display_name, binder.lower, binder.upper
        )
    } else {
        format!(
            "{} in {}:{}:{}",
            binder.display_name, binder.lower, binder.step, binder.upper
        )
    }
}

const fn scalar_type_name(scalar: dae::ScalarType) -> &'static str {
    match scalar {
        dae::ScalarType::Real => "Real",
        dae::ScalarType::Integer => "Integer",
        dae::ScalarType::Boolean => "Boolean",
        dae::ScalarType::String => "String",
        dae::ScalarType::Record => "record",
    }
}

fn render_literal(literal: &dae::DaeLiteral) -> String {
    match literal {
        dae::DaeLiteral::Real(value) => format!("{value:?}"),
        dae::DaeLiteral::Integer(value) => value.to_string(),
        dae::DaeLiteral::Enumeration(value) => value.to_string(),
        dae::DaeLiteral::Boolean(value) => value.to_string(),
        dae::DaeLiteral::String(value) => quote(value),
    }
}

const fn binary_operator(operator: dae::BinaryOperator) -> &'static str {
    match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => "+",
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => "-",
        dae::BinaryOperator::Multiply => "*",
        dae::BinaryOperator::ElementwiseMultiply => ".*",
        dae::BinaryOperator::Divide => "/",
        dae::BinaryOperator::ElementwiseDivide => "./",
        dae::BinaryOperator::Power => "^",
        dae::BinaryOperator::ElementwisePower => ".^",
        dae::BinaryOperator::Equal => "==",
        dae::BinaryOperator::NotEqual => "<>",
        dae::BinaryOperator::Less => "<",
        dae::BinaryOperator::LessEqual => "<=",
        dae::BinaryOperator::Greater => ">",
        dae::BinaryOperator::GreaterEqual => ">=",
        dae::BinaryOperator::And => "and",
        dae::BinaryOperator::Or => "or",
    }
}

const fn builtin_name(builtin: dae::PureBuiltin) -> &'static str {
    match builtin {
        dae::PureBuiltin::Abs => "abs",
        dae::PureBuiltin::Sign => "sign",
        dae::PureBuiltin::Sqrt => "sqrt",
        dae::PureBuiltin::Mod => "mod",
        dae::PureBuiltin::Floor => "floor",
        dae::PureBuiltin::Ceil => "ceil",
        dae::PureBuiltin::Integer => "integer",
        dae::PureBuiltin::Sin => "sin",
        dae::PureBuiltin::Cos => "cos",
        dae::PureBuiltin::Tan => "tan",
        dae::PureBuiltin::Asin => "asin",
        dae::PureBuiltin::Acos => "acos",
        dae::PureBuiltin::Atan => "atan",
        dae::PureBuiltin::Atan2 => "atan2",
        dae::PureBuiltin::Sinh => "sinh",
        dae::PureBuiltin::Cosh => "cosh",
        dae::PureBuiltin::Tanh => "tanh",
        dae::PureBuiltin::Exp => "exp",
        dae::PureBuiltin::Log => "log",
        dae::PureBuiltin::Log10 => "log10",
        dae::PureBuiltin::Smooth => "smooth",
        dae::PureBuiltin::NoEvent => "noEvent",
        dae::PureBuiltin::Min => "min",
        dae::PureBuiltin::Max => "max",
        dae::PureBuiltin::Sum => "sum",
        dae::PureBuiltin::Product => "product",
        dae::PureBuiltin::Size => "size",
        dae::PureBuiltin::Zeros => "zeros",
        dae::PureBuiltin::Ones => "ones",
        dae::PureBuiltin::Fill => "fill",
        dae::PureBuiltin::Linspace => "linspace",
        dae::PureBuiltin::Cross => "cross",
    }
}

fn quote(value: &str) -> String {
    serde_json::to_string(value).expect("JSON string escaping is infallible")
}
