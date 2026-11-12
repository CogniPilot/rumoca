//! Record outputs and locals assigned field by field (MLS 3.6 §11.2.1,
//! §12.4.4), against the field layout of the record's constructor.

use super::*;

/// One declared field of a record, from its record constructor's input
/// (MLS 3.6 §12.6).
#[derive(Debug, Clone)]
pub(super) struct RecordField {
    name: String,
    /// A scalar of the field's declared type, the target of a write's
    /// conversion (MLS 3.6 §10.6.13).
    element: Value,
    /// The field declares a default, which this interpreter does not evaluate.
    defaulted: bool,
    /// The field is itself a record.
    record: bool,
}

impl RecordField {
    /// Why this field, never written, has no value.
    fn missing_value(&self) -> &'static str {
        if self.defaulted {
            "has only its declared default, which is not evaluated here"
        } else {
            "has no value"
        }
    }
}

impl FunctionEnv {
    /// Write `value` into the field `fields` of the record output or local
    /// `root`, converted to the field's declared type; `false` when `root` has
    /// no declared field layout, the field is not declared, or the target is
    /// a field of a nested record.
    pub(super) fn set_field(
        &mut self,
        root: &str,
        fields: &[ComponentRefPart],
        value: Value,
    ) -> bool {
        let [field] = fields else {
            return false;
        };
        let Some(Some(layout)) = self.record_layouts.get(root) else {
            return false;
        };
        let Some(declared) = layout
            .iter()
            .find(|declared| declared.name == field.ident && !declared.record)
        else {
            return false;
        };
        let value = convert_to_element(&declared.element, value);
        let slot = match self.outputs.get_mut(root) {
            Some(slot) => slot,
            None => match self.locals.get_mut(root) {
                Some(slot) => slot,
                None => return false,
            },
        };
        let Value::Record(record) = slot else {
            return false;
        };
        record.insert(field.ident.clone(), value);
        true
    }

    /// The value of the record `name` once every declared field has one, in
    /// declaration order (MLS 3.6 §12.4.4); `value` itself for a name with no
    /// tracked record layout.
    pub(super) fn complete_record(
        &self,
        name: &str,
        value: &Value,
        span: Span,
    ) -> Result<Value, EvalError> {
        let unproven = |reason: String| EvalError::UnsupportedExpression {
            kind: format!("record `{name}`: {reason}"),
            span,
        };
        let layout = match self.record_layouts.get(name) {
            None => return Ok(value.clone()),
            Some(None) => {
                return match value {
                    Value::Record(fields) if fields.is_empty() => {
                        Err(unproven("no declared field layout is in scope".to_string()))
                    }
                    _ => Ok(value.clone()),
                };
            }
            Some(Some(layout)) => layout,
        };
        let Value::Record(record) = value else {
            return Err(unproven(format!("holds a {}", value.type_name())));
        };
        let mut complete = IndexMap::new();
        for field in layout {
            let Some(field_value) = record.get(&field.name) else {
                return Err(unproven(format!(
                    "field `{}` {}",
                    field.name,
                    field.missing_value()
                )));
            };
            complete.insert(field.name.clone(), field_value.clone());
        }
        Ok(Value::Record(complete))
    }
}

/// The declared field layout of the scalar record `param`, from its record
/// constructor in scope (MLS 3.6 §12.6).
pub(super) fn record_layout(
    param: &rumoca_core::FunctionParam,
    eval: &EvalState<'_>,
) -> Option<Vec<RecordField>> {
    let constructor = eval
        .ctx
        .get_function(&param.type_name)
        .filter(|function| function.is_constructor)?;
    Some(
        constructor
            .inputs
            .iter()
            .map(|field| RecordField {
                name: field.name.clone(),
                element: type_default_value(&field.type_name, &[]),
                defaulted: field.default.is_some(),
                record: field.type_class == Some(rumoca_core::ClassType::Record),
            })
            .collect(),
    )
}

/// `value` written into a component whose declared scalar type is that of
/// `element`: an Integer, alone or in an array, becomes Real in a Real
/// component (MLS 3.6 §10.6.13).
fn convert_to_element(element: &Value, value: Value) -> Value {
    match (element, value) {
        (Value::Real(_), Value::Integer(written)) => Value::Real(written as f64),
        (Value::Real(_), Value::Array(written)) => Value::Array(
            written
                .into_iter()
                .map(|written| convert_to_element(element, written))
                .collect(),
        ),
        (_, value) => value,
    }
}
