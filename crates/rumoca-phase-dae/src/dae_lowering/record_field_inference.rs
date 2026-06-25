use indexmap::IndexMap;
use std::collections::{BTreeMap, HashMap, HashSet};

use rumoca_core::ExpressionVisitor;

pub(super) type FieldUseMap = BTreeMap<String, BTreeMap<String, Vec<i64>>>;

pub(super) fn infer_record_fields_by_function(
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    record_fields_by_type: &HashMap<String, Vec<String>>,
) -> HashMap<String, FieldUseMap> {
    let mut fields = HashMap::new();
    for (name, function) in functions {
        fields.insert(
            name.as_str().to_string(),
            collect_local_record_field_uses(function, record_fields_by_type),
        );
    }

    let mut changed = true;
    while changed {
        changed = false;
        for (name, function) in functions {
            let propagated = collect_callee_record_field_uses(function, functions, &fields);
            let entry = fields.entry(name.as_str().to_string()).or_default();
            if merge_field_use_map(entry, propagated) {
                changed = true;
            }
        }
    }
    fields
}

fn collect_local_record_field_uses(
    function: &rumoca_core::Function,
    record_fields_by_type: &HashMap<String, Vec<String>>,
) -> FieldUseMap {
    let prefixes = record_field_prefixes(function, record_fields_by_type);
    let mut collector = RecordFieldUseCollector {
        prefixes: &prefixes,
        fields: BTreeMap::new(),
    };
    for statement in &function.body {
        visit_statement_expressions(statement, &mut collector);
    }
    collector.fields
}

fn record_field_prefixes(
    function: &rumoca_core::Function,
    record_fields_by_type: &HashMap<String, Vec<String>>,
) -> HashSet<String> {
    let mut prefixes = HashSet::new();
    for input in &function.inputs {
        if input.type_class == Some(rumoca_core::ClassType::Record)
            || record_fields_by_type.contains_key(&input.type_name)
        {
            prefixes.insert(input.name.clone());
        }
        if let Some((prefix, _)) = input.name.split_once('_') {
            prefixes.insert(prefix.to_string());
        }
    }
    prefixes
}

struct RecordFieldUseCollector<'a> {
    prefixes: &'a HashSet<String>,
    fields: FieldUseMap,
}

impl RecordFieldUseCollector<'_> {
    fn record_var_ref(&mut self, name: &rumoca_core::Reference, dims: Vec<i64>) {
        let Some((prefix, field)) = split_record_field_name(name.as_str()) else {
            return;
        };
        if !self.prefixes.contains(prefix) {
            return;
        }
        merge_field_dims(
            self.fields
                .entry(prefix.to_string())
                .or_default()
                .entry(field.to_string())
                .or_default(),
            dims,
        );
    }
}

impl ExpressionVisitor for RecordFieldUseCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.record_var_ref(name, dims_from_subscripts(subscripts));
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = expr
            && let rumoca_core::Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
        {
            let dims = dims_from_subscripts(base_subscripts)
                .into_iter()
                .chain(dims_from_subscripts(subscripts))
                .collect();
            self.record_var_ref(name, dims);
        }
        ExpressionVisitor::walk_expression(self, expr);
    }
}

fn split_record_field_name(name: &str) -> Option<(&str, &str)> {
    let (prefix, field) = name.split_once('_')?;
    (!prefix.is_empty() && !field.is_empty()).then_some((prefix, field))
}

fn dims_from_subscripts(subscripts: &[rumoca_core::Subscript]) -> Vec<i64> {
    let max_index = subscripts
        .iter()
        .filter_map(const_subscript_index)
        .max()
        .unwrap_or_default();
    if max_index > 0 {
        vec![max_index]
    } else {
        Vec::new()
    }
}

fn const_subscript_index(subscript: &rumoca_core::Subscript) -> Option<i64> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Some(*value),
        rumoca_core::Subscript::Expr { expr, .. } => {
            let rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } = expr.as_ref()
            else {
                return None;
            };
            Some(*value)
        }
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn collect_callee_record_field_uses(
    function: &rumoca_core::Function,
    functions: &IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    fields_by_function: &HashMap<String, FieldUseMap>,
) -> FieldUseMap {
    let mut collector = CalleeFieldUseCollector {
        functions,
        fields_by_function,
        fields: BTreeMap::new(),
    };
    for statement in &function.body {
        visit_statement_expressions(statement, &mut collector);
    }
    collector.fields
}

struct CalleeFieldUseCollector<'a> {
    functions: &'a IndexMap<rumoca_core::VarName, rumoca_core::Function>,
    fields_by_function: &'a HashMap<String, FieldUseMap>,
    fields: FieldUseMap,
}

impl ExpressionVisitor for CalleeFieldUseCollector<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if let rumoca_core::Expression::FunctionCall { name, args, .. } = expr
            && let Some(callee_fields) = self.fields_by_function.get(name.as_str())
            && let Some(callee) = self.functions.get(name.var_name())
        {
            merge_callee_field_uses(args, callee, callee_fields, &mut self.fields);
        }
        ExpressionVisitor::walk_expression(self, expr);
    }
}

fn merge_callee_field_uses(
    args: &[rumoca_core::Expression],
    callee: &rumoca_core::Function,
    callee_fields: &FieldUseMap,
    target: &mut FieldUseMap,
) {
    for (idx, input) in callee.inputs.iter().enumerate() {
        let Some(fields) = callee_fields.get(&input.name) else {
            continue;
        };
        let Some(rumoca_core::Expression::VarRef { name: arg_name, .. }) = args.get(idx) else {
            continue;
        };
        let entry = target.entry(arg_name.as_str().to_string()).or_default();
        for (field, dims) in fields {
            merge_field_dims(entry.entry(field.clone()).or_default(), dims.clone());
        }
    }
}

fn merge_field_use_map(target: &mut FieldUseMap, source: FieldUseMap) -> bool {
    let mut changed = false;
    for (prefix, fields) in source {
        let target_fields = target.entry(prefix).or_default();
        for (field, dims) in fields {
            let target_dims = target_fields.entry(field).or_default();
            let before = target_dims.clone();
            merge_field_dims(target_dims, dims);
            changed |= *target_dims != before;
        }
    }
    changed
}

fn visit_statement_expressions(
    statement: &rumoca_core::Statement,
    visitor: &mut impl ExpressionVisitor,
) {
    match statement {
        rumoca_core::Statement::Assignment { value, .. }
        | rumoca_core::Statement::Reinit { value, .. } => visitor.visit_expression(value),
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for index in indices {
                visitor.visit_expression(&index.range);
            }
            for statement in equations {
                visit_statement_expressions(statement, visitor);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            visitor.visit_expression(&block.cond);
            for statement in &block.stmts {
                visit_statement_expressions(statement, visitor);
            }
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                visitor.visit_expression(&block.cond);
                for statement in &block.stmts {
                    visit_statement_expressions(statement, visitor);
                }
            }
            if let Some(else_block) = else_block {
                for statement in else_block {
                    visit_statement_expressions(statement, visitor);
                }
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                visitor.visit_expression(&block.cond);
                for statement in &block.stmts {
                    visit_statement_expressions(statement, visitor);
                }
            }
        }
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                visitor.visit_expression(arg);
            }
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            visitor.visit_expression(condition);
            visitor.visit_expression(message);
            if let Some(level) = level {
                visitor.visit_expression(level);
            }
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
}

fn merge_field_dims(target: &mut Vec<i64>, source: Vec<i64>) {
    if source.len() > target.len() {
        target.resize(source.len(), 0);
    }
    for (idx, value) in source.into_iter().enumerate() {
        target[idx] = target[idx].max(value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ClassType, Expression, Function, FunctionParam, Span, Statement, VarName};

    fn span(start: usize) -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            start,
            start + 1,
        )
    }

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: vec![],
            span: span(1),
        }
    }

    fn assignment(value: Expression) -> Statement {
        Statement::Assignment {
            comp: rumoca_core::ComponentReference {
                local: false,
                span: span(1),
                parts: vec![rumoca_core::ComponentRefPart {
                    ident: "y".to_string(),
                    span: span(1),
                    subs: vec![],
                }],
                def_id: None,
            },
            value,
            span: span(1),
        }
    }

    #[test]
    fn callee_field_uses_follow_signature_order_not_btree_order() {
        let mut callee = Function::new("Pkg.callee", span(1));
        callee.add_input(
            FunctionParam::new("port", "Pkg.Port", span(1)).with_type_class(ClassType::Connector),
        );
        callee.add_input(
            FunctionParam::new("state", "Pkg.State", span(1)).with_type_class(ClassType::Record),
        );
        callee.body.push(assignment(var_ref("state_phase")));

        let mut caller = Function::new("Pkg.caller", span(1));
        caller.body.push(assignment(Expression::FunctionCall {
            name: VarName::new("Pkg.callee").into(),
            args: vec![var_ref("port_a"), var_ref("state_a")],
            is_constructor: false,
            span: span(1),
        }));

        let mut functions = IndexMap::new();
        functions.insert(callee.name.clone(), callee);
        functions.insert(caller.name.clone(), caller);

        let record_fields_by_type =
            HashMap::from([("Pkg.State".to_string(), vec!["phase".to_string()])]);
        let inferred = infer_record_fields_by_function(&functions, &record_fields_by_type);
        let caller_fields = inferred.get("Pkg.caller").expect("caller fields");

        assert!(
            !caller_fields.contains_key("port_a"),
            "connector argument must not inherit the callee record field"
        );
        assert_eq!(
            caller_fields
                .get("state_a")
                .and_then(|fields| fields.get("phase")),
            Some(&Vec::<i64>::new())
        );
    }
}
