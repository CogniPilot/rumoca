//! Zero-sized array materialization (MLS §10.3.4, ARR-036/039).
//!
//! Zero-sized components emit no variables, but references to them survive in
//! equations, asserts, bindings, and start values. This pass materializes a
//! zero-size placeholder variable for every dangling reference so reductions
//! lower to their identity values instead of failing as unresolved.
//! Retained record containers already own their type and need no placeholder.

use crate::{Context, FlattenError};
use rumoca_core::ExpressionVisitor;
use rumoca_ir_flat as flat;

pub(crate) fn materialize_referenced_zero_sized_array_variables(
    flat: &mut flat::Model,
    ctx: &Context,
) -> Result<(), FlattenError> {
    let mut collector = MissingZeroSizedArrayRefCollector {
        flat,
        ctx,
        refs: Vec::new(),
    };
    collector.collect();

    for name in collector.refs {
        let Some(dims) = zero_sized_array_dims_for_ref(&name, ctx) else {
            continue;
        };
        let var_name = name.var_name().clone();
        if flat.variables.contains_key(&var_name) {
            continue;
        }
        let source_span = name.span().ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "zero-sized array reference `{}` is missing source provenance",
                name.as_str()
            ))
        })?;
        // The referenced declaration produced no instantiated occurrence, so the
        // placeholder owns a Flat-allocated occurrence identity that keeps its
        // exact declaration provenance.
        let instance_id = flat.materialize_instance(flat::InstanceRelation {
            owner: name.instance_id(),
            declaration: name.target_def_id(),
            indices: Box::default(),
            kind: flat::InstanceKind::Materialized,
        });
        let variable = flat::Variable {
            instance_id,
            name: var_name.clone(),
            component_ref: name.component_ref().cloned(),
            source_span,
            dims,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            is_protected: true,
            ..flat::Variable::empty_with_span(source_span)
        };
        flat.variable_type_names
            .entry(var_name.clone())
            .or_insert_with(|| "Real".to_string());
        flat.add_variable(var_name, variable);
    }
    Ok(())
}

struct MissingZeroSizedArrayRefCollector<'a> {
    flat: &'a flat::Model,
    ctx: &'a Context,
    refs: Vec<rumoca_core::Reference>,
}

impl MissingZeroSizedArrayRefCollector<'_> {
    fn collect(&mut self) {
        for equation in &self.flat.equations {
            self.visit_expression(&equation.residual);
        }
        for equation in &self.flat.initial_equations {
            self.visit_expression(&equation.residual);
        }
        for assertion in &self.flat.assert_equations {
            self.visit_expression(&assertion.condition);
            self.visit_expression(&assertion.message);
            if let Some(level) = &assertion.level {
                self.visit_expression(level);
            }
        }
        for assertion in &self.flat.initial_assert_equations {
            self.visit_expression(&assertion.condition);
            self.visit_expression(&assertion.message);
            if let Some(level) = &assertion.level {
                self.visit_expression(level);
            }
        }
        // Variable bindings and start values reference zero-sized arrays just
        // like equations do (e.g. `Real s = sum(z)` with `Real z[0]`).
        let variables: Vec<_> = self.flat.variables.values().collect();
        for variable in variables {
            if let Some(binding) = &variable.binding {
                self.visit_expression(binding);
            }
            if let Some(start) = &variable.start {
                self.visit_expression(start);
            }
        }
    }
}

impl rumoca_core::ExpressionVisitor for MissingZeroSizedArrayRefCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if subscripts.is_empty()
            && !self.flat.variables.contains_key(name.var_name())
            && !self.flat.record_instances.contains_key(name.var_name())
            && zero_sized_array_dims_for_ref(name, self.ctx).is_some()
        {
            self.refs.push(name.clone());
        }
        self.walk_var_ref(name, subscripts);
    }
}

fn zero_sized_array_dims_for_ref(name: &rumoca_core::Reference, ctx: &Context) -> Option<Vec<i64>> {
    let dims = ctx.array_dimensions.get(name.as_str()).or_else(|| {
        name.target_def_id()
            .and_then(|def_id| ctx.target_def_names.get(&def_id))
            .and_then(|target_name| ctx.array_dimensions.get(target_name))
    })?;
    (!dims.is_empty() && dims.iter().any(|dim| *dim <= 0)).then(|| dims.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{Span, VarName};

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("zero_sized_array_test.mo"),
            1,
            2,
        )
    }

    fn var_ref(path: &str, def_id: u32) -> rumoca_core::Expression {
        let parts = rumoca_core::ComponentPath::from_flat_path(path)
            .parts()
            .iter()
            .enumerate()
            .map(|(index, ident)| rumoca_core::ComponentRefPart {
                ident: ident.clone(),
                span: test_span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(def_id + u32::try_from(index).expect("small path")),
            })
            .collect();
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(
                rumoca_core::ComponentReference::construct(false, test_span(), parts)
                    .expect("fixture reference carries an exact identity for every part"),
            ),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    #[test]
    fn empty_record_concat_preserves_typed_records_without_placeholders() {
        // MLS §10.7 / SPEC_0007: an empty record keeps its declared effective type.
        let source = r"
            model EmptyRecordConcat
                record Part
                    parameter Real mass=1;
                end Part;
                function combine
                    input Part parts[:];
                    output Part result;
                algorithm
                    result.mass := sum(parts.mass);
                end combine;
                model Assembly
                    parameter Boolean assembled=false;
                    parameter Integer n=0;
                    parameter Part payloads[n];
                    parameter Part aggregate;
                    final parameter Part properties=if assembled then
                        combine(cat(1, {aggregate}, payloads)) else combine({aggregate});
                    final parameter Real mass=properties.mass;
                end Assembly;
                parameter Integer n=0;
                parameter Part payloads[n];
                Assembly chassis(n=n,payloads=payloads);
                Real x(start=0,fixed=true);
            equation
                der(x)=chassis.mass;
            end EmptyRecordConcat;
        ";
        let file_name = "EmptyRecordConcat.mo";
        let parsed = rumoca_phase_parse::parse_to_ast(source, file_name).expect("fixture parses");
        let mut tree = rumoca_ir_ast::ClassTree::from_parsed(parsed);
        tree.source_map.add(file_name, source);
        let resolved = rumoca_phase_resolve::resolve(rumoca_ir_ast::ParsedTree::new(tree))
            .expect("fixture resolves");
        let instanced = rumoca_phase_instantiate::instantiate(resolved, "EmptyRecordConcat")
            .expect("fixture instantiates");
        let rumoca_ir_ast::InstancedTree { tree, mut overlay } = instanced;
        rumoca_phase_typecheck::typecheck_instanced(&tree, &mut overlay, "EmptyRecordConcat")
            .expect("fixture typechecks");

        let flat = crate::flatten_ref(&tree, &overlay, "EmptyRecordConcat")
            .expect("typed empty record references must not become untyped placeholders");

        for path in ["payloads", "chassis.payloads"] {
            let name = VarName::new(path);
            let typed = overlay
                .components
                .values()
                .find(|data| data.qualified_name.to_flat_string() == path)
                .expect("typed empty record declaration");
            let record = flat.record_instances.get(&name).expect("retained record");
            assert_eq!(record.instance_id, typed.instance_id);
            assert_eq!(Some(record.type_def_id), typed.type_def_id);
            assert_eq!(record.dims, [0]);
            assert_eq!(record.effective_type_id, typed.type_id);
            let effective = flat
                .effective_types
                .get(&record.effective_type_id)
                .expect("record effective type is registered");
            assert_eq!(Some(effective), overlay.effective_types.get(&typed.type_id));
            assert_eq!(effective.dimensions(), [0]);
            assert!(
                !flat.variables.contains_key(&name),
                "duplicate variable {path}"
            );
        }
    }

    #[test]
    fn materialized_placeholders_carry_distinct_allocated_occurrence_identities() {
        let mut flat = flat::Model::new();
        for (index, path) in ["CriticalDamping.c0", "CriticalDamping.c1"]
            .into_iter()
            .enumerate()
        {
            flat.equations.push(flat::Equation::new(
                var_ref(path, 10 + u32::try_from(index).expect("small fixture") * 10),
                test_span(),
                flat::EquationOrigin::ComponentEquation {
                    component: "CriticalDamping".to_string(),
                },
            ));
        }
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("CriticalDamping.c0".to_string(), vec![0]);
        ctx.array_dimensions
            .insert("CriticalDamping.c1".to_string(), vec![0]);

        materialize_referenced_zero_sized_array_variables(&mut flat, &ctx)
            .expect("zero-sized placeholders materialize");

        let identities: Vec<rumoca_core::InstanceId> = ["CriticalDamping.c0", "CriticalDamping.c1"]
            .into_iter()
            .map(|name| {
                flat.variables
                    .get(&VarName::new(name))
                    .unwrap_or_else(|| panic!("missing placeholder {name}"))
                    .instance_id
            })
            .collect();
        for instance_id in &identities {
            assert!(
                !instance_id.is_unset(),
                "placeholder kept the reserved unset occurrence identity"
            );
            assert_eq!(
                flat.instance_relations
                    .get(instance_id)
                    .expect("materialized occurrence is registered")
                    .kind,
                flat::InstanceKind::Materialized
            );
        }
        assert_ne!(identities[0], identities[1]);
        assert_eq!(flat.validate_shape_contract(), Ok(()));
    }
}
