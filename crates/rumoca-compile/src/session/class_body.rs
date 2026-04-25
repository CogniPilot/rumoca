use super::FileId;
use super::declaration_index::{ItemKey, ItemKind};
use indexmap::IndexMap;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::visitor::Visitor;
use serde::Serialize;
use std::ops::ControlFlow::{self, Continue};

#[derive(Debug, Clone, Default, Serialize)]
pub(crate) struct FileClassBodyIndex {
    bodies: IndexMap<ItemKey, ClassBody>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub(crate) struct ClassBody {
    component_occurrences: IndexMap<String, Vec<ast::Location>>,
    equation_section: Option<BodySectionSummary>,
    algorithm_section: Option<BodySectionSummary>,
    modifier_class_targets: Vec<ModifierClassTarget>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) struct BodySectionSummary {
    count: usize,
    range: Option<ast::Location>,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub(crate) struct ModifierClassTarget {
    raw_name: String,
    token_text: String,
    location: ast::Location,
}

#[derive(Debug, Serialize)]
struct OutlineBodyFingerprintValue {
    bodies: IndexMap<ItemKey, OutlineBodyFingerprintEntry>,
}

#[derive(Debug, Serialize)]
struct OutlineBodyFingerprintEntry {
    equation_section: Option<BodySectionSummary>,
    algorithm_section: Option<BodySectionSummary>,
}

impl FileClassBodyIndex {
    pub(crate) fn from_definition(file_id: FileId, definition: &ast::StoredDefinition) -> Self {
        let mut index = Self::default();
        let within_prefix = definition
            .within
            .as_ref()
            .map(ToString::to_string)
            .filter(|path| !path.is_empty())
            .unwrap_or_default();
        for (name, class) in &definition.classes {
            collect_class_bodies(file_id, &within_prefix, name, class, &mut index);
        }
        index
    }

    pub(crate) fn class_body(&self, item_key: &ItemKey) -> Option<&ClassBody> {
        self.bodies.get(item_key)
    }
}

pub(crate) fn class_body_fingerprints(
    definition: &ast::StoredDefinition,
) -> (super::Fingerprint, super::Fingerprint) {
    let index = FileClassBodyIndex::from_definition(FileId::default(), definition);
    let body = fingerprint_value(&index);
    let outline = fingerprint_value(&OutlineBodyFingerprintValue::from_index(&index));
    (body, outline)
}

impl ClassBody {
    pub(crate) fn component_occurrences(&self, component_name: &str) -> &[ast::Location] {
        self.component_occurrences
            .get(component_name)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    pub(crate) fn equation_section(&self) -> Option<&BodySectionSummary> {
        self.equation_section.as_ref()
    }

    pub(crate) fn algorithm_section(&self) -> Option<&BodySectionSummary> {
        self.algorithm_section.as_ref()
    }

    pub(crate) fn modifier_class_targets(&self) -> &[ModifierClassTarget] {
        &self.modifier_class_targets
    }
}

impl BodySectionSummary {
    pub(crate) fn count(&self) -> usize {
        self.count
    }

    pub(crate) fn range(&self) -> Option<&ast::Location> {
        self.range.as_ref()
    }
}

impl ModifierClassTarget {
    pub(crate) fn raw_name(&self) -> &str {
        &self.raw_name
    }

    pub(crate) fn token_text(&self) -> &str {
        &self.token_text
    }

    pub(crate) fn location(&self) -> &ast::Location {
        &self.location
    }
}

fn collect_class_bodies(
    file_id: FileId,
    container_path: &str,
    class_name: &str,
    class: &ast::ClassDef,
    index: &mut FileClassBodyIndex,
) {
    let class_path = join_qualified_name(container_path, class_name);
    let item_key = ItemKey::new(file_id, ItemKind::Class, container_path, class_name);
    let local_components = class
        .components
        .keys()
        .cloned()
        .collect::<std::collections::HashSet<_>>();
    let body = ClassBodyCollector::new(&local_components).collect(class);
    index.bodies.insert(item_key, body);

    for (nested_name, nested_class) in &class.classes {
        collect_class_bodies(file_id, &class_path, nested_name, nested_class, index);
    }
}

struct ClassBodyCollector<'a> {
    local_components: &'a std::collections::HashSet<String>,
    body: ClassBody,
}

impl<'a> ClassBodyCollector<'a> {
    fn new(local_components: &'a std::collections::HashSet<String>) -> Self {
        Self {
            local_components,
            body: ClassBody::default(),
        }
    }

    fn collect(mut self, class: &ast::ClassDef) -> ClassBody {
        self.body.equation_section =
            collect_equation_section(&class.equations, &class.initial_equations);
        self.body.algorithm_section =
            collect_algorithm_section(&class.algorithms, &class.initial_algorithms);
        if let Some(constrainedby) = &class.constrainedby {
            let _ = self.visit_type_name(
                constrainedby,
                ast::visitor::TypeNameContext::ClassConstrainedBy,
            );
        }
        for extend in &class.extends {
            let _ = self.visit_extend(extend);
        }
        for component in class.components.values() {
            let _ = self.visit_component(component);
        }
        let _ = self.visit_each(&class.equations, Self::visit_equation);
        let _ = self.visit_each(&class.initial_equations, Self::visit_equation);
        for section in &class.algorithms {
            let _ = self.visit_each(section, Self::visit_statement);
        }
        for section in &class.initial_algorithms {
            let _ = self.visit_each(section, Self::visit_statement);
        }
        for annotation in &class.annotation {
            let _ = self
                .visit_expression_ctx(annotation, ast::visitor::ExpressionContext::ClassAnnotation);
        }
        if let Some(external) = &class.external {
            let _ = self.visit_external_function(external);
        }
        self.body
    }

    fn record_component_reference(&mut self, component_reference: &ast::ComponentReference) {
        let Some(first_part) = component_reference.parts.first() else {
            return;
        };
        let component_name = first_part.ident.text.as_ref();
        if !self.local_components.contains(component_name) {
            return;
        }
        self.body
            .component_occurrences
            .entry(component_name.to_string())
            .or_default()
            .push(first_part.ident.location.clone());
    }

    fn record_modifier_class_target(&mut self, component_reference: &ast::ComponentReference) {
        let Some(last_part) = component_reference.parts.last() else {
            return;
        };
        self.body.modifier_class_targets.push(ModifierClassTarget {
            raw_name: component_reference.to_string(),
            token_text: last_part.ident.text.to_string(),
            location: last_part.ident.location.clone(),
        });
    }
}

impl ast::visitor::Visitor for ClassBodyCollector<'_> {
    fn visit_component_reference_ctx(
        &mut self,
        component_reference: &ast::ComponentReference,
        ctx: ast::visitor::ComponentReferenceContext,
    ) -> ControlFlow<()> {
        if matches!(
            ctx,
            ast::visitor::ComponentReferenceContext::ClassModificationTarget
        ) {
            self.record_modifier_class_target(component_reference);
        }
        self.visit_component_reference(component_reference)
    }

    fn visit_component_reference(
        &mut self,
        component_reference: &ast::ComponentReference,
    ) -> ControlFlow<()> {
        self.record_component_reference(component_reference);
        for part in &component_reference.parts {
            if let Some(subscripts) = &part.subs {
                self.visit_each(subscripts, Self::visit_subscript)?;
            }
        }
        Continue(())
    }

    fn visit_expr_function_call(
        &mut self,
        component_reference: &ast::ComponentReference,
        args: &[ast::Expression],
    ) -> ControlFlow<()> {
        self.record_component_reference(component_reference);
        self.visit_each(args, Self::visit_expression)
    }
}

fn join_qualified_name(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    }
}

fn collect_equation_section(
    equations: &[ast::Equation],
    initial_equations: &[ast::Equation],
) -> Option<BodySectionSummary> {
    let count = equations.len() + initial_equations.len();
    if count == 0 {
        return None;
    }

    Some(BodySectionSummary {
        count,
        range: merge_locations(
            equations
                .iter()
                .chain(initial_equations.iter())
                .filter_map(ast::Equation::get_location),
        ),
    })
}

fn collect_algorithm_section(
    algorithms: &[Vec<ast::Statement>],
    initial_algorithms: &[Vec<ast::Statement>],
) -> Option<BodySectionSummary> {
    let count = algorithms.len() + initial_algorithms.len();
    if count == 0 {
        return None;
    }

    Some(BodySectionSummary {
        count,
        range: merge_locations(
            algorithms
                .iter()
                .chain(initial_algorithms.iter())
                .flat_map(|section| section.iter())
                .filter_map(ast::Statement::get_location),
        ),
    })
}

fn merge_locations<'a>(
    locations: impl Iterator<Item = &'a ast::Location>,
) -> Option<ast::Location> {
    let mut iter = locations;
    let first = iter.next()?.clone();
    let mut merged = first.clone();
    for location in iter {
        if location.start_line < merged.start_line
            || (location.start_line == merged.start_line
                && location.start_column < merged.start_column)
        {
            merged.start_line = location.start_line;
            merged.start_column = location.start_column;
        }
        if location.end_line > merged.end_line
            || (location.end_line == merged.end_line && location.end_column > merged.end_column)
        {
            merged.end_line = location.end_line;
            merged.end_column = location.end_column;
        }
    }
    Some(merged)
}

fn fingerprint_value<T: Serialize>(value: &T) -> super::Fingerprint {
    let encoded =
        bincode::serialize(value).expect("query fingerprint serialization should succeed");
    *blake3::hash(&encoded).as_bytes()
}

impl OutlineBodyFingerprintValue {
    fn from_index(index: &FileClassBodyIndex) -> Self {
        let bodies = index
            .bodies
            .iter()
            .map(|(item_key, body)| {
                (
                    item_key.clone(),
                    OutlineBodyFingerprintEntry {
                        equation_section: body.equation_section.clone(),
                        algorithm_section: body.algorithm_section.clone(),
                    },
                )
            })
            .collect();
        Self { bodies }
    }
}
