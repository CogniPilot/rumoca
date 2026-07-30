use rumoca_core::{ComponentReference, EffectiveType, InstanceId, Reference, SourceId, TypeId};

use super::super::*;

pub(super) struct TestSource {
    pub(super) map: SourceMap,
    pub(super) source: SourceId,
    text: String,
}

const TEST_REAL_TYPE: TypeId = TypeId(0x00ff_ff01);
const TEST_INTEGER_TYPE: TypeId = TypeId(0x00ff_ff02);
const TEST_BOOLEAN_TYPE: TypeId = TypeId(0x00ff_ff03);
const TEST_STRING_TYPE: TypeId = TypeId(0x00ff_ff04);
const TEST_CLOCK_TYPE: TypeId = TypeId(0x00ff_ff05);

pub(super) fn test_model() -> flat::Model {
    let mut model = flat::Model::new();
    model.predefined_types = flat::PredefinedTypeIds {
        real: TEST_REAL_TYPE,
        integer: TEST_INTEGER_TYPE,
        boolean: TEST_BOOLEAN_TYPE,
        string: TEST_STRING_TYPE,
        clock: TEST_CLOCK_TYPE,
    };
    model
}

pub(super) fn real_function_param(
    name: &str,
    dimensions: Vec<i64>,
    span: Span,
) -> rumoca_core::FunctionParam {
    function_param(
        name,
        "Real",
        TEST_REAL_TYPE,
        TEST_REAL_TYPE,
        dimensions,
        span,
    )
}

pub(super) fn integer_function_param(
    name: &str,
    dimensions: Vec<i64>,
    span: Span,
) -> rumoca_core::FunctionParam {
    function_param(
        name,
        "Integer",
        TEST_INTEGER_TYPE,
        TEST_INTEGER_TYPE,
        dimensions,
        span,
    )
}

pub(super) fn real_alias_function_param(
    name: &str,
    type_name: &str,
    nominal: TypeId,
    dimensions: Vec<i64>,
    span: Span,
) -> rumoca_core::FunctionParam {
    function_param(
        name,
        type_name,
        nominal,
        TEST_REAL_TYPE,
        dimensions,
        span,
    )
}

pub(super) fn enumeration_function_param(
    name: &str,
    type_name: &str,
    type_id: TypeId,
    dimensions: Vec<i64>,
    span: Span,
) -> rumoca_core::FunctionParam {
    function_param(name, type_name, type_id, type_id, dimensions, span)
}

pub(super) fn register_test_enumeration_type(model: &mut flat::Model, type_id: TypeId) {
    model.enumeration_type_roots.insert(type_id);
}

pub(super) fn function_param(
    name: &str,
    type_name: &str,
    nominal: TypeId,
    canonical: TypeId,
    dimensions: Vec<i64>,
    span: Span,
) -> rumoca_core::FunctionParam {
    let effective_type = EffectiveType::new(nominal, canonical, dimensions)
        .expect("fixture function type is resolved");
    rumoca_core::FunctionParam::new(name, type_name, effective_type, span)
}

pub(super) fn test_instance_id(name: &str) -> InstanceId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    InstanceId::new(hash)
}

pub(super) fn test_reference(name: &str) -> Reference {
    Reference::new(name).with_instance_id(test_instance_id(name))
}

pub(super) fn test_component_reference(name: &str, span: Span) -> ComponentReference {
    ComponentReference::construct(
        false,
        span,
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
            def_id: rumoca_core::DefId::new(test_instance_id(name).index().max(1)),
        }],
    )
    .expect("test component reference has exact identity")
}

pub(super) fn register_test_effective_type(
    model: &mut flat::Model,
    nominal: TypeId,
    canonical: TypeId,
    dimensions: &[i64],
) {
    model.effective_types.insert(
        nominal,
        EffectiveType::new(nominal, canonical, dimensions.to_vec()).unwrap(),
    );
}

pub(super) fn register_test_clock_type(
    model: &mut flat::Model,
    nominal: TypeId,
    dimensions: &[i64],
) {
    register_test_effective_type(model, nominal, TEST_CLOCK_TYPE, dimensions);
}

pub(super) fn register_test_real_type(
    model: &mut flat::Model,
    nominal: TypeId,
    dimensions: &[i64],
) {
    register_test_effective_type(model, nominal, TEST_REAL_TYPE, dimensions);
}

pub(super) fn register_test_integer_type(
    model: &mut flat::Model,
    nominal: TypeId,
    dimensions: &[i64],
) {
    register_test_effective_type(model, nominal, TEST_INTEGER_TYPE, dimensions);
}

pub(super) fn register_test_boolean_type(
    model: &mut flat::Model,
    nominal: TypeId,
    dimensions: &[i64],
) {
    register_test_effective_type(model, nominal, TEST_BOOLEAN_TYPE, dimensions);
}

impl TestSource {
    pub(super) fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let source = map.add("direct_todae.mo", text);
        Self {
            map,
            source,
            text: text.to_string(),
        }
    }

    pub(super) fn span(&self, needle: &str, occurrence: usize) -> Span {
        let start = self
            .text
            .match_indices(needle)
            .nth(occurrence)
            .map(|(start, _)| start)
            .unwrap();
        Span::from_offsets(self.source, start, start + needle.len())
    }
}

pub(super) fn scalar_real_model(source: &TestSource) -> flat::Model {
    let declaration = source.span("Real x", 0);
    let use_span = source.span("x", 1);
    let literal_span = source.span("1.0", 0);
    let equation_span = source.span("x - 1.0", 0);
    let mut model = test_model();
    let mut variable = flat::Variable::empty_with_span(declaration);
    variable.name = VarName::new("x");
    variable.instance_id = test_instance_id("x");
    variable.type_id = TypeId::new(7);
    variable.variability = Variability::Continuous(Default::default());
    variable.is_primitive = true;
    register_test_real_type(&mut model, variable.type_id, &variable.dims);
    model.add_variable(variable.name.clone(), variable);
    model
        .variable_type_names
        .insert(VarName::new("x"), "Real".to_string());
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("x"),
                subscripts: Vec::new(),
                span: use_span,
            }),
            rhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: literal_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

pub(super) fn add_primitive_variable(
    model: &mut flat::Model,
    source: &TestSource,
    name: &str,
    declaration: &str,
    type_id: u32,
    dims: Vec<i64>,
    discrete: bool,
) {
    let mut variable = flat::Variable::empty_with_span(source.span(declaration, 0));
    variable.name = VarName::new(name);
    variable.instance_id = test_instance_id(name);
    variable.type_id = TypeId::new(type_id);
    variable.dims = dims;
    variable.variability = if discrete {
        Variability::Discrete(Default::default())
    } else {
        Variability::Continuous(Default::default())
    };
    variable.is_discrete_type = discrete;
    variable.is_primitive = true;
    if discrete {
        register_test_boolean_type(model, variable.type_id, &variable.dims);
    } else {
        register_test_real_type(model, variable.type_id, &variable.dims);
    }
    model.add_variable(variable.name.clone(), variable);
    model.variable_type_names.insert(
        VarName::new(name),
        if discrete { "Boolean" } else { "Real" }.to_string(),
    );
}

pub(super) fn variable_reference(
    source: &TestSource,
    name: &str,
    owner: &str,
    occurrence: usize,
    subscripts: Vec<Subscript>,
) -> Expression {
    Expression::VarRef {
        name: Reference::new(name).with_instance_id(test_instance_id(name)),
        subscripts,
        span: source.span(owner, occurrence),
    }
}
