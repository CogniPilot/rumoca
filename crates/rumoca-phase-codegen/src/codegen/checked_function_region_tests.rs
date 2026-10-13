use rumoca_core::{SourceMap, Span, VarName};

use super::*;

const SOURCE: &str = "function guarded input Boolean flag; output Integer y; protected Integer spare; algorithm y := 0; if flag then (y, spare) := (1, 1); assert(flag, \"selected\"); if flag then y := 2; else y := 3; end if; else y := 4; end if; y := spare; end guarded;";

#[derive(Clone, Copy)]
struct RegionSource(rumoca_core::SourceId);

impl RegionSource {
    fn at(self, snippet: &str, occurrence: usize) -> dae::DaeProvenance {
        let start = SOURCE.match_indices(snippet).nth(occurrence).unwrap().0;
        dae::DaeProvenance::source(Span::from_offsets(self.0, start, start + snippet.len()))
            .unwrap()
    }
}

fn scoped_model() -> (dae::Dae, RegionSource) {
    let mut sources = SourceMap::new();
    let source = RegionSource(sources.add("guarded.mo", SOURCE));
    let at = source.at("function guarded", 0);
    let model = dae::Dae::construct(sources, |model| {
        let (integer, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        model.function(
            dae::FunctionSignature::new(VarName::new("guarded"), [boolean], [integer], at),
            |model, reservation| {
                let (flag, output, spare) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("flag"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("spare"), integer, at)?,
                    ))
                })?;
                let (condition, zero, one, message) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).function_parameter(flag)?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(1))?,
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::String("selected".into()))?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, output, zero, source.at("y := 0", 0))
                })?;
                let outer = source.at("if flag then", 0);
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch(body, outer))?;
                model.functions(|functions| {
                    functions.assign_all(
                        &mut child,
                        &[(output, one), (spare, one)],
                        source.at("(y, spare)", 0),
                    )?;
                    functions.assertion(&mut child, condition, message, source.at("assert(flag", 0))
                })?;
                let child = nested_branch(model, child, condition, output, source)?;
                let (body, selected) =
                    model.functions(|functions| functions.finish_branch(finish, child))?;
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch(body, outer))?;
                let four = model.expressions(|expressions| {
                    expressions.at(at).literal(dae::DaeLiteral::Integer(4))
                })?;
                model.functions(|functions| {
                    functions.assign(&mut child, output, four, source.at("y := 4", 0))
                })?;
                let (mut body, fallback) =
                    model.functions(|functions| functions.finish_branch(finish, child))?;
                model.functions(|functions| {
                    functions.finish_conditional(
                        &mut body,
                        vec![(condition, selected)],
                        fallback,
                        outer,
                    )?;
                    let check = source.at("y := spare", 0);
                    functions.require_assigned(&mut body, spare, check)?;
                    let rhs = functions.read(&body, spare, check)?;
                    functions.assign(&mut body, output, rhs, check)?;
                    functions.define(body, at)
                })
            },
        )?;
        Ok(())
    })
    .unwrap();
    (model, source)
}

fn nested_branch<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    body: dae::FunctionBody<'dae>,
    condition: dae::ExprId<'dae>,
    output: dae::FunctionValueId<'dae>,
    source: RegionSource,
) -> Result<dae::FunctionBody<'dae>, dae::DaeConstructionError> {
    let at = source.at("if flag then", 1);
    let (mut child, finish) = model.functions(|functions| functions.begin_branch(body, at))?;
    let two =
        model.expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(2)))?;
    model
        .functions(|functions| functions.assign(&mut child, output, two, source.at("y := 2", 0)))?;
    let (body, selected) = model.functions(|functions| functions.finish_branch(finish, child))?;
    let (mut child, finish) = model.functions(|functions| functions.begin_branch(body, at))?;
    let three =
        model.expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(3)))?;
    model.functions(|functions| {
        functions.assign(&mut child, output, three, source.at("y := 3", 0))
    })?;
    let (mut body, fallback) =
        model.functions(|functions| functions.finish_branch(finish, child))?;
    model.functions(|functions| {
        functions.finish_conditional(&mut body, vec![(condition, selected)], fallback, at)
    })?;
    Ok(body)
}

fn assert_lexical_predecessors(model: &dae::Dae) {
    let projected = dae_template_json(model).unwrap();
    let function = &projected["functions"][0];
    let statements = &function["statements"];
    let initial = statements[0]["definition"].as_u64().unwrap() as usize;
    let outer = &statements[1];
    let prefix = &outer["branches"][0]["statements"];
    assert_eq!(prefix[0]["kind"], "assignment_group");
    assert_eq!(prefix[0]["statements"].as_array().unwrap().len(), 2);
    assert_eq!(prefix[1]["kind"], "assertion");
    let tuple_y = prefix[0]["statements"][0]["definition"].as_u64().unwrap() as usize;
    let nested = &prefix[2];
    let selected = nested["branches"][0]["statements"][0]["definition"]
        .as_u64()
        .unwrap() as usize;
    let fallback = nested["fallback"][0]["definition"].as_u64().unwrap() as usize;
    let outer_fallback = outer["fallback"][0]["definition"].as_u64().unwrap() as usize;
    let definitions = &function["definitions"];
    assert_eq!(definitions[selected]["previous_target_definition"], tuple_y);
    assert_eq!(definitions[fallback]["previous_target_definition"], tuple_y);
    assert_eq!(
        definitions[outer_fallback]["previous_target_definition"],
        initial
    );
    let checked = statements[2]["definition"].as_u64().unwrap() as usize;
    assert!(definitions[checked]["previous_target_definition"].is_null());
    assert_eq!(
        definitions[checked]["previous_target_state"],
        serde_json::json!({
            "kind": "conditional", "conditional": outer["ordinal"], "value": statements[2]["value"],
        })
    );
}

#[test]
fn dae_template_scoped_predecessors_follow_lexical_incoming_state() {
    let (model, _) = scoped_model();
    assert_lexical_predecessors(&model);
    let bytes = serde_json::to_vec(&model).unwrap();
    let decoded: dae::Dae = serde_json::from_slice(&bytes).unwrap();
    assert_lexical_predecessors(&decoded);
}

#[test]
fn dae_modelica_refuses_ordered_conditional_at_its_exact_source_span() {
    let (model, source) = scoped_model();
    let template =
        crate::templates::builtin_template_source("dae-modelica", "dae_modelica.mo.jinja").unwrap();
    let error = render_template_with_name(&model, template, "M").unwrap_err();
    assert!(
        error
            .to_string()
            .contains("unsupported-feature:dae-modelica-function-ordered-conditional"),
        "expected the ordered-conditional capability refusal, got {error:?}"
    );
    match error {
        CodegenError::TemplateRenderError { src, span, .. } => {
            assert_eq!(src.name(), "guarded.mo");
            assert_eq!(src.inner(), SOURCE);
            let expected = source.at("if flag then", 0).span();
            assert_eq!(span.offset(), expected.start.0);
            assert_eq!(span.len(), expected.end.0 - expected.start.0);
        }
        other => panic!("expected source-bound refusal, got {other:?}"),
    }
}
