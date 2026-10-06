//! Merge controls: exact Integer profiles cannot share unprofiled fold facts.
use super::*;
use rumoca_core::{SourceMap, StructuredIndexBinder, StructuredIndexDomain, VarName};

fn function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    integer: dae::ValueTypeId<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    model
        .function(
            dae::FunctionSignature::new(
                VarName::new("profiled"),
                [vector, integer],
                [real, real, real],
                at,
            ),
            |model, reservation| {
                let (u, i, y, z, copy) = model.functions(|f| {
                    Ok((
                        f.parameter(&reservation, VarName::new("u"), 0, at)?,
                        f.parameter(&reservation, VarName::new("i"), 1, at)?,
                        f.output(&reservation, VarName::new("y"), 0, at)?,
                        f.output(&reservation, VarName::new("z"), 1, at)?,
                        f.output(&reservation, VarName::new("copy"), 2, at)?,
                    ))
                })?;
                let (selected, zero, sum) = model.expressions(|e| {
                    let u = e.at(at).function_parameter(u)?;
                    let i = e.at(at).function_parameter(i)?;
                    let selected = e.at(at).index(
                        u,
                        [dae::Subscript::Index {
                            expression: i,
                            provenance: at,
                        }],
                    )?;
                    let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                    let sum = e.at(at).builtin(dae::PureBuiltin::Sum, [u])?;
                    Ok((selected, zero, sum))
                })?;
                let mut body = model.functions(|f| f.begin(reservation, at))?;
                model.functions(|f| {
                    f.assign(&mut body, y, zero, at)?;
                    f.assign(&mut body, z, sum, at)
                })?;
                let domain = model.domains(|d| {
                    d.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "j".into(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let mut loop_body = model.functions(|f| f.begin_loop(body, domain, [y, z], at))?;
                let (old_y, old_z) = model.functions(|f| {
                    Ok((
                        f.read(loop_body.body(), y, at)?,
                        f.read(loop_body.body(), z, at)?,
                    ))
                })?;
                let next = model
                    .expressions(|e| e.at(at).binary(dae::BinaryOperator::Add, old_y, selected))?;
                model.functions(|f| {
                    f.assign_loop(&mut loop_body, y, next, at)?;
                    f.assign_loop(&mut loop_body, z, old_z, at)
                })?;
                body = model.functions(|f| f.finish_loop(loop_body, at))?;
                let last = model.functions(|f| f.read(&body, y, at))?;
                model.functions(|f| {
                    f.assign(&mut body, copy, last, at)?;
                    f.define(body, at)
                })
            },
        )
        .map(|(function, ())| function)
}

fn model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function profiled input Real u[3]; input Integer i; output Real y; output Real z; output Real copy; algorithm y:=0; z:=sum(u); for j in 1:2 loop y:=y+u[i]; z:=z; end for; copy:=y; end profiled;";
    let source = sources.add("merge_profiles.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector =
            model.types(|t| t.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let integer =
            model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let function = function(model, vector, integer, real, at)?;
        let x =
            model.variables(|v| v.algebraic(VarName::new("x"), vector, at, Default::default()))?;
        let input = model.variables(|v| {
            v.input(
                VarName::new("index"),
                integer,
                dae::InputVariability::Discrete,
                at,
                Default::default(),
            )
        })?;
        model.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            for index in [1, 3, 0, 4] {
                let i = e.at(at).literal(dae::DaeLiteral::Integer(index))?;
                e.at(at).call(function, 0, [x, i])?;
                e.at(at).call(function, 1, [x, i])?;
                e.at(at).call(function, 2, [x, i])?;
            }
            let dynamic = e.at(at).coordinate(dae::CoordinateInput::Input(input))?;
            e.at(at).call(function, 0, [x, dynamic])?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap()
}

fn project<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Result<Vec<(bool, u32, usize)>, ProjectionError> {
    let mut values = Vec::new();
    for_each_scalar_coordinate_cached(view, root, 0, None, cache, |coordinate, scalar| {
        let value = match coordinate {
            dae::CoordinateView::Algebraic(v) => (false, v.index(), scalar),
            dae::CoordinateView::Input(v) => (true, v.index(), scalar),
            _ => panic!("fixture has only two model coordinates"),
        };
        if !values.contains(&value) {
            values.push(value);
        }
    })?;
    Ok(values)
}

#[test]
fn exact_profiles_keep_fold_results_separate_and_restore_generic_shared_caches() {
    model().inspect(|view| {
        let roots=(0..view.expression_count()).filter_map(|i| view.expression_id(i))
            .filter(|id| matches!(view.expression(*id).unwrap().operation(),dae::ExpressionOperation::Call {..}))
            .collect::<Vec<_>>();
        assert_eq!(roots.len(),13);
        let mut cache=ScalarCoordinateProjectionCache::default();
        let mut reference=ScalarCoordinateProjectionCache {uncached_fold_reference:true,
            uncached_parameter_fragments:true,..Default::default()};
        // The unrelated carried output is a complete generic summary, with
        // real shared fold/fragment facts. Profiled calls must restore them.
        assert_eq!(project(view,roots[1],&mut cache).unwrap(),vec![(false,0,0),(false,0,1),(false,0,2)]);
        let folds=cache.completed_folds.clone();
        assert!(!folds.is_empty());
        let fragments=(0..view.expression_count()).flat_map(|expression| (0..3).map(move |scalar| (expression,scalar)))
            .filter_map(|(expression,scalar)| {
                let key=parameter_fragments::reuse::Key { activation: crate::projection::Activation::Guaranteed,function:0,expression:expression as u32,
                    field:None,scalar,parent:Arc::new(Vec::new())};
                cache.parameter_fragments.get(&key).map(|values| (key,values))
            }).collect::<Vec<_>>();
        assert!(!fragments.is_empty());
        for (root,expected) in [(0,0),(5,2),(2,0),(3,2),(0,0),(5,2)] {
            let actual=project(view,roots[root],&mut cache).unwrap();
            assert_eq!(actual,vec![(false,0,expected)]);
            assert_eq!(actual,project(view,roots[root],&mut reference).unwrap());
            assert_eq!(cache.completed_folds,folds);
            for (key,values) in &fragments {
                assert!(Arc::ptr_eq(values,&cache.parameter_fragments.get(key).unwrap()));
            }
        }
        let profile_values=cache.function_results.iter().filter_map(|(key,value)| {
            if key.dependency.output==0 && matches!(value,FunctionSummaryEntry::Complete(_)) {
                Some(key.integers.iter().map(|i| i.value).collect::<Vec<_>>())
            } else { None }
        }).collect::<std::collections::BTreeSet<_>>();
        assert_eq!(profile_values,std::collections::BTreeSet::from([vec![1],vec![3]]));
        for (root,index) in [(6,0),(9,4)] {
            assert!(matches!(project(view,roots[root],&mut cache),Err(ProjectionError::IndexOutOfBounds {index:i,extent:3,..}) if i==index));
            assert_eq!(cache.completed_folds,folds);
        }
        // Runtime integer selection uses the original conservative direct walk.
        assert_eq!(project(view,roots[12],&mut cache).unwrap(),
            vec![(false,0,0),(false,0,1),(false,0,2),(true,1,0)]);
        assert_eq!(project(view,roots[0],&mut cache).unwrap(),vec![(false,0,0)]);
    });
}
