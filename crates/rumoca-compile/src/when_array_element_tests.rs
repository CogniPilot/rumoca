use crate::{Session, SessionConfig};

const SOURCE: &str = r#"
model ArrayEvents
  Real x[3](each start=0, each fixed=true);
  Boolean e[3](each start=false, each fixed=true);
equation
  for i in 1:3 loop
    der(x[i])=i;
    when x[i]>1 then
      e[i]=true;
    end when;
  end for;
end ArrayEvents;
"#;

#[test]
fn primitive_array_when_coordinates_retain_one_declaration_and_independent_owners() {
    let mut session = Session::new(SessionConfig::default());
    session.add_document("ArrayEvents.mo", SOURCE).unwrap();
    let compiled = session.compile_model("ArrayEvents").unwrap();
    compiled.dae.inspect(|view| {
        let e = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "e")
            .unwrap()
            .0;
        assert_eq!(view.variable(e).unwrap().scalar_count(), 3);
        assert_eq!(view.discrete_value_owner_count(), 3);
        let mut coordinates = Vec::new();
        let mut triggers = Vec::new();
        for ordinal in 0..view.discrete_value_owner_count() {
            let owner = view
                .discrete_value_owner(view.discrete_value_owner_id(ordinal).unwrap())
                .unwrap();
            let target = owner.targets().get(0).unwrap();
            assert_eq!(target.index(), e.index());
            coordinates.push(target.scalar_selection().unwrap());
            let rumoca_ir_dae::DiscreteBranchActivation::When { trigger, .. } =
                owner.branches().get(0).unwrap().activation()
            else {
                panic!("each element keeps its source event activation")
            };
            triggers.push(trigger.index());
        }
        coordinates.sort_unstable();
        triggers.sort_unstable();
        triggers.dedup();
        assert_eq!(coordinates, [0, 1, 2]);
        assert_eq!(triggers.len(), 3);
    });
}

#[test]
fn partial_array_when_definition_issues_explicit_held_element_owners() {
    let source = "model Partial Boolean e[3](each start=false,each fixed=true); equation when time>0.1 then e[2]=true; end when; end Partial;";
    let mut session = Session::new(SessionConfig::default());
    session.add_document("Partial.mo", source).unwrap();
    let compiled = session.compile_model("Partial").unwrap();
    compiled.dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 3);
        let holds = (0..3)
            .filter(|&ordinal| {
                let owner = view
                    .discrete_value_owner(view.discrete_value_owner_id(ordinal).unwrap())
                    .unwrap();
                matches!(
                    owner.branches().get(0).unwrap().activation(),
                    rumoca_ir_dae::DiscreteBranchActivation::Always
                )
            })
            .count();
        assert_eq!(holds, 2);
    });
}

#[test]
fn invalid_array_when_coordinates_and_overlapping_owners_are_refused() {
    for (equations, diagnostic) in [
        (
            "when time>0.1 then e[0]=true; end when;",
            "ToDae error [ED019]",
        ),
        (
            "when time>0.1 then e[4]=true; end when;",
            "ToDae error [ED019]",
        ),
        (
            "when time>0.1 then e[1,1]=true; end when;",
            "ToDae error [ED019]",
        ),
        (
            "when time>0.1 then e[1]=true; end when; when time>0.2 then e[1]=false; end when;",
            "ToDae error [ED010]",
        ),
        (
            "when time>0.1 then e={true,true,true}; end when; when time>0.2 then e[1]=false; end when;",
            "ToDae error [ED010]",
        ),
    ] {
        let source = format!(
            "model Invalid Boolean e[3](each start=false,each fixed=true); equation {equations} end Invalid;"
        );
        let mut session = Session::new(SessionConfig::default());
        session.add_document("Invalid.mo", &source).unwrap();
        let error = session.compile_model("Invalid").unwrap_err().to_string();
        assert!(error.contains(diagnostic), "{equations}: {error}");
    }
}

#[test]
fn selected_current_reads_order_independent_array_element_owners() {
    let source = "model Dependency Boolean e[2](each start=false,each fixed=true); equation when time>0.1 then e[2]=e[1]; end when; when time>0.1 then e[1]=true; end when; end Dependency;";
    let mut session = Session::new(SessionConfig::default());
    session.add_document("Dependency.mo", source).unwrap();
    let compiled = session.compile_model("Dependency").unwrap();
    compiled.dae.inspect(|view| {
        let first = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(first.targets().get(0).unwrap().scalar_selection(), Some(0));
    });
}

#[test]
fn selected_current_dependency_cycles_are_refused() {
    let source = "model Cycle Boolean e[2](each start=false,each fixed=true); equation when time>0.1 then e[2]=e[1]; end when; when time>0.1 then e[1]=e[2]; end when; end Cycle;";
    let mut session = Session::new(SessionConfig::default());
    session.add_document("Cycle.mo", source).unwrap();
    let error = session.compile_model("Cycle").unwrap_err().to_string();
    assert!(error.contains("ToDae error [ED010]"), "{error}");
}

#[test]
fn selected_when_branch_mismatch_and_repeated_branch_target_are_refused() {
    for body in [
        "when time>0.1 then e[1]=true; elsewhen time>0.2 then e[2]=true; end when;",
        "when time>0.1 then if time>0.2 then e[1]=true; else e[2]=true; end if; end when;",
        "when time>0.1 then e[1]=true; e[1]=false; end when;",
    ] {
        let source = format!(
            "model BadBranches Boolean e[2](each start=false,each fixed=true); equation {body} end BadBranches;"
        );
        let mut session = Session::new(SessionConfig::default());
        session.add_document("BadBranches.mo", &source).unwrap();
        let error = session
            .compile_model("BadBranches")
            .unwrap_err()
            .to_string();
        assert!(error.contains("ToDae error [ED010]"), "{body}: {error}");
    }
}

#[test]
fn zero_extent_array_and_empty_event_loop_have_no_element_owners() {
    let source = "model Empty Boolean e[0]; equation for i in 1:0 loop when time>1 then e[i]=true; end when; end for; end Empty;";
    let mut session = Session::new(SessionConfig::default());
    session.add_document("Empty.mo", source).unwrap();
    let compiled = session.compile_model("Empty").unwrap();
    compiled.dae.inspect(|view| {
        let variable = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "e")
            .unwrap()
            .1;
        assert_eq!(variable.scalar_count(), 0);
        assert_eq!(view.root_count(), 0);
        for ordinal in 0..view.discrete_value_owner_count() {
            let owner = view
                .discrete_value_owner(view.discrete_value_owner_id(ordinal).unwrap())
                .unwrap();
            assert!(
                owner
                    .targets()
                    .iter()
                    .all(|target| target.scalar_selection().is_none())
            );
        }
    });
}

#[test]
fn matrix_and_non_boolean_array_event_coordinates_compile() {
    let source = r#"
model MatrixEvents
  type Mode=enumeration(Off,On);
  Boolean e[2,2](each start=false,each fixed=true);
  Integer k[2](each start=0,each fixed=true);
  Mode m[2](each start=Mode.Off,each fixed=true);
  discrete Real z[2](each start=0,each fixed=true);
equation
  for i in 1:2 loop
    for j in 1:2 loop
      when time>0.1*i+0.01*j then e[i,j]=true; end when;
    end for;
    when time>0.1*i then k[i]=i; m[i]=Mode.On; z[i]=i; end when;
  end for;
end MatrixEvents;
"#;
    let mut session = Session::new(SessionConfig::default());
    session.add_document("MatrixEvents.mo", source).unwrap();
    session.compile_model("MatrixEvents").unwrap();
}

#[test]
fn parameter_expression_target_is_fixed_at_translation() {
    let source = "model ParameterTarget parameter Integer k=1; Boolean e[3](each start=false,each fixed=true); equation when time>0.1 then e[k+1]=true; end when; end ParameterTarget;";
    let mut session = Session::new(SessionConfig::default());
    session.add_document("ParameterTarget.mo", source).unwrap();
    let compiled = session.compile_model("ParameterTarget").unwrap();
    compiled.dae.inspect(|view| {
        let parameter = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "k")
            .unwrap()
            .1;
        assert!(parameter.is_evaluable());
    });
}
