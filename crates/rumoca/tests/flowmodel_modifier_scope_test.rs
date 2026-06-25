//! Regression tests for FlowModel modifier scope resolution.

#[test]
fn test_flowmodel_modifier_keeps_enclosing_port_scope() {
    let source = r#"
        package Medium
          function setState_p
            input Real p;
            output Real s;
          algorithm
            s := p;
          end setState_p;
        end Medium;

        connector FluidPort
          Real p;
          flow Real m_flow;
        end FluidPort;

        model FlowModel
          parameter Real states[2];
          Real m_flows[1];
        end FlowModel;

        model StaticPipe
          FluidPort port_a;
          FluidPort port_b;
          FlowModel flowModel(states={
            Medium.setState_p(port_a.p),
            Medium.setState_p(port_b.p)});
        equation
          port_a.m_flow = flowModel.m_flows[1];
        end StaticPipe;

        model Top
          StaticPipe pipe;
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("Top should compile");

    let flat_dump = format!("{:#?}", compiled.flat);
    assert!(
        !flat_dump.contains("pipe.flowModel.port_a.p"),
        "flowModel modifier should resolve port_a.p in enclosing scope, got over-qualified ref"
    );
    assert!(
        flat_dump.contains("pipe.port_a.p"),
        "expected canonical enclosing connector pressure path in flattened model"
    );
}
