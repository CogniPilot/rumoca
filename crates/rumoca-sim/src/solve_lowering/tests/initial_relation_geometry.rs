use super::compile;
use crate::SimOptions;
use rumoca_solver::runtime::solve_runtime::SolveRuntime;

fn initial_geometry(source: &str) -> (rumoca_ir_solve::SolveModel, Vec<f64>, Vec<f64>) {
    let dae = compile(source, "InitialGeometry");
    let model =
        super::super::entry::lower_dae_for_simulation(&dae, &SimOptions::default()).unwrap();
    let runtime = SolveRuntime::new(&model).unwrap();
    let mut y = model.initial_y.to_vec();
    let mut p = model.parameters.to_vec();
    runtime.set_initial_event_flag(&mut p, true);
    runtime.initialize_delay_history(0.0, &y, &mut p).unwrap();
    runtime
        .settle_initialization_system(&mut y, &mut p, 0.0, 1e-10, 64)
        .unwrap();
    (model, y, p)
}

fn assert_selected_geometry(simplified: f64, shifted: bool) {
    let declaration = if shifted { "Real v[3];" } else { "" };
    let equation = if shifted { "v={1.0,2.0,3.0}*x;" } else { "" };
    let source = format!(
        r#"model InitialGeometry
      {declaration}
      Real x; Real vin; Real y(start=0, fixed=true);
    equation
      {equation}
      vin=0.4*x;
      x=homotopy(actual=if 15000*vin>15.0 then 15.0
        else if 15000*vin < -15.0 then -15.0 else 15000*vin,
        simplified={simplified});
      der(y)=x;
    end InitialGeometry;"#
    );
    let (model, y, p) = initial_geometry(&source);
    let coordinate = |name| match model.problem.layout.binding(name).unwrap() {
        rumoca_ir_solve::ScalarSlot::Y { index, .. } => y[index],
        _ => panic!("the source Real has a solver coordinate"),
    };
    assert!((coordinate("x") - simplified).abs() < 1e-9);
    assert!((coordinate("vin") - 0.4 * simplified).abs() < 1e-9);
    if shifted {
        let rumoca_ir_solve::ScalarSlot::Y { index, .. } =
            model.problem.layout.binding("v").unwrap()
        else {
            panic!("the vector retains its declared solver range")
        };
        assert!((y[index + 2] - 3.0 * simplified).abs() < 1e-9);
    }
    let memory = &model.problem.solve_layout.relation_memory_parameter_indices;
    assert_eq!(
        memory.len(),
        2,
        "both authored relations retain their buffers"
    );
    assert_eq!(p[memory[0]], if simplified > 0.0 { 1.0 } else { 0.0 });
    assert_eq!(p[memory[1]], if simplified < 0.0 { 1.0 } else { 0.0 });
}

#[test]
fn homotopy_lower_limit_preserves_its_relation_geometry() {
    assert_selected_geometry(-15.0, false);
}

#[test]
fn homotopy_upper_limit_preserves_its_relation_geometry() {
    assert_selected_geometry(15.0, false);
}

#[test]
fn homotopy_relation_geometry_survives_shifted_output_placement() {
    assert_selected_geometry(-15.0, true);
}

#[test]
fn without_homotopy_the_root_cone_still_settles_before_memory_reads() {
    let (model, y, p) = initial_geometry(
        r#"model InitialGeometry
      Real clock(start=0,fixed=true);
      Real x(start=0);
      Boolean positive;
    equation
      der(clock)=0;
      x=exp(clock)+2;
      positive=x>1;
    end InitialGeometry;"#,
    );
    assert!(
        model
            .problem
            .solve_layout
            .initial_homotopy_parameter_index
            .is_none()
    );
    let rumoca_ir_solve::ScalarSlot::Y { index, .. } = model.problem.layout.binding("x").unwrap()
    else {
        panic!("the algebraic has its declared solver coordinate")
    };
    assert_eq!(y[index], 3.0);
    let memory = &model.problem.solve_layout.relation_memory_parameter_indices;
    assert_eq!(memory.len(), 1);
    assert_eq!(p[memory[0]], 1.0);
}

#[test]
fn unchanged_relation_truth_still_resettles_delay_at_the_new_geometry() {
    let (model, y, p) = initial_geometry(
        r#"model InitialGeometry
      Real clock(start=0,fixed=true);
      Real x(start=0);
      Real delayed;
      Boolean small;
    equation
      der(clock)=0;
      x=exp(clock)+2;
      delayed=delay(x,0.01);
      small=x<1;
    end InitialGeometry;"#,
    );
    let rumoca_ir_solve::ScalarSlot::Y { index, .. } = model.problem.layout.binding("x").unwrap()
    else {
        panic!("the algebraic has its declared solver coordinate")
    };
    assert_eq!(y[index], 3.0);
    let memory = &model.problem.solve_layout.relation_memory_parameter_indices;
    assert_eq!(memory.len(), 1);
    assert_eq!(p[memory[0]], 0.0);
    let delayed = &model.problem.events.delays.value_parameter_indices;
    assert_eq!(delayed.len(), 1);
    assert_eq!(
        p[delayed[0]], y[index],
        "initial delay is the current source"
    );
}
