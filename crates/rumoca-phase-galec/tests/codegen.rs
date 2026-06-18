use rumoca_phase_galec::{
    GalecBlock, GalecExpr, GalecInterface, GalecModel, GalecSampleTime, GalecStmt, GalecType,
    GalecVariable, GalecVariableRole, render_galec,
};

fn variable(name: &str, ty: GalecType, role: GalecVariableRole) -> GalecVariable {
    GalecVariable {
        name: name.to_string(),
        ty,
        role,
        description: None,
        start: None,
        min: None,
        max: None,
        nominal: None,
        unit: None,
    }
}

#[test]
fn renders_statement_level_if_for_first_tick_latch() {
    let model = GalecModel {
        name: "FirstTickLatch".to_string(),
        interface: GalecInterface {
            inputs: Vec::new(),
            outputs: vec![variable("y", GalecType::Real, GalecVariableRole::Output)],
            parameters: Vec::new(),
            states: vec![variable(
                "firstTick",
                GalecType::Boolean,
                GalecVariableRole::State,
            )],
        },
        sample_time: GalecSampleTime {
            period_seconds: 0.001,
            variable_name: "samplePeriod".to_string(),
        },
        declarations: Vec::new(),
        init: GalecBlock {
            statements: vec![
                GalecStmt::Assign {
                    lhs: "firstTick".to_string(),
                    rhs: GalecExpr::BooleanLiteral(true),
                },
                GalecStmt::Assign {
                    lhs: "y".to_string(),
                    rhs: GalecExpr::RealLiteral(0.0),
                },
            ],
        },
        recalibrate: GalecBlock::default(),
        step: GalecBlock {
            statements: vec![
                GalecStmt::If {
                    branches: vec![(
                        GalecExpr::Variable("firstTick".to_string()),
                        vec![GalecStmt::Assign {
                            lhs: "y".to_string(),
                            rhs: GalecExpr::RealLiteral(0.0),
                        }],
                    )],
                    else_branch: vec![GalecStmt::Assign {
                        lhs: "y".to_string(),
                        rhs: GalecExpr::RealLiteral(1.0),
                    }],
                },
                GalecStmt::Assign {
                    lhs: "firstTick".to_string(),
                    rhs: GalecExpr::BooleanLiteral(false),
                },
            ],
        },
    };

    let rendered = render_galec(&model).expect("statement-level if should render as GALEC");

    assert!(rendered.contains("if self.firstTick then"));
    assert!(rendered.contains("self.y := 0.0;"));
    assert!(rendered.contains("else"));
    assert!(rendered.contains("self.y := 1.0;"));
    assert!(rendered.contains("end if;"));
    assert!(rendered.contains("self.firstTick := false;"));
}
