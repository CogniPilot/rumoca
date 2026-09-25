//! MLS §9.4 regression: `Connections.rooted` and the `equalityConstraint` cut
//! describe one spanning tree (SPEC_0022 §3.12), whatever the order of the
//! `connect` statements.
//!
//! `Joint` propagates the overdetermined `Orientation` away from the root,
//! choosing its equations with `Connections.rooted(frame_a.R)` like
//! `Modelica.Mechanics.MultiBody.Joints.Revolute`, and records the chosen
//! branch in `dir`. The redundant field `c` is defined only on the side
//! farther from the root, so a joint whose branch disagrees with the cut
//! defines one orientation set twice and leaves another undefined. Three joints
//! close one loop at the ground frame; two are driven, and the third follows
//! from the loop's single `equalityConstraint` residual.

use rumoca::Compiler;
use rumoca_ir_flat::EquationOrigin;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const DECLARATIONS: &str = r#"
package VcgRooted
  record Orientation "planar orientation: an angle and its redundant cosine"
    Real phi;
    Real c;
    function equalityConstraint
      input Orientation R1;
      input Orientation R2;
      output Real residue[1];
    algorithm
      residue := {sin(R1.phi - R2.phi)};
    end equalityConstraint;
  end Orientation;

  connector Frame
    Orientation R;
    flow Real t;
  end Frame;

  model Ground
    Frame frame_b;
  equation
    Connections.root(frame_b.R);
    frame_b.R.phi = 0;
    frame_b.R.c = 1;
  end Ground;

  partial model Joint
    Frame frame_a;
    Frame frame_b;
    Real phi "Relative angle";
    Real dir "+1 when frame_a is the root side of the joint";
  equation
    Connections.branch(frame_a.R, frame_b.R);
    if Connections.rooted(frame_a.R) then
      frame_b.R.phi = frame_a.R.phi + phi;
      frame_b.R.c = cos(frame_b.R.phi);
      dir = 1;
    else
      frame_a.R.phi = frame_b.R.phi - phi;
      frame_a.R.c = cos(frame_a.R.phi);
      dir = -1;
    end if;
    frame_a.t + frame_b.t = 0;
  end Joint;

  model DrivenJoint
    extends Joint;
    parameter Real rate;
  equation
    phi = rate*time;
  end DrivenJoint;

  model FreeJoint
    extends Joint;
  equation
    frame_a.t = 0;
  end FreeJoint;

  model Loop
    Ground ground;
    DrivenJoint j1(rate = 0.2);
    DrivenJoint j2(rate = 0.3);
    FreeJoint j3;
  equation
"#;

const CONNECTS: [(&str, &str); 4] = [
    ("ground.frame_b", "j1.frame_a"),
    ("j1.frame_b", "j2.frame_a"),
    ("j2.frame_b", "j3.frame_a"),
    ("j3.frame_b", "ground.frame_b"),
];

fn source(connects: &[(&str, &str)]) -> String {
    let mut text = DECLARATIONS.to_string();
    for (lhs, rhs) in connects {
        text.push_str(&format!("    connect({lhs}, {rhs});\n"));
    }
    text.push_str("  end Loop;\nend VcgRooted;\n");
    text
}

/// Every ordering of the four `connect` statements, each also written with
/// swapped arguments.
fn orderings() -> Vec<Vec<(&'static str, &'static str)>> {
    let mut orderings = Vec::new();
    let mut indices = [0, 1, 2, 3];
    permute(&mut indices, 0, &mut |order| {
        let forward = order.iter().map(|&i| CONNECTS[i]).collect::<Vec<_>>();
        let swapped = forward.iter().map(|&(lhs, rhs)| (rhs, lhs)).collect();
        orderings.push(forward);
        orderings.push(swapped);
    });
    orderings
}

fn permute(indices: &mut [usize; 4], start: usize, visit: &mut impl FnMut(&[usize; 4])) {
    if start == indices.len() {
        visit(indices);
        return;
    }
    for next in start..indices.len() {
        indices.swap(start, next);
        permute(indices, start + 1, visit);
        indices.swap(start, next);
    }
}

/// The two records the single `equalityConstraint` relates, in name order.
fn broken_edge(flat: &rumoca_ir_flat::Model) -> (String, String) {
    let calls = flat
        .equations
        .iter()
        .filter_map(|equation| match &equation.origin {
            EquationOrigin::Connection { rhs, .. } if rhs.contains("equalityConstraint") => {
                Some(rhs.clone())
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(
        calls.len(),
        1,
        "one loop, one equalityConstraint: {calls:?}"
    );
    let arguments = calls[0]
        .trim_end_matches(')')
        .split_once('(')
        .expect("equalityConstraint origin names its records")
        .1;
    let (lhs, rhs) = arguments.split_once(", ").expect("two record arguments");
    let mut pair = [lhs.to_string(), rhs.to_string()];
    pair.sort();
    let [lhs, rhs] = pair;
    (lhs, rhs)
}

/// `dir` each joint must take: +1 exactly when its `frame_a` stays connected
/// to the ground once the broken edge is removed from the loop
/// ground -- j1 -- j2 -- j3 -- ground.
fn expected_directions(broken: &(String, String)) -> [f64; 3] {
    let edge = |lhs: &str, rhs: &str| {
        let mut pair = [format!("{lhs}.R"), format!("{rhs}.R")];
        pair.sort();
        (pair[0].clone(), pair[1].clone()) == *broken
    };
    // Position of the cut along the loop: edge k joins joint k and joint k+1,
    // with the ground as joints 0 and 4.
    let cut = CONNECTS
        .iter()
        .position(|(lhs, rhs)| edge(lhs, rhs))
        .expect("the broken edge is one of the loop's connects");
    [1, 2, 3].map(|joint| if joint <= cut { 1.0 } else { -1.0 })
}

#[test]
fn rooted_branches_agree_with_the_emitted_cut_for_every_connect_order() {
    let mut cuts = std::collections::BTreeSet::new();
    for connects in orderings() {
        let compiled = match Compiler::new()
            .model("VcgRooted.Loop")
            .compile_str(&source(&connects), "vcg_rooted.mo")
        {
            Ok(compiled) => compiled,
            Err(error) => panic!("{connects:?}: {error}"),
        };
        let broken = broken_edge(&compiled.flat);
        let directions = expected_directions(&broken);
        cuts.insert(broken);

        let result = match simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 1.0,
                dt: Some(0.25),
                ..Default::default()
            },
        ) {
            Ok(result) => result,
            Err(error) => panic!("{connects:?}: {error}"),
        };
        let series = |name: &str| {
            let Some(index) = result.names.iter().position(|candidate| candidate == name) else {
                panic!("missing {name}");
            };
            &result.data[index]
        };
        for (joint, expected) in ["j1", "j2", "j3"].into_iter().zip(directions) {
            assert!(
                series(&format!("{joint}.dir"))
                    .iter()
                    .all(|&dir| dir == expected),
                "{connects:?}: {joint} branch disagrees with the cut"
            );
        }
        let (closing, times) = (series("j3.phi"), &result.times);
        for (&t, &phi) in times.iter().zip(closing) {
            assert!(
                (phi + 0.5 * t).abs() < 1e-8,
                "{connects:?}: loop closure j3.phi({t}) = {phi}"
            );
        }
    }
    assert_eq!(
        cuts.into_iter().collect::<Vec<_>>(),
        [("j2.frame_b.R".to_string(), "j3.frame_a.R".to_string())],
        "the cut must not depend on connect order"
    );
}
