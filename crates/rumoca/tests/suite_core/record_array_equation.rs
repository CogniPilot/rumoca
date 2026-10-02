//! Equations between arrays of records (MLS §10.6.1) and zero-sized `der`
//! equations (MLS §3.7.4) inside a component, in the form the
//! Modelica.Fluid distributed pipe writes them:
//!
//! - `statesFM[1:n] = mediums[1:n].state` equates element records of a record
//!   array with the `state` record of each element of a model array. Each
//!   element pair is one whole-record equality; the slice and the structured
//!   family flattening attaches to it are not separate owners.
//! - `der(mCs[i, :]) = mbC_flows[i, :] ./ Medium.C_nominal` over a zero-sized
//!   trace-substance dimension states no scalar equations.

use rumoca::Compiler;

const SOURCE: &str = r#"
package RecordArrays
  record State
    Real p;
    Real T;
  end State;
  partial package Medium
    constant String names[:] = fill("", 0);
    constant Integer nC = size(names, 1);
    constant Real C_nominal[nC] = 1e-6*ones(nC);
  end Medium;
  package Water
    extends Medium;
  end Water;
  model Volume
    Real p;
    Real T;
    State state;
  equation
    state.p = p;
    state.T = T;
  end Volume;
  model Pipe
    replaceable package Medium = RecordArrays.Medium;
    parameter Integer nNodes = 2;
    final parameter Integer n = nNodes;
    Volume mediums[n];
    State[n] statesFM;
    Real mCs[n, Medium.nC];
    Real mbC_flows[n, Medium.nC];
    Real x(start = 1, fixed = true);
  equation
    statesFM[1:n] = mediums[1:n].state;
    for i in 1:n loop
      mediums[i].p = i*x;
      mediums[i].T = 300 + i;
      der(mCs[i, :]) = mbC_flows[i, :]./Medium.C_nominal;
    end for;
    der(x) = -statesFM[1].p;
  end Pipe;
  model Top
    Pipe pipe(redeclare package Medium = Water, nNodes = 3);
  end Top;
end RecordArrays;
"#;

#[test]
fn a_record_array_slice_equation_equates_each_element_record() {
    let compiled = Compiler::new()
        .model("RecordArrays.Top")
        .compile_str(SOURCE, "RecordArrays.mo")
        .unwrap_or_else(|error| panic!("RecordArrays.Top compiles: {error:?}"));
    assert!(
        compiled.balance_detail.is_balanced(),
        "the zero-sized der equations state no scalars and each record pair two"
    );
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.5,
            ..Default::default()
        },
    )
    .expect("RecordArrays.Top simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (index, _) in result.times.iter().enumerate() {
        let x = column("pipe.x")[index];
        for k in 1..=3 {
            let p = column(&format!("pipe.statesFM[{k}].p"))[index];
            let t = column(&format!("pipe.statesFM[{k}].T"))[index];
            assert!((p - k as f64 * x).abs() < 1e-9, "statesFM[{k}].p = {p}");
            assert!((t - (300 + k) as f64).abs() < 1e-9, "statesFM[{k}].T = {t}");
        }
    }
}
