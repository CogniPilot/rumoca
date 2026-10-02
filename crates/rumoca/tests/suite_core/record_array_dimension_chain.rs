//! An array dimension is evaluable at translation (MLS §10.1) through any
//! chain of parameter bindings, including bindings that select on enumeration
//! comparisons. Integer, Boolean, and enumeration evaluation at instantiation
//! share one recursion depth, so the enumeration comparison reached through
//! `nFM -> nFMDistributed` must be judged against the same bound as the
//! integer chain that reached it. A record array whose dimension is evaluated
//! expands into one record per element, `statesFM[k].p`, instead of keeping
//! one array coordinate per field.

use rumoca::Compiler;
use rumoca_core::VarName;

const SOURCE: &str = r#"
package DimensionChain
  record State
    Real p;
    Real T;
  end State;
  package Types
    type Structure = enumeration(a_v_b, a_vb, av_b, av_vb);
  end Types;
  model Base
    parameter Integer nNodes(min = 1) = 2 annotation(Evaluate = true);
    final parameter Integer n = nNodes;
  end Base;
  model Pipe
    extends Base;
    parameter Types.Structure modelStructure = Types.Structure.av_vb annotation(Evaluate = true);
    parameter Boolean useLumpedPressure = false annotation(Evaluate = true);
    final parameter Integer nFM = if useLumpedPressure then nFMLumped else nFMDistributed;
    final parameter Integer nFMDistributed = if modelStructure == Types.Structure.a_v_b then n + 1
      else if (modelStructure == Types.Structure.a_vb or modelStructure == Types.Structure.av_b) then n
      else n - 1;
    final parameter Integer nFMLumped = if modelStructure == Types.Structure.a_v_b then 2 else 1;
    State[nFM + 1] states;
  equation
    for i in 1:nFM + 1 loop
      states[i].p = i;
      states[i].T = 300;
    end for;
  end Pipe;
  model Top
    Pipe pipe(nNodes = 3);
  end Top;
end DimensionChain;
"#;

#[test]
fn a_record_array_dimension_through_an_enumeration_chain_expands() {
    let flat = Compiler::new()
        .model("DimensionChain.Top")
        .compile_str_flat(SOURCE, "DimensionChain.mo")
        .unwrap_or_else(|error| panic!("DimensionChain.Top flattens: {error:?}"));
    // nNodes = 3 under av_vb gives nFM = n - 1 = 2, so three records.
    for k in 1..=3 {
        for field in ["p", "T"] {
            let name = format!("pipe.states[{k}].{field}");
            assert!(
                flat.variables.contains_key(&VarName::new(&name)),
                "{name} missing; have {:?}",
                flat.variables.keys().collect::<Vec<_>>()
            );
        }
    }
    assert!(
        !flat
            .variables
            .contains_key(&VarName::new("pipe.states[4].p"))
    );
    assert!(!flat.variables.contains_key(&VarName::new("pipe.states.p")));
}
