//! Regression: a conditional `stateSelect` attribute referencing enclosing
//! package constants (MLS §5.3.2 lexical lookup) must evaluate instead of
//! failing EI032. This is the MSL Media pattern
//! `T(stateSelect = if pT_explicit and preferredMediumStates then ...)`.

use rumoca::Compiler;

const ENCLOSING_CONSTANT_MODEL: &str = r#"
package P
  constant Boolean pT_explicit = true;
  constant Boolean ph_explicit = false;
  model Base
    parameter Boolean preferredMediumStates = false;
    Real T(
      stateSelect = if (pT_explicit or ph_explicit) and preferredMediumStates
        then StateSelect.prefer else StateSelect.default,
      start = 300,
      fixed = true);
  equation
    der(T) = 1;
  end Base;
end P;
model StateSel
  P.Base b(preferredMediumStates = true);
end StateSel;
"#;

#[test]
fn state_select_conditional_on_enclosing_constants_evaluates() {
    let result = Compiler::new()
        .model("StateSel")
        .compile_str(ENCLOSING_CONSTANT_MODEL, "StateSel.mo")
        .expect("conditional stateSelect over enclosing constants must compile");
    assert_eq!(result.dae.variables.states.len(), 1);
}

const QUALIFIED_CONSTANT_MODEL: &str = r#"
package P
  constant Boolean pT_explicit = true;
  model Base
    Real T(
      stateSelect = if P.pT_explicit then StateSelect.prefer else StateSelect.default,
      start = 300,
      fixed = true);
  equation
    der(T) = 1;
  end Base;
end P;
model StateSelQ
  P.Base b;
end StateSelQ;
"#;

#[test]
fn state_select_conditional_on_qualified_class_constant_evaluates() {
    let result = Compiler::new()
        .model("StateSelQ")
        .compile_str(QUALIFIED_CONSTANT_MODEL, "StateSelQ.mo")
        .expect("conditional stateSelect over a qualified class constant must compile");
    assert_eq!(result.dae.variables.states.len(), 1);
}
