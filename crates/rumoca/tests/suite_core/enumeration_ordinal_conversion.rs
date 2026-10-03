//! `Integer(e)` of an enumeration value is its ordinal (MLS §4.9.5).
//!
//! It is a type conversion, not the event-generating `integer(x)` of a Real
//! argument (MLS §3.7.2), so it owns no step root. The transport delay of
//! `Modelica.Electrical.Digital.Delay.TransportDelay` reads `Integer(pre(x))`
//! of its Logic input.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
package P
  type Logic = enumeration('U', 'X', '0', '1');
  model Ordinals
    Logic x(start = Logic.'U', fixed = true);
    Integer n;
    Real xr;
  equation
    x = if time < 0.3 then Logic.'0' else Logic.'1';
    n = Integer(x);
    xr = Integer(pre(x));
  end Ordinals;
end P;
"#;

#[test]
fn integer_of_an_enumeration_is_its_ordinal() {
    let compiled = Compiler::new()
        .model("P.Ordinals")
        .compile_str(SOURCE, "EnumerationOrdinalConversion.mo")
        .unwrap_or_else(|error| panic!("P.Ordinals compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            ..SimOptions::default()
        },
    )
    .expect("P.Ordinals simulates");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("P.Ordinals exposes {name}"));
        &result.data[index]
    };
    let (n, xr) = (column("n"), column("xr"));
    assert_eq!(n.first(), Some(&3.0), "'0' is the third literal");
    assert_eq!(n.last(), Some(&4.0), "'1' is the fourth literal");
    assert_eq!(
        xr.last(),
        Some(&4.0),
        "pre(x) has settled to '1' after the event"
    );
}
