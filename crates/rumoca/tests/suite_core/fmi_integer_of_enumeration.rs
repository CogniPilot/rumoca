//! `Integer(e)` of an enumeration inside a function body is the ordinal and
//! must render for the FMI C targets, which build function bodies as typed
//! programs. `integer(x)` on a Real still floors.

const SOURCE: &str = r#"package EnumOrdinal
  type Mode = enumeration(Off, Low, High);

  function ordinal
    input Mode m;
    output Integer k;
  algorithm
    k := Integer(m);
  end ordinal;

  function floored
    input Real x;
    output Integer k;
  algorithm
    k := integer(x);
  end floored;

  model IntegerOfEnum
    parameter Mode m = Mode.High;
    parameter Real p = -1.5;
    discrete Integer k(start = 0, fixed = true);
    discrete Integer f(start = 0, fixed = true);
    Real x(start = 0, fixed = true);
  equation
    der(x) = 1;
    when time > 0.5 then
      k = ordinal(m);
      f = floored(p);
    end when;
  end IntegerOfEnum;
end EnumOrdinal;
"#;

const MODEL: &str = "EnumOrdinal.IntegerOfEnum";

#[test]
fn integer_of_enumeration_in_a_function_renders_for_both_fmi_targets() {
    let compiled = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, "EnumOrdinal.mo")
        .expect("compile Integer(e) in a function body");
    for target in ["fmi3", "fmi2"] {
        let files = rumoca::render_target_files(&compiled, MODEL, target, None)
            .unwrap_or_else(|error| panic!("render {target}: {error:#}"));
        assert!(files.iter().any(|file| file.path == "sources/model.c"));
        assert!(files.iter().any(|file| file.path == "modelDescription.xml"));
    }
}
