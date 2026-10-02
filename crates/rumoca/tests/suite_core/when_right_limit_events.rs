//! A dynamic time event is observed at its right limit (`time >= c` holds
//! exactly at `c`). That observation is the same event, so it starts again
//! from the event-entry discrete values: a `when` body runs once, and a
//! condition that fell during the event is not seen to rise. OpenModelica
//! keeps `t_start = 0` in both models.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model RightLimit
  Boolean enableFire;
  discrete Real t_start(start = 0, fixed = true);
  Boolean done(start = false, fixed = true);
equation
  enableFire = not pre(done);
  when enableFire then
    t_start = time;
  end when;
  when time >= 1 then
    done = true;
  end when;
end RightLimit;
model Timer
  parameter Real waitTime = 1;
  Boolean enableFire;
  discrete Real t_start;
  Boolean fire;
  Boolean done(start = false, fixed = true);
initial equation
  pre(t_start) = time;
  pre(enableFire) = false;
equation
  enableFire = not pre(done);
  when enableFire then
    t_start = time;
  end when;
  fire = enableFire and time >= t_start + waitTime;
  when fire then
    done = true;
  end when;
end Timer;
"#;

#[test]
fn a_right_limit_observation_runs_each_when_body_once() {
    for model in ["RightLimit", "Timer"] {
        let compiled = Compiler::new()
            .model(model)
            .compile_str(SOURCE, "RightLimit.mo")
            .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 2.0,
                ..SimOptions::default()
            },
        )
        .unwrap_or_else(|error| panic!("{model} simulates: {error}"));
        let last = |name: &str| {
            let index = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("{name} is recorded"));
            *result.data[index].last().expect("samples")
        };
        assert_eq!(last("t_start"), 0.0, "{model}: the when body ran again");
        assert_eq!(last("done"), 1.0, "{model}");
        assert_eq!(last("enableFire"), 0.0, "{model}");
    }
}
