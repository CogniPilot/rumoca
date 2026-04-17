#![allow(clippy::excessive_nesting)]
//! Interactive real-time stepper demo.
//!
//! Simulates a mass-spring-damper with keyboard control:
//!   Left/Right arrows apply force, space resets force to zero.
//!
//! Usage:
//!   cargo run --example stepper_demo -p rumoca

use std::io::{Write, stdout};
use std::time::{Duration, Instant};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{self, ClearType},
};
use rumoca_sim_diffsol::{SimStepper, StepperOptions};

const MODEL_SOURCE: &str = r#"
model MassSpringDamperControl
  Real x(start = 1) "Position";
  Real v(start = 0) "Velocity";
  input Real u "Control force";
  parameter Real m = 1.0 "Mass";
  parameter Real k = 1.0 "Spring stiffness";
  parameter Real c = 0.5 "Damping";
equation
  der(x) = v;
  m * der(v) = -k * x - c * v + u;
end MassSpringDamperControl;
"#;

const DT: f64 = 0.02; // 50 Hz simulation
const FORCE_STEP: f64 = 2.0;

fn main() -> anyhow::Result<()> {
    // Compile the model
    let compiler = rumoca::Compiler::new().model("MassSpringDamperControl");
    let result = compiler.compile_str(MODEL_SOURCE, "demo.mo")?;

    // Create the stepper
    let mut stepper = SimStepper::new(&result.dae, StepperOptions::default())?;

    println!("Inputs:  {:?}", stepper.input_names());
    println!("Variables: {:?}", stepper.variable_names());
    println!();
    println!("Controls: Left/Right = apply force, Space = zero force, q = quit");
    println!();

    // Enter raw terminal mode
    terminal::enable_raw_mode()?;
    let mut stdout = stdout();

    let mut force: f64 = 0.0;
    let mut running = true;

    while running {
        let step_start = Instant::now();

        // Poll for keyboard input (non-blocking)
        while event::poll(Duration::from_millis(0))? {
            if let Event::Key(KeyEvent {
                code, modifiers, ..
            }) = event::read()?
            {
                match code {
                    KeyCode::Left => force -= FORCE_STEP,
                    KeyCode::Right => force += FORCE_STEP,
                    KeyCode::Char(' ') => force = 0.0,
                    KeyCode::Char('q') | KeyCode::Esc => running = false,
                    KeyCode::Char('c') if modifiers.contains(KeyModifiers::CONTROL) => {
                        running = false
                    }
                    _ => {}
                }
            }
        }

        if !running {
            break;
        }

        // Apply input and step
        stepper.set_input("u", force)?;
        stepper.step(DT)?;

        let t = stepper.time();
        let x = stepper.get("x").unwrap_or(0.0);
        let v = stepper.get("v").unwrap_or(0.0);

        // Render a simple ASCII visualization
        let bar_width = 60i32;
        let center = bar_width / 2;
        let pos = (center as f64 + x * 8.0).round() as i32;
        let pos = pos.clamp(0, bar_width - 1);

        let mut bar = vec![b' '; bar_width as usize];
        bar[center as usize] = b'|'; // equilibrium
        bar[pos as usize] = b'O'; // mass

        execute!(
            stdout,
            cursor::MoveToColumn(0),
            terminal::Clear(ClearType::CurrentLine)
        )?;
        write!(
            stdout,
            "t={:.2}  x={:+.3}  v={:+.3}  u={:+.1}  [{}]",
            t,
            x,
            v,
            force,
            std::str::from_utf8(&bar).unwrap()
        )?;
        stdout.flush()?;

        // Wait for the remainder of the time step
        let elapsed = step_start.elapsed();
        let target = Duration::from_secs_f64(DT);
        if elapsed < target {
            std::thread::sleep(target - elapsed);
        }
    }

    // Restore terminal
    terminal::disable_raw_mode()?;
    println!();
    println!("Done.");
    Ok(())
}
