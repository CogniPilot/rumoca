//! Interactive real-time session demo.
//!
//! Simulates a mass-spring-damper with keyboard control:
//!   Left/Right arrows apply force, space resets force to zero.
//!
//! Usage:
//!   cargo run --example simulation_session_demo -p rumoca

use std::io::{Write, stdout};
use std::time::{Duration, Instant};

use crossterm::{
    cursor,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{self, ClearType},
};
use rumoca_sim::{SimOptions, SimulationSession};

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
    let compiler = rumoca::Compiler::new().model("MassSpringDamperControl");
    let result = compiler.compile_str(MODEL_SOURCE, "demo.mo")?;
    let mut session = SimulationSession::new(result.dae().as_ref(), SimOptions::default())?;

    println!("Inputs:  {:?}", session.input_names());
    println!("Variables: {:?}", session.variable_names());
    println!();
    println!("Controls: Left/Right = apply force, Space = zero force, q = quit");
    println!();

    terminal::enable_raw_mode()?;
    let mut stdout = stdout();
    let run_result = run_demo(&mut session, &mut stdout);
    let restore_result = terminal::disable_raw_mode();
    run_result?;
    restore_result?;
    println!();
    println!("Done.");
    Ok(())
}

fn run_demo(session: &mut SimulationSession, stdout: &mut impl Write) -> anyhow::Result<()> {
    let mut force: f64 = 0.0;
    loop {
        let step_start = Instant::now();
        if !poll_controls(&mut force)? {
            break;
        }

        session.set_input("u", force)?;
        session.advance_to(session.time() + DT)?;
        render_state(stdout, session, force)?;
        wait_for_step(step_start);
    }
    Ok(())
}

fn poll_controls(force: &mut f64) -> anyhow::Result<bool> {
    let mut running = true;
    while event::poll(Duration::from_millis(0))? {
        let Event::Key(KeyEvent {
            code, modifiers, ..
        }) = event::read()?
        else {
            continue;
        };
        match code {
            KeyCode::Left => *force -= FORCE_STEP,
            KeyCode::Right => *force += FORCE_STEP,
            KeyCode::Char(' ') => *force = 0.0,
            KeyCode::Char('q') | KeyCode::Esc => running = false,
            KeyCode::Char('c') if modifiers.contains(KeyModifiers::CONTROL) => running = false,
            _ => {}
        }
    }
    Ok(running)
}

fn read_visible(session: &SimulationSession, name: &str) -> anyhow::Result<f64> {
    session
        .get(name)?
        .ok_or_else(|| anyhow::anyhow!("session variable '{name}' is not visible"))
}

fn render_state(
    stdout: &mut impl Write,
    session: &SimulationSession,
    force: f64,
) -> anyhow::Result<()> {
    let time = session.time();
    let x = read_visible(session, "x")?;
    let v = read_visible(session, "v")?;
    let bar_width = 60i32;
    let center = bar_width / 2;
    let pos = (center as f64 + x * 8.0).round() as i32;
    let pos = pos.clamp(0, bar_width - 1);

    let mut bar = vec![b' '; bar_width as usize];
    bar[center as usize] = b'|';
    bar[pos as usize] = b'O';
    let bar = std::str::from_utf8(&bar)?;

    execute!(
        stdout,
        cursor::MoveToColumn(0),
        terminal::Clear(ClearType::CurrentLine)
    )?;
    write!(
        stdout,
        "t={:.2}  x={:+.3}  v={:+.3}  u={:+.1}  [{}]",
        time, x, v, force, bar
    )?;
    stdout.flush()?;
    Ok(())
}

fn wait_for_step(step_start: Instant) {
    let elapsed = step_start.elapsed();
    let target = Duration::from_secs_f64(DT);
    if elapsed < target {
        std::thread::sleep(target - elapsed);
    }
}
