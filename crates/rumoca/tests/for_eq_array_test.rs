//! Test for for-equation parameter lookup in array components.

/// Test with MSL-style imports and SISO.
#[test]
fn test_msl_style_imports() {
    // Add import statements like MSL
    let source = r#"
package Modelica
    package Constants
        constant Real pi = 3.141592653589793;
    end Constants;
end Modelica;

package TestPkg
    import Modelica.Constants.pi;
    
    connector RealInput = input Real;
    connector RealOutput = output Real;
    
    partial model SISO
        RealInput u;
        RealOutput y;
    end SISO;
    
    model TransferFunc
        extends SISO;
        parameter Real[:] b = {1};
        parameter Real[:] a = {1, 1};
    equation
        y = u;
    end TransferFunc;
    
    block Filter "PT1 + all-pass filter"
        extends SISO;
        import Modelica.Constants.pi;
        parameter Real f = 50 "Mains Frequency";
        parameter Real fCut = 2*f "Cut off frequency";
        final parameter Integer na(final min=2) = 2 "Count of all-pass";
        final parameter Real fa = f/tan(pi/na) "Characteristic frequency";
        parameter Real yStart = 0 "Start value";
        TransferFunc transferFunction[na](
            each final b={-1/(2*pi*fa),1},
            each final a={+1/(2*pi*fa),1});
    equation
        for j in 1:na - 1 loop
            connect(transferFunction[j].y, transferFunction[j + 1].u);
        end for;
        connect(u, transferFunction[1].u);
        connect(transferFunction[na].y, y);
    end Filter;

    block Signal2mPulse "Generic control"
        import Modelica.Constants.pi;
        parameter Integer m(final min=1) = 3 "Number of phases";
        parameter Boolean useFilter = true "Enable filter";
        parameter Real f = 50 "Frequency";
        parameter Real fCut = 2*f "Cut off frequency";
        parameter Real vStart[m] = zeros(m) "Start voltage";
        RealInput firingAngle;
        RealOutput fire_p[m];
        RealOutput fire_n[m];
        Filter filter[m](
            each final f=f,
            each final fCut=fCut,
            yStart=vStart) if useFilter;
    end Signal2mPulse;

    partial model Dimmer "Dimmer template"
        import Modelica.Constants.pi;
        parameter Real f = 50 "Source frequency";
        Signal2mPulse adaptor(
            m=1,
            useFilter=true,
            f=f);
    end Dimmer;
end TestPkg;
"#;

    let def = rumoca_phase_parse::parse_to_ast(source, "test.mo").unwrap();
    let library = rumoca::CompiledLibrary::from_stored_definition(def).unwrap();

    // Test Dimmer
    println!("\n=== Compiling TestPkg.Dimmer ===");
    match library.compile_model_phases("TestPkg.Dimmer") {
        rumoca::PhaseResult::Success(result) => {
            println!("Success!");
            let balance = result.dae.balance();
            println!("Balance: {}", balance);

            // List all variables
            println!("\n--- Variables ---");
            for (name, _) in result.flat.variables.iter() {
                println!("  {}", name);
            }

            // List equations
            println!("\n--- Equations ({}) ---", result.flat.equations.len());
            for eq in result.flat.equations.iter().take(20) {
                println!("  {:?}", eq);
            }
        }
        rumoca::PhaseResult::NeedsInner { missing_inners } => {
            println!("Needs inner: {:?}", missing_inners);
        }
        rumoca::PhaseResult::Failed { phase, error, .. } => {
            println!("Failed at {:?}: {}", phase, error);
        }
    }
}
