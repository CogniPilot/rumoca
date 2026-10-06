use rumoca::Compiler;
fn compile(path: &str, model: &str) {
    let path = format!("{}/{}", std::env::var("HOME").unwrap(), path);
    let source = std::fs::read_to_string(&path).unwrap();
    let start = std::time::Instant::now();
    match Compiler::new().model(model).compile_str(&source, "source.mo") {
        Err(error) => println!("APP {model}: COMPILE ERR after {:?}: {error} :: {error:?}", start.elapsed()),
        Ok(compiled) => {
            println!("APP {model}: DAE ok after {:?}", start.elapsed());
            if std::env::var("APP_SOLVE").is_ok() {
                match rumoca_phase_solve::lower_solve_package(&compiled.dae) {
                    Ok(_) => println!("APP {model}: SOLVE ok after {:?}", start.elapsed()),
                    Err(error) => println!("APP {model}: SOLVE ERR {error}"),
                }
            }
        }
    }
}
#[test]
fn app_probe_run() {
    for (path, model) in [
        ("scratch/slam-blockers/spatial/combined.mo", "FindOnly"),
        ("scratch/slam-blockers/landmark/source.mo", "RGBDLandmarkMap"),
    ] {
        compile(path, model);
    }
}

const SLICES: &str = r#"
function sliceGate
  input Boolean mask[3];
  input Real x[3,2];
  output Real y[3,2];
algorithm
  y := zeros(3,2);
  for i in 1:3 loop
    if mask[i] then
      y[i,:] := x[i,:];
    end if;
  end for;
end sliceGate;
model SliceGate
  input Boolean mask[3] = {true,false,true};
  input Real x[3,2] = [1,2;3,4;5,6];
  output Real y[3,2];
equation
  y = sliceGate(mask, x);
end SliceGate;
function trackingLike
  input Real index[:]; input Real currentPixels[:,2]; input Real oldPixels[size(index,1),2];
  output Real currentPixel[size(index,1),2];
  output Real enabled[size(index,1)];
protected
  Integer partner;
  Boolean valid;
algorithm
  currentPixel := zeros(size(index,1),2);
  enabled := zeros(size(index,1)); partner := 1; valid := false;
  for i in 1:size(index,1) loop
    valid := index[i] >= 1.0 and index[i] <= size(currentPixels,1);
    partner := if valid then integer(index[i]) else 1;
    if valid then
      valid := currentPixels[partner,1] >= 0.0;
      for coordinate in 1:2 loop
        valid := valid and oldPixels[i,coordinate] >= 0.0;
      end for;
      if valid then
        currentPixel[i,:] := currentPixels[partner,:];
        enabled[i] := 1.0;
      end if;
    end if;
  end for;
end trackingLike;
model TrackingLike
  input Real index[3] = {2,0,1};
  input Real currentPixels[2,2] = [1,2;3,4];
  input Real oldPixels[3,2] = [0,0;0,0;-1,0];
  output Real currentPixel[3,2];
  output Real enabled[3];
equation
  (currentPixel, enabled) = trackingLike(index, currentPixels, oldPixels);
end TrackingLike;
function skewFn
  input Real v[3];
  output Real S[3,3];
algorithm
  S := {{0,-v[3],v[2]},{v[3],0,-v[1]},{-v[2],v[1],0}};
end skewFn;
function blockGate
  input Boolean mask[2];
  input Real p[2,3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J := zeros(3,6);
      J[:,1:3] := identity(3); J[:,4:6] := -skewFn(p[i,:]);
      N := N + transpose(J)*J;
    end if;
  end for;
end blockGate;
model BlockGate
  input Boolean mask[2] = {true,false};
  input Real p[2,3] = [1,2,3;4,5,6];
  output Real N[6,6];
equation
  N = blockGate(mask, p);
end BlockGate;
function blockPlain
  input Real p[3];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  J[:,1:3] := identity(3); J[:,4:6] := -skewFn(p);
end blockPlain;
model BlockPlain
  input Real p[3] = {1,2,3};
  output Real J[3,6];
equation
  J = blockPlain(p);
end BlockPlain;
function b1
  input Boolean mask[2];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J[:,1:3] := identity(3);
    end if;
  end for;
end b1;
model B1
  input Boolean mask[2] = {true,false};
  output Real J[3,6];
equation
  J = b1(mask);
end B1;
function b2
  input Boolean mask;
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  if mask then
    J[:,1:3] := identity(3);
  end if;
end b2;
model B2
  input Boolean mask = true;
  output Real J[3,6];
equation
  J = b2(mask);
end B2;
function b3
  input Boolean mask;
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  if mask then
    J[1,1:3] := {1,2,3};
  end if;
end b3;
model B3
  input Boolean mask = true;
  output Real J[3,6];
equation
  J = b3(mask);
end B3;
function b4
  input Boolean mask;
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  if mask then
    J[:,2] := {1,2,3};
  end if;
end b4;
model B4
  input Boolean mask = true;
  output Real J[3,6];
equation
  J = b4(mask);
end B4;
function b5
  input Boolean mask;
  input Real p[3];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  if mask then
    J[:,4:6] := -skewFn(p);
  end if;
end b5;
model B5
  input Boolean mask = true;
  input Real p[3] = {1,2,3};
  output Real J[3,6];
equation
  J = b5(mask, p);
end B5;
function b6
  input Boolean mask;
  input Real p[3];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  if mask then
    J[:,4:6] := skewFn(p);
  end if;
end b6;
model B6
  input Boolean mask = true;
  input Real p[3] = {1,2,3};
  output Real J[3,6];
equation
  J = b6(mask, p);
end B6;
function b7
  input Boolean mask;
  input Real p[3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  if mask then
    J[:,1:3] := identity(3);
    N := N + transpose(J)*J;
  end if;
end b7;
model B7
  input Boolean mask = true;
  input Real p[3] = {1,2,3};
  output Real N[6,6];
equation
  N = b7(mask, p);
end B7;
function b8
  input Boolean mask;
  input Real p[3];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  if mask then
    J := zeros(3,6);
    J[:,1:3] := identity(3);
  end if;
end b8;
model B8
  input Boolean mask = true;
  input Real p[3] = {1,2,3};
  output Real J[3,6];
equation
  J = b8(mask, p);
end B8;
function l1
  input Boolean mask[2];
  input Real p[2,3];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J[:,4:6] := -skewFn(p[i,:]);
    end if;
  end for;
end l1;
model L1
  input Boolean mask[2] = {true,false};
  input Real p[2,3] = [1,2,3;4,5,6];
  output Real J[3,6];
equation
  J = l1(mask, p);
end L1;
function l2
  input Boolean mask[2];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J[:,1:3] := identity(3);
      N := N + transpose(J)*J;
    end if;
  end for;
end l2;
model L2
  input Boolean mask[2] = {true,false};
  output Real N[6,6];
equation
  N = l2(mask);
end L2;
function l3
  input Boolean mask[2];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J := zeros(3,6);
      J[:,1:3] := identity(3);
    end if;
  end for;
end l3;
model L3
  input Boolean mask[2] = {true,false};
  output Real J[3,6];
equation
  J = l3(mask);
end L3;
function l4
  input Boolean mask[2];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J[:,1:3] := identity(3);
      J[:,4:6] := identity(3);
    end if;
  end for;
end l4;
model L4
  input Boolean mask[2] = {true,false};
  output Real J[3,6];
equation
  J = l4(mask);
end L4;
function l5
  input Boolean mask[2];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J := zeros(3,6);
      J[:,1:3] := identity(3);
      J[:,4:6] := 2*identity(3);
    end if;
  end for;
end l5;
model L5
  input Boolean mask[2] = {true,false};
  output Real J[3,6];
equation
  J = l5(mask);
end L5;
function b9
  input Boolean mask;
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  if mask then
    J := zeros(3,6);
    J[:,1:3] := identity(3);
    J[:,4:6] := 2*identity(3);
  end if;
end b9;
model B9
  input Boolean mask = true;
  output Real J[3,6];
equation
  J = b9(mask);
end B9;
function l6
  input Boolean mask[2];
  output Real J[3,6];
algorithm
  J := zeros(3,6);
  for i in 1:2 loop
    J := zeros(3,6);
    J[:,1:3] := identity(3);
    J[:,4:6] := 2*identity(3);
  end for;
end l6;
model L6
  input Boolean mask[2] = {true,false};
  output Real J[3,6];
equation
  J = l6(mask);
end L6;
function v1
  input Boolean mask[2];
  input Real p[2,3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J := zeros(3,6);
      J[:,1:3] := identity(3); J[:,4:6] := identity(3);
      N := N + transpose(J)*J;
    end if;
  end for;
end v1;
model V1
  input Boolean mask[2] = {true,false};
  input Real p[2,3] = [1,2,3;4,5,6];
  output Real N[6,6];
equation
  N = v1(mask, p);
end V1;
function v2
  input Boolean mask[2];
  input Real p[2,3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J := zeros(3,6);
      J[:,1:3] := identity(3); J[:,4:6] := -skewFn({1,2,3});
      N := N + transpose(J)*J;
    end if;
  end for;
end v2;
model V2
  input Boolean mask[2] = {true,false};
  input Real p[2,3] = [1,2,3;4,5,6];
  output Real N[6,6];
equation
  N = v2(mask, p);
end V2;
function v3
  input Boolean mask[2];
  input Real p[2,3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J[:,1:3] := identity(3); J[:,4:6] := -skewFn(p[i,:]);
      N := N + transpose(J)*J;
    end if;
  end for;
end v3;
model V3
  input Boolean mask[2] = {true,false};
  input Real p[2,3] = [1,2,3;4,5,6];
  output Real N[6,6];
equation
  N = v3(mask, p);
end V3;
function v4
  input Boolean mask[2];
  input Real p[2,3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J := zeros(3,6);
      J[:,4:6] := -skewFn(p[i,:]);
      N := N + transpose(J)*J;
    end if;
  end for;
end v4;
model V4
  input Boolean mask[2] = {true,false};
  input Real p[2,3] = [1,2,3;4,5,6];
  output Real N[6,6];
equation
  N = v4(mask, p);
end V4;
function w1
  input Boolean mask;
  input Real p[3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  if mask then
    J := zeros(3,6);
    J[:,4:6] := identity(3);
    N := N + transpose(J)*J;
  end if;
end w1;
model W1
  input Boolean mask = true;
  input Real p[3] = {1,2,3};
  output Real N[6,6];
equation
  N = w1(mask, p);
end W1;
function w2
  input Real p[3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  J := zeros(3,6);
  J[:,4:6] := identity(3);
  N := transpose(J)*J;
end w2;
model W2
  input Real p[3] = {1,2,3};
  output Real N[6,6];
equation
  N = w2(p);
end W2;
function w3
  input Boolean mask;
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  if mask then
    J := zeros(3,6);
    J[1,4] := 1.0;
    N := N + transpose(J)*J;
  end if;
end w3;
model W3
  input Boolean mask = true;
  output Real N[6,6];
equation
  N = w3(mask);
end W3;
function w4
  input Boolean mask;
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  if mask then
    J := ones(3,6);
    N := N + transpose(J)*J;
  end if;
end w4;
model W4
  input Boolean mask = true;
  output Real N[6,6];
equation
  N = w4(mask);
end W4;
function x1
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  J := ones(3,6);
  N := transpose(J)*J;
end x1;
model X1
  output Real N[6,6];
equation
  N = x1();
end X1;
function x2
  input Real u;
  output Real N[6,6];
algorithm
  N := u*transpose(ones(3,6))*ones(3,6);
end x2;
model X2
  input Real u = 1;
  output Real N[6,6];
equation
  N = x2(u);
end X2;
function x3
  input Real u;
  output Real N[3,6];
algorithm
  N := u*(ones(3,6)*1.0);
end x3;
model X3
  input Real u = 1;
  output Real N[3,6];
equation
  N = x3(u);
end X3;
"#;

#[test]
fn app_probe_slices() {
    for model in ["X1", "X2", "X3", "W4"] {
        match Compiler::new().model(model).compile_str(SLICES, "S.mo") {
            Err(error) => println!("APP {model}: COMPILE ERR {error}"),
            Ok(compiled) => {
                let probe = rumoca_sim::eval_dae_at(&compiled.dae, &rumoca_sim::SimOptions::default(), &[], 0.0);
                match probe {
                    Err(e) => println!("APP {model}: EVAL ERR {e}"),
                    Ok(p) => println!("APP {model}: {:?} {:?}", p.report.error, p.report.solver_y.iter().map(|s| (s.name.clone(), s.value)).collect::<Vec<_>>()),
                }
            }
        }
    }
}

fn shadow_root(root: &str, models: &[String]) {
    use rumoca_compile::compile::{Session, SessionConfig, SourceRootKind};
    let parsed = rumoca_compile::source_roots::parse_source_root_with_cache(std::path::Path::new(root))
        .expect("parse source root");
    let mut session = Session::new(SessionConfig::default());
    session.replace_parsed_source_set("shadow", SourceRootKind::DurableExternal, parsed.documents, None);
    for model in models {
        eprintln!("SHADOWMODEL {model}");
        let started = std::time::Instant::now();
        let report = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            session.compile_model_strict_reachable_uncached_with_recovery(model)
        }));
        eprintln!("SHADOWDONE {model} {:?} {}", started.elapsed(), if report.is_ok() { "returned" } else { "panicked" });
    }
}

#[test]
fn shadow_msl_sweep() {
    let table = std::fs::read_to_string(format!("{}/target/msl/results/msl_band_table.json", env!("CARGO_MANIFEST_DIR").replace("/crates/rumoca", ""))).unwrap();
    let models = table
        .split("\"model_name\": \"")
        .skip(1)
        .map(|rest| rest.split('"').next().unwrap().to_string())
        .collect::<Vec<_>>();
    let root = format!("{}/target/msl/ModelicaStandardLibrary-4.1.0", env!("CARGO_MANIFEST_DIR").replace("/crates/rumoca", ""));
    shadow_root(&root, &models);
}

#[test]
fn shadow_modelica_models() {
    shadow_root(
        &format!("{}/git/modelica_models", std::env::var("HOME").unwrap()),
        &["Tests.All".to_string(), "Tests.LieGroupTests.SO2".to_string(), "Tests.LieGroupTests.SE2".to_string()],
    );
}

const TIMING: &str = r#"
function chainFill
  input Real u;
  output Real y[14401];
algorithm
  for i in 1:14401 loop
    y[i] := u * i;
  end for;
end chainFill;
model ChainFill
  input Real u = 1;
  output Real s;
protected
  Real y[14401];
equation
  y = chainFill(u);
  s = y[14401];
end ChainFill;
function prefixFill
  input Real u;
  output Real y[14401];
algorithm
  y[1] := u;
  for i in 2:14401 loop
    y[i] := y[i - 1] + 1.0;
  end for;
end prefixFill;
model PrefixFill
  input Real u = 1;
  output Real s;
protected
  Real y[14401];
equation
  y = prefixFill(u);
  s = y[14401];
end PrefixFill;
function catalogStore
  input Real descriptor[350, 49];
  input Integer slot;
  output Real catalog[128, 350, 49];
algorithm
  for s in 1:128 loop
    for f in 1:350 loop
      for d in 1:49 loop
        catalog[s, f, d] := if s == slot then descriptor[f, d] else 0.0;
      end for;
    end for;
  end for;
end catalogStore;
model CatalogStore
  input Real descriptor[350, 49] = fill(1.0, 350, 49);
  input Integer slot = 3;
  output Real s;
protected
  Real catalog[128, 350, 49];
equation
  catalog = catalogStore(descriptor, slot);
  s = catalog[3, 1, 1];
end CatalogStore;
function catalogDecay
  input Real previous[128, 350, 49];
  input Real rate;
  output Real catalog[128, 350, 49];
algorithm
  catalog := previous;
  for s in 1:128 loop
    for f in 1:350 loop
      for d in 1:49 loop
        catalog[s, f, d] := rate * catalog[s, f, d];
      end for;
    end for;
  end for;
end catalogDecay;
model CatalogDecay
  input Real previous[128, 350, 49] = fill(1.0, 128, 350, 49);
  input Real rate = 0.5;
  output Real s;
protected
  Real catalog[128, 350, 49];
equation
  catalog = catalogDecay(previous, rate);
  s = catalog[3, 1, 1];
end CatalogDecay;
"#;

#[test]
fn app_probe_timing() {
    for model in ["CatalogDecay"] {
        let started = std::time::Instant::now();
        let result = Compiler::new().model(model).compile_str(TIMING, "T.mo");
        println!("APP {model}: {} after {:?}", if result.is_ok() { "DAE ok" } else { "ERR" }, started.elapsed());
        if let Err(error) = result { println!("APP {model} error: {error}"); }
    }
}
