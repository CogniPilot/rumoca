//! A conditional region inside a doubly nested loop reads the binders of both
//! loops through a call whose argument the loops build: the region captures
//! each enclosing binder as one scalar of its own register file. The scoring
//! functions follow a corner detector over a circle of samples.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation};

const SOURCE: &str = r#"
package FastCircleStencil
  constant Integer radius = 3;
  constant Integer sampleCount = 16;
  // Row/column offsets, clockwise from the top of the radius-three circle.
  constant Integer offsets[sampleCount,2] = [
    -3,0; -3,1; -2,2; -1,3; 0,3; 1,3; 2,2; 3,1;
    3,0; 3,-1; 2,-2; 1,-3; 0,-3; -1,-3; -2,-2; -3,-1];
end FastCircleStencil;
function FastCircleScore
  input Real differences[FastCircleStencil.sampleCount];
  output Real score;
protected
  constant Integer circleSize = FastCircleStencil.sampleCount;
  constant Integer arcLength = 9;
  Real extended[circleSize+arcLength-1];
  Real low2[circleSize+arcLength-3];
  Real high2[circleSize+arcLength-3];
  Real low4[circleSize+arcLength-5];
  Real high4[circleSize+arcLength-5];
  Real low8;
  Real high8;
  Real bright;
  Real dark;
  Real response;
algorithm
  // Reuse ordered minima/maxima for windows of 2, 4, 8, then 9 samples.
  for i in 1:circleSize loop
    extended[i] := differences[i];
  end for;
  for i in 1:arcLength-1 loop
    extended[i+circleSize] := differences[i];
  end for;
  for i in 1:size(low2,1) loop
    low2[i] := if noEvent(extended[i] < extended[i+1]) then extended[i] else extended[i+1];
    high2[i] := if noEvent(extended[i] > extended[i+1]) then extended[i] else extended[i+1];
  end for;
  for i in 1:size(low4,1) loop
    low4[i] := if noEvent(low2[i] < low2[i+2]) then low2[i] else low2[i+2];
    high4[i] := if noEvent(high2[i] > high2[i+2]) then high2[i] else high2[i+2];
  end for;
  // Final arc stages need no intermediate arrays.
  score := 0.0;
  for arc in 1:circleSize loop
    low8 := if noEvent(low4[arc] < low4[arc+4]) then low4[arc] else low4[arc+4];
    high8 := if noEvent(high4[arc] > high4[arc+4]) then high4[arc] else high4[arc+4];
    bright := if noEvent(low8 < extended[arc+8]) then low8 else extended[arc+8];
    dark := -(if noEvent(high8 > extended[arc+8]) then high8 else extended[arc+8]);
    response := if noEvent(bright > dark) then bright else dark;
    score := if noEvent(score > response) then score else response;
  end for;
end FastCircleScore;
function FastCircleCanReachScore
  input Real differences[FastCircleStencil.sampleCount];
  input Real scoreFloor;
  output Boolean possible;
protected
  constant Integer cardinalCount = 4;
  constant Integer stride = div(FastCircleStencil.sampleCount,cardinalCount);
  Integer sample;
  Boolean firstBright;
  Boolean firstDark;
  Boolean lastBright;
  Boolean lastDark;
  Boolean bright;
  Boolean dark;
algorithm
  possible := true;
  if scoreFloor > 0.0 and scoreFloor <= 255.0 then
    firstBright := differences[1] >= scoreFloor;
    firstDark := -differences[1] >= scoreFloor;
    lastBright := firstBright;
    lastDark := firstDark;
    possible := false;
    for cardinal in 2:cardinalCount loop
      sample := 1+(cardinal-1)*stride;
      bright := differences[sample] >= scoreFloor;
      dark := -differences[sample] >= scoreFloor;
      possible := possible or (lastBright and bright) or (lastDark and dark);
      lastBright := bright;
      lastDark := dark;
    end for;
    possible := possible or (lastBright and firstBright) or (lastDark and firstDark);
    if not possible then
      // Conservatively retain enormous finite values as well as NaN/infinity.
      for slot in 1:size(differences,1) loop
        possible := possible or not (abs(differences[slot]) <= 1e308);
      end for;
    end if;
  end if;
end FastCircleCanReachScore;
function frame
  input Real rgb[:,:,:];
  input Real scoreFloor = 0.0 "Optional conservative selection floor; zero keeps every score";
  output Real scores[size(rgb,1)*size(rgb,2)];
protected
  constant Integer radius = FastCircleStencil.radius;
  Real gray[size(rgb,1),size(rgb,2)];
  Real differences[FastCircleStencil.sampleCount];
  Real center;
algorithm
  scores := zeros(size(rgb,1)*size(rgb,2));
  gray := rgb[:,:,1];
  for row in radius+1:size(rgb,1)-radius loop
    for column in radius+1:size(rgb,2)-radius loop
      center := gray[row,column];
      for sample in 1:FastCircleStencil.sampleCount loop
        differences[sample] := gray[row+FastCircleStencil.offsets[sample,1],
          column+FastCircleStencil.offsets[sample,2]]-center;
      end for;
      if FastCircleCanReachScore(differences,scoreFloor) then
        scores[(row-1)*size(rgb,2)+column] := FastCircleScore(differences);
      end if;
    end for;
  end for;
end frame;
function wrap
  input Real rgb[:,:,:];
  input Real w[:];
  input Real u;
  output Real s[size(rgb,1)*size(rgb,2)];
  output Real peak;
algorithm
  peak := max(w)*u;
  s := frame(rgb, 20.0*u);
end wrap;
model Frame
  Real x(start = 1.0, fixed = true);
  input Real rgb[12,12,3] = fill(0.0,12,12,3);
  output Real s[144];
  output Real peak;
equation
  der(x) = 0;
  (s, peak) = wrap(x*rgb, {1.0, 2.0}, x);
end Frame;
"#;

#[test]
fn a_conditional_region_in_a_loop_nest_captures_both_loop_binders() {
    let compiled = Compiler::new()
        .model("Frame")
        .compile_str(SOURCE, "frame.mo")
        .expect("the loop nest constructs checked DAE");
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .expect("the region captures the enclosing binders");
}
