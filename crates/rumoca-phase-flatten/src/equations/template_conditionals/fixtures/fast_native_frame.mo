// Ordered FAST-9 patch score. Strict comparisons select the second operand on ties.
function FastPatchScore
  input Real gray[7,7];
  output Real score;
protected
  Real differences[16];
  Real extended[24];
  Real low2[22]; Real high2[22];
  Real low4[20]; Real high4[20];
  Real low8[16]; Real high8[16];
  Real bright[16]; Real dark[16];
  Real arcs[16]; Real responses[17];
algorithm
  differences[1] := gray[1,4]-gray[4,4];
  differences[2] := gray[1,5]-gray[4,4];
  differences[3] := gray[2,6]-gray[4,4];
  differences[4] := gray[3,7]-gray[4,4];
  differences[5] := gray[4,7]-gray[4,4];
  differences[6] := gray[5,7]-gray[4,4];
  differences[7] := gray[6,6]-gray[4,4];
  differences[8] := gray[7,5]-gray[4,4];
  differences[9] := gray[7,4]-gray[4,4];
  differences[10] := gray[7,3]-gray[4,4];
  differences[11] := gray[6,2]-gray[4,4];
  differences[12] := gray[5,1]-gray[4,4];
  differences[13] := gray[4,1]-gray[4,4];
  differences[14] := gray[3,1]-gray[4,4];
  differences[15] := gray[2,2]-gray[4,4];
  differences[16] := gray[1,3]-gray[4,4];
  // Shared ordered windows:2,4,8 then9 circle samples. Static loop bounds
  // avoid modulo-binder lowering and keep the same bright/dark FAST-9 math.
  for i in 1:16 loop
    extended[i] := differences[i];
  end for;
  for i in 1:8 loop
    extended[i+16] := differences[i];
  end for;
  for i in 1:22 loop
    low2[i] := if noEvent(extended[i] < extended[i+1]) then extended[i] else extended[i+1];
    high2[i] := if noEvent(extended[i] > extended[i+1]) then extended[i] else extended[i+1];
  end for;
  for i in 1:20 loop
    low4[i] := if noEvent(low2[i] < low2[i+2]) then low2[i] else low2[i+2];
    high4[i] := if noEvent(high2[i] > high2[i+2]) then high2[i] else high2[i+2];
  end for;
  for i in 1:16 loop
    low8[i] := if noEvent(low4[i] < low4[i+4]) then low4[i] else low4[i+4];
    high8[i] := if noEvent(high4[i] > high4[i+4]) then high4[i] else high4[i+4];
    bright[i] := if noEvent(low8[i] < extended[i+8]) then low8[i] else extended[i+8];
    dark[i] := -(if noEvent(high8[i] > extended[i+8]) then high8[i] else extended[i+8]);
  end for;
  // NumPy maximum retains its second operand on ties, including signed zero.
  // Ordered comparisons keep that exact score behavior across all16 arcs.
  responses[1] := 0.0;
  for arc in 1:16 loop
    arcs[arc] := if noEvent(bright[arc] > dark[arc]) then bright[arc] else dark[arc];
    responses[arc+1] := if noEvent(responses[arc] > arcs[arc]) then responses[arc] else arcs[arc];
  end for;
  score := responses[17];
end FastPatchScore;

// Full-frame candidate: production FastRasterStages and presets remain separate.
// Raw GPU RGBA and scores are row-major; channels are contiguous in the native ABI.
// Alpha is transported unchanged and never read by grayscale or FAST scoring.
model FastNativeFrame
  parameter Integer height = 90;
  parameter Integer width = 160;
  constant Integer radius = 3;
  constant Integer channels = 4;
  parameter Real absolute_threshold = 18;
  parameter Real relative_threshold = 0;
  parameter Real rank_scale = 1e8;
  parameter Real suppression_radius = 3;
  parameter Real feature_cap = 240;
  input Real rgb[height,width,channels] = fill(0.0,height,width,channels);
  output Real scores[height*width];
  output Real selection[8];
protected
  Real gray[height,width];
equation
  for row in 1:height loop
    for column in 1:width loop
      gray[row,column] = ((rgb[row,column,1]+rgb[row,column,2])+rgb[row,column,3])/3.0;
      if row > radius and row <= height-radius and column > radius and column <= width-radius then
        scores[(row-1)*width+column] = FastPatchScore(gray[row-radius:row+radius,column-radius:column+radius]);
      else
        scores[(row-1)*width+column] = 0.0;
      end if;
    end for;
  end for;
  selection = {absolute_threshold,relative_threshold,rank_scale,suppression_radius,feature_cap,1,3,3};
end FastNativeFrame;
