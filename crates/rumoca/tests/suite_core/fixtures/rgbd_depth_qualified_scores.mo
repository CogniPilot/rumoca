function RGBDCalibratedPoint
  input Real depth[:,:];
  input Real pixel[2];
  input Boolean enabled;
  input Real rgbCalibration[4];
  input Real depthCalibration[4];
  input Real nearDepth;
  input Real farDepth;
  input Real disparityNoise;
  input Real noiseReferenceFx;
  input Real baseline;
  output Real point[3] "Optical RDF: right, down, forward";
  output Boolean valid;
  input Real depthUnits = 1.0 "Meters per depth sample; 1 for metric depth, SDK scale for Z16";
protected
  Real mapped[2];
  Real bearing[2];
  Real nearest;
  Real low;
  Real part;
  Integer lower[2];
  Integer sampleX;
  Integer sampleY;
  Integer sampleIndex;
  Real fraction[2];
  Real weight[4];
  Real sample;
  Real inverseDepth;
  Real minimumDepth;
  Real maximumDepth;
  Real threshold;
  Real axialDepth;
  Boolean configuration;
  Boolean inside;
  Boolean usable;
algorithm
  nearest := 0.0;
  low := 0.0;
  part := 0.0;
  inside := false;
  sampleX := 0;
  sampleY := 0;
  sampleIndex := 1;
  sample := 0.0;
  usable := false;
  configuration := enabled and depthUnits > 0.0 and depthUnits <= 1e6 and size(depth,1) > 1 and size(depth,2) > 1 and
    pixel[1] >= 0.0 and pixel[1] < size(depth,2) and pixel[2] >= 0.0 and pixel[2] < size(depth,1) and
    rgbCalibration[1] >= 1e-6 and rgbCalibration[1] <= 1e6 and rgbCalibration[2] >= 1e-6 and rgbCalibration[2] <= 1e6 and
    depthCalibration[1] >= 1e-6 and depthCalibration[1] <= 1e6 and depthCalibration[2] >= 1e-6 and depthCalibration[2] <= 1e6 and
    abs(rgbCalibration[3]) <= 1e6 and abs(rgbCalibration[4]) <= 1e6 and
    abs(depthCalibration[3]) <= 1e6 and abs(depthCalibration[4]) <= 1e6 and
    nearDepth > 0.0 and farDepth > nearDepth+0.05 and farDepth <= 1e6 and
    disparityNoise >= 0.0 and disparityNoise <= 1.0 and noiseReferenceFx >= 1e-6 and noiseReferenceFx <= 1e6 and
    baseline >= 1e-6 and baseline <= 10.0;
  mapped := zeros(2);
  bearing := zeros(2);
  lower := fill(0,2);
  fraction := zeros(2);
  for k in 1:2 loop
    mapped[k] := if configuration then (pixel[k]-rgbCalibration[k+2])*depthCalibration[k]/rgbCalibration[k]+depthCalibration[k+2] else 0.0;
    low := floor(mapped[k]);
    part := mapped[k]-low;
    nearest := if part < 0.5 then low else if part > 0.5 then low+1.0 else if floor(low/2.0)*2.0 == low then low else low+1.0;
    bearing[k] := if abs(mapped[k]-nearest) <= 1e-9 then nearest else mapped[k];
    inside := configuration and bearing[k] >= 0.0 and bearing[k] <= (if k == 1 then size(depth,2)-1 else size(depth,1)-1);
    lower[k] := if inside then integer(floor(bearing[k])) else 0;
    fraction[k] := if inside then bearing[k]-lower[k] else 0.0;
    configuration := configuration and inside;
  end for;
  weight := {(1.0-fraction[1])*(1.0-fraction[2]),fraction[1]*(1.0-fraction[2]),
    (1.0-fraction[1])*fraction[2],fraction[1]*fraction[2]};
  valid := configuration;
  inverseDepth := 0.0;
  minimumDepth := farDepth;
  maximumDepth := 0.0;
  for row in 0:1 loop
    for column in 0:1 loop
      sampleIndex := 2*row+column+1;
      sampleX := lower[1]+column;
      sampleY := lower[2]+row;
      sample := if configuration and weight[sampleIndex] > 0.0
        and sampleX < size(depth,2) and sampleY < size(depth,1)
        then depth[sampleY+1,sampleX+1]*depthUnits else 0.0;
      usable := weight[sampleIndex] <= 0.0 or (sample > nearDepth and sample < farDepth-0.05);
      valid := valid and usable;
      minimumDepth := if weight[sampleIndex] > 0.0 and usable then min(minimumDepth,sample) else minimumDepth;
      maximumDepth := if weight[sampleIndex] > 0.0 and usable then max(maximumDepth,sample) else maximumDepth;
      inverseDepth := inverseDepth+(if weight[sampleIndex] > 0.0 and usable
        then weight[sampleIndex]/max(sample,1e-12) else 0.0);
    end for;
  end for;
  threshold := if configuration then 0.03+0.025*minimumDepth+3.0*minimumDepth^2*disparityNoise/(noiseReferenceFx*baseline) else 0.0;
  valid := valid and maximumDepth-minimumDepth <= threshold and inverseDepth > 0.0;
  axialDepth := if valid then 1.0/max(inverseDepth,1e-12) else 0.0;
  point := zeros(3);
  point[1] := if valid then (pixel[1]-rgbCalibration[3])*axialDepth/rgbCalibration[1] else 0.0;
  point[2] := if valid then (pixel[2]-rgbCalibration[4])*axialDepth/rgbCalibration[2] else 0.0;
  point[3] := axialDepth;
end RGBDCalibratedPoint;

// Reserve feature slots for pixels with valid calibrated depth.
function RGBDDepthQualifiedScores

  input Real depth[:,:];
  input Real scores[size(depth,1)*size(depth,2)];
  input Real rgbCalibration[4];
  input Real depthCalibration[4];
  input Real nearDepth;
  input Real farDepth;
  input Real disparityNoise;
  input Real noiseReferenceFx;
  input Real baseline;
  input Boolean enabled = true;
  output Real qualified[size(scores,1)];
  input Real depthUnits = 1.0 "Meters per depth sample; 1 for metric depth, SDK scale for Z16";
protected
  Real point[3];
  Boolean valid;
  Integer index;
algorithm
  qualified := zeros(size(scores,1));
  if enabled then
    qualified := scores;
    for row in 1:size(depth,1) loop
      for column in 1:size(depth,2) loop
        index := (row-1)*size(depth,2)+column;
        // Leave invalid scores untouched for the selector to refuse. Zero
        // scores need no depth work; disabled acquisitions read no depth.
        if scores[index] > 0.0 and scores[index] <= 1e8 then
          (point,valid) := RGBDCalibratedPoint(depth,{column-1.0,row-1.0},true,
            rgbCalibration,depthCalibration,nearDepth,farDepth,
            disparityNoise,noiseReferenceFx,baseline,depthUnits=depthUnits);
          if not valid then
            qualified[index] := 0.0;
          end if;
        end if;
      end for;
    end for;
  end if;
end RGBDDepthQualifiedScores;
