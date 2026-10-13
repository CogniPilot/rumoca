//! The ranked raster selector with its three heap passes and the occupancy
//! suppression runs inside a fuel bound proportional to its algorithmic work:
//! no pass copies the carried candidate or occupancy arrays.

use super::*;

const FUNCTION: &str = r#"
function SelectRasterFeatures
  input Real scores[:];
  input Integer width;
  input Integer height;
  input Integer capacity;
  input Integer minimumBorder;
  input Real settings[8]; // absolute,relative,rankScale,radius,cap,spacing,border,start
  input Boolean grid;
  input Boolean enabled = true "False between camera acquisitions";
  output Real features[capacity,3];
  output Real count;
  output Real valid;
protected
  Integer candidateIndex[size(scores,1)];
  Real candidateRank[size(scores,1)];
  Boolean occupied[size(scores,1)];
  Integer candidateCount;
  Integer radius;
  Integer featureLimit;
  Integer sampleSpacing;
  Integer border;
  Integer firstPixel;
  Integer x;
  Integer y;
  Integer index;
  Integer position;
  Integer root;
  Integer child;
  Integer heapSize;
  Integer temporaryIndex;
  Real temporaryRank;
  Real scaled;
  Real low;
  Real fraction;
  Real rank;
  Real maximum;
  Real threshold;
  Real guard;
  Boolean aboveThreshold;
  Boolean sifting;
algorithm
  features := zeros(capacity,3);
  count := 0.0;
  valid := if width > 0 and height > 0 and size(scores,1) == width*height and capacity > 0 and
    settings[1] >= 0.0 and settings[1] <= 255.0 and
    settings[2] >= 0.0 and settings[2] <= 1.0 and
    settings[3] >= 1.0 and settings[3] <= 1e12 and
    settings[4] >= 0.0 and settings[4] <= 16.0 and floor(settings[4]) == settings[4] and
    settings[5] >= 1.0 and settings[5] <= width*height and settings[5] <= capacity and floor(settings[5]) == settings[5] and
    settings[6] >= 1.0 and settings[6] <= width and floor(settings[6]) == settings[6] and
    settings[7] >= minimumBorder and settings[7] <= floor((height-1)/2.0) and floor(settings[7]) == settings[7] and
    settings[8] >= settings[7] and settings[8] < height-settings[7] and floor(settings[8]) == settings[8] and
    (not grid or (settings[1] == 0.0 and settings[2] == 0.0 and settings[3] == 1.0 and settings[4] == 0.0))
    then 1.0 else 0.0;
  // Protected conversion is needed even for rejected nonfinite settings.
  radius := if valid > 0.0 then integer(settings[4]) else 0;
  featureLimit := if valid > 0.0 then integer(settings[5]) else 1;
  sampleSpacing := if valid > 0.0 then integer(settings[6]) else 1;
  border := if valid > 0.0 then integer(settings[7]) else 0;
  firstPixel := if valid > 0.0 then integer(settings[8]) else 0;
  candidateCount := 0;
  maximum := 0.0;
  threshold := 0.0;
  guard := 0.0;
  if valid > 0.0 and enabled then
    candidateIndex := fill(0,size(scores,1));
    candidateRank := zeros(size(scores,1));
    occupied := fill(false,size(scores,1));
    if grid then
      // Grid mode reads only visited pixels.
      y := firstPixel;
      while y < height-border and count < featureLimit loop
        x := firstPixel;
        while x < width-border and count < featureLimit loop
          index := y*width+x+1;
          if abs(scores[index]) <= 1.7976931348623157e308 then
            count := count+1.0;
            features[integer(count),1] := x;
            features[integer(count),2] := y;
            features[integer(count),3] := scores[index];
          else
            valid := 0.0;
          end if;
          x := x+sampleSpacing;
        end while;
        y := y+sampleSpacing;
      end while;
    else
      // Validate every score, including pixels outside the candidate border.
      for i in 1:size(scores,1) loop
        if abs(scores[i]*settings[3]) <= 9007199254740991.0 then
          maximum := max(maximum,scores[i]);
        else
          valid := 0.0;
        end if;
      end for;
      threshold := max(settings[1],maximum*settings[2]);
      guard := floor(threshold*settings[3])-1.0;
      if valid > 0.0 then
        y := firstPixel;
        while y < height-border loop
          x := firstPixel;
          while x < width-border loop
            index := y*width+x+1;
            scaled := scores[index]*settings[3];
            low := floor(scaled);
            fraction := scaled-low;
            rank := if fraction < 0.5 then low else if fraction > 0.5 then low+1.0
              else if low-floor(low/2.0)*2.0 == 0.0 then low else low+1.0;
            if rank >= guard then
              candidateCount := candidateCount+1;
              candidateIndex[candidateCount] := index;
              candidateRank[candidateCount] := rank;
            end if;
            x := x+sampleSpacing;
          end while;
          y := y+sampleSpacing;
        end while;
        // Heap order: ascending rank, descending raster index. Read in reverse.
        heapSize := candidateCount;
        for build in 1:candidateCount loop
          root := candidateCount-build+1;
          sifting := true;
          while root <= div(heapSize,2) and sifting loop
            child := 2*root;
            if child < heapSize then
              if candidateRank[child+1] > candidateRank[child] or
                (candidateRank[child+1] == candidateRank[child] and candidateIndex[child+1] < candidateIndex[child]) then
                child := child+1;
              end if;
            end if;
            if candidateRank[child] > candidateRank[root] or
              (candidateRank[child] == candidateRank[root] and candidateIndex[child] < candidateIndex[root]) then
              temporaryRank := candidateRank[root];
              temporaryIndex := candidateIndex[root];
              candidateRank[root] := candidateRank[child];
              candidateIndex[root] := candidateIndex[child];
              candidateRank[child] := temporaryRank;
              candidateIndex[child] := temporaryIndex;
              root := child;
            else
              sifting := false;
            end if;
          end while;
        end for;
        for remove in 1:candidateCount loop
          heapSize := candidateCount-remove+1;
          temporaryRank := candidateRank[heapSize];
          temporaryIndex := candidateIndex[heapSize];
          candidateRank[heapSize] := candidateRank[1];
          candidateIndex[heapSize] := candidateIndex[1];
          candidateRank[1] := temporaryRank;
          candidateIndex[1] := temporaryIndex;
          heapSize := heapSize-1;
          root := 1;
          sifting := true;
          while root <= div(heapSize,2) and sifting loop
            child := 2*root;
            if child < heapSize then
              if candidateRank[child+1] > candidateRank[child] or
                (candidateRank[child+1] == candidateRank[child] and candidateIndex[child+1] < candidateIndex[child]) then
                child := child+1;
              end if;
            end if;
            if candidateRank[child] > candidateRank[root] or
              (candidateRank[child] == candidateRank[root] and candidateIndex[child] < candidateIndex[root]) then
              temporaryRank := candidateRank[root];
              temporaryIndex := candidateIndex[root];
              candidateRank[root] := candidateRank[child];
              candidateIndex[root] := candidateIndex[child];
              candidateRank[child] := temporaryRank;
              candidateIndex[child] := temporaryIndex;
              root := child;
            else
              sifting := false;
            end if;
          end while;
        end for;
        aboveThreshold := true;
        for candidate in 1:candidateCount loop
          position := candidateCount-candidate+1;
          index := candidateIndex[position];
          // Preserve the original early stop on raw score, before occupancy.
          if scores[index] < threshold then
            aboveThreshold := false;
          end if;
          if aboveThreshold and count < featureLimit and not occupied[index] then
            x := mod(index-1,width);
            y := div(index-1,width);
            count := count+1.0;
            features[integer(count),1] := x;
            features[integer(count),2] := y;
            features[integer(count),3] := scores[index];
            for yy in max(border,y-radius):min(height-border-1,y+radius) loop
              for xx in max(border,x-radius):min(width-border-1,x+radius) loop
                occupied[yy*width+xx+1] := true;
              end for;
            end for;
          end if;
        end for;
      end if;
    end if;
  end if;
  if valid <= 0.0 then
    features := zeros(capacity,3);
    count := 0.0;
  end if;
end SelectRasterFeatures;

model SelectRaster
  parameter Integer width = @WIDTH@;
  parameter Integer height = @HEIGHT@;
  input Boolean enabled = true;
  input Real scores[width*height] = zeros(width*height);
  input Real settings[8] = {18.0,0.0,1e8,1.0,@LIMIT@.0,1.0,3.0,3.0};
  output Real features[@LIMIT@,3];
  output Real count;
  output Real valid;
equation
  (features,count,valid) =
    SelectRasterFeatures(scores,width,height,@LIMIT@,3,settings,false,enabled);
end SelectRaster;
"#;

const LIMIT: usize = 240;
const BORDER: usize = 3;
const THRESHOLD: f64 = 18.0;

/// A cell reaches the absolute threshold when `stride` divides its index, with
/// a few distinct ranks so ties break by raster order.
fn score(cell: usize, stride: usize) -> f64 {
    if cell % stride == 1 {
        20.0 + (cell % 5) as f64 * 0.25
    } else {
        (cell % 11) as f64 * 0.5
    }
}

/// The selector's features in the source's order: candidates by descending
/// rank then raster order, each kept when its 3x3 neighbourhood is free.
fn reference((width, height): (usize, usize), scores: &[f64]) -> (usize, Vec<[f64; 3]>) {
    let mut candidates = (BORDER..height - BORDER)
        .flat_map(|y| (BORDER..width - BORDER).map(move |x| y * width + x))
        .filter(|cell| scores[*cell] >= THRESHOLD)
        .collect::<Vec<_>>();
    let rank = |cell: usize| (scores[cell] * 1e8).round();
    candidates.sort_by(|a, b| rank(*b).total_cmp(&rank(*a)).then(a.cmp(b)));
    let mut occupied = vec![false; scores.len()];
    let mut features = Vec::new();
    for cell in candidates.iter().copied() {
        if features.len() == LIMIT || occupied[cell] {
            continue;
        }
        let (x, y) = (cell % width, cell / width);
        features.push([x as f64, y as f64, scores[cell]]);
        for yy in (y - 1).max(BORDER)..=(y + 1).min(height - BORDER - 1) {
            for xx in (x - 1).max(BORDER)..=(x + 1).min(width - BORDER - 1) {
                occupied[yy * width + xx] = true;
            }
        }
    }
    (candidates.len(), features)
}

fn artifact((width, height): (usize, usize)) -> serde_json::Value {
    let source = FUNCTION
        .replace("@WIDTH@", &width.to_string())
        .replace("@HEIGHT@", &height.to_string())
        .replace("@LIMIT@", &LIMIT.to_string());
    serde_json::from_str(
        &crate::native_program_api::prepare_native_program(&source, "SelectRaster").unwrap(),
    )
    .unwrap()
}

struct Case {
    fuel: u64,
    features: Vec<[f64; 3]>,
}

fn run(artifact: &serde_json::Value, scores: &[f64]) -> Case {
    let layout: rumoca_ir_solve::VarLayout =
        serde_json::from_value(artifact["var_layout"].clone()).unwrap();
    let binding = |name: &str| match layout.binding(name).unwrap() {
        rumoca_ir_solve::ScalarSlot::Y { index, .. }
        | rumoca_ir_solve::ScalarSlot::P { index, .. } => index,
        _ => panic!("{name} is a scalar slot"),
    };
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for (cell, value) in scores.iter().enumerate() {
        parameters[binding(&format!("scores[{}]", cell + 1))] = *value;
    }
    let mut execution = CallExecution::new(artifact);
    execution.set_input("enabled", 1);
    let (status, y) = execution.run_typed(&parameters);
    assert_eq!(status, 0);
    let count = y[binding("count")] as usize;
    let features = (1..=count)
        .map(|row| [1, 2, 3].map(|column| y[binding(&format!("features[{row},{column}]"))]))
        .collect();
    Case {
        fuel: execution.fuel_consumed(),
        features,
    }
}

/// Wasmi fuel one cell of the image may cost. The selector's work is its
/// scans, a heap over the candidates, and a bounded neighbourhood per kept
/// feature; a fold that runs to its static capacity or sweeps the image per
/// kept feature costs over 100,000 per cell here and grows with the image.
const FUEL_PER_CELL: u64 = 12_000;

#[test]
fn ranked_selection_with_candidates_matches_the_source_within_its_algorithmic_fuel() {
    let _lock = session_test_guard();
    for size in [(100, 12), (200, 20), (400, 40)] {
        let artifact = artifact(size);
        let cells = size.0 * size.1;
        let scores = (0..cells).map(|cell| score(cell, 10)).collect::<Vec<_>>();
        let (candidates, features) = reference(size, &scores);
        assert!(candidates > 1, "the heap passes sift real candidates");
        let case = run(&artifact, &scores);
        assert_eq!(case.features, features);
        let budget = FUEL_PER_CELL * cells as u64;
        assert!(
            case.fuel < budget,
            "{} fuel over {cells} cells and {candidates} candidates, budget {budget}",
            case.fuel
        );
    }
}
