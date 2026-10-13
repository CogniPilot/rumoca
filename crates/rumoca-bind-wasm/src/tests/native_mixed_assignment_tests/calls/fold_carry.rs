//! A raster selector carries two candidate arrays through a `for` fold whose
//! body re-carries them through a nested `while` fold. The carried arrays stay
//! in the storage the fold already owns, so a pass costs its own work and never
//! a copy of the arrays.

use super::*;

const SOURCE: &str = r#"
function SelectRasterFeatures
  input Real scores[:];
  input Integer width;
  input Integer height;
  input Integer capacity;
  input Real settings[8];
  input Boolean enabled;
  output Real features[capacity,3];
  output Real count;
  output Real valid;
  output Real sortedRank[size(scores,1)];
  output Real sortedIndex[size(scores,1)];
protected
  Integer candidateIndex[size(scores,1)];
  Real candidateRank[size(scores,1)];
  Integer candidateCount;
  Integer radius;
  Integer featureLimit;
  Integer sampleSpacing;
  Integer border;
  Integer firstPixel;
  Integer x;
  Integer y;
  Integer index;
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
    settings[7] >= 1.0 and settings[7] <= floor((height-1)/2.0) and floor(settings[7]) == settings[7] and
    settings[8] >= settings[7] and settings[8] < height-settings[7] and floor(settings[8]) == settings[8]
    then 1.0 else 0.0;
  radius := if valid > 0.0 then integer(settings[4]) else 0;
  featureLimit := if valid > 0.0 then integer(settings[5]) else 1;
  sampleSpacing := if valid > 0.0 then integer(settings[6]) else 1;
  border := if valid > 0.0 then integer(settings[7]) else 0;
  firstPixel := if valid > 0.0 then integer(settings[8]) else 0;
  candidateCount := 0;
  maximum := 0.0;
  threshold := 0.0;
  guard := 0.0;
  sortedRank := zeros(size(scores,1));
  sortedIndex := zeros(size(scores,1));
  if valid > 0.0 and enabled then
    candidateIndex := fill(0,size(scores,1));
    candidateRank := zeros(size(scores,1));
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
      heapSize := candidateCount;
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
      for i in 1:size(scores,1) loop
        sortedRank[i] := candidateRank[i];
        sortedIndex[i] := candidateIndex[i];
      end for;
    end if;
  end if;
end SelectRasterFeatures;

model SelectRaster
  parameter Integer width = @WIDTH@;
  parameter Integer height = @HEIGHT@;
  input Boolean enabled = true;
  input Real scores[width*height] = zeros(width*height);
  input Real settings[8] = {18.0,0.0,1e8,1.0,@LIMIT@.0,1.0,3.0,3.0};
  output Real count;
  output Real valid;
  output Real sortedRank[width*height];
  output Real sortedIndex[width*height];
protected
  Real features[@LIMIT@,3];
equation
  (features,count,valid,sortedRank,sortedIndex) =
    SelectRasterFeatures(scores,width,height,@LIMIT@,settings,enabled);
end SelectRaster;
"#;

/// Image sizes: a small one checked against the source algorithm, and a large
/// one whose carried arrays make any per-pass copy dominate the pass.
const SMALL: (usize, usize) = (100, 12);
const LARGE: (usize, usize) = (400, 30);
const LIMIT: usize = 240;
const BORDER: usize = 3;
/// Wasmi fuel one pass over an idle cell may spend. A copy of the two carried
/// arrays costs several times this at the large size.
const FUEL_PER_CELL: u64 = 2500;
/// Wasmi fuel a selector with no candidate may spend per cell: the raster scans
/// only, since the removal loop runs the zero passes its run-time bound allows
/// and the inner sift loop never starts. Running every one of the capacity
/// passes costs about 1,700 per cell here, the scans alone about 750.
const IDLE_FUEL_PER_CELL: u64 = 1000;

/// The score at one cell: candidates are the cells whose score reaches the
/// absolute threshold, spread so several share a rank.
fn score(cell: usize, candidates: bool) -> f64 {
    if candidates && cell.is_multiple_of(7) {
        20.0 + (cell % 5) as f64 * 0.25
    } else {
        (cell % 11) as f64 * 0.5
    }
}

fn round_half_even_rank(score: f64) -> f64 {
    let scaled = score * 1e8;
    let low = scaled.floor();
    let fraction = scaled - low;
    if fraction < 0.5 {
        low
    } else if fraction > 0.5 {
        low + 1.0
    } else if low - (low / 2.0).floor() * 2.0 == 0.0 {
        low
    } else {
        low + 1.0
    }
}

/// The source algorithm's raster-order candidates.
fn candidates((width, height): (usize, usize), scores: &[f64]) -> (usize, Vec<f64>, Vec<f64>) {
    let guard = (18.0f64 * 1e8).floor() - 1.0;
    let mut rank = vec![0.0; scores.len()];
    let mut index = vec![0.0; scores.len()];
    let mut count = 0;
    let cells = (BORDER..height - BORDER)
        .flat_map(|y| (BORDER..width - BORDER).map(move |x| y * width + x));
    for cell in cells {
        let candidate = round_half_even_rank(scores[cell]);
        if candidate >= guard {
            rank[count] = candidate;
            index[count] = (cell + 1) as f64;
            count += 1;
        }
    }
    (count, rank, index)
}

/// One removal pass's sift of the root through the first `heap` entries.
fn sift(rank: &mut [f64], index: &mut [f64], heap: usize) {
    let mut root = 1;
    while root <= heap / 2 {
        let mut child = 2 * root;
        let larger = |a: usize, b: usize| {
            rank[a - 1] > rank[b - 1] || (rank[a - 1] == rank[b - 1] && index[a - 1] < index[b - 1])
        };
        if child < heap && larger(child + 1, child) {
            child += 1;
        }
        if !larger(child, root) {
            return;
        }
        rank.swap(child - 1, root - 1);
        index.swap(child - 1, root - 1);
        root = child;
    }
}

/// The source algorithm: candidates, then the in-place removal passes over
/// the candidate arrays.
fn reference(size: (usize, usize), scores: &[f64]) -> (usize, Vec<f64>, Vec<f64>) {
    let (count, mut rank, mut index) = candidates(size, scores);
    for remove in 1..=count {
        let last = count - remove + 1;
        rank.swap(last - 1, 0);
        index.swap(last - 1, 0);
        sift(&mut rank, &mut index, last - 1);
    }
    (count, rank, index)
}

fn artifact((width, height): (usize, usize)) -> serde_json::Value {
    let source = SOURCE
        .replace("@WIDTH@", &width.to_string())
        .replace("@HEIGHT@", &height.to_string())
        .replace("@LIMIT@", &LIMIT.to_string());
    serde_json::from_str(
        &crate::native_program_api::prepare_native_program(&source, "SelectRaster").unwrap(),
    )
    .unwrap()
}

struct Case {
    status: i32,
    fuel: u64,
    count: f64,
    rank: Vec<f64>,
    index: Vec<f64>,
}

fn run(artifact: &serde_json::Value, scores: &[f64], enabled: bool) -> Case {
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
    execution.set_input("enabled", i64::from(enabled));
    let (status, y) = execution.run_typed(&parameters);
    let column = |name: &str, cell: usize| y[binding(&format!("{name}[{}]", cell + 1))];
    Case {
        status,
        fuel: execution.fuel_consumed(),
        count: y[binding("count")],
        rank: (0..scores.len())
            .map(|cell| column("sortedRank", cell))
            .collect(),
        index: (0..scores.len())
            .map(|cell| column("sortedIndex", cell))
            .collect(),
    }
}

/// The largest output-slot storage any fold transition region allocates.
fn largest_transition_slots(frame: &serde_json::Value) -> u64 {
    frame["regions"]
        .as_array()
        .unwrap()
        .iter()
        .map(|region| {
            let own = if region["role"] == "transition" {
                region["frame"]["slot_bytes"].as_u64().unwrap()
            } else {
                0
            };
            own.max(largest_transition_slots(&region["frame"]))
        })
        .max()
        .unwrap_or(0)
}

#[test]
fn carried_arrays_of_nested_folds_are_returned_in_place_and_sort_like_the_source() {
    let _lock = session_test_guard();
    let artifact = artifact(SMALL);
    let array_bytes = (SMALL.0 * SMALL.1 * 8) as u64;
    for owner in artifact["abi"]["scratch_report"]["owners"]
        .as_array()
        .unwrap()
    {
        let slots = largest_transition_slots(&owner["frame"]);
        assert!(
            slots < array_bytes,
            "a fold transition allocated {slots} output bytes beside {array_bytes} byte arrays"
        );
    }
    let scores = (0..SMALL.0 * SMALL.1)
        .map(|cell| score(cell, true))
        .collect::<Vec<_>>();
    let (count, rank, index) = reference(SMALL, &scores);
    assert!(count > 1, "the removal passes sift real candidates");
    let case = run(&artifact, &scores, true);
    assert_eq!(case.status, 0);
    assert_eq!(case.count, 0.0, "the selector publishes no features");
    assert_eq!((case.rank, case.index), (rank, index));
}

#[test]
fn a_pass_over_nested_carried_arrays_costs_no_array_copy() {
    let _lock = session_test_guard();
    let artifact = artifact(LARGE);
    let cells = LARGE.0 * LARGE.1;
    let budget = FUEL_PER_CELL * cells as u64;
    let scores = (0..cells)
        .map(|cell| score(cell, false))
        .collect::<Vec<_>>();
    assert_eq!(reference(LARGE, &scores).0, 0);
    for enabled in [true, false] {
        let case = run(&artifact, &scores, enabled);
        assert_eq!(case.status, 0);
        assert!(case.rank.iter().chain(&case.index).all(|v| *v == 0.0));
        assert!(
            case.fuel < budget,
            "enabled={enabled}: {} fuel over {cells} cells, budget {budget}",
            case.fuel
        );
    }
}

#[test]
fn a_selection_without_candidates_runs_none_of_the_removal_passes() {
    let _lock = session_test_guard();
    let artifact = artifact(LARGE);
    let cells = LARGE.0 * LARGE.1;
    let budget = IDLE_FUEL_PER_CELL * cells as u64;
    let scores = (0..cells)
        .map(|cell| score(cell, false))
        .collect::<Vec<_>>();
    assert_eq!(reference(LARGE, &scores).0, 0);
    for enabled in [true, false] {
        let case = run(&artifact, &scores, enabled);
        assert_eq!(case.status, 0);
        assert!(
            case.fuel < budget,
            "enabled={enabled}: {} fuel over {cells} cells, budget {budget}",
            case.fuel
        );
    }
}
