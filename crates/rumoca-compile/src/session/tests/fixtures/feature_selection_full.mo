// Full raster selection mathematics, including ranking and suppression. The
// host only transfers arrays, preserves source provenance and rejects valid=0.
// No algorithm is generated or executed by host selection code.
function SelectRasterFeatures
  input Real scores[:];
  input Integer width;
  input Integer height;
  input Integer capacity;
  input Integer minimumBorder;
  input Real settings[8]; // absolute,relative,rankScale,radius,cap,spacing,border,start
  input Boolean grid;
  output Real features[capacity,3];
  output Real count;
  output Real valid;
protected
  Integer candidateIndex[size(scores,1)];
  Real candidateRank[size(scores,1)];
  Boolean occupied[size(scores,1)];
  Integer candidateCount;
  Integer radius; Integer cap; Integer spacing; Integer border; Integer start;
  Integer x; Integer y; Integer index; Integer position;
  Integer root; Integer child; Integer heapSize; Integer temporaryIndex;
  Real temporaryRank; Real scaled; Real low; Real fraction; Real rank;
  Real maximum; Real threshold; Real guard;
  Boolean active; Boolean descending;
algorithm
  features := zeros(capacity,3);
  candidateIndex := fill(0,size(scores,1));
  candidateRank := zeros(size(scores,1));
  occupied := fill(false,size(scores,1));
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
  cap := if valid > 0.0 then integer(settings[5]) else 1;
  spacing := if valid > 0.0 then integer(settings[6]) else 1;
  border := if valid > 0.0 then integer(settings[7]) else 0;
  start := if valid > 0.0 then integer(settings[8]) else 0;
  candidateCount := 0;
  maximum := 0.0;
  threshold := 0.0;
  guard := 0.0;
  if valid > 0.0 then
    if grid then
      // Original grid traversal validates only visited scores, stopping at cap.
      y := start;
      while y < height-border and count < cap loop
        x := start;
        while x < width-border and count < cap loop
          index := y*width+x+1;
          if abs(scores[index]) <= 1.7976931348623157e308 then
            count := count+1.0;
            features[integer(count),1] := x;
            features[integer(count),2] := y;
            features[integer(count),3] := scores[index];
          else
            valid := 0.0;
          end if;
          x := x+spacing;
        end while;
        y := y+spacing;
      end while;
    else
      // Include the entire image in maximum/domain validation, even outside the
      // candidate border. The safe-integer rank domain is not silently widened.
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
        y := start;
        while y < height-border loop
          x := start;
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
            x := x+spacing;
          end while;
          y := y+spacing;
        end while;
        // In-place heap sort, O(N log N). Key is ascending rank, descending
        // raster index; reverse traversal gives exact descending-rank stable
        // raster ties without a quadratic full-frame comparison expansion.
        heapSize := candidateCount;
        for build in 1:candidateCount loop
          root := candidateCount-build+1;
          descending := true;
          while root <= div(heapSize,2) and descending loop
            child := 2*root;
            if child < heapSize then
              if candidateRank[child+1] > candidateRank[child] or
                (candidateRank[child+1] == candidateRank[child] and candidateIndex[child+1] < candidateIndex[child]) then
                child := child+1;
              end if;
            end if;
            if candidateRank[child] > candidateRank[root] or
              (candidateRank[child] == candidateRank[root] and candidateIndex[child] < candidateIndex[root]) then
              temporaryRank := candidateRank[root]; temporaryIndex := candidateIndex[root];
              candidateRank[root] := candidateRank[child]; candidateIndex[root] := candidateIndex[child];
              candidateRank[child] := temporaryRank; candidateIndex[child] := temporaryIndex;
              root := child;
            else
              descending := false;
            end if;
          end while;
        end for;
        for remove in 1:candidateCount loop
          heapSize := candidateCount-remove+1;
          temporaryRank := candidateRank[heapSize]; temporaryIndex := candidateIndex[heapSize];
          candidateRank[heapSize] := candidateRank[1]; candidateIndex[heapSize] := candidateIndex[1];
          candidateRank[1] := temporaryRank; candidateIndex[1] := temporaryIndex;
          heapSize := heapSize-1;
          root := 1;
          descending := true;
          while root <= div(heapSize,2) and descending loop
            child := 2*root;
            if child < heapSize then
              if candidateRank[child+1] > candidateRank[child] or
                (candidateRank[child+1] == candidateRank[child] and candidateIndex[child+1] < candidateIndex[child]) then
                child := child+1;
              end if;
            end if;
            if candidateRank[child] > candidateRank[root] or
              (candidateRank[child] == candidateRank[root] and candidateIndex[child] < candidateIndex[root]) then
              temporaryRank := candidateRank[root]; temporaryIndex := candidateIndex[root];
              candidateRank[root] := candidateRank[child]; candidateIndex[root] := candidateIndex[child];
              candidateRank[child] := temporaryRank; candidateIndex[child] := temporaryIndex;
              root := child;
            else
              descending := false;
            end if;
          end while;
        end for;
        active := true;
        for candidate in 1:candidateCount loop
          position := candidateCount-candidate+1;
          index := candidateIndex[position];
          // Preserve the original early stop on raw score, before occupancy.
          if scores[index] < threshold then
            active := false;
          end if;
          if active and count < cap and not occupied[index] then
            x := mod(index-1,width); y := div(index-1,width);
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

model FeatureSelection
  parameter Integer width = 160;
  parameter Integer height = 90;
  // Existing editable cap accepts any1..14400; retain storage for that domain.
  parameter Integer capacity = 14400;
  parameter Integer minimumBorder = 0;
  parameter Boolean grid = false;
  input Real scores[width*height] = zeros(width*height);
  input Real settings[8] = {1e-9,0.01,1e12,3.0,240.0,1.0,0.0,0.0};
  output Real features[capacity,3];
  output Real count;
  output Real valid;
equation
  (features,count,valid) = SelectRasterFeatures(scores,width,height,capacity,minimumBorder,settings,grid);
end FeatureSelection;

model GridFeatureSelection
  extends FeatureSelection(capacity=14400,grid=true,settings={0.0,0.0,1.0,0.0,14400.0,6.0,5.0,5.0});
end GridFeatureSelection;

model FastFeatureSelection
  extends FeatureSelection(minimumBorder=3,settings={18.0,0.0,1e8,3.0,240.0,1.0,3.0,3.0});
end FastFeatureSelection;
