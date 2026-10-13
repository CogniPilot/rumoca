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

model ExactRaster
  input Real scores[9] = {3,2,1,0,0,0,0,0,0};
  input Boolean enabled = true;
  input Boolean grid = false;
  Real features[3,3];
  Real count;
  Real valid;
  Real feature11 = features[1,1];
  Real feature12 = features[1,2];
  Real feature13 = features[1,3];
  Real feature21 = features[2,1];
  Real feature22 = features[2,2];
  Real feature23 = features[2,3];
  Real feature31 = features[3,1];
  Real feature32 = features[3,2];
  Real feature33 = features[3,3];
equation
  (features,count,valid) = SelectRasterFeatures(scores,3,3,3,0,{1.5,0,1,0,3,1,0,0},grid,enabled);
end ExactRaster;
