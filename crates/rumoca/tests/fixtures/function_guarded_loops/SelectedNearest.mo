package SelectedVocabulary
  function Scan
    input Real samples[3,2];
    input Real words[2,2];
    input Integer wordCount;
    input Boolean requested;
    input Boolean accepted;
    output Real y;
  protected
    Real nearest;
    Real distance;
    Boolean gate;
  algorithm
    y := 0;
    for feature in 1:3 loop
      if requested then
        nearest := 5;
        for word in 1:2 loop
          if word <= wordCount then
            distance := 0;
            for component in 1:2 loop
              distance := distance+(samples[feature,component]-words[word,component])
                *(samples[feature,component]-words[word,component]);
            end for;
            nearest := min(nearest,distance);
          end if;
        end for;
        y := y+nearest;
      end if;
    end for;
  end Scan;
end SelectedVocabulary;
model SelectedNearest
  input Real samples[3,2] = {{3,0},{1,0},{4,0}};
  input Real words[2,2] = {{0,0},{2,0}};
  input Integer wordCount = 2;
  input Boolean requested = true;
  input Boolean accepted = true;
  Real y = SelectedVocabulary.Scan(samples,words,wordCount,requested,accepted);
end SelectedNearest;
