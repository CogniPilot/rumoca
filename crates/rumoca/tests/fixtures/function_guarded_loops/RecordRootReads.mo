package RecordRead
  record Leaf
    Real weight;
    Real other;
  end Leaf;
  record Tree
    Leaf branch;
    Real scale;
  end Tree;
  function Score
    input Real values[2];
    input Boolean requested;
    output Real y;
  protected
    Tree tree;
    Boolean valid;
  algorithm
    y := -1;
    valid := requested;
    if valid then
      tree := Tree(Leaf(values[1],values[2]),2);
    end if;
    if valid then
      y := tree.branch.weight + tree.scale;
    end if;
  end Score;
  function Quoted
    input Real values[2];
    input Boolean requested;
    output Real y;
  protected
    Tree 'tree.dot';
  algorithm
    y := -1;
    if requested then
      'tree.dot' := Tree(Leaf(values[1],values[2]),2);
    end if;
    if requested then
      y := 'tree.dot'.branch.other + 'tree.dot'.scale;
    end if;
  end Quoted;
  function InputOnly
    input Tree tree;
    output Real y;
  algorithm
    y := tree.branch.weight + tree.branch.other + tree.scale;
  end InputOnly;
  function FieldOnly
    input Real value;
    output Real y;
  protected
    Tree fieldRecord;
  algorithm
    fieldRecord.scale := value;
    y := fieldRecord.scale;
  end FieldOnly;
end RecordRead;
model RecordRootReads
  input Real values[2] = {1,2};
  input Boolean requested = true;
  Real score = RecordRead.Score(values,requested);
  Real quoted = RecordRead.Quoted(values,requested);
  Real immutable = RecordRead.InputOnly(RecordRead.Tree(RecordRead.Leaf(values[1],values[2]),2));
end RecordRootReads;
model RecordFieldOnly
  input Real values[2] = {1,2};
  Real fieldOnly = RecordRead.FieldOnly(values[2]);
end RecordFieldOnly;
