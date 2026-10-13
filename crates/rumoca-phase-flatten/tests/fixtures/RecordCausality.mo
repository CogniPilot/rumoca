package RecordCausality
  record Leaf
    Real values[2](each start=1.25);
    Integer count(start=2);
    Boolean valid(start=true);
  end Leaf;

  record Packet
    Leaf leaf;
    Real scalar(start=0.5);
  end Packet;

  model Internal
    input Packet incoming;
    output Packet outgoing;
  equation
    outgoing = incoming;
  end Internal;

  model NestedRecord
    input Packet incoming;
    output Packet outgoing;
    Packet local;
    Internal child(incoming=incoming);
  equation
    outgoing = incoming;
    local = incoming;
  end NestedRecord;

  model RecordArray
    input Packet incoming[2];
    output Packet outgoing[2];
    Packet local[2];
  equation
    outgoing = incoming;
    local = incoming;
  end RecordArray;

  model InheritedRecord
    extends NestedRecord(incoming(leaf(values(each start=3.5)), scalar(start=7.0)));
  end InheritedRecord;

  model ProtectedRecord
    extends NestedRecord;
  protected
    input Packet hidden;
    output Packet hiddenResult;
  equation
    hidden = incoming;
    hiddenResult = hidden;
  end ProtectedRecord;
end RecordCausality;
