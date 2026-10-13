package CompactTransfer
  constant Integer nodeCount = 16;
  constant Integer width = 512;
  record Node
    Integer counts[width];
    Boolean active[width];
  end Node;
  record Packet
    Node nodes[nodeCount];
  end Packet;
  function Copy
    input Integer slots[2];
    input Real values[2];
    input Boolean requested;
    output Packet payload;
    output Real score;
  algorithm
    for node in 1:nodeCount loop
      payload.nodes[node].counts := fill(slots[1],width);
      payload.nodes[node].active := fill(requested,width);
    end for;
    score := if requested then values[1]+values[2] else -1.0;
  end Copy;
end CompactTransfer;

model CompactCallTransfer
  input Integer slots[2] = {1,2};
  input Real values[2] = {1.0,2.0};
  input Boolean requested = true;
  output CompactTransfer.Packet payload;
  Real score;
equation
  (payload,score) = CompactTransfer.Copy(slots,values,requested);
end CompactCallTransfer;
