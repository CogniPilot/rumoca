package AttributeNameCollision
  record R
    Real start;
    Real b;
  end R;
  partial model PartialRHolder
    parameter R p;
  end PartialRHolder;
  model RHolder
    extends PartialRHolder;
  end RHolder;
  model RecordKept
    replaceable RHolder h(p = R(5, 4)) constrainedby PartialRHolder(p(start = 3));
  end RecordKept;
  model RecordClassKept
    replaceable model H = RHolder(p = R(5, 4)) constrainedby PartialRHolder(p(start = 3));
    H h;
  end RecordClassKept;
  model RecordFinal
    replaceable RHolder h(p = R(5, 4)) constrainedby PartialRHolder(p(final start = 3));
  end RecordFinal;
  model RecordClassFinal
    replaceable model H = RHolder(p = R(5, 4)) constrainedby PartialRHolder(p(final start = 3));
    H h;
  end RecordClassFinal;
end AttributeNameCollision;

package UpperNestedAttributes
  record R
    Real a;
    Real b;
  end R;
  partial model PartialRHolder
    parameter R p;
  end PartialRHolder;
  model RHolder
    extends PartialRHolder;
  end RHolder;
  model RecordKept
    replaceable RHolder h(p(a(start = 0)) = R(5, 4)) constrainedby PartialRHolder(p(b = 3));
  end RecordKept;
  model RecordFinal
    replaceable RHolder h(p(a(start = 0)) = R(5, 4)) constrainedby PartialRHolder(p(final b = 3));
  end RecordFinal;
end UpperNestedAttributes;
