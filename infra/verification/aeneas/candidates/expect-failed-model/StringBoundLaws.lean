import Aeneas

open Aeneas Aeneas.Std

namespace StringBoundLaws

theorem explicit_symbolic_bound (s : String) (h : s.toByteArray.size ≤ U32.max) :
    (toStr s h).val.length = s.toByteArray.size := by
  simp [toStr, Slice.from_val, ByteArray.length_toList]

theorem explicit_symbolic_bytes (s : String) (h : s.toByteArray.size ≤ U32.max) :
    (toStr s h).val.map (fun b => b.val) = s.toByteArray.toList.map UInt8.toNat := by
  simp only [toStr, Slice.from_val, List.map_map]
  rfl

theorem empty_length : (toStr "").val.length = 0 := by
  rw [explicit_symbolic_bound]
  decide

theorem ascii_length : (toStr "A").val.length = 1 := by
  rw [explicit_symbolic_bound]
  decide

theorem two_byte_utf8 : (toStr "é").val.length = 2 := by
  rw [explicit_symbolic_bound]
  decide

theorem three_byte_utf8 : (toStr "€").val.length = 3 := by
  rw [explicit_symbolic_bound]
  decide

theorem four_byte_utf8 : (toStr "😀").val.length = 4 := by
  rw [explicit_symbolic_bound]
  decide

theorem mixed_utf8_length : (toStr "Aé€😀").val.length = 10 := by
  rw [explicit_symbolic_bound]
  decide

/-- info: 'StringBoundLaws.explicit_symbolic_bound' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms explicit_symbolic_bound
/-- info: 'StringBoundLaws.explicit_symbolic_bytes' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms explicit_symbolic_bytes
/-- info: 'StringBoundLaws.empty_length' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms empty_length
/-- info: 'StringBoundLaws.ascii_length' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms ascii_length
/-- info: 'StringBoundLaws.two_byte_utf8' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms two_byte_utf8
/-- info: 'StringBoundLaws.three_byte_utf8' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms three_byte_utf8
/-- info: 'StringBoundLaws.four_byte_utf8' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms four_byte_utf8
/-- info: 'StringBoundLaws.mixed_utf8_length' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms mixed_utf8_length

end StringBoundLaws
