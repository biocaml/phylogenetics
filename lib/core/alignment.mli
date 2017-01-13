open Sigs

module Make (S:SEQUENCE):
  ALIGNMENT with type base = S.base and type sequence = S.t and type index=TopoTree.index
