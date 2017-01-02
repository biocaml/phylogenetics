open Sigs

module Make (S:SEQUENCE):
  ALIGNMENT with type base = S.base and type sequence = S.t

module Make_alist (S:SEQUENCE):
  ALIGNMENT with type base = S.base and type sequence = S.t
