open Sigs

module Make: functor (E:EVOL_MODEL) -> sig
  type base
  type align
  type sequence
  type param

  val felsenstein_single : ?shift:(float -> float -> LATools.vec -> LATools.vec * float) ->
    param -> site:int -> TopoTree.t -> align -> float

  val felsenstein_single_shift : ?threshold:float -> param -> site:int -> TopoTree.t -> align -> float

  val felsenstein_noshift : param -> TopoTree.t -> align -> float

  val felsenstein : param -> TopoTree.t -> align -> float
end with type param = E.t
     and type sequence = E.Seq.t
     and type align = E.Align.t
     and type base = E.Base.t
