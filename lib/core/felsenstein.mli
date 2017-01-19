open Sigs

module Make: functor (E:EVOL_MODEL) -> sig

  val felsenstein_single : ?shift:(float -> float -> LATools.vec -> LATools.vec * float) ->
    E.t -> site:int -> TopoTree.t -> E.Align.t -> float

  val felsenstein_single_shift : ?threshold:float -> E.t -> site:int -> TopoTree.t -> E.Align.t -> float

  val felsenstein_noshift : E.t -> TopoTree.t -> E.Align.t -> float

  val felsenstein : E.t -> TopoTree.t -> E.Align.t -> float

end
