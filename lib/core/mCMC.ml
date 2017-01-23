open Core_kernel.Std

let accept p =
  Random.float 1.0 <= p

module Make (E:Sigs.EVOL_MODEL) = struct
  include Felsenstein.Make(E)
  include E

  let walk (theta0:'a) (step:'a->'a) (f:'a->float) (nb_points:int) =
    let rec aux i prev acc =
      if i>=nb_points then acc
      else
        let candidate = step prev in
        let ratio = (f candidate)/.(f prev) in (* FIXME unnecessary computation of f prev *)
        if accept ratio then aux (i+1) candidate (candidate::acc)
        else aux i prev acc
    in
    aux 0 theta0 []

end

module MCMC_DNA = Make(Models.K80)

let myalign = MCMC_DNA.Align.of_string_list ["A";"A";"A";"T"]

let mybasetree = TopoTree.of_preorder "0.1;0.1;0.1;0.1;0;1;0.1;0.1;2;3"
