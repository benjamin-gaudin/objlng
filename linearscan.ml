open Imp
open Nimp

(* sort by ascending lower bound, and sort equals by ascending upper bound *)
let sort_2 l =
  List.stable_sort (fun (_, l1, _) (_, l2, _) -> l1 - l2) l
let sort_3 l =
  List.stable_sort (fun (_, _, h1) (_, _, h2) -> h1 - h2) l
let sort_intervals l =
  sort_2 (sort_3 l)

(* insert interval [i] in active list [l]
   pre/post-condition: sorted by ascending upper bound *)
let rec insert_active i l =
  sort_intervals (i :: l)

(* raw allocation information for a variable *)
type raw_alloc =
  | RegN  of int  (* index of the register *)
  | Spill of int  (* index of the spill *)

let get_regN e = match e with
  | RegN n -> n
  | _ -> failwith "it's not a RegN"

(* allocation of the local variables of a function [fdef] using linear scan
   algorithm, with [nb_regs] registers available for allocation ;
   return a raw allocation for each variable, as well as the maximum index of
   used registers, and the number of used stack slots (spills) *)
let lscan_alloc nb_regs fdef =
  let live_intervals = Liveness.liveness_intervals_from_liveness fdef in
  let alloc = Hashtbl.create (List.length (fdef.locals)) in
  let (active : (string * int * int) list ref ) = ref [] in
  let free = ref (List.init nb_regs (fun i -> i)) in
  let r_max = ref (-1) in (* maximum index of used register *)
  let spill_count = ref 0 in (* number of spilled variables *)
  (* free registers allocated to intervals that stop before timestamp a,
     returns remaining intervals *)
  let expire a l =
    let remove_active x =
      let memory_place = Hashtbl.find alloc x in
      match memory_place with
      | RegN n -> free := n :: !free;
      | _ -> () ;
      Hashtbl.remove alloc x;
      (*
      List.filter (fun (x2,_,_) -> x2 <> x) l
      *)
    in

    let is_continued a interval = match interval with
      | (_, _, h) when h > a -> true
      | (x,_,_) -> (* active := *) remove_active x;
                    false
    in
    List.filter (is_continued a) l
  in
  (* for each interval i, in sorted order *)
  let list_is_empty l = List.compare_length_with l 0 = 0 in
(*
  List.iter (fun i ->
    let xi, li, hi = i in
    print_string ("xi :"^ xi ^ " li :");
    print_int li;
    print_string " hi :";
    print_int hi;
    print_endline "";
*)



  List.iter (fun i ->
      let xi, li, hi = i in
      active := expire li !active;
      if not (list_is_empty !free) then (
        let new_reg = List.hd !free in
        (*
        print_endline "avant";
        List.iter print_int !free;
        print_endline "";
        *)
        free := List.tl !free;
        (*
        print_endline "apres";
        List.iter print_int !free;
        print_endline "";
        *)
        r_max := max (!r_max) (nb_regs - (List.length !free) -1);
        active := insert_active i !active;
        Hashtbl.replace alloc xi (RegN new_reg);
      )
      else
        Hashtbl.replace alloc xi (Spill !spill_count);
        incr spill_count;

      (* free registers that expire before the lower bound of i *)
      (* if there are available registers *)
        (* ... then allocate one *)
        (* otherwise, may replace an already used register if this can
           make this register available again earlier *)
    ) (sort_intervals live_intervals);
  alloc, !r_max, !spill_count
