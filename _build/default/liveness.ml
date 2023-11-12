open Imp
open Nimp

module VSet = Set.Make(String)

(* returns the set of variables accessed by the expression [e] *)
let rec use_expr e = match e with
  | Cst _ -> VSet.empty
  | Bool _ -> VSet.empty
  | Var s -> VSet.singleton s
  | Binop (_, e1, e2) -> VSet.union (use_expr e1) (use_expr e2)
  | Call (f, args) -> List.fold_left(fun acc x -> VSet.union (use_expr x) acc) VSet.empty args
  | Deref e -> use_expr e
  | Alloc e -> use_expr e
  | Addr s -> VSet.empty
  | DCall (e, args) ->
      VSet.union
        (use_expr e)
        (List.fold_left(fun acc x -> VSet.union (use_expr x) acc) VSet.empty args)


let vset = VSet.empty

let liveness fdef =
  let n = max_instr_list fdef.code in
  let live = Array.make (n+1) VSet.empty in
  (* returns the set of variable that live in entry to the numbered
     instruction [i], assuming a set of live variables [lv_out] on
     exit of [i] *)
  let rec lv_in_instr i lv_out = match i.instr with
    | (Putchar e | Expr e) -> VSet.union lv_out (use_expr e)
    | Set (x, e) -> VSet.union (VSet.diff lv_out (VSet.singleton x)) (use_expr e)
    | If (e, s1, s2) ->
        VSet.union
          (lv_in_list s2 lv_out)
          (VSet.union
            (lv_in_list s1 lv_out)
            (use_expr e)
          )
    | While (e, s1) ->
        lv_in_while_second_time
          (e, s1)
          (VSet.union (lv_in_list s1 lv_out) (use_expr e))
    | Return e -> VSet.union lv_out (use_expr e)
    | Write (e1, e2) ->
        VSet.union
          (VSet.diff lv_out (use_expr e1))
          (* (use_expr e2) *)
          (VSet.union
            (use_expr e1) (use_expr e2)
          )
    | Seq s -> lv_in_list s lv_out
  and lv_in_while_second_time (e, s1) lv_out = VSet.union (lv_in_list s1 lv_out) (use_expr e)

    (* by case on the contents of i.instr *)
  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list l lv_out = match l with
    | [] -> lv_out
    | i :: tl ->
        let lv_mid = lv_in_list tl lv_out in
        let lv_in = lv_in_instr i lv_mid in
        live.(i.nb) <- lv_in;
        lv_in
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live

let liveness_intervals_from_liveness fdef =
  let live = Array.to_list (liveness fdef) in
  let rec find_index f lst =
    match lst with
    | [] -> raise Not_found
    | h :: t -> if  f h = true then 0 else 1 + find_index f t
  in
  let is_in x set =
    try
      let _ = VSet.find x set in
      true
    with
      | Not_found -> false
  in
  let rec last_index x previous n arr = match arr with
    | [] -> previous
    | h :: t when is_in x h -> last_index x n (n+1) t
    | h :: t -> last_index x previous (n+1) t
  in

  let make_intervalle x =
    try
      let low = find_index (is_in x) live in
      let high = (last_index x (-1) 0 live) in
      (x, low, high)
    with
      | Not_found -> (x, -1, -1)
  in

  List.map make_intervalle (fdef.locals)

  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)
