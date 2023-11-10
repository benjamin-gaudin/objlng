open Objlng


let new_var =
  let cpt = ref (-1) in
  fun () -> incr cpt; Printf.sprintf "_%i" !cpt



let preprocessing_program (p : typ program ) : typ program =


  (*
  let rec tr_expr = function
    | Cst   x -> Expr (Cst x)
    | Bool  b -> Expr (Bool b)
    | Var   x -> Expr (Var x)
    | Binop (op, e1, e2) ->
        let var_e1 = new_var() in
        let var_e2 = new_var() in
        let tr_e1 = Set var_e1 (tr_expr e1) in
        let tr_e2 = Set var_e2 (tr_expr e2) in
        Seq ( [tr_e1; tr_e2; Binop (op, (Var var_e1), (Var var_e2)) ] )
    | Call  (f, args) -> let tr_args = List.flatten (List.map tr_expr args) in
        Expr (f, tr_args)
    | MCall (e1, method_name, e2) ->
        let var_e1 = new_var() in
        let var_e2 = new_var() in
        let tr_e1 = Set var_e1 (tr_expr e1) in
        let tr_e2 = Set var_e2 (tr_expr e2) in
        Seq ( [tr_e1; tr_e2; MCall ((Var var_e1), method_name, (Var var_e2)) ] )
    | New   (x, e) ->
        let var_e1 = new_var() in
        let tr_e1 = Set var_e1 (tr_expr e1) in
        Seq ( tr_e1, New (x, (Var var_e1))
    | NewTab (typ, e) ->
        let var_e1 = new_var() in
        let tr_e1 = Set var_e1 (tr_expr e1) in
        Seq ( tr_e1, NewTab (typ, (Var var_e1))
    | Read  mem ->                (* read in memory *)
        let var_mem = new_var() in
        let tr_mem = Set var_mem (tr_mem mem) in
        Seq ( tr_e1, NewTab (typ, (Var var_e1))
    | This (* current object *)
  and tr_mem = function
    | Arr (e1, e2) ->
        let var_e1 = new_var() in
        let var_e2 = new_var() in
        let tr_e1 = Set var_e1 (tr_expr e1) in
        let tr_e2 = Set var_e2 (tr_expr e2) in
        Seq ( tr_e1, tr_e2, Arr ((Var var_e1), (Var var_e2))
    | Atr (e, s) ->
        let var_e1 = new_var() in
        let tr_e1 = Set var_e1 (tr_expr e1) in
        Seq ( tr_e1, Atr ((Var var_e1), s)
  in
*)
  let var_typ = Hashtbl.create 16 in

  let rec tr_expr  (e : typ expression) : (typ instruction * string) = match e.expr with
    | Cst  x ->
        let var_x = new_var() in
        Hashtbl.add var_typ var_x TInt;
        ((Set (var_x, mk_expr e.annot (Cst x))), var_x)
    | Bool  b ->
        let var_b = new_var() in
        Hashtbl.add var_typ var_b TBool;
        (Set (var_b, (mk_expr e.annot (Bool b))), var_b)
    | Var   x ->
        (Expr (mk_expr e.annot (Var x)), x)
    | Binop (op, e1, e2) ->
        let tr_e1, var_e1 = tr_expr e1 in
        (* Hashtbl.add var_typ var_e1 e1.annot; *)
        let tr_e2, var_e2 = tr_expr e2 in
        (* Hashtbl.add var_typ var_e2 e2.annot; *)
        let var_res = new_var() in
        Hashtbl.add var_typ var_res e.annot;
        (Seq ( [tr_e1; tr_e2; Set (var_res, mk_expr e.annot (Binop (op,
        mk_expr e1.annot (Var var_e1),
        mk_expr e2.annot (Var var_e2)))) ]) , var_res )
    | Call  (f, args) ->
        let tr_args = List.map tr_expr args in
        let init_args = List.map fst tr_args in
        let vars_args = List.map snd tr_args in
        let typ_args = List.map (fun x -> x.annot) args in
        (* List.iter2 (Hashtbl.add var_typ) vars_args typ_args; *)
        let typed_args =
          List.map2 (fun typ var -> mk_expr typ (Var var)) typ_args vars_args
        in
        let var_res = new_var() in
        Hashtbl.add var_typ var_res e.annot;
        (Seq (init_args @ [ Set (var_res, mk_expr e.annot (Call (f, typed_args))) ]), var_res)
    | MCall (e1, method_name, args) ->
        let tr_e1, var_e1 = tr_expr e1 in
        (* Hashtbl.add var_typ var_e1 e1.annot; *)
        let tr_args = List.map tr_expr args in
        let init_args = List.map fst tr_args in
        let vars_args = List.map snd tr_args in
        let typ_args = List.map (fun x -> x.annot) args in
        (* List.iter2 (Hashtbl.add var_typ) vars_args typ_args; *)
        let typed_args =
          List.map2 (fun typ var -> mk_expr typ (Var var)) typ_args vars_args
        in
        let var_res = new_var() in
        Hashtbl.add var_typ var_res e.annot;
        (Seq ( [tr_e1] @ init_args @
          [Set (var_res, mk_expr e.annot
          (MCall (mk_expr e1.annot (Var var_e1), method_name, typed_args))) ]), var_res)
    | New   (x, args) ->
        let tr_args = List.map tr_expr args in
        let init_args = List.map fst tr_args in
        let vars_args = List.map snd tr_args in
        let typ_args = List.map (fun x -> x.annot) args in
        (* List.iter2 (Hashtbl.add var_typ) vars_args typ_args; *)
        let typed_args =
          List.map2 (fun typ var -> mk_expr typ (Var var)) typ_args vars_args
        in
        let var_res = new_var() in
        Hashtbl.add var_typ var_res e.annot;
        (Seq ( init_args @ [ Set (var_res, mk_expr e.annot (New (x, typed_args)))]), var_res)
    | NewTab (typ, e1) ->
        let tr_e1, var_e1 = tr_expr e1 in
        (* Hashtbl.add var_typ var_e1 e1.annot; *)
        let var_res = new_var() in
        Hashtbl.add var_typ var_res e.annot;
        (Seq [ tr_e1; Set (var_res,
          mk_expr e1.annot (NewTab (typ, mk_expr e1.annot (Var var_e1))))], var_res)
        (*
    | Read  mem ->                (* read in memory *)
        let tr_mem, var_mem = tr_mem mem in
        let var_res = new_var() in
        (Seq [ tr_mem; Set (var_res, mk_expr e.annot (Read (mk_expr (Var var_mem))))], var_res)
        *)
    | Read (Arr (e1, e2)) ->                (* read in memory *)
        let tr_e1, var_e1 = tr_expr e1 in
        (* Hashtbl.add var_typ var_e1 e1.annot; *)
        let tr_e2, var_e2 = tr_expr e2 in
        (* Hashtbl.add var_typ var_e2 e2.annot; *)
        let var_res = new_var() in
        Hashtbl.add var_typ var_res e.annot;
        (Seq [ tr_e1; tr_e2; Set (var_res, mk_expr e.annot (Read
         (Arr ( mk_expr e1.annot (Var var_e1), mk_expr
          e2.annot (Var var_e2)))))], var_res)
    | Read (Atr (e1, s)) ->                (* read in memory *)
        let tr_e1, var_e1 = tr_expr e1 in
        (* Hashtbl.add var_typ var_e1 e1.annot; *)
        let var_res = new_var() in
        Hashtbl.add var_typ var_res e.annot;
        (Seq [ tr_e1; Set (var_res, mk_expr e.annot (Read
         (Atr ( mk_expr e1.annot (Var var_e1), s))))], var_res)
    | This -> (Expr (mk_expr e.annot This), "_this")
    (*
  and tr_mem = function
    | Arr (e1, e2) ->
        let tr_e1, var_e1 = tr_expr e1 in
        let tr_e2, var_e2 = tr_expr e2 in
        (Seq [ tr_e1; tr_e2; Set (var_res, (Arr ((Var var_e1), (Var var_e2))))], var_res)
    | Atr (e1, s) ->
        let tr_e1, var_e1 = tr_expr e1 in
        let var_res = new_var() in
        (Seq [ tr_e1; Set (var_res, (Atr ((Var var_e1), s)))], var_res)
        *)
  in

  let rec tr_instr ( e : typ instruction) : typ instruction = match e with
    | Putchar e ->
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e; Putchar (mk_expr e.annot (Var var_e)) ] )
    | Set (s, e) ->
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e; Set (s, mk_expr e.annot (Var var_e)) ] )
    | If (e, s1, s2) ->
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e; If (mk_expr e.annot (Var var_e), [(tr_instr (Seq s1))],
          [(tr_instr (Seq s2))]) ] )
    | While (e, s) ->
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e; While (mk_expr e.annot (Var var_e), [(tr_instr (Seq s))]) ] )
    | Return e ->
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e; Return (mk_expr e.annot (Var var_e)) ] )
    | Expr e ->
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e; Expr (mk_expr e.annot (Var var_e)) ] )
    | Write (Arr (e1, e2), e) ->
        let tr_e1, var_e1 = tr_expr e1 in
        let tr_e2, var_e2 = tr_expr e2 in
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e1; tr_e2; tr_e;
          Write (Arr
          (mk_expr e1.annot (Var var_e1),
           mk_expr e2.annot (Var var_e2)),
           mk_expr e.annot (Var var_e)) ] )
    | Write (Atr (e1, s), e) ->
        let tr_e1, var_e1 = tr_expr e1 in
        let tr_e, var_e = tr_expr e in
        Seq ( [tr_e1; tr_e;
          Write (Atr
          (mk_expr e1.annot (Var var_e1), s),
           mk_expr e.annot (Var var_e)) ] )
    | Seq seq ->
        Seq ( List.map tr_instr seq)
  in

  let tr_function (f : typ function_def) : typ function_def =
    {f with code = List.map tr_instr f.code}
  in

  let functions = List.map tr_function p.functions in
  (* let max_temp_var = (int_of_string (new_var())) - 1 in *)
  (* let create_new_locals j = *)
  (*   let rec aux n acc = *)
  (*     if n < 0 then *)
  (*       acc *)
  (*     else aux (n-1) (("_" ^ (string_of_int n)) :: acc) *)
  (*   in aux j [] *)
  (* in *)
  (* let new_temp_var = create_new_locals max_temp_var in *)
  let new_temp_var = Hashtbl.fold (fun k v acc -> (k, v) :: acc) var_typ [] in
  (* print_endline "new :"; *)
  (* List.iter (fun (k,v) -> print_endline k;) new_temp_var; *)
  {p with functions = functions;
          globals = new_temp_var @ p.globals}






