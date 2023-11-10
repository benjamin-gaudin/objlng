open Imp
open Objlng

let tr_op: Objlng.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

let rec find_index f lst =
  match lst with
  | [] -> raise Not_found
  | h :: t -> if  f h = true then 0 else 1 + find_index f t

let remove_method_class method_name=
  String.concat "_" ( List.tl (String.split_on_char '_' method_name))

let find_index_method method_name method_list =
  find_index
    (fun (x : 'a function_def) -> method_name = remove_method_class x.name)
    method_list

let imp_add x y = Binop (Add, x, y)

(* main translation function *)
let translate_program (p: Objlng.typ Objlng.program) =

  let find_class_def name =
    List.find (fun cla_def -> String.equal cla_def.name name) p.classes
  in

  (* translation of an expression *)
  let rec tr_expr (te: Objlng.typ Objlng.expression): Imp.expression = match te.expr with
    | Cst n  -> Cst n
    | Bool b -> Bool b
    | Var x  -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    | Call (f,e) -> Call (f, List.map tr_expr e)
    | MCall (e1, method_name, args) ->
       let cla_name = get_name e1 in
       let cla_def = find_class_def cla_name in
       let index_method = find_index_method method_name cla_def.methods in
       DCall ((Deref (Binop (Add, (Deref (tr_expr e1)), (Cst (4 * (index_method + 1)))))),
              (List.map tr_expr (e1 :: args)))
    | NewTab (typ, e) -> Alloc (Binop (Mul, tr_expr e, Cst 4))
    | Read mem -> Deref (tr_mem mem)
    | This -> Var "_this"
    | _ -> assert false
  and tr_mem = function
    | Arr (e1, e2) -> Imp.array_offset (tr_expr e1) (tr_expr e2)
    | Atr (e1, s) ->
        let cla_name = get_name e1 in
        let index =
          find_index (fun x -> fst x = s) (find_class_def cla_name).fields
        in
        Imp.array_offset (tr_expr e1) (Cst (index+1))

  in

  let rec tr_seq s = List.map tr_instr s
  and tr_instr: Objlng.typ Objlng.instruction -> Imp.instruction = function
    | Putchar e     -> Putchar(tr_expr e)
    | Set     (s, { annot; expr = (New (class_name, args))}) ->
        Seq
        [
        Set (s, (Alloc (Cst(4 * (1 + List.length (find_class_def class_name).fields)))));
        Write (Var s, Addr(class_name ^ "_" ^ "descr"));
        Expr (Call (class_name ^ "_constructor", Var s :: List.map tr_expr args));
        ]
    | Set     (s, e)-> Set (s, tr_expr e)
    | If      (e, seq1, seq2) -> If (tr_expr e, tr_seq seq1, tr_seq seq2)
    | While   (e, seq) -> While (tr_expr e, tr_seq seq)
    | Return  e -> Return (tr_expr e)
    | Expr    e -> Expr (tr_expr e)
    | Write   (mem, e) -> Write (tr_mem mem, tr_expr e)
    | Seq seq -> Seq (tr_seq seq)
  in

  let tr_class_methods cla =
    let tr_method (f : 'a function_def) =
      (*{ name = cla.name ^ "_" ^ f.name;)*)
      { name = f.name;
        params = List.map fst f.params;
        locals = List.map fst f.locals;
        code = tr_seq f.code; }
    in
    let methods_of_this_class =
      List.filter ( fun (x : 'a function_def) ->
        cla.name = List.hd (String.split_on_char '_' x.name))
      cla.methods
    in
    List.map tr_method methods_of_this_class
  in

  let tr_fdef (fdef: Objlng.typ Objlng.function_def) =
    { Imp.name = fdef.name;
      params = List.map fst fdef.params;
      locals = List.map fst fdef.locals;
      code = tr_seq fdef.code;
    }
  in

  let functions = List.map tr_fdef p.functions in
  let functions2 = List.flatten (List.map tr_class_methods p.classes) in

  let class_descrs = List.map (fun (cla_def :'a Objlng.class_def) ->
    { descr_name = cla_def.name ^ "_descr";
    (*
     methods = List.map (fun (m : 'a function_def) -> cla_def.name ^"_"^ m.name) cla_def.methods;
     *)
     methods = List.map (fun (m : 'a function_def) -> m.name) cla_def.methods;
     parent = if Option.is_some cla_def.parent then
                Some (Option.get cla_def.parent ^ "_descr")
              else None;
    }) p.classes
  in

    { Imp.globals = List.map fst p.globals @
      List.map (fun x -> x.descr_name) class_descrs;
    class_descrs = class_descrs;
    functions = functions @ functions2 }

