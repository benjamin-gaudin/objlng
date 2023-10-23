open Imp
open Objlng

let tr_op: Objlng.binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

let rec find_index f lst =
  match lst with
  | [] -> -1
  | h :: t -> if  f h = true then 0 else 1 + find_index f t


(* main translation function *)
let translate_program (p: Objlng.typ Objlng.program) =

  (* translation of an expression *)
  let rec tr_expr (te: Objlng.typ Objlng.expression): Imp.expression = match te.expr with
    | Cst n  -> Cst n
    | Bool b -> Bool b
    | Var x  -> Var x
    | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
    | Call (f,e) -> Call (f, List.map tr_expr e)
    | MCall (e1, method_name, args) -> 
                                       (match e1.annot with
                                        | TClass cla_name -> let cla_def = List.find (fun (x : 'a Objlng.class_def) -> x.name = cla_name) p.classes in
                                                             let index_method = (match find_index (fun (x : 'a Objlng.function_def) -> x.name = method_name) cla_def.methods with
                                                                                | -1 -> failwith "method does not exist"
                                                                                | n -> n
                                                                                )
                                                             in 
                                                             DCall ((Deref (Binop (Add, Deref (tr_expr e1), Cst (4 * (index_method + 1))))), (List.map tr_expr (e1 :: args)))
                                                             
                                                             
                                        | _ -> failwith "method call not on function"
                                       )



(*                                       
      let c = tr_expression e1 in (* c = TClass cla_name*)
      let args = c :: (List.map tr_expression args) in
      let f = Imp.Binop (Add, c, Cst (get_method_offset o.annot s)) in
      Imp.DCall (Deref f, args)
*)



                                       (*
    | New (s, constructor) -> Seq (Expr (Alloc (Binop (Mul, Cst ( 1 + (List.length (List.find (fun (x : 'a Objlng.class_def) -> x.name = s) p.classes).fields)), Cst 4))) :: (List.map (fun x -> Imp.Expr (tr_expr x)) constructor))
    | New (s, args) -> let cla_def = List.find (fun (x : 'a Objlng.class_def) -> x.name = s) p.classes in
                        Seq (Expr (Alloc (Binop (Mul, Cst ( 1 + (List.length (cla_def).fields)), Cst 4))) :: 
                        ()
                        )
    *)
    | New (class_name, args) -> Call (class_name^"_constructor", List.map tr_expr args)
    | NewTab (typ, e) -> Alloc (Binop (Mul, tr_expr e, Cst 4))
    | Read mem -> Deref (tr_mem mem)
    | This -> Var "_this"
    (*
    | This -> (match te.annot with
               | TClass cla -> Deref (List.find (fun (x :'a Objlng.class_def) -> x.name = cla) p.classes)
               | _ -> failwith "error this"
              )
              *)
  and tr_mem = function
    | Arr (e1, e2) -> Imp.array_offset (tr_expr e1) (tr_expr e2)
    | Atr (e1, s) -> let t_e1 = match e1.annot with
                            | TClass name -> print_string "read conversion : "; print_endline name; name
                            | _ -> failwith ""
                     in
                     let index = match (find_index (fun x -> fst x = s) (List.find (fun (x : 'a Objlng.class_def) -> x.name = t_e1) p.classes).fields) with
                       | -1 -> failwith "field not recognized" 
                       | n -> n
                     in 
                     Imp.array_offset (tr_expr e1) (Cst (index+1))
 
  in

  (* translation of instructions *)
  let rec tr_seq s = List.map tr_instr s
  and tr_instr: Objlng.typ Objlng.instruction -> Imp.instruction = function
    | Putchar e     -> Putchar(tr_expr e)
    | Set     (s, e)-> Set (s, tr_expr e)
    | If      (e, seq1, seq2) -> If (tr_expr e, tr_seq seq1, tr_seq seq2)
    | While   (e, seq) -> While (tr_expr e, tr_seq seq)
    | Return  e -> Return (tr_expr e)
    | Expr    e -> Expr (tr_expr e)
    | Write   (mem, e) -> Write (tr_mem mem, tr_expr e)
    (* Faire un tr_mem dans tr_expr*)
  in

  let tr_class_methods cla =
    let tr_method (f : 'a function_def) =
      let code = 
        if f.name = "constructor" then 
          let nb_of_fields = List.length cla.fields in
            Imp.Set("_this", Alloc (Cst (4 * (nb_of_fields + 1)) ))
            :: Write(Var "_this", Addr(cla.name ^ "_" ^ "descr"))
            :: tr_seq f.code @ [Return (Var "_this")]
        else
          tr_seq f.code
      in
      { name = cla.name ^ "_" ^ f.name;
        params = List.map fst f.params;
        locals = List.map fst f.locals;
        code = code; }
    in
    List.map tr_method cla.methods
      (*
    List.fold_left (fun f m -> tr_method m :: f) f cla.methods
    *)
  in
  

  (* translation of function definitions *)
  let methods = List.map (fun (c_def : 'a Objlng.class_def) -> 
                          List.map (fun (f_def: 'a Objlng.function_def) -> 
                                    {f_def with name = c_def.name ^ "_" ^ f_def.name}) c_def.methods) 
                p.classes 
  in
  (*
  let p = {p with functions = p.functions @ List.flatten(methods)} in
  *)


  let tr_fdef (fdef: Objlng.typ Objlng.function_def) =
    { Imp.name = fdef.name; 
      params = List.map fst fdef.params; 
      locals = List.map fst fdef.locals; 
      code = tr_seq fdef.code;
    }
  in

  let functions = List.map tr_fdef p.functions in
  let functions2 = List.flatten (List.map tr_class_methods p.classes) in
  
  let class_descrs = List.map (fun (cla_def :'a Objlng.class_def) -> { descr_name = cla_def.name ^ "_descr";
                                                                     methods = List.map (fun (methods : 'a Objlng.function_def) -> cla_def.name ^ "_" ^ methods.name) cla_def.methods;
                                                                     parent = None
                                                                    }) p.classes
  in
  let () = List.iter (fun x -> print_endline x.descr_name) class_descrs; in

  { Imp.globals = List.map fst p.globals;
    class_descrs = class_descrs;
    functions = functions @ functions2 }

