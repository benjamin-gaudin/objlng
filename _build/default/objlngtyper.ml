open Objlng

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = unit function_def Env.t
type cenv = unit class_def Env.t

(* utility function *)
let check te t =
  if te.annot <> t then failwith "type error (check fuction)";
  te

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* main typing function *)
let type_program (p: unit program): typ program =

  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv = add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty in
  let cenv = add2env (List.map (fun s -> s.name, s) p.classes) Env.empty in

  (* typing a function definition *)
  let type_fdef fdef =
    (* add local elements to the environments *)
    let tenv = add2env (fdef.params @ fdef.locals) tenv in

    (* note: nested definitions ensure that all environments are known to the
       inner functions, without making them explicit arguments *)

    (* type expressions *)
    let rec type_expr (e: unit expression): typ expression = match e.expr with
      | Cst n -> mk_expr TInt (Cst n)
      | Bool b -> mk_expr TBool (Bool b)
      | Var x -> mk_expr (Env.find x tenv) (Var x)
      | Binop (Add, e1, e2) -> mk_expr TInt (Binop (Add, check (type_expr e1) TInt, check (type_expr e2) TInt))
      | Binop (Mul, e1, e2) -> mk_expr TInt (Binop (Mul, check (type_expr e1) TInt, check (type_expr e2) TInt))
      | Binop (Lt, e1, e2) -> mk_expr TBool (Binop (Lt, check (type_expr e1) TInt, check (type_expr e2) TInt))
      | Call (f, e) -> let args =  List.map type_expr e in
                       List.iter2 (fun x y -> ignore(check y (snd x)) ) (Env.find f fenv).params args; 
                       mk_expr (Env.find f fenv).return (Call (f, args))
      | MCall (e, method_name, args) ->
                                       (match (type_expr e).annot with
                                        | TClass cla -> let cla_def = Env.find cla cenv in
                                                        let method_def = List.find (fun (x :unit function_def) -> x.name = method_name) (cla_def.methods) in
                                                        let t_args = List.map type_expr args in
                                                        List.iter2 (fun x y -> ignore(check y (snd x)) ) method_def.params t_args; 
                                                        mk_expr method_def.return (MCall (type_expr e, method_name, t_args))
                                        | _ -> failwith "method call not on function"
                                       )
      | New (s, constructor) -> mk_expr (TClass s) (New (s, List.map type_expr constructor))
      | NewTab (typ, e) -> mk_expr (TArray typ) (NewTab (typ, type_expr e))
      | Read(Arr (e1, e2)) -> let t_e1 = match (type_expr e1).annot with
                                         | TArray t -> t
                                         | _ -> failwith "type error Arr"
                              in
                              mk_expr (t_e1) (Read(Arr(type_expr e1, check (type_expr e2) TInt)))
      | Read(Atr (e1, field)) ->  let name = (match ((type_expr e1).annot) with
                                                | TClass s -> s
                                                | _ -> failwith "type error Atr")
                                  in
                                  mk_expr (snd (List.find (fun x -> fst x = field) (Env.find name cenv).fields))
                                          (Read(Atr(type_expr e1, field)))
      (* TODO *)
      | This -> mk_expr (Env.find "_this" tenv) This 
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     -> Putchar (check (type_expr e) TInt)
      | Set     (s, e)-> Set (s, type_expr e) 
      | If      (e, seq1, seq2) -> If (check (type_expr e) TBool, type_seq seq1, type_seq seq2)
      | While   (e, seq) -> While (check (type_expr e) TBool, type_seq seq)
      | Return  e -> Return (type_expr e)
      | Expr    e -> Expr (type_expr e)
      | Write (Arr (e1, e2), e) -> (match (type_expr e1).annot with
                                    | TArray t_e1 when t_e1 = (type_expr e).annot ->
                                        Write (Arr(type_expr e1, check (type_expr e2) TInt), type_expr e)
                                    | _ -> failwith "type error instr Arr"
                                   )
      | Write (Atr (e1, field), e) -> (match (type_expr e1).annot with 
                                        | TClass cla when (snd (List.find (fun x -> fst x = field) (Env.find cla cenv).fields)) = (type_expr e).annot -> 
                                           Write (Atr (type_expr e1, field), type_expr e)
                                        | _ -> failwith "type error intr Atr"
                                      )
    in
    { fdef with code = type_seq fdef.code }
  in
  let add_this (cla:string) (f:'a function_def) = if f.name = "constructor" then
                                                    {f with locals = ("_this", TClass cla) :: f.locals}  
                                                  else
                                                    {f with params = ("_this", TClass cla) :: f.params}

  in
  let classes = List.map (fun cla -> {cla with methods = List.map type_fdef (List.map (add_this cla.name) cla.methods)}) p.classes in
  { p with functions = List.map type_fdef p.functions ; classes = classes }
