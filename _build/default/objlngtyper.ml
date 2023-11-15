open Objlng

(* types for various environments *)
module Env = Map.Make(String)
type tenv = typ Env.t
type fenv = unit function_def Env.t
type cenv = unit class_def Env.t

(* utility function *)
let check te t =
  if te.annot <> t then raise TypeError;
  te

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

let get_array_type e = match e.annot with
  | TArray t -> t
  | _ -> raise TypeError

let remove_class name =
  String.concat "_" (List.tl (String.split_on_char '_' name))

let rec string_of_typ typ = match typ with
  | TInt -> "integer"
  | TBool -> "boolean"
  | TClass s -> "class of " ^ s
  | TArray t -> "array of " ^ (string_of_typ t)
  | TVoid -> "void"

let string_of_op op = match op with
  | Add -> "addition"
  | Mul -> "multiplication"
  | Lt -> "less than"

let check_args f_name f_param e =
  ignore(
    try check e (snd f_param) with
    | TypeError ->
      let s_t_e = string_of_typ (e.annot) in
      let s_exp_t_e = string_of_typ (snd f_param) in
      failwith ("Call of function " ^ f_name ^ " with the argument : " ^
      (fst f_param) ^ " of type " ^ s_t_e ^ ", expected type : " ^ s_exp_t_e))

(* main typing function *)
let type_program (p: unit program): typ program =

  let rec add_attr_inheritance classes classes_previous =
    let rename_method (cla : 'a class_def) (met : 'a function_def) =
      {met with name = (cla.name ^ "_" ^ met.name)}
    in
    let aux cla classes_list = match cla.parent with
      | None -> { cla with methods = List.map (rename_method cla) cla.methods}
      | Some parent_cla ->

        try
          let parent_def =
            List.find (fun x -> x.name = parent_cla) classes_list
          in
          let parent_fields = parent_def.fields in
          let cla_methods_name =
            List.map (fun (x : 'a function_def) -> x.name) cla.methods
          in
          let inheritance_methods =
            List.filter
            (fun (x : 'a function_def) ->
              not (List.mem (remove_class x.name) cla_methods_name))
            parent_def.methods
          in
          let renamed_cla_methods = List.map (rename_method cla) cla.methods in
          {cla with fields = parent_fields @ cla.fields;
                    methods = inheritance_methods @ renamed_cla_methods}
        with
        | Not_found -> failwith
          ("class " ^ cla.name ^ " defined before it's parent class : " ^
           parent_cla ^ " or this parent class is not defined" )
    in
    match classes with
    | [] -> classes_previous
    | cla :: t -> let new_cla = aux cla classes_previous in
                  add_attr_inheritance t (new_cla :: classes_previous)
  in
  let classes = List.rev (add_attr_inheritance p.classes []) in


  (* initialize global environments *)
  let tenv = add2env p.globals Env.empty in
  let fenv =
    add2env (List.map (fun (f: unit function_def) -> f.name, f) p.functions) Env.empty
  in
  let cenv = add2env (List.map (fun s -> s.name, s) classes) Env.empty in

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
      | Var x ->
        ( let t_e = try Env.find x tenv with
            | Not_found -> try ( let parent_def =
                                    match Env.find "_this" tenv with
                                      | TClass name -> Env.find name cenv
                                      | _ -> raise Not_found
                                  in
                                  snd (List.find
                                       (fun field -> (fst field) = x)
                                       parent_def.fields
                                     )
                                ) with
                            | Not_found  ->
                                failwith (x ^ " is not found in this
                                          environment")
          in
          mk_expr t_e (Var x)
        )
      | Binop (op, e1, e2) ->
          let exp_t_e1, exp_t_e2 = type_op_args op in
          let s_exp_t_e1 = string_of_typ exp_t_e1 in
          let s_exp_t_e2 = string_of_typ exp_t_e2 in
          let s_op = string_of_op op in
          let t_e1 = type_expr e1 in
          let t_e2 = type_expr e2 in
          let _ = try check (t_e1) TInt with
              | TypeError -> failwith ("left part of " ^ s_op ^
                " incompatible with type " ^ (string_of_typ t_e1.annot ) ^
                " need type " ^ s_exp_t_e1)
          in
          let _ = try check (t_e2) TInt with
              | TypeError -> failwith ("right part of " ^ s_op ^
                " incompatible with type " ^ (string_of_typ t_e2.annot ) ^
                " need type " ^ s_exp_t_e2)
          in
          mk_expr (type_op op) (Binop(op, t_e1, t_e2))
      | Call (f, e) ->
          let args = List.map type_expr e in
          List.iter2 (check_args f) (Env.find f fenv).params args;
          mk_expr (Env.find f fenv).return (Call (f, args))
      | MCall (e, method_name, args) ->
          let rec aux cla_def =
            (try List.find (fun (x : unit function_def) ->
              remove_class x.name = method_name)
              cla_def.methods
             with
               | Not_found ->
                   let name_parent =
                     try Option.get cla_def.parent with
                     | Invalid_argument _ -> raise TypeError
                     in
                     let parent_def = Env.find name_parent cenv in
                     aux parent_def
            )
          in
          let cla_name = try get_name (type_expr e) with
                         | TypeError ->
                             let s_t_e = string_of_typ (type_expr e).annot in
                             failwith ("Call a method but not on a class, " ^
                               s_t_e ^ " found")
          in
          let cla_def = Env.find cla_name cenv in
          let method_def =
            try aux cla_def with
            | TypeError -> failwith ("Method " ^ method_name ^ " not found in " ^
              cla_name ^ " or in its parents")
          in
          let t_args = List.map type_expr args in
          List.iter2 (check_args method_def.name) method_def.params t_args;
          mk_expr method_def.return (MCall (type_expr e, method_name, t_args))
      | New (s, args) -> mk_expr (TClass s) (New (s, List.map type_expr args)) (* TODO *)
      | NewTab (typ, e) ->
          let t_e = try check (type_expr e) TInt with
                     | TypeError ->
                        let s_t_e = string_of_typ (type_expr e).annot in
                         failwith ("To initialize a array need a size, " ^
                         s_t_e  ^ " found" )
          in
          mk_expr (TArray typ) (NewTab (typ, t_e))
      | Read(Arr (e1, e2)) ->
          let t_e1 = try get_array_type(type_expr e1) with
                       | TypeError ->
                         let s_t_e1 = string_of_typ (type_expr e1).annot in
                           failwith ("Need array type but " ^ s_t_e1 ^ " found")
          in
          let _ = try check (type_expr e2) TInt with
                  | TypeError ->
                      let s_t_e2 = string_of_typ (type_expr e2).annot in
                      failwith ("Array index need to be integer, " ^ s_t_e2 ^
                      " found")
          in
          mk_expr (t_e1) (Read(Arr(type_expr e1, type_expr e2)))
      | Read(Atr (e1, field)) ->
          let cla_name = try get_name (type_expr e1) with
                         | TypeError ->
                             let s_t_e1 = string_of_typ (type_expr e1).annot in
                             failwith ("Access a field but not on a class, " ^
                               s_t_e1 ^ " found")
          in
          (try
          mk_expr (snd (List.find (fun x -> fst x = field) (Env.find cla_name cenv).fields))
                  (Read(Atr(type_expr e1, field)))
          with
          | Not_found -> failwith ("Fields " ^ field ^ " not found in class " ^
                         cla_name ^ " or in parents")
          )
      | This -> (try mk_expr (Env.find "_this" tenv) This with
                  | Not_found -> failwith "this not in class"
                )
      | Super -> let cla_this = try (
                    match Env.find "_this" tenv with
                      | TClass s -> s
                      | _ -> assert false
                    ) with
                    | Not_found -> failwith "super not in class"
                 in
                 let cla_def_this =
                   List.find (fun x -> x.name = cla_this) p.classes
                 in
                 let cla_super = try
                   List.find (fun x -> x.name = Option.get(cla_def_this.parent))
                   p.classes with
                   | Invalid_argument _ ->
                       failwith ("super but " ^ cla_this ^ " have no parent")
                 in
                 mk_expr (TClass (cla_super.name)) Super
    in

    (* type instructions *)
    let rec type_seq s = List.map type_instr s
    and type_instr = function
      | Putchar e     ->
          let _ = try check (type_expr e) TInt with
                  | TypeError ->
                      let s_t_e = string_of_typ (type_expr e).annot in
                      failwith ("Putchar need a int but " ^ s_t_e ^ "found")
          in
          Putchar (type_expr e)
      | Set     (s, e)->
          if Env.mem s tenv then
            Set (s, type_expr e)
          else
            failwith ( s ^ " is not declared in the current environment")
      | If      (e, seq1, seq2) ->
          let _ = try check (type_expr e) TBool with
                  | TypeError ->
                      let s_t_e = string_of_typ (type_expr e).annot in
                        failwith ("if condition need a boolean, " ^ s_t_e ^
                        " found")
          in
          If (type_expr e, type_seq seq1, type_seq seq2)
      | While   (e, seq) ->
          let _ = try check (type_expr e) TBool with
                  | TypeError ->
                      let s_t_e = string_of_typ (type_expr e).annot in
                        failwith ("While condition need a boolean, " ^ s_t_e ^
                        " found")
          in
          While (check (type_expr e) TBool, type_seq seq)
      | Return  e -> Return (type_expr e) (* TODO check return value, possible ? *)
      | Expr    e -> Expr (type_expr e)
      | Write (Arr (e1, e2), e) ->
          let t_e1 = try get_array_type (type_expr e1) with
                     | TypeError ->
                        let s_t_e1 = string_of_typ (type_expr e1).annot in
                        failwith ("Write a array but not on array, " ^
                        s_t_e1 ^ " found")
          in
          if t_e1 <> (type_expr e).annot then
            (let s_t_e = string_of_typ (type_expr e).annot in
            let s_t_e1 = string_of_typ t_e1 in
            failwith ("Add a value of type " ^ s_t_e ^ " in a array of type " ^
            s_t_e1));
          let _ = try check (type_expr e2) TInt with
                  | TypeError ->
                      let s_t_e2 = string_of_typ (type_expr e2).annot in
                      failwith ("array index need to be integer, " ^ s_t_e2 ^
                        " found")
          in
          Write (Arr(type_expr e1, check (type_expr e2) TInt), type_expr e)
      | Write (Atr (e1, field), e) ->
          let cla_name = try get_name (type_expr e1) with
                         | TypeError ->
                            let s_t_e1 = string_of_typ (type_expr e1).annot in
                            failwith ("Write class field but not on a class, " ^
                            s_t_e1 ^ " found")
          in
          let exp_t = try (snd
            (List.find
              (fun x -> fst x = field)
              (Env.find cla_name cenv).fields))
            with
            | Not_found -> failwith ("Fields " ^ field ^ " not found in class " ^
                           cla_name ^ " or in parents")
          in
          if exp_t <> (type_expr e).annot
          then
            let s_t_e = string_of_typ (type_expr e).annot in
            let s_exp_t = string_of_typ exp_t in
            failwith ("assigment of type " ^ s_t_e ^ " but expected " ^ s_exp_t
            ^ " for the field " ^ field ^ " in " ^ cla_name)
          else Write (Atr (type_expr e1, field), type_expr e)
      | Seq seq -> Seq (type_seq seq)
    in
    { fdef with code = type_seq fdef.code }
  in
  let add_this (cla:string) (f:'a function_def) = {f with params = ("_this", TClass cla) :: f.params}in
  let classes = List.map
    (fun cla -> {cla with methods = List.map type_fdef (List.map (add_this cla.name) cla.methods)})
    classes
  in
  { p with functions = List.map type_fdef p.functions ; classes = classes }
