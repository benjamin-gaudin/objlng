
open Imp
open Mips

   
let push reg =
  subi sp sp 4
  @@ sw reg 0 sp
let pop  reg =
  lw reg 0 sp
  @@ addi sp sp 4

let tr_function fdef =
  let env = Hashtbl.create 16 in
  List.iteri (fun k id -> Hashtbl.add env id (4*(k+1))) fdef.params;
  List.iteri (fun k id -> Hashtbl.add env id (-4*(k+2))) fdef.locals;
  
  let new_label =
    let cpt = ref (-1) in
    fun () -> incr cpt; Printf.sprintf "__%s_%i" fdef.name !cpt
  in

  let rec tr_expr_stack = function
    | Cst(n)  -> li t6 n
    | Bool(b) -> if b then li t6 1 else li t6 0
    | Var(id) -> begin
        match Hashtbl.find_opt env id with
        | Some offset -> lw t6 offset fp
        | None -> la t6 id @@ lw t6 0 t6
      end
             
    | Binop(bop, e1, e2) ->
       let op = match bop with
         | Add -> add
         | Mul -> mul
         | Lt  -> slt
       in
       tr_expr_stack e2
       @@ push t6
       @@ tr_expr_stack e1
       @@ pop t7
       @@ op t6 t6 t7      
    | Call(f, params) ->
       let params_code =
         List.fold_right
           (fun e code -> code @@ tr_expr_stack e @@ push t6)
           params nop
       in
       params_code
       @@ jal f
       @@ addi sp sp (4 * List.length params)
    | Deref e ->
       tr_expr 0 e 
       @@ lw t0 0(t0)
    | Alloc e ->
       tr_expr_stack e
       @@ move a0 t0
       @@ li v0 9
       @@ syscall 
       @@ move t0 v0 
    | Addr s ->
       la t0 s
    | DCall (e1, params) ->
       let params_code =
         List.fold_right
           (fun e code -> code @@ tr_expr_stack e @@ push t0)
           params nop
       in
       params_code
       @@ tr_expr_stack e1
       @@ jalr t0
       @@ addi sp sp (4 * List.length params)

  and tr_expr i e = 
   let regs = [|t0; t1; t2; t3; t4; t5; t6; t7|] in
   if i > 5 then 
      tr_expr_stack e
      @@ move regs.(i) t6
   else
   match e with
    | Cst(n)  -> li regs.(i) n
    | Bool(b) -> if b then li regs.(i) 1 else li regs.(i) 0
    | Var(id) -> begin
        match Hashtbl.find_opt env id with
        | Some offset -> lw regs.(i) offset fp
        | None -> la regs.(i) id @@ lw regs.(i) 0 regs.(i)
      end
    | Binop(bop, e1, e2) ->
      if i > 4 then 
         tr_expr_stack e
         @@ move regs.(i) t6
      else
       let op = match bop with
         | Add -> add
         | Mul -> mul
         | Lt  -> slt
       in
       tr_expr i e2 
       @@ tr_expr (i+1) e1
       @@ op regs.(i) regs.(i+1) regs.(i)      

    | Call(f, params) ->
       let params_code =
         List.fold_right
           (fun e code -> code @@ tr_expr_stack e @@ push t6)
           params nop
       in
       params_code
       @@ jal f
       @@ addi sp sp (4 * List.length params)
    | Deref e ->
       tr_expr i e 
       @@ lw regs.(i) 0(regs.(i))
    | Alloc e ->
       tr_expr i e
       @@ move a0 regs.(i)
       @@ li v0 9
       @@ syscall 
       @@ move regs.(i) v0 
    | Addr s ->
       la regs.(i) s
    | DCall (e1, params) ->
       let params_code =
         List.fold_right
           (fun e code -> code @@ tr_expr_stack e @@ push t6)
           params nop
       in
       params_code
       @@ tr_expr i e1
       @@ jalr regs.(i)
       @@ addi sp sp (4 * List.length params)

  and tr_seq = function
    | []   -> nop
    | [i]  -> tr_instr i
    | i::s -> tr_instr i @@ tr_seq s

  and tr_instr = function
    | Putchar(e) ->
       tr_expr 0 e
       @@ move a0 t0
       @@ li v0 11
       @@ syscall
    | Set(id, e) ->
       let set_code = match Hashtbl.find_opt env id with
         | Some offset -> sw t0 offset fp
         | None -> la t1 id @@ sw t0 0 t1
       in
       tr_expr 0 e @@ set_code
    | If(c, s1, s2) ->
       let then_label = new_label()
       and end_label = new_label()
       in
       tr_expr 0 c
       @@ bnez t0 then_label
       @@ tr_seq s2
       @@ b end_label
       @@ label then_label
       @@ tr_seq s1
       @@ label end_label
    | While(c, s) ->
       let test_label = new_label()
       and code_label = new_label()
       in
       b test_label
       @@ label code_label
       @@ tr_seq s
       @@ label test_label
       @@ tr_expr 0 c
       @@ bnez t0 code_label
    | Return(e) ->
       tr_expr 0 e
       @@ addi sp fp (-4)
       @@ pop ra
       @@ pop fp
       @@ jr ra
    | Expr(e) ->
       tr_expr 0 e
    | Write(e1, e2) ->
       tr_expr 0 e1  (* t0: pointer *)
       @@ push t0
       @@ tr_expr 0 e2  (* t0: value to be written *)
       @@ pop t1
       @@ sw t0 0(t1)
    | Seq s -> match s with
       | [] -> nop
       | h::t -> tr_instr h @@ tr_instr (Seq t)

  in

  push fp
  @@ push ra
  @@ addi fp sp 4
  @@ addi sp sp (-4 * List.length fdef.locals)
  @@ tr_seq fdef.code
  @@ li t0 0
  @@ addi sp fp (-4)
  @@ pop ra
  @@ pop fp
  @@ jr ra

let translate_program prog =
  let init =
    beqz a0 "init_end"
    @@ lw a0 0 a1
    @@ jal "atoi"
    @@ label "init_end"
    @@ push v0
    @@ jal "main"
    @@ li v0 10
    @@ syscall
  and built_ins =
    comment "built-in atoi"
    @@ label "atoi"
    @@ li   v0 0
    @@ label "atoi_loop"
    @@ lbu  t0 0 a0
    @@ beqz t0 "atoi_end"
    @@ addi t0 t0 (-48)
    @@ bltz t0 "atoi_error"
    @@ bgei t0 10 "atoi_error"
    @@ muli v0 v0 10
    @@ add  v0 v0 t0
    @@ addi a0 a0 1
    @@ b "atoi_loop"
    @@ label "atoi_error"
    @@ li   v0 10
    @@ syscall
    @@ label "atoi_end"
    @@ jr   ra
  in

  let function_codes = List.fold_right
    (fun fdef code ->
      label fdef.name @@ tr_function fdef @@ code)
    prog.functions nop
  in
  let text = init @@ function_codes @@ built_ins
  and data = List.fold_right
    (fun id code -> label id @@ dword [0] @@ code)
    prog.globals nop
  in
  let data2 = List.fold_right
    (fun id code -> label id.descr_name @@
                    (if Option.is_none id.parent then dword[0] else dword[0]) @@ (*TODO Inheritance*)
                    List.fold_right (fun x acc -> string_word x @@ acc) id.methods nop @@
                    code)
    prog.class_descrs nop
  in
  
  { text; data = data @@ data2 }
