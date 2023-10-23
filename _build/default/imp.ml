type binop = Add | Mul | Lt

type expression =
  | Cst   of int
  (* Boolean constant: true, false *)
  | Bool  of bool
  (* Variable, identified by a name *)
  | Var   of string
  (* Binary operation, with an operator and two operands *)
  | Binop of binop * expression * expression
  (* Function call, with a function name and a list of parameters *)
  | Call  of string * expression list
  (* Dereference a pointer *)
  | Deref of expression   (*   *e   *)
  (* Allocate some memory *)
  | Alloc of expression

  (* Return adrress of an statically allocated element *)
  | Addr of string
  (* Dynamic call *)
  | DCall of expression * expression list
  (* Constructor *)
  | Seq of sequence

(**
   Data structure for instructions
*)
and instruction =
  (* Primitive operation for printing a char, given as ASCII code *)
  | Putchar of expression
  (* Assignment of a new value to a variable *)
  | Set     of string * expression
  (* Conditional *)
  | If      of expression * sequence * sequence
  (* Loop *)
  | While   of expression * sequence
  (* Function termination *)
  | Return  of expression
  (* Expression used as an instruction (typically function call) *)
  | Expr    of expression
  (* writing in memory *)
  | Write   of expression * expression (*   *e1 = e2;   *)
(* Instruction sequence *)
and sequence = instruction list

let i = While(Binop(Lt, Var "c", Cst 58),
              [ Putchar(Var "c");
                Set("c", Binop(Add, Var "x", Cst 1)) ]
          )
let array_offset t i = Binop(Add, t, Binop(Mul, Cst 4, i))
let array_access t i = Deref(array_offset t i)


let array_write t i e =
  Write(array_offset t i, e)

type function_def = {
    name: string;
    params: string list;
    locals: string list;
    code: sequence;
  }

type class_descr = {
  descr_name:   string;
  methods: string list;
  parent: string option;
}

type program = {
    globals: string list;
    class_descrs : class_descr list;
    functions: function_def list;
  }
