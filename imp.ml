type binop = Add | Mul | Lt

type expression =
  | Cst   of int
  | Bool  of bool
  | Var   of string
  | Binop of binop * expression * expression
  | Call  of string * expression list
  | Deref of expression   (*   *e   *)
  | Alloc of expression

  | Addr of string
  | DCall of expression * expression list
  | Seq of sequence

and instruction =
  | Putchar of expression
  | Set     of string * expression
  | If      of expression * sequence * sequence
  | While   of expression * sequence
  | Return  of expression
  | Expr    of expression
  | Write   of expression * expression (*   *e1 = e2;   *)
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
