(* Stack manipulation *)
%token NOP SWAP XCHG0 PUSHINT
(* Arithmetics *)
%token ADD
(* Cell *)
%token NEWC STIX ENDC

%token <int> INT
%token EOF

%start <Asm.instr list> code

%{              
   open Asm
%}

%%

let code := l = list(instr); EOF; { l }

let instr :=
    | NOP; { NOP }
    | SWAP; { SWAP }
    | ~= INT; XCHG0; <XCHG0> 
    | ~= INT; PUSHINT; <PUSHINT> 
    | ADD; { ADD }
    | NEWC; { NEWC }
    | STIX; { STIX }
    | ENDC; { ENDC }
