(* Stack manipulation *)
%token NOP SWAP XCHG0
(* Arithmetics *)
%token ADD

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
    | ADD; { ADD }
