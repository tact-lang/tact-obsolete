module L = MenhirLib.LexerUtil

let pp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

let parse ?(filename = "noname.tlb") text =
  let lexbuf = L.init filename @@ Lexing.from_string text in
  Parser.root Lexer.token lexbuf

let%expect_test "block.tlb" =
  pp @@ Syntax.sexp_of_root @@ parse ~filename:"block.tlb" Block.tlb ;
  [%expect
    {|
    ((decls
      (((combinator_exotic false) (combinator_constructor (Constructor unit))
        (combinator ((combinator_ident Unit))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false) (combinator_constructor (Constructor true))
        (combinator ((combinator_ident True))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bool_false (0 0 1))))
        (combinator ((combinator_ident Bool))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bool_true (1 0 1))))
        (combinator ((combinator_ident Bool))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bool_false (0 0 1))))
        (combinator ((combinator_ident BoolFalse))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bool_true (1 0 1))))
        (combinator ((combinator_ident BoolTrue))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (nothing (0 0 1))))
        (combinator ((combinator_ident Maybe)))
        (combinator_fields ((ImplicitField (field_ident X) (field_type Type))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (just (1 0 1))))
        (combinator ((combinator_ident Maybe)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident value) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (left (0 0 1))))
        (combinator ((combinator_ident Either)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident value) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((NamedRef X) (NamedRef Y))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (right (1 0 1))))
        (combinator ((combinator_ident Either)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident value) (field_expr ((NamedRef Y) () ())))))
        (combinator_params ((NamedRef X) (NamedRef Y))))
       ((combinator_exotic false) (combinator_constructor (Constructor pair))
        (combinator ((combinator_ident Both)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident first) (field_expr ((NamedRef X) () ())))
          (NamedField (field_ident second) (field_expr ((NamedRef Y) () ())))))
        (combinator_params ((NamedRef X) (NamedRef Y))))
       ((combinator_exotic false) (combinator_constructor (Constructor bit))
        (combinator ((combinator_ident Bit)))
        (combinator_fields
         ((ExprField
           (Exprs (((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ()))))))
        (combinator_params ()))
       ((combinator_exotic false) (combinator_constructor (Constructor hm_edge))
        (combinator ((combinator_ident Hashmap)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident l) (field_type (Uint 32)))
          (ImplicitField (field_ident m) (field_type (Uint 32)))
          (NamedField (field_ident label)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HmLabel) () ()) ((Runtime (NamedRef l)) () ())
                ((NamedRef n) () ()))))
             () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef n) () ())))
             (Plus
              ((Exprs (((Expr (Exprs (((Runtime (NamedRef m)) () ())))) () ())))
               (Exprs (((NamedRef l) () ()))))))))
          (NamedField (field_ident node)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapNode) () ()) ((NamedRef m) () ())
                ((NamedRef X) () ()))))
             () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false) (combinator_constructor (Constructor hmn_leaf))
        (combinator ((combinator_ident HashmapNode)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident value) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((Int 0) (NamedRef X))))
       ((combinator_exotic false) (combinator_constructor (Constructor hmn_fork))
        (combinator ((combinator_ident HashmapNode)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident left)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef Hashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))
          (NamedField (field_ident right)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef Hashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (hml_short (0 0 1))))
        (combinator ((combinator_ident HmLabel)))
        (combinator_fields
         ((ImplicitField (field_ident m) (field_type (Uint 32)))
          (ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident len)
           (field_expr
            ((Expr
              (Exprs (((NamedRef Unary) () ()) ((Runtime (NamedRef n)) () ()))))
             () ())))
          (ExprField
           (Leq ((Exprs (((NamedRef n) () ()))) (Exprs (((NamedRef m) () ()))))))
          (NamedField (field_ident s)
           (field_expr
            ((Expr
              (Times
               ((Exprs (((NamedRef n) () ()))) (Exprs (((NamedRef Bit) () ()))))))
             () ())))))
        (combinator_params ((Runtime (NamedRef n)) (NamedRef m))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (hml_long (01 0 2))))
        (combinator ((combinator_ident HmLabel)))
        (combinator_fields
         ((ImplicitField (field_ident m) (field_type (Uint 32)))
          (NamedField (field_ident n)
           (field_expr
            ((Expr (Exprs ((NatLeq () ()) ((NamedRef m) () ())))) () ())))
          (NamedField (field_ident s)
           (field_expr
            ((Expr
              (Times
               ((Exprs (((NamedRef n) () ()))) (Exprs (((NamedRef Bit) () ()))))))
             () ())))))
        (combinator_params ((Runtime (NamedRef n)) (NamedRef m))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (hml_same (11 0 2))))
        (combinator ((combinator_ident HmLabel)))
        (combinator_fields
         ((ImplicitField (field_ident m) (field_type (Uint 32)))
          (NamedField (field_ident v) (field_expr ((NamedRef Bit) () ())))
          (NamedField (field_ident n)
           (field_expr
            ((Expr (Exprs ((NatLeq () ()) ((NamedRef m) () ())))) () ())))))
        (combinator_params ((Runtime (NamedRef n)) (NamedRef m))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (unary_zero (0 0 1))))
        (combinator ((combinator_ident Unary))) (combinator_fields ())
        (combinator_params ((Runtime (Int 0)))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (unary_succ (1 0 1))))
        (combinator ((combinator_ident Unary)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident x)
           (field_expr
            ((Expr
              (Exprs (((NamedRef Unary) () ()) ((Runtime (NamedRef n)) () ()))))
             () ())))))
        (combinator_params
         ((Runtime
           (Expr
            (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ()))))))))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (hme_empty (0 0 1))))
        (combinator ((combinator_ident HashmapE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (hme_root (1 0 1))))
        (combinator ((combinator_ident HashmapE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident root)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef Hashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident BitstringSet)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (AnonymousField
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Hashmap) () ()) ((NamedRef n) () ())
                ((NamedRef True) () ()))))
             () ())))))
        (combinator_params ((NamedRef n))))
       ((combinator_exotic false) (combinator_constructor (Constructor ahm_edge))
        (combinator ((combinator_ident HashmapAug)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (ImplicitField (field_ident l) (field_type (Uint 32)))
          (ImplicitField (field_ident m) (field_type (Uint 32)))
          (NamedField (field_ident label)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HmLabel) () ()) ((Runtime (NamedRef l)) () ())
                ((NamedRef n) () ()))))
             () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef n) () ())))
             (Plus
              ((Exprs (((Expr (Exprs (((Runtime (NamedRef m)) () ())))) () ())))
               (Exprs (((NamedRef l) () ()))))))))
          (NamedField (field_ident node)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapAugNode) () ()) ((NamedRef m) () ())
                ((NamedRef X) () ()) ((NamedRef Y) () ()))))
             () ())))))
        (combinator_params ((NamedRef n) (NamedRef X) (NamedRef Y))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor ahmn_leaf))
        (combinator ((combinator_ident HashmapAugNode)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident extra) (field_expr ((NamedRef Y) () ())))
          (NamedField (field_ident value) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((Int 0) (NamedRef X) (NamedRef Y))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor ahmn_fork))
        (combinator ((combinator_ident HashmapAugNode)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident left)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef HashmapAug) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ()) ((NamedRef Y) () ())))))
             () ())))
          (NamedField (field_ident right)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef HashmapAug) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ()) ((NamedRef Y) () ())))))
             () ())))
          (NamedField (field_ident extra) (field_expr ((NamedRef Y) () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedRef X) (NamedRef Y))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (ahme_empty (0 0 1))))
        (combinator ((combinator_ident HashmapAugE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident extra) (field_expr ((NamedRef Y) () ())))))
        (combinator_params ((NamedRef n) (NamedRef X) (NamedRef Y))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (ahme_root (1 0 1))))
        (combinator ((combinator_ident HashmapAugE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident root)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef HashmapAug) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ()) ((NamedRef Y) () ())))))
             () ())))
          (NamedField (field_ident extra) (field_expr ((NamedRef Y) () ())))))
        (combinator_params ((NamedRef n) (NamedRef X) (NamedRef Y))))
       ((combinator_exotic false) (combinator_constructor (Constructor vhm_edge))
        (combinator ((combinator_ident VarHashmap)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident l) (field_type (Uint 32)))
          (ImplicitField (field_ident m) (field_type (Uint 32)))
          (NamedField (field_ident label)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HmLabel) () ()) ((Runtime (NamedRef l)) () ())
                ((NamedRef n) () ()))))
             () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef n) () ())))
             (Plus
              ((Exprs (((Expr (Exprs (((Runtime (NamedRef m)) () ())))) () ())))
               (Exprs (((NamedRef l) () ()))))))))
          (NamedField (field_ident node)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef VarHashmapNode) () ()) ((NamedRef m) () ())
                ((NamedRef X) () ()))))
             () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vhmn_leaf (00 0 2))))
        (combinator ((combinator_ident VarHashmapNode)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident value) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vhmn_fork (10 0 2))))
        (combinator ((combinator_ident VarHashmapNode)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident left)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef VarHashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))
          (NamedField (field_ident right)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef VarHashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))
          (NamedField (field_ident value)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef X) () ())))) ()
             ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vhmn_cont (1 0 1))))
        (combinator ((combinator_ident VarHashmapNode)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident branch) (field_expr ((NamedRef Bit) () ())))
          (NamedField (field_ident child)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef VarHashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))
          (NamedField (field_ident value) (field_expr ((NamedRef X) () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vhme_empty (0 0 1))))
        (combinator ((combinator_ident VarHashmapE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vhme_root (1 0 1))))
        (combinator ((combinator_ident VarHashmapE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident root)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef VarHashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false) (combinator_constructor (Constructor phm_edge))
        (combinator ((combinator_ident PfxHashmap)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident l) (field_type (Uint 32)))
          (ImplicitField (field_ident m) (field_type (Uint 32)))
          (NamedField (field_ident label)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HmLabel) () ()) ((Runtime (NamedRef l)) () ())
                ((NamedRef n) () ()))))
             () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef n) () ())))
             (Plus
              ((Exprs (((Expr (Exprs (((Runtime (NamedRef m)) () ())))) () ())))
               (Exprs (((NamedRef l) () ()))))))))
          (NamedField (field_ident node)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef PfxHashmapNode) () ()) ((NamedRef m) () ())
                ((NamedRef X) () ()))))
             () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (phmn_leaf (0 0 1))))
        (combinator ((combinator_ident PfxHashmapNode)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident value) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (phmn_fork (1 0 1))))
        (combinator ((combinator_ident PfxHashmapNode)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident left)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef PfxHashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))
          (NamedField (field_ident right)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef PfxHashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (phme_empty (0 0 1))))
        (combinator ((combinator_ident PfxHashmapE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (phme_root (1 0 1))))
        (combinator ((combinator_ident PfxHashmapE)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident root)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef PfxHashmap) () ()) ((NamedRef n) () ())
                 ((NamedRef X) () ())))))
             () ())))))
        (combinator_params ((NamedRef n) (NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (addr_none (00 0 2))))
        (combinator ((combinator_ident MsgAddressExt))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (addr_extern (10 0 2))))
        (combinator ((combinator_ident MsgAddressExt)))
        (combinator_fields
         ((NamedField (field_ident len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 9) () ())))) () ())))
          (NamedField (field_ident external_address)
           (field_expr
            ((Expr (Exprs (((NamedRef bits) () ()) ((NamedRef len) () ())))) ()
             ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor anycast_info))
        (combinator ((combinator_ident Anycast)))
        (combinator_fields
         ((NamedField (field_ident depth)
           (field_expr ((Expr (Exprs ((NatLeq () ()) ((Int 30) () ())))) () ())))
          (ExprField
           (Geq ((Exprs (((NamedRef depth) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident rewrite_pfx)
           (field_expr
            ((Expr (Exprs (((NamedRef bits) () ()) ((NamedRef depth) () ())))) ()
             ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (addr_std (01 0 2))))
        (combinator ((combinator_ident MsgAddressInt)))
        (combinator_fields
         ((NamedField (field_ident anycast)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef Anycast) () ()))))
             () ())))
          (NamedField (field_ident workchain_id)
           (field_expr ((NamedRef int8) () ())))
          (NamedField (field_ident address)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (addr_var (11 0 2))))
        (combinator ((combinator_ident MsgAddressInt)))
        (combinator_fields
         ((NamedField (field_ident anycast)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef Anycast) () ()))))
             () ())))
          (NamedField (field_ident addr_len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 9) () ())))) () ())))
          (NamedField (field_ident workchain_id)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident address)
           (field_expr
            ((Expr (Exprs (((NamedRef bits) () ()) ((NamedRef addr_len) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident MsgAddress)))
        (combinator_fields
         ((AnonymousField (field_expr ((NamedRef MsgAddressInt) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident MsgAddress)))
        (combinator_fields
         ((AnonymousField (field_expr ((NamedRef MsgAddressExt) () ())))))
        (combinator_params ()))
       ((combinator_exotic false) (combinator_constructor (Constructor var_uint))
        (combinator ((combinator_ident VarUInteger)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident len)
           (field_expr
            ((Expr (Exprs ((NatLess () ()) ((NamedRef n) () ())))) () ())))
          (NamedField (field_ident value)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef uint) () ())
                ((Expr
                  (Times
                   ((Exprs (((NamedRef len) () ()))) (Exprs (((Int 8) () ()))))))
                 () ()))))
             () ())))))
        (combinator_params ((NamedRef n))))
       ((combinator_exotic false) (combinator_constructor (Constructor var_int))
        (combinator ((combinator_ident VarInteger)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident len)
           (field_expr
            ((Expr (Exprs ((NatLess () ()) ((NamedRef n) () ())))) () ())))
          (NamedField (field_ident value)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef int) () ())
                ((Expr
                  (Times
                   ((Exprs (((NamedRef len) () ()))) (Exprs (((Int 8) () ()))))))
                 () ()))))
             () ())))))
        (combinator_params ((NamedRef n))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor nanograms))
        (combinator ((combinator_ident Grams)))
        (combinator_fields
         ((NamedField (field_ident amount)
           (field_expr
            ((Expr (Exprs (((NamedRef VarUInteger) () ()) ((Int 16) () ())))) ()
             ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor extra_currencies))
        (combinator ((combinator_ident ExtraCurrencyCollection)))
        (combinator_fields
         ((NamedField (field_ident dict)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 32) () ())
                ((Expr (Exprs (((NamedRef VarUInteger) () ()) ((Int 32) () ()))))
                 () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor currencies))
        (combinator ((combinator_ident CurrencyCollection)))
        (combinator_fields
         ((NamedField (field_ident grams) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident other)
           (field_expr ((NamedRef ExtraCurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (int_msg_info (0 0 1))))
        (combinator ((combinator_ident CommonMsgInfo)))
        (combinator_fields
         ((NamedField (field_ident ihr_disabled)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident bounce) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident bounced) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident src)
           (field_expr ((NamedRef MsgAddressInt) () ())))
          (NamedField (field_ident dest)
           (field_expr ((NamedRef MsgAddressInt) () ())))
          (NamedField (field_ident value)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (NamedField (field_ident ihr_fee)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident fwd_fee)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident created_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident created_at)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (ext_in_msg_info (01 0 2))))
        (combinator ((combinator_ident CommonMsgInfo)))
        (combinator_fields
         ((NamedField (field_ident src)
           (field_expr ((NamedRef MsgAddressExt) () ())))
          (NamedField (field_ident dest)
           (field_expr ((NamedRef MsgAddressInt) () ())))
          (NamedField (field_ident import_fee)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (ext_out_msg_info (11 0 2))))
        (combinator ((combinator_ident CommonMsgInfo)))
        (combinator_fields
         ((NamedField (field_ident src)
           (field_expr ((NamedRef MsgAddressInt) () ())))
          (NamedField (field_ident dest)
           (field_expr ((NamedRef MsgAddressExt) () ())))
          (NamedField (field_ident created_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident created_at)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (int_msg_info (0 0 1))))
        (combinator ((combinator_ident CommonMsgInfoRelaxed)))
        (combinator_fields
         ((NamedField (field_ident ihr_disabled)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident bounce) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident bounced) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident src)
           (field_expr ((NamedRef MsgAddress) () ())))
          (NamedField (field_ident dest)
           (field_expr ((NamedRef MsgAddressInt) () ())))
          (NamedField (field_ident value)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (NamedField (field_ident ihr_fee)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident fwd_fee)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident created_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident created_at)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (ext_out_msg_info (11 0 2))))
        (combinator ((combinator_ident CommonMsgInfoRelaxed)))
        (combinator_fields
         ((NamedField (field_ident src)
           (field_expr ((NamedRef MsgAddress) () ())))
          (NamedField (field_ident dest)
           (field_expr ((NamedRef MsgAddressExt) () ())))
          (NamedField (field_ident created_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident created_at)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor tick_tock))
        (combinator ((combinator_ident TickTock)))
        (combinator_fields
         ((NamedField (field_ident tick) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident tock) (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident StateInit)))
        (combinator_fields
         ((NamedField (field_ident split_depth)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((Expr (Exprs ((UIntN () ()) ((Int 5) () ())))) () ()))))
             () ())))
          (NamedField (field_ident special)
           (field_expr
            ((Expr
              (Exprs (((NamedRef Maybe) () ()) ((NamedRef TickTock) () ()))))
             () ())))
          (NamedField (field_ident code)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((CellRef (NamedRef Cell)) () ()))))
             () ())))
          (NamedField (field_ident data)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((CellRef (NamedRef Cell)) () ()))))
             () ())))
          (NamedField (field_ident library)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 256) () ())
                ((NamedRef SimpleLib) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor simple_lib))
        (combinator ((combinator_ident SimpleLib)))
        (combinator_fields
         ((NamedField (field_ident public) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident root)
           (field_expr ((CellRef (NamedRef Cell)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false) (combinator_constructor (Constructor message))
        (combinator ((combinator_ident Message)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident info)
           (field_expr ((NamedRef CommonMsgInfo) () ())))
          (NamedField (field_ident init)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((Expr
                  (Exprs
                   (((NamedRef Either) () ()) ((NamedRef StateInit) () ())
                    ((CellRef (NamedRef StateInit)) () ()))))
                 () ()))))
             () ())))
          (NamedField (field_ident body)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Either) () ()) ((NamedRef X) () ())
                ((CellRef (NamedRef X)) () ()))))
             () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false) (combinator_constructor (Constructor message))
        (combinator ((combinator_ident MessageRelaxed)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident info)
           (field_expr ((NamedRef CommonMsgInfoRelaxed) () ())))
          (NamedField (field_ident init)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((Expr
                  (Exprs
                   (((NamedRef Either) () ()) ((NamedRef StateInit) () ())
                    ((CellRef (NamedRef StateInit)) () ()))))
                 () ()))))
             () ())))
          (NamedField (field_ident body)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Either) () ()) ((NamedRef X) () ())
                ((CellRef (NamedRef X)) () ()))))
             () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident MessageAny)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr (Exprs (((NamedRef Message) () ()) ((NamedRef Any) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (interm_addr_regular (0 0 1))))
        (combinator ((combinator_ident IntermediateAddress)))
        (combinator_fields
         ((NamedField (field_ident use_dest_bits)
           (field_expr ((Expr (Exprs ((NatLeq () ()) ((Int 96) () ())))) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (interm_addr_simple (01 0 2))))
        (combinator ((combinator_ident IntermediateAddress)))
        (combinator_fields
         ((NamedField (field_ident workchain_id)
           (field_expr ((NamedRef int8) () ())))
          (NamedField (field_ident addr_pfx)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (interm_addr_ext (11 0 2))))
        (combinator ((combinator_ident IntermediateAddress)))
        (combinator_fields
         ((NamedField (field_ident workchain_id)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident addr_pfx)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_envelope (0010 0 4))))
        (combinator ((combinator_ident MsgEnvelope)))
        (combinator_fields
         ((NamedField (field_ident cur_addr)
           (field_expr ((NamedRef IntermediateAddress) () ())))
          (NamedField (field_ident next_addr)
           (field_expr ((NamedRef IntermediateAddress) () ())))
          (NamedField (field_ident fwd_fee_remaining)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident msg)
           (field_expr
            ((CellRef
              (Expr (Exprs (((NamedRef Message) () ()) ((NamedRef Any) () ())))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_import_ext (000 0 3))))
        (combinator ((combinator_ident InMsg)))
        (combinator_fields
         ((NamedField (field_ident msg)
           (field_expr
            ((CellRef
              (Expr (Exprs (((NamedRef Message) () ()) ((NamedRef Any) () ())))))
             () ())))
          (NamedField (field_ident transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_import_ihr (010 0 3))))
        (combinator ((combinator_ident InMsg)))
        (combinator_fields
         ((NamedField (field_ident msg)
           (field_expr
            ((CellRef
              (Expr (Exprs (((NamedRef Message) () ()) ((NamedRef Any) () ())))))
             () ())))
          (NamedField (field_ident transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))
          (NamedField (field_ident ihr_fee)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident proof_created)
           (field_expr ((CellRef (NamedRef Cell)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_import_imm (110 0 3))))
        (combinator ((combinator_ident InMsg)))
        (combinator_fields
         ((NamedField (field_ident in_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))
          (NamedField (field_ident fwd_fee)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_import_fin (001 0 3))))
        (combinator ((combinator_ident InMsg)))
        (combinator_fields
         ((NamedField (field_ident in_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))
          (NamedField (field_ident fwd_fee)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_import_tr (101 0 3))))
        (combinator ((combinator_ident InMsg)))
        (combinator_fields
         ((NamedField (field_ident in_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident transit_fee)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_discard_fin (011 0 3))))
        (combinator ((combinator_ident InMsg)))
        (combinator_fields
         ((NamedField (field_ident in_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident transaction_id)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident fwd_fee)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_discard_tr (111 0 3))))
        (combinator ((combinator_ident InMsg)))
        (combinator_fields
         ((NamedField (field_ident in_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident transaction_id)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident fwd_fee)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident proof_delivered)
           (field_expr ((CellRef (NamedRef Cell)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor import_fees))
        (combinator ((combinator_ident ImportFees)))
        (combinator_fields
         ((NamedField (field_ident fees_collected)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident value_imported)
           (field_expr ((NamedRef CurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident InMsgDescr)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapAugE) () ()) ((Int 256) () ())
                 ((NamedRef InMsg) () ()) ((NamedRef ImportFees) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_export_ext (000 0 3))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident msg)
           (field_expr
            ((CellRef
              (Expr (Exprs (((NamedRef Message) () ()) ((NamedRef Any) () ())))))
             () ())))
          (NamedField (field_ident transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_export_imm (010 0 3))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))
          (NamedField (field_ident reimport)
           (field_expr ((CellRef (NamedRef InMsg)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_export_new (100 0 3))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_export_tr (110 0 3))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident imported)
           (field_expr ((CellRef (NamedRef InMsg)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (msg_export_deq (0011 0 4))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident import_block_lt)
           (field_expr ((NamedRef uint63) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (msg_export_deq_short (1011 0 4))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident msg_env_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident next_workchain)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident next_addr_pfx)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident import_block_lt)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (msg_export_tr_req (111 0 3))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident imported)
           (field_expr ((CellRef (NamedRef InMsg)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (msg_export_deq_imm (001 0 3))))
        (combinator ((combinator_ident OutMsg)))
        (combinator_fields
         ((NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))
          (NamedField (field_ident reimport)
           (field_expr ((CellRef (NamedRef InMsg)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident EnqueuedMsg)))
        (combinator_fields
         ((NamedField (field_ident enqueued_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident out_msg)
           (field_expr ((CellRef (NamedRef MsgEnvelope)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident OutMsgDescr)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapAugE) () ()) ((Int 256) () ())
                 ((NamedRef OutMsg) () ()) ((NamedRef CurrencyCollection) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident OutMsgQueue)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapAugE) () ()) ((Int 352) () ())
                 ((NamedRef EnqueuedMsg) () ()) ((NamedRef uint64) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor processed_upto))
        (combinator ((combinator_ident ProcessedUpto)))
        (combinator_fields
         ((NamedField (field_ident last_msg_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident last_msg_hash)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ProcessedInfo)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapE) () ()) ((Int 96) () ())
                 ((NamedRef ProcessedUpto) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor ihr_pending))
        (combinator ((combinator_ident IhrPendingSince)))
        (combinator_fields
         ((NamedField (field_ident import_lt)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident IhrPendingInfo)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapE) () ()) ((Int 320) () ())
                 ((NamedRef IhrPendingSince) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident OutMsgQueueInfo)))
        (combinator_fields
         ((NamedField (field_ident out_queue)
           (field_expr ((NamedRef OutMsgQueue) () ())))
          (NamedField (field_ident proc_info)
           (field_expr ((NamedRef ProcessedInfo) () ())))
          (NamedField (field_ident ihr_pending)
           (field_expr ((NamedRef IhrPendingInfo) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor storage_used))
        (combinator ((combinator_ident StorageUsed)))
        (combinator_fields
         ((NamedField (field_ident cells)
           (field_expr
            ((Expr (Exprs (((NamedRef VarUInteger) () ()) ((Int 7) () ())))) ()
             ())))
          (NamedField (field_ident bits)
           (field_expr
            ((Expr (Exprs (((NamedRef VarUInteger) () ()) ((Int 7) () ())))) ()
             ())))
          (NamedField (field_ident public_cells)
           (field_expr
            ((Expr (Exprs (((NamedRef VarUInteger) () ()) ((Int 7) () ())))) ()
             ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor storage_used_short))
        (combinator ((combinator_ident StorageUsedShort)))
        (combinator_fields
         ((NamedField (field_ident cells)
           (field_expr
            ((Expr (Exprs (((NamedRef VarUInteger) () ()) ((Int 7) () ())))) ()
             ())))
          (NamedField (field_ident bits)
           (field_expr
            ((Expr (Exprs (((NamedRef VarUInteger) () ()) ((Int 7) () ())))) ()
             ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor storage_info))
        (combinator ((combinator_ident StorageInfo)))
        (combinator_fields
         ((NamedField (field_ident used)
           (field_expr ((NamedRef StorageUsed) () ())))
          (NamedField (field_ident last_paid)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident due_payment)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef Grams) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (account_none (0 0 1))))
        (combinator ((combinator_ident Account))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (account (1 0 1))))
        (combinator ((combinator_ident Account)))
        (combinator_fields
         ((NamedField (field_ident addr)
           (field_expr ((NamedRef MsgAddressInt) () ())))
          (NamedField (field_ident storage_stat)
           (field_expr ((NamedRef StorageInfo) () ())))
          (NamedField (field_ident storage)
           (field_expr ((NamedRef AccountStorage) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor account_storage))
        (combinator ((combinator_ident AccountStorage)))
        (combinator_fields
         ((NamedField (field_ident last_trans_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident balance)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (NamedField (field_ident state)
           (field_expr ((NamedRef AccountState) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (account_uninit (00 0 2))))
        (combinator ((combinator_ident AccountState))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (account_active (1 0 1))))
        (combinator ((combinator_ident AccountState)))
        (combinator_fields
         ((AnonymousField (field_expr ((NamedRef StateInit) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (account_frozen (10 0 2))))
        (combinator ((combinator_ident AccountState)))
        (combinator_fields
         ((NamedField (field_ident state_hash)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (acc_state_uninit (00 0 2))))
        (combinator ((combinator_ident AccountStatus))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (acc_state_frozen (10 0 2))))
        (combinator ((combinator_ident AccountStatus))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (acc_state_active (01 0 2))))
        (combinator ((combinator_ident AccountStatus))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (acc_state_nonexist (11 0 2))))
        (combinator ((combinator_ident AccountStatus))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor account_descr))
        (combinator ((combinator_ident ShardAccount)))
        (combinator_fields
         ((NamedField (field_ident account)
           (field_expr ((CellRef (NamedRef Account)) () ())))
          (NamedField (field_ident last_trans_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident last_trans_lt)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor depth_balance))
        (combinator ((combinator_ident DepthBalanceInfo)))
        (combinator_fields
         ((NamedField (field_ident split_depth)
           (field_expr ((Expr (Exprs ((NatLeq () ()) ((Int 30) () ())))) () ())))
          (NamedField (field_ident balance)
           (field_expr ((NamedRef CurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ShardAccounts)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapAugE) () ()) ((Int 256) () ())
                 ((NamedRef ShardAccount) () ())
                 ((NamedRef DepthBalanceInfo) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (transaction (1110 0 4))))
        (combinator ((combinator_ident Transaction)))
        (combinator_fields
         ((NamedField (field_ident account_addr)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident lt) (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident prev_trans_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident prev_trans_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident now) (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident outmsg_cnt)
           (field_expr ((NamedRef uint15) () ())))
          (NamedField (field_ident orig_status)
           (field_expr ((NamedRef AccountStatus) () ())))
          (NamedField (field_ident end_status)
           (field_expr ((NamedRef AccountStatus) () ())))
          (ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident in_msg)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef Maybe) () ())
                       ((CellRef
                         (Expr
                          (Exprs
                           (((NamedRef Message) () ()) ((NamedRef Any) () ())))))
                        () ()))))
                    () ())))
                 (NamedField (field_ident out_msgs)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef HashmapE) () ()) ((Int 15) () ())
                       ((CellRef
                         (Expr
                          (Exprs
                           (((NamedRef Message) () ()) ((NamedRef Any) () ())))))
                        () ()))))
                    () ()))))))
              () ()))))
          (NamedField (field_ident total_fees)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (NamedField (field_ident state_update)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef HASH_UPDATE) () ()) ((NamedRef Account) () ())))))
             () ())))
          (NamedField (field_ident description)
           (field_expr ((CellRef (NamedRef TransactionDescr)) () ())))))
        (combinator_params ()))
       ((combinator_exotic true)
        (combinator_constructor
         (TaggedConstructor (merkle_update (01000000 0 8))))
        (combinator ((combinator_ident MERKLE_UPDATE)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident old_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident new_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident old)
           (field_expr ((CellRef (NamedRef X)) () ())))
          (NamedField (field_ident new)
           (field_expr ((CellRef (NamedRef X)) () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (update_hashes (01001110 0 8))))
        (combinator ((combinator_ident HASH_UPDATE)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident old_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident new_hash)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic true)
        (combinator_constructor
         (TaggedConstructor (merkle_proof (11000000 0 8))))
        (combinator ((combinator_ident MERKLE_PROOF)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident virtual_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident depth) (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident virtual_root)
           (field_expr ((CellRef (NamedRef X)) () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (acc_trans (1010 0 4))))
        (combinator ((combinator_ident AccountBlock)))
        (combinator_fields
         ((NamedField (field_ident account_addr)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident transactions)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapAug) () ()) ((Int 64) () ())
                ((CellRef (NamedRef Transaction)) () ())
                ((NamedRef CurrencyCollection) () ()))))
             () ())))
          (NamedField (field_ident state_update)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef HASH_UPDATE) () ()) ((NamedRef Account) () ())))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ShardAccountBlocks)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapAugE) () ()) ((Int 256) () ())
                 ((NamedRef AccountBlock) () ())
                 ((NamedRef CurrencyCollection) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor tr_phase_storage))
        (combinator ((combinator_ident TrStoragePhase)))
        (combinator_fields
         ((NamedField (field_ident storage_fees_collected)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident storage_fees_due)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef Grams) () ()))))
             () ())))
          (NamedField (field_ident status_change)
           (field_expr ((NamedRef AccStatusChange) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (acst_unchanged (0 0 1))))
        (combinator ((combinator_ident AccStatusChange))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (acst_frozen (01 0 2))))
        (combinator ((combinator_ident AccStatusChange))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (acst_deleted (11 0 2))))
        (combinator ((combinator_ident AccStatusChange))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor tr_phase_credit))
        (combinator ((combinator_ident TrCreditPhase)))
        (combinator_fields
         ((NamedField (field_ident due_fees_collected)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef Grams) () ()))))
             () ())))
          (NamedField (field_ident credit)
           (field_expr ((NamedRef CurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (tr_phase_compute_skipped (0 0 1))))
        (combinator ((combinator_ident TrComputePhase)))
        (combinator_fields
         ((NamedField (field_ident reason)
           (field_expr ((NamedRef ComputeSkipReason) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (tr_phase_compute_vm (1 0 1))))
        (combinator ((combinator_ident TrComputePhase)))
        (combinator_fields
         ((NamedField (field_ident success) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident msg_state_used)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident account_activated)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident gas_fees)
           (field_expr ((NamedRef Grams) () ())))
          (ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident gas_used)
                  (field_expr
                   ((Expr
                     (Exprs (((NamedRef VarUInteger) () ()) ((Int 7) () ()))))
                    () ())))
                 (NamedField (field_ident gas_limit)
                  (field_expr
                   ((Expr
                     (Exprs (((NamedRef VarUInteger) () ()) ((Int 7) () ()))))
                    () ())))
                 (NamedField (field_ident gas_credit)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef Maybe) () ())
                       ((Expr
                         (Exprs (((NamedRef VarUInteger) () ()) ((Int 3) () ()))))
                        () ()))))
                    () ())))
                 (NamedField (field_ident mode)
                  (field_expr ((NamedRef int8) () ())))
                 (NamedField (field_ident exit_code)
                  (field_expr ((NamedRef int32) () ())))
                 (NamedField (field_ident exit_arg)
                  (field_expr
                   ((Expr
                     (Exprs (((NamedRef Maybe) () ()) ((NamedRef int32) () ()))))
                    () ())))
                 (NamedField (field_ident vm_steps)
                  (field_expr ((NamedRef uint32) () ())))
                 (NamedField (field_ident vm_init_state_hash)
                  (field_expr ((NamedRef bits256) () ())))
                 (NamedField (field_ident vm_final_state_hash)
                  (field_expr ((NamedRef bits256) () ()))))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (cskip_no_state (00 0 2))))
        (combinator ((combinator_ident ComputeSkipReason)))
        (combinator_fields ()) (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (cskip_bad_state (10 0 2))))
        (combinator ((combinator_ident ComputeSkipReason)))
        (combinator_fields ()) (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (cskip_no_gas (01 0 2))))
        (combinator ((combinator_ident ComputeSkipReason)))
        (combinator_fields ()) (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor tr_phase_action))
        (combinator ((combinator_ident TrActionPhase)))
        (combinator_fields
         ((NamedField (field_ident success) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident valid) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident no_funds)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident status_change)
           (field_expr ((NamedRef AccStatusChange) () ())))
          (NamedField (field_ident total_fwd_fees)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef Grams) () ()))))
             () ())))
          (NamedField (field_ident total_action_fees)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef Grams) () ()))))
             () ())))
          (NamedField (field_ident result_code)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident result_arg)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef int32) () ()))))
             () ())))
          (NamedField (field_ident tot_actions)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident spec_actions)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident skipped_actions)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident msgs_created)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident action_list_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident tot_msg_size)
           (field_expr ((NamedRef StorageUsedShort) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (tr_phase_bounce_negfunds (00 0 2))))
        (combinator ((combinator_ident TrBouncePhase))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (tr_phase_bounce_nofunds (10 0 2))))
        (combinator ((combinator_ident TrBouncePhase)))
        (combinator_fields
         ((NamedField (field_ident msg_size)
           (field_expr ((NamedRef StorageUsedShort) () ())))
          (NamedField (field_ident req_fwd_fees)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (tr_phase_bounce_ok (1 0 1))))
        (combinator ((combinator_ident TrBouncePhase)))
        (combinator_fields
         ((NamedField (field_ident msg_size)
           (field_expr ((NamedRef StorageUsedShort) () ())))
          (NamedField (field_ident msg_fees)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident fwd_fees)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (trans_ord (0000 0 4))))
        (combinator ((combinator_ident TransactionDescr)))
        (combinator_fields
         ((NamedField (field_ident credit_first)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident storage_ph)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((NamedRef TrStoragePhase) () ()))))
             () ())))
          (NamedField (field_ident credit_ph)
           (field_expr
            ((Expr
              (Exprs (((NamedRef Maybe) () ()) ((NamedRef TrCreditPhase) () ()))))
             () ())))
          (NamedField (field_ident compute_ph)
           (field_expr ((NamedRef TrComputePhase) () ())))
          (NamedField (field_ident action)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef TrActionPhase)) () ()))))
             () ())))
          (NamedField (field_ident aborted) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident bounce)
           (field_expr
            ((Expr
              (Exprs (((NamedRef Maybe) () ()) ((NamedRef TrBouncePhase) () ()))))
             () ())))
          (NamedField (field_ident destroyed)
           (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (trans_storage (1000 0 4))))
        (combinator ((combinator_ident TransactionDescr)))
        (combinator_fields
         ((NamedField (field_ident storage_ph)
           (field_expr ((NamedRef TrStoragePhase) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (trans_tick_tock (100 0 3))))
        (combinator ((combinator_ident TransactionDescr)))
        (combinator_fields
         ((NamedField (field_ident is_tock) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident storage_ph)
           (field_expr ((NamedRef TrStoragePhase) () ())))
          (NamedField (field_ident compute_ph)
           (field_expr ((NamedRef TrComputePhase) () ())))
          (NamedField (field_ident action)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef TrActionPhase)) () ()))))
             () ())))
          (NamedField (field_ident aborted) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident destroyed)
           (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor split_merge_info))
        (combinator ((combinator_ident SplitMergeInfo)))
        (combinator_fields
         ((NamedField (field_ident cur_shard_pfx_len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 6) () ())))) () ())))
          (NamedField (field_ident acc_split_depth)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 6) () ())))) () ())))
          (NamedField (field_ident this_addr)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident sibling_addr)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (trans_split_prepare (0010 0 4))))
        (combinator ((combinator_ident TransactionDescr)))
        (combinator_fields
         ((NamedField (field_ident split_info)
           (field_expr ((NamedRef SplitMergeInfo) () ())))
          (NamedField (field_ident storage_ph)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((NamedRef TrStoragePhase) () ()))))
             () ())))
          (NamedField (field_ident compute_ph)
           (field_expr ((NamedRef TrComputePhase) () ())))
          (NamedField (field_ident action)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef TrActionPhase)) () ()))))
             () ())))
          (NamedField (field_ident aborted) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident destroyed)
           (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (trans_split_install (1010 0 4))))
        (combinator ((combinator_ident TransactionDescr)))
        (combinator_fields
         ((NamedField (field_ident split_info)
           (field_expr ((NamedRef SplitMergeInfo) () ())))
          (NamedField (field_ident prepare_transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))
          (NamedField (field_ident installed)
           (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (trans_merge_prepare (0110 0 4))))
        (combinator ((combinator_ident TransactionDescr)))
        (combinator_fields
         ((NamedField (field_ident split_info)
           (field_expr ((NamedRef SplitMergeInfo) () ())))
          (NamedField (field_ident storage_ph)
           (field_expr ((NamedRef TrStoragePhase) () ())))
          (NamedField (field_ident aborted) (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (trans_merge_install (1110 0 4))))
        (combinator ((combinator_ident TransactionDescr)))
        (combinator_fields
         ((NamedField (field_ident split_info)
           (field_expr ((NamedRef SplitMergeInfo) () ())))
          (NamedField (field_ident prepare_transaction)
           (field_expr ((CellRef (NamedRef Transaction)) () ())))
          (NamedField (field_ident storage_ph)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((NamedRef TrStoragePhase) () ()))))
             () ())))
          (NamedField (field_ident credit_ph)
           (field_expr
            ((Expr
              (Exprs (((NamedRef Maybe) () ()) ((NamedRef TrCreditPhase) () ()))))
             () ())))
          (NamedField (field_ident compute_ph)
           (field_expr ((NamedRef TrComputePhase) () ())))
          (NamedField (field_ident action)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef TrActionPhase)) () ()))))
             () ())))
          (NamedField (field_ident aborted) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident destroyed)
           (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (smc_info (01010111100011110111011011100000 0 32))))
        (combinator ((combinator_ident SmartContractInfo)))
        (combinator_fields
         ((NamedField (field_ident actions)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident msgs_sent)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident unixtime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident block_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident trans_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident rand_seed)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident balance_remaining)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (NamedField (field_ident myself)
           (field_expr ((NamedRef MsgAddressInt) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor out_list_empty))
        (combinator ((combinator_ident OutList))) (combinator_fields ())
        (combinator_params ((Int 0))))
       ((combinator_exotic false) (combinator_constructor (Constructor out_list))
        (combinator ((combinator_ident OutList)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident prev)
           (field_expr
            ((CellRef
              (Expr (Exprs (((NamedRef OutList) () ()) ((NamedRef n) () ())))))
             () ())))
          (NamedField (field_ident action)
           (field_expr ((NamedRef OutAction) () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ())))))))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (action_send_msg (10110110000100111100001101110000 0 32))))
        (combinator ((combinator_ident OutAction)))
        (combinator_fields
         ((NamedField (field_ident mode)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (NamedField (field_ident out_msg)
           (field_expr
            ((CellRef
              (Expr
               (Exprs (((NamedRef MessageRelaxed) () ()) ((NamedRef Any) () ())))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (action_set_code (01110001000001111011001010110101 0 32))))
        (combinator ((combinator_ident OutAction)))
        (combinator_fields
         ((NamedField (field_ident new_code)
           (field_expr ((CellRef (NamedRef Cell)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (action_reserve_currency (10010000000111010110011101101100 0 32))))
        (combinator ((combinator_ident OutAction)))
        (combinator_fields
         ((NamedField (field_ident mode)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (NamedField (field_ident currency)
           (field_expr ((NamedRef CurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (libref_hash (0 0 1))))
        (combinator ((combinator_ident LibRef)))
        (combinator_fields
         ((NamedField (field_ident lib_hash)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (libref_ref (1 0 1))))
        (combinator ((combinator_ident LibRef)))
        (combinator_fields
         ((NamedField (field_ident library)
           (field_expr ((CellRef (NamedRef Cell)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (action_change_library (00101011101110000101111101100100 0 32))))
        (combinator ((combinator_ident OutAction)))
        (combinator_fields
         ((NamedField (field_ident mode)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 7) () ())))) () ())))
          (ExprField
           (Leq ((Exprs (((NamedRef mode) () ()))) (Exprs (((Int 2) () ()))))))
          (NamedField (field_ident libref)
           (field_expr ((NamedRef LibRef) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor out_list_node))
        (combinator ((combinator_ident OutListNode)))
        (combinator_fields
         ((NamedField (field_ident prev)
           (field_expr ((CellRef (NamedRef Cell)) () ())))
          (NamedField (field_ident action)
           (field_expr ((NamedRef OutAction) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (shard_ident (00 0 2))))
        (combinator ((combinator_ident ShardIdent)))
        (combinator_fields
         ((NamedField (field_ident shard_pfx_bits)
           (field_expr ((Expr (Exprs ((NatLeq () ()) ((Int 60) () ())))) () ())))
          (NamedField (field_ident workchain_id)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident shard_prefix)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor ext_blk_ref))
        (combinator ((combinator_ident ExtBlkRef)))
        (combinator_fields
         ((NamedField (field_ident end_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident seq_no)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident root_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident file_hash)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor block_id_ext))
        (combinator ((combinator_ident BlockIdExt)))
        (combinator_fields
         ((NamedField (field_ident shard_id)
           (field_expr ((NamedRef ShardIdent) () ())))
          (NamedField (field_ident seq_no)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident root_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident file_hash)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor master_info))
        (combinator ((combinator_ident BlkMasterInfo)))
        (combinator_fields
         ((NamedField (field_ident master)
           (field_expr ((NamedRef ExtBlkRef) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (shard_state (01000111111101011100010000001001 0 32))))
        (combinator ((combinator_ident ShardStateUnsplit)))
        (combinator_fields
         ((NamedField (field_ident global_id)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident shard_id)
           (field_expr ((NamedRef ShardIdent) () ())))
          (NamedField (field_ident seq_no)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident vert_seq_no)
           (field_expr ((Type (Uint 32)) () ())))
          (NamedField (field_ident gen_utime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident gen_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident min_ref_mc_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident out_msg_queue_info)
           (field_expr ((CellRef (NamedRef OutMsgQueueInfo)) () ())))
          (NamedField (field_ident before_split)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident accounts)
           (field_expr ((CellRef (NamedRef ShardAccounts)) () ())))
          (ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident overload_history)
                  (field_expr ((NamedRef uint64) () ())))
                 (NamedField (field_ident underload_history)
                  (field_expr ((NamedRef uint64) () ())))
                 (NamedField (field_ident total_balance)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident total_validator_fees)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident libraries)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef HashmapE) () ()) ((Int 256) () ())
                       ((NamedRef LibDescr) () ()))))
                    () ())))
                 (NamedField (field_ident master_ref)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef Maybe) () ()) ((NamedRef BlkMasterInfo) () ()))))
                    () ()))))))
              () ()))))
          (NamedField (field_ident custom)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef McStateExtra)) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ShardState)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef ShardStateUnsplit) () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (split_state (10100101101111100100110011111010 0 32))))
        (combinator ((combinator_ident ShardState)))
        (combinator_fields
         ((NamedField (field_ident left)
           (field_expr ((CellRef (NamedRef ShardStateUnsplit)) () ())))
          (NamedField (field_ident right)
           (field_expr ((CellRef (NamedRef ShardStateUnsplit)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (shared_lib_descr (00 0 2))))
        (combinator ((combinator_ident LibDescr)))
        (combinator_fields
         ((NamedField (field_ident lib)
           (field_expr ((CellRef (NamedRef Cell)) () ())))
          (NamedField (field_ident publishers)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Hashmap) () ()) ((Int 256) () ())
                ((NamedRef True) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (block_info (11100001100101011110001111011001 0 32))))
        (combinator ((combinator_ident BlockInfo)))
        (combinator_fields
         ((NamedField (field_ident version)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident not_master)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident after_merge)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident before_split)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident after_split)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident want_split)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident want_merge)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident key_block)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident vert_seqno_incr)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (ExprField
           (Leq ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident seq_no) (field_expr ((Type (Uint 32)) () ())))
          (NamedField (field_ident vert_seq_no)
           (field_expr ((Type (Uint 32)) () ())))
          (ExprField
           (Geq
            ((Exprs (((NamedRef vert_seq_no) () ())))
             (Exprs (((NamedRef vert_seqno_incr) () ()))))))
          (ImplicitField (field_ident prev_seq_no) (field_type (Uint 32)))
          (ExprField
           (Equals
            ((Plus
              ((Exprs (((Runtime (NamedRef prev_seq_no)) () ())))
               (Exprs (((Int 1) () ())))))
             (Exprs (((NamedRef seq_no) () ()))))))
          (NamedField (field_ident shard)
           (field_expr ((NamedRef ShardIdent) () ())))
          (NamedField (field_ident gen_utime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident start_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident end_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident gen_validator_list_hash_short)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident gen_catchain_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident min_ref_mc_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident prev_key_block_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident gen_software)
           (field_expr ((NamedRef flags) ((Int 0)) ((NamedRef GlobalVersion)))))
          (NamedField (field_ident master_ref)
           (field_expr
            ((NamedRef not_master) () ((CellRef (NamedRef BlkMasterInfo))))))
          (NamedField (field_ident prev_ref)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef BlkPrevInfo) () ()) ((NamedRef after_merge) () ())))))
             () ())))
          (NamedField (field_ident prev_vert_ref)
           (field_expr
            ((NamedRef vert_seqno_incr) ()
             ((CellRef
               (Expr (Exprs (((NamedRef BlkPrevInfo) () ()) ((Int 0) () ())))))))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor prev_blk_info))
        (combinator ((combinator_ident BlkPrevInfo)))
        (combinator_fields
         ((NamedField (field_ident prev)
           (field_expr ((NamedRef ExtBlkRef) () ())))))
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor prev_blks_info))
        (combinator ((combinator_ident BlkPrevInfo)))
        (combinator_fields
         ((NamedField (field_ident prev1)
           (field_expr ((CellRef (NamedRef ExtBlkRef)) () ())))
          (NamedField (field_ident prev2)
           (field_expr ((CellRef (NamedRef ExtBlkRef)) () ())))))
        (combinator_params ((Int 1))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (block (01010101101010101111011110001000 0 32))))
        (combinator ((combinator_ident Block)))
        (combinator_fields
         ((NamedField (field_ident global_id)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident info)
           (field_expr ((CellRef (NamedRef BlockInfo)) () ())))
          (NamedField (field_ident value_flow)
           (field_expr ((CellRef (NamedRef ValueFlow)) () ())))
          (NamedField (field_ident state_update)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef MERKLE_UPDATE) () ()) ((NamedRef ShardState) () ())))))
             () ())))
          (NamedField (field_ident extra)
           (field_expr ((CellRef (NamedRef BlockExtra)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor block_extra))
        (combinator ((combinator_ident BlockExtra)))
        (combinator_fields
         ((NamedField (field_ident in_msg_descr)
           (field_expr ((CellRef (NamedRef InMsgDescr)) () ())))
          (NamedField (field_ident out_msg_descr)
           (field_expr ((CellRef (NamedRef OutMsgDescr)) () ())))
          (NamedField (field_ident account_blocks)
           (field_expr ((CellRef (NamedRef ShardAccountBlocks)) () ())))
          (NamedField (field_ident rand_seed)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident created_by)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident custom)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef McBlockExtra)) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor value_flow))
        (combinator ((combinator_ident ValueFlow)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident from_prev_blk)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident to_next_blk)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident imported)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident exported)
                  (field_expr ((NamedRef CurrencyCollection) () ()))))))
              () ()))))
          (NamedField (field_ident fees_collected)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident fees_imported)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident recovered)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident created)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident minted)
                  (field_expr ((NamedRef CurrencyCollection) () ()))))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bt_leaf (0 0 1))))
        (combinator ((combinator_ident BinTree)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident leaf) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bt_fork (1 0 1))))
        (combinator ((combinator_ident BinTree)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (NamedField (field_ident left)
           (field_expr
            ((CellRef
              (Expr (Exprs (((NamedRef BinTree) () ()) ((NamedRef X) () ())))))
             () ())))
          (NamedField (field_ident right)
           (field_expr
            ((CellRef
              (Expr (Exprs (((NamedRef BinTree) () ()) ((NamedRef X) () ())))))
             () ())))))
        (combinator_params ((NamedRef X))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (fsm_none (0 0 1))))
        (combinator ((combinator_ident FutureSplitMerge))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (fsm_split (01 0 2))))
        (combinator ((combinator_ident FutureSplitMerge)))
        (combinator_fields
         ((NamedField (field_ident split_utime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident interval)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (fsm_merge (11 0 2))))
        (combinator ((combinator_ident FutureSplitMerge)))
        (combinator_fields
         ((NamedField (field_ident merge_utime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident interval)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (shard_descr (1101 0 4))))
        (combinator ((combinator_ident ShardDescr)))
        (combinator_fields
         ((NamedField (field_ident seq_no)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident reg_mc_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident start_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident end_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident root_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident file_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident before_split)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident before_merge)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident want_split)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident want_merge)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident nx_cc_updated)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 3) () ())))) () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 0) () ()))))))
          (NamedField (field_ident next_catchain_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident next_validator_shard)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident min_ref_mc_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident gen_utime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident split_merge_at)
           (field_expr ((NamedRef FutureSplitMerge) () ())))
          (NamedField (field_ident fees_collected)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (NamedField (field_ident funds_created)
           (field_expr ((NamedRef CurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (shard_descr_new (0101 0 4))))
        (combinator ((combinator_ident ShardDescr)))
        (combinator_fields
         ((NamedField (field_ident seq_no)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident reg_mc_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident start_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident end_lt)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident root_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident file_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident before_split)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident before_merge)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident want_split)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident want_merge)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident nx_cc_updated)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 3) () ())))) () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 0) () ()))))))
          (NamedField (field_ident next_catchain_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident next_validator_shard)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident min_ref_mc_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident gen_utime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident split_merge_at)
           (field_expr ((NamedRef FutureSplitMerge) () ())))
          (ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident fees_collected)
                  (field_expr ((NamedRef CurrencyCollection) () ())))
                 (NamedField (field_ident funds_created)
                  (field_expr ((NamedRef CurrencyCollection) () ()))))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ShardHashes)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapE) () ()) ((Int 32) () ())
                 ((CellRef
                   (Expr
                    (Exprs
                     (((NamedRef BinTree) () ()) ((NamedRef ShardDescr) () ())))))
                  () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bta_leaf (0 0 1))))
        (combinator ((combinator_ident BinTreeAug)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident extra) (field_expr ((NamedRef Y) () ())))
          (NamedField (field_ident leaf) (field_expr ((NamedRef X) () ())))))
        (combinator_params ((NamedRef X) (NamedRef Y))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (bta_fork (1 0 1))))
        (combinator ((combinator_ident BinTreeAug)))
        (combinator_fields
         ((ImplicitField (field_ident X) (field_type Type))
          (ImplicitField (field_ident Y) (field_type Type))
          (NamedField (field_ident left)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef BinTreeAug) () ()) ((NamedRef X) () ())
                 ((NamedRef Y) () ())))))
             () ())))
          (NamedField (field_ident right)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef BinTreeAug) () ()) ((NamedRef X) () ())
                 ((NamedRef Y) () ())))))
             () ())))
          (NamedField (field_ident extra) (field_expr ((NamedRef Y) () ())))))
        (combinator_params ((NamedRef X) (NamedRef Y))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ShardFeeCreated)))
        (combinator_fields
         ((NamedField (field_ident fees)
           (field_expr ((NamedRef CurrencyCollection) () ())))
          (NamedField (field_ident create)
           (field_expr ((NamedRef CurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ShardFees)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapAugE) () ()) ((Int 96) () ())
                 ((NamedRef ShardFeeCreated) () ())
                 ((NamedRef ShardFeeCreated) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParams)))
        (combinator_fields
         ((NamedField (field_ident config_addr)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident config)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef Hashmap) () ()) ((Int 32) () ())
                 ((CellRef (NamedRef Cell)) () ())))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor validator_info))
        (combinator ((combinator_ident ValidatorInfo)))
        (combinator_fields
         ((NamedField (field_ident validator_list_hash_short)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident catchain_seqno)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident nx_cc_updated)
           (field_expr ((NamedRef Bool) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor validator_base_info))
        (combinator ((combinator_ident ValidatorBaseInfo)))
        (combinator_fields
         ((NamedField (field_ident validator_list_hash_short)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident catchain_seqno)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident KeyMaxLt)))
        (combinator_fields
         ((NamedField (field_ident key) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident max_end_lt)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident KeyExtBlkRef)))
        (combinator_fields
         ((NamedField (field_ident key) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident blk_ref)
           (field_expr ((NamedRef ExtBlkRef) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident OldMcBlocksInfo)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapAugE) () ()) ((Int 32) () ())
                 ((NamedRef KeyExtBlkRef) () ()) ((NamedRef KeyMaxLt) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false) (combinator_constructor (Constructor counters))
        (combinator ((combinator_ident Counters)))
        (combinator_fields
         ((NamedField (field_ident last_updated)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident total) (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident cnt2048)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident cnt65536)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (creator_info (0010 0 4))))
        (combinator ((combinator_ident CreatorStats)))
        (combinator_fields
         ((NamedField (field_ident mc_blocks)
           (field_expr ((NamedRef Counters) () ())))
          (NamedField (field_ident shard_blocks)
           (field_expr ((NamedRef Counters) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (block_create_stats (11101000 0 8))))
        (combinator ((combinator_ident BlockCreateStats)))
        (combinator_fields
         ((NamedField (field_ident counters)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 256) () ())
                ((NamedRef CreatorStats) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (block_create_stats_ext (00101100 0 8))))
        (combinator ((combinator_ident BlockCreateStats)))
        (combinator_fields
         ((NamedField (field_ident counters)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapAugE) () ()) ((Int 256) () ())
                ((NamedRef CreatorStats) () ()) ((NamedRef uint32) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (masterchain_state_extra (0110010000110011 0 16))))
        (combinator ((combinator_ident McStateExtra)))
        (combinator_fields
         ((NamedField (field_ident shard_hashes)
           (field_expr ((NamedRef ShardHashes) () ())))
          (NamedField (field_ident config)
           (field_expr ((NamedRef ConfigParams) () ())))
          (ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident flags)
                  (field_expr
                   ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
                 (ExprField
                  (Leq
                   ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 1) () ()))))))
                 (NamedField (field_ident validator_info)
                  (field_expr ((NamedRef ValidatorInfo) () ())))
                 (NamedField (field_ident prev_blocks)
                  (field_expr ((NamedRef OldMcBlocksInfo) () ())))
                 (NamedField (field_ident after_key_block)
                  (field_expr ((NamedRef Bool) () ())))
                 (NamedField (field_ident last_key_block)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef Maybe) () ()) ((NamedRef ExtBlkRef) () ()))))
                    () ())))
                 (NamedField (field_ident block_create_stats)
                  (field_expr
                   ((Expr (Exprs (((NamedRef flags) ((Int 0)) ())))) ()
                    ((NamedRef BlockCreateStats))))))))
              () ()))))
          (NamedField (field_ident global_balance)
           (field_expr ((NamedRef CurrencyCollection) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (ed25519_pubkey (01010001111001001000000101110001 0 32))))
        (combinator ((combinator_ident SigPubKey)))
        (combinator_fields
         ((NamedField (field_ident pubkey)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (ed25519_signature (1010 0 4))))
        (combinator ((combinator_ident CryptoSignatureSimple)))
        (combinator_fields
         ((NamedField (field_ident R) (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident s) (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident CryptoSignature)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef CryptoSignatureSimple) () ()))))))
        (combinator_params ()))
       ((combinator_exotic false) (combinator_constructor (Constructor sig_pair))
        (combinator ((combinator_ident CryptoSignaturePair)))
        (combinator_fields
         ((NamedField (field_ident node_id_short)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident sign)
           (field_expr ((NamedRef CryptoSignature) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (certificate (0010 0 4))))
        (combinator ((combinator_ident Certificate)))
        (combinator_fields
         ((NamedField (field_ident temp_key)
           (field_expr ((NamedRef SigPubKey) () ())))
          (NamedField (field_ident valid_since)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident valid_until)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (certificate_env (1011111011011001100000100101 0 28))))
        (combinator ((combinator_ident CertificateEnv)))
        (combinator_fields
         ((NamedField (field_ident certificate)
           (field_expr ((NamedRef Certificate) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor signed_certificate))
        (combinator ((combinator_ident SignedCertificate)))
        (combinator_fields
         ((NamedField (field_ident certificate)
           (field_expr ((NamedRef Certificate) () ())))
          (NamedField (field_ident certificate_signature)
           (field_expr ((NamedRef CryptoSignature) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (chained_signature (1111 0 4))))
        (combinator ((combinator_ident CryptoSignature)))
        (combinator_fields
         ((NamedField (field_ident signed_cert)
           (field_expr ((CellRef (NamedRef SignedCertificate)) () ())))
          (NamedField (field_ident temp_key_signature)
           (field_expr ((NamedRef CryptoSignatureSimple) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (masterchain_block_extra (1010010100110011 0 16))))
        (combinator ((combinator_ident McBlockExtra)))
        (combinator_fields
         ((NamedField (field_ident key_block)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident shard_hashes)
           (field_expr ((NamedRef ShardHashes) () ())))
          (NamedField (field_ident shard_fees)
           (field_expr ((NamedRef ShardFees) () ())))
          (ExprField
           (Exprs
            (((CellRef
               (AnonymousConstr
                ((NamedField (field_ident prev_blk_signatures)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef HashmapE) () ()) ((Int 16) () ())
                       ((NamedRef CryptoSignaturePair) () ()))))
                    () ())))
                 (NamedField (field_ident recover_create_msg)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef Maybe) () ())
                       ((CellRef (NamedRef InMsg)) () ()))))
                    () ())))
                 (NamedField (field_ident mint_msg)
                  (field_expr
                   ((Expr
                     (Exprs
                      (((NamedRef Maybe) () ())
                       ((CellRef (NamedRef InMsg)) () ()))))
                    () ()))))))
              () ()))))
          (NamedField (field_ident config)
           (field_expr ((NamedRef key_block) () ((NamedRef ConfigParams)))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (validator (11001010 0 8))))
        (combinator ((combinator_ident ValidatorDescr)))
        (combinator_fields
         ((NamedField (field_ident public_key)
           (field_expr ((NamedRef SigPubKey) () ())))
          (NamedField (field_ident weight)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (validator_addr (11001110 0 8))))
        (combinator ((combinator_ident ValidatorDescr)))
        (combinator_fields
         ((NamedField (field_ident public_key)
           (field_expr ((NamedRef SigPubKey) () ())))
          (NamedField (field_ident weight)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident adnl_addr)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (validators (10001000 0 8))))
        (combinator ((combinator_ident ValidatorSet)))
        (combinator_fields
         ((NamedField (field_ident utime_since)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident utime_until)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident total)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (NamedField (field_ident main)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (ExprField
           (Leq
            ((Exprs (((NamedRef main) () ())))
             (Exprs (((NamedRef total) () ()))))))
          (ExprField
           (Geq ((Exprs (((NamedRef main) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident list)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Hashmap) () ()) ((Int 16) () ())
                ((NamedRef ValidatorDescr) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (validators_ext (01001000 0 8))))
        (combinator ((combinator_ident ValidatorSet)))
        (combinator_fields
         ((NamedField (field_ident utime_since)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident utime_until)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident total)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (NamedField (field_ident main)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (ExprField
           (Leq
            ((Exprs (((NamedRef main) () ())))
             (Exprs (((NamedRef total) () ()))))))
          (ExprField
           (Geq ((Exprs (((NamedRef main) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident total_weight)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident list)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 16) () ())
                ((NamedRef ValidatorDescr) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident config_addr)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident elector_addr)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ((Int 1))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident minter_addr)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ((Int 2))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident fee_collector_addr)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ((Int 3))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident dns_root_addr)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ((Int 4))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident mint_new_price)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident mint_add_price)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ((Int 6))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident to_mint)
           (field_expr ((NamedRef ExtraCurrencyCollection) () ())))))
        (combinator_params ((Int 7))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (capabilities (00100011 0 8))))
        (combinator ((combinator_ident GlobalVersion)))
        (combinator_fields
         ((NamedField (field_ident version)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident capabilities)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef GlobalVersion) () ()))))))
        (combinator_params ((Int 8))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident mandatory_params)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Hashmap) () ()) ((Int 32) () ())
                ((NamedRef True) () ()))))
             () ())))))
        (combinator_params ((Int 9))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident critical_params)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Hashmap) () ()) ((Int 32) () ())
                ((NamedRef True) () ()))))
             () ())))))
        (combinator_params ((Int 10))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (cfg_vote_cfg (01101100 0 8))))
        (combinator ((combinator_ident ConfigProposalSetup)))
        (combinator_fields
         ((NamedField (field_ident min_tot_rounds)
           (field_expr ((NamedRef uint8) () ())))
          (NamedField (field_ident max_tot_rounds)
           (field_expr ((NamedRef uint8) () ())))
          (NamedField (field_ident min_wins)
           (field_expr ((NamedRef uint8) () ())))
          (NamedField (field_ident max_losses)
           (field_expr ((NamedRef uint8) () ())))
          (NamedField (field_ident min_store_sec)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident max_store_sec)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident bit_price)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident cell_price)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (cfg_vote_setup (10001001 0 8))))
        (combinator ((combinator_ident ConfigVotingSetup)))
        (combinator_fields
         ((NamedField (field_ident normal_params)
           (field_expr ((CellRef (NamedRef ConfigProposalSetup)) () ())))
          (NamedField (field_ident critical_params)
           (field_expr ((CellRef (NamedRef ConfigProposalSetup)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef ConfigVotingSetup) () ()))))))
        (combinator_params ((Int 11))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (cfg_proposal (11001111 0 8))))
        (combinator ((combinator_ident ConfigProposal)))
        (combinator_fields
         ((NamedField (field_ident param_id)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident param_value)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((CellRef (NamedRef Cell)) () ()))))
             () ())))
          (NamedField (field_ident if_hash_equal)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef uint256) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (cfg_proposal_status (01110011 0 8))))
        (combinator ((combinator_ident ConfigProposalStatus)))
        (combinator_fields
         ((NamedField (field_ident expires)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident proposal)
           (field_expr ((CellRef (NamedRef ConfigProposal)) () ())))
          (NamedField (field_ident is_critical)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident voters)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 16) () ())
                ((NamedRef True) () ()))))
             () ())))
          (NamedField (field_ident remaining_weight)
           (field_expr ((NamedRef int64) () ())))
          (NamedField (field_ident validator_set_id)
           (field_expr ((NamedRef uint256) () ())))
          (NamedField (field_ident rounds_remaining)
           (field_expr ((NamedRef uint8) () ())))
          (NamedField (field_ident wins) (field_expr ((NamedRef uint8) () ())))
          (NamedField (field_ident losses) (field_expr ((NamedRef uint8) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (wfmt_basic (1000 0 4))))
        (combinator ((combinator_ident WorkchainFormat)))
        (combinator_fields
         ((NamedField (field_ident vm_version)
           (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident vm_mode)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ((Int 1))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (wfmt_ext (0000 0 4))))
        (combinator ((combinator_ident WorkchainFormat)))
        (combinator_fields
         ((NamedField (field_ident min_addr_len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 12) () ())))) () ())))
          (NamedField (field_ident max_addr_len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 12) () ())))) () ())))
          (NamedField (field_ident addr_len_step)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 12) () ())))) () ())))
          (ExprField
           (Geq
            ((Exprs (((NamedRef min_addr_len) () ())))
             (Exprs (((Int 64) () ()))))))
          (ExprField
           (Leq
            ((Exprs (((NamedRef min_addr_len) () ())))
             (Exprs (((NamedRef max_addr_len) () ()))))))
          (ExprField
           (Leq
            ((Exprs (((NamedRef max_addr_len) () ())))
             (Exprs (((Int 1023) () ()))))))
          (ExprField
           (Leq
            ((Exprs (((NamedRef addr_len_step) () ())))
             (Exprs (((Int 1023) () ()))))))
          (NamedField (field_ident workchain_type_id)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 32) () ())))) () ())))
          (ExprField
           (Geq
            ((Exprs (((NamedRef workchain_type_id) () ())))
             (Exprs (((Int 1) () ()))))))))
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (workchain (01100101 0 8))))
        (combinator ((combinator_ident WorkchainDescr)))
        (combinator_fields
         ((NamedField (field_ident enabled_since)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident actual_min_split)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (NamedField (field_ident min_split)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (NamedField (field_ident max_split)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (ExprField
           (Leq
            ((Exprs (((NamedRef actual_min_split) () ())))
             (Exprs (((NamedRef min_split) () ()))))))
          (NamedField (field_ident basic)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 1) () ())))) () ())))
          (NamedField (field_ident active) (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident accept_msgs)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 13) () ())))) () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 0) () ()))))))
          (NamedField (field_ident zerostate_root_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident zerostate_file_hash)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident version)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident format)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef WorkchainFormat) () ()) ((NamedRef basic) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident workchains)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 32) () ())
                ((NamedRef WorkchainDescr) () ()))))
             () ())))))
        (combinator_params ((Int 12))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (complaint_prices (01011000 0 8))))
        (combinator ((combinator_ident ComplaintPricing)))
        (combinator_fields
         ((NamedField (field_ident deposit)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident bit_price)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident cell_price)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef ComplaintPricing) () ()))))))
        (combinator_params ((Int 13))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (block_grams_created (11010110 0 8))))
        (combinator ((combinator_ident BlockCreateFees)))
        (combinator_fields
         ((NamedField (field_ident masterchain_block_fee)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident basechain_block_fee)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef BlockCreateFees) () ()))))))
        (combinator_params ((Int 14))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident validators_elected_for)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident elections_start_before)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident elections_end_before)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident stake_held_for)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ((Int 15))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident max_validators)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (NamedField (field_ident max_main_validators)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (NamedField (field_ident min_validators)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (ExprField
           (Geq
            ((Exprs (((NamedRef max_validators) () ())))
             (Exprs (((NamedRef max_main_validators) () ()))))))
          (ExprField
           (Geq
            ((Exprs (((NamedRef max_main_validators) () ())))
             (Exprs (((NamedRef min_validators) () ()))))))
          (ExprField
           (Geq
            ((Exprs (((NamedRef min_validators) () ())))
             (Exprs (((Int 1) () ()))))))))
        (combinator_params ((Int 16))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident min_stake)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident max_stake)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident min_total_stake)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident max_stake_factor)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ((Int 17))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ((00110011 0 8))))
        (combinator ((combinator_ident StoragePrices)))
        (combinator_fields
         ((NamedField (field_ident utime_since)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident bit_price_ps)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident cell_price_ps)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident mc_bit_price_ps)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident mc_cell_price_ps)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef Hashmap) () ()) ((Int 32) () ())
                 ((NamedRef StoragePrices) () ()))))
              () ()))))))
        (combinator_params ((Int 18))))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (gas_prices (10111011 0 8))))
        (combinator ((combinator_ident GasLimitsPrices)))
        (combinator_fields
         ((NamedField (field_ident gas_price)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident gas_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident gas_credit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident block_gas_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident freeze_due_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident delete_due_limit)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (gas_prices_ext (01111011 0 8))))
        (combinator ((combinator_ident GasLimitsPrices)))
        (combinator_fields
         ((NamedField (field_ident gas_price)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident gas_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident special_gas_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident gas_credit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident block_gas_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident freeze_due_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident delete_due_limit)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (gas_flat_pfx (10001011 0 8))))
        (combinator ((combinator_ident GasLimitsPrices)))
        (combinator_fields
         ((NamedField (field_ident flat_gas_limit)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident flat_gas_price)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident other)
           (field_expr ((NamedRef GasLimitsPrices) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor config_mc_gas_prices))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef GasLimitsPrices) () ()))))))
        (combinator_params ((Int 20))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor config_gas_prices))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef GasLimitsPrices) () ()))))))
        (combinator_params ((Int 21))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (param_limits (11000011 0 8))))
        (combinator ((combinator_ident ParamLimits)))
        (combinator_fields
         ((NamedField (field_ident underload)
           (field_expr ((Type (Uint 32)) () ())))
          (NamedField (field_ident soft_limit)
           (field_expr ((Type (Uint 32)) () ())))
          (ExprField
           (Leq
            ((Exprs (((NamedRef underload) () ())))
             (Exprs (((NamedRef soft_limit) () ()))))))
          (NamedField (field_ident hard_limit)
           (field_expr ((Type (Uint 32)) () ())))
          (ExprField
           (Leq
            ((Exprs (((NamedRef soft_limit) () ())))
             (Exprs (((NamedRef hard_limit) () ()))))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (block_limits (10111010 0 8))))
        (combinator ((combinator_ident BlockLimits)))
        (combinator_fields
         ((NamedField (field_ident bytes)
           (field_expr ((NamedRef ParamLimits) () ())))
          (NamedField (field_ident gas)
           (field_expr ((NamedRef ParamLimits) () ())))
          (NamedField (field_ident lt_delta)
           (field_expr ((NamedRef ParamLimits) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor config_mc_block_limits))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef BlockLimits) () ()))))))
        (combinator_params ((Int 22))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor config_block_limits))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef BlockLimits) () ()))))))
        (combinator_params ((Int 23))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (msg_forward_prices (01010111 0 8))))
        (combinator ((combinator_ident MsgForwardPrices)))
        (combinator_fields
         ((NamedField (field_ident lump_price)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident bit_price)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident cell_price)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident ihr_price_factor)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident first_frac)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident next_frac)
           (field_expr ((NamedRef uint16) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor config_mc_fwd_prices))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef MsgForwardPrices) () ()))))))
        (combinator_params ((Int 24))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor config_fwd_prices))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef MsgForwardPrices) () ()))))))
        (combinator_params ((Int 25))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (catchain_config (10000011 0 8))))
        (combinator ((combinator_ident CatchainConfig)))
        (combinator_fields
         ((NamedField (field_ident mc_catchain_lifetime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident shard_catchain_lifetime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident shard_validators_lifetime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident shard_validators_num)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (catchain_config_new (01000011 0 8))))
        (combinator ((combinator_ident CatchainConfig)))
        (combinator_fields
         ((NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 7) () ())))) () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 0) () ()))))))
          (NamedField (field_ident shuffle_mc_validators)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident mc_catchain_lifetime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident shard_catchain_lifetime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident shard_validators_lifetime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident shard_validators_num)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (consensus_config (01101011 0 8))))
        (combinator ((combinator_ident ConsensusConfig)))
        (combinator_fields
         ((NamedField (field_ident round_candidates)
           (field_expr ((Type (Uint 32)) () ())))
          (ExprField
           (Geq
            ((Exprs (((NamedRef round_candidates) () ())))
             (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident next_candidate_delay_ms)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident consensus_timeout_ms)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident fast_attempts)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident attempt_duration)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident catchain_max_deps)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident max_block_bytes)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident max_collated_bytes)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (consensus_config_new (11101011 0 8))))
        (combinator ((combinator_ident ConsensusConfig)))
        (combinator_fields
         ((NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 7) () ())))) () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 0) () ()))))))
          (NamedField (field_ident new_catchain_ids)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident round_candidates)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (ExprField
           (Geq
            ((Exprs (((NamedRef round_candidates) () ())))
             (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident next_candidate_delay_ms)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident consensus_timeout_ms)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident fast_attempts)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident attempt_duration)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident catchain_max_deps)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident max_block_bytes)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident max_collated_bytes)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (consensus_config_v3 (00011011 0 8))))
        (combinator ((combinator_ident ConsensusConfig)))
        (combinator_fields
         ((NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 7) () ())))) () ())))
          (ExprField
           (Equals
            ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 0) () ()))))))
          (NamedField (field_ident new_catchain_ids)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident round_candidates)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (ExprField
           (Geq
            ((Exprs (((NamedRef round_candidates) () ())))
             (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident next_candidate_delay_ms)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident consensus_timeout_ms)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident fast_attempts)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident attempt_duration)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident catchain_max_deps)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident max_block_bytes)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident max_collated_bytes)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident proto_version)
           (field_expr ((NamedRef uint16) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef CatchainConfig) () ()))))))
        (combinator_params ((Int 28))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef ConsensusConfig) () ()))))))
        (combinator_params ((Int 29))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident fundamental_smc_addr)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 256) () ())
                ((NamedRef True) () ()))))
             () ())))))
        (combinator_params ((Int 31))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident prev_validators)
           (field_expr ((NamedRef ValidatorSet) () ())))))
        (combinator_params ((Int 32))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident prev_temp_validators)
           (field_expr ((NamedRef ValidatorSet) () ())))))
        (combinator_params ((Int 33))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident cur_validators)
           (field_expr ((NamedRef ValidatorSet) () ())))))
        (combinator_params ((Int 34))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident cur_temp_validators)
           (field_expr ((NamedRef ValidatorSet) () ())))))
        (combinator_params ((Int 35))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident next_validators)
           (field_expr ((NamedRef ValidatorSet) () ())))))
        (combinator_params ((Int 36))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((NamedField (field_ident next_temp_validators)
           (field_expr ((NamedRef ValidatorSet) () ())))))
        (combinator_params ((Int 37))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (validator_temp_key (1100 0 4))))
        (combinator ((combinator_ident ValidatorTempKey)))
        (combinator_fields
         ((NamedField (field_ident adnl_addr)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident temp_public_key)
           (field_expr ((NamedRef SigPubKey) () ())))
          (NamedField (field_ident seqno) (field_expr ((Type (Uint 32)) () ())))
          (NamedField (field_ident valid_until)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (signed_temp_key (0010 0 4))))
        (combinator ((combinator_ident ValidatorSignedTempKey)))
        (combinator_fields
         ((NamedField (field_ident key)
           (field_expr ((CellRef (NamedRef ValidatorTempKey)) () ())))
          (NamedField (field_ident signature)
           (field_expr ((NamedRef CryptoSignature) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapE) () ()) ((Int 256) () ())
                 ((NamedRef ValidatorSignedTempKey) () ()))))
              () ()))))))
        (combinator_params ((Int 39))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (misbehaviour_punishment_config_v1 (10000000 0 8))))
        (combinator ((combinator_ident MisbehaviourPunishmentConfig)))
        (combinator_fields
         ((NamedField (field_ident default_flat_fine)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident default_proportional_fine)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident severity_flat_mult)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident severity_proportional_mult)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident unpunishable_interval)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident long_interval)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident long_flat_mult)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident long_proportional_mult)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident medium_interval)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident medium_flat_mult)
           (field_expr ((NamedRef uint16) () ())))
          (NamedField (field_ident medium_proportional_mult)
           (field_expr ((NamedRef uint16) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef MisbehaviourPunishmentConfig) () ()))))))
        (combinator_params ((Int 40))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor oracle_bridge_params))
        (combinator ((combinator_ident OracleBridgeParams)))
        (combinator_fields
         ((NamedField (field_ident bridge_address)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident oracle_mutlisig_address)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident oracles)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 256) () ())
                ((NamedRef uint256) () ()))))
             () ())))
          (NamedField (field_ident external_chain_address)
           (field_expr ((NamedRef bits256) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef OracleBridgeParams) () ()))))))
        (combinator_params ((Int 71))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef OracleBridgeParams) () ()))))))
        (combinator_params ((Int 72))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident ConfigParam)))
        (combinator_fields
         ((ExprField (Exprs (((NamedRef OracleBridgeParams) () ()))))))
        (combinator_params ((Int 73))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor block_signatures_pure))
        (combinator ((combinator_ident BlockSignaturesPure)))
        (combinator_fields
         ((NamedField (field_ident sig_count)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident sig_weight)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident signatures)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 16) () ())
                ((NamedRef CryptoSignaturePair) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (block_signatures (10001000 0 8))))
        (combinator ((combinator_ident BlockSignatures)))
        (combinator_fields
         ((NamedField (field_ident validator_info)
           (field_expr ((NamedRef ValidatorBaseInfo) () ())))
          (NamedField (field_ident pure_signatures)
           (field_expr ((NamedRef BlockSignaturesPure) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (block_proof (11000011 0 8))))
        (combinator ((combinator_ident BlockProof)))
        (combinator_fields
         ((NamedField (field_ident proof_for)
           (field_expr ((NamedRef BlockIdExt) () ())))
          (NamedField (field_ident root)
           (field_expr ((CellRef (NamedRef Cell)) () ())))
          (NamedField (field_ident signatures)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef BlockSignatures)) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chain_empty))
        (combinator ((combinator_ident ProofChain))) (combinator_fields ())
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chain_link))
        (combinator ((combinator_ident ProofChain)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident root)
           (field_expr ((CellRef (NamedRef Cell)) () ())))
          (NamedField (field_ident prev)
           (field_expr
            ((NamedRef n) ()
             ((CellRef
               (Expr
                (Exprs (((NamedRef ProofChain) () ()) ((NamedRef n) () ())))))))))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ())))))))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (top_block_descr (10101011 0 8))))
        (combinator ((combinator_ident TopBlockDescr)))
        (combinator_fields
         ((NamedField (field_ident proof_for)
           (field_expr ((NamedRef BlockIdExt) () ())))
          (NamedField (field_ident signatures)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ())
                ((CellRef (NamedRef BlockSignatures)) () ()))))
             () ())))
          (NamedField (field_ident len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (ExprField
           (Geq ((Exprs (((NamedRef len) () ()))) (Exprs (((Int 1) () ()))))))
          (ExprField
           (Leq ((Exprs (((NamedRef len) () ()))) (Exprs (((Int 8) () ()))))))
          (NamedField (field_ident chain)
           (field_expr
            ((Expr
              (Exprs (((NamedRef ProofChain) () ()) ((NamedRef len) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (top_block_descr_set (11001111100100011110001101010010 0 32))))
        (combinator ((combinator_ident TopBlockDescrSet)))
        (combinator_fields
         ((NamedField (field_ident collection)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 96) () ())
                ((CellRef (NamedRef TopBlockDescr)) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (prod_info (00101100 0 8))))
        (combinator ((combinator_ident ProducerInfo)))
        (combinator_fields
         ((NamedField (field_ident utime) (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident mc_blk_ref)
           (field_expr ((NamedRef ExtBlkRef) () ())))
          (NamedField (field_ident state_proof)
           (field_expr
            ((CellRef
              (Expr
               (Exprs (((NamedRef MERKLE_PROOF) () ()) ((NamedRef Block) () ())))))
             () ())))
          (NamedField (field_ident prod_proof)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef MERKLE_PROOF) () ()) ((NamedRef ShardState) () ())))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor no_blk_gen))
        (combinator ((combinator_ident ComplaintDescr)))
        (combinator_fields
         ((NamedField (field_ident from_utime)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident prod_info)
           (field_expr ((CellRef (NamedRef ProducerInfo)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor no_blk_gen_diff))
        (combinator ((combinator_ident ComplaintDescr)))
        (combinator_fields
         ((NamedField (field_ident prod_info_old)
           (field_expr ((CellRef (NamedRef ProducerInfo)) () ())))
          (NamedField (field_ident prod_info_new)
           (field_expr ((CellRef (NamedRef ProducerInfo)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (validator_complaint (00111101 0 8))))
        (combinator ((combinator_ident ValidatorComplaint)))
        (combinator_fields
         ((NamedField (field_ident validator_pubkey)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident description)
           (field_expr ((CellRef (NamedRef ComplaintDescr)) () ())))
          (NamedField (field_ident created_at)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident severity)
           (field_expr ((NamedRef uint8) () ())))
          (NamedField (field_ident reward_addr)
           (field_expr ((NamedRef uint256) () ())))
          (NamedField (field_ident paid) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident suggested_fine)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident suggested_fine_part)
           (field_expr ((NamedRef uint32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (complaint_status (10110100 0 8))))
        (combinator ((combinator_ident ValidatorComplaintStatus)))
        (combinator_fields
         ((NamedField (field_ident complaint)
           (field_expr ((CellRef (NamedRef ValidatorComplaint)) () ())))
          (NamedField (field_ident voters)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 16) () ())
                ((NamedRef True) () ()))))
             () ())))
          (NamedField (field_ident vset_id)
           (field_expr ((NamedRef uint256) () ())))
          (NamedField (field_ident weight_remaining)
           (field_expr ((NamedRef int64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vm_stk_null (00000000 0 8))))
        (combinator ((combinator_ident VmStackValue))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (vm_stk_tinyint (10000000 0 8))))
        (combinator ((combinator_ident VmStackValue)))
        (combinator_fields
         ((NamedField (field_ident value) (field_expr ((NamedRef int64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vm_stk_int (100000000 0 9))))
        (combinator ((combinator_ident VmStackValue)))
        (combinator_fields
         ((NamedField (field_ident value) (field_expr ((NamedRef int257) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (vm_stk_nan (1111111101000000 0 16))))
        (combinator ((combinator_ident VmStackValue))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vm_stk_cell (11000000 0 8))))
        (combinator ((combinator_ident VmStackValue)))
        (combinator_fields
         ((NamedField (field_ident cell)
           (field_expr ((CellRef (NamedRef Cell)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident VmCellSlice)))
        (combinator_fields
         ((NamedField (field_ident cell)
           (field_expr ((CellRef (NamedRef Cell)) () ())))
          (NamedField (field_ident st_bits)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 10) () ())))) () ())))
          (NamedField (field_ident end_bits)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 10) () ())))) () ())))
          (ExprField
           (Leq
            ((Exprs (((NamedRef st_bits) () ())))
             (Exprs (((NamedRef end_bits) () ()))))))
          (NamedField (field_ident st_ref)
           (field_expr ((Expr (Exprs ((NatLeq () ()) ((Int 4) () ())))) () ())))
          (NamedField (field_ident end_ref)
           (field_expr ((Expr (Exprs ((NatLeq () ()) ((Int 4) () ())))) () ())))
          (ExprField
           (Leq
            ((Exprs (((NamedRef st_ref) () ())))
             (Exprs (((NamedRef end_ref) () ()))))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (vm_stk_slice (00100000 0 8))))
        (combinator ((combinator_ident VmStackValue)))
        (combinator_fields
         ((AnonymousField (field_expr ((NamedRef VmCellSlice) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (vm_stk_builder (10100000 0 8))))
        (combinator ((combinator_ident VmStackValue)))
        (combinator_fields
         ((NamedField (field_ident cell)
           (field_expr ((CellRef (NamedRef Cell)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vm_stk_cont (01100000 0 8))))
        (combinator ((combinator_ident VmStackValue)))
        (combinator_fields
         ((NamedField (field_ident cont) (field_expr ((NamedRef VmCont) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_tupref_nil))
        (combinator ((combinator_ident VmTupleRef))) (combinator_fields ())
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_tupref_single))
        (combinator ((combinator_ident VmTupleRef)))
        (combinator_fields
         ((NamedField (field_ident entry)
           (field_expr ((CellRef (NamedRef VmStackValue)) () ())))))
        (combinator_params ((Int 1))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_tupref_any))
        (combinator ((combinator_ident VmTupleRef)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident ref)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef VmTuple) () ())
                 ((Expr
                   (Plus
                    ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 2) () ()))))))
                  () ())))))
             () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 2) () ())))))))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_tuple_nil))
        (combinator ((combinator_ident VmTuple))) (combinator_fields ())
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_tuple_tcons))
        (combinator ((combinator_ident VmTuple)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident head)
           (field_expr
            ((Expr (Exprs (((NamedRef VmTupleRef) () ()) ((NamedRef n) () ()))))
             () ())))
          (NamedField (field_ident tail)
           (field_expr ((CellRef (NamedRef VmStackValue)) () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ())))))))))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (vm_stk_tuple (11100000 0 8))))
        (combinator ((combinator_ident VmStackValue)))
        (combinator_fields
         ((NamedField (field_ident len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 16) () ())))) () ())))
          (NamedField (field_ident data)
           (field_expr
            ((Expr (Exprs (((NamedRef VmTuple) () ()) ((NamedRef len) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false) (combinator_constructor (Constructor vm_stack))
        (combinator ((combinator_ident VmStack)))
        (combinator_fields
         ((NamedField (field_ident depth)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 24) () ())))) () ())))
          (NamedField (field_ident stack)
           (field_expr
            ((Expr
              (Exprs (((NamedRef VmStackList) () ()) ((NamedRef depth) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_stk_cons))
        (combinator ((combinator_ident VmStackList)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident rest)
           (field_expr
            ((CellRef
              (Expr
               (Exprs (((NamedRef VmStackList) () ()) ((NamedRef n) () ())))))
             () ())))
          (NamedField (field_ident tos)
           (field_expr ((NamedRef VmStackValue) () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ())))))))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_stk_nil))
        (combinator ((combinator_ident VmStackList))) (combinator_fields ())
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident VmSaveList)))
        (combinator_fields
         ((NamedField (field_ident cregs)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 4) () ())
                ((NamedRef VmStackValue) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor gas_limits))
        (combinator ((combinator_ident VmGasLimits)))
        (combinator_fields
         ((NamedField (field_ident remaining)
           (field_expr ((NamedRef int64) () ())))
          (AnonymousField
           (field_expr
            ((CellRef
              (AnonymousConstr
               ((NamedField (field_ident max_limit)
                 (field_expr ((NamedRef int64) () ())))
                (NamedField (field_ident cur_limit)
                 (field_expr ((NamedRef int64) () ())))
                (NamedField (field_ident credit)
                 (field_expr ((NamedRef int64) () ()))))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident VmLibraries)))
        (combinator_fields
         ((NamedField (field_ident libraries)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef HashmapE) () ()) ((Int 256) () ())
                ((CellRef (NamedRef Cell)) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor vm_ctl_data))
        (combinator ((combinator_ident VmControlData)))
        (combinator_fields
         ((NamedField (field_ident nargs)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef uint13) () ()))))
             () ())))
          (NamedField (field_ident stack)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef VmStack) () ()))))
             () ())))
          (NamedField (field_ident save)
           (field_expr ((NamedRef VmSaveList) () ())))
          (NamedField (field_ident cp)
           (field_expr
            ((Expr (Exprs (((NamedRef Maybe) () ()) ((NamedRef int16) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_std (00 0 2))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident cdata)
           (field_expr ((NamedRef VmControlData) () ())))
          (NamedField (field_ident code)
           (field_expr ((NamedRef VmCellSlice) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_envelope (10 0 2))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident cdata)
           (field_expr ((NamedRef VmControlData) () ())))
          (NamedField (field_ident next)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_quit (0001 0 4))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident exit_code)
           (field_expr ((NamedRef int32) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_quit_exc (1001 0 4))))
        (combinator ((combinator_ident VmCont))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_repeat (00101 0 5))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident count) (field_expr ((NamedRef uint63) () ())))
          (NamedField (field_ident body)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))
          (NamedField (field_ident after)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_until (000011 0 6))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident body)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))
          (NamedField (field_ident after)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_again (100011 0 6))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident body)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (vmc_while_cond (010011 0 6))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident cond)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))
          (NamedField (field_ident body)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))
          (NamedField (field_ident after)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (vmc_while_body (110011 0 6))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident cond)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))
          (NamedField (field_ident body)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))
          (NamedField (field_ident after)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (vmc_pushint (1111 0 4))))
        (combinator ((combinator_ident VmCont)))
        (combinator_fields
         ((NamedField (field_ident value) (field_expr ((NamedRef int32) () ())))
          (NamedField (field_ident next)
           (field_expr ((CellRef (NamedRef VmCont)) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (AnonymousConstructor ()))
        (combinator ((combinator_ident DNS_RecordSet)))
        (combinator_fields
         ((ExprField
           (Exprs
            (((Expr
               (Exprs
                (((NamedRef HashmapE) () ()) ((Int 256) () ())
                 ((NamedRef DNSRecord) () ()))))
              () ()))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chunk_ref))
        (combinator ((combinator_ident TextChunkRef)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident ref)
           (field_expr
            ((CellRef
              (Expr
               (Exprs
                (((NamedRef TextChunks) () ())
                 ((Expr
                   (Plus
                    ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ()))))))
                  () ())))))
             () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ())))))))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chunk_ref_empty))
        (combinator ((combinator_ident TextChunkRef))) (combinator_fields ())
        (combinator_params ((Int 0))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor text_chunk))
        (combinator ((combinator_ident TextChunks)))
        (combinator_fields
         ((ImplicitField (field_ident n) (field_type (Uint 32)))
          (NamedField (field_ident len)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (NamedField (field_ident data)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef bits) () ())
                ((Expr
                  (Times
                   ((Exprs (((NamedRef len) () ()))) (Exprs (((Int 8) () ()))))))
                 () ()))))
             () ())))
          (NamedField (field_ident next)
           (field_expr
            ((Expr
              (Exprs (((NamedRef TextChunkRef) () ()) ((NamedRef n) () ()))))
             () ())))))
        (combinator_params
         ((Expr
           (Plus ((Exprs (((NamedRef n) () ()))) (Exprs (((Int 1) () ())))))))))
       ((combinator_exotic false)
        (combinator_constructor (Constructor text_chunk_empty))
        (combinator ((combinator_ident TextChunks))) (combinator_fields ())
        (combinator_params ((Int 0))))
       ((combinator_exotic false) (combinator_constructor (Constructor text))
        (combinator ((combinator_ident Text)))
        (combinator_fields
         ((NamedField (field_ident chunks)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (NamedField (field_ident rest)
           (field_expr
            ((Expr
              (Exprs (((NamedRef TextChunks) () ()) ((NamedRef chunks) () ()))))
             () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (dns_text (0101101101111000 0 16))))
        (combinator ((combinator_ident DNSRecord)))
        (combinator_fields
         ((AnonymousField (field_expr ((NamedRef Text) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (dns_next_resolver (1100100101011101 0 16))))
        (combinator ((combinator_ident DNSRecord)))
        (combinator_fields
         ((NamedField (field_ident resolver)
           (field_expr ((NamedRef MsgAddressInt) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (dns_adnl_address (1000000010110101 0 16))))
        (combinator ((combinator_ident DNSRecord)))
        (combinator_fields
         ((NamedField (field_ident adnl_addr)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (ExprField
           (Leq ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident proto_list)
           (field_expr ((NamedRef flags) ((Int 0)) ((NamedRef ProtoList)))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (proto_list_nil (0 0 1))))
        (combinator ((combinator_ident ProtoList))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (proto_list_next (1 0 1))))
        (combinator ((combinator_ident ProtoList)))
        (combinator_fields
         ((NamedField (field_ident head)
           (field_expr ((NamedRef Protocol) () ())))
          (NamedField (field_ident tail)
           (field_expr ((NamedRef ProtoList) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (proto_http (0010101000010010 0 16))))
        (combinator ((combinator_ident Protocol))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (dns_smc_address (1100101111111001 0 16))))
        (combinator ((combinator_ident DNSRecord)))
        (combinator_fields
         ((NamedField (field_ident smc_addr)
           (field_expr ((NamedRef MsgAddressInt) () ())))
          (NamedField (field_ident flags)
           (field_expr ((Expr (Exprs ((UIntN () ()) ((Int 8) () ())))) () ())))
          (ExprField
           (Leq ((Exprs (((NamedRef flags) () ()))) (Exprs (((Int 1) () ()))))))
          (NamedField (field_ident cap_list)
           (field_expr ((NamedRef flags) ((Int 0)) ((NamedRef SmcCapList)))))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (cap_list_nil (0 0 1))))
        (combinator ((combinator_ident SmcCapList))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (cap_list_next (1 0 1))))
        (combinator ((combinator_ident SmcCapList)))
        (combinator_fields
         ((NamedField (field_ident head)
           (field_expr ((NamedRef SmcCapability) () ())))
          (NamedField (field_ident tail)
           (field_expr ((NamedRef SmcCapList) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (cap_method_seqno (1000111011001010 0 16))))
        (combinator ((combinator_ident SmcCapability))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (cap_method_pubkey (0010111110001110 0 16))))
        (combinator ((combinator_ident SmcCapability))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (cap_is_wallet (1110111010000100 0 16))))
        (combinator ((combinator_ident SmcCapability))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (cap_name (11111111 0 8))))
        (combinator ((combinator_ident SmcCapability)))
        (combinator_fields
         ((NamedField (field_ident name) (field_expr ((NamedRef Text) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chan_config))
        (combinator ((combinator_ident ChanConfig)))
        (combinator_fields
         ((NamedField (field_ident init_timeout)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident close_timeout)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident a_key)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident b_key)
           (field_expr ((NamedRef bits256) () ())))
          (NamedField (field_ident a_addr)
           (field_expr ((CellRef (NamedRef MsgAddressInt)) () ())))
          (NamedField (field_ident b_addr)
           (field_expr ((CellRef (NamedRef MsgAddressInt)) () ())))
          (NamedField (field_ident channel_id)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident min_A_extra)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (chan_state_init (000 0 3))))
        (combinator ((combinator_ident ChanState)))
        (combinator_fields
         ((NamedField (field_ident signed_A)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident signed_B)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident min_A) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident min_B) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident expire_at)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident A) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident B) (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (TaggedConstructor (chan_state_close (100 0 3))))
        (combinator ((combinator_ident ChanState)))
        (combinator_fields
         ((NamedField (field_ident signed_A)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident signed_B)
           (field_expr ((NamedRef Bool) () ())))
          (NamedField (field_ident promise_A)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident promise_B)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident expire_at)
           (field_expr ((NamedRef uint32) () ())))
          (NamedField (field_ident A) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident B) (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor (chan_state_payout (010 0 3))))
        (combinator ((combinator_ident ChanState)))
        (combinator_fields
         ((NamedField (field_ident A) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident B) (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chan_promise))
        (combinator ((combinator_ident ChanPromise)))
        (combinator_fields
         ((NamedField (field_ident channel_id)
           (field_expr ((NamedRef uint64) () ())))
          (NamedField (field_ident promise_A)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident promise_B)
           (field_expr ((NamedRef Grams) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chan_signed_promise))
        (combinator ((combinator_ident ChanSignedPromise)))
        (combinator_fields
         ((NamedField (field_ident sig)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((CellRef (NamedRef bits512)) () ()))))
             () ())))
          (NamedField (field_ident promise)
           (field_expr ((NamedRef ChanPromise) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (chan_msg_init (01000100000111101000110011100100 0 32))))
        (combinator ((combinator_ident ChanMsg)))
        (combinator_fields
         ((NamedField (field_ident inc_A) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident inc_B) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident min_A) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident min_B) (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident channel_id)
           (field_expr ((NamedRef uint64) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (chan_msg_close (11000001100001110101000101001111 0 32))))
        (combinator ((combinator_ident ChanMsg)))
        (combinator_fields
         ((NamedField (field_ident extra_A)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident extra_B)
           (field_expr ((NamedRef Grams) () ())))
          (NamedField (field_ident promise)
           (field_expr ((NamedRef ChanSignedPromise) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (chan_msg_timeout (00010100010100011110010011000010 0 32))))
        (combinator ((combinator_ident ChanMsg))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (chan_msg_payout (00001000000111100111111111101100 0 32))))
        (combinator ((combinator_ident ChanMsg))) (combinator_fields ())
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chan_signed_msg))
        (combinator ((combinator_ident ChanSignedMsg)))
        (combinator_fields
         ((NamedField (field_ident sig_A)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((CellRef (NamedRef bits512)) () ()))))
             () ())))
          (NamedField (field_ident sig_B)
           (field_expr
            ((Expr
              (Exprs
               (((NamedRef Maybe) () ()) ((CellRef (NamedRef bits512)) () ()))))
             () ())))
          (NamedField (field_ident msg) (field_expr ((NamedRef ChanMsg) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor
         (TaggedConstructor
          (chan_op_cmd (10001011000111000001010010001001 0 32))))
        (combinator ((combinator_ident ChanOp)))
        (combinator_fields
         ((NamedField (field_ident msg)
           (field_expr ((NamedRef ChanSignedMsg) () ())))))
        (combinator_params ()))
       ((combinator_exotic false)
        (combinator_constructor (Constructor chan_data))
        (combinator ((combinator_ident ChanData)))
        (combinator_fields
         ((NamedField (field_ident config)
           (field_expr ((CellRef (NamedRef ChanConfig)) () ())))
          (NamedField (field_ident state)
           (field_expr ((CellRef (NamedRef ChanState)) () ())))))
        (combinator_params ()))))) |}]
