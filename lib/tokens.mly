%token LET INTERFACE IMPL STRUCT ENUM UNION FN IF ELSE RETURN VAL CASE
%token EQUALS
%token <string> IDENT
%token <string> STRING
%token EOF
%token LBRACE LPAREN
%token RBRACE RPAREN
%token COMMA
%token COLON RARROW SEMICOLON
%token TILDE DOT
%token <Z.t> INT
%token <bool> BOOL

%%
