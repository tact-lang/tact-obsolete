%token AS LET INTERFACE IMPL STRUCT ENUM UNION FN IF ELSE RETURN VAL CASE SWITCH
%token EQUALS
%token <string> IDENT
%token <string> STRING
%token EOF
%token DOUBLEDOT 
%token LBRACE LPAREN LBRACKET
%token RBRACE RPAREN RBRACKET
%token COMMA
%token COLON RARROW REARROW SEMICOLON
%token TILDE DOT
%token <Z.t> INT
%token <bool> BOOL

%%
