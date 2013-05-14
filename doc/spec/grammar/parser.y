/* Copyright 2012 Microsoft Corporation, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*/

%pure_parser

%parse-param { yyscan_t scanner }
%lex-param { yyscan_t scanner }


/* token structure. The memory for Id and String is kept in a list and deallocated after parsing (in 'doneScanState') */
%union {  
  const char*   Id;      /* used for operators OP too */
  const char*   String;  /* 'modified' UTF-8 string (\0 chars are encoded as \xC0\x80) */
  double        Float;
  unsigned long Nat;
  unsigned int  Char;
}

%{
#include <stdio.h>
#define yylex        mylex
typedef void*        yyscan_t;

void yyerror(YYLTYPE* loc, yyscan_t scanner, char* s, ...);
int  mylex( YYSTYPE* val, YYLTYPE* loc, yyscan_t scanner );

typedef int bool;
#define true (1==1)
#define false (!true)

void printDecl( const char* sort, const char* name );
%}


%token <Id>     ID CONID OP IDOP QID  QCONID QIDOP WILDCARD '(' ')' '[' ']'
%token <Nat>    NAT 
%token <Float>  FLOAT 
%token <String> STRING 
%token <Char>   CHAR

%token IF THEN ELSE ELIF
%token MATCH 
%token RARROW 

%token FUN FUNCTION VAL VAR
%token TYPE COTYPE RECTYPE STRUCT
%token ALIAS CON
%token FORALL EXISTS SOME
%token WITH 

%token IMPORT AS MODULE 
%token PUBLIC PRIVATE ABSTRACT
%token EXTERNAL
%token INFIX INFIXL INFIXR 

%token LEX_WHITE LEX_COMMENT 
%token SEMI
%token LE ASSIGN DCOLON EXTEND
%token RETURN 

%token YIELD REC TRY IFACE INST

%token INLINE INCLUDE ID_CS ID_JS ID_FILE

%type <Id>  varid conid qvarid qconid op  
%type <Id>  identifier qidentifier qoperator qconstructor
%type <Id>  funid typeid modulepath binder 
%type <Id>  valdecl fundecl aliasdecl typedecl externdecl puredecl 

%%


/* ---------------------------------------------------------
-- Program
----------------------------------------------------------*/
program     : semis visibility MODULE modulepath moduledecl  { printDecl("module",$4); }
            | moduledecl                                     { printDecl("module","main"); }
            ;

moduledecl  : '{' semis modulebody '}' semis
            | semis modulebody
            ;

modulebody  : importdecl semis1 modulebody
            | declarations
            ;

importdecl  : visibility IMPORT modulepath
            | visibility IMPORT modulepath '=' modulepath 
            ;

modulepath  : varid                       { $$ = $1 }
            | qvarid                      { $$ = $1 }
            ;

visibility  : PUBLIC
            | PRIVATE
            | /* empty */
            ;
                
semis1      : semis semi
            ;

semis       : semis1
            | /* empty */   
            ;

semi        : ';' 
            | SEMI
            ;

/* ---------------------------------------------------------
-- Top level declarations
----------------------------------------------------------*/

declarations: fixitydecl semis1 declarations
            | topdecls
            ;

fixitydecl  : visibility fixity oplist1 
            ;

fixity      : INFIX NAT 
            | INFIXR NAT 
            | INFIXL NAT
            ;

oplist1     : oplist1 ',' identifier
            | identifier
            ;


topdecls    : topdecls1 
            | /* empty */
            ;

topdecls1   : topdecls1 topdecl semis1
            | topdecl semis1
            /* error recovery */
            | topdecls1 error semis1
            | error semis1                                    { yyerror(&@1,scanner,"skipped top-level declaration");  }
            ; 

topdecl     : visibility puredecl                             { printDecl("value",$2); }
            | visibility aliasdecl                            { printDecl("alias",$2); }
            | visibility typedecl                             { printDecl("type",$2); }
            | ABSTRACT typedecl                               { printDecl("type",$2); }
            | visibility externdecl                           { printDecl("external",$2); }
            ;


/* ---------------------------------------------------------
-- External declarations
----------------------------------------------------------*/

externdecl  : EXTERNAL externinline funid ':' typesig externbody   { $$ = $3; }
            | EXTERNAL externinline funid '(' parameters ')' annotres externbody { $$ = $3; } 
            | EXTERNAL INCLUDE externincbody                       { $$ = "<external include>"; }
            ;

externbody  : '=' externstat
            | '{' semis externstats1 '}'
            ;

externstats1: externstats1 externstat semis1
            | externstat semis1
            ;

externstat  : externtarget externinline STRING
            ;


externincbody: '=' externinc
            | '{' semis externincs1 '}'
            ;

externincs1 : externincs1 externinc semis1
            | externinc semis1
            ;

externinc   : externtarget externfile STRING
            ;                        

externtarget: ID_CS
            | ID_JS
            | /* empty */
            ;            

externfile  : ID_FILE
            | /* empty */
            ;

externinline: INLINE
            | /* empty */
            ;



/* ---------------------------------------------------------
-- Type declarations
----------------------------------------------------------*/
aliasdecl   : ALIAS typeid typeparams kannot '=' type     { $$ = $2; }
            ;

typedecl    : typesort typeid typeparams kannot  '{' semis constructors '}'     { $$ = $2; }
            | typesort typeid typeparams kannot                                 { $$ = $2; }
            | STRUCT typeid typeparams kannot  conparams                        { $$ = $2; }
            ;

typesort    : TYPE | COTYPE | RECTYPE
            ;

typeid      : '(' commas ')'      { $$ = "(,)"; }       /* tuples */
            | '[' ']'             { $$ = "[]"; }        /* lists */
            | '<' '>'             { $$ = "<>"; }        /* total effect */
            | '<' '|' '>'         { $$ = "<|>"; }       /* effect extension */
            | varid               { $$ = $1; }    
            ;

commas      : commas1
            | /* empty */
            ;

commas1     : commas ','
            ;


typeparams  : '<' tbinders '>'
            | /* empty */
            ;

constructors: constructors1 semis
            | /* empty */
            ;

constructors1: constructors1 semis1 constructor 
            | constructor
            ;

constructor : visibility con equantifier conid conparams
            | visibility con conid conparams
            ;

con         : CON 
            | /* empty */
            ; 

conparams   : '(' conpars1 ')'
            | '(' ')'
            | /* empty */
            ;

conpars1    : conpars1 ',' conpar
            | conpar
            ;

conpar      : paramid ':' paramtype
            | paramid ':' paramtype '=' expr            
            | ':' paramtype
            | ':' paramtype '=' expr
            ;

/* ---------------------------------------------------------
-- Pure Declarations
----------------------------------------------------------*/   
puredecl    : VAL valdecl                   { $$ = $2; }
            | FUN fundecl                   { $$ = $2; }
            | FUNCTION fundecl              { $$ = $2; }
            ;

valdecl     : binder '=' expr               { $$ = $1; }
            ;

binder      : identifier                    { $$ = $1; }
            | identifier ':' type           { $$ = $1; }
            ;

funid       : identifier         { $$ = $1; }
            | '[' commas ']'     { $$ = "[]"; }
            ;


fundecl     : quantifiers funid fundef block          { $$ = $2; }
            | quantifiers funid fundef '=' blockexpr  { $$ = $2; } 
            ;

fundef      : '(' parameters ')' annotres 
            ;


parameters  : parameters1
            | /* empty */
            ;

parameters1 : parameters1 ',' parameter
            | parameter
            ;

parameter   : paramid 
            | paramid ':' paramtype
            | paramid ':' paramtype '=' expr
            | paramid '=' expr
            ;

paramid     : identifier
            | WILDCARD
            ;

paramtype   : type
            | '?' type
            ;

annotres    : ':' tresult
            | /* empty */
            ;


/* ---------------------------------------------------------
-- Statements
----------------------------------------------------------*/

block       : '{' semis statements1 '}'    /* must end with an expression statement (and not a declaration) */
            ;
            
statements1 : statements1 statement semis1      
            | statement semis1
            | error semis1
            ;

statement   : decl 
            | nofunexpr
            ;

decl        : FUN fundecl     
            | FUNCTION fundecl
            | VAL localvaldecl              /* local value declaration can use a pattern binding */
            | valdecl                       /* for a local declaration the VAL keyword is optional */
            | VAR binder ASSIGN expr        /* local variable declaration */
            ;

localvaldecl: pattern '=' expr
            ;


/* ---------------------------------------------------------
-- Expressions
--
-- returnexpr is not allowed anywhere under 'expr' or 'returnexpr'
----------------------------------------------------------*/
expr        : funexpr            
            | nofunexpr
            ;  

blockexpr   : expr               /* block is interpreted specially; used in branches and functions */
            ;            

nofunexpr   : ifexpr             /* not 'function' (or 'block'); used for statement expressions */
            | matchexpr
            | opexpr
            | returnexpr           
            ;

noifexpr    : matchexpr          /* not an 'if' but includes 'block'; used for nested-if disambiguation */
            | opexpr
            | returnexpr
            | funexpr
            ;


/* keyword expressions: if, match, fun, return  */

ifexpr      : IF atom then elifs else
            ;

then        : THEN noifexpr 
            | noifexpr                 /* then keyword is optional */
            ;

else        : ELSE noifexpr
            | /* empty */
            ;

elifs       : elifs ELIF atom then 
            | /* empty */
            ;


matchexpr   : MATCH atom '{' semis matchrules '}'
            ;            

funexpr     : FUN quantifiers fundef block
            | FUNCTION quantifiers fundef block
            | block /* zero-argument function */
            ;

/* returnexpr is not allowed under expr or returnexpr */
returnexpr  : RETURN noifexpr          
            ;


/* operator expressions */
/* note: associativity and precedence is to be handled in a later compiler phase */

opexpr      : opexpr qoperator funappexpr
            | funappexpr
            ;

funappexpr  : appexpr funexprs
            ;

funexprs    : funexprs funexpr
            | /* empty */
            ;

/* dot and application expressions */

appexpr     : appexpr '(' arguments ')'       /* application */
            | appexpr '[' arguments ']'       /* index expression */
            | appexpr '.' prefix              /* dot application */
            | prefix
            ;

prefix      : qoperator prefix
            | atom
            ;


/* arguments: separated or terminated by comma */


arguments   : arguments1 
            | /* empty */
            ;

arguments1  : arguments1 ',' argument
            | argument
            ;

argument    : expr
            | identifier '=' expr                  /* named arguments */
            ;
 

/* annotated expressions: separated or terminated by comma */

aexprs      : aexprs1                              /* separated by comma */
            | /* empty */
            ;            

aexprs1     : aexprs1 ',' aexpr
            | aexpr
            ;

cexprs      : cexprs0                              /* terminated or separated by comma */
            | cexprs0 aexpr
            ;

cexprs0     : cexprs0 aexpr ','
            | /* empty */
            ;            

aexpr       : expr annot
            ;

annot       : ':' typescheme
            | /* empty */
            ;


/* atomic expressions */

atom        : qidentifier
            | qconstructor
            | literal
            | '(' aexprs ')'             /* unit, parenthesized (possibly annotated) expression, tuple expression */
            | '[' cexprs ']'             /* list expression (elements may be terminated with comma instead of separated) */
            ;


literal     : NAT | FLOAT | CHAR | STRING
            ;

/* ---------------------------------------------------------
-- Identifiers and operators
----------------------------------------------------------*/

qoperator   : '`' qidentifier '`'   { $$ = $2; }
            | '`' qconstructor '`'  { $$ = $2; }
            | op
            ;

qidentifier : qvarid 
            | QIDOP
            | identifier           
            ;

identifier  : varid 
            | IDOP
            ;

qvarid      : QID
            ;

varid       : ID
            | ID_CS           { $$ = "cs" }
            | ID_JS           { $$ = "js" }      
            | ID_FILE         { $$ = "file" }
            ; 

qconstructor: conid 
            | qconid
            ;
            
qconid      : QCONID { $$ = $1; }
conid       : CONID  { $$ = $1; }
            ;

op          : OP 
            | '>'       { $$ = ">"}
            | '<'       { $$ = "<"}
            | '|'       { $$ = "|"}
            | ASSIGN    { $$ = ":="}
            ;            


/* ---------------------------------------------------------
-- Matching
----------------------------------------------------------*/

matchrules  : matchrules1 semis
            | /* empty */
            ;

matchrules1 : matchrules1 semis1 matchrule
            | matchrule
            ;

matchrule   : patterns1 guard RARROW blockexpr
            ;            

guard       : '|' expr
            | /* empty */
            ;

patterns    : patterns1
            | /* empty */
            ;

patterns1   : patterns1 ',' pattern
            | pattern
            ;

pattern     : identifier
            | conid
            | conid '(' patargs ')'
            | '(' patterns ')'                   /* unit, parenthesized pattern, tuple pattern */
            | '[' patterns ']'                   /* list pattern */
            | pattern AS identifier              /* named pattern */
            | literal
            | WILDCARD
            ;

patargs     : patargs1
            | /* empty */
            ;

patargs1    : patargs ',' patarg
            | patarg
            ;

patarg      : identifier '=' pattern                  /* named argument */
            | pattern
            ;

/* ---------------------------------------------------------
-- Types
----------------------------------------------------------*/
tbinders    : tbinders1
            | /* empty */
            ;

tbinders1   : tbinders1 ',' tbinder
            | tbinder
            ;

tbinder     : varid kannot
            ;


/* full type */
typesig     : quantifiers tarrow sigqualifier     /* used for full type signatures in definitions */
            ;

typescheme  : quantifiers tarrow qualifier        /* used for type annotations */
            ;

type        : aquantifier tarrow qualifier        /* used for plain types (without some or exists quantifiers) */
            | tarrow qualifier
            ;

quantifiers : squantifier aquantifier
            | squantifier
            | aquantifier
            | /* empty */
            ;
            
aquantifier : FORALL '<' tbinders1 '>'
            ;

squantifier : SOME '<' tbinders1 '>'
            ;

equantifier : EXISTS '<' tbinders1 '>'
            ;


sigqualifier: WITH predicates1
            | qualifier
            ;

qualifier   : WITH '(' predicates1 ')'
            | /* empty */
            ;

predicates1 : predicates1 ',' predicate
            | predicate
            ;


predicate   : typeapp                     /* interface:  identifier '<' targuments '>' */
            ;


/* mono types */
tarrow      : tatomic RARROW tresult
            | tatomic
            ;

tresult     : tatomic tbasic                 /* effect and result type */
            | tatomic                        /* just a result type (with a default total effect) */ 
            ;

tatomic     : tbasic
            | '<' targuments1 '|' tatomic '>' /* extensible effect type */
            | '<' targuments '>'             /* fixed effect type */
            ;

tbasic      : typeapp
            | '(' tparams ')'                /* unit, parenthesis, tuple, named parameters */
            | '[' anntype ']'                /* list type */
            ; 

typeapp     : typecon
            | typecon '<' targuments '>' 
            ;

typecon     : varid | qvarid                 /* type name */
            | WILDCARD                       /* wildcard type variable */
            | '(' commas1 ')'                /* tuple constructor */
            | '[' ']'                        /* list constructor */
            | '(' RARROW ')'                 /* function constructor */ 
            ;


tparams     : tparams1
            | /* empty */
            ;

tparams1    : tparams1 ',' tparam
            | tparam
            ;

tparam      : identifier ':' anntype              /* named parameter */
            | anntype
            ;
            

targuments  : targuments1
            | /* empty */
            ;

targuments1 : targuments1 ',' anntype
            | anntype
            ;

anntype     : type kannot
            ;


/* ---------------------------------------------------------
-- Kinds
----------------------------------------------------------*/
kannot      : DCOLON kind
            | /* empty */
            ;

kind        : '(' kinds1 ')' RARROW katom  
            | katom RARROW kind
            | katom
            ;

kinds1      : kinds1 ',' kind
            | kind
            ;

katom       : conid
            ;

%%

void printDecl( const char* sort, const char* name )
{
  fprintf( stderr, "parsed %s declaration: %s\n", sort, name );
}
