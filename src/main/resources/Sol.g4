grammar Sol;

// Lexer rules
// fun "org.snt.wrapper.PHPWrapper.mysql_real_escape_string(Ljava.lang.String;)Ljava/lang/String;" esc;
// java bytecode signature in HR format
Bytecodesig : '"'[a-z]+[0-9a-zA-Z_.]*'('[A-Za-z;/0-9]+')'[A-Za-z;/0-9]+ ';"';

Number: '-'?([0-9]|[1-9][0-9]+);
String : '"' (~('"') | '\\\"')* '"' | '\'' (~('\'') | '\\\'')* '\'' ;
Boollit : 'true' | 'false';

Stmtend: ';';

// Parser rules
s: (decl)* (link | constraint | assignment)* EOF;

string: String;
number: Number;
boollit: Boollit;

Identifier : [a-zA-Z][a-zA-Z0-9_]*;

literal: boollit | string | number;

tmodeltype: Identifier;

term: literal | funccall | videntifier | util;

link: lnk '(' videntifier ',' tmodeltype ')' Stmtend;

constraint: boolop '(' term ',' term ')' Stmtend;

assignment: ass '(' videntifier ',' literal ')' Stmtend;

videntifier: Identifier;

decl: vardecl | funcdecl;
vardecl: 'var' vartype videntifier Stmtend;
funcdecl: 'fun' bytecodesig videntifier Stmtend;

op: 'add' |
    'sub' |
    'substr' |
    'indexof' |
    'valueof' |
    'tostr' |
    'tolower' |
    'toupper' |
    'concat' |
    'trim' |
    'len' |
    'replace' |
    'strinv' | 'search' | boolop;

esc: 'apache_eschtml' |
    'apache_ueschtml' |
    'apache_escxml10' |
    'apache_escxml11' |
    'apache_escjson' |
    'esapi_escldap' |
    'esapi_escdn' |
    'esapi_eschtml' |
    'esapi_eschtmlattr' |
    'esapi_escxml' |
    'esapi_escxmlattr' |
    'esapi_escxpath';

trans: 'tolit';

boolop: '>' |
        '<' |
        '>=' |
        '<=' |
        '==' |
        '!=' |
        '~~' |
        '!~' |
        'matches' |
        'startswith' |
        'endswith' |
        'emtpy' |
        'contains' |
        'not' |
        'or' |
        'and' |
        'xor'|
        'div';

ass: '=';
lnk: '&';

userdef: Identifier;

util : trans '(' string ')';

funccall : ( op | esc | userdef ) '(' parlist ')';

parlist: parameter (',' parameter)*;

parameter: videntifier | literal | funccall | util;

vartype : 'string' | 'bool' | 'int';
bytecodesig: Bytecodesig;

WS  :  [ \t\r\n]+ -> skip
    ;

Comment
  : '#' ~( '\r' | '\n' )* -> skip
  ;
