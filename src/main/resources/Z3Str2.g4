grammar Z3Str2;


StringType: [Ss][Tt][Rr][Ii][Nn][Gg];
IntType: [Ii][Nn][Tt];
BoolType: [Bb][Oo][Oo][Ll];

Number: '-'? ([0-9]|[1-9][0-9]+);
Varname: [a-zA-Z][a-zA-Z_0-9]*;
ParenthesisOpen: '(';
ParenthesisClose: ')';


String : StringLiteral;

fragment
StringLiteral:	'"' StringCharacters? '"';

fragment
StringCharacters: StringCharacter+;

fragment
StringCharacter: ~["\\] | EscapeSequence;

fragment
EscapeSequence:	'\\' [btnfr"'\\];


// Parser rules
s: decl (assertion)* (end)? EOF;


decl: (vardecl | funcdecl)*;

param: (number | boollit | strlit | varname | operation);

number: Number;

boollit: 'true' | 'false';

strlit: String;

assertion: ParenthesisOpen 'assert' ( operation | varname ) ParenthesisClose;

vardecl: ParenthesisOpen 'declare-variable' varname vartype ParenthesisClose;

funcdecl: ParenthesisOpen 'declare-fun' varname ParenthesisOpen ParenthesisClose vartype ParenthesisClose;

operation: ParenthesisOpen (binoperation | booloperation | stroperation |
regexoperation | numoperation | copoperation) ParenthesisClose;

booloperation: boolop param+;

stroperation: strop param+;

regexoperation: regexop param+;

numoperation: numop param+;

binoperation: binop param+;

copoperation: cop param+;

boolop: '='
    | '<'
    | '>'
    | '<='
    | '>='
    | '!='
    | 'not'
    | 'and'
    | 'or'
    | 'implies'
    | 'RegexIn'
    | 'EndsWith'
    | 'StartsWith'
    | 'Contains';

numop: '-' | '+' | 'div';

strop: 'Substring' | 'Concat' | 'Indexof' | 'LastIndexof' | 'Replace';

cop: 'ite';

regexop: 'RegexStar' | 'RegexUnion' | 'RegexCharRange' | 'Str2Reg' | 'RegexConcat';

binop: 'Length';

varname: Varname;

vartype: StringType | IntType | BoolType;


end: ParenthesisOpen 'check-sat' ParenthesisClose ( ParenthesisOpen 'get-model' ParenthesisClose)?;

WS  :  [ \t\r\n]+ -> skip
    ;

Comment
  : '#' ~( '\r' | '\n' )* -> skip
  ;
