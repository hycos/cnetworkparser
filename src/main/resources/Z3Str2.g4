grammar Z3Str2;


Number: '-'? ([0-9]|[1-9][0-9]+);
Varname: [a-zA-Z]+[a-zA-Z_A0-9]+;
String : '"' (~('"') | '\\\"')* '"' | '\'' (~('\'') | '\\\'')* '\'' ;



ParenthesisOpen: '(';
ParenthesisClose: ')';

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

operation: ParenthesisOpen (binoperation | booloperation | stroperation | regexoperation | numoperation) ParenthesisClose;

booloperation: boolop param+;

stroperation: strop param+;

regexoperation: regexop param+;

numoperation: numop param+;

binoperation: binop param+;

boolop: '=' | '<' | '<=' | '>=' | '!=' | 'not' | 'and' | 'or' | 'ite' | 'RegexIn' | 'EndsWith' | 'StartsWith' | 'Contains';

numop: '-' | '+' | 'div';

strop: 'SubString' | 'Concat' | 'IndexOf';

regexop: 'RegexStar' | 'RegexUnion' | 'RegexCharRange' | 'Str2Reg' | 'RegexConcat';

binop: 'Length';

varname: Varname;

vartype: 'String' | 'Int' | 'Bool';

end: ParenthesisOpen 'check-sat' ParenthesisClose ( ParenthesisOpen 'get-model' ParenthesisClose)?;

WS  :  [ \t\r\n]+ -> skip
    ;

Comment
  : '#' ~( '\r' | '\n' )* -> skip
  ;
