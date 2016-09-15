grammar Z3Str2;


Number: '-'?([0-9]|[1-9][0-9]+);
Varname: [a-zA-Z]+[a-zA-Z_A0-9]+;
String : '"' (~('"') | '\\\"')* '"' | '\'' (~('\'') | '\\\'')* '\'' ;



ParenthesisOpen: '(';
ParenthesisClose: ')';

// Parser rules
s: decl (assertion)* (end)? EOF;


decl: (vardecl | funcdecl)*;


boollit: 'true' | 'false';

strlit: String;

number: Number;

assertion: ParenthesisOpen 'assert' ( operation | varname ) ParenthesisClose;

vardecl: ParenthesisOpen 'declare-variable' varname vartype ParenthesisClose;

funcdecl: ParenthesisOpen 'declare-fun' varname ParenthesisOpen ParenthesisClose vartype ParenthesisClose;

operation: ParenthesisOpen (booloperation | stroperation | regexoperation) ParenthesisClose;

booloperation: boolop param+;

stroperation: strop param+;

regexoperation: regexop param+;

param: (boollit | strlit | varname | operation);

boolop: '=' | '<' | '<=' | '>=' | '!=' | 'not' | 'and' | 'or' | 'RegexIn' | 'EndsWith' | 'StartsWith' | 'Contains';

strop: 'SubString' | 'Concat' | 'IndexOf';

regexop: 'RegexStar' | 'RegexUnion' | 'RegexCharRange' | 'Str2Reg' | 'RegexConcat';

varname: Varname;


vartype: 'String' | 'Int' | 'Bool';

end: ParenthesisOpen 'check-sat' ParenthesisClose ParenthesisOpen 'get-model' ParenthesisClose;


WS  :  [ \t\r\n]+ -> skip
    ;

Comment
  : '#' ~( '\r' | '\n' )* -> skip
  ;
