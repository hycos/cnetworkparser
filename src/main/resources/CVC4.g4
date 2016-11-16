grammar CVC4;

//http://cvc4.cs.nyu.edu/wiki/Strings

Number: '-'?([0-9]|[1-9][0-9]+);
Varname: [a-zA-Z]+[a-z_A-ZA0-9]+;
String : StringLiteral;

fragment
StringLiteral:	'"' StringCharacters? '"';

fragment
StringCharacters: StringCharacter+;

fragment
StringCharacter: ~["\\] | EscapeSequence;

fragment
EscapeSequence:	'\\' [btnfr"'\\];



ParenthesisOpen: '(';
ParenthesisClose: ')';

// Parser rules
s: preamble? decl (assertion)* (end)? EOF;



preamble: ParenthesisOpen 'set-logic' 'QF_S' ParenthesisClose
          ParenthesisOpen 'set-option' ':' 'produce-models' 'true' ParenthesisClose;

decl: (vardecl | funcdecl)*;

allchar: 're.allchar';

boollit: 'true' | 'false';

strlit: String;

number: Number;

assertion: ParenthesisOpen 'assert' operation ParenthesisClose;

vardecl: ParenthesisOpen 'declare-variable' varname vartype ParenthesisClose;

funcdecl: ParenthesisOpen 'declare-fun' varname ParenthesisOpen ParenthesisClose vartype ParenthesisClose;

operation: ParenthesisOpen (booloperation | stroperation | regexoperation | numop) ParenthesisClose;

booloperation: boolop param+;

stroperation: strop param+;

regexoperation: regexop param+;

numoperation: numop param+;

param: (boollit | strlit | varname | allchar | operation);

numop: '+' | '-' | 'div';

boolop: '=' | '<' | '>' | '<=' | '>=' | '!=' | 'not' | 'and' | 'or' | 'str.in.re' | 'str.suffixof' | 'str.prefixof' | 'str.contains';

strop: 'str.substr' | 'str++' | 'str.indexof';

regexop: 'str.to.re' | 're.*' | 're.++' | 're.union' | 're.opt' | 're.range';

varname: Varname;


vartype: 'String' | 'Int' | 'Bool';

end: ParenthesisOpen 'check-sat' ParenthesisClose ParenthesisOpen 'get-model' ParenthesisClose;


WS  :  [ \t\r\n]+ -> skip
    ;

Comment
  : '#' ~( '\r' | '\n' )* -> skip
  ;
