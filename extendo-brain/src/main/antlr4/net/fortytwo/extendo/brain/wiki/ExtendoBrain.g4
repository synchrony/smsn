grammar ExtendoBrain;

options
{
    language = Java;
}

document: line | document NEWLINE line ;

line: WS? (atom | property)? ;
atom: BULLET WS (ID WS FLOAT | ID | FLOAT);
property: alias | sharability | weight ;

alias: '@alias' WS FLOAT ;
sharability: '@sharability' WS FLOAT ;
weight: '@weight' WS FLOAT ;

NEWLINE : [\r\n]+ ;
WS      : [ \t]+ ;
BULLET  : ~[ \r\n] ;
ID      : ':' [a-zA-Z0-9_-]+ ':' ;
//fragment VALUE   : (~[\n\r])+ ;
FLOAT   : [0-9]+ ('.' [0-9]+)? ;
ALIAS   : ~([ \n\r])+ ;

/*

view     ::= line | view EOL line
line     ::= WS? (atom | comment | property)?
atom     ::= BULLET WS (ID (WS value)? | value)
comment  ::= "@{" TEXT
property ::= "@alias" WS TEXT
             | ("@sharability" | "@weight") WS DEGREE
value    ::= TEXT /* also note:
                     1) line breaks allowed within {{{ }}}
                     2) [...] is special at the end of a line */
EOL      ::= [#xA#xD]     /* line feed or carriage return */
WS       ::= [#x9#x20]    /* tab or space */
BULLET   ::= [#x21-x7E#xA1-#xFF] /* visible characters */
ID       ::= ":" [-_0-9A-Za-z]+ ":"
TEXT     ::= [^#xA#xD]+
DEGREE   ::= "1" ("." "0"+)? | "0" ("." [0-9]+)?

*/