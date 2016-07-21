/*
See also MyOtherBrain-EBNF.txt
*/

grammar MyOtherBrain;

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
