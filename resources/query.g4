/*
 query.g4

 Describes grammar for query DSL
 Syntax summary:
 - variable: ?parameter or ?[parameter] are the same
 - query: <section>(<query>)
 Example:
 host(google.com);
 path(/?[root]/?[subroot]);

 Note: actually, the query language places no restrictions on sections,
 so this query are also valid:
 my-custom-section(?v1/?v2/v2)
*/
grammar query;

WS             : [ \n\r\t]+ -> skip;

SECTION_END    : '(';
SECTION_START  : ')';
VARIABLE_START : '[';
VARIABLE_END   : ']';
VAR_SIGN       : '?';
PATH_SEPARATOR : [\\/];
QUERY_SEPARATOR: [=&];

// Set in the rule below excludes all character used in other lexer rules.
// Be sure to include them here when defining new rules as ANTLR does not support references inside negations
LITERAL        : ~[ \t\r\n?;()[\]\\/=&]+;
DELIMITER      : ';';


name     : LITERAL;
variable : VAR_SIGN (name | (VARIABLE_START name VARIABLE_END));

literal  : (LITERAL | PATH_SEPARATOR | QUERY_SEPARATOR)+;
pattern  : (literal | variable)+;
section  : LITERAL;
subquery : section SECTION_END pattern? SECTION_START;
query    : (subquery (DELIMITER subquery)* DELIMITER? EOF)
         | EOF;
