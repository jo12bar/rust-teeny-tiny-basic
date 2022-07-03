# Teeny BASIC grammar

For reference, the following is the grammar for Teeny BASIC, taken from
[part 2 of the blog series](https://austinhenley.com/blog/teenytinycompiler2.html).
And modified slightly based on my own additions to the language.

Note that all alphabetic keywords are actually case-insensitive. However, **[identifers](#ident) are case-_sensitive_**. This is to appease the lords of SQL.

## _Program_

::=&nbsp;{[_Statement_](#statement)}

## _Statement_

::=&nbsp;`PRINT` ([_Expression_](#expression) | [String](#string)) [_nl_](#nl)

&nbsp;&nbsp;|&nbsp; `IF` [_Comparison_](#comparison) `THEN` [_nl_](#nl) {[_Statement_](#statement)} `ENDIF` [_nl_](#nl)

&nbsp;&nbsp;|&nbsp; `WHILE` [_Comparison_](#comparison) `REPEAT` [_nl_](#nl) {[_Statement_](#statement)} `ENDWHILE` [_nl_](#nl)

&nbsp;&nbsp;|&nbsp; `LABEL` [_Ident_](#ident) [_nl_](#nl)

&nbsp;&nbsp;|&nbsp; `GOTO` [_Ident_](#ident) [_nl_](#nl)

&nbsp;&nbsp;|&nbsp; `LET` [_Ident_](#ident) `=` [_Expression_](#expression) [_nl_](#nl)

&nbsp;&nbsp;|&nbsp; `INPUT` [_Ident_](#ident) [_nl_](#nl)

## _Comparison_

::=&nbsp;[_Expression_](#expression) ((`==` | `!=` | `>` | `>=` | `<` | `<=`) [_Expression_](#expression))+

## _Expression_

::=&nbsp;[_Local_](#local) | [_Numeric_Expression_](#numericexpression) | [_String_](#string)

## _Numeric_Expression_

::=&nbsp;[_Numeric_Product_Term_](#numeric_product_term) {(`+` | `-`) [_Numeric_Product_Term_](#numeric_product_term)}

## _Numeric_Product_Term_

::=&nbsp;[_Numeric_Unary_Term_](#numeric_unary_term) {(`*` | `/`) [_Numeric_Unary_Term_](#numeric_unary_term)}

## _Numeric_Unary_Term_

::=&nbsp;\[`+` | `-`\] [_Numeric_Primary_Term_](#numeric_primary_term)

## _Numeric_Primary_Term_

::=&nbsp;[_Number_](#number) | [_Local_](#local)

## _Local_

::=&nbsp;[_Ident_](#ident)

## _Ident_

::=&nbsp;/[a-zA-Z\_][a-za-z0-9_]\*/

## _Number_

::=&nbsp;\[ `-` \] ( /[0-9]/ )+ \[ `.` ( /[0-9]/ )+ \] \[ ( `e` | `E` ) ( `+` | `-` ) ( /[0-9]/ )+ \]

## _String_

::=&nbsp;`"` (<br>
&nbsp;&nbsp;&nbsp;&nbsp;Valid UTF-8 text, excluding backslashes, double straight quotes, or any member of [_nl_](#nl)<br>
&nbsp;&nbsp;|&nbsp;`\\`<br>
&nbsp;&nbsp;|&nbsp;`\"`<br>
&nbsp;&nbsp;|&nbsp;`\b`<br>
&nbsp;&nbsp;|&nbsp;`\f`<br>
&nbsp;&nbsp;|&nbsp;`\n`<br>
&nbsp;&nbsp;|&nbsp;`\r`<br>
&nbsp;&nbsp;|&nbsp;`\t`<br>
&nbsp;&nbsp;|&nbsp;`\u` /\[0-9\]/{4}<br>
)\* `"`

## _nl_

::=&#09;(\n | \r | \r\n | \x0B | \x0C | \u0085 | \u2028 | \u2029)+
