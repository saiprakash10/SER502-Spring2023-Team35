/* 
Mapping 
P --> Program 
K --> Block 
D --> Declaration 
CL --> Command List
C --> Multi Line Command
E --> Expression
I --> Identifier
*/
 
 
P ::= CL.

K ::= ['{'], CL, ['}'].

CL ::= C, CL.
        |single_line_commands, CL.
        |C.
        |single_line_commands.


single_line_commands ::= print.
                            |assignment_command.
                            |D.


C ::= for_loop.
        |while_loop.
        |for_range.
        |if_command.
        |if_elif_else_command.
        |if_else_command.
 
 
if_command ::= if_part.
if_elif_else_command ::= if_part, elif_part, else_part.
if_else_command ::= if_part, else_part.

if_part ::= ['if'], ['('], condition, [')'], K.
else_part ::= ['else'], K.
                |['elif'], ['('], condition, [')'], K.
                |['elif'], ['('], condition, [')'], K, elif_command.

while_loop ::= ['while'], ['('], condition, [')'], K.

for_range ::= ['for'], I, ['in'], ['range'], ['('], inRange, [';'], inRange, [')'], K.

inRange ::= I | integer.

for_loop ::= ['for'], ['('], assignment, [';'], condition, [';'], variableChange, [')'], K.

variableChange ::= increment.
                    |decrement.
                    |I, assignmentConstruct, E.

condition ::= E, comparisonConstructs, E.

decrement ::= I, decrementConstruct.
                |decrementConstruct, I.
increment ::= I, incrementConstruct.
                |incrementConstruct, I.

print ::= [print_string], ['('], string, [')'], end_of_command.
            |[print_string], ['('], I, [')'], end_of_command.
            |[print_expression], ['('], E, [')'], end_of_command.
 
ternary_expression ::= ['('], condition, [')'], ['?'], E, [':'], E.

value ::= float | integer | boolean | string | I.

boolean_operators ::= andConstruct | orConstruct | notConstruct.

operators ::= ['+'] | ['-'] | ['*'] | ['/'] | boolean_operators.

assignment_command ::= I, assignmentConstruct, E, end_of_command.

 
D ::= variable_type, I, end_of_command.
        |variable_type, I, assignmentConstruct, E, end_of_command.

I ::= lower_case, I.
        |I, upper_case.
        |I, upper_case, I.
        |I, ['_'], I.
        |lower_case.

string ::= single_quote, character_phrase, single_quote.
                    |double_quote, character_phrase, double_quote.

character_phrase ::= character, character_phrase.
                        |character.

character ::= lower_case | upper_case | digit | symbol.

float ::= integer, ['.'], integer.
            |integer.

integer ::= digit, integer.
            |digit.



variable_type --> int | float | bool | string.

decrementConstruct --> --.
incrementConstruct --> ++.

comparisonConstructs --> < | > | <= | >= | == | !=.

single_quote --> '.
double_quote --> ".

lower_case --> a | b | c | d | e | f | g | h | i | j | k | l | m | n | o | p | q | r | s | t | u | v | w | x | y | z.

upper_case --> A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z.

symbol --> ' '| ! | " | # | $ | & | ' | ( | ) | * | + | , | - | . | / | : | ; | < | = | > | ? | @ | [ | \ | ] | ^ | _ | ` | { | | | } | ~.

digit --> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9.

boolean --> ['True'] | ['False'].

assignmentConstruct --> =.
end_of_command --> ;.

andConstruct --> and.
orConstruct --> or.
notConstruct --> not.
