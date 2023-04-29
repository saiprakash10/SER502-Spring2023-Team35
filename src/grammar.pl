/* 
Mapping 
P --> Program 
K --> Block 
D --> Declaration 
CL --> Command List
DT --> Data Type 
BI --> Boolean Value 
B --> Boolean Expression 
E --> Expression 
T --> Term 
F --> Form   (Form is used to assign Identifier or Number to Term) 
I --> Identifier 
N --> Number 
DG --> Digit 
S --> Symbols
ID --> increment/Decrement
Data --> Literals
ST --> String
CH --> Characters    
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
 
 
if_command --> if_part.
if_elif_else_command --> if_part, elif_part, else_part.
if_else_command --> if_part, else_part.

if_part --> ['if'], ['('], condition, [')'], K.
else_part --> ['else'], K.
                |['elif'], ['('], condition, [')'], K.
                |['elif'], ['('], condition, [')'], K, elif_command.

while_loop --> ['while'], ['('], condition, [')'], K.

for_range --> ['for'], I, ['in'], ['range'], ['('], inRange, [';'], inRange, [')'], K.

inRange --> I | integer.

for_loop --> ['for'], ['('], assignment, [';'], condition, [';'], variableChange, [')'], K.

variableChange --> increment.
                    |decrement.
                    |I, assignmentConstruct, E.

condition --> E, comparisonConstructs, E.

decrement --> I, decrementConstruct.
                |decrementConstruct, I.
increment --> I, incrementConstruct.
                |incrementConstruct, I.

print --> [print_string], ['('], string, [')'], end_of_command.
            |[print_string], ['('], I, [')'], end_of_command.
            |[print_expression], ['('], E, [')'], end_of_command.
 
ternary_expression::= ( B ) ? E : E

 
B ::=     BI | E S E | not B | B and B| B or B
             
E ::=     T + E | T - E | T
T ::=     ( E ) T
T ::=     F * T | F / T | F
F ::=     I | N
BI ::=    true | false

I ::= CH ST N;

N ::=   DG N | ε
ST ::=  CH ST | ε
DG ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
CH ::=  Upper_case
        | Lower_case
        | special_char


Upper_case ::= A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

Lower_case ::= a | b | c | d | e | f | g | h | i | j | k | l | m | n | o | p | q | r | s | t | u | v | w | x | y | z

special_char ::= ! | " | # | $ | & | ' | ( | ) | * | + | , | - | . | / | : | ; | < | = | > | ? | @ | [ | \ | ] | ^ | _ | ` | { | | | } | ~

S ::=    < | > | >= | <= | == | !=
ID ::=   I++ | I--
