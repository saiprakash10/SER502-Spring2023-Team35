/* 
Mapping 
P --> Program 
K --> Block 
D --> Declaration 
C --> Command Lines (Statements) 
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
 
 
P ::=     K 
K ::=     start C stop. 
 
D ::=  DT I = data; 
           | Dt I; 
           | I = I; 
           | I = data;  

DT ::=    int | float | string | boolean 
data:= BI | N | I | ST


C ::=   D, C %Check semi-colon use here
           | I = E; C 
           |print_statement; C
           |If_then_else; C
           |while_loop; C
           |for_loop; C
           |ternary_expression; C
           | K 
           | ε 
 
 
If_then_else::=  if B { C } 
                | if B { C } else { C } 
                | if B { C } Elif else { C } 
 
Elif ::= elif B { C } Elif | ε 

while_loop::=  while B { C }
 
for_loop::=  for I in range (N,N){ C }
            | for(I = N; I S N; ID){ C }
 
ternary_expression::= ( B ) ? E : E

print_statement::= print(E)
                  |print(I)
                  |print(ST)
                  |print(B)
 
B ::=     BI | E S E | not B | B and B| B or B
             
E ::=     T + E | T - E | T
T ::=     ( E ) T
T ::=     F * T | F / T | F
F ::=     I | N
BI ::=    true | false % Should expr have decl?

I ::= var CH, ST, N; %Fix I and D grammar?
N ::=   DG, N | ε
ST ::=  CH, ST | ε
DG ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
CH ::=  Upper_case
        | Lower_case
        | special_char


Upper_case ::= A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

Lower_case ::= a | b | c | d | e | f | g | h | i | j | k | l | m | n | o | p | q | r | s | t | u | v | w | x | y | z

special_char ::= ! | " | # | $ | & | ' | ( | ) | * | + | , | - | . | / | : | ; | < | = | > | ? | @ | [ | \ | ] | ^ | _ | ` | { | | | } | ~

S ::=    < | > | >= | <= | == | !=
ID ::=   I++ | I--
