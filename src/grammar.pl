/* 
Mapping 
P --> Program 
K --> Block 
D --> Declaration 
C --> Command Lines (Statements) 
DT --> Data Type 
Bl --> Boolean Value 
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

 
C ::=   D, C; 
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
                | if B { C } elif { C } Elif else { C } 
 
Elif ::= elif { C } Elif | ε 
