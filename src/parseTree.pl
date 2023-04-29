:- module(program, [program/3]).
:- table expr_1/3, expr_2/3, expr_3/3.

% % Program is a single block followed by a period
program(t_program(P)) --> comm_list(P).

% Code Block - has a '{' keyword, followed by command list and a '}'
block(t_block(CL)) --> ['{'], comm_list(CL),['}'].

% Commands can be a list of commands or a single command
comm_list(t_comm_list(C, CL)) --> comm(C), comm_list(CL).
comm_list(t_comm(C)) --> comm(C).

% Commands are either declaration statements, print statements, if-then-else statements, for loops, while loop statements, a code block or ternary operator.
comm(C) --> assign_command(C) | for_range_command(C) | for_loop_command(C) | if_command(C) | print_statement(C) | decl(C) | while_loop_command(C).

% If-Elif-Else Commands
if_command(t_if_comm(If, Elif, Else)) --> if_part(If), elif_part(Elif), else_part(Else).
if_command(t_if_comm(If, Else)) --> if_part(If), else_part(Else).
if_command(t_if_comm(If)) --> if_part(If).

if_part(t_if(B, K)) --> [if], ['('], bool(B), [')'], block(K).
elif_part(t_elif(B, K)) --> [elif], ['('], bool(B), [')'], block(K).
elif_part(t_elif(B, K, Elif)) --> [elif], ['('], bool(B), [')'], block(K), elif_part(Elif).
else_part(t_else(K)) --> [else], block(K).

print_statement_pred(t_print_stat('print', '(', E, ')')) --> [print], ['('], expr(E), [')'].
print_statement_pred(t_print_stat('print', '(', I, ')')) --> [print], ['('], iden(I), [')'].
print_statement_pred(t_print_stat('print', '(', ST, ')')) --> [print], ['('], string_iden(ST), [')'].
print_statement_pred(t_print_stat('print', '(', B, ')')) --> [print], ['('], bool(B), [')'].

if_then_else_pred(t_if_then_else('if', B, '{', C, '}')) --> [if], bool(B), ['{'], comm(C), ['}'].
if_then_else_pred(t_if_then_else('if', B, '{', C, '}', 'else', '{', C, '}')) --> [if], bool(B), ['{'], comm(C), ['}'], [else],['{'], comm(C), ['}'].
if_then_else_pred(t_if_then_else('if', B, '{', C, '}', Elif, 'else', '{', C, '}')) --> [if], bool(B), ['{'], comm(C), ['}'], elif_pred(Elif), [else], ['{'], comm(C), ['}'].

elif_pred(t_elif('elif', B, '{', C, '}', Elif)) --> [elif], bool(B), ['{'], comm(C), ['}'], elif_pred(Elif).
elif_pred([]) --> [].

while_loop_pred(t_while('while', B, '{', C, '}' )) --> ['while'], bool(B), ['{'], comm(C), ['}'].

for_loop_pred(t_for('for', I, 'in', 'range', '(', N, ',', N, ')', '{', C, '}')) --> [for], iden(I), [in], [range], ['('], number_iden(N), [','], number_iden(N), [')'], ['{'], comm(C), ['}'].
for_loop_pred(t_for('for', '(', I, '=', N, ';', I, S, I, ';', ID, ')')) --> [for], ['('], iden(I), ['='], number_iden(N), [';'], iden(I), symbol[S], iden(I), [;], iter(ID), [')'].

ternary_expression_pred(t_ternary('(', B, ')', '?', E, ':', E)) --> ['('], bool(B), [')'], ['?'], expr(E), [':'], expr(E). 

%Boolean Expressions
bool(t_bool(BI)) --> binary_iden(BI).
bool(t_bool(E,S,E)) --> expr(E),symbol(S),expr(E).
bool(t_bool('not',B)) --> [not],bool(B).
bool(t_bool(B,'and',B)) --> bool(B),[and],bool(B).
bool(t_bool(B,'or',B)) --> bool(B),[or],bool(B).

%Arithemetic Expressions
expr(t_expr(T,'+',E)) --> term(T),['+'],expr(E).
expr(t_expr(T,'-',E)) --> term(T),['-'],expr(E).
expr(t_expr(T)) --> term(T).

term(t_term('(',E,')',T)) --> ['('],expr(E),[')'],term(T).

term(t_term(F,'*',T)) --> form(F),['*'],term(T).
term(t_term(F,'/',T)) --> form(F),['/'],term(T).
term(t_term(F)) --> form(F).

form(t_form(I)) --> iden(I).
form(t_form(N)) --> number_iden(N).

binary_iden(t_bool('true')) --> [true].
binary_iden(t_bool('false')) --> [false].


iden(t_iden(CH,ST,N)) --> char(CH),string_iden(ST),number_iden(N).

number_iden(t_number(DG,N)) --> digit(DG), number_iden(N).
number_iden([]) --> [].

string_iden(t_string(CH,ST)) --> char(CH), string_iden(ST).
string_iden([]) --> [].

digit(t_digit(0)) --> [0].
digit(t_digit(1)) --> [1].
digit(t_digit(2)) --> [2].
digit(t_digit(3)) --> [3].
digit(t_digit(4)) --> [4].
digit(t_digit(5)) --> [5].
digit(t_digit(6)) --> [6].
digit(t_digit(7)) --> [7].
digit(t_digit(8)) --> [8].
digit(t_digit(9)) --> [9].

char(t_char(Upper_case)) --> upper_case_pred(Upper_case).
char(t_char(Lower_case)) --> lower_case_pred(Lower_case).
char(t_char(special_char)) --> special_char_pred(special_char).

upper_case_pred(t_Upper_case('A')) --> ['A'].
upper_case_pred(t_Upper_case('B')) --> ['B'].
upper_case_pred(t_Upper_case('C')) --> ['C'].
upper_case_pred(t_Upper_case('D')) --> ['D'].
upper_case_pred(t_Upper_case('E')) --> ['E'].
upper_case_pred(t_Upper_case('F')) --> ['F'].
upper_case_pred(t_Upper_case('G')) --> ['G'].
upper_case_pred(t_Upper_case('H')) --> ['H'].
upper_case_pred(t_Upper_case('I')) --> ['I'].
upper_case_pred(t_Upper_case('J')) --> ['J'].
upper_case_pred(t_Upper_case('K')) --> ['K'].
upper_case_pred(t_Upper_case('L')) --> ['L'].
upper_case_pred(t_Upper_case('M')) --> ['M'].
upper_case_pred(t_Upper_case('N')) --> ['N'].
upper_case_pred(t_Upper_case('O')) --> ['O'].
upper_case_pred(t_Upper_case('P')) --> ['P'].
upper_case_pred(t_Upper_case('Q')) --> ['Q'].
upper_case_pred(t_Upper_case('R')) --> ['R'].
upper_case_pred(t_Upper_case('S')) --> ['S'].
upper_case_pred(t_Upper_case('T')) --> ['T'].
upper_case_pred(t_Upper_case('U')) --> ['U'].
upper_case_pred(t_Upper_case('V')) --> ['V'].
upper_case_pred(t_Upper_case('W')) --> ['W'].
upper_case_pred(t_Upper_case('X')) --> ['X'].
upper_case_pred(t_Upper_case('Y')) --> ['Y'].
upper_case_pred(t_Upper_case('Z')) --> ['Z'].

lower_case_pred(t_Lower_case('a')) --> ['a'].
lower_case_pred(t_Lower_case('b')) --> ['b'].
lower_case_pred(t_Lower_case('c')) --> ['c'].
lower_case_pred(t_Lower_case('d')) --> ['d'].
lower_case_pred(t_Lower_case('e')) --> ['e'].
lower_case_pred(t_Lower_case('f')) --> ['f'].
lower_case_pred(t_Lower_case('g')) --> ['g'].
lower_case_pred(t_Lower_case('h')) --> ['h'].
lower_case_pred(t_Lower_case('i')) --> ['i'].
lower_case_pred(t_Lower_case('j')) --> ['j'].
lower_case_pred(t_Lower_case('k')) --> ['k'].
lower_case_pred(t_Lower_case('l')) --> ['l'].
lower_case_pred(t_Lower_case('m')) --> ['m'].
lower_case_pred(t_Lower_case('n')) --> ['n'].
lower_case_pred(t_Lower_case('o')) --> ['o'].
lower_case_pred(t_Lower_case('p')) --> ['p'].
lower_case_pred(t_Lower_case('q')) --> ['q'].
lower_case_pred(t_Lower_case('r')) --> ['r'].
lower_case_pred(t_Lower_case('s')) --> ['s'].
lower_case_pred(t_Lower_case('t')) --> ['t'].
lower_case_pred(t_Lower_case('u')) --> ['u'].
lower_case_pred(t_Lower_case('v')) --> ['v'].
lower_case_pred(t_Lower_case('w')) --> ['w'].
lower_case_pred(t_Lower_case('x')) --> ['x'].
lower_case_pred(t_Lower_case('y')) --> ['y'].
lower_case_pred(t_Lower_case('z')) --> ['z'].

special_char_pred(t_special_char('!')) --> ['!'].
special_char_pred(t_special_char('"')) --> ['"'].
special_char_pred(t_special_char('#')) --> ['#'].
special_char_pred(t_special_char('$')) --> ['$'].
special_char_pred(t_special_char('&')) --> ['&'].
special_char_pred(t_special_char('\'')) --> ['\''].
special_char_pred(t_special_char('(')) --> ['('].
special_char_pred(t_special_char(')')) --> [')'].
special_char_pred(t_special_char('*')) --> ['*'].
special_char_pred(t_special_char('+')) --> ['+'].
special_char_pred(t_special_char(',')) --> [','].
special_char_pred(t_special_char('-')) --> ['-'].
special_char_pred(t_special_char('.')) --> ['.'].
special_char_pred(t_special_char('/')) --> ['/'].
special_char_pred(t_special_char(':')) --> [':'].
special_char_pred(t_special_char(';')) --> [';'].
special_char_pred(t_special_char('<')) --> ['<'].
special_char_pred(t_special_char('=')) --> ['='].
special_char_pred(t_special_char('>')) --> ['>'].
special_char_pred(t_special_char('?'))--> ['?'].
special_char_pred(t_special_char('@')) --> ['@'].
special_char_pred(t_special_char('[')) --> ['['].
special_char_pred(t_special_char('\\')) --> ['\\'].
special_char_pred(t_special_char(']')) --> [']'].
special_char_pred(t_special_char('^')) --> ['^'].
special_char_pred(t_special_char('_')) --> ['_'].
special_char_pred(t_special_char('`')) --> ['`'].

symbol(t_symbol('<')) --> ['<'].
symbol(t_symbol('>')) --> ['>'].
symbol(t_symbol('>=')) --> ['>='].
symbol(t_symbol('<=')) --> ['<='].
symbol(t_symbol('==')) --> ['=='].
symbol(t_symbol('!=')) --> ['!='].

iter(t_increment(I,'++')) --> iden(I),['++'].
iter(t_decrement(I,'--')) --> iden(I),['--'].


