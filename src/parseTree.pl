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

%Print Commands
print_statement(t_print_string(I)) --> [print_string], ['('], string_iden(t_string(I)), [')'], end_of_command(_).
print_statement(t_print_string(I)) --> [print_string], ['('], iden(t_iden(I)), [')'], end_of_command(_).
print_statement(t_print_expr(E)) --> [print_expression], ['('], expr(E), [')'], end_of_command(_).

%Declarations
decl(t_decl(DT, I)) --> data_type(DT), iden(I), end_of_command(_).
decl(t_decl(DT, I, E)) --> data_type(DT), iden(I), assign_opr(_), expr(E), end_of_command(_).

assign_command(E) --> assign(E), end_of_command(_).

%For Loop Commands
for_loop_command(t_for_loop_comm(A, B, ID, K)) --> [for], ['('], assign(A), [;], bool(B), [;], inc_dec(ID), [')'], block(K).

inc_dec(E) --> inc(E) | dec(E).
inc_dec(E) --> assign(E).

for_range_command(t_for_range_comm(I, E1, E2, K)) --> [for], iden(I), [in], [range], ['('], expr(E1), [; ], expr(E2), [')'], block(K).

%While Loop Commands
while_loop_command(t_while_comm(B, K)) --> [while], ['('], bool(B), [')'], block(K).

%Boolean Expression Evaluation
bool(t_bool(E1, Comp, E2)) --> expr(E1), comp_opr(Comp), expr(E2).

% Arthemetic Expression Evaluation
expr(t_expr(E)) --> expr_1(E).

expr_1(t_add(X, Y)) --> expr_1(X), [+], expr_2(Y).
expr_1(t_sub(X, Y)) --> expr_1(X), [-], expr_2(Y).
expr_1(X) --> expr_2(X).

expr_2(t_multiply(X, Y)) --> expr_2(X), [*], expr_3(Y).
expr_2(t_divide(X, Y)) --> expr_2(X), [/], expr_3(Y).
expr_2(t_boolean_expression(X, Bool_opr, Y)) --> expr(X), boolean_opr(Bool_opr), expr(Y).
expr_2(X) --> expr_3(X).

expr_3(X) --> ['('], expr(X), [')'].
expr_3(X) --> ternary_expression(X) | iden(X) | data(X).

ternary_expression(t_ternary_expression(B, Expr_true, Expr_false)) --> ['('], bool(B), ['?'], expr(Expr_true), [':'], expr(Expr_false), [')'].

assign(t_assign(I, E)) --> iden(I), assign_opr(_), expr(E).

%Data Types
data(Data) --> integer_iden(Data) | float_iden(Data) | string_iden(Data) | boolean_iden(Data).

dec(t_post_dec(V)) --> iden(V), [--].
dec(t_pre_dec(V)) --> [--], iden(V).
inc(t_post_inc(V)) --> iden(V), [++].
inc(t_pre_inc(V)) --> [++], iden(V).

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


