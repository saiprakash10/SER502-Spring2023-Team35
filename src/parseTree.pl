% parse tree production for grammar
:- use_rendering(svgtree).

% Program is a block of code
program(t_prog(K)) --> block(K).

% Code Block - has a 'start' keyword, followed by declarations, commands, and an 'stop' keyword with period
block(t_block('start', C, 'stop', '.')) --> [start], comm(C), [end], ['.'].

% Declarations and Data Type Definitions
decl(t_decl('var', DT, I, '=', data, ';', D)) --> [var], data_type(DT), iden(I), ['='], data_literal(data), [';'], decl(D).
decl(t_decl('var', DT, I, ';', D)) --> [var], data_type(DT), iden(I), [';'], decl(D).
decl(t_decl('var', I, '=', I, ';', D)) --> [var], iden(I), ['='], iden(I), [';'], decl(D).
decl([]) --> [].

data_type(t_dt('int')) --> [int].
data_type(t_dt('float')) --> [float].
data_type(t_dt('string')) --> [string].
data_type(t_dt('boolean')) --> [boolean].

data_literal(t_data(BI)) --> binary_iden(BI).
data_literal(t_data(N)) --> number_iden(N).
data_literal(t_data(I)) --> iden(I).
data_literal(t_data(ST)) --> string_iden(ST).


% Commands are either declaration statements, print statements, if-then-else statements, for loops, while loop statements, a code block or ternary operator.
comm(t_comm(D, C)) --> decl(D), comm(C).
comm(t_comm(I, '=', E, ';', C)) --> id(I), [':='], expr(E), [';'], comm(C).
comm(t_comm(print_statement, ';', C)) --> print_statement_pred(print_statement), [';'], comm(C).
comm(t_comm(if_then_else, ';', C)) --> if_then_else_pred(if_then_else), [';'], comm(C).
comm(t_comm(while_loop, ';', C)) --> while_loop_pred(while_loop), [';'], comm(C).
comm(t_comm(for_loop, ';', C)) --> for_loop_pred(for_loop), [';'], comm(C).
comm(t_comm(ternary_expression, ';', C)) --> ternary_expression_pred(ternary_expression), [';'], comm(C).
comm(K) --> block(K).
comm([]) --> [].

print_statement_pred(t_print_stat('print', '(', E, ')')) --> [print], ['('], expr(E), [')'].
print_statement_pred(t_print_stat('print', '(', I, ')')) --> [print], ['('], iden(I), [')'].
print_statement_pred(t_print_stat('print', '(', ST, ')')) --> [print], ['('], string(ST), [')'].
print_statement_pred(t_print_stat('print', '(', B, ')')) --> [print], ['('], bool(B), [')'].

if_then_else_pred(t_if_then_else('if', B, '{', C, '}')) --> [if], bool(B), ['{'], comm(C), [}].
if_then_else_pred(t_if_then_else('if', B, '{', C, '}', 'else', '{', C, '}')) --> [if], bool(B), ['{'], comm(C), ['}'], [else],['{'], comm(C), ['}'].
if_then_else_pred(t_if_then_else('if', B, '{', C, '}', Elif 'else', '{', C, '}')) --> [if], bool(B), ['{'], comm(C), ['}'], elif_pred(Elif), [else], ['{'], comm(C), ['}'].

elif_pred(t_elif('elif', B, '{', C, '}', Elif)) --> [elif], bool(B), ['{'] comm(C), ['}'], elif_pred(Elif).
elif_pred([]) --> [].

while_loop_pred(t_while('while', B, '{', C, '}' )) --> ['while'], bool(B), ['{'], comm(C), ['}'].

for_loop_pred(t_for('for', I, 'in', 'range', '(', N, ',', N, ')', '{', C, '}') --> [for], iden(I), [in], [range], ['('], number_iden(N), [,], number_iden(N), [')'], ['{'], comm(C), ['}'].
for_loop_pred(t_for('for', '(', I, '=', N, ';', I, S, I, ';', ID, ')') --> [for], ['('], iden(I), ['='], number_iden(N), [';'], iden(I), symbol[S], iden(I), [;], iter(ID), [')'].

ternary_expression_pred(t_ternary('(', B, ')', '?', E, ':', E)) --> ['('], bool(B), [')'], ['?'], expr(E), [':'], expr(E). 

bool(t_bool(BI)) --> binary_iden(BI).
bool(t_bool(E,S,E)) --> expr(E),symbol(S),expr(E).
bool(t_bool('not',B)) --> [not],bool(B).
bool(t_bool(B,'and',B)) --> bool(B),[and],bool(B).
bool(t_bool(B,'or',B)) --> bool(B),[or],bool(B).

expr(t_expr(T,'+',E)) --> term(T),['+'],expr(E).
expr(t_expr(T,'-',E)) --> term(T),['-'],expr(E).
expr(t_expr(T)) --> term(T).

term(t_term('(',E,')',T)) --> ['('],expr(E),[')'],term(T).

term(t_term(F,'*',T)) --> form(F),['*'],term(T).
term(t_term(F,'/',T)) --> form(F),['/'],term(T).
term(t_term(F)) --> form(F).

form(t_form(I)) --> iden(I).
form(t_form(N)) --> number_iden(N).

bool(t_bool('true')) --> [true].
bool(t_bool('false')) --> [false].

iden(t_iden(CH,ST,N)) --> char(CH),string_iden(ST),number_iden(N).

number_iden(t_number(DG,N)) --> digit(DG),number_iden(N).
number_iden([]) --> [].

string_iden(t_string(CH,ST)) --> string_iden(ST),number_iden(N).
string_iden([]) --> [].
