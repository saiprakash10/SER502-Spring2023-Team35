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

bool(t_bool('true')) --> [true].
bool(t_bool('false')) --> [false].


iden(t_iden(CH,ST,N)) --> char(CH),string_iden(ST),number_iden(N).

number_iden(t_number(DG,N)) --> digit(DG),number_iden(N).
number_iden([]) --> [].

string_iden(t_string(CH,ST)) --> string_iden(ST),number_iden(N).
string_iden([]) --> [].

digit(t_digit(0)) --> [0].
digit(t_digit(1) --> [1].
digit(t_digit(2) --> [2].
digit(t_digit(3) --> [3].
digit(t_digit(4) --> [4].
digit(t_digit(5) --> [5].
digit(t_digit(6) --> [6].
digit(t_digit(7) --> [7].
digit(t_digit(8) --> [8].
digit(t_digit(9) --> [9].

char(t_char('Upper_case')) --> [Upper_case].
char(t_char('Upper_case')) --> [Upper_case].
char(t_char('Lower_case')) --> [Lower_case].

Upper_case(t_Upper_case('A')) --> [A].
Upper_case(t_Upper_case('B')) --> [B].
Upper_case(t_Upper_case('C')) --> [C].
Upper_case(t_Upper_case('D')) --> [D].
Upper_case(t_Upper_case('E')) --> [E].
Upper_case(t_Upper_case('F')) --> [F].
Upper_case(t_Upper_case('G')) --> [G].
Upper_case(t_Upper_case('H')) --> [H].
Upper_case(t_Upper_case('I')) --> [I].
Upper_case(t_Upper_case('J')) --> [J].
Upper_case(t_Upper_case('K')) --> [K].
Upper_case(t_Upper_case('L')) --> [L].
Upper_case(t_Upper_case('M')) --> [M].
Upper_case(t_Upper_case('N')) --> [N].
Upper_case(t_Upper_case('O')) --> [O].
Upper_case(t_Upper_case('P')) --> [P].
Upper_case(t_Upper_case('Q')) --> [Q].
Upper_case(t_Upper_case('R')) --> [R].
Upper_case(t_Upper_case('S')) --> [S].
Upper_case(t_Upper_case('T')) --> [T].
Upper_case(t_Upper_case('U')) --> [U].
Upper_case(t_Upper_case('V')) --> [V].
Upper_case(t_Upper_case('W')) --> [W].
Upper_case(t_Upper_case('X')) --> [X].
Upper_case(t_Upper_case('Y')) --> [Y].
Upper_case(t_Upper_case('Z')) --> [Z].

Lower_case(t_Lower_case('a')) --> [a].
Lower_case(t_Lower_case('b')) --> [b].
Lower_case(t_Lower_case('c')) --> [c].
Lower_case(t_Lower_case('d')) --> [d].
Lower_case(t_Lower_case('e')) --> [e].
Lower_case(t_Lower_case('f')) --> [f].
Lower_case(t_Lower_case('g')) --> [g].
Lower_case(t_Lower_case('h')) --> [h].
Lower_case(t_Lower_case('i')) --> [i].
Lower_case(t_Lower_case('j')) --> [j].
Lower_case(t_Lower_case('k')) --> [k].
Lower_case(t_Lower_case('l')) --> [l].
Lower_case(t_Lower_case('m')) --> [m].
Lower_case(t_Lower_case('n')) --> [n].
Lower_case(t_Lower_case('o')) --> [o].
Lower_case(t_Lower_case('p')) --> [p].
Lower_case(t_Lower_case('q')) --> [q].
Lower_case(t_Lower_case('r')) --> [r].
Lower_case(t_Lower_case('s')) --> [s].
Lower_case(t_Lower_case('t')) --> [t].
Lower_case(t_Lower_case('u')) --> [u].
Lower_case(t_Lower_case('v')) --> [v].
Lower_case(t_Lower_case('w')) --> [w].
Lower_case(t_Lower_case('x')) --> [x].
Lower_case(t_Lower_case('y')) --> [y].
Lower_case(t_Lower_case('z')) --> [z].

special_char(t_special_char('!')) --> [!].
special_char(t_special_char('"')) --> ["].
special_char(t_special_char('#')) --> [#].
special_char(t_special_char('$')) --> [$].
special_char(t_special_char('&')) --> [&].
special_char(t_special_char(''')) --> ['].
special_char(t_special_char('(')) --> [(].
special_char(t_special_char(')')) --> [)].
special_char(t_special_char('*')) --> [*].
special_char(t_special_char('+')) --> [+].
special_char(t_special_char(',')) --> [,].
special_char(t_special_char('-')) --> [-].
special_char(t_special_char('.')) --> [.].
special_char(t_special_char('/')) --> [/].
special_char(t_special_char(':')) --> [:].
special_char(t_special_char(';')) --> [;].
special_char(t_special_char('<')) --> [<].
special_char(t_special_char('=')) --> [=].
special_char(t_special_char('>')) --> [>].
special_char(t_special_char('?'))--> [?].
special_char(t_special_char('@')) --> [@].
special_char(t_special_char('[')) --> [[].
special_char(t_special_char('\')) --> [\].
special_char(t_special_char(']')) --> []].
special_char(t_special_char('^')) --> [^].
special_char(t_special_char('_')) --> [_].
special_char(t_special_char('`')) --> [`].

symbol(t_symbol('<')) --> [<]
symbol(t_symbol('>')) --> [>]
symbol(t_symbol('>=')) --> [>=]
symbol(t_symbol('<=')) --> [<=]
symbol(t_symbol('==')) --> [==]
symbol(t_symbol('!=')) --> [!=]

increment(t_increment(I,'++')) --> iden(I),['++'].
decrement(t_decrement(I,'--')) --> iden(I),['--'].


