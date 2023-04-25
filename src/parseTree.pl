% parse tree production for grammar
:- use_rendering(svgtree).

% Program is a block of code
program(t_prog(K)) --> block(K).

% Code Block - has a 'start' keyword, followed by declarations, commands, and an 'stop' keyword with period.
block(t_block('start', C, 'stop', '.')) --> [start], comm(C), [end], ['.'].


% Commands are either declaration statements, print statements, if-then-else statements, for loops, while loop statements, a code block or ternary operator.
comm(t_comm(D, C, ';')) --> decl(D), comm(C).
comm(t_comm(I, '=', E, ';', C)) --> id(I), [':='], expr(E), [';'], comm(C).
comm(t_comm(print_statement, ';', C)) --> print_statement_pred(print_statement), [';'], comm(C).
comm(t_comm(if_then_else, ';', C)) --> if_then_else_pred(if_then_else), [';'], comm(C).
comm(t_comm(while_loop, ';', C)) --> while_loop_pred(while_loop), [';'], comm(C).
comm(t_comm(for_loop, ';', C)) --> for_loop_pred(for_loop), [';'], comm(C).
comm(t_comm(ternary_expression, ';', C)) --> ternary_expression_pred(ternary_expression), [';'], comm(C).
comm(K) --> block(K).
% Preicate for epsilon?

print_statement_pred(t_print_stat('print', '(', E, ')')) --> [print], [(], expr(E), [)].
print_statement_pred(t_print_stat('print', '(', I, ')')) --> [print], [(], iden(E), [)].
print_statement_pred(t_print_stat('print', '(', ST, ')')) --> [print], [(], string(E), [)].
print_statement_pred(t_print_stat('print', '(', B, ')')) --> [print], [(], bool(E), [)].

if_then_else_pred(t_if_then_else('if', B, '{', C, '}')) --> [if], bool(B), [{], comm(C), [}].
if_then_else_pred(t_if_then_else('if', B, '{', C, '}', 'else', '{', C, '}')) --> [if], bool(B), [{], comm(C), [}], [else],[{], comm(C), [}].
if_then_else_pred(t_if_then_else('if', B, '{', C, '}', Elif 'else', '{', C, '}')) --> [if], bool(B), [{], comm(C), [}], elif_pred(Elif), [else], [{], comm(C), [}].

elif_pred(t_elif('elif', B, '{', C, '}', Elif)) --> [elif], bool(B), [{], comm(C), [}], elif_pred(Elif).
% Epsilon Predicate

while_loop_pred(t_while('while', B, '{', C, '}' )) --> ['while'], bool(B), [{], comm(C).

for_loop_pred(t_for('for', I, 'in', 'range', '(', N, ',', N, ')', '{', C, '}') --> [for], id(I), [in], [range], [(], number(N), [,], number(N), [)], [{], comm(C), [}].
for_loop_pred(t_for('for', '(', I, '=', N, ';', I, S, I, ';', ID, ')') --> [for], [(], id(I), [=], number(N), [;], id(I), symbol[S], id(I), [;], iter(ID), [)].

ternary_expression_pred(t_ternary('(', B, ')', '?', E, ':', E)) --> [(], bool(B), [)], [?], expr(E), [:], expr(E). 
