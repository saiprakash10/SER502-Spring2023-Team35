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

% Checks valid variable names
iden(t_iden(V), [V | Tail], Tail) :-
    atom(V), not_keyword(V).

not_keyword(V) :-
    not(member(V, [int, float, bool, string, true, false, for,
    if, elif, else, while, range, and, or, not, in, range, <, >, <=, >=, ==,
    '!=', ++, --, +, -, *, /])).

data_type(t_variable_type(Head), [Head | T], T) :-
    member(Head, [int, float, bool, string]).

comp_opr(t_comp_opr(Head), [Head | T], T) :- member(Head, [<, >, <=, >=, ==, '!=']).

integer_iden(t_integer(Data), [Data | Tail], Tail) :- integer(Data).
float_iden(t_float(Data), [Data | Tail], Tail) :- float(Data).
string_iden(t_string(Data), [Data | Tail], Tail) :- string(Data).
boolean_iden(t_boolean(I), [I | Tail], Tail) :- member(I, [true, false]).

assign_opr(t_assignment_operator) --> [=].
end_of_command(t_end_of_command) --> [;].

boolean_opr(t_boolean_operator(Bool_opr), [Bool_opr | Tail], Tail) :- member(Bool_opr, [and, or, not]).

% Testing Predicates
parse(T, L) :- assign_command(T, L, []);assign(T, L, []);assign_opr(T, L, []);block(T, L, []);comm(T, L, []);comm_list(T, L, []);bool(T, L, []);dec(T, L, []);elif_part(T, L, []);else_part(T, L, []);end_of_command(T, L, []);expr(T, L, []);expr_1(T, L, []);expr_2(T, L, []);expr_3(T, L, []);for_range_command(T, L, []);for_loop_command(T, L, []);if_command(T, L, []);if_part(T, L, []);inc(T, L, []);print_statement(T, L, []);program(T, L, []); ternary_expression(T, L, []);data(T, L, []);variable_change_part(T, L, []);decl(T, L, []);while_loop_command(T, L, []);iden(T, L, []);data_type(T, L, []);comp_opr(T, L, []);integer_iden(T, L, []);float_iden(T, L, []);string_iden(T, L, []);boolean_iden(T, L, []);boolean_opr(T, L, []).
