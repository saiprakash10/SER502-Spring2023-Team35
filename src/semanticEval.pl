% Load the module with a single exported predicate, eval_program/2
:- module(eval_program, [eval_program/2]).

% Evaluate a program, which consists of a command list, by invoking eval_comm_list with an empty environment
eval_program(t_program(P), NewEnv) :- eval_comm_list(P, [], NewEnv).

% Evaluate a command list by sequentially evaluating each command and updating the environment as necessary
eval_comm_list(t_comm_list(C, CL), Env, NewEnv) :- eval_comm(C, Env, E1), eval_comm_list(CL, E1, NewEnv).
eval_comm_list(t_comm(C), Env, NewEnv) :- eval_comm(C, Env, NewEnv).

% Evaluate a block of commands by evaluating each command in the block and updating the environment as necessary
eval_block(t_block(CL), Env, NewEnv) :- eval_comm_list(CL, Env, NewEnv).

% Evaluate an assignment command by evaluating the right-hand side expression and updating the value of the variable in the environment
eval_comm(t_assign(t_iden(I), E), Env, NewEnv) :- eval_expr(E, Env, R1), update(I, R1, Env, NewEnv).

% Evaluate a declaration command with an initialization expression by evaluating the expression and adding the variable to the environment
eval_comm(t_decl(DT, t_iden(I), E), Env, NewEnv) :- eval_variable_type(DT, Env, R1), eval_expr(E, Env, R2), update(R1, I, R2, Env, NewEnv).

% Evaluate a declaration command without an initialization expression by adding the variable to the environment with a default value
eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = int,  update(R1, I, 0, Env, NewEnv).
eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = float,  update(R1, I, 0.0, Env, NewEnv).
eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = string,  update(R1, I, "", Env, NewEnv).
eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = bool,  update(R1, I, false, Env, NewEnv).

% Evaluate a print expression command by evaluating the expression and printing the result
eval_comm(t_print_expr(E), Env, Env) :- eval_expr(E, Env, Result), write(Result), nl.

% Evaluate a print string command by printing the specified string
eval_comm(t_print_string(String), Env, Env) :- write(String), nl.

% Evaluate a for loop command by evaluating the initialization command, the condition, the update command, and the loop body
eval_comm(t_for_loop_comm(A, B, ID, K), Env, NewEnv) :- eval_comm(A, Env, E1), eval_for_comm(B, ID, K, E1, NewEnv).

% Evaluate a while loop command by evaluating the condition and the loop body, repeating until the condition is false
eval_comm(t_while_comm(C, B), Env, NewEnv) :- eval_bool(C, Env, true), eval_block(B, Env, E1), eval_comm(t_while_comm(C, B), E1, NewEnv).
eval_comm(t_while_comm(C, _), Env, _) :- eval_bool(C, Env, false).

% Semantic for Enhanced for loop
eval_comm(t_for_range_comm(I, Expr1, Expr2, K), Env, NewEnv) :- 
    eval_comm(t_assign(I, Expr1), Env, E1),
    eval_bool(t_bool(Expr1, t_comp_opr(>), Expr2), E1, false),
    eval_for_comm(t_bool(I, t_comp_opr(=<), Expr2), t_pre_inc(I), K, E1, NewEnv).

eval_comm(t_for_range_comm(I, Expr1, Expr2, K), Env, NewEnv) :-
    eval_comm(t_assign(I, Expr1), Env, E1),
    eval_bool(t_bool(Expr1, t_comp_opr(<), Expr2), E1, false),
    eval_for_comm(t_bool(I, t_comp_opr(>=), Expr2), t_pre_dec(I), K, E1, NewEnv).

% Semantic for If else
eval_comm(t_if_comm(If), Env, NewEnv) :- eval_if_part(If, Env, NewEnv, _).
eval_comm(t_if_comm(If, _, _), Env, NewEnv) :- eval_if_part(If, Env, NewEnv, true).
eval_comm(t_if_comm(If, Elif, _), Env, NewEnv) :- eval_if_part(If, Env, _, false), eval_elif_part(Elif, Env, NewEnv, true).
eval_comm(t_if_comm(If, Elif, Else), Env, NewEnv) :- eval_if_part(If, Env, _, false), eval_elif_part(Elif, Env, _, false), eval_else_part(Else, Env, NewEnv, true).
eval_comm(t_if_comm(If, _), Env, NewEnv) :- eval_if_part(If, Env, NewEnv, true).
eval_comm(t_if_comm(If, Else), Env, NewEnv) :- eval_if_part(If, Env, _, false), eval_else_part(Else, Env, NewEnv, true).

eval_for_comm(B, _, _, Env, Env) :- eval_bool(B, Env, false).

eval_for_comm(B, t_pre_inc(I), K, Env, NewEnv) :-
    eval_bool(B, Env, true),
    eval_block(K, Env, E1),
    eval_expr(t_increment(I), E1, E2),
    eval_for_comm(B, t_pre_inc(I), K, E2, NewEnv).

eval_for_comm(B, t_pre_dec(I), K, Env, NewEnv) :-
    eval_bool(B, Env, true),
    eval_block(K, Env, E1),
    eval_expr(t_decrement(I), E1, E2),
    eval_for_comm(B, t_pre_dec(I), K, E2, NewEnv).

eval_for_comm(B, t_post_dec(I), K, Env, NewEnv) :-
    eval_bool(B, Env, true),
    eval_block(K, Env, E1),
    eval_expr(t_decrement(I), E1, E2),
    eval_for_comm(B, t_post_dec(I), K, E2, NewEnv).

eval_for_comm(B, t_post_inc(I), K, Env, NewEnv) :-
    eval_bool(B, Env, true),
    eval_block(K, Env, E1),
    eval_expr(t_increment(I), E1, E2),
    eval_for_comm(B, t_post_inc(I), K, E2, NewEnv).

eval_if_part(t_if(B, K), Env, NewEnv, true) :- eval_bool(B, Env, true), eval_block(K, Env, NewEnv).
eval_if_part(t_if(B, _), Env, Env, false) :- eval_bool(B, Env, false).

eval_elif_part(t_elif(B, K), Env, NewEnv, true) :- eval_bool(B, Env, true), eval_block(K, Env, NewEnv).
eval_elif_part(t_elif(B, K, _), Env, NewEnv, true) :- eval_bool(B, Env, true), eval_block(K, Env, NewEnv).
eval_elif_part(t_elif(B, _, ElifPart), Env, NewEnv, R) :- eval_bool(B, Env, false), eval_elif_part(ElifPart, Env, NewEnv, R).
eval_elif_part(t_elif(B, _, _), Env, Env, false) :- eval_bool(B, Env, false).

eval_else_part(t_else(K), Env, NewEnv, true) :- eval_block(K, Env, NewEnv).

eval_bool(t_bool(Expr1, Opr, Expr2), Env, Result) :-
    eval_expr(Expr1, Env, R1),
    eval_expr(Expr2, Env, R2),
    eval_comp(R1, Opr, R2, Result).

eval_comp(V1, t_comp_opr(>), V2, true)  :- V1 > V2.
eval_comp(V1, t_comp_opr(>), V2, false)  :- V1 =< V2.
eval_comp(V1, t_comp_opr(<), V2, true)  :- V1 < V2.
eval_comp(V1, t_comp_opr(<), V2, false)  :- V1 >= V2.
eval_comp(V1, t_comp_opr(>=), V2, true)  :- V1 >= V2.
eval_comp(V1, t_comp_opr(>=), V2, false) :- V1 < V2.
eval_comp(V1, t_comp_opr(=<), V2, true)  :- V1 =< V2.
eval_comp(V1, t_comp_opr(=<), V2, false) :- V1 > V2.
eval_comp(V1, t_comp_opr(==), V2, true)  :- V1 =:= V2.
eval_comp(V1, t_comp_opr(==), V2, false) :- V1 =\= V2.
eval_comp(V1, t_comp_opr('!='), V2, true)  :- V1 =\= V2.
eval_comp(V1, t_comp_opr('!='), V2, false) :- V1 =:= V2.

eval_expr(t_expr(X), Env, Result) :- eval_expr(X, Env, Result).
eval_expr(t_add(X, Y), Env, Result) :- eval_expr(X, Env, R1), eval_expr(Y, Env, R2), Result is R1+R2.
eval_expr(t_sub(X, Y), Env, Result) :- eval_expr(X, Env, R1), eval_expr(Y, Env, R2), Result is R1-R2.
eval_expr(t_multiply(X, Y), Env, Result) :- eval_expr(X, Env, R1), eval_expr(Y, Env, R2), Result is R1*R2.
eval_expr(t_divide(X, Y), Env, Result) :- eval_expr(X, Env, R1), eval_expr(Y, Env, R2), Result is R1/R2.
eval_expr(t_boolean(I), _, I).
eval_expr(t_integer(I), _, I).
eval_expr(t_float(I) , _, I).
eval_expr(t_string(I) , _, I).
eval_expr(t_iden(I), Env, Data) :- lookup(I, Data, Env).
eval_expr(t_iden(I), Env, I) :- not(lookup(I, _, Env)), string(I).
eval_expr(t_increment(t_iden(I)), Env, NewEnv) :- lookup(I, Result, Env), NewValue is Result+1, update(I, NewValue, Env, NewEnv).
eval_expr(t_decrement(t_iden(I)), Env, NewEnv) :- lookup(I, Result, Env), NewValue is Result-1, update(I, NewValue, Env, NewEnv).
eval_expr(t_increment(I), Env, NewEnv) :- lookup(I, Result, Env), NewValue is Result+1, update(I, NewValue, Env, NewEnv).
eval_expr(t_decrement(I), Env, NewEnv) :- lookup(I, Result, Env), NewValue is Result-1, update(I, NewValue, Env, NewEnv).

% Semantic for Ternary operation
eval_expr(t_ternary_expression(B, Expr_true, _), Env, Result) :-
    eval_bool(B, Env, true),
    eval_expr(Expr_true, Env, Result).

eval_expr(t_ternary_expression(B, _, Expr_false), Env, Result) :-
    eval_bool(B, Env, false),
    eval_expr(Expr_false, Env, Result).

eval_expr(t_boolean_expression(X, t_boolean_operator(Opr), Y), Env, Result) :-
    eval_expr(X, Env, R1),
    eval_expr(Y, Env, R2),
    eval_boolean(R1,  Opr, R2, Result).

eval_boolean(true , and, true  , true).
eval_boolean(true , and, false , false).
eval_boolean(false , and, true  , false).
eval_boolean(false , and, false , false).
eval_boolean(true , or , true  , true).
eval_boolean(true , or , false , true).
eval_boolean(false , or , true  , true).
eval_boolean(false , or , false , false).

eval_variable_type(t_variable_type(DT), _, DT).

lookup(I, Data, [(_, I, Data) | _]).
lookup(I, Data, [_Head | Tail]) :- lookup(I, Data, Tail).

update(I, _, [], []) :- error_undeclared(I).
update(I, Data, [Head | Tail], [Head | NewEnv]) :- Head \= (_, I, _), update(I, Data, Tail, NewEnv).
update(I, Data, [(int , I, _) | Env], [ (int , I, Data) | Env]) :- integer(Data).
update(I, Data, [(float , I, _) | Env], [ (float , I, Data) | Env]) :- float(Data).
update(I, Data, [(bool , I, _) | Env], [ (bool , I, Data) | Env]) :- member(Data, [true, false]).
update(I, Data, [(string, I, _) | Env], [ (string, I, Data) | Env]) :- string(Data).

update(I, Data, [(int , I, _) | _], _)  :- not(integer(Data)),     error_type_conversion(I, int).
update(I, Data, [(float, I, _) | _], _)  :- not(float(Data)),     error_type_conversion(I, float).
update(I, Data, [(bool , I, _) | _], _)  :- not(member(Data, [true, false])), error_type_conversion(I, bool).
update(I, Data, [(string, I, _) | _], _) :- not(string(Data)) ,     error_type_conversion(I, string).

update(DT, I, Data, [], [(DT, I, Data)]).
update(DT, I, Data, [Head | Tail], [Head | NewEnv]) :- Head \= (_, I, _), update(DT, I, Data, Tail, NewEnv).
update(_, I, _, [(_, I, _) | _], _NewEnv) :- error_redefinition(I).

error(String, List) :-
    ansi_format([bold, fg(red)], String, List), halt.
error_redefinition(I) :-
    error('Error: Redefinition of ~w', [I]).
error_type_conversion(I, DT) :-
    error('Error: IMPRO doesn\'t support type conversion. (Variable \'~w\' is not of type \'~w\')', [I, DT]).
error_undeclared(I) :- error('Error: ~w Undeclared', [I]).
