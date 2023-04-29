:- module(eval_program, [eval_program/2]).

eval_program(t_program(P), NewEnv) :- eval_comm_list(P, [], NewEnv).

eval_comm_list(t_comm_list(C, CL), Env, NewEnv) :- eval_comm(C, Env, E1), eval_comm_list(CL, E1, NewEnv).
eval_comm_list(t_comm(C), Env, NewEnv) :- eval_comm(C, Env, NewEnv).

eval_block(t_block(CL), Env, NewEnv) :- eval_comm_list(CL, Env, NewEnv).

eval_comm(t_assign(t_iden(I), E), Env, NewEnv) :- eval_expr(E, Env, R1), update(I, R1, Env, NewEnv).
eval_comm(t_decl(DT, t_iden(I), E), Env, NewEnv) :- eval_variable_type(DT, Env, R1), eval_expr(E, Env, R2), update(R1, I, R2, Env, NewEnv).

eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = int,  update(R1, I, 0, Env, NewEnv).
eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = float,  update(R1, I, 0.0, Env, NewEnv).
eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = string,  update(R1, I, "", Env, NewEnv).
eval_comm(t_decl(DT, t_iden(I)), Env, NewEnv) :- eval_variable_type(DT, Env, R1), R1 = bool,  update(R1, I, false, Env, NewEnv).

eval_comm(t_print_expr(E), Env, Env) :- eval_expr(E, Env, Result), write(Result), nl.
eval_comm(t_print_string(String), Env, Env) :- write(String), nl.

eval_comm(t_for_loop_comm(A, B, ID, K), Env, NewEnv) :- eval_comm(A, Env, E1), eval_for_comm(B, ID, K, E1, NewEnv).

eval_comm(t_while_comm(C, B), Env, NewEnv) :- eval_bool(C, Env, true), eval_block(B, Env, E1), eval_comm(t_while_comm(C, B), E1, NewEnv).
eval_comm(t_while_comm(C, _), Env, _) :- eval_bool(C, Env, false).

eval_comm(t_for_range_comm(I, Expr1, Expr2, K), Env, NewEnv) :- 
    eval_comm(t_assign(I, Expr1), Env, E1),
    eval_bool(t_bool(Expr1, t_comp_opr(>), Expr2), E1, false),
    eval_for_comm(t_bool(I, t_comp_opr(=<), Expr2), t_pre_inc(I), K, E1, NewEnv).

eval_comm(t_for_range_comm(I, Expr1, Expr2, K), Env, NewEnv) :-
    eval_comm(t_assign(I, Expr1), Env, E1),
    eval_bool(t_bool(Expr1, t_comp_opr(<), Expr2), E1, false),
    eval_for_comm(t_bool(I, t_comp_opr(>=), Expr2), t_pre_dec(I), K, E1, NewEnv).

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