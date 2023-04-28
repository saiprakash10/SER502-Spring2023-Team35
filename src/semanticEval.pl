:- module(eval_program, [eval_program/2]).


eval_program(t_program(P), NewEnv) :- eval_command_list(P, [], NewEnv).

eval_command_list(t_command_list(Command, CommandList), Env, NewEnv) :-
    eval_command(Command, Env, E1),
    eval_command_list(CommandList, E1, NewEnv).
eval_command_list(t_command(Command), Env, NewEnv) :-
    eval_command(Command, Env, NewEnv).

eval_block(t_block(CommandList), Env, NewEnv) :- eval_command_list(CommandList, Env, NewEnv).

eval_command(t_assignment_expression(t_variable_name(Name), Expression), Env, NewEnv) :-
    eval_expression(Expression, Env, R1),
    update(Name, R1, Env, NewEnv).
eval_command(t_variable_declaration_command(Type, t_variable_name(Name), Expression), Env, NewEnv) :-
    eval_variable_type(Type, Env, R1),
    eval_expression(Expression, Env, R2),
    update(R1, Name, R2, Env, NewEnv).

eval_command(t_variable_declaration_command(Type, t_variable_name(Name)), Env, NewEnv) :- eval_variable_type(Type, Env, R1), R1 = int,  update(R1, Name, 0, Env, NewEnv).
eval_command(t_variable_declaration_command(Type, t_variable_name(Name)), Env, NewEnv) :- eval_variable_type(Type, Env, R1), R1 = float,  update(R1, Name, 0.0, Env, NewEnv).
eval_command(t_variable_declaration_command(Type, t_variable_name(Name)), Env, NewEnv) :- eval_variable_type(Type, Env, R1), R1 = string,  update(R1, Name, "", Env, NewEnv).
eval_command(t_variable_declaration_command(Type, t_variable_name(Name)), Env, NewEnv) :- eval_variable_type(Type, Env, R1), R1 = bool,  update(R1, Name, false, Env, NewEnv).

eval_command(t_print_expression(Expression), Env, Env) :- eval_expression(Expression, Env, Result), write(Result), nl.
eval_command(t_print_string(String), Env, Env) :- write(String), nl.

eval_command(t_for_loop_command(Assignment, Condition, Variable_Change, Block), Env, NewEnv) :-
    eval_command(Assignment, Env, E1),
    eval_for_command(Condition, Variable_Change, Block, E1, NewEnv).

eval_command(t_while_command(C, B), Env, NewEnv) :-
    eval_condition(C, Env, true),
    eval_block(B, Env, E1),
    eval_command(t_while_command(C, B), E1, NewEnv).
eval_command(t_while_command(C, _), Env, _) :-
    eval_condition(C, Env, false).

eval_command(t_for_enhanced_command(Variable, Expression1, Expression2, Block), Env, NewEnv) :-
    eval_command(t_assignment_expression(Variable, Expression1), Env, E1),
    eval_condition(t_condition(Expression1, t_comparison_operator(>), Expression2), E1, false),
    eval_for_command(t_condition(Variable, t_comparison_operator(=<), Expression2), t_pre_increment(Variable), Block, E1, NewEnv).

eval_command(t_for_enhanced_command(Variable, Expression1, Expression2, Block), Env, NewEnv) :-
    eval_command(t_assignment_expression(Variable, Expression1), Env, E1),
    eval_condition(t_condition(Expression1, t_comparison_operator(<), Expression2), E1, false),
    eval_for_command(t_condition(Variable, t_comparison_operator(>=), Expression2), t_pre_decrement(Variable), Block, E1, NewEnv).

eval_command(t_if_command(IfTree), Env, NewEnv) :- eval_if_part(IfTree, Env, NewEnv, _).
eval_command(t_if_command(IfTree, _, _), Env, NewEnv) :-
    eval_if_part(IfTree, Env, NewEnv, true).
eval_command(t_if_command(IfTree, ElifTree, _), Env, NewEnv) :-
    eval_if_part(IfTree, Env, _, false),
    eval_elif_part(ElifTree, Env, NewEnv, true).
eval_command(t_if_command(IfTree, ElifTree, ElseTree), Env, NewEnv) :-
    eval_if_part(IfTree, Env, _, false),
    eval_elif_part(ElifTree, Env, _, false),
    eval_else_part(ElseTree, Env, NewEnv, true).
eval_command(t_if_command(IfTree, _), Env, NewEnv) :-
    eval_if_part(IfTree, Env, NewEnv, true).
eval_command(t_if_command(IfTree, ElseTree), Env, NewEnv) :-
    eval_if_part(IfTree, Env, _, false),
    eval_else_part(ElseTree, Env, NewEnv, true).

eval_for_command(Condition, _, _, Env, Env) :-
    eval_condition(Condition, Env, false).

eval_for_command(Condition, t_pre_increment(Variable), Block, Env, NewEnv) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    eval_expression(t_increment(Variable), E1, E2),
    eval_for_command(Condition, t_pre_increment(Variable), Block, E2, NewEnv).

eval_for_command(Condition, t_pre_decrement(Variable), Block, Env, NewEnv) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    eval_expression(t_decrement(Variable), E1, E2),
    eval_for_command(Condition, t_pre_decrement(Variable), Block, E2, NewEnv).

eval_for_command(Condition, t_post_decrement(Variable), Block, Env, NewEnv) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    eval_expression(t_decrement(Variable), E1, E2),
    eval_for_command(Condition, t_post_decrement(Variable), Block, E2, NewEnv).

eval_for_command(Condition, t_post_increment(Variable), Block, Env, NewEnv) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, E1),
    eval_expression(t_increment(Variable), E1, E2),
    eval_for_command(Condition, t_post_increment(Variable), Block, E2, NewEnv).

eval_if_part(t_if(Condition, Block), Env, NewEnv, true) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, NewEnv).
eval_if_part(t_if(Condition, _), Env, Env, false) :-
    eval_condition(Condition, Env, false).

eval_elif_part(t_elif(Condition, Block), Env, NewEnv, true) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, NewEnv).
eval_elif_part(t_elif(Condition, Block, _), Env, NewEnv, true) :-
    eval_condition(Condition, Env, true),
    eval_block(Block, Env, NewEnv).
eval_elif_part(t_elif(Condition, _, ElifPart), Env, NewEnv, R) :-
    eval_condition(Condition, Env, false),
    eval_elif_part(ElifPart, Env, NewEnv, R).
eval_elif_part(t_elif(Condition, _, _), Env, Env, false) :-
    eval_condition(Condition, Env, false).

eval_else_part(t_else(Block), Env, NewEnv, true) :-
    eval_block(Block, Env, NewEnv).

eval_condition(t_condition(Expression1, Operator, Expression2), Env, Result) :-
    eval_expression(Expression1, Env, R1),
    eval_expression(Expression2, Env, R2),
    eval_comparison(R1, Operator, R2, Result).