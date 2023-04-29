:- use_module(token_reader).
:- use_module(parseTree).
:- use_module(semanticEval).

main(Filename) :- nl,
    ansi_format([bold,fg(yellow)], 'Parser in progress', []), nl,
    read_file(Filename, FileData),
    program(ParseTree, FileData, []),
    write("Parse Tree generation in progress: "), nl,
    write(ParseTree), nl,
    ansi_format([bold,fg(yellow)], 'Evaluation in progress', []), nl,
    eval_program(ParseTree, NewEnv), nl,
    ansi_format([bold,fg(yellow)], 'Environment after evaluation', []), nl,
    write(NewEnv), nl,
    halt.


