readFile(File, Final):-
    open(File, read, Stream),
    read(Stream,Final),
    close(Stream).

main(File, Op):-
    readFile(File,Tokens),
    program(ParseTree,Tokens,[]),
    eval_program(ParseTree, Op).
