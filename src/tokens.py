import argparse
import os
import sys

from sly import Lexer

class extension:
      Token_extension = ".spetokens"


class SpectraLexer(Lexer):

    tokens = {ASSIGN, DECREMENT, DIVIDE, EQUAL, FLOAT, GE, GT, ID, INCREMENT, LE, LT, MINUS, MODULO,
              MULTIPLY, NOT_EQUAL, NUMBER, PLUS, POW, SINGLE_QUOTES, STRING}


    literals = {'{', '}', ',', '?', ';', ':', '[', ']', '(', ')', '.', '!'}

    ignore = ' \t'
    ignore_newline = r'\n+'
    ignore_comment = r'\#(.*)'

    EQUAL = r'=='
    DECREMENT = r'\--'
    ASSIGN = r'='
    INCREMENT = r'\++'
    DIVIDE = r'/'
    FLOAT = r'\d+\.\d+'
    GE = r'>='
    GT = r'>'
    ID = r'[a-zA-Z_][a-zA-Z0-9_]*'
    LE = r'<='
    LT = r'<'
    MINUS = r'-'
    MODULO = r'%'
    MULTIPLY = r'\*'
    NOT_EQUAL = r'!='
    NUMBER = r'\d+'
    PLUS = r'\+'
    POW = r'\^'
    SINGLE_QUOTES = r'\''
    STRING = r'\".*\"'


def parse_arguments():
    parser = argparse.ArgumentParser(
        description='Spectra source code is tokenized and stored in'
                    '<InputFileName>' + extension.Token_extension)
    parser.add_argument('input', metavar='InputFileName', type=str,
                        nargs=1, help='Path to source code')
    parser.add_argument('--evaluate', action='store_true', help='Evaluation of the generated tokens')
    return parser.parse_args()


def input_file(filename):
    content = None
    filename = "../data/" + filename
    
    try:
        with open(filename, "r") as input_file:
            content = input_file.read()
    except FileNotFoundError:
        print("No such file in path:", sys.argv[1])
    #print("Reading Source Code: "  + 'SUCCESSFUL' )
    return content


def store_tokens(tokens, filename):
    path = "tokenized files//" + filename
    with open(path, "w") as f:
        for t in tokens:
            f.write('{}\n'.format(t.value))
        print("Tokens are stored in " + filename)


if __name__ == '__main__':
    print("Tokenization in progress")
    parsed_args = parse_arguments()
    input_filename = parsed_args.input[0]
    output_filename = parsed_args.input[0][:-4:] + extension.Token_extension
    file_data = input_file(input_filename)

    lexer = SpectraLexer()
    tokens = lexer.tokenize(file_data)
    store_tokens(tokens, output_filename)

    eval = parsed_args.evaluate
    output_filename = "tokenized files//"+output_filename
    if eval:
        os.system("swipl -g \"main('" + output_filename + "')\" main.pl")
