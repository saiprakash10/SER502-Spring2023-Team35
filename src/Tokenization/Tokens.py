def Tokenization(filename):
    assert filename[-7:] == 'spectra', "File Extension not supported"

    Tokens_list, one_line = [], []
    tk = ''
    s_flag = False


    s_char = ['/', '*', '+', '-', '{', '}', '=', '~', '!','.','>','<']

    with open(filename, 'r') as grabber:
        lines = grabber.readlines()

    for line in lines:
        if line[0] != "#" and line[0] != '\n':

            for i, ch in enumerate(line):

                if ch == '"':
                    if s_flag is False:
                        s_flag = True
                        if tk != '':
                            one_line.append('\'' + tk + '\'')
                            tk = ''
                    else:
                        s_flag = False
                        if tk != '':
                            one_line.append('\'' + tk + '\'')
                        tk = ''
                    one_line.append("'{}'".format(ch))

                elif s_flag is True:
                    tk += ch

                elif ch.isalnum() and i > 0 and line[i-1].isalnum():
                    tk += ch

                elif 48 <= ord(ch) <= 57:
                    if tk != '':
                        one_line.append(tk)
                        tk = ''
                    one_line.append(ch)

                elif ch == ' ' or ch == '\t':
                    if tk != '':
                        one_line.append(tk)
                        tk = ''

                elif ch == '\n':
                    if tk != '':
                        one_line.append(tk)
                    tk = ''
                    Tokens_list.append(one_line.copy())
                    del one_line[:]

                elif ch == ";":
                    if tk != '':
                        one_line.append(tk)
                    one_line.append(ch)
                    Tokens_list.append(one_line.copy())
                    tk = ''
                    del one_line[:]

                elif ch == "#":
                    if tk != '':
                        one_line.append(tk)
                        Tokens_list.append(one_line.copy())
                        tk = ''
                        del one_line[:]

                elif s_flag is False and ch in s_char:
                    if tk != '':
                        one_line.append(tk)
                    tk = ''

                    if ch == '{' or ch == '}':
                        one_line.append('\'' + ch + '\'')
                    else:
                        one_line.append(ch)
                else:
                    if tk != '':
                        one_line.append(tk)
                        tk = ''
                    tk += ch

    if tk != '' and tk == '\n':
        one_line.append(tk)
        one_line.append(';')
    Tokens_list.append(one_line.copy())
    del tk
    del one_line
    token_temp = []
    for i in Tokens_list:
        token_temp.extend(i)
    return token_temp


if __name__ == "__main__":
    tks = Tokenization("..\\spectra Samples//addition.spectra")
    print(tks)
