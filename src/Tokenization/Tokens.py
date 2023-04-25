def Tokenization(filename):
    assert filename[-3:] == 'lol', "Unsupported File Extension"

    TknsList, eachline = [], []
    token = ''
    strflag = False

    """
    Speacial Symbols to replace prebuild operators.
    ** -> ^
    != -> !
    // -> |
    == -> ~
    <= ≤  # ≤ alt + 243
    >= ≥  # ≥ alt + 242
    """

    # specialChar = ['/', '*', '+', '-', '^', '|', '%', '{', '}', '(', ')', '[', ']', '=', '~', '!', '≤', '≥', '<', '>', ':']
    specialChar = ['/', '*', '+', '-', '{', '}', '=', '~', '!','.','>','<']

    with open(filename, 'r') as grabber:
        lines = grabber.readlines()

    for line in lines:
        if line[0] != "#" and line[0] != '\n':

            for c in line:

                if c == '"':
                    if strflag is False:
                        strflag = True
                        if token != '':
                            eachline.append('\'' + token + '\'')
                            token = ''
                    else:
                        strflag = False
                        if token != '':
                            eachline.append('\'' + token + '\'')
                        token = ''
                    eachline.append("'{}'".format(c))

                elif strflag is True:
                    token += c

                elif 48 <= ord(c) <= 57:
                    if token != '':
                        eachline.append(token)
                        token = ''
                    eachline.append(c)

                elif c == ' ' or c == '\t':
                    if token != '':
                        eachline.append(token)
                        token = ''
                elif c == '\n':
                    if token != '':
                        eachline.append(token)
                    token = ''
                    TknsList.append(eachline.copy())
                    del eachline[:]

                elif c == ";":
                    if token != '':
                        eachline.append(token)
                    eachline.append(c)
                    TknsList.append(eachline.copy())
                    token = ''
                    del eachline[:]

                elif c == "#":
                    if token != '':
                        eachline.append(token)
                        TknsList.append(eachline.copy())
                        token = ''
                        del eachline[:]

                elif strflag is False and c in specialChar:
                    if token != '':
                        eachline.append(token)
                    token = ''

                    if c == '{' or c == '}':
                        eachline.append('\'' + c + '\'')
                    else:
                        eachline.append(c)
                else:
                    token += c

    if token != '' and token == '\n':
        eachline.append(token)
        eachline.append(';')
    TknsList.append(eachline.copy())
    del token
    del eachline
    temp = []
    for i in TknsList:
        temp.extend(i)
    return temp


if __name__ == "__main__":
    tokens = Tokenization("..\\!xobile Samples//basic.lol")
    print(tokens)
# if __name__ == "__main__":
#     tokens = Tokenization("..\\First.lol")
#     print(tokens)
