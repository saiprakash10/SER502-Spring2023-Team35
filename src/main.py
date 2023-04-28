from Tokenization import Tokens

fileName = str(input("Enter the file fileName: "))
LinesOfTokens = (Tokens.Tokenization("spectra Samples//{}".format(fileName)))

with open('spectra Samples//{}tokenstreams'.format(fileName[:]), 'w') as tokens:
    for i in LinesOfTokens:
        print(i, file=tokens, flush=True)

with open('spectra Samples//{}tokenstreams'.format(fileName[:]), 'r') as grabber:
    lines = grabber.read()

temp = '['
for i in range(len(lines)):
    if lines[i] != '\n':
        temp += lines[i]
    else:
        if i != len(lines) - 1:
            temp += ', '
temp += '].'

with open('spectra Samples//{}tokens'.format(fileName[:]), 'w') as tokens:
    print(temp, file=tokens, flush=True)

print('{}tokens'.format(fileName[:]))
