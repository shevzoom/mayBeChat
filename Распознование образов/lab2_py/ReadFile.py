d = { }
with open('/Users/glebsevcenko/Downloads/myVar.txt') as f:
    lst = f.readlines()
    for s in lst:
        s = s.replace(',', '.').split('\t')
        d[s[0]] = tuple(float(x) for x in s[1:])

