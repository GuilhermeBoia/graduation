
s = input()
t = []
u = ""

tamanho = len(s)

while len(u) < tamanho:
    if len(s) == 0:
        u += t.pop()
    elif  len(t) == 0:
        t += s[0]
        s = s[1:]
    else:
        idx = -1
        tem = False
        if s[0] < t[-1]:
            tem = True
            idx = 0
            menor = s[0]
        else:
            menor = t[-1]
        
        for i in range(1, len(s)):
            if s[i] < menor:
                menor = s[i]
                idx = i
                tem = True

        if not tem:
            u += t.pop()
        else:
            t += s[0:idx+1]      
            s = s[idx+1:]
            u += t.pop()

print(u)