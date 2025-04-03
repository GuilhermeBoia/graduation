from collections import deque

s = input()
t = deque()
u = []

tamanho = len(s)

while len(u) < tamanho:
    if not s:
        u.append(t.pop())
    elif not t:
        t.append(s[0])
        s = s[1:]
    else:
        min_in_s = min(s)
        if t and t[-1] <= min_in_s:
            u.append(t.pop())
        else:
            t.append(s[0])
            s = s[1:]

u = ''.join(u)
print(u)