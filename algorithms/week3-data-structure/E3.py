s = input().strip()
t = []
u = []

sufix = [None] * len(s)
sufix[-1] = s[-1]
for i in range(len(s) - 2, -1, -1):
    sufix[i] = min(s[i], sufix[i + 1])

for i in range(len(s)):
    t.append(s[i])
    while t and (i == len(s) - 1 or t[-1] <= sufix[i + 1]):
        u.append(t.pop())

print(''.join(u))