t = int(input())
for _ in range(t):
    n = int(input())
    w = [int(w) for w in input().split()]

    a = [[w[i], -1] for i in range(n)]
    r = []

    for i in range(n-1):
        s, d = [int(w) for w in input().split()]
        a[s-1][1] += 1
        a[d-1][1] += 1
    
    soma = sum(w)
    r.append(soma)
    a.sort(reverse=True)

    for c, re in a:
        while re:
            r.append(c+r[-1])
            re -= 1
    print(*r)