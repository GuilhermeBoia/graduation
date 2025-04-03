linha = input().split()
n, m = int(linha[0]), int(linha[1])

a = input().split()
b = input().split()
array_a = [int(e) for e in a]
array_b = [int(e) for e in b]
array_a.sort()

count = 0
diferenca = []
for e in array_b:
    l = 0
    r = n-1
    while l <= r:
        if e >= array_a[(l+r)//2]:
            l = (l+r)//2 + 1
        else:
            r = (l+r)//2 - 1
    print(r+1, end=" ")