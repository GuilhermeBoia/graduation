n = int(input())

for i in range(n):
    nums = int(input())
    linha = [int(e) for e in input().split()]
    linha.sort()

    repetidos = {}
    adicionar = []
    output = []
    for e in linha:
        if e not in output:
            output.append(e)
        if linha.count(e) > 1:
            repetidos[e] = linha.count(e)

    for k in repetidos.keys():
        add = repetidos[k] - 1
        for i in range(add):
            output.append(k)

    for n in output:
        print(n, end = " ")