n = int(input())

for i in range(n):
    nums = int(input())
    linha = [int(e) for e in input().split()]
    linha.sort()

    adicionar = []
    output = []

    for e in linha:
        if e in output and linha.count(e) > 1:
            adicionar.append(e)
            
        if e not in output:
            output.append(e)

    for e in adicionar:
        output.append(e)

    for n in output:
        print(n, end=" ")