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
    count = 0
    for i in range(n):
        if e >= array_a[i]:
            count += 1

        else:
            print(count, end=" ")
            break

        if i == n-1:
            print(count, end=" ")

#for e in diferenca:
    #print(e, end=" ")