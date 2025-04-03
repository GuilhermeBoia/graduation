n = int(input())
linha = input().split()
horas = [int(e) for e in linha]

i = 0
rest = 0
max_rest = 0
seq_inicial = 0
first = False
while True:
    if i >= n:
        break

    if horas[i] == 1:
        if i == 0:
            first = True
            rest += 1
            seq_inicial += 1

        elif i == n-1:
            #if horas[0] == 1:
            rest += 1 + seq_inicial

        else:
            if first:
                rest += 1
                seq_inicial += 1
            else:
                rest += 1
        i += 1

    else:
        first = False
        i += 1
        rest = 0

    if max_rest < rest:
        max_rest = rest

print(max_rest)
