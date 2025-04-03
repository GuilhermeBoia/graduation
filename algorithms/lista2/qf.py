n = int(input())
linha = input()

count = 0
i = 0
while i < n:
    if linha[i] + linha[i-1] == "UR" or linha[i] + linha[i-1] == "RU":
        count += 1
        i += 2
    else:
        i += 1

tamanho = n - count

print(tamanho)
