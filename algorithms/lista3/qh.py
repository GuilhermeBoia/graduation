linha = input()

count = 0
j = 0
while True:
    if j == len(linha) - 1 or linha == "":
        break

    if linha[j] == linha[j+1]:
        linha = linha[:j] + linha[j+2:]
        count += 1
        j = max(0, j-1)
    else:
        j += 1


if count % 2 == 0:
    print("No")
else:
    print("Yes")